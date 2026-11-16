(ns sutra.catalog.streaming
  (:require [sutra.types.transform-io :as tio]
            [sutra.catalog.backing :as backing]
            [sutra.catalog.lookup :as lookup]
            [sutra.parser.impact :as impact]
            [sutra.types.attribute :as attribute]))

(set! *warn-on-reflection* true)

(def consistencies
  #{:exactly-once :at-least-once})

(defn- stream-dataset
  [schema ds]
  (when-let [rid (or (when (contains? (:datasets schema) ds) ds)
                     (some->> ds name keyword (lookup/rid-of schema :datasets)))]
    (when (get-in schema [:datasets rid :stream]) rid)))

(defn stream-backed
  [schema]
  (into {}
        (keep (fn [[ot spec]]
                (when-let [s (some->> (backing/object-backing schema ot) :dataset
                                      (stream-dataset schema))]
                  (let [b (:backing spec)]
                    [ot {:stream           s
                         :direct?          (true? (:direct b))
                         :consistency      (or (some-> (:consistency b) keyword)
                                               :exactly-once)
                         :source-timestamp (some-> (:source-timestamp b) keyword)}]))))
        (:object-types schema)))

(defn settings
  [schema ot]
  (get (stream-backed schema) ot))

(defn- type-key
  [schema t]
  (cond
    (contains? (:object-types schema) t) t
    (some? t) (some->> t name keyword (lookup/rid-of schema :object-types))))

(defn- shown [schema x] (or (lookup/name-of schema :object-types x) x))

(defn edit-violations
  [schema]
  (let [streaming (into #{} (keep (fn [[ot s]] (when-not (:direct? s) ot)))
                        (stream-backed schema))]
    (when (seq streaming)
      (for [[an a] (:actions schema)
            [kind t] (impact/action-targets a)
            :let [ot (when (= :object-type kind) (type-key schema t))]
            :when (contains? streaming ot)]
        {:action (or (:action-name a) an) :object-type (shown schema ot)}))))

(defn multi-datasource-violations
  [schema]
  (let [streaming (set (keys (stream-backed schema)))]
    (when (seq streaming)
      (for [[tk t] (:transforms schema)
            out (tio/outputs-of t)
            :let [ot (type-key schema (:object-type out))]
            :when (and (contains? streaming ot) (= :supplement (some-> (:role out) keyword)))]
        {:transform (or (:transform-name t) tk) :object-type (shown schema ot)}))))

(def ^:private timestamp-types #{:timestamp :datetime :instant :date-time})

(defn- timestamp-attr?
  [spec attr]
  (let [t (get-in (attribute/of spec) [:types attr])]
    (contains? timestamp-types (if (sequential? t) (first t) (some-> t keyword)))))

(defn setting-violations
  [schema]
  (let [backed (stream-backed schema)]
    (for [[ot spec] (:object-types schema)
          :let [b (:backing spec)
                s (get backed ot)
                ts (some-> (:source-timestamp b) keyword)
                why (cond
                      (and (nil? s) (some #(contains? b %)
                                          [:direct :consistency :source-timestamp]))
                      (str "sets :direct, :consistency or :source-timestamp on its :backing,"
                           " which only a stream-backed type takes")

                      (and s (contains? b :consistency)
                           (not (contains? consistencies (keyword (:consistency b)))))
                      (str ":consistency " (pr-str (:consistency b)) " is none of "
                           (pr-str (sort consistencies)))

                      (and ts (not (:direct? s)))
                      (str ":source-timestamp orders direct writes — set :direct true on the"
                           " :backing, or drop it")

                      (and ts (not (timestamp-attr? spec ts)))
                      (str ":source-timestamp " (pr-str ts) " names no timestamp property of"
                           " the type"))]
          :when why]
      {:object-type (shown schema ot) :reason why})))

(defn timestamp-ms
  [v]
  (cond (nil? v) nil
        (number? v) (long v)
        (inst? v) (inst-ms v)
        :else (try (inst-ms (java.time.Instant/parse (str v)))
                   (catch Exception _ nil))))

(defn derive-violations
  [schema]
  (for [[ds spec] (:datasets schema)
        :let [{:keys [from function] :as d} (get-in spec [:stream :derive])
              src (when from (stream-dataset schema from))
              fn? (when function
                    (or (contains? (:functions schema) function)
                        (some->> function name keyword (lookup/rid-of schema :functions))))
              why (cond
                    (nil? d) nil
                    (nil? src) (str ":derive :from " (pr-str from) " names no stream")
                    (= src ds) ":derive reads the stream it writes"
                    (not fn?) (str ":derive :function " (pr-str function)
                                   " names no function"))]
        :when why]
    {:dataset (or (:dataset-name spec) ds) :reason why}))

(defn out-of-order
  [ts-attr rows stored]
  (into #{}
        (keep (fn [{:keys [id values]}]
                (let [new (timestamp-ms (get values ts-attr))
                      old (timestamp-ms (get stored id))]
                  (when (and new old (< (long new) (long old))) id))))
        rows))
