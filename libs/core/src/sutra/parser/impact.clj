(ns sutra.parser.impact
  (:require [sutra.types.entities :as ents]
            [sutra.types.refs :as refs]
            [clojure.string :as str]
            [sutra.parser.spec :as spec]
            [sutra.types.links :refer [links-of]]
            [sutra.types.rid :as rid]
            [sutra.types.transform-io :as tio]))

(set! *warn-on-reflection* true)

(def ^:private nm rid/->name)

(def transform-outputs
  tio/outputs-of)

(def transform-inputs
  tio/inputs-of)

(defn object-type-entry
  [otypes name-like]
  (let [want (nm name-like)]
    (some (fn [[rid spec]] (when (= want (nm (:object-type-name spec))) [rid spec])) otypes)))

(defn edit-targets
  [a]
  (into #{} (map second) (refs/ref-values ents/action-type-refs a)))

(defn dependents-of
  [schema name-like]
  (when-let [[otid _] (object-type-entry (:object-types schema) name-like)]
    (let [links (mapv #(-> % (select-keys [:from :to :dir]) (assoc :link (:name %)))
                      (links-of schema otid))
          transforms (vec (for [[_ t] (:transforms schema)
                                :let [reads?  (boolean (some #(and (= :object-type (:kind %))
                                                                   (= otid (:object-type %)))
                                                             (transform-inputs t)))
                                      writes? (boolean (some #(and (= :object-type (:kind %))
                                                                   (= otid (:object-type %)))
                                                             (transform-outputs t)))]
                                :when (or reads? writes?)]
                            {:transform (nm (:transform-name t)) :reads reads? :writes writes?}))
          actions (vec (for [[an a] (:actions schema)
                             :when (contains? (edit-targets a) otid)]
                         {:action (nm (or (:action-name a) an))
                          :ops (vec (into (sorted-set)
                                          (comp (filter #(or (= otid (:object-type %))
                                                             (= spec/function-rule-op (:op %))))
                                                (keep :op) (map nm))
                                          (:rules a)))}))
          apps (vec (for [[_ a] (:applications schema)
                          :when (= otid (:object-type a))]
                      {:app (nm (:application-name a))}))]
      {:object-type (nm name-like) :rid otid
       :links links :transforms transforms :actions actions :apps apps
       :total (+ (count links) (count transforms) (count actions) (count apps))})))

(defn action-targets
  [action]
  (into #{} (map (fn [[targets v]] [(ents/section->tag (first targets)) v]))
        (refs/ref-values ents/action-edit-refs action)))

(defn- refs-after
  [s anchor lookahead]
  (let [alen (count anchor)]
    (loop [i 0 acc #{}]
      (if-let [j (str/index-of s anchor i)]
        (let [start (+ (long j) alen)
              end   (min (count s) (+ start lookahead))
              hit   (second (re-find #"\.([a-zA-Z][a-zA-Z0-9_-]*)" (subs s start end)))]
          (recur start (cond-> acc hit (conj hit))))
        acc))))

(defn function-params
  [source]
  (when (string? source)
    (some-> (re-find #"(?s)\(defn-?\s+(?:\^\{.*?\}\}\s+)?[^\s\[]+\s*(?:\"[^\"]*\"\s*)?\[([^\]]*)\]"
                     source)
            second
            str/trim)))

(defn function-references
  [source]
  (if-not (and (string? source) (seq source))
    {:invokes-actions #{} :reads-types #{} :edits-types #{}}
    {:invokes-actions (into #{} (map keyword) (refs-after source "OntologyClient/action" 80))
     :reads-types     (into #{} (map keyword) (refs-after source "OntologyClient/search" 80))
     :edits-types     (into #{}
                            (comp (mapcat (fn [[_ ts]] (re-seq #":([^\s,\]]+)" ts)))
                                  (map (comp keyword second)))
                            (re-seq #":sutra/function\s*\{:edits\s*\[([^\]]*)\]" source))}))

(defn- literal?
  [v]
  (or (string? v) (number? v) (boolean? v) (keyword? v)))

(defn action-property-writes
  [action]
  (let [ots  (edit-targets action)
        only (when (= 1 (count ots)) (first ots))]
    (reduce
     (fn [acc step]
       (let [ot (or (:object-type step) (:interface step) only)
             vs (:values step)]
         (if (and ot (map? vs))
           (reduce-kv (fn [a p v]
                        (cond-> (update-in a [ot p] (fnil into #{}) [])
                          (literal? v) (update-in [ot p] conj v)))
                      acc vs)
           acc)))
     {} (refs/steps (:rules action)))))

(defn value-keywords
  [x]
  (let [acc (volatile! #{})]
    (letfn [(walk [v]
              (cond
                (keyword? v)    (vswap! acc conj v)
                (map? v)        (run! walk (vals v))
                (sequential? v) (run! walk v)
                (set? v)        (run! walk v)
                :else           nil))]
      (walk x))
    @acc))

(defn source-name-refs
  [source]
  (if-not (and (string? source) (seq source))
    #{}
    (into #{} (map second) (re-seq #"[:.]([a-zA-Z][a-zA-Z0-9_-]*)" source))))

(defn condition-property-values
  [when-spec]
  (let [acc (volatile! {})
        add (fn [p vs]
              (when (keyword? p)
                (vswap! acc update p (fnil into #{}) (filter literal? vs))))]
    (letfn [(walk [w]
              (cond
                (vector? w)
                (let [[_op a & more] w]
                  (if (keyword? a)
                    (add a more)
                    (run! walk (rest w))))

                (map? w)
                (do (when-let [{:keys [attr value]} (:eq w)] (add attr [value]))
                    (when-let [{:keys [attr values]} (:contains-any w)] (add attr values))
                    (run! walk (concat (:and w) (:or w)
                                       (when (:not w) [(:not w)]))))

                :else nil))]
      (walk when-spec))
    @acc))

