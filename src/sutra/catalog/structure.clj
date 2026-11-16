(ns sutra.catalog.structure
  (:require [sutra.types.refs :as refs]
            [sutra.types.rid :as rid]
            [sutra.types.entities :as ents]
            [sutra.types.object-set :as os]
            [sutra.parser.spec :as spec]
            [sutra.catalog.llm-models :as llm-models]
            [clojure.set :as set]
            [sutra.catalog.lookup :as lookup]
            [sutra.catalog.actions :as actions]))

(set! *warn-on-reflection* true)

(defn object-set-refs
  [spec]
  (reduce (fn [acc {:keys [op root link include]}]
            (cond-> acc
              (and (#{:scan :static} op) (some? root)) (update :roots conj root)
              (= :search-around op) (update :links conj link)
              (= :filter op) (update :links into (map #(if (map? %) (:link %) %)) include)))
          {:roots [] :links []}
          (os/nodes (if (:op spec) spec (os/qt->ir (os/definition spec))))))

(defn function-schema-violations
  [schema compiled]
  (let [actions  (set (keys (actions/executable-actions schema)))
        ots      (lookup/names-and-rids schema :object-types)
        cts      (lookup/names-and-rids schema :link-types)
        qts      (or (:object-sets compiled) {})
        sys?     (fn [k] (and (keyword? k) (= "sys" (namespace k))))
        type-ok? (fn [t] (or (nil? t) (contains? ots t)))
        viol     (volatile! [])
        add!     (fn [kind ref step] (vswap! viol conj {:kind kind :ref ref :step step}))]
    (doseq [step (refs/steps (:steps compiled))]
      (case (:op step)
        (:effect :call)
        (when-not (or (contains? actions (:fn step)) (sys? (:fn step)))
          (add! :unknown-action (:fn step) step))
        (:create :search)
        (when-not (type-ok? (:type step))
          (add! :unknown-type (:type step) step))
        :use-llm
        (do (when-not (llm-models/registered? schema (:model step))
              (add! :unregistered-model (:model step) step))
            (doseq [t (:tools step) :when (map? t)]
              (when-let [a (:apply-action t)]
                (when-not (contains? actions a) (add! :unknown-action a step)))
              (when-let [ot (:query-objects t)]
                (when-not (contains? ots ot) (add! :unknown-type ot step)))))
        nil))
    (doseq [[_ qt] qts
            :let [{:keys [roots links]} (object-set-refs qt)]]
      (doseq [t roots :when (not (type-ok? t))]
        (add! :unknown-type t qt))
      (doseq [l links :when (not (contains? cts l))]
        (add! :unknown-link l qt)))
    @viol))

(def schema-section-keys (set ents/sections))

(defn unknown-section-keys
  [schema]
  (set/difference (set (keys schema)) schema-section-keys))

(def value-checks
  {:object-sets (fn [v] (and (keyword? (:object-set-name v))
                             (spec/object-set-valid? (os/definition v))))
   :llm-models  llm-models/valid-rate?})

(defn entry-ok?
  [cls k v type?]
  (let [{:keys [key id-match?]} (ents/by-section cls)
        key-ok? (case key
                  :rid     #(rid/well-formed-for? cls %)
                  :keyword keyword?
                  :string  string?)
        value   (or (value-checks cls) (spec/validators cls))]
    (boolean (and (key-ok? k)
                  (or (not id-match?) (= k (:id v)))
                  (value v)
                  (or (not= :functions cls) (:library v) (type? (:target-type v)))))))

(defn valid-schema-structure? [schema]
  (and (empty? (unknown-section-keys schema))
       (every?
        (fn [{:keys [section required?]}]
          (let [m (get schema section)]
            (or (and (not required?) (nil? m))
                (and (map? m)
                     (every? (fn [[k v]]
                               (entry-ok? section k v #(contains? (:object-types schema) %)))
                             m)))))
        ents/entities)))
