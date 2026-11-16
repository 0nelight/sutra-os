(ns sutra.catalog.function-contract
  (:require [sutra.capps.catalog :as cat]
            [sutra.catalog.functions :as functions]
            [sutra.catalog.program :as prog]
            [sutra.kit.coerce :refer [->name-kw]]
            [sutra.kit.coll :as coll]
            [sutra.types.refs :as refs]
            [sutra.parser.spec :as spec]
            [sutra.types.transform-io :as tio]))

(set! *warn-on-reflection* true)

(defn- declared
  [spec]
  (try (prog/declared-params spec) (catch RuntimeException _ nil)))

(defn- edit-signature
  [spec]
  (let [from-source (declared spec)
        params      (merge (update-vals from-source :type) (:param-types spec))]
    {:params      params
     :required    (into #{} (remove #(:optional (get from-source %))) (keys params))
     :target-type (:target-type spec)}))

(defn- query-signature
  [spec]
  (let [from-source (declared spec)
        type-of     (fn [[alias in]] (or (:type in) (:type (get from-source alias))))]
    (cond-> {:params   (into {} (map (juxt first type-of)) (tio/call-inputs spec))
             :required (set (tio/unbound-scalars spec #{}))}
      (seq (:returns spec)) (assoc :returns (:returns spec)))))

(defn signature
  [kind spec]
  (case kind
    :edit  (edit-signature spec)
    :query (query-signature spec)
    nil))

(def ^:private numeric-rank
  {:byte 0 :short 1 :integer 2 :long 3 :float 4 :double 5 :decimal 6})

(defn- widens?
  [from to]
  (let [a (numeric-rank from) b (numeric-rank to)]
    (boolean (and a b (< a b)))))

(defn- break [text] {:change :break :text text})

(defn- input-changes
  [{op :params} {np :params required :required :as new} old]
  (concat
   (for [p (keys op) :when (not (contains? np p))]
     (break (str "input " p " was dropped")))
   (for [[p t] op
         :let [t' (get np p)]
         :when (and (some? t) (some? t') (not= t t'))
         :let [text (str "input " p " changed type from " t " to " t')]]
     (if (widens? t t') {:change :widened :text text} (break text)))
   (for [p (sort-by str required) :when (not (contains? op p))]
     (break (str "required input " p " was added")))
   (for [p (sort-by str required)
         :when (and (contains? op p) (:required old) (not (contains? (:required old) p)))]
     (break (str "optional input " p " became required")))
   (when (and (:target-type old) (not= (:target-type old) (:target-type new)))
     [(break (str "the function is bound to " (:target-type new)
                  " instead of " (:target-type old)))])))

(defn- output-changes
  [{old :returns} {new :returns}]
  (if (and (seq old) (empty? new))
    [(break "the declared output was dropped")]
    (for [[c t] old
          :let [t' (get new c ::gone)]
          :when (not= t t')]
      (break (if (= ::gone t')
               (str "output column " c " was dropped")
               (str "output column " c " changed type from " t " to " t'))))))

(defn changes
  [old new]
  (vec (concat (input-changes old new old) (output-changes old new))))

(defn- texts-of [kind cs] (into [] (comp (filter #(= kind (:change %))) (map :text)) cs))

(defn breaks [old new] (texts-of :break (changes old new)))

(defn widenings [old new] (texts-of :widened (changes old new)))

(defn returns-problem
  [returns rows]
  (when (seq returns)
    (let [row-keys (fn [r] (into #{} (map name) (keys r)))
          missing  (into (sorted-set)
                         (mapcat (fn [r]
                                   (if (map? r)
                                     (remove (comp (row-keys r) name) (keys returns))
                                     (keys returns))))
                         rows)]
      (when (seq missing)
        (str "it declares the output column(s) " (vec missing) " under :returns, and a row"
             " it returned does not hold them")))))

(defn call-key
  [f]
  (when (or (keyword? f) (string? f)) (->name-kw f)))

(defn- action-calls
  [action-name {:keys [params rules]}]
  (let [steps (refs/steps rules)
        named (keep :as steps)]
    (for [{:keys [op function inputs version auto-upgrade batched]} steps
          :when (#{spec/function-rule-op :invoke-function} op)
          :let [f (call-key function)]
          :when (and f (not batched) (or (seq inputs) (map? params)))]
      {:consumer [:action action-name]
       :function f
       :dep      (when version {:version version :auto-upgrade auto-upgrade})
       :binds    (into #{} (map ->name-kw)
                       (if (seq inputs) (keys inputs) (concat (keys params) named)))})))

(defn- app-calls
  [app-name app]
  (for [{:keys [function args]} (cat/function-vars app)]
    {:consumer [:application app-name]
     :function function
     :binds    (into #{} (keep ->name-kw) (keys args))}))

(defn entry-calls
  [sec k e]
  (when-let [f ({:actions action-calls :applications app-calls} sec)] (f k e)))

(defn calls
  [{:keys [actions applications]}]
  (vec (sort-by (comp str :consumer)
                (concat (mapcat (fn [[nm a]] (action-calls nm a)) actions)
                        (mapcat (fn [[nm a]] (app-calls nm a)) applications)))))

(defn- follows-head?
  [{:keys [dep]}]
  (or (nil? dep) (:auto-upgrade dep)))

(defn callers-of
  [all-calls fn-name]
  (filterv #(and (= fn-name (:function %)) (follows-head? %)) all-calls))

(defn newly-unbound
  [old new {:keys [binds]}]
  (let [unbound (fn [sig] (into #{} (remove binds) (:required sig)))]
    (vec (sort-by str (remove (unbound old) (unbound new))))))

(defn- by-entry
  [w]
  (coll/index-by (juxt :kind :rid) (functions/all-functions w)))

(defn function-changes
  [before after]
  (let [sites (calls after)
        olds  (by-entry before)]
    (vec
     (sort-by
      (juxt (comp str :function) (comp str :kind))
      (for [{:keys [name kind spec rid]} (functions/all-functions after)
            :let [old-entry (get olds [kind rid])]
            :when (and old-entry (= name (:name old-entry)) (not (:library spec)))
            :let [old (signature kind (:spec old-entry))
                  new (signature kind spec)]
            :when (not= old new)
            :let [callers (callers-of sites name)]]
        {:function  name
         :kind      kind
         :breaks    (breaks old new)
         :widenings (widenings old new)
         :callers   (vec (distinct (map :consumer callers)))
         :unbound   (vec (for [c callers
                               :let [inputs (newly-unbound old new c)]
                               :when (seq inputs)]
                           {:consumer (:consumer c) :inputs inputs}))})))))

(defn- unbound-by
  [U {:keys [function dep binds]}]
  (let [{:keys [kind spec unsatisfied]} (functions/function-at-version U function dep)]
    (when (and spec (not unsatisfied))
      (vec (sort-by str (remove binds (:required (signature kind spec))))))))

(defn- action-call? [call] (= :action (first (:consumer call))))

(defn action-regressions
  [before after]
  (let [prior (group-by (juxt :consumer :function) (filter action-call? (calls before)))]
    (vec
     (for [{[_ action] :consumer :keys [function consumer] :as call}
           (filter action-call? (calls after))
           :when (not= (get-in before [:actions action]) (get-in after [:actions action]))
           :let [earlier (get prior [consumer function])
                 bound   (into #{} (mapcat :binds) earlier)
                 inputs  (cond->> (unbound-by after call)
                           (seq earlier) (filterv bound))]
           :when (seq inputs)]
       {:action action :function function :inputs inputs}))))

(defn undeclared-outputs
  [before after]
  (let [olds  (by-entry before)
        sites (calls after)]
    (vec
     (sort-by
      (comp str :function)
      (for [{:keys [name kind spec rid]} (functions/all-functions after)
            :when (and (= :query kind) (empty? (:returns spec))
                       (not= spec (:spec (get olds [kind rid]))))
            :let [callers (into [] (comp (filter #(= name (:function %))) (map :consumer)
                                         (distinct))
                                sites)]
            :when (seq callers)]
        {:function name :callers callers})))))
