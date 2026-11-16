(ns sutra.catalog.functions
  (:require [sutra.types.rid :as rid]
            [sutra.kit.refusal :as refusal]
            [sutra.types.versions :as vers]
            [sutra.types.schema :as sch]
            [sutra.types.semver :as semver]
            [sutra.types.run-limits :as limits]
            [sutra.parser.spec :as spec]
            [clojure.string :as str]
            [sutra.types.world-rules :as rules]
            [sutra.catalog.lookup :as lookup]
            [sutra.catalog.actions :as actions]
            [sutra.catalog.interfaces :as interfaces]))

(set! *warn-on-reflection* true)

(defn query-function?
  [spec]
  (boolean (:function-name spec)))

(defn query-functions
  [schema]
  (into {} (filter (comp query-function? val)) (:transforms schema)))

(defn build-transforms
  [schema]
  (into {} (remove (comp query-function? val)) (:transforms schema)))

(defn all-functions
  [schema]
  (concat
   (for [[rid spec] (:functions schema)]
     {:name (:function-name spec) :kind :edit :rid rid :spec spec})
   (for [[rid spec] (query-functions schema)]
     {:name (:function-name spec) :kind :query :rid rid :spec spec})))

(declare function-by-name)

(defn releases
  [spec]
  (or (:releases spec) {}))

(defn released-versions
  [spec]
  (vec (sort semver/compare-versions (keys (releases spec)))))

(defn release-conflict
  [live-spec version snapshot]
  (when-let [published (get (releases live-spec) version)]
    (when (not= (vers/snapshot published) (vers/snapshot snapshot))
      [(if (= (dissoc (vers/snapshot published) :libraries)
              (dissoc (vers/snapshot snapshot) :libraries))
         :refusal.authoring/release-libraries-changed
         :refusal.authoring/release-source-changed)
       {:version version}])))

(defn function-at-version
  [schema fn-name {:keys [version] :as dep}]
  (when-let [{:keys [spec] :as entry} (function-by-name schema fn-name)]
    (let [available (released-versions spec)
          resolved  (when version (semver/max-satisfying available dep))]
      (cond
        (nil? version) (assoc entry :version (:version spec))
        resolved       (assoc entry :version resolved
                              :spec (merge spec (get (releases spec) resolved)))
        :else          (assoc entry :unsatisfied dep :available available)))))

(defn unresolvable-function-deps
  [schema action]
  (vec (for [{:keys [op function version] :as dep} (spec/function-deps action)
             :let  [entry (when version (function-at-version schema function dep))]
             :when (and entry (:unsatisfied entry))]
         (str op " on " (pr-str function) " depends on version " version
              (when (:auto-upgrade dep) " (auto-upgrade)")
              ", which no release satisfies — published: "
              (if (seq (:available entry))
                (str/join ", " (:available entry))
                "none")))))

(declare implementers)

(def function-rule
  spec/function-rule)

(defn batched-action?
  [schema action-name]
  (boolean (:batched (function-rule (actions/resolve-action schema action-name)))))

(defn action-batch-limit
  [schema action-name]
  (let [{:keys [calls-max unbatched-function-calls-max]} limits/action-batch
        rule (function-rule (actions/resolve-action schema action-name))]
    (if (and rule (not (:batched rule))) unbatched-function-calls-max calls-max)))

(defn- edit-of-event
  [{:keys [op target]}]
  (case op
    (:create :update :delete-object) (when (:type target) {:object-type (:type target)})
    (:create-link :delete-link)      (when (:type target) {:link-type (:type target)})
    nil))

(defn provenance-violations
  [schema action events]
  (if-not (function-rule action)
    []
    (let [canon    (fn [e] (cond-> e
                             (:object-type e)
                             (update :object-type #(or (lookup/rid-of schema :object-types %) %))
                             (:link-type e)
                             (update :link-type #(or (lookup/rid-of schema :link-types %) %))))
          declared (into #{} (map canon)
                         (concat (:edits action)
                                 (->> (:rules action)
                                      (filter spec/action-log-rule?)
                                      (map #(select-keys % [:object-type])))))
          ifaces   (into #{} (comp (keep :interface)
                                   (mapcat #(rules/implementers schema %))
                                   (map #(or (lookup/rid-of schema :object-types %) %)))
                         (:edits action))]
      (into []
            (comp (keep edit-of-event)
                  (remove #(let [c (canon %)]
                             (or (contains? declared c)
                                 (contains? ifaces (:object-type c)))))
                  (distinct))
            events))))

(defn function-by-name
  [schema fn-name]
  (let [k (some-> fn-name name keyword)]
    (first (filter #(= k (:name %)) (all-functions schema)))))

(defn- library-order
  [fn-of nm spec]
  (let [step (fn step [[order problems seen] path user lib-kw]
               (let [lib (fn-of lib-kw)
                     bad #(vector order (conj problems (str (name user) " uses " (name lib-kw)
                                                            ", " %)) seen)]
                 (cond
                   (some #{lib-kw} path)
                   (bad (str "a cycle: " (str/join " → " (map name (conj path lib-kw)))))
                   (seen lib-kw)  [order problems seen]
                   (nil? lib)     (bad "which names no function")
                   (not (:library lib))
                   (bad "which is a published function, not a library (:library true)")
                   (not= :clojure (keyword (name (or (:language lib) :clojure))))
                   (bad "which is not Clojure")
                   (not= (some-> (:repository lib) rid/->name)
                         (some-> (:repository spec) rid/->name))
                   (bad "which lives in another repository — a library is shared within one")
                   :else
                   (let [[o p s] (reduce #(step %1 (conj path lib-kw) lib-kw %2)
                                         [order problems seen] (:uses lib))]
                     [(conj o [lib-kw lib]) p (conj s lib-kw)]))))
        [order problems] (reduce #(step %1 [nm] nm %2) [[] [] #{}] (:uses spec))]
    [order problems]))

(defn uses-problems
  [fn-of nm spec]
  (cond-> (second (library-order fn-of nm spec))
    (and (seq (:uses spec)) (not= :clojure (keyword (name (or (:language spec) :clojure)))))
    (conj (str (name nm) " :uses libraries, which only a Clojure function can"))))

(defn bound-libraries
  [fn-of nm spec]
  (let [[order problems] (library-order fn-of nm spec)]
    (when (seq problems)
      (refusal/refuse! :refusal.catalog/function-invalid {:function nm}
                       {:function-name (name nm) :listed (str/join "; " problems)}))
    (mapv (fn [[k lib]] {:name k :source (:source lib)}) order)))

(defn function-code
  [fn-of nm spec]
  (let [release (get (releases spec) (:version spec))
        libs    (cond (contains? spec :libraries) (:libraries spec)
                      release                     (:libraries release)
                      :else                       (bound-libraries fn-of nm spec))]
    (str/join "\n" (concat (map :source libs) [(:source spec)]))))

(defn function-lookup
  [schema]
  (fn [nm] (:spec (function-by-name schema nm))))

(defn library-lookup
  [fns current]
  (let [names (sch/rid-name-index current)]
    (fn [nm]
      (or (get fns (keyword (name nm)))
          (some-> (function-by-name current nm) :spec
                  (update :repository #(get names % %)))))))

(defn build-transform-by-name
  [schema t-name]
  (let [k (some-> t-name name keyword)]
    (some (fn [spec] (when (= k (:transform-name spec)) spec))
          (vals (build-transforms schema)))))
