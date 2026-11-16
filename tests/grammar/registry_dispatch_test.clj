(ns grammar.registry-dispatch-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [clojure.tools.reader :as r]
            [clojure.tools.reader.reader-types :as rt]
            [clojure.walk :as walk]
            [sutra.types.entities :as ents]
            [sutra.types.kinds :as kinds]
            [sutra.types.table :as tbl]))

(def ^:private roots
  ["src" "libs/core/src" "libs/dialect/src" "libs/kernel/src" "libs/world/src" "libs/commits/src"
   "libs/service/src" "plugins"])

(def ^:private any-alias
  (reify
    clojure.lang.ILookup (valAt [_ k] k) (valAt [_ k _] k)
    clojure.lang.IFn (invoke [_ k] k) (invoke [_ k _] k)))

(defn- forms [f]
  (try
    (binding [r/*alias-map* any-alias r/*read-eval* false
              r/*default-data-reader-fn* (fn [_ v] v)]
      (let [rdr (rt/indexing-push-back-reader (slurp f))]
        (loop [acc []]
          (let [x (r/read {:eof ::eof :read-cond :allow :features #{:clj}} rdr)]
            (if (= ::eof x) acc (recur (conj acc x)))))))
    (catch Exception e
      (throw (ex-info (str "registry-dispatch cannot read " f ": " (ex-message e))
                      {:file (str f)} e)))))

(defn- source-files []
  (for [r roots
        f (file-seq (io/file r))
        :when (re-find #"\.cljc?$" (.getName ^java.io.File f))]
    f))

(defn- parsed
  []
  (for [f (source-files)
        :let [[nsf & body] (forms f)]
        :when (and (seq? nsf) (= 'ns (first nsf)))]
    (let [reqs (into #{} (for [clause (drop 2 nsf)
                               :when (and (seq? clause) (= :require (first clause)))
                               spec (rest clause)]
                           (if (sequential? spec) (first spec) spec)))]
      [(second nsf) reqs body])))

(def ^:private module-registries
  {:filter-ops       ['sutra.capps.object-filter 'ops keys]
   :use-case-classes ['sutra.use-case.draft 'classes keys]
   :lineage-ports    ['sutra.viz.lineage-projection 'port-kinds keys]
   :lineage-nodes    ['sutra.viz.lineage-projection 'node-kinds #(map :kind %)]
   :control-tables   ['sutra.store.control.tables 'tables #(map :name %)]
   :install-classes  ['sutra.ref.install 'class-by-tag keys]
   :graph-node-kinds ['sutra.store.control.tables 'graph-node-kinds identity]
   :backed           ['sutra.types.backing 'backed keys]
   :catalog-nodes    ['sutra.catalog.nodes 'node-kinds keys]})

(defn- lifecycle-homes
  [files]
  (for [[nsn _ body] files
        :when (some #(and (seq? %) (= 'lc/table (first %))) (tree-seq coll? seq body))]
    nsn))

(defn- lifecycles
  [files]
  (into {}
        (for [nsn (lifecycle-homes files)
              :let [_ (require nsn)]
              [_ v] (ns-interns nsn)
              :let [t @v]
              :when (and (map? t) (set? (:states t)) (map? (:edges t)) (:name t))]
          [(keyword "lifecycle" (name (:name t)))
           {:keys (set (:states t)) :home nsn :scope :requirers}])))

(defn registries
  [files]
  (merge {:input-kinds {:keys (set (keys kinds/kinds)) :home 'sutra.types.kinds :scope :all}
          :sections    {:keys (set ents/sections) :home 'sutra.types.entities :scope :all}}
         (into {} (for [[reg [home sym keys-fn]] module-registries
                        :let [_ (require home)]]
                    [reg {:keys (set (keys-fn @(ns-resolve home sym))) :home home
                          :scope :requirers}]))
         (lifecycles files)))

(defn- case-constants
  [[_ _ & clauses]]
  (mapcat #(if (seq? %) % [%]) (take-nth 2 (if (odd? (count clauses))
                                             (butlast clauses)
                                             clauses))))

(defn- condp-constants [[_ _ _ & clauses]]
  (take-nth 2 (if (odd? (count clauses)) (butlast clauses) clauses)))

(defn- compared
  [test]
  (when (and (seq? test) (= '= (first test)) (= 3 (count test)))
    (let [[_ a b] test]
      (cond (keyword? a) [b a]
            (keyword? b) [a b]))))

(defn- eq-chain-constants
  [y]
  (let [tests (case (first y)
                cond (take-nth 2 (rest y))
                or   (rest y))]
    (->> (keep compared tests)
         (group-by first)
         vals
         (map #(map second %))
         (reduce (fn [a b] (if (> (count b) (count a)) b a)) []))))

(defn- branch-constants [y]
  (cond
    (and (seq? y) (= 'case (first y)))     (case-constants y)
    (and (seq? y) (= 'condp (first y)))    (condp-constants y)
    (and (seq? y) ('#{cond or} (first y))) (eq-chain-constants y)
    (set? y)                               y))

(defn- per-key-table?
  [y]
  (and (seq? y) (symbol? (first y)) (= "per-key" (name (first y)))))

(defn- reads-declared
  [y]
  (if (and (map? y) (contains? y :reads) (contains? y :step)) (dissoc y :reads) y))

(defn sites
  [ks body]
  (let [out (volatile! [])]
    (doseq [top body]
      (walk/prewalk (fn [y]
                      (if (per-key-table? y)
                        ()
                        (let [y (reads-declared y)]
                          (when-let [cs (branch-constants y)]
                            (let [hit (into #{} (filter ks) (set cs))]
                              (when (<= 2 (count hit))
                                (vswap! out conj [(when (seq? top) (second top)) y hit]))))
                          y)))
                    top))
    @out))

(defn- own-vocabulary?
  [regs reg nsn hit]
  (some (fn [[r {:keys [home] ks :keys}]]
          (and (not= r reg) (= home nsn) (every? ks hit)))
        regs))

(defn site-map
  []
  (let [files (vec (parsed))
        regs  (registries files)]
    (into (sorted-map)
          (for [[reg {:keys [home scope] ks :keys}] regs]
            [reg (into (sorted-map)
                       (for [[nsn reqs body] files
                             :when (and (not= home nsn)
                                        (or (= :all scope) (contains? reqs home)))
                             :let [s (remove (fn [[_ _ hit]] (own-vocabulary? regs reg nsn hit))
                                             (sites ks body))]
                             :when (seq s)]
                         [nsn (mapv (fn [[top f _]] [top f]) s)]))]))))

(defn current []
  (update-vals (site-map) #(update-vals % count)))

(deftest no-namespace-dispatches-on-a-registry-by-hand
  (doseq [[reg by-ns] (site-map)]
    (testing (str reg ": a branch over its keys -- ask the registry (a facet, a table keyed "
                  "by it, or tbl/per-key) instead")
      (is (= {} (update-vals by-ns #(mapv first %)))))))

(deftest every-lifecycle-table-is-a-registry
  (let [files (vec (parsed))
        ls    (lifecycles files)]
    (is (seq ls))
    (is (= (set (lifecycle-homes files)) (set (map :home (vals ls))))
        "a namespace builds a lifecycle table no var holds")))

(deftest a-per-key-table-names-every-key-of-its-registry
  (let [reg {:a {} :b {} :c {}}]
    (is (= {:a 1 :b nil :c nil} (tbl/per-key "t" reg {:a 1 #{:b :c} nil}))
        "a set of keys shares one value")
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"names no entry.*:c"
                          (tbl/per-key "t" reg {:a 1 :b 2}))
        "a key the table leaves out stops it")
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"does not hold.*:d"
                          (tbl/per-key "t" reg {:a 1 :b 2 :c 3 :d 4}))
        "a key the registry lacks stops it")
    (is (= 3 (tbl/on {:a +} :a 1 2)) "a fn is applied")
    (is (= :v (tbl/on {:a :v} :a 1)) "a value is the answer")
    (is (nil? (tbl/on {:a nil} :b)) "an absent key answers nil")))
