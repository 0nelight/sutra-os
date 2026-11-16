(ns platform.world-gates-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.string :as str]
            [sutra.catalog.world-gates :as world-gates]
            [sutra.catalog.kernel :as kernel]
            [sutra.catalog.problems :as problems]
            [sutra.catalog.structure :as cstructure]
            [sutra.world.gate :as gate]
            [sutra.world.gate.structure :as structure]
            [sutra.world.gen :as kg]
            [sutra.parser.lint :as lint]
            [sutra.parser.spec :as spec]
            [sutra.world.db :as db]
            [sutra.commits.log :as log]
            [sutra.store.kept :as kept]
            [sutra.world.gen.near :as gn]
            [sutra.world.gen.space :as gs]
            [sutra.types.rid :as rid]
            [sutra.types.schema :as sch]))

(def ^:private rows world-gates/rows)

(def ^:private views world-gates/views)

(def ^:private ported
  [:primary-organization-unique? :project-organizations-in-space?
   :ontology-references-local? :media-sets-declared? :media-set-sources?
   :stream-derivations-valid? :stream-types-unedited? :stream-types-single-datasource?
   :stream-backing-settings-valid?])

(defn- platform [id W]
  (frequencies (map ::problems/row ((get problems/gate-problems id) W))))

(defn- kernel [id d]
  (frequencies (mapcat val (get (db/view d :findings) id))))

(defn- agree [W]
  (let [d (db/db W views)]
    (into {} (for [id ported :let [p (platform id W) k (kernel id d)] :when (not= p k)]
               [id {:platform p :kernel k}]))))

(defspec the-kernel-finds-what-the-platform-finds 300
  (prop/for-all [W gs/world]
    (or (not (gs/one-name-per-scope? W #(sch/scope-of W %)))
        (not (gs/unshadowed? W [:datasets :functions :object-types]))
        (not (gs/one-producer-per-type? W))
        (empty? (agree W)))))

(def ^:private near-ported
  [:link-types-valid? :actions-valid? :object-sets-valid? :transforms-valid?])

(defn- side-row?
  [r]
  (str/starts-with? (str (:reason r)) "object type "))

(defn- normal [id rows]
  (if (= :object-sets-valid? id)
    (boolean (seq rows))
    (frequencies (map #(cond-> % (side-row? %) (dissoc :link)) rows))))

(defn- near-agree [W]
  (let [d  (db/db W views)
        fs (db/view d :findings)]
    (into {} (for [id near-ported
                   :let [p (normal id (map ::problems/row ((get problems/gate-problems id) W)))
                         k (normal id (for [r rows :when (= id (:gate r (:id r)))
                                            ps (vals (get fs (:id r)))
                                            p ps]
                                        p))]
                   :when (not= p k)]
               [id {:platform p :kernel k}]))))

(defspec the-kernel-runs-the-platforms-checks-on-neighbourhoods 300
  (prop/for-all [W gn/world]
    (or (not (gn/oracle-defined? W))
        (empty? (near-agree W)))))

(defn- platform-lint [W] (frequencies (lint/gate-violations W (spec/vocabulary))))

(defn- kernel-lint [d]
  (frequencies (for [r rows :when (:lint r) ps (vals (get (db/view d :findings) (:id r))) p ps]
                 p)))

(defspec the-kernel-lints-on-neighbourhoods 300
  (prop/for-all [W gn/world]
    (or (not (gn/oracle-defined? W))
        (= (platform-lint W) (kernel-lint (db/db W views))))))

(defn- lint-key [f] [(:rule f) (str (:entity f))])

(defspec a-change-is-linted-for-what-it-adds 300
  (prop/for-all [W gn/world cs (gen/vector gn/change 1 4)]
    (let [W1 (reduce gn/apply-change W cs)]
      (or (not (and (gn/oracle-defined? W) (gn/oracle-defined? W1)))
          (let [old (set (map lint-key (lint/gate-violations W (spec/vocabulary))))
                d0  (db/db W views)
                d1  (db/db-with d0 (kg/diff W W1))]
            (= (frequencies (remove #(contains? old (lint-key %))
                                    (lint/gate-violations W1 (spec/vocabulary))))
               (frequencies (for [f (gate/report-since rows d0 d1)
                                  :when (= :lint-gate-clean? (:gate f))]
                              (dissoc f :gate)))))))))

(deftest the-worlds-have-findings
  (let [ws (filter gn/oracle-defined? (gen/sample gn/world 600))
        n  (fn [id] (count (filter #(seq ((get problems/gate-problems id) %)) ws)))]
    (is (< 100 (count ws)))
    (doseq [id near-ported] (is (< 20 (n id)) (str id)))
    (let [by-rule (frequencies (for [W ws f (lint/gate-violations W (spec/vocabulary))]
                                 (:rule f)))]
      (doseq [r (disj lint/gated-rules :action-rule-op-unknown :action-override-misleading)]
        (is (pos? (by-rule r 0)) (str r))))))

(deftest every-ported-gate-is-a-row
  (is (every? (set (map #(:gate % (:id %)) rows)) (into ported near-ported))))

(def ^:private structure-views
  [gate/facts (gate/findings-of (structure/rows world-gates/checks))])

(defn- nodes-of [W]
  (sort-by str (for [[s es] W :when (map? es) k (keys es)] [s k])))

(defn- break
  [W [kind n]]
  (let [ns   (nodes-of W)
        [s k :as node] (when (seq ns) (nth ns (mod n (count ns))))
        e    (when node (get-in W node))]
    (case kind
      :rekey   (cond-> W node (-> (update s dissoc k)
                                  (assoc-in [s (if (even? n) (keyword (str "k" n)) (str "k" n))]
                                            e)))
      :bad-id  (cond-> W node (assoc-in (conj node :id) (str "x" n)))
      :shape   (cond-> W node (update-in node dissoc (rid/name-field-of s)))
      :unknown (assoc W (keyword (str "bogus" (mod n 3))) {})
      :not-map (cond-> W node (assoc s [])))))

(def ^:private broken
  (gen/let [W    gn/world
            hits (gen/vector (gen/tuple (gen/elements [:rekey :bad-id :shape :unknown :not-map])
                                        gen/nat)
                             0 3)]
    (reduce break W hits)))

(defn- structure-of [d]
  (frequencies (map #(dissoc % :gate) (gate/report-of (structure/rows world-gates/checks) d))))

(defspec the-kernel-judges-structure-as-the-platform 300
  (prop/for-all [W broken]
    (= (frequencies (problems/structural-findings W))
       (structure-of (db/db W structure-views)))))

(defspec a-structure-step-is-the-rebuild 300
  (prop/for-all [W broken hits (gen/vector (gen/tuple (gen/elements [:rekey :bad-id :shape])
                                                      gen/nat)
                                           1 3)]
    (let [W1 (reduce break W hits)
          d1 (db/db-with (db/db W structure-views) (kg/diff W W1))]
      (= (db/view (db/db W1 structure-views) :findings) (db/view d1 :findings)))))

(defn- accepted-structure? [d]
  (let [gs (structure/rows world-gates/checks)]
    (and (every? #(empty? (get (db/view d :findings) (:id %))) (filter :silent gs))
         (every? #(empty? ((:frame %) d)) (filter :frame gs)))))

(defspec the-structure-is-accepted-as-the-platform-accepts-it 300
  (prop/for-all [W broken]
    (= (cstructure/valid-schema-structure? W)
       (accepted-structure? (db/db W structure-views)))))

(defn- stepped
  [db0 W0 W1]
  (assert (= W0 (db/world db0)))
  (log/stepped db0 W1))

(defn- verdict-of [v]
  {:ok?      (contains? v :ok)
   :findings (frequencies (:findings v))})

(defspec the-verdict-is-the-references 200
  (prop/for-all [W gn/world cs (gen/vector gn/change 1 4)]
    (let [W1 (reduce gn/apply-change W cs)]
      (or (not (and (gn/oracle-defined? W) (gn/oracle-defined? W1)))
          (let [d0 (db/db W views)
                d1 (stepped d0 W W1)]
            (= (verdict-of (kernel/verdict W1 W))
               (verdict-of (world-gates/verdict d0 d1))))))))

(defn- reasons-cut [r]
  (into {} (for [[k v] r] [k (if (sequential? v) (frequencies v) v)])))

(defspec a-previews-reasons-are-the-references 200
  (prop/for-all [W gn/world cs (gen/vector gn/change 1 4)]
    (let [W1 (reduce gn/apply-change W cs)]
      (or (not (and (gn/oracle-defined? W) (gn/oracle-defined? W1)))
          (let [d0 (db/db W views)
                d1 (stepped d0 W W1)]
            (and (= (reasons-cut (kernel/reasons W)) (reasons-cut (kept/value-reasons d0)))
                 (= (reasons-cut (kernel/reasons W1)) (reasons-cut (kept/value-reasons d1)))))))))

(def ^:private installed-change
  (gen/let [W0 kg/world steps (gen/vector kg/step 1 3)]
    (let [cx (kg/sigma "w")]
      [W0 (reduce (fn [W st] (or (kg/install cx W st) W)) W0 steps)])))

(defspec the-verdict-on-installs-is-the-references 200
  (prop/for-all [[W W1] installed-change]
    (or (not (and (kg/oracle-defined? W) (kg/oracle-defined? W1)))
        (let [d0 (db/db W views)
              d1 (stepped d0 W W1)]
          (= (verdict-of (kernel/verdict W1 W))
             (verdict-of (world-gates/verdict d0 d1)))))))

(deftest the-verdicts-accept-and-refuse
  (let [vs (for [[W W1] (gen/sample installed-change 120)
                 :when (and (kg/oracle-defined? W) (kg/oracle-defined? W1))]
             (contains? (kernel/verdict W1 W) :ok))]
    (is (some true? vs))
    (is (some false? vs))))

(deftest the-broken-worlds-reach-every-structural-fault
  (let [rows (mapcat problems/structural-findings (gen/sample broken 200))]
    (doseq [k [:section-error :key-error :id-error :shape-error]]
      (is (some k rows) (str k)))))

