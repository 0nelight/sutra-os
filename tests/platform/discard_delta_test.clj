(ns ^:unit platform.discard-delta-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [sutra.catalog.world-gates :as world-gates]
            [sutra.catalog.kernel :as kernel]
            [sutra.commits.log :as log]
            [sutra.catalog.system-datasets :as sysds]
            [sutra.world.db :as kdb]
            [sutra.world.gen :as kg]
            [sutra.world.install :as k]
            [sutra.ref.install :as ri]
            [sutra.store.kept :as kept]
            [sutra.types.rid :as rid]))

(defn- cx [prefix]
  (let [n (atom 0)]
    (assoc-in kernel/install-cx [:sigma :fresh-uuid] #(str prefix "-" (swap! n inc)))))

(defn- stepped
  [cx d [verb tag body]]
  (try (:db (k/run cx d (ri/build cx verb tag body)))
       (catch clojure.lang.ExceptionInfo _ nil)))

(defn- based
  [W0]
  (k/of-world (update W0 :datasets merge sysds/system-datasets)))

(defn- accepted?
  [d0 d1]
  (let [v0 (kdb/db (kdb/world d0) world-gates/views)]
    (contains? (world-gates/verdict v0 (log/stepped v0 (kdb/world d1))) :ok)))

(defn- staged
  [d steps]
  (let [c (cx "s")]
    (reduce (fn [{:keys [head rows]} st]
              (if-let [d1 (some-> (stepped c head st) (as-> d1 (when (accepted? head d1) d1)))]
                {:head d1
                 :rows (conj rows {:seq (count rows) :step st :args (nth st 2) :tx-vector []
                                   :delta (vec (drop (count (:delta head)) (:delta d1)))})}
                {:head head :rows rows}))
            {:head d :rows []} steps)))

(defn- code-of [e] (let [d (ex-data e)] (or (:refusal/code d) (:ref/kind d) :refused)))

(defn- replayed
  [prefix d rows]
  (let [c (cx prefix)]
    (reduce (fn [d {[verb tag body] :step}]
              (try (:db (k/run c d (ri/build c verb tag body)))
                   (catch clojure.lang.ExceptionInfo e (reduced {:refused (code-of e)}))))
            d rows)))

(defn- refused? [x] (and (map? x) (contains? x :refused)))

(defn- landed
  [p d]
  (if (refused? d)
    d
    (if (accepted? p d) d {:refused :refusal.catalog/plan-problems})))

(defn- discarded
  [p candidate old]
  (cond (refused? candidate)                candidate
        (refused? (landed p candidate))     old
        :else                               candidate))

(defn- outcome
  [W0 steps i]
  (let [d0               (based W0)
        {:keys [head rows]} (staged d0 steps)
        k                (mod i (count rows))
        old              (landed d0 (replayed "o" d0 (into (subvec rows 0 k)
                                                               (subvec rows (inc k)))))
        {:keys [form db replay] :as p} (kept/discarding head rows k)
        new              (case form
                           :inverse (discarded d0 db old)
                           :suffix  (discarded d0 (replayed "n" db replay) old)
                           :rebuild old)]
    [d0 old new p]))

(def ^:private tagged #"ri\.sutra\.main\.([^.]+)\..*")

(defn- normal
  [W0 W]
  (let [old   (set (for [[_ es] W0 :when (map? es) k (keys es)] k))
        fresh #(and (rid/rid? %) (not (old %)))
        named (into {} (for [[s es] W :when (map? es)
                             [k e] es :when (fresh k)]
                         [k [::minted s (some-> (get e (rid/name-field-of s)) name)]]))
        rename (fn [x]
                 (walk/postwalk (fn [x]
                                  (or (get named x)
                                      (when (and (string? x) (fresh x))
                                        [::dangling (second (re-matches tagged x))])
                                      x))
                                x))]
    (into {} (for [[s es] W]
               [s (if (map? es) (frequencies (map rename (vals es))) (rename es))]))))

(defn- same? [d0 a b]
  (let [W0 (kdb/world d0)] (= (normal W0 (kdb/world a)) (normal W0 (kdb/world b)))))

(def ^:private stacks
  (gen/let [W0 kg/world steps (gen/vector kg/step 2 7) i gen/nat]
    [W0 steps i]))

(defn- staged-any? [W0 steps] (seq (:rows (staged (based W0) steps))))

(defspec a-discard-leaves-what-the-replay-leaves 1000
  (prop/for-all [[W0 steps i] stacks]
    (or (not (staged-any? W0 steps))
        (let [[d0 old new] (outcome W0 steps i)]
          (if (refused? old)
            (= old new)
            (and (not (refused? new))
                 (same? d0 old new)))))))

(deftest the-stacks-reach-every-form
  (let [forms (frequencies
               (for [[W0 steps i] (gen/sample stacks 400)
                     :when (staged-any? W0 steps)
                     :let [[_ _ _ p] (outcome W0 steps i)]]
                 [(:form p) (:why p)]))]
    (is (pos? (get forms [:inverse nil] 0)) (pr-str forms))
    (is (pos? (get forms [:suffix nil] 0)) (pr-str forms))))

(def ^:private row
  {:seq 0 :tx-vector []
   :delta [[[:object-types "x"] :object-type-name :sutra.kern.delta/absent :x]]})

(deftest a-row-that-cannot-be-turned-back-asks-for-the-batch-anew
  (let [d (kdb/db {:object-types {"x" {:object-type-name :x}}})]
    (testing "a data row: its prior values are not kept"
      (is (= {:form :rebuild :why :data-row :seq 0}
             (kept/discarding d [(assoc row :data? true)] 0))))
    (testing "a row staged before rows carried their changes"
      (is (= {:form :rebuild :why :no-delta :seq 0}
             (kept/discarding d [(dissoc row :delta)] 0))))
    (testing "a later data row that writes what the discarded row made"
      (is (= {:form :rebuild :why :data-row :seq 1}
             (kept/discarding d [row {:seq 1 :data? true :delta []
                                      :tx-vector [{:op :create-object :type :x}]}]
                              0))))
    (testing "a later data row that writes something else stays"
      (is (= :inverse
             (:form (kept/discarding d [row {:seq 1 :data? true :delta []
                                             :tx-vector [{:op :create-object :type :y}]}]
                                     0)))))
    (testing "the head moved at the row's elements since"
      (is (= {:form :rebuild :why :stale :seq 0}
             (kept/discarding (kdb/db {:object-types {"x" {:object-type-name :z}}}) [row] 0))))))

(defn- moved
  [d0 p]
  (let [ns (sutra.world.delta/nodes (vec (drop (count (:delta d0)) (:delta p))))]
    (vec (kdb/changes-to d0 (into {} (keep (fn [n] (let [v (kdb/value p n)]
                                                     (when (not= v (kdb/value d0 n))
                                                       [n v]))))
                                  ns)))))

(defn- rebase-outcome
  [W0 steps psteps i]
  (let [d0                  (based W0)
        {:keys [head rows]} (staged d0 steps)
        _                   head
        k                   (mod i (count rows))
        c                   (cx "p")
        p                   (reduce (fn [d st] (or (stepped c d st) d)) d0 psteps)
        old                 (landed p (replayed "o" p (into (subvec rows 0 k)
                                                              (subvec rows (inc k)))))
        {:keys [form db replay] :as plan} (kept/rebasing p (moved d0 p) rows k)
        new                 (case form
                              :rebase  (discarded p (replayed "n" db replay) old)
                              :rebuild old)]
    [d0 old new plan]))

(def ^:private moved-stacks
  (gen/let [W0 kg/world steps (gen/vector kg/step 2 6) psteps (gen/vector kg/step 0 4)
            i gen/nat]
    [W0 steps psteps i]))

(defspec a-discard-onto-a-moved-parent-leaves-what-the-replay-leaves 1000
  (prop/for-all [[W0 steps psteps i] moved-stacks]
    (or (not (staged-any? W0 steps))
        (let [[d0 old new] (rebase-outcome W0 steps psteps i)]
          (if (refused? old)
            (= old new)
            (and (not (refused? new)) (same? d0 old new)))))))

(deftest a-moved-parent-reaches-every-way
  (let [ways (frequencies
              (for [[W0 steps psteps i] (gen/sample moved-stacks 400)
                    :when (staged-any? W0 steps)
                    :let [[_ _ _ p] (rebase-outcome W0 steps psteps i)]]
                [(:form p) (if (seq (:replay p)) :replays :lays-on)]))]
    (is (pos? (get ways [:rebase :lays-on] 0)) (pr-str ways))
    (is (pos? (get ways [:rebase :replays] 0)) (pr-str ways))))

(deftest a-parent-that-moved-nothing-costs-no-row
  (let [d (kdb/db {:object-types {"x" {:object-type-name :x}}})
        y {:seq 1 :tx-vector []
           :delta [[[:object-types "y"] :object-type-name :sutra.kern.delta/absent :y]]}]
    (is (= {:form :rebase :replay []}
           (dissoc (kept/rebasing d [] [row y] 0) :db))
        "a data commit on the parent: the other rows take their changes, none runs again")
    (is (= {:form :rebuild :why :no-delta :seq 1}
           (kept/rebasing d [] [row (dissoc y :delta)] 0)))))
