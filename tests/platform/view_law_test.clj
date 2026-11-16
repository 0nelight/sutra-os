(ns ^:unit platform.view-law-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [platform.world-advice-test :as advice]
            [platform.world-columns-test :as columns]
            [platform.world-derived-test :as derived]
            [platform.sig-view-test :as sig]
            [sutra.catalog.world-advice :as ka]
            [sutra.catalog.world-columns :as kc]
            [sutra.catalog.world-derived :as kd]
            [sutra.catalog.world-export :as world-export]
            [sutra.catalog.world-install :as world-install]
            [sutra.catalog.world-schema-index :as world-schema-index]
            [sutra.catalog.link-ends :as link-ends]
            [sutra.world.db :as db]
            [sutra.world.gate.near :as n]
            [sutra.world.gen.near :as gn]
            [sutra.world.gen.space :as gs]
            [sutra.commits.log :as log]))

(defn- histories
  ([g-world g-change] (histories g-world g-change gn/apply-change))
  ([g-world g-change apply]
   (gen/let [W   g-world
             css (gen/vector (gen/vector g-change 1 3) 1 4)]
     (vec (reductions (fn [W cs] (reduce apply W cs)) W css)))))

(def ^:private isolated :advice.object-type-isolated/object-types)
(def ^:private visible [[:gate-facts :visible] [:visible]])

(defn- witnessed
  [d]
  (into {} (for [[k {:keys [facts nodes]}] (get-in d [:views :near :reads isolated])
                 :when (not (contains? facts visible))]
             [k (set (for [[s w] nodes :when (and (= :object-types s) (not= w k))] w))])))

(defn- witness?
  [d k w]
  (and (not= w k)
       (contains? (set (n/lookup d visible)) [:object-types w])
       (empty? (remove #(= :interfaces (first %))
                       (mapcat #(n/lookup d %) (#'ka/log-facts d w))))))

(def ^:private witness-law
  {:without (fn [vs] (cond-> vs (:near vs) (update :near update-vals #(dissoc % isolated))))
   :holds?  (fn [d1 di]
              (let [w1 (witnessed d1)]
                (and (= (set (keys (get-in d1 [:views :near :reads isolated])))
                        (set (keys (get-in di [:views :near :reads isolated]))))
                     (= (set (keys w1)) (set (keys (witnessed di))))
                     (every? (fn [[k ws]] (some #(witness? di k %) ws)) w1))))})

(def ^:private register
  {:world-columns {:rows kc/rows :worlds (histories gn/world gn/change)}
   :graph        {:rows   kc/rows
                  :worlds (histories columns/g-graph columns/g-graph-change columns/changed)}
   :world-derived {:rows   kd/rows
                  :worlds (gen/fmap (fn [[W cs]]
                                      (vec (take-while #(not= ::derived/threw %)
                                                       (reductions derived/next-world W cs))))
                                    derived/g-history)}
   :signature    {:rows   [kd/signature]
                  :worlds (histories (gen/let [W gn/world h0 (gen/vector sig/g-query-app 0 2)]
                                       (reduce gn/apply-change W h0))
                                     sig/g-change)}
   :world-advice  {:rows   ka/views
                  :chosen witness-law
                  :worlds (gen/fmap #(vec (take-while advice/defined? %))
                                    (histories advice/world advice/change))}
   :world-install {:rows [world-install/view] :worlds (histories gn/world gn/change)}
   :loop-state   {:rows   [kc/loop-state]
                  :worlds (histories columns/g-graph columns/g-graph-change columns/changed)}
   :link-ends    {:rows [link-ends/view] :worlds (histories gn/world gn/change)}
   :schema-index {:rows [world-schema-index/view] :worlds (histories gn/world gn/change)}
   :schema-index-spaces {:rows   [world-schema-index/view]
                         :worlds (histories gs/world gs/change gs/apply-change)}
   :world-export  {:rows world-export/rows :worlds (histories gn/world gn/change)}})

(defn- outcome [f] (try (f) (catch Exception _ ::threw)))

(defn- views [rows {:keys [without] :or {without identity}} d]
  (if (= ::threw d) d (without (select-keys (:views d) (map :id rows)))))

(defn- lawful?
  [rows chosen Ws]
  (let [[W & more] Ws
        holds?     (or (:holds? chosen) (constantly true))]
    (loop [d (outcome #(db/db W rows)) [W1 & more] more]
      (cond
        (= ::threw d) true
        (nil? W1)     true
        :else (let [d1 (outcome #(log/stepped d W1))
                    di (outcome #(db/db W1 rows))]
                (and (= (views rows chosen d1) (views rows chosen di))
                     (or (= ::threw d1) (holds? d1 di))
                     (recur d1 more)))))))

(deftest a-kept-witness-an-action-starts-to-log-to-is-replaced
  (let [ot (fn [k] {:id (str "ri.sutra.main.object-type." k) :object-type-name (keyword k)
                    :attribute-list [{:attribute-name :x :attribute-type :string}]})
        W0 {:object-types {"ri.sutra.main.object-type.a" (ot "a")
                           "ri.sutra.main.object-type.b" (ot "b")}}
        W1 (assoc W0 :actions {"ri.sutra.main.action.z"
                               {:id "ri.sutra.main.action.z" :action-name :z
                                :action-log {:object-type :b} :params {} :rules []}})]
    (is (lawful? ka/views witness-law [W0 W1]))))

(defspec every-stepped-view-is-its-init 2000
  (prop/for-all [[id Ws] (gen/bind (gen/elements (keys register))
                                   (fn [id] (gen/tuple (gen/return id)
                                                       (:worlds (register id)))))]
    (let [{:keys [rows chosen]} (register id)]
      (or (empty? Ws) (lawful? rows chosen Ws)))))
