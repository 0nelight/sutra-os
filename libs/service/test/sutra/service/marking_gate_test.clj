(ns sutra.service.marking-gate-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [sutra.service.rights-test :as rt]
            [sutra.world.db :as db]
            [sutra.world.gate :as gate]
            [sutra.world.gen :as kg]
            [sutra.world.guard :as guard]
            [sutra.types.authority :as policy]))

(defn- of-world [W] (db/db W gate/views))

(defn- kern [d id] (frequencies (mapcat val (get (db/view d :findings) id))))

(defn- both [W d]
  {:constraints    [(frequencies (policy/constraint-violations W))
                    (kern d :marking-constraints-met?)]
   :classification [(frequencies (policy/unclassified-projects W))
                    (kern d :project-classification-met?)]})

(defspec the-marking-gates-find-what-the-policy-finds 300
  (prop/for-all [W rt/world]
    (every? (fn [[a b]] (= a b)) (vals (both W (of-world W))))))

(defn- judged-alike [guard W es]
  (let [W1 (reduce rt/edited W es)
        d1 (db/db-with (guard (of-world W)) (kg/diff W W1))]
    (= (db/view (of-world W1) :findings) (db/view d1 :findings))))

(defspec a-step-judges-as-a-fresh-build 300
  (prop/for-all [W rt/world es (gen/vector rt/edit 1 4)]
    (judged-alike identity W es)))

(def ^:private resource-edit
  (gen/such-that (fn [[[sec]]] (not= :markings sec)) rt/edit 100))

(defspec a-step-off-the-markings-reads-only-through-keys-and-indexes 300
  (prop/for-all [W rt/world es (gen/vector resource-edit 1 4)]
    (judged-alike guard/guarded W es)))

(deftest the-worlds-have-findings
  (let [ws (gen/sample rt/world 200)
        n  (fn [f] (count (filter #(seq (f %)) ws)))]
    (is (< 20 (n policy/constraint-violations)))
    (is (< 20 (n policy/unclassified-projects)))))
