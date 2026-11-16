(ns sutra.world.law-test
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [malli.core :as m]
            [sutra.world.db :as db]
            [sutra.world.gate :as gate]
            [sutra.world.gen :as g]
            [sutra.world.index :as ix]
            [sutra.world.install :as k]
            [sutra.world.schema :as schema]
            [sutra.ref.install :as ri]))

(defn norm-findings
  [fs]
  (frequencies (map #(cond-> % (:rids %) (update :rids (fn [rs] (sort-by str rs)))) fs)))

(defn- stepped-equals-fresh? [W W1]
  (let [{db1 :db-after} (db/with (k/of-world W) (g/diff W W1))
        fresh           (k/of-world (:ents db1))]
    (and (= W1 (:ents db1))
         (= (:idx db1) (ix/init (:ents db1)))
         (= (:views db1) (:views fresh))
         (m/validate schema/Db db1))))

(def ^:private installed-pair
  (gen/let [W g/world s g/step]
    [W (or (g/install (g/sigma "s") W s) W)]))

(def ^:private raw-pair
  (gen/let [W g/world cs (gen/vector g/raw-change 1 6)]
    [W (reduce g/apply-raw W cs)]))

(defspec stepping-an-install-equals-building-afresh 150
  (prop/for-all [[W W1] installed-pair]
    (stepped-equals-fresh? W W1)))

(defspec stepping-a-raw-edit-equals-building-afresh 200
  (prop/for-all [[W W1] raw-pair]
    (stepped-equals-fresh? W W1)))

(def stated
  (filterv #(contains? ri/stated-gates (:gate % (:id %))) gate/rows))

(defspec findings-equal-the-reference-report 200
  (prop/for-all [[_ W] (gen/one-of [installed-pair raw-pair])]
    (or (not (g/oracle-defined? W))
        (= (norm-findings (ri/report (g/sigma "r") W nil))
           (norm-findings (gate/report-of stated (k/of-world W)))))))
