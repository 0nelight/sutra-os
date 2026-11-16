(ns sutra.world.gate-space-test
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [sutra.world.db :as db]
            [sutra.world.gate :as gate]
            [sutra.world.gen :as g]
            [sutra.world.gen.space :as gs]
            [sutra.world.view :as view]))

(def media? #{:media-reference :attachment})

(def views (into view/rows (gate/views-with {:media? media?})))

(def ^:private pair
  (gen/let [W gs/world cs (gen/vector gs/change 1 6)]
    [W (reduce gs/apply-change W cs)]))

(defspec stepping-equals-building-afresh 300
  (prop/for-all [[W W1] pair]
    (let [{db1 :db-after} (db/with (db/db W views) (g/diff W W1))]
      (= (:views db1) (:views (db/db (:ents db1) views))))))
