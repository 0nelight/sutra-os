(ns sutra.world.near-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [sutra.world.db :as db]
            [sutra.world.gate :as gate]
            [sutra.world.gen :as g]
            [sutra.world.gen.near :as gn]
            [sutra.world.guard :as guard]
            [sutra.world.view :as view]))

(def ^:private checks
  (into {:media? #{:media-reference}}
        (map (fn [s] [(:check s) (fn [W k] [{:world W :key k}])]))
        gate/near-specs))

(def ^:private views (into view/rows (gate/views-with checks)))

(def ^:private pair
  (gen/let [W gn/world cs (gen/vector gn/change 1 6)]
    [W (reduce gn/apply-change W cs)]))

(defn- stepped [W W1]
  (db/db-with (guard/guarded (db/db W views)) (g/diff W W1)))

(defspec stepping-equals-building-afresh 300
  (prop/for-all [[W W1] pair]
    (let [d1 (stepped W W1) fresh (db/db (guard/unguarded (:ents d1)) views)]
      (and (= (db/view d1 :near) (db/view fresh :near))
           (= (db/view d1 :findings) (db/view fresh :findings))))))

(deftest a-step-moves-only-what-it-reads
  (let [l  (gn/rid-of :link-types :a)
        t  (gn/rid-of :object-types :a)
        u  (gn/rid-of :object-types :b)
        W  {:link-types   {l {:id l :link-type-name :a :from-object-type t :to-object-type t}}
            :object-types {t {:id t :object-type-name :a} u {:id u :object-type-name :b}}}
        d  (db/db W views)
        ks (fn [W1 id] (set ((:keys (some #(when (= id (:id %)) %) (gate/rows-with checks)))
                             d (db/db-with d (g/diff W W1)) (g/diff W W1))))]
    (is (= #{l} (ks (assoc-in W [:object-types t :attribute-list] [{:attribute-name :x}])
                    :link-types-valid?))
        "an end of the link moves the link")
    (is (= #{} (ks (assoc-in W [:object-types u :attribute-list] [{:attribute-name :x}])
                   :link-types-valid?))
        "a type the link does not read moves nothing")))
