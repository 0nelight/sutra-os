(ns sutra.service.access-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [sutra.service.rights-test :as rt]
            [sutra.service.access :as z]
            [sutra.world.db :as db]
            [sutra.world.gen :as kg]
            [sutra.world.guard :as guard]
            [sutra.world.policy :as p]
            [sutra.types.authority :as policy]))

(def ^:private roles-after
  (gen/elements [rt/roles
                 rt/roles
                 (assoc-in rt/roles [:custom :operations] #{:export :delete})
                 (assoc-in rt/roles [:ontology-viewer :operations] #{})
                 (dissoc rt/roles :consumer)]))

(defn- policy-of [W roles]
  (policy/schema-database (policy/schema-facts W) (policy/role-facts roles)))

(defn- step [W W1]
  (db/with (guard/guarded (db/db W p/rows)) (kg/diff W W1)))

(defn- both [W W1 roles1]
  (let [b (policy-of W rt/roles) a (policy-of W1 roles1) s (step W W1)
        dl (policy/access-delta b a)
        dz (z/access-delta s rt/roles roles1)]
    {:oracle {:delta dl :lifted (policy/lifted dl b) :applied (policy/applied b a)}
     :local  {:delta dz :lifted (z/lifted dz (:db-before s)) :applied (z/applied s)}}))

(defspec the-access-a-step-moves-equals-the-policies-compared 300
  (prop/for-all [W rt/world es (gen/vector rt/edit 1 4) roles1 roles-after]
    (let [{:keys [oracle local]} (both W (reduce rt/edited W es) roles1)]
      (= oracle local))))

(deftest the-worlds-reach-every-relation
  (let [ds (for [[W es roles1] (gen/sample (gen/tuple rt/world (gen/vector rt/edit 1 4)
                                                      roles-after)
                                           200)]
             (:delta (:oracle (both W (reduce rt/edited W es) roles1))))
        rels (frequencies (for [d ds side [:expands :restricts] f (get d side)]
                            [side (:relation f)]))]
    (is (every? #(< 5 (get rels % 0))
                (for [side [:expands :restricts] rel [:holds :marked :walled :requires]]
                  [side rel]))
        (pr-str rels))))
