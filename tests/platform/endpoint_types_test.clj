(ns ^:unit platform.endpoint-types-test
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [sutra.catalog.interfaces :as interfaces]
            [sutra.world.gen.near :as gn]
            [sutra.objects.views :as views]
            [sutra.types.rid :as rid]))

(defn- old-endpoint-types
  [W]
  (let [ots (keys (:object-types W))]
    (into {}
          (for [[cn ct] (:link-types W)
                :let [from-ep (:from-object-type ct)
                      to-ep   (:to-object-type ct)]]
            [cn {:from (into #{} (filter #(interfaces/satisfies-endpoint? W % from-ep)) ots)
                 :to   (into #{} (filter #(interfaces/satisfies-endpoint? W % to-ep)) ots)}]))))

(defn- answer [f] (try (f) (catch Exception e [:threw (ex-message e)])))

(defn- same? [W]
  (= (answer #(old-endpoint-types W)) (answer #(views/endpoint-types W))))

(defspec endpoint-types-are-the-cross-product 500
  (prop/for-all [W gn/world cs (gen/vector gn/change 0 3)]
    (let [W (reduce gn/apply-change W cs)]
      (and (same? W)
           (or (not (rid/rid-keyed? W)) (same? (rid/name-keyed W)))))))
