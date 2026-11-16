(ns ^:unit platform.link-ends-test
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [sutra.catalog.interfaces :as interfaces]
            [sutra.catalog.link-ends :as link-ends]
            [sutra.world.gen.near :as gn]
            [sutra.types.schema :as sch]))

(defn- old-filter
  [W T]
  (into #{} (keep (fn [[L ct]]
                    (when (or (interfaces/satisfies-endpoint? W T (:from-object-type ct))
                              (interfaces/satisfies-endpoint? W T (:to-object-type ct)))
                      L)))
        (:link-types W)))

(defn- answer [f] (try (f) (catch Exception e [:threw (ex-message e)])))

(defspec reaching-is-the-old-filter 500
  (prop/for-all [W gn/world cs (gen/vector gn/change 0 3)]
    (let [W  (sch/indexed (reduce gn/apply-change W cs))
          ts (concat (keys (:object-types W)) (keys (:interfaces W)) [:none])]
      (every? #(= (answer (fn [] (old-filter W %))) (answer (fn [] (link-ends/reaching W %))))
              ts))))
