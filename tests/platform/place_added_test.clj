(ns ^:unit platform.place-added-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [sutra.world.gen :as kg]
            [sutra.types.forms :as forms]
            [sutra.types.membership :as member]))

(defn- nodes [W]
  (set (for [[s es] W :when (map? es) k (keys es)] [s k])))

(def ^:private installed-change
  (gen/let [W0 kg/world steps (gen/vector kg/step 1 3) i gen/nat]
    (let [cx (kg/sigma "w")
          W1 (reduce (fn [W st] (or (kg/install cx W st) W)) W0 steps)
          ps (sort (keys (:projects W1)))]
      [W0 W1 (when (seq ps) (nth ps (mod i (count ps))))])))

(defspec placing-the-added-is-placing-the-new 300
  (prop/for-all [[W0 W1 p] installed-change]
    (or (nil? p)
        (= (member/place-new forms/installed W0 W1 p)
           (member/place-added forms/installed W1 p
                               (remove (nodes W0) (nodes W1)))))))

(deftest the-worlds-place-something
  (is (some (fn [[W0 W1 p]]
              (and p (not= W1 (member/place-new forms/installed W0 W1 p))))
            (gen/sample installed-change 200))))
