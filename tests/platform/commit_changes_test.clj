(ns ^:unit platform.commit-changes-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [sutra.world.delta :as d]
            [sutra.store.core.resources :as res]))

(def ^:private g-entry
  (gen/one-of [(gen/return {})
               (gen/map (gen/elements [:x :y :z]) (gen/elements [0 1 2]) {:max-elements 3})]))

(def ^:private g-section
  (gen/map (gen/elements ["k1" "k2" "k3" "k4"]) g-entry {:max-elements 4}))

(def ^:private g-world
  (gen/let [m (gen/map (gen/elements [:object-types :datasets :actions]) g-section
                       {:max-elements 3})
            v (gen/elements [nil 0 1 [:a]])]
    (cond-> m (some? v) (assoc :settings v))))

(defn- changes
  [W0 W1]
  (d/completed W0
               (d/around (d/frame-delta W0 W1)
                         (vec (for [s (distinct (concat (keys W0) (keys W1)))
                                    :when (or (map? (get W0 s)) (map? (get W1 s)))
                                    k (distinct (concat (keys (get W0 s)) (keys (get W1 s))))
                                    c (d/entry-delta [s k] (get-in W0 [s k])
                                                     (get-in W1 [s k]))]
                                c)))))

(defn- effects [es0 es heads]
  (res/commit-effects "b" 7 {:kept {:since 1 :n 0} :es0 es0 :es es :heads heads}))

(defspec changes-keep-what-the-whole-commit-keeps 300
  (prop/for-all [W0 g-world W1 g-world]
    (let [cs    (changes W0 W1)
          W     (d/apply-to W0 cs)
          ids   (res/ids-of (d/nodes cs))
          full0 (res/entries W0)
          full  (res/entries W)
          heads (zipmap (res/changed full0 full) (repeat 3))]
      (and (= (res/entries W1) full)
           (= (set (res/changed full0 full))
              (set (res/changed (res/only W0 ids) (res/only W ids))))
           (= (effects full0 full heads)
              (effects (res/only W0 ids) (res/only W ids) heads))))))

(deftest a-stale-change-is-refused
  (let [cs (changes {:datasets {"k1" {:x 0}}} {:datasets {"k1" {:x 1}}})]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"another value"
                          (d/apply-to {:datasets {"k1" {:x 2}}} cs)))
    (is (= {:datasets {"k1" {:x 1} "k2" {:y 0}}}
           (d/apply-to {:datasets {"k1" {:x 0} "k2" {:y 0}}} cs)))))
