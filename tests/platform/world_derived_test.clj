(ns ^:unit platform.world-derived-test
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [sutra.capps.refs :as app-refs]
            [sutra.capps.variables :as vars]
            [sutra.catalog.world-derived :as kd]
            [sutra.world.db :as db]
            [sutra.world.gen.near :as gn]
            [sutra.ref.install :as ri]
            [sutra.commits.log :as log]))

(defn- derive* [W]
  (try (ri/derived {} W nil) (catch Exception _ ::threw)))

(def ^:private g-old-layout
  (gen/let [n (gen/elements [:a :b :c :v]) url? gen/boolean]
    {:vars     {:v (cond-> {:type :string} url? (assoc :url? true))}
     :children [{:from n}]}))

(def ^:private g-app-change
  (gen/let [n (gen/elements [:a :b :c])
            fv (gen/one-of [(gen/fmap (fn [l] {:layout l}) g-old-layout)
                            (gen/return {:form-factor :mobile})
                            (gen/fmap (fn [t] {:object-type (gn/rid-of :object-types t)})
                                      (gen/elements [:a :b :c]))])]
    [[:applications (gn/rid-of :applications n)] fv]))

(def ^:private g-change (gen/frequency [[3 gn/change] [1 g-app-change]]))

(def g-history
  (gen/let [W   gn/world
            d0? gen/boolean
            cs  (gen/vector (gen/tuple (gen/vector g-change 1 3) gen/boolean) 1 4)]
    [(if d0? (derive* W) W) cs]))

(defn next-world [W [cs d?]]
  (if (= ::threw W)
    W
    (let [W1 (reduce gn/apply-change W cs)] (if d? (derive* W1) W1))))

(defspec fixed-is-whether-the-world-is-its-own-derivation 500
  (prop/for-all [[W cs] g-history]
    (let [Ws (reductions next-world W cs)]
      (every? (fn [W]
                (or (= ::threw W)
                    (= (empty? (kd/unfixed (db/db W kd/rows))) (= W (derive* W)))))
              Ws))))

(defn- readable-apps [W]
  (:applications (app-refs/lift-schema-applications (vars/lift-schema-applications W))))

(defspec lifted-is-what-readable-lifts-in-a-derived-world 500
  (prop/for-all [[W cs] g-history]
    (every? (fn [W]
              (let [d (when (and (map? W) (map? (:applications W))) (db/db W kd/rows))]
                (or (nil? d)
                    (seq (kd/unfixed d))
                    (= (readable-apps W) (merge (:applications W) (kd/lifted-apps d))))))
            (reductions next-world W cs))))

(defspec a-stepped-world-keeps-what-it-did-not-change 300
  (prop/for-all [[W cs] g-history]
    (let [W1 (next-world W (first cs))]
      (or (= ::threw W) (= ::threw W1)
          (let [W' (db/world (log/stepped (db/db W kd/rows) W1))
                kept? (fn [x x1 x'] (or (not= x x1) (identical? x x')))]
            (every? (fn [[s es]]
                      (and (kept? es (get W1 s) (get W' s))
                           (or (not (map? es))
                               (every? (fn [[k e]] (kept? e (get-in W1 [s k]) (get-in W' [s k])))
                                       es))))
                    W))))))
