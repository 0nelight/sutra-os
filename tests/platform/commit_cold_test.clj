(ns ^:unit platform.commit-cold-test
  (:require [clojure.test :refer [deftest is]]
            [sutra.world.delta :as d]
            [sutra.ref.clock :as clock]
            [sutra.ref.install :as ri]
            [sutra.store.core.orakel :as orakel]
            [sutra.store.mem :as mem]
            [sutra.store.storage.heads :as heads]
            [sutra.store.world :as world]
            [sutra.types.world-rules :as rules]))

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

(defn- ot [nm & attrs]
  [(ri/rid (ri/tag-of :object-types) (name nm))
   {:object-type-name nm :plural (str (name nm) "s")
    :attribute-list (vec (for [a attrs] {:attribute-name a :attribute-type :string}))}])

(def ^:private worlds
  (let [a (ot :a :nr)]
    [{:object-types (into {} [a])}
     {:object-types (into {} [a (ot :b :nr)])}
     {:object-types (into {} [a (ot :b :nr :stufe)])}
     {:object-types (into {} [a (ot :b :nr :stufe) (ot :c :nr)])}
     {:object-types (into {} [a (ot :c :nr)])}
     {:object-types (into {} [a (ot :c :nr) (ot :d :nr)])}
     {:object-types (into {} [a])}]))

(defn- counting [opts n]
  (-> opts
      (update :derive (fn [f] (fn [W] (swap! n update :derive inc) (f W))))
      (update-in [:sig-view :init] (fn [f] (fn [W] (swap! n update :sig-init inc) (f W))))))

(def ^:private zero {:derive 0 :sig-init 0 :view-init 0})

(defn- step! [s derive W]
  (world/commit! s {:changes (changes (world/authored s) (derive W)) :at-head? true
                    :actor {:principal "test"}}))

(defn- run
  [oracle?]
  (let [n      (atom zero)
        init   rules/sig-init]
    (with-redefs [orakel/on?     oracle?
                  rules/sig-init (fn [W] (swap! n update :view-init inc) (init W))]
      (let [opts   (merge (rules/store-opts clock/system) heads/store-opts)
            derive (:derive opts)
            a      (mem/store {:world (derive (first worlds))} (counting opts n))
            b      (assoc a :memo (atom {}))
            sa     (world/store a "main" {})
            sb     (world/store b "main" {})]
        (step! sa derive (nth worlds 1))
        (step! sb derive (nth worlds 2))
        (vec (for [[x y] (partition 2 (drop 3 worlds))]
               (do (step! sa derive x)
                   (reset! n zero)
                   (step! sb derive y)
                   (is (= (derive y) (world/authored sb)))
                   @n)))))))

(deftest a-cold-head-steps-from-the-loaders-db
  (is (every? #{zero} (run false))
      "a commit on a head another process left derives nothing and builds no view whole"))

(deftest a-cold-head-holds-against-the-whole-computation
  (is (seq (run true))))
