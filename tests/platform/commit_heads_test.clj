(ns ^:unit platform.commit-heads-test
  (:require [clojure.test :refer [deftest is]]
            [sutra.world.delta :as d]
            [sutra.ref.clock :as clock]
            [sutra.ref.install :as ri]
            [sutra.store.api :as api]
            [sutra.store.core.orakel :as orakel]
            [sutra.store.mem :as mem]
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
     {:object-types (into {} [a])}]))

(defn- counting [opts n]
  (-> opts
      (update :derive (fn [f] (fn [W] (swap! n update :derive inc) (f W))))
      (update-in [:sig-view :init] (fn [f] (fn [W] (swap! n update :sig-init inc) (f W))))))

(defn- commits
  [s n derive]
  (vec (for [W1 (rest worlds)]
         (let [cs (changes (world/authored s) (derive W1))]
           (reset! n {:derive 0 :sig-init 0})
           (world/commit! s {:changes cs :at-head? true :actor {:principal "test"}})
           (world/world s)
           @n))))

(deftest a-head-is-derived-whole-once
  (with-redefs [orakel/on? false]
    (let [opts   (rules/store-opts clock/system)
          derive (:derive opts)
          n      (atom {:derive 0 :sig-init 0})
          db     (mem/store {:world (derive (first worlds))} (counting opts n))
          main   (world/store db "main" {})
          on-main (commits main n derive)
          _      (api/branch! db {:op :fork :name "f" :from {:branch "main"}})
          on-f   (commits (world/at main "f") n derive)
          never  {:derive 0 :sig-init 0}
          at-most-once? (fn [m] (every? #(<= % 1) (vals m)))]
      (doseq [[b cs] {"main" on-main "f" on-f}]
        (is (at-most-once? (first cs)) (str b ": the first commit derives its head once"))
        (is (every? #{never} (rest cs)) (str b ": the commits after it never"))))))

(deftest a-branch-named-anew-is-not-its-old-head
  (with-redefs [orakel/on? true]
    (let [opts   (rules/store-opts clock/system)
          derive (:derive opts)
          db     (mem/store {:world (derive (first worlds))} opts)
          main   (world/store db "main" {})
          f      (world/at main "f")
          step!  (fn [s W] (world/commit! s {:changes (changes (world/authored s) (derive W))
                                             :at-head? true :actor {:principal "test"}}))]
      (api/branch! db {:op :fork :name "f" :from {:branch "main"}})
      (step! f (nth worlds 3))
      (api/branch! db {:op :retire :name "f" :expect-rev (:rev (world/head f))})
      (step! main (nth worlds 1))
      (api/branch! db {:op :fork :name "f" :from {:branch "main"}})
      (step! f (nth worlds 2))
      (is (= (derive (nth worlds 2)) (world/authored f)))
      (is (= (derive (nth worlds 2)) (dissoc (world/world f) :settings)))
      (step! main (nth worlds 4))
      (is (= (derive (nth worlds 4)) (world/authored main))))))
