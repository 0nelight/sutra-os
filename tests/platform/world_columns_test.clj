(ns ^:unit platform.world-columns-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [sutra.catalog.world-columns :as kc]
            [sutra.catalog.transforms :as transforms]
            [sutra.world.db :as db]
            [sutra.world.gen :as kg]
            [sutra.world.gen.near :as gn]
            [sutra.world.guard :as guard]
            [sutra.commits.log :as log]
            [sutra.types.schema :as sch]))

(def ^:private columns kc/column-fns)

(defn- column-of [W id]
  (try ((columns id) W) (catch Exception e [:threw (ex-message e)])))

(defn- first-of [x] (when (vector? x) (first x)))

(defn- held [d id]
  (try (db/view d id) (catch Exception e [:threw (ex-message e)])))

(defspec a-stepped-column-is-the-columns-function 300
  (prop/for-all [W gn/world cs (gen/vector gn/change 1 4)]
    (let [W1 (reduce gn/apply-change W cs)
          d1 (try (log/stepped (db/db W kc/rows) W1) (catch Exception e e))]
      (if (instance? Exception d1)
        (some (fn [id] (some #(= :threw (first-of (column-of % id))) [W W1])) kc/columns)
        (every? (fn [id] (= (column-of W1 id) (held d1 id))) kc/columns)))))

(def ^:private t-keys (mapv #(keyword (str "t" %)) (range 8)))

(def ^:private g-in
  (gen/one-of [(gen/fmap (fn [t] {:kind :transform :transform t}) (gen/elements t-keys))
               (gen/fmap (fn [o] {:kind :object-type :object-type o}) (gen/elements [:a :b :c]))
               (gen/fmap (fn [d] {:kind :dataset :dataset d}) (gen/elements [:d :e]))]))

(def ^:private g-out
  (gen/one-of [(gen/let [o (gen/elements [:a :b :c]) r (gen/elements [nil :supplement])]
                 (cond-> {:kind :object-type :object-type o} r (assoc :role r)))
               (gen/fmap (fn [d] {:kind :dataset :dataset d}) (gen/elements [:d :e]))
               (gen/fmap (fn [l] {:kind :link-type :link-type l}) (gen/elements [:l :m]))]))

(def ^:private g-transform
  (gen/let [in  (gen/frequency [[1 (gen/return nil)] [2 g-in]])
            ins (gen/vector g-in 0 3)
            outs (gen/vector g-out 0 2)]
    (cond-> {:inputs (zipmap [:i0 :i1 :i2] ins) :outputs outs} in (assoc :input in))))

(def ^:private g-link
  (gen/let [f (gen/elements [:a :b :c nil]) t (gen/elements [:a :b :c])]
    {:from-object-type f :to-object-type t}))

(def g-graph
  (gen/let [ts (gen/map (gen/elements t-keys) g-transform {:max-elements 8})
            ls (gen/map (gen/elements [:l :m]) g-link)]
    {:transforms ts :link-types ls}))

(def g-graph-change
  (gen/one-of
   [(gen/tuple (gen/fmap #(vector :transforms %) (gen/elements t-keys))
               (gen/frequency [[1 (gen/return nil)] [4 g-transform]]))
    (gen/tuple (gen/fmap #(vector :link-types %) (gen/elements [:l :m]))
               (gen/frequency [[1 (gen/return nil)] [3 g-link]]))]))

(defn changed [W [[sec k] e]] (if e (assoc-in W [sec k] e) (update W sec dissoc k)))

(defspec the-graph-steps-by-key-as-graph-of 300
  (prop/for-all [W g-graph css (gen/vector (gen/vector g-graph-change 1 3) 1 6)]
    (loop [W W d (db/db W kc/rows) [cs & more] css]
      (or (nil? cs)
          (let [W1 (reduce changed W cs)
                d1 (db/db-with (guard/guarded d) (kg/diff W W1))]
            (and (= (transforms/graph-of W1) (db/view d1 :sutra.catalog.transforms/graph))
                 (recur W1 (update d1 :ents guard/unguarded) more)))))))

(deftest every-column-is-a-row
  (is (every? (set (map :id kc/rows)) kc/columns)))

(deftest a-world-carrying-its-views-answers-from-them
  (let [d (db/db {:interfaces {} :object-types {}} kc/rows)
        W (sch/with-views (db/world d) {:sutra.catalog.lookup/attrs ::held})]
    (is (= ::held (sch/column W :sutra.catalog.lookup/attrs (fn [_] ::computed))))
    (is (= ::computed (sch/column (assoc W :x 1) :sutra.catalog.lookup/attrs
                                  (fn [_] ::computed))))))
