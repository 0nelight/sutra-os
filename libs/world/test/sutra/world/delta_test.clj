(ns sutra.world.delta-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [malli.generator :as mg]
            [sutra.world.delta :as d]
            [sutra.world.gen :as g]
            [sutra.world.schema :as schema]))

(def ^:private edited
  (gen/let [W g/world cs (gen/vector g/raw-change 1 6)]
    [W (reduce g/apply-raw W cs)]))

(defspec invert-undoes 60
  (prop/for-all [[W W'] edited]
    (let [delta (g/diff W W')]
      (and (= W' (d/apply-to W delta))
           (= W (d/apply-to W' (d/invert delta)))))))

(def ^:private framed
  (gen/let [W  g/world
            s  (gen/elements [:object-types :value-types :projects :schema-version :x])
            op (gen/elements [:drop :empty :value])
            v  gen/small-integer]
    [W (case op :drop (dissoc W s) :empty (assoc W s {}) :value (assoc W s v))]))

(defspec the-frame-moves-by-a-delta 100
  (prop/for-all [[W W'] framed]
    (let [delta (g/diff W W')]
      (and (= W' (d/apply-to W delta))
           (= W (d/apply-to W' (d/invert delta)))))))

(deftest an-entry-entering-a-missing-section-opens-it-in-the-delta
  (let [[o e] (d/completed {} [[[:object-types "r"] :id d/absent "r"]])]
    (is (= [[:frame :object-types] :value d/absent {}] o))
    (is (= [[:object-types "r"] :id d/absent "r"] e))))

(deftest closing-a-section-that-holds-entries-is-refused
  (is (thrown? Exception
               (d/apply-to {:object-types {"r" {:id "r"}}}
                           [[[:frame :object-types] :value {} d/absent]]))))

(defspec invert-is-an-involution 100
  (prop/for-all [delta (mg/generator schema/Delta)]
    (= delta (d/invert (d/invert delta)))))

(defspec squash-is-idempotent 100
  (prop/for-all [delta (mg/generator schema/Delta)]
    (= (d/squash delta) (d/squash (d/squash delta)))))

(defspec squash-applies-alike 60
  (prop/for-all [W g/world cs (gen/vector g/raw-change 1 6)]
    (let [Ws    (reductions g/apply-raw W cs)
          steps (mapcat g/diff Ws (rest Ws))]
      (= (d/apply-to W steps) (d/apply-to W (d/squash steps)) (last Ws)))))

(defspec merge3-commutes-without-conflict 80
  (prop/for-all [W g/world ca (gen/vector g/raw-change 1 4) cb (gen/vector g/raw-change 1 4)]
    (let [ours   (g/diff W (reduce g/apply-raw W ca))
          theirs (g/diff W (reduce g/apply-raw W cb))
          ab     (d/merge3 ours theirs)
          ba     (d/merge3 theirs ours)]
      (or (seq (:conflicts ab))
          (= (d/apply-to (d/apply-to W ours) (:changes ab))
             (d/apply-to (d/apply-to W theirs) (:changes ba)))))))

(defspec merge3-theirs-takes-every-element-of-theirs 80
  (prop/for-all [W g/world ca (gen/vector g/raw-change 1 4) cb (gen/vector g/raw-change 1 4)]
    (let [ours   (g/diff W (reduce g/apply-raw W ca))
          theirs (g/diff W (reduce g/apply-raw W cb))
          {:keys [changes conflicts]} (d/merge3 ours theirs :theirs)
          W'     (d/apply-to (d/apply-to W ours) changes)]
      (and (empty? conflicts)
           (every? (fn [[n f _ b]] (= b (d/held W' n f))) theirs)))))

(deftest a-conflict-names-the-element
  (let [n [:object-types "ri.sutra.main.object-type.a"]]
    (is (= [{:node n :field :title-key :base d/absent :ours :x :theirs :y}]
           (:conflicts (d/merge3 [[n :title-key d/absent :x]] [[n :title-key d/absent :y]]))))
    (is (empty? (:changes (d/merge3 [[n :title-key d/absent :x]] [[n :title-key d/absent :x]]))))))

(deftest a-stale-change-is-refused
  (let [n [:object-types "ri.sutra.main.object-type.a"]]
    (is (= :stale-change
           (try (d/apply-to {} [[n :title-key :x :y]])
                (catch clojure.lang.ExceptionInfo e (:ref/kind (ex-data e))))))))
