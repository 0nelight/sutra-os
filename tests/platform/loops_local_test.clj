(ns ^:unit platform.loops-local-test
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [sutra.authoring.install.diff :as diff]
            [sutra.authoring.install.findings.loops :as loops]
            [sutra.authoring.install.live :as live]
            [sutra.catalog.world-install :as world-install]
            [sutra.catalog.transforms :as transforms]
            [sutra.world.db :as kdb]
            [sutra.types.schema :as sch]))

(def ^:private ks [:t0 :t1 :t2 :t3 :t4 :t5])

(def ^:private g-in
  (gen/one-of [(gen/fmap (fn [t] {:kind :transform :transform t}) (gen/elements ks))
               (gen/fmap (fn [o] {:kind :object-type :object-type o}) (gen/elements [:a :b :c]))
               (gen/fmap (fn [d] {:kind :dataset :dataset d}) (gen/elements [:d :e]))]))

(def ^:private g-out
  (gen/one-of [(gen/let [o (gen/elements [:a :b :c]) r (gen/elements [nil :supplement])]
                 (cond-> {:kind :object-type :object-type o} r (assoc :role r)))
               (gen/fmap (fn [d] {:kind :dataset :dataset d}) (gen/elements [:d :e]))]))

(def ^:private g-transform
  (gen/let [in   (gen/frequency [[1 (gen/return nil)] [2 g-in]])
            ins  (gen/vector g-in 0 3)
            outs (gen/vector g-out 0 2)]
    (cond-> {:inputs (zipmap [:i0 :i1 :i2] ins) :outputs outs} in (assoc :input in))))

(def ^:private g-world (gen/map (gen/elements ks) g-transform {:max-elements 6}))

(def ^:private g-plan
  (gen/vector (gen/let [k  (gen/elements ks)
                        oc (gen/elements [:created :updated :deleted])
                        t  g-transform]
                {:kind :transform :entity k :outcome oc :args t})
              1 3))

(defn- outcome [f] (try {:ok (set (f))} (catch Exception e {:thrown (ex-message e)})))

(defn- found [W plan ix]
  (outcome #(set ((:find loops/loops) (diff/diff {} W plan nil ix)))))

(defn- loops-in [w]
  (set (transforms/loop-problems (sch/indexed {:transforms (live/whole (:transforms w))}))))

(defn- whole
  [W plan]
  (outcome #(let [{:keys [before after] :as d} (diff/diff {} W plan nil (live/world-index W))]
              (when (seq (diff/changed d :transform))
                (set (for [p (remove (loops-in before) (loops-in after))]
                       (assoc p :entity-class :transforms
                              :entity (or (:transform p) (first (:cycle p))))))))))

(defspec the-loops-read-by-name-are-those-read-whole 1500
  (prop/for-all [ts g-world plan g-plan]
    (let [W  {:transforms ts}
          ix (live/index-of (kdb/db W world-install/views))]
      (= (whole W plan) (found W plan ix)))))

(defn- rid-of [k] (str "ri.sutra.main.transform." (name k)))

(defn- reading-by-rid [t]
  (letfn [(by-rid [in] (if (= :transform (:kind in)) (update in :transform rid-of) in))]
    (cond-> (update t :inputs #(into {} (for [[a in] %] [a (by-rid in)])))
      (:input t) (update :input by-rid))))

(defn- twisted
  [ts unnamed twice]
  (into {}
        (for [[k t] ts]
          [(rid-of k)
           (cond-> (reading-by-rid t)
             (not (unnamed k)) (assoc :transform-name (get twice k k)))])))

(defspec named-transforms-read-by-name-are-those-read-whole 1500
  (prop/for-all [ts g-world plan g-plan
                 unnamed (gen/set (gen/elements ks))
                 twice   (gen/frequency [[4 (gen/return {})]
                                         [1 (gen/map (gen/elements ks) (gen/elements ks)
                                                     {:max-elements 1})]])]
    (let [W  {:transforms (twisted ts unnamed twice)}
          ix (live/index-of (kdb/db W world-install/views))]
      (= (whole W plan) (found W plan ix)))))
