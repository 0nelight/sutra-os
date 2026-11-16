(ns sutra.world.gen.space
  (:require [clojure.test.check.generators :as gen]
            [sutra.ref.install :as ri]))

(def ^:private names [:a :b :c])

(def ^:private g-name (gen/elements names))

(defn- rid-of [sec n] (ri/rid (ri/tag-of sec) (name n)))

(defn- g-ref
  [sec]
  (gen/frequency [[3 (gen/fmap #(rid-of sec %) g-name)]
                  [2 g-name]
                  [1 (gen/return nil)]]))

(defn- g-refs [sec] (gen/fmap #(vec (remove nil? %)) (gen/vector (g-ref sec) 0 2)))

(def ^:private g-markings (gen/vector (gen/elements [:m1 :m2]) 0 2))

(def ^:private g-attr
  (gen/let [n (gen/elements [:x :y :z])
            t (gen/elements [:string :media-reference :attachment :timestamp])
            ms (gen/frequency [[2 (gen/return nil)] [3 g-name]])
            where (gen/elements [:top :opts])]
    (cond-> {:attribute-name n :attribute-type t}
      (and ms (= :top where))  (assoc :media-set ms)
      (and ms (= :opts where)) (assoc :opts {:media-set ms}))))

(def ^:private g-output
  (gen/one-of
   [(gen/let [t (g-ref :object-types) d (g-ref :datasets)
              r (gen/elements [nil :primary :supplement])]
      (into {:kind :object-type} (remove (comp nil? val)) {:object-type t :dataset d :role r}))
    (gen/fmap (fn [d] {:kind :dataset :dataset d}) (g-ref :datasets))]))

(def ^:private g-backing
  (gen/frequency
   [[2 (gen/return nil)]
    [3 (gen/let [d (g-ref :datasets) di (gen/elements [nil true false])
                 c (gen/elements [nil :exactly-once :at-least-once :bogus])
                 ts (gen/elements [nil :x :y :z])]
         (into {} (remove (comp nil? val))
               {:dataset d :direct di :consistency c :source-timestamp ts}))]]))

(def ^:private g-edit-rules
  (gen/vector (gen/let [op (gen/elements [:create :update :delete-object :create-link])
                        t (g-ref :object-types)]
                {:op op :object-type t})
              0 2))

(def ^:private ontology-sections
  [:object-types :link-types :interfaces :functions :shared-properties :value-types
   :object-type-groups])

(def ^:private fields
  (merge
   {:organizations {:organization-name g-name
                    :members (gen/vector (gen/one-of [(gen/elements ["p1" "p2" "p3"])
                                                      (gen/fmap (fn [p] {:principal p})
                                                                (gen/elements ["p1" "p2"]))])
                                         0 3)}
    :spaces        {:space-name g-name
                    :organization (g-ref :organizations)
                    :organizations (g-refs :organizations)
                    :shared-with (g-refs :organizations)}
    :projects      {:project-name g-name
                    :space (g-ref :spaces)
                    :shared-with (g-refs :organizations)}
    :media-sets    {:media-set-name g-name
                    :source (gen/one-of [(gen/fmap #(rid-of :sources %) g-name)
                                         (gen/return nil)])
                    :virtual gen/boolean
                    :sync (gen/elements [nil {:every :day}])
                    :markings g-markings}
    :sources       {:source-name g-name :markings g-markings}
    :transforms    {:transform-name g-name :project (g-ref :projects)
                    :outputs (gen/vector g-output 0 2)
                    :output (gen/frequency [[2 (gen/return nil)] [1 g-output]])}
    :actions       {:action-name g-name :project (g-ref :projects)
                    :rules g-edit-rules
                    :edits (gen/vector (gen/fmap (fn [t] {:object-type t}) (g-ref :object-types))
                                       0 1)}
    :datasets      {:dataset-name g-name :columns (gen/vector g-attr 0 2)
                    :project (g-ref :projects)
                    :stream (gen/frequency
                             [[2 (gen/return nil)] [2 (gen/return {:kind :kafka})]
                              [3 (gen/let [f (g-ref :datasets) fn (g-ref :functions)]
                                   {:derive (into {} (remove (comp nil? val))
                                                  {:from f :function fn})})]])}}
   (into {} (for [sec ontology-sections]
              [sec (cond-> {(ri/name-field sec) g-name :project (g-ref :projects)}
                     (= :object-types sec)  (assoc :attribute-list (gen/vector g-attr 0 2)
                                                   :implements (g-refs :interfaces)
                                                   :backing g-backing)
                     (= :link-types sec)    (assoc :from-object-type (g-ref :object-types)
                                                   :to-object-type (g-ref :interfaces))
                     (= :interfaces sec)    (assoc :extends (g-refs :interfaces))
                     (= :functions sec)     (assoc :target-type (g-ref :object-types))
                     (= :object-type-groups sec) (assoc :members (g-refs :object-types)))]))))

(defn- g-entry [sec k]
  (gen/fmap (fn [kvs] (into {:id k} (remove (comp nil? val)) kvs))
            (apply gen/hash-map (mapcat identity (fields sec)))))

(def ^:private base
  (let [sp #(rid-of :spaces %) pr #(rid-of :projects %)]
    {:spaces   {(sp :a) {:id (sp :a) :space-name :a} (sp :b) {:id (sp :b) :space-name :b}}
     :projects {(pr :a) {:id (pr :a) :project-name :a :space (sp :a)}
                (pr :b) {:id (pr :b) :project-name :b :space :b}
                (pr :c) {:id (pr :c) :project-name :c}}}))

(def ^:private stream-base
  (let [ds (rid-of :datasets :a) ta (rid-of :object-types :a) tb (rid-of :object-types :b)
        x  (rid-of :transforms :a)]
    (merge base
           {:datasets     {ds {:id ds :dataset-name :a :stream {:kind :kafka}}}
            :object-types {ta {:id ta :object-type-name :a :backing {:dataset :a}}
                           tb {:id tb :object-type-name :b}}
            :transforms   {x {:id x :transform-name :a
                              :outputs [{:kind :object-type :object-type tb :dataset ds}]}}})))

(def world
  (gen/let [b    (gen/elements [{} base stream-base])
            secs (gen/vector (gen/elements (keys fields)) 0 12)
            ns   (gen/vector g-name (count secs))
            es   (apply gen/tuple (gen/return nil)
                        (map (fn [sec n] (g-entry sec (rid-of sec n))) secs ns))]
    (reduce (fn [W [sec n e]] (assoc-in W [sec (rid-of sec n)] e))
            b (map vector secs ns (rest es)))))

(def change
  (gen/let [sec  (gen/elements (keys fields))
            n    g-name
            drop (gen/frequency [[1 (gen/return true)] [6 (gen/return false)]])
            [f v] (gen/bind (gen/elements (keys (fields sec)))
                            (fn [f] (gen/tuple (gen/return f) ((fields sec) f))))]
    [[sec (rid-of sec n)] (when-not drop {f v})]))

(defn apply-change
  [W [[sec k :as node] fv]]
  (if (nil? fv)
    (if (contains? W sec) (update W sec dissoc k) W)
    (let [[f v] (first fv)
          e     (merge {:id k} (get-in W node))]
      (assoc-in W node (if (nil? v) (dissoc e f) (assoc e f v))))))

(defn unshadowed?
  [W secs]
  (every? (fn [sec]
            (let [nf (ri/name-field sec)]
              (every? #(= 1 %) (vals (frequencies (for [[_ e] (get W sec)
                                                        :let [n (get e nf)] :when (some? n)]
                                                    (name n)))))))
          secs))

(defn one-name-per-scope?
  [W scope-of]
  (every? (fn [[sec es]]
            (or (not (map? es))
                (let [nf (ri/name-field sec)]
                  (every? #(= 1 %)
                          (vals (frequencies
                                 (for [[_ e] es :let [n (get e nf)] :when (some? n)]
                                   [(name n) (scope-of e)])))))))
          W))

(defn one-producer-per-type?
  [W]
  (let [producers (reduce (fn [m [k t]]
                            (reduce (fn [m o]
                                      (if (and (map? o) (= :object-type (:kind o))
                                               (some? (:object-type o)))
                                        (update m (:object-type o) (fnil conj #{}) k)
                                        m))
                                    m (or (seq (:outputs t)) (some-> (:output t) vector))))
                          {} (:transforms W))]
    (every? #(= 1 (count %)) (vals producers))))
