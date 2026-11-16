(ns sutra.capps.prop-source
  (:require [sutra.kit.coerce :refer [->kw]]
            [sutra.capps.catalog :as cat]
            [sutra.capps.derived :as cd]))

(def set-param-kinds #{:object-set :value})

(defn vars-index [tree]
  (let [acc (volatile! [])]
    (letfn [(go [x depth]
              (cond
                (map? x)        (do (when (map? (:vars x))
                                      (vswap! acc conj [depth (:vars x)]))
                                    (doseq [v (vals (dissoc x :vars))] (go v (inc depth))))
                (sequential? x) (doseq [v x] (go v (inc depth)))
                :else           nil))]
      (go tree 0))
    (reduce merge {} (map second (sort-by first @acc)))))

(declare ^:private var-type)

(defn- spec-type [index spec seen]
  (cond
    (not (map? spec))       nil
    (:agg spec)             nil
    (:aggregate spec)       nil
    (:function spec)        nil
    (:expr spec)            nil
    (contains? spec :default)     nil
    (contains? spec :default-agg) nil
    (:compare spec)         (var-type index (:of (:compare spec)) seen)
    (:source spec)          (->kw (:source spec))
    (:linked spec)          (->kw (:linked spec))
    (contains? spec :from)  (let [x (->kw (:from spec))]
                              (if (contains? index x)
                                (var-type index x seen)
                                x))
    :else                   nil))

(defn- var-type [index nm seen]
  (let [nm (->kw nm)]
    (when (and nm (not (contains? seen nm)))
      (spec-type index (get index nm) (conj seen nm)))))

(defn ref-type
  ([index ref] (ref-type index ref #{}))
  ([index ref seen]
   (when (map? ref)
     (cond
       (contains? ref :from) (->kw (:from ref))
       (contains? ref :source) (->kw (:source ref))
       (contains? ref :linked) (->kw (:linked ref))
       (contains? ref :val)  (var-type index (:val ref) seen)
       (contains? ref :var)  (var-type index (:var ref) seen)
       :else                 nil))))

(defn- set-params [node param-key]
  (let [params (cat/node-params node)
        here   (or (first (keep-indexed (fn [i p] (when (= param-key (:key p)) i)) params)) -1)]
    (->> params
         (map-indexed vector)
         (filter (fn [[_ p]] (contains? set-param-kinds (:kind p))))
         (sort-by (fn [[i p]] [(if (= :object-set (:kind p)) 0 1)
                               (if (<= i here) 0 1)
                               (max (- i here) (- here i))]))
         (mapv (fn [[_ p]] (:key p))))))

(defn node-object-type
  ([index node] (node-object-type index node nil))
  ([index node param-key]
   (when (map? node)
     (first (keep #(ref-type index (get node %)) (set-params node param-key))))))

(defn prop-type
  ([tree node] (prop-type tree node nil))
  ([tree node param-key] (node-object-type (vars-index tree) node param-key)))

(defn native-properties [objects type]
  (let [k     (->kw type)
        entry (or (get objects k) (when k (get objects (name k))))
        fields (cond (map? entry)        (:fields entry)
                     (sequential? entry) entry
                     :else               nil)]
    (vec (distinct (keep (fn [f]
                           (some-> (if (map? f) (or (:name f) (get f "name")) f)
                                   ->kw))
                         fields)))))

(defn prop-options [objects module type]
  (when-let [k (->kw type)]
    (let [native (native-properties objects k)
          known  (set native)
          extra  (vec (remove known (map first (cd/properties module k))))]
      (vec (for [[g label props] [[:native "Eigenschaften" native]
                                  [:derived "abgeleitet" extra]]
                 :when (seq props)]
             {:group g :label label :props props})))))

(defn prop-names [objects module type]
  (vec (mapcat :props (prop-options objects module type))))
