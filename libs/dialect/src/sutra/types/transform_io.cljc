(ns sutra.types.transform-io
  (:require [sutra.types.kinds :as kinds]))

(def backing-fields
  [:primary-key :column-map :from-key-column :to-key-column :fk-attribute])

(defn outputs-of
  [t]
  (cond
    (seq (:outputs t)) (vec (:outputs t))

    (some? (:output t))
    [(merge (select-keys t backing-fields)
            (:output t)
            {:name (:transform-name t)})]

    :else []))

(defn inputs-of
  [t]
  (cond
    (map? (:inputs t)) (vec (vals (:inputs t)))
    (seq (:inputs t))  (vec (:inputs t))
    (:input t)         [(:input t)]
    :else              []))

(defn object-type-inputs
  [t]
  (into [] (comp (filter #(= :object-type (keyword (:kind %))))
                 (keep :object-type))
        (inputs-of t)))

(defn call-inputs
  [t]
  (into [] (comp (filter (fn [[_ in]] (kinds/is? :call-input? (:kind in))))
                 (map (fn [[alias in]] [(keyword alias) in])))
        (sort-by (comp str key) (when (map? (:inputs t)) (:inputs t)))))

(defn optional-input?
  [in]
  (boolean (and (= :scalar (keyword (:kind in))) (:optional in))))

(defn- holds-map?
  [v]
  (or (map? v) (and (sequential? v) (some holds-map? v))))

(defn takes-map?
  [in]
  (contains? #{:struct "struct"} (:type in)))

(defn unfit-scalars
  [t args]
  (into [] (comp (filter (fn [[alias in]]
                           (and (= :scalar (keyword (:kind in)))
                                (not (takes-map? in))
                                (holds-map? (get args alias)))))
                 (map first))
        (call-inputs t)))

(defn unbound-scalars
  [t bound]
  (into [] (comp (remove (comp optional-input? second))
                 (map first)
                 (remove #(contains? bound %)))
        (call-inputs t)))
