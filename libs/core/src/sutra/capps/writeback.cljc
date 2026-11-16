(ns sutra.capps.writeback
  (:require [sutra.kit.coerce :refer [->kw]]
            [sutra.kit.tree :as tree]))

(def form-field-widgets
  #{:text-input :number-input :date-input :select-input :combo-input :checkbox :rtf-text})

(def form-field-threshold
  2)

(defn- value-reads [layout]
  (frequencies (keep #(when (contains? % :val) (:val %)) (tree/nodes layout))))

(defn- ref-var [v]
  (when (map? v) (or (:val v) (:var v))))

(defn- form-fields [layout]
  (into {}
        (keep (fn [n]
                (when-let [w (form-field-widgets (->kw (:widget n)))]
                  (when-let [h (get-in n [:sel :var])]
                    [h w]))))
        (tree/nodes layout)))

(defn- action-buttons [layout]
  (filter #(= :action-button (->kw (:widget %))) (tree/nodes layout)))

(defn- param-reads [layout]
  (->> (action-buttons layout)
       (map (comp value-reads :params))
       (apply merge-with + {})))

(defn hand-built-forms
  [layout]
  (let [fields (form-fields layout)
        gelesen (value-reads layout)
        fuer-action (param-reads layout)
        nur-formular? (fn [h] (and (contains? fields h)
                                   (= (get gelesen h) (get fuer-action h))))]
    (when (seq fields)
      (for [n (action-buttons layout)
            :let [eigen (for [[p v] (:params n)
                              :let [h (ref-var v)]
                              :when (nur-formular? h)]
                          [p h])]
            :when (>= (count eigen) form-field-threshold)]
        {:action (->kw (:action n))
         :params (vec (sort (map first eigen)))
         :fields (vec (sort (map second eigen)))
         :widgets (vec (sort (distinct (map #(get fields (second %)) eigen))))}))))
