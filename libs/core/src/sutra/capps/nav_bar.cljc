(ns sutra.capps.nav-bar
  (:require [sutra.kit.coerce :refer [->kw]]))

(defn same-id?
  [a b]
  (let [id #(if (keyword? %) (name %) (str %))]
    (and (some? a) (some? b) (= (id a) (id b)))))

(defn- layout-effect? [e] (and (map? e) (= :navigate (->kw (:do e)))))

(defn- effects-of [on-click] (if (map? on-click) [on-click] (vec on-click)))

(defn- settled
  [on-click value-of]
  (count (filter #(and (layout-effect? %) (same-id? (value-of (:var %)) (:to %)))
                 (effects-of on-click))))

(defn selected-tab
  [items sel-value value-of]
  (if (some? sel-value)
    (some #(when (same-id? (:id %) sel-value) (:id %)) items)
    (let [scored (map (fn [it] [(settled (:on-click it) value-of) it]) items)
          best   (reduce max 0 (map first scored))]
      (when (pos? best)
        (some (fn [[n it]] (when (= n best) (:id it))) scored)))))

(defn tab-state
  [{:keys [when-false] :as it}]
  (if (and (contains? it :visible) (not (:visible it)))
    (if (= :hidden (->kw when-false)) :hidden :disabled)
    :shown))

(defn- size-of
  [design size]
  (let [s (->kw size)]
    (cond
      (= :large s)                           :large
      (and (= :small s) (= :grouped design)) :small
      :else                                  :default)))

(defn tabs-class
  [{:keys [design size minimal? direction alignment fill?]}]
  (let [d (or (->kw design) :outline)]
    (cond-> (str "capp-tabs capp-tabs--" (name d) " capp-tabs--size-" (name (size-of d size)))
      (and minimal? (= :block d)) (str " capp-tabs--minimal")
      fill?                       (str " capp-tabs--fill")
      (= :vertical (->kw direction))
      (str " capp-tabs--vertical capp-tabs--align-" (name (or (->kw alignment) :left))))))

(defn tabs-style
  [{:keys [design active-color height width]}]
  (cond-> {}
    active-color (assoc "--capp-tab-active" active-color)
    height       (assoc :height (str height "px"))
    (and width (= :prominent (->kw design))) (assoc :width (str width "px"))))

(def max-items 4)

(defn bar-items
  [items]
  (vec (take max-items items)))

(defn- item-effects
  [{:keys [on-click items]}]
  (concat (effects-of on-click) (mapcat (comp effects-of :on-click) items)))

(defn bar-page-var
  [items]
  (some (fn [it] (some #(when (layout-effect? %) (:var %)) (item-effects it))) items))

(defn lit-index
  [items active prev]
  (or (first (keep-indexed
              (fn [i it]
                (when (some #(and (layout-effect? %) (same-id? (:to %) active)) (item-effects it))
                  i))
              items))
      prev))

(defn favicon-href
  [icon]
  (str "data:image/svg+xml,"
       "%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100'%3E"
       "%3Ctext y='.9em' font-size='90'%3E" icon "%3C/text%3E%3C/svg%3E"))
