(ns sutra.capps.interaction
  (:require [sutra.kit.coerce :refer [->kw]]))

(defn- value-of
  [x row]
  (if (and (map? x) (contains? x :row))
    (let [k (:row x)]
      (if (contains? row k) (get row k) (get (:values row) k)))
    x))

(defn action-defaults
  [params row]
  (into {} (map (fn [[k x]] [(->kw k) (value-of x row)])) params))

(defn plan
  [{:keys [action on-click params hide-form?]} row]
  (cond
    action   [(if hide-form? :submit :form) (action-defaults params row)]
    on-click [:events on-click]
    :else    nil))

(defn usable?
  [{:keys [for]} kind]
  (or (nil? for) (= :both (->kw for)) (= kind (->kw for))))
