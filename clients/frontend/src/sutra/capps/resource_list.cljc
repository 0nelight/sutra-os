(ns sutra.capps.resource-list
  (:require [clojure.string :as str]
            [sutra.kit.coerce :refer [->kw]]))

(defn- text [x] (some-> x (#(if (keyword? %) (name %) (str %)))))

(defn- same?
  [family nm entry]
  (and (= family (->kw (:family entry)))
       (or (= (text nm) (text (:name entry))) (= (text nm) (text (:rid entry))))))

(defn- find-entry
  [resources family nm]
  (some #(when (same? family nm %) %) resources))

(defn- files?
  [entry]
  (not= :object-types (->kw (:family entry))))

(defn- object-type?
  [entry]
  (= :object-types (->kw (:family entry))))

(defn- family-of-kind
  [kind]
  (let [k (text kind)]
    (->kw (if (str/ends-with? (str k) "s") k (str k "s")))))

(defn- matching-refs
  [refs resources]
  (keep (fn [{:keys [kind id]}] (find-entry resources (family-of-kind kind) id)) refs))

(defn- dynamic
  [{:keys [resource-kind content projects folders tags]}
   {:keys [resources folder-refs favorites recents]}]
  (let [want (if (= :object-types (->kw resource-kind)) object-type? files?)
        of   (filter want resources)]
    (case (->kw content)
      :all       of
      :prominent (filter :prominent of)
      :favorite  (filter want (matching-refs favorites resources))
      :recent    (filter want (matching-refs recents resources))
      :tags      (let [ts (set (map text tags))]
                   (filter #(some ts (map text (:tags %))) of))
      :folders   (let [ps   (set (map text projects))
                       refs (into #{} (mapcat #(get folder-refs (text %))) folders)]
                   (filter #(or (ps (text (:project %)))
                                (refs (str (:rid %)))
                                (refs (str (text (:family %)) "/" (text (:name %)))))
                           of))
      [])))

(defn- static-entry
  [{:keys [family name rid object-type rows] :as item} resources]
  (cond
    (some? rows)
    (assoc item :object-set? true :family :object-sets)

    object-type
    (merge (find-entry resources :object-types object-type)
           {:family :object-types :name object-type}
           item)

    :else
    (merge (or (find-entry resources (->kw family) (or name rid))
               {:family (->kw family) :name (or name rid)})
           item)))

(defn- overrides
  [{:keys [label name title] :as e} cfg]
  (assoc e
         :title (or title label (text name))
         :style (or (->kw (:style e)) (->kw (:style cfg)) :classic)
         :thumb-at (or (->kw (:thumbnail-position e)) :top)))

(defn items
  [{:keys [content] :as cfg} answer]
  (let [resources (:resources answer)
        raw       (if (or (nil? content) (= :static (->kw content)))
                    (map #(static-entry % resources) (:items cfg))
                    (dynamic cfg answer))]
    (into []
          (comp (remove #(false? (:visible %)))
                (map #(overrides % cfg))
                (remove #(str/blank? (:title %))))
          raw)))

(defn needs-answer?
  [{:keys [resource-kind]}]
  (not= :object-sets (->kw resource-kind)))

(defn click-plan
  [{:keys [on-click family object-set? name] :as entry}]
  (cond
    (seq on-click)                  [:events on-click]
    object-set?                     [:open-set entry]
    (= :object-types (->kw family)) [:open-type name]
    (= :documents (->kw family))    [:open-document name]
    (= :applications (->kw family)) [:open-app name]
    :else                           [:open-resource [(->kw family) name]]))
