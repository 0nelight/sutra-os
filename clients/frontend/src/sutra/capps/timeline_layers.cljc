(ns sutra.capps.timeline-layers
  (:require [clojure.string :as str]
            [sutra.capps.table :as tbl]))

(defn- show-at
  [v]
  (let [s (str v)]
    (if-let [[_ d t] (re-matches #"(\d{4}-\d{2}-\d{2})(?:[T ](\d{2}:\d{2}).*)?" s)]
      (if t (str d " " t) d)
      s)))

(defn layer-events
  [{:keys [rows at title title-prop body tone label]}]
  (let [af (tbl/field-fn at)
        tf (tbl/field-fn title-prop)
        bs (keep tbl/field-fn (if (sequential? body) body (when body [body])))]
    (into []
          (keep (fn [row]
                  (let [a (af row)]
                    (when-not (str/blank? (str a))
                      (let [body* (->> bs (map #(% row)) (remove #(str/blank? (str %)))
                                       (map str) (str/join " · ") not-empty)]
                        (cond-> {:sort  (str a)
                                 :at    (show-at a)
                                 :title (or (some-> tf (apply [row]) str not-empty) title label "—")
                                 :row   row}
                          body* (assoc :body body*)
                          tone  (assoc :tone tone)
                          label (assoc :layer label)))))))
          rows)))

(defn events
  [layers]
  (->> (map-indexed (fn [i l] (map #(assoc % :i i) (layer-events l))) (or layers []))
       (apply concat)
       (sort-by (juxt :sort :i))
       (mapv #(dissoc % :sort :i))))
