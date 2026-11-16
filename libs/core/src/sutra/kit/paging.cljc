(ns sutra.kit.paging)

(defn next-offset
  [{:keys [rows offset total]}]
  (let [seen (+ (or offset 0) (count rows))]
    (when (and (number? total) (< seen total)) seen)))

(defn page
  [{:keys [rows] :as p}]
  (let [p (assoc p :rows (vec rows))]
    (assoc p :next-offset (next-offset p))))

(defn continues?
  [held more]
  (and (some? (:offset more)) (= (next-offset held) (:offset more))))

(defn append
  [held more]
  (if (continues? held more)
    (page (assoc held
                 :rows  (into (vec (:rows held)) (:rows more))
                 :total (if (contains? more :total) (:total more) (:total held))))
    held))

(defn sentinel
  [p load!]
  (when-let [o (next-offset p)]
    {:loaded (+ (or (:offset p) 0) (count (:rows p))) :total (:total p) :load! #(load! o)}))
