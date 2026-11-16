(ns sutra.notifications.content
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

(def limits
  {:heading 250 :message 1000 :email-html 51200 :email-subject 250})

(defn truncate
  [s n]
  (when s
    (let [s (str s)]
      (if (> (count s) n) (str (subs s 0 (max 0 (- n 3))) "...") s))))

(defn- shown [v]
  (cond (sequential? v) (str/join ", " (map shown v))
        (map? v)        (str (or (:name v) (:username v) (:id v) ""))
        (keyword? v)    (name v)
        :else           (str (or v ""))))

(defn- lookup [values path]
  (get-in values (mapv keyword (str/split path #"\."))))

(def ^:private placeholder
  #"\{\{\{?\s*([A-Za-z0-9_\-.]+)\s*\}?\}\}|\{([A-Za-z0-9_\-]+)\}")

(defn render
  [template values]
  (when template
    (str/replace (str template) placeholder
                 (fn [[_ path attr]] (shown (lookup values (or path attr)))))))

(defn link-url
  [{:keys [object app url]} values]
  (cond
    object (let [[t id] object]
             (when (and t (some? id))
               (str "/object/" (name t) "/"
                    (str/replace (java.net.URLEncoder/encode (str id) "UTF-8") "+" "%20"))))
    app    (str "/apps/" (name app))
    url    (not-empty (render url values))))

(defn sized
  [n]
  (reduce-kv (fn [n k limit] (cond-> n (string? (get n k)) (update k truncate limit)))
             n limits))
