(ns sutra-ui.storage
  (:require [cljs.reader :as reader]))

(defn get-item
  [k]
  (try (.getItem js/localStorage k) (catch :default _ nil)))

(defn remove-item!
  [k]
  (try (.removeItem js/localStorage k) true (catch :default _ false)))

(defn set-item!
  [k v]
  (if (nil? v)
    (remove-item! k)
    (try (.setItem js/localStorage k (str v)) true (catch :default _ false))))

(defn get-edn
  [k]
  (try (some-> (get-item k) reader/read-string) (catch :default _ nil)))

(defn set-edn!
  [k v]
  (set-item! k (some-> v pr-str)))

(defn get-json
  ([k] (get-json k {}))
  ([k {:keys [keywordize?] :or {keywordize? true}}]
   (try (some-> (get-item k) js/JSON.parse (js->clj :keywordize-keys keywordize?))
        (catch :default _ nil))))

(defn set-json!
  [k v]
  (set-item! k (some-> v clj->js js/JSON.stringify)))
