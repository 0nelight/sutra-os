(ns sutra.kit.refusal
  (:require [sutra.i18n.catalog :as catalog]
            [sutra.kit.anomaly :as anomaly]
            #?(:clj [clojure.edn :as edn])
            #?(:clj [clojure.java.io :as io]))
  #?(:cljs (:require-macros [sutra.i18n.inline :as inline])))

(def categories
  #?(:clj  (edn/read-string (slurp (io/resource "refusal/category.edn")))
     :cljs (inline/refusal-categories)))

(defn message
  ([code params] (message :en code params))
  ([l code params] (catalog/text l code params)))

(defn refused
  ([code data] (refused code data nil))
  ([code data params]
   (let [data (cond-> (assoc data :refusal/code code)
                (seq params) (assoc :refusal/params params))
         c    (or (::anomaly/category data) (categories code :fault))]
     (anomaly/refused c (message code (merge data params)) data))))

(defn refusal
  ([code data] (refusal code data nil nil))
  ([code data params] (refusal code data params nil))
  ([code data params cause] (anomaly/ex (refused code data params) cause)))

(defn refuse!
  ([code data] (throw (refusal code data nil nil)))
  ([code data params] (throw (refusal code data params nil)))
  ([code data params cause] (throw (refusal code data params cause))))
