(ns sutra.types.llm-registration
  (:require [clojure.string :as str]
            [sutra.kit.coerce :refer [->name-kw ->num]]
            [sutra.types.llm-cost :as llm-cost]))

(def fields
  [:in :out :currency :source :provider :provider-model :capabilities :default :deprecated
   :description :context-window])

(defn rate
  [v]
  (when-let [n (->num v)] (when-not (neg? n) n)))

(defn problems
  [{:keys [id in out]}]
  (cond-> []
    (str/blank? (str id)) (conj :id)
    (nil? (rate in))      (conj :in)
    (nil? (rate out))     (conj :out)))

(defn- window [v]
  (when-let [n (->num v)]
    (when (pos? n) (long #?(:clj (Math/round (double n)) :cljs (js/Math.round n))))))

(defn registration
  [m]
  (->> (assoc (select-keys m fields)
              :description    (some-> (:description m) str not-empty)
              :name           (str/trim (str (:id m)))
              :in             (rate (:in m))
              :out            (rate (:out m))
              :currency       (or (some-> (:currency m) str str/trim not-empty)
                                  llm-cost/default-currency)
              :source         (->name-kw (:source m))
              :provider       (->name-kw (:provider m))
              :provider-model (some-> (:provider-model m) str)
              :capabilities   (some->> (:capabilities m) (mapv ->name-kw) not-empty)
              :context-window (window (:context-window m)))
       (remove (comp nil? val))
       (into {})))
