(ns sutra.foundation.wire
  (:require [malli.core :as m]
            [malli.error :as me]))

(def bus
  {:build/requested   [:map [:run-id :string]]
   :build/cancel      [:map [:run-id :string]]
   :hot/patched       [:map [:files [:vector :string]]]
   :live/events       [:vector [:map [:at {:optional true} :int]]]
   :settings/changed  [:map]
   :observe/arm       [:map [:tracer [:enum :sql :profile]] [:on :boolean]
                       [:opts {:optional true} [:maybe :map]]]
   :observe/arm-fn    [:map [:on :boolean] [:opts :map]]
   :observe/arm-decisions [:map [:on :boolean] [:opts :map]]
   :observe/report-now [:map [:request :string]]
   :observe/collect   [:map [:request :string]]
   :observe/collected [:map [:request :string] [:node :string]]
   :notifications/new [:map [:id :any] [:recipient :any] [:heading {:optional true} :any]]
   :watch/notify      [:map [:branch :any] [:object-type :any] [:payload [:maybe :map]]]
   :watch/wake        [:map]
   :stream/wake       [:map]})

(def context
  [:map [:branch :string]
   [:authority {:optional true} [:maybe :map]]
   [:as-of {:optional true} [:maybe :int]]])

(def rows
  {:build-requests {:context    context
                    :preset     [:map]
                    :result     [:map]
                    :error-data [:map]}})

(def json-data
  [:schema {:registry {::json [:or :nil :string 'number? :boolean
                               [:vector [:ref ::json]] [:map-of :keyword [:ref ::json]]]}}
   ::json])

(def http
  {:action-params
   [:map
    [:action        {:optional true} :any]
    [:params        [:map-of :keyword :map]]
    [:values        [:map-of :keyword json-data]]
    [:objects       [:map-of :keyword [:vector [:map [:id :string]]]]]
    [:unencodable   [:vector [:map [:parameter [:or :keyword :string]]]]]
    [:options       [:maybe :map]]
    [:option-errors {:optional true} :map]
    [:missing       [:vector :any]]
    [:invalid       [:vector :any]]
    [:violated      [:vector :map]]
    [:blocked?      :boolean]
    [:reason        [:maybe :string]]]})

(def definitions {:bus bus :rows rows :http http :context context})

(def ^:private validator (memoize (fn [form] (m/validator form))))

(defn problem
  [path x]
  (if-let [form (get-in definitions path)]
    (when-not ((validator form) x)
      (pr-str (me/humanize (m/explain form x))))
    (str "no wire definition for " (pr-str path))))

#?(:clj
   (def module
     {:name     :wire
      :provides {:cluster/check-message (fn [topic message] (problem [:bus topic] message))}}))
