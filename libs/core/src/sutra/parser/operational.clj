(ns sutra.parser.operational
  (:require [clojure.string :as str]
            [sutra.types.duration :as dur]
            [sutra.types.health-metric :as hm]
            [malli.core :as m]))

(set! *warn-on-reflection* true)

(def ^:private EffectMode [:enum :apply :propose])

(def ActionEffect
  [:and
   [:map {:closed false}
    [:action :keyword]
    [:mode   {:optional true} EffectMode]
    [:reason     {:optional true} :any]
    [:provenance {:optional true} [:map-of :keyword :any]]
    [:key        {:optional true} :any]]
   [:fn
    {:error/message ":key, :reason and :provenance describe a proposal — they need :mode :propose"}
    #(or (= :propose (:mode %)) (not-any? (partial contains? %) [:key :reason :provenance]))]])

(def ^:private RetryStrategy
  [:map {:closed true}
   [:attempts     {:optional true} [:int {:min 0 :max 3}]]
   [:backoff      {:optional true} [:enum :constant :exponential]]
   [:delay-ms     {:optional true} [:int {:min 0 :max 60000}]]
   [:max-delay-ms {:optional true} [:int {:min 0 :max 300000}]]
   [:jitter       {:optional true} [:double {:min 0.0 :max 1.0}]]])

(def ^:private FallbackEffect
  [:map {:closed true}
   [:action :keyword]
   [:args   {:optional true} [:map-of :keyword :any]]])

(def LogicEffect
  [:map {:closed true}
    [:function     :keyword]
    [:version      {:optional true} :string]
    [:auto-upgrade {:optional true} :boolean]
    [:inputs       {:optional true} [:map-of :keyword :any]]
    [:mode         {:optional true} EffectMode]
    [:parallelism  {:optional true} [:int {:min 1 :max 100}]]
    [:retries      {:optional true} [:int {:min 0 :max 3}]]
    [:retry        {:optional true} RetryStrategy]
    [:once         {:optional true} [:enum :object :version]]
    [:fallback     {:optional true} FallbackEffect]])

(def NotifyLink
  [:and
   [:map {:closed true}
    [:label       {:optional true} :string]
    [:object-type {:optional true} :keyword]
    [:id          {:optional true} :string]
    [:app         {:optional true} :keyword]
    [:url         {:optional true} :string]]
   [:fn {:error/message "a :link names one target: :object-type with :id, :app or :url"}
    #(= 1 (count (filter some? [(:object-type %) (:app %) (:url %)])))]])

(def NotifyEffect
  [:map {:closed true}
   [:notify [:and
             [:map {:closed true}
              [:to            {:optional true} [:vector :string]]
              [:to-property   {:optional true} :keyword]
              [:heading       {:optional true} :string]
              [:message       {:optional true} :string]
              [:email-subject {:optional true} :string]
              [:email-html    {:optional true} :string]
              [:content       {:optional true} [:map {:closed true} [:function :keyword]]]
              [:link          {:optional true} NotifyLink]
              [:grouping      {:optional true}
               [:or [:enum :each :all] [:map {:closed true} [:by [:vector :keyword]]]]]
              [:objects       {:optional true} [:enum :added :changed]]]
             [:fn
              {:error/message "a :notify effect names its recipients: :to, :to-property or both"}
              #(or (seq (:to %)) (:to-property %))]
             [:fn {:error/message "a :notify effect has a :heading or :content {:function …}"}
              #(or (:heading %) (get-in % [:content :function]))]]]])

(def EntAutomation
  [:map {:closed true}
   [:name     :string]
   [:on       :keyword]
   [:when     {:optional true} [:maybe [:or vector? [:map-of :keyword :any]]]]
   [:effect   [:multi {:dispatch #(cond (and (map? %) (contains? % :function)) :logic
                                        (and (map? %) (contains? % :notify))   :notify
                                        :else                                  :action)}
               [:logic LogicEffect]
               [:notify NotifyEffect]
               [:action ActionEffect]]]
   [:every-ms {:optional true} :int]
   [:live     {:optional true} :boolean]
   [:auto-mute {:optional true} :boolean]
   [:paused   {:optional true} :boolean]])

(def ^:private ScheduleTrigger
  [:schema
   {:registry
    {::trigger
     [:multi {:dispatch :kind}
      [:all-of [:map {:closed true}
                [:kind [:= :all-of]]
                [:triggers [:vector [:ref ::trigger]]]]]
      [:any-of [:map {:closed true}
                [:kind [:= :any-of]]
                [:triggers [:vector [:ref ::trigger]]]]]
      [::m/default
       [:map {:closed true}
        [:kind     [:enum :cadence :parent-updated :logic-updated]]
        [:of       {:optional true} [:or :keyword :string
                                     [:vector [:or :keyword :string]]]]
        [:cron     {:optional true} [:maybe :string]]
        [:every-ms {:optional true} :int]]]]}}
   ::trigger])

(def EntSchedule
  [:map {:closed true}
   [:name     :string]
   [:target   [:and [:or :keyword :string]
               [:fn {:error/message "a schedule must name a non-blank :target"}
                (fn [t] (not (and (string? t) (str/blank? t))))]]]
   [:cron     {:optional true} [:maybe :string]]
   [:every-ms {:optional true} :int]
   [:trigger  {:optional true} ScheduleTrigger]
   [:scope    {:optional true} [:enum :self :with-dependencies :downstream :connecting]]
   [:from     {:optional true} [:or :keyword :string]]
   [:paused   {:optional true} :boolean]
   [:retry-count      {:optional true} [:int {:min 0}]]
   [:retry-backoff    {:optional true}
    [:fn
     {:error/message "a duration: milliseconds, `<n>ms|s|m|h|d` or {:unit \"SECONDS\" :value n}"}
     dur/valid?]]
   [:abort-on-failure {:optional true} :boolean]
   [:parameters       {:optional true} [:map-of :keyword [:maybe [:or :string number? :boolean]]]]
   [:run-setup-key    {:optional true} :keyword]])

(def EntHealthCheck
  [:map {:closed true}
   [:name         :string]
   [:target       {:optional true} [:maybe [:or :keyword :string]]]
   [:on           {:optional true} [:enum :object-type :transform :dataset]]
   [:kind         (into [:enum] (sort (keys hm/kinds)))]
   [:severity     {:optional true} [:enum :moderate :critical]]
   [:column       {:optional true} :keyword]
   [:within-hours {:optional true} number?]
   [:min          {:optional true} number?]
   [:max          {:optional true} number?]
   [:comparison   {:optional true} :keyword]
   [:query        {:optional true} :string]
   [:measure      {:optional true} [:enum :count :sum]]
   [:against      {:optional true} [:map {:closed true}
                                    [:on     [:enum :object-type :transform :dataset]]
                                    [:target [:or :keyword :string]]
                                    [:column {:optional true} :keyword]]]
   [:tolerance    {:optional true} number?]
   [:paused       {:optional true} :boolean]])
