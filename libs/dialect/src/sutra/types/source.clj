(ns sutra.types.source
  (:require [malli.core :as m]
            [malli.error :as me]
            [sutra.types.value-types :as vt]))

(set! *warn-on-reflection* true)

(def jdbc-scan
  {:preds {:= :inexact :in :inexact :nil? :exact} :project? true :page? true :compound? true})

(def local-scan
  {:preds {:= :exact :not= :exact :< :exact :<= :exact :> :exact :>= :exact :in :exact
           :nil? :exact :starts-with :exact :has :exact}
   :project? true :page? true :compound? true})

(def profiles
  {:sqlite     {:capabilities #{:catalog :scan :sql :write} :route #{:direct}
                :needs [:jdbc-url] :scan jdbc-scan}
   :postgres   {:capabilities #{:catalog :scan :sql :write} :endpoint :jdbc-url
                :route #{:direct :agent}
                :needs [:jdbc-url :egress :egress-allowed] :scan jdbc-scan}
   :mysql      {:capabilities #{:catalog :scan :sql :write} :endpoint :jdbc-url
                :route #{:direct :agent}
                :needs [:jdbc-url :egress :egress-allowed] :scan jdbc-scan}
   :mssql      {:capabilities #{:catalog :scan :sql :write} :endpoint :jdbc-url
                :route #{:direct :agent}
                :needs [:jdbc-url :egress :egress-allowed] :scan jdbc-scan}
   :sftp       {:capabilities #{:land :scan} :endpoint :host :route #{:agent}
                :needs [:host :remote-path :path :egress :egress-allowed] :scan local-scan
                :file :csv}
   :command    {:capabilities #{:land} :endpoint :host :route #{:agent}
                :needs [:host :command :path :allow-commands :egress :egress-allowed]}
   :filesystem {:capabilities #{:land} :route #{:agent} :needs [:path]}
   :glpi-api   {:capabilities #{:land :invoke} :route #{:agent} :needs [:path :egress]
                :preset :glpi}
   :rest       {:capabilities #{:catalog :scan :invoke} :endpoint :base-url :route #{:agent}
                :needs [:egress]}
   :imap       {:capabilities #{} :route #{:agent} :needs [:egress]}
   :csv        {:capabilities #{:catalog :scan} :route #{:csv} :needs [:path] :scan local-scan
                :file :csv}})

(defn- claimable?
  [t]
  (empty? (:capabilities (get profiles t))))

(defn collisions
  [cs]
  (vec (sort (filter #(and (contains? profiles %) (not (claimable? %))) (keys cs)))))

(defn announced
  [cs]
  (let [bad (collisions cs)]
    {:connectors (into {} (for [[t c] (apply dissoc cs bad)]
                            [t (assoc-in c [:profile :route] #{:agent})]))
     :left-out   bad}))

(defn connectors
  []
  (merge (some-> (find-ns 'sutra.sources.datasources) (ns-resolve 'announced) deref
                 deref)
         (if-let [sys-ns (find-ns 'sutra.kit.system)]
           ((ns-resolve sys-ns 'table) ((ns-resolve sys-ns 'process)) :foreign/connectors)
           {})))

(defn connector-in
  [connectors type]
  (when (or (not (contains? profiles type)) (claimable? type))
    (get connectors type)))

(defn connector
  [type]
  (connector-in (connectors) type))

(defn profile-in
  [connectors type]
  (or (:profile (connector-in connectors type)) (get profiles type)))

(defn profile [type]
  (profile-in (connectors) type))

(defn file-format
  [type]
  (:file (profile type)))

(defn local?
  [{:keys [needs route] :as p}]
  (boolean (and p (not (some #{:egress} needs)) (not (contains? route :agent)))))

(defn csv-route?
  [p]
  (= #{:csv} (:route p)))

(defn all-profiles
  []
  (let [cs (connectors)]
    (into profiles (for [[t c] cs :when (connector-in cs t)] [t (:profile c)]))))

(defn read-mode-in
  [connectors {:keys [type]}]
  (let [{:keys [capabilities route]} (profile-in connectors type)]
    (cond (connector-in connectors type)  :connector
          (csv-route? {:route route})     :csv
          (contains? capabilities :sql)   :jdbc
          (contains? capabilities :land)  :landed
          (contains? capabilities :scan)  :rest)))

(defn read-mode
  [source]
  (read-mode-in (connectors) source))

(defn needs
  [{:keys [type external-transform]}]
  (if external-transform [:egress] (:needs (profile type) [:jdbc-url])))

(def field-ok?
  {:jdbc-url       #(seq (str (:jdbc-url %)))
   :host           #(seq (str (:host %)))
   :path           #(seq (str (:path %)))
   :remote-path    #(seq (str (:remote-path %)))
   :command        #(seq (str (:command %)))
   :allow-commands #(seq (:allow-commands %))
   :egress         #(seq (:egress %))})

(def Webhook
  [:or
   [:map {:closed true}
    [:kind [:= :table]] [:table :string] [:op [:enum :insert :update :delete]]
    [:description {:optional true} :string]]
   [:map {:closed true}
    [:kind [:= :table]]
    [:requests [:vector {:min 1}
                [:map {:closed true}
                 [:as :keyword] [:table :string] [:op [:enum :insert :update :delete]]
                 [:from {:optional true} [:map-of :keyword [:tuple :keyword :keyword]]]
                 [:optional? {:optional true} :boolean]
                 [:on-duplicate {:optional true} [:enum :bind :abort]]]]]
    [:description {:optional true} :string]]
   [:map {:closed true}
    [:method [:enum :get :post :put :patch :delete]]
    [:path-template :string]
    [:body-template {:optional true} any?]
    [:query {:optional true} [:map-of any? any?]]
    [:params {:optional true} [:sequential :keyword]]
    [:idempotency-key {:optional true} :string]
    [:description {:optional true} :string]]])

(def ^:private Request
  [:map
   [:method {:optional true} [:enum :get :post :put :delete]]
   [:path :string]
   [:headers {:optional true} [:map-of :string :string]]
   [:query {:optional true} [:map-of :string any?]]
   [:form {:optional true} [:map-of :string :string]]
   [:body {:optional true} any?]])

(def RestAuth
  [:multi {:dispatch :mode}
   [:session [:map
              [:mode [:= :session]]
              [:open Request]
              [:token-at [:vector [:or :keyword :string]]]
              [:header :string]
              [:prefix {:optional true} :string]
              [:close {:optional true} Request]
              [:headers {:optional true} [:map-of :string :string]]]]
   [:glpi-session [:map [:mode [:= :glpi-session]]]]
   [:bearer [:map [:mode [:= :bearer]]]]
   [:basic [:map [:mode [:= :basic]]]]
   [:api-key
    [:map [:mode [:= :api-key]] [:header {:optional true} :string]
     [:query {:optional true} :string]]]
   [:oauth2-client-credentials
    [:map [:mode [:= :oauth2-client-credentials]] [:token-url :string]]]])

(def RestResource
  [:map {:closed true}
   [:path :string]
   [:paging {:optional true}
    [:map
     [:style [:enum :none :range :offset :page :cursor :link-header]]
     [:size {:optional true} pos-int?]]]
   [:query {:optional true} [:map-of :string any?]]
   [:records-at {:optional true} [:vector [:or :keyword :string :int]]]
   [:key {:optional true} :keyword]
   [:columns {:optional true}
    [:vector [:map {:closed true} [:name :string] [:type (into [:enum] (sort vt/storable-types))]]]]
   [:filters {:optional true}
    [:map-of [:enum := :in :starts-with :< :<= :> :>=]
     [:map {:closed true}
      [:param :string]
      [:exact? {:optional true} :boolean]
      [:attrs {:optional true} [:set :keyword]]]]]
   [:description {:optional true} :string]])

(def Rest
  [:map
   [:preset {:optional true} [:enum :glpi]]
   [:base-url {:optional true} :string]
   [:tls-pin {:optional true} :string]
   [:auth {:optional true} RestAuth]
   [:resources {:optional true} [:map-of :keyword RestResource]]
   [:operations {:optional true} [:map-of :keyword Webhook]]
   [:read-itemtypes {:optional true} [:or [:set :string] [:sequential :string]]]])

(defn- need-ok?
  [spec k]
  (cond (= :egress-allowed k) true
        (field-ok? k)         ((field-ok? k) spec)
        :else                 (some? (get spec k))))

(defn lifted
  [{:keys [config] :as spec}]
  (if (map? config) (merge (dissoc spec :config) config) spec))

(defn- needs-check [type]
  [:fn {:error/fn (fn [{:keys [value]} _]
                    (let [value (lifted value)]
                      (str (name type) " source is missing "
                           (vec (remove #(need-ok? value %) (needs value))))))}
   (fn [spec] (let [spec (lifted spec)] (every? #(need-ok? spec %) (needs spec))))])

(def ^:private config-validator (memoize (fn [schema] (m/explainer schema))))

(defn- connector-problem
  [{:keys [type] :as spec}]
  (if-let [c (connector type)]
    (when-let [cfg (:config c)]
      (some->> ((config-validator cfg) (lifted spec)) me/humanize (str (name type) " source: ")))
    (when-not (contains? profiles type)
      (str ":type " (pr-str type) " is not a connector type — one of "
           (vec (sort (keys (all-profiles))))))))

(def ^:private connector-check
  [:fn {:error/fn (fn [{:keys [value]} _] (connector-problem value))}
   (fn [spec] (nil? (connector-problem spec)))])

(def ^:private common
  [:map {:closed false}
   [:egress {:optional true} [:or [:set :string] [:sequential :string]]]
   [:allow-commands {:optional true} [:or [:set :string] [:sequential :string]]]
   [:webhooks {:optional true} [:map-of :keyword Webhook]]
   [:config {:optional true} [:map {:closed false} [:rest {:optional true} Rest]]]
   [:rest {:optional true} Rest]])

(def SourceConn
  (-> [:multi {:dispatch :type}]
      (into (for [t (sort (keys profiles))]
              [t (cond-> [:and common [:map [:type [:= t]]] (needs-check t)]
                   (claimable? t)
                   (conj connector-check)
                   (= :command t)
                   (conj [:fn {:error/message ":command must be a member of :allow-commands"}
                          (fn [{:keys [command allow-commands external-transform]}]
                            (or external-transform (contains? (set allow-commands) command)))]))]))
      (conj [::m/default
             [:and common [:map [:type :keyword]] connector-check (needs-check :plugin)]])))

(defn file-source?
  [spec]
  (boolean (:files? (profile (:type spec)))))

(defn one-file?
  [spec]
  (some? (file-format (:type spec))))
