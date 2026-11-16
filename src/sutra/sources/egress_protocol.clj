(ns sutra.sources.egress-protocol
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [malli.core :as m]
            [malli.json-schema :as js]
            [sutra.foundation.boundaries :as bnd]))

(def version-file "boundaries/connectors/egress.openapi.json")

(def sidecar-file "boundaries/connectors/sidecar.openapi.json")

(def media-type "application/edn")

(def SourceSpec
  [:map
   [:source-name {:optional true} :string]
   [:type {:optional true} [:or :keyword :string]]
   [:agent {:optional true} [:or :keyword :string]]
   [:secret-cipher {:optional true} :string]
   [:config {:optional true} [:map-of :keyword :any]]])

(def ^:private common
  [[:timeout-ms {:optional true} pos-int?]
   [:span {:optional true} :any]])

(defn- req [& fields] (into [:map] (concat fields common)))

(def ^:private with-spec [:spec SourceSpec])

(def ^:private Name [:or :string :keyword])

(def Row [:map-of :keyword :any])

(def routes
  {"/pubkey"
   {:summary "The agent's public key, to seal credentials to. Needs no token."
    :open?   true
    :request (req)
    :result  [:map [:public-key :string] [:alg [:= "RSA-OAEP-256"]]]}

   "/status"
   {:summary "Who the agent is, its key fingerprint, open write sessions, connectors."
    :request (req)
    :result  [:map [:agent :string] [:alg :string] [:fingerprint :string]
              [:started-at {:optional true} [:maybe int?]]
              [:uptime-ms {:optional true} [:maybe int?]]
              [:sessions [:vector [:map [:tx :string] [:source [:maybe :string]]
                                   [:age-ms int?] [:expires-in-ms int?]]]]
              [:connectors [:map-of :keyword [:map-of :keyword :any]]]
              [:java {:optional true} :string]
              [:trace {:optional true} :map] [:heap {:optional true} :map]]}

   "/reach"
   {:summary "Read rows of a JDBC source by query, or land a file source and name its path."
    :request (req with-spec [:query {:optional true} :string]
                  [:params {:optional true} [:vector :any]])
    :result  [:or [:map [:rows [:vector Row]]] [:map [:path :string]]]}

   "/connect"
   {:summary "Probe TCP reachability of host:port endpoints from the agent (≤64)."
    :request (req [:endpoints [:vector :string]])
    :result  [:map [:endpoints [:vector [:map [:endpoint :string] [:open :boolean]
                                         [:ms {:optional true} int?]
                                         [:error {:optional true} :string]]]]]}

   "/write"
   {:summary "Apply a write plan to a JDBC source, alone or inside an open session :tx."
    :request (req with-spec [:write-spec :map] [:tx {:optional true} :string])
    :result  :any}

   "/write-begin"
   {:summary "Open a write session (one transaction, ≤120 s idle) on a JDBC source."
    :request (req with-spec)
    :result  [:map [:tx :string]]}

   "/write-end"
   {:summary "Commit or roll back a write session."
    :request (req [:tx :string] [:commit {:optional true} :boolean])
    :result  [:map [:ended :string] [:gone {:optional true} :boolean]]}

   "/webhook"
   {:summary "Call a webhook of a REST source; CloudEvents attributes ride as ce-* headers."
    :request (req with-spec [:webhook-id Name] [:params {:optional true} :any]
                  [:idempotency-key {:optional true} :string]
                  [:cloudevent {:optional true} [:map-of :keyword :any]])
    :result  :any}

   "/rest"
   {:summary "Catalogue and scan a REST source."
    :request (req with-spec [:op [:enum :tables :columns :scan-profile :scan]]
                  [:table {:optional true} Name] [:scan {:optional true} :map])
    :result  [:map [:result :any]]}

   "/connector"
   {:summary "Run one operation of a plugin or sidecar connector (sutra.sources.protocols)."
    :request (req with-spec
                  [:op [:enum :probe :tables :columns :scan-profile :scan :write :invoke :token]]
                  [:table {:optional true} Name] [:scan {:optional true} :map]
                  [:write-spec {:optional true} :map] [:webhook-id {:optional true} Name]
                  [:params {:optional true} :any] [:opts {:optional true} :map])
    :result  [:map [:result :any]]}

   "/listen"
   {:summary "Wait for a change in IMAP folders, up to window-ms (IMAP IDLE)."
    :request (req with-spec [:window-ms pos-int?] [:folders {:optional true} [:vector :string]]
                  [:state {:optional true} :any])
    :result  :any}

   "/secret"
   {:summary "The decrypted credential fields of a source marked :external-transform."
    :request (req with-spec)
    :result  [:map [:fields [:map-of :keyword :any]]]}

   "/chat"
   {:summary "One language-model chat call through the source's wire (B-10)."
    :request (req with-spec [:payload :map])
    :result  [:map [:status int?] [:latency-ms int?] [:body :any]]}

   "/embed"
   {:summary "One embedding call through the source's wire (B-10)."
    :request (req with-spec [:payload :map])
    :result  [:map [:status int?] [:latency-ms int?] [:body :any]]}

   "/models"
   {:summary "The model inventory of a language-model source."
    :request (req with-spec [:models-path {:optional true} [:maybe :string]])
    :result  [:map [:status int?] [:latency-ms int?] [:body :string]]}

   "/trace-arm"
   {:summary "Arm or disarm the agent's SQL trace (operator)."
    :request (req [:on :boolean] [:file {:optional true} [:maybe :string]]
                  [:capture-params? {:optional true} :boolean])
    :result  :any}

   "/trace"
   {:summary "Traced statements after a cursor (operator)."
    :request (req [:after int?])
    :result  :any}

   "/trace-kill"
   {:summary "Cancel a traced statement, or all (operator)."
    :request (req [:all {:optional true} :boolean] [:seq {:optional true} int?])
    :result  :any}})

(def Envelope
  [:or
   [:map [:ok [:= true]] [:result :any]]
   [:map [:ok [:= false]] [:error :string] [:data {:optional true} [:map-of :keyword :any]]]])

(defn valid-request?
  [route payload]
  (m/validate (get-in routes [route :request]) payload))

(defn explain-request [route payload]
  (m/explain (get-in routes [route :request]) payload))

(defn- jschema [s] (js/transform s))

(defn- envelope-of [result]
  {:oneOf [{:type "object" :required ["ok" "result"]
            :properties {:ok {:const true} :result (jschema result)}}
           {:type "object" :required ["ok" "error"]
            :properties {:ok {:const false} :error {:type "string"}
                         :data {:type "object"}}}]})

(def ^:private envelope-ref {:$ref "#/components/schemas/Envelope"})

(defn document
  []
  (let [version (get-in (bnd/register) [:boundaries :boundary/connectors :version])]
    {:openapi "3.1.0"
     :info {:title "Sutra egress agent protocol"
            :version version
            :license {:name "Apache-2.0" :identifier "Apache-2.0"}
            :description
            (str "Boundary B-8: what a platform process sends an egress "
                 "agent, the out-of-process boundary a third-party connector implements. "
                 "Bodies are EDN (application/edn); the schemas describe the EDN data model "
                 "(a keyword is a string here). Generated from "
                 "src/sutra/sources/egress_protocol.clj; do not edit by hand.")}
     :components
     {:securitySchemes
      {:agentToken {:type "http" :scheme "bearer"
                    :description (str "HMAC-SHA256 of the agent's name under the deployment's "
                                      "egress master secret, hex; or a token file given to "
                                      "the agent (SUTRA_EGRESS_TOKEN_FILE).")}}
      :schemas {:SourceSpec (jschema SourceSpec) :Envelope (jschema Envelope)}
      :parameters
      {:requestKey {:name "x-sutra-request-key" :in "header" :required false
                    :schema {:type "string"}
                    :description "A retry with the same key attaches to the running work."}
       :waitMs {:name "x-sutra-wait-ms" :in "header" :required false
                :schema {:type "integer"}
                :description "How long the caller waits; past it the work is cancelled."}}}
     :paths
     (into {}
           (for [[path {:keys [summary request result open?]}] routes]
             [path {:post (cond-> {:operationId (str/replace (subs path 1) #"-" "_")
                                   :summary summary
                                   :parameters [{:$ref "#/components/parameters/requestKey"}
                                                {:$ref "#/components/parameters/waitMs"}]
                                   :requestBody {:required true
                                                 :content {media-type
                                                           {:schema (jschema request)}}}
                                   :responses
                                   {"200" {:description "Done."
                                           :content {media-type
                                                     {:schema (envelope-of result)}}}
                                    "400" {:description "Refused or failed: {:ok false}."
                                           :content {media-type {:schema envelope-ref}}}
                                    "401" {:description "Missing or wrong agent token."}
                                    "404" {:description "No such route."}}}
                            open?       (assoc :security [])
                            (not open?) (assoc :security [{:agentToken []}]))}]))}))

(def ScanProfile
  [:map [:preds [:map-of Name [:enum "exact" "inexact" :exact :inexact]]]
   [:project? {:optional true} :boolean] [:page? {:optional true} :boolean]
   [:compound? {:optional true} :boolean]])

(def sidecar-routes
  {"/describe"
   {:summary "The connector types this sidecar serves, their capabilities and form."
    :request [:map]
    :result  [:map [:types [:map-of Name
                            [:map [:profile [:map [:capabilities [:vector Name]]
                                             [:route {:optional true} [:vector Name]]
                                             [:endpoint {:optional true} Name]
                                             [:needs {:optional true} [:vector Name]]
                                             [:scan {:optional true} ScanProfile]]]
                             [:form {:optional true} :map]]]]]}
   "/op"
   {:summary "One connector operation on the inline source spec (credentials decrypted)."
    :request [:map
              [:op [:enum :probe :tables :columns :scan-profile :scan :write :invoke :token]]
              [:type Name] [:spec :map]
              [:table {:optional true} Name] [:scan {:optional true} :map]
              [:plan {:optional true} :map] [:webhook-id {:optional true} Name]
              [:params {:optional true} :any] [:opts {:optional true} :map]]
    :result  :any}})

(defn valid-sidecar-request? [route payload]
  (m/validate (get-in sidecar-routes [route :request]) payload))

(defn sidecar-document
  []
  (let [version (get-in (bnd/register) [:boundaries :boundary/connectors :version])]
    {:openapi "3.1.0"
     :info {:title "Sutra connector sidecar protocol"
            :version version
            :license {:name "Apache-2.0" :identifier "Apache-2.0"}
            :description
            (str "Boundary B-8: what an egress agent sends a connector "
                 "sidecar, a process in any language that implements connector types. "
                 "Generated from src/sutra/sources/egress_protocol.clj; do not edit by hand.")}
     :components {:securitySchemes
                  {:sidecarToken {:type "http" :scheme "bearer"
                                  :description "SUTRA_CONNECTOR_SIDECAR_TOKEN_FILE's token."}}}
     :security [{:sidecarToken []}]
     :paths
     (into {}
           (for [[path {:keys [summary request result]}] sidecar-routes]
             [path {:post {:operationId (subs path 1)
                           :summary summary
                           :requestBody {:required true
                                         :content {"application/json"
                                                   {:schema (jschema request)}}}
                           :responses
                           {"200" {:description "{result …} or {error {code message}}."
                                   :content
                                   {"application/json"
                                    {:schema
                                     {:oneOf [{:type "object" :required ["result"]
                                               :properties {:result (jschema result)}}
                                              {:type "object" :required ["error"]
                                               :properties
                                               {:error {:type "object"
                                                        :required ["code" "message"]
                                                        :properties
                                                        {:code {:type "string"}
                                                         :message {:type "string"}}}}}]}}}}}}}]))}))

(defn- text [k] (if (keyword? k) (subs (str k) 1) (str k)))

(defn- stable [x]
  (walk/postwalk (fn [v]
                   (cond (map? v)     (into (sorted-map) (map (fn [[k x]] [(text k) x])) v)
                         (keyword? v) (text v)
                         :else        v))
                 x))

(defn render
  ([] (render (document)))
  ([doc] (with-out-str (json/pprint (stable doc) :escape-slash false))))

(defn -main [& _]
  (doseq [[f doc] [[version-file (document)] [sidecar-file (sidecar-document)]]]
    (io/make-parents f)
    (spit f (render doc))
    (println "wrote" f)))
