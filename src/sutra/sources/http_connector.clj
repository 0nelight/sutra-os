(ns sutra.sources.http-connector
  (:require [sutra.kit.noted :as noted]
            [clojure.data.json :as json]
            [clojure.set :as set]
            [clojure.string :as str]
            [sutra.foundation.outbound :as out]
            [sutra.sources.cloudevents :as cloudevents]
            [sutra.sources.protocols :as api]
            [sutra.types.decimal :as d]
            [sutra.foundation.encoding :as enc]
            [sutra.kit.refusal :as refusal]
            [sutra.sources.tls :as tls]
            [sutra.types.source :as source])
  (:import (java.net URI)
           (java.util Base64)))

(set! *warn-on-reflection* true)

(defn- embedded [v] (if (d/exact? v) (d/plain v) (str v)))

(defn render
  [tmpl params]
  (cond
    (string? tmpl)
    (if-let [[_ k] (re-matches #"\{([a-z0-9_-]+)\}" tmpl)]
      (get params (keyword k))
      (str/replace tmpl #"\{([a-z0-9_-]+)\}" (fn [[_ k]] (embedded (get params (keyword k))))))
    (map? tmpl)    (into {} (map (fn [[k v]] [k (render v params)])) tmpl)
    (vector? tmpl) (mapv #(render % params) tmpl)
    :else tmpl))

(defn- url-part
  [v]
  (-> (java.net.URLEncoder/encode (str (embedded v)) "UTF-8")
      (str/replace "+" "%20")))

(defn render-path
  [tmpl params]
  (str/replace (str tmpl) #"\{([a-z0-9_-]+)\}"
               (fn [[_ k]] (url-part (get params (keyword k))))))

(defn- rendered-headers
  [headers auth]
  (into {} (keep (fn [[h tmpl]]
                   (when (every? #(some? (get auth (keyword %)))
                                 (map second (re-seq #"\{([a-z0-9_-]+)\}" tmpl)))
                     [h (str (render tmpl auth))])))
        headers))

(defn- url-host-port
  [url]
  (let [u (URI. (str url))
        p (.getPort u)]
    [(.getHost u)
     (if (neg? p) (if (= "https" (.getScheme u)) 443 80) p)]))

(defn assert-egress-allowed!
  [{:keys [egress]} url]
  (let [[host port] (url-host-port url)
        target (str host ":" port)
        allow  (set (map str egress))]
    (when-not (or (contains? allow target) (contains? allow host))
      (refusal/refuse! :refusal.foreign/rest-egress-not-allowed {:target target :allow allow}))))

(def glpi-session
  {:mode     :session
   :open     {:path "/apirest.php/initSession" :headers {"Authorization" "user_token {user-token}"}}
   :token-at [:session_token]
   :header   "Session-Token"
   :close    {:path "/apirest.php/killSession"}
   :headers  {"App-Token" "{app-token}"}})

(defn- glpi-resources [itemtypes]
  (into (sorted-map)
        (for [it itemtypes]
          [(keyword it) {:path   (str "/apirest.php/" it)
                         :paging {:style :range :size 500}
                         :query  {"expand_dropdowns" "false" "get_hateoas" "false"}
                         :key    :id}])))

(defn declaration
  [spec]
  (let [r    (or (:rest spec) (get-in spec [:config :rest]) {})
        base (or (:base-url r) (:base-url spec) (get-in spec [:config :base-url]))
        glpi (= :glpi (or (:preset r) (:preset (source/profile (:type spec)))))
        auth (:auth r)]
    (cond-> (assoc r :base-url base
                   :operations (merge (:webhooks spec) (:operations r)))
      (= :glpi-session (:mode auth)) (assoc :auth (merge (dissoc auth :mode) glpi-session))
      glpi                           (update :resources
                                             #(merge (glpi-resources (:read-itemtypes r)) %)))))

(defn- request-url [base path] (str (str/replace (str base) #"/+$" "") path))

(defn- body-json [body] (json/read-str (if (string? body) body (slurp body))))

(defn- checked
  [what {:keys [status body error] :as resp} ok?]
  (when error (refusal/refuse! :refusal.sources/http-transport-error {:error (str error)} {:what what}))
  (when-not (ok? status)
    (refusal/refuse! :refusal.sources/http-status {:status status :body (enc/clip 300 (str body))}
                     {:what what}))
  resp)

(defn- template-request [{:keys [base-url]} {:keys [method path headers query form body]} auth]
  {:method       (or method (if form :post :get))
   :url          (request-url base-url (render path auth))
   :headers      (rendered-headers (or headers {}) auth)
   :query-params (render query auth)
   :form-params  (render form auth)
   :body         (some-> body (render auth) json/write-str)
   :timeout      30000})

(defn- with-auth-headers
  [req auth]
  (update req :headers #(merge (rendered-headers (:headers auth) auth) %)))

(def auth-modes
  {:session
   {:open  (fn [{:keys [send] :as ctx} {:keys [open token-at] :as auth}]
             (let [resp (checked "session open"
                                 (send (with-auth-headers (template-request ctx open auth) auth))
                                 #{200})]
               (or (get-in (body-json (:body resp)) (mapv #(if (keyword? %) (name %) %) token-at))
                   (refusal/refuse! :refusal.sources/session-no-token {:token-at token-at}))))
    :apply (fn [req token {:keys [header prefix]}]
             (assoc-in req [:headers header] (str prefix token)))
    :close (fn [{:keys [send] :as ctx} {:keys [close header prefix] :as auth} token]
             (when close
               (noted/quietly "http connector: session close"
                 (send (-> (template-request ctx close auth)
                           (with-auth-headers auth)
                           (assoc-in [:headers header] (str prefix token))
                           (assoc :timeout 15000))))))}
   :bearer
   {:apply (fn [req _ {:keys [token]}]
             (assoc-in req [:headers "Authorization"] (str "Bearer " token)))}
   :basic
   {:apply (fn [req _ {:keys [user password]}]
             (assoc-in req [:headers "Authorization"]
                       (str "Basic "
                            (.encodeToString (Base64/getEncoder)
                                             (.getBytes (str user ":" password) "UTF-8")))))}
   :api-key
   {:apply (fn [req _ {:keys [header query key]}]
             (if query
               (assoc-in req [:query-params query] key)
               (assoc-in req [:headers (or header "X-API-Key")] key)))}
   :oauth2-client-credentials
   {:open  (fn [{:keys [send] :as ctx} {:keys [token-url client-id client-secret scope]}]
             (assert-egress-allowed! (:spec ctx) token-url)
             (let [resp (checked "oauth2 token"
                                 (send {:method :post :url token-url :timeout 30000
                                        :form-params (cond-> {"grant_type" "client_credentials"
                                                              "client_id" client-id
                                                              "client_secret" client-secret}
                                                       scope (assoc "scope" scope))})
                                 #{200})]
               (get (body-json (:body resp)) "access_token")))
    :apply (fn [req token _] (assoc-in req [:headers "Authorization"] (str "Bearer " token)))}})

(defn- mode-of [auth]
  (when auth
    (or (get auth-modes (:mode auth))
        (refusal/refuse! :refusal.foreign/rest-auth-mode {:mode (:mode auth)}
                         {:mode (str (:mode auth))}))))

(defn with-auth
  [ctx auth f]
  (let [{:keys [open apply close]} (mode-of auth)
        state (when open (open ctx auth))
        authorize (fn [req]
                    (cond-> (update req :headers merge (rendered-headers (:headers auth) auth))
                      apply (apply state auth)))]
    (try (f authorize)
         (finally (when close (close ctx auth state))))))

(defn- records [body records-at]
  (let [parsed (body-json body)
        at     (get-in parsed (mapv #(if (keyword? %) (name %) %) (or records-at [])))]
    (cond (map? at) [at] (nil? at) [] :else (vec at))))

(defn- next-link [headers]
  (some (fn [part] (second (re-find #"<([^>]+)>\s*;\s*rel=\"?next\"?" part)))
        (str/split (str (or (get headers :link) (get headers "link") (get headers "Link"))) #",")))

(defn fetch
  [{:keys [send base-url spec]} authorize {:keys [path paging records-at] :as res} query limit]
  (let [{:keys [style size cursor-at cursor-param offset-param limit-param page-param size-param]
         :or   {style :none size 100 cursor-param "cursor" offset-param "offset" limit-param "limit"
                page-param "page" size-param "per_page"}} paging
        base-q (merge (:query res) query)
        get!   (fn [url q]
                 (checked (str "read of " path)
                          (send (authorize {:method :get :url url :query-params q :timeout 60000}))
                          #{200 206}))
        enough? #(and limit (>= (count %) limit))
        done    (fn [acc] (cond-> acc limit (->> (take limit) vec)))]
    (loop [acc [] n 0 url (request-url base-url path) cursor nil]
      (let [q    (case style
                   :range  (assoc base-q "range" (str n "-" (+ n size -1)))
                   :offset (assoc base-q offset-param n limit-param size)
                   :page   (assoc base-q page-param (inc (quot n size)) size-param size)
                   :cursor (cond-> (assoc base-q limit-param size)
                             cursor (assoc cursor-param cursor))
                   base-q)
            resp (get! url (if (and (= :link-header style) (pos? n)) nil q))
            rows (records (:body resp) records-at)
            acc  (into acc rows)
            n'   (+ n (count rows))]
        (cond
          (or (enough? acc) (empty? rows)) (done acc)
          (#{:range :offset
             :page} style)  (if (< (count rows) size) (done acc) (recur acc n' url nil))
          (= :cursor style)                (if-let [c (when (seq cursor-at)
                                                        (get-in (body-json (:body resp))
                                                                (mapv name cursor-at)))]
                                             (recur acc n' url c)
                                             (done acc))
          (= :link-header style)           (if-let [nxt (next-link (:headers resp))]
                                             (do (assert-egress-allowed! spec nxt)
                                                 (recur acc n' nxt nil))
                                             (done acc))
          :else                            (done acc))))))

(defn- as-is
  [v]
  (if (coll? v) (json/write-str v) v))

(defn- declared-keys
  [{:keys [columns]}]
  (when (seq columns)
    (mapv #(keyword (str/lower-case (:name %))) columns)))

(defn- row-out [rec]
  (if (map? rec)
    (into {} (map (fn [[k v]] [(keyword (str/lower-case (name k))) (as-is v)])) rec)
    {:value (as-is rec)}))

(defn- ontology-type
  [v]
  (cond (boolean? v) :boolean (integer? v) :long (decimal? v) :decimal (number? v) :double
    :else :string))

(defn declared-columns
  [spec table]
  (when-let [cs (seq (get-in (declaration spec) [:resources (keyword table) :columns]))]
    (mapv (fn [{n :name t :type}] {:name n :type (name t) :ontology-type t}) cs)))

(defn scan-profile
  [{:keys [filters]}]
  {:preds     (into {} (for [[op {:keys [exact? attrs]}] filters]
                         [op (if (and exact? (empty? attrs)) :exact :inexact)]))
   :project?  false
   :page?     true
   :compound? false})

(defn- filter-query
  [{:keys [filters]} where]
  (into {} (keep (fn [[op a v]]
                   (when-let [{:keys [param attrs]} (get filters op)]
                     (when (or (empty? attrs) (contains? attrs a))
                       [(render param {:attr (name a)}) (embedded v)]))))
        where))

(defn- default-send [req] (out/request! req))

(defn- resource [decl table]
  (or (get-in decl [:resources (keyword table)])
      (refusal/refuse! :refusal.foreign/rest-resource-unknown
                       {:resource table :declared (vec (keys (:resources decl)))}
                       {:resource (str table)})))

(def ^:private max-redirects 5)

(defn- redirect-target [{:keys [status headers]} url]
  (when-let [loc (and (#{301 302 303 307 308} status)
                      (or (:location headers) (get headers "location")))]
    (str (.resolve (java.net.URI. (str url)) (str loc)))))

(defn following-redirects
  [send spec]
  (fn [req]
    (loop [req req hops 0]
      (let [resp (send req)]
        (if-let [url (redirect-target resp (:url req))]
          (do (when (>= hops max-redirects)
                (refusal/refuse! :refusal.foreign/rest-too-many-redirects {:url url}))
              (assert-egress-allowed! spec url)
              (recur (cond-> (-> req (assoc :url url) (dissoc :query-params))
                       (#{301 302 303} (:status resp))
                       (-> (assoc :method :get) (dissoc :body :form-params)))
                     (inc hops)))
          resp)))))

(defn- ctx-of [{:keys [spec decl send]}]
  (assert-egress-allowed! spec (:base-url decl))
  {:send (following-redirects send spec) :base-url (:base-url decl) :spec spec})

(defn- read! [src table query limit]
  (let [ctx (ctx-of src)
        res (resource (:decl src) table)]
    (with-auth ctx (get-in src [:decl :auth]) #(fetch ctx % res query limit))))

(defn- params-checked [wh webhook-id params]
  (let [declared (set (:params wh))
        supplied (set (keys params))]
    (when-not (= declared supplied)
      (refusal/refuse! :refusal.foreign/webhook-params
                       {:webhook-id webhook-id :declared declared :supplied supplied
                        :missing (set/difference declared supplied)
                        :extra   (set/difference supplied declared)}))
    wh))

(defn invoke
  [{:keys [decl] :as src} webhook-id params {:keys [timeout-ms idempotency-key cloudevent]}]
  (let [wh  (or (get-in decl [:operations webhook-id])
                (refusal/refuse! :refusal.foreign/webhook-not-allowed
                                 {:webhook-id webhook-id :allowed (vec (keys (:operations decl)))}))
        _   (params-checked wh webhook-id params)
        ctx (ctx-of src)]
    (with-auth ctx (:auth decl)
      (fn [authorize]
        (let [{:keys [status body error]}
              ((:send ctx)
               (authorize
                {:method       (:method wh)
                 :url          (request-url (:base-url decl)
                                            (render-path (:path-template wh) params))
                 :query-params (:query wh)
                 :headers      (let [k (if (:idempotency-key wh)
                                         (str (render (:idempotency-key wh) params))
                                         idempotency-key)]
                                 (cond-> (assoc (some-> cloudevent cloudevents/headers)
                                                "Content-Type" "application/json")
                                   k (assoc "Idempotency-Key" k)))
                 :body         (some-> (:body-template wh) (render params) json/write-str)
                 :timeout      (min 120000 (or timeout-ms 120000))}))]
          (when error (refusal/refuse! :refusal.sources/http-transport-error {:error (str error)}
                       {:what "webhook"}))
          {:status status :body (enc/clip 500 (if (string? body) body (some-> body slurp)))})))))

(defrecord RestSource [spec decl send]
  api/Source
  (-profile [_] (source/profile :rest))
  (-probe [this] (when-let [t (first (keys (:resources decl)))] (read! this t nil 1)) nil)
  (-close [_] nil)
  api/Catalog
  (-tables [_] (vec (sort (map name (keys (:resources decl))))))
  (-columns [this table]
    (or (declared-columns spec table)
        (vec (for [[k v] (first (map row-out (read! this table nil 1)))]
               {:name (name k) :type "json" :ontology-type (ontology-type v)}))))
  (-relations [_] [])
  api/Scan
  (-scan-profile [_ table] (scan-profile (resource decl table)))
  (-scan [this table {:keys [where limit]}]
    (let [res  (resource decl table)
          keep (declared-keys res)]
      (mapv (if keep #(select-keys (row-out %) keep) row-out)
            (read! this table (filter-query res where) limit))))
  api/Invoke
  (-invoke! [this webhook-id params opts] (invoke this webhook-id params opts)))

(defn- pinned
  [send pin]
  (fn [req]
    (send (cond-> req
            (str/starts-with? (str (:url req)) "https:")
            (assoc :sslengine (tls/pinned-engine pin (:url req)))))))

(defn source
  ([spec] (source spec {}))
  ([spec {:keys [send] :or {send default-send}}]
   (let [decl (declaration spec)]
     (->RestSource spec decl (cond-> send (:tls-pin decl) (pinned (:tls-pin decl)))))))

(defn records-of
  [src table]
  (read! src table nil nil))
