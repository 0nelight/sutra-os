(ns platform-sdk.client
  (:require [clojure.string :as str]
            [platform-sdk.errors :as err]
            [sutra.types.paths :as paths]
            [sutra.wire.codec :as codec]
            [sutra.wire.protocol :as protocol]))

(def ^:dynamic *client* nil)

(defn client? [c] (and (map? c) (string? (:base-url c)) (satisfies? IDeref (:token c))))

(defn current [] *client*)

(defn check!
  [c label]
  (when (and (some? c) (not (client? c)))
    (err/raise! :usage
                (str label ": the first argument is a client, or nil for *client* — got "
                     (pr-str c))
                {:got c}))
  c)

(defn- on-branch
  [path branch]
  (if branch
    (str path (if (str/includes? path "?") "&" "?") "branch=" (js/encodeURIComponent branch))
    path))

(defn- exchange
  [{:keys [base-url branch token]} method path body]
  (-> (js/fetch (str base-url (on-branch path branch))
                (clj->js (cond-> {:method  (str/upper-case (name method))
                                  :headers (cond-> {"Accept"        "application/transit+json"
                                                    "Content-Type"  "application/transit+json"
                                                    protocol/header (str protocol/version)}
                                             @token (assoc "Authorization" (str "Bearer " @token)))}
                           (some? body) (assoc :body (codec/encode body)))))
      (.then (fn [^js r]
               (.then (.text r)
                      (fn [text]
                        {:status (.-status r)
                         :body   (when (seq text)
                                   (try (codec/decode text) (catch :default _ text)))}))))))

(defn- token-answer
  [c {:keys [status body] :as resp}]
  (when-not (:access_token body)
    (err/check! (assoc resp :status (if (<= 200 status 299) 401 status)) {:endpoint :oauth-token}))
  (reset! (:token c) (:access_token body))
  c)

(defn login!
  [{:keys [client-id client-secret] :as c}]
  (when-not (and client-id client-secret)
    (err/raise! :auth "this client holds no :client-id/:client-secret to log in with" {}))
  (reset! (:token c) nil)
  (-> (exchange c :post (paths/address :oauth-token)
                {:grant_type "client_credentials" :client_id client-id
                 :client_secret client-secret})
      (.then #(token-answer c %))))

(defn fresh-token!
  [c]
  (if-let [t @(:token c)]
    (js/Promise.resolve t)
    (.then (login! c) (fn [c] @(:token c)))))

(defn request
  ([c method endpoint] (request c method endpoint nil nil))
  ([c method endpoint params] (request c method endpoint params nil))
  ([c method endpoint params body]
   (let [c    (or c *client* (err/raise! :usage "no client" {:endpoint endpoint}))
         path (paths/address endpoint params)
         ctx  {:endpoint endpoint :method method :path path}
         send (fn [] (.then (fresh-token! c) (fn [_] (exchange c method path body))))]
     (check! c (str "request " endpoint))
     (-> (send)
         (.then (fn [resp]
                  (if (and (= 401 (:status resp)) (:client-id c))
                    (do (reset! (:token c) nil) (send))
                    resp)))
         (.then #(err/check! % ctx))))))

(defn client
  [{:keys [base-url token client-id client-secret branch]}]
  (let [base (some-> base-url str/trim (str/replace #"/+$" ""))]
    (when-not (seq base)
      (err/raise! :usage ":base-url is required" {}))
    {:base-url base :token (atom token) :client-id client-id :client-secret client-secret
     :branch branch}))
