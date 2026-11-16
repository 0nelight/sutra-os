(ns sutra.access.oauth
  (:require [sutra.kit.digest :as digest]
            [clojure.string :as str]
            [sutra.access.password :as password]
            [sutra.kit.lifecycle :as lc]
            [sutra.store.control :as ctl]
            [sutra.store.control.lifecycle :as lifecycle]
            [sutra.access.principals :as pr])
  (:import [java.util UUID]))

(set! *warn-on-reflection* true)

(def ^:const code-ttl-seconds 120)

(def ^:const app-scope-prefix "app:")

(def ^:const ops-marker "ops:")

(defn app-scope
  [app-rid operations]
  (str app-scope-prefix app-rid
       " " ops-marker (str/join "," (sort (map name operations)))))

(defn app-scoped?
  [sess]
  (some-> (:scope sess) (str/starts-with? app-scope-prefix)))

(defn scoped-app-rid
  [sess]
  (when (app-scoped? sess)
    (-> (:scope sess) (subs (count app-scope-prefix)) (str/split #"\s+") first)))

(defn granted-operations
  [sess]
  (let [m (second (re-find #"\bops:(\S*)" (str (:scope sess))))]
    (if (str/blank? m)
      #{}
      (into #{} (comp (remove str/blank?) (map keyword)) (str/split m #",")))))

(defn- salted
  [secret]
  (digest/salted (password/random-token 16) secret))

(declare grants?)

(defn register-client!
  [store {:keys [app app-name grant-types redirect-uris created-by]}]
  (let [client-id (str "cid-" (subs (str (UUID/randomUUID)) 0 18))
        secret    (password/random-token 32)
        grants    (vec (or grant-types [:authorization-code]))
        svc       (when (grants? {:grant-types grants} :client-credentials) client-id)
        row       {:client-id client-id :app (str app) :app-name (name app-name)
                   :secret-hash (salted secret) :secret-gen 0
                   :grant-types grants
                   :redirect-uris (vec (or redirect-uris []))
                   :service-user svc :created-by created-by :created-at ctl/now}]
    (ctl/commit! store (cond-> [[:swap :oauth-clients {:client-id client-id} (fn [_] row)]]
                         svc (conj [:plan :principals (pr/registering-service-user svc)])))
    (cond-> {:client-id client-id :client-secret secret :app app :app-name app-name}
      svc (assoc :service-user svc))))

(defn own-service-user?
  [client]
  (and (some? (:service-user client)) (= (:service-user client) (:client-id client))))

(defn fetch-client
  [store client-id]
  (when client-id
    (let [c (ctl/get store :oauth-clients {:client-id client-id})]
      (when-not (:revoked-at c) c))))

(defn clients-of
  [store app-rid]
  (vec (ctl/list store :oauth-clients {:app (str app-rid)} {:limit 200})))

(defn- secret-ok? [client secret]
  (boolean (and (:secret-hash client) (digest/salted-ok? (str secret) (:secret-hash client)))))

(def client-lifecycle
  (lc/table {:name   :oauth-client
             :states #{:active :revoked}
             :final  #{:revoked}
             :edges  {:rotate {:from #{:active} :to :active}
                      :issue  {:from #{:active} :to :active
                               :guard (fn [client {:keys [secret]}]
                                        (when-not (secret-ok? client secret)
                                          "the secret is not the client's current one"))}
                      :revoke {:from #{:active} :to :revoked}}}))

(defn- client-state [row] (when row (if (:revoked-at row) :revoked :active)))

(defn- refused? [e]
  (= :refusal.lifecycle/transition-refused (:refusal/code (ex-data e))))

(defn- client-transitioning [client-id event input sets]
  (lifecycle/transitioning :oauth-clients {:client-id client-id} client-lifecycle event
                           {:id client-id :state-of client-state :input input :sets sets}))

(defn- refusable
  [f]
  (try (f)
       (catch clojure.lang.ExceptionInfo e
         (if (refused? e) nil (throw e)))))

(defn revoke-client!
  [store client-id]
  (boolean
   (refusable
    #(ctl/commit! store [[:plan :oauth-clients
                          (client-transitioning client-id :revoke nil
                                                (fn [_ _] {:revoked-at ctl/now}))]
                         [:delete :sessions {:client-id client-id}]]))))

(defn rotate-secret!
  [store client-id]
  (let [secret (password/random-token 32)]
    (when (refusable
           #(ctl/commit! store
                         [[:plan :oauth-clients
                           (client-transitioning
                            client-id :rotate nil
                            (fn [old _] {:secret-hash (salted secret)
                                         :secret-gen (inc (long (or (:secret-gen old) 0)))}))]]))
      {:client-id client-id :client-secret secret})))

(defn authenticate-client
  [store client-id client-secret]
  (when-let [c (fetch-client store client-id)]
    (when (secret-ok? c client-secret) c)))

(defn issuing!
  [store client-id secret f]
  (refusable
   #(first (ctl/commit!
            store
            [[:plan :oauth-clients
              (fn [store at]
                (let [{c :result :as issue} ((client-transitioning client-id :issue
                                                                   {:secret secret}
                                                                   (fn [_ _] {}))
                                             store at)
                      {r :result :as out}   (f store at c)]
                  (if (= 200 (:status r))
                    {:records (into (vec (:records issue)) (:records out)) :result r}
                    {:result r})))]]))))

(defn grant-key
  [g]
  (keyword (str/replace (name g) "_" "-")))

(defn grants?
  [client grant]
  (let [gs (into #{} (map grant-key) (or (:grant-types client) []))]
    (contains? gs (grant-key grant))))

(defn redirect-allowed?
  [client uri]
  (let [allowed (set (map str (or (:redirect-uris client) [])))]
    (contains? allowed (str uri))))

(defn- code-key [code] (digest/sha256-hex code))

(def ^:private verifier-shape #"[A-Za-z0-9\-._~]{43,128}")

(defn challenge-of
  [verifier]
  (.encodeToString (.withoutPadding (java.util.Base64/getUrlEncoder))
                   (digest/sha256 (str verifier))))

(defn challenge-problem
  [challenge method]
  (cond
    (str/blank? (str challenge)) "code_challenge is required (PKCE, S256)"
    (not= "S256" (str method))   "code_challenge_method must be S256"
    (not (re-matches #"[A-Za-z0-9_-]{43}" (str challenge)))
    "code_challenge is not a base64url SHA-256"))

(defn verifier-ok?
  [challenge verifier]
  (boolean (and (seq challenge) (re-matches verifier-shape (str verifier))
                (digest/same? challenge (challenge-of verifier)))))

(defn mint-code!
  [store {:keys [client-id user-id username redirect-uri scope code-challenge nonce auth-time]}]
  (let [code (password/random-token 32)]
    (ctl/insert! store :oauth-codes
                 {:code (code-key code) :client-id client-id :user-id user-id
                  :username username :code-challenge code-challenge
                  :nonce nonce :auth-time auth-time
                  :redirect-uri redirect-uri :scope scope :created-at ctl/now
                  :expires-at
                    (java.util.Date. (+ (ctl/millis store) (* 1000 code-ttl-seconds)))})
    code))

(defn redeeming
  [code client-id]
  (fn [store _]
    (let [k   (some-> code str not-empty code-key)
          row (when k (ctl/get store :oauth-codes {:code k}))]
      (if (and row (= client-id (:client-id row))
               (.after ^java.util.Date (:expires-at row) (ctl/clock store)))
        (assoc (ctl/deleting store :oauth-codes {:code k}) :result row)
        {:result nil}))))

(defn sweep-expired-codes!
  [store]
  (ctl/commit!
   store
   [[:plan :oauth-codes
     (fn [store _]
       (let [dead (remove #(.after ^java.util.Date (:expires-at %) (ctl/clock store))
                          (ctl/list store :oauth-codes {}))]
         {:records (vec (mapcat #(:records (ctl/deleting store :oauth-codes {:code (:code %)}))
                                dead))
          :result  (count dead)}))]]))

(defn- today
  [store]
  (str (.toLocalDate (.atZone (ctl/instant store) (java.time.ZoneId/systemDefault)))))

(defn record-call!
  [store app-rid endpoint {:keys [ms error?]}]
  (when app-rid
    (let [where {:app      (str app-rid)
                 :day      (today store)
                 :endpoint (str/join "" (take 191 (str endpoint)))}]
      (ctl/swap! store :custom-app-metrics where
                 (fn [old]
                   {:calls    (inc (long (or (:calls old) 0)))
                    :errors   (+ (long (or (:errors old) 0)) (if error? 1 0))
                    :ms-total (+ (long (or (:ms-total old) 0)) (long (or ms 0)))}))))
  nil)

(defn metrics-of
  ([store app-rid] (metrics-of store app-rid 60))
  ([store app-rid limit]
   (vec (ctl/list store :custom-app-metrics {:app (str app-rid)} {:limit (max 1 limit)}))))

(defn metrics-summary
  [store app-rid]
  (let [rows (metrics-of store app-rid 500)
        calls (reduce + 0 (map (comp long #(or % 0) :calls) rows))
        errs  (reduce + 0 (map (comp long #(or % 0) :errors) rows))
        ms    (reduce + 0 (map (comp long #(or % 0) :ms-total) rows))]
    {:calls calls :errors errs
     :avg-ms (when (pos? calls) (long (/ ms calls)))
     :days (count (into #{} (map :day) rows))
     :endpoints (count (into #{} (map :endpoint) rows))}))
