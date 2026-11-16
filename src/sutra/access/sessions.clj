(ns sutra.access.sessions
  (:require [sutra.kit.attempt :as attempt]
            [sutra.kit.noted :as noted]
            [clojure.string :as str]
            [sutra.access.password :as password]
            [sutra.foundation.env :as env]
            [sutra.kit.digest :as digest]
            [sutra.kit.lifecycle :as lc]
            [sutra.store.control :as ctl])
  (:import [java.util Date UUID]))

(set! *warn-on-reflection* true)

(def lifecycle
  (lc/table {:name   :session
             :states #{:live :ended}
             :final  #{:ended}
             :edges  {:login       {:from #{} :to :live}
                      :logout      {:from #{:live} :to :ended}
                      :revoke      {:from #{:live} :to :ended}
                      :cred-change {:from #{:live} :to :ended}}}))

(def kinds
  {:login {:ttl-seconds (* 60 60 16)}
   :clone {:ttl-seconds (* 60 60 24 7)}
   :agent {:ttl-seconds (* 60 60)}
   :limited {:ttl-seconds (* 60 15)}
   :app   {:ttl-seconds nil}})

(defn ttl-of [kind] (get-in kinds [kind :ttl-seconds]))

(def idle-seconds
  (* 60 (or (some-> (env/getenv "SUTRA_SESSION_IDLE_MINUTES") str/trim parse-long) (* 8 60))))

(defn new-token [] (password/random-token 32))

(defn token-key
  [token]
  (digest/sha256-hex token))

(defn- stored-key? [k] (boolean (re-matches #"[0-9a-f]{64}" (str k))))

(def clone-ttl-seconds (ttl-of :clone))

(def ^:private endings
  {:logout      {:outcome :logout}
   :revoke      {:outcome :revoked :detail "signed out from another client"}
   :cred-change {:outcome :stale}})

(def ^:private batch 200)

(defn session-id
  [token]
  (subs (digest/sha256-hex token) 0 24))

(defn- sid-of
  [row]
  (or (:sid row) (some-> (:token row) (subs 0 24))))

(def ^:const agent-scope-prefix "agent:")

(def ^:const routes-marker "routes:")

(defn agent-scope
  [agent routes]
  (str agent-scope-prefix agent " " routes-marker (str/join "," (sort (map name routes)))))

(defn agent-scoped?
  [sess]
  (boolean (some-> (:scope sess) (str/starts-with? agent-scope-prefix))))

(defn agent-routes
  [sess]
  (if-let [part (when (agent-scoped? sess)
                  (second (re-find #"(?:^| )routes:(\S*)" (:scope sess))))]
    (into #{} (comp (remove str/blank?) (map keyword)) (str/split part #","))
    #{}))

(def ^:const limited-scope-prefix "limited:")

(defn limited-scope
  [reasons]
  (str limited-scope-prefix (str/join "," (sort (map name reasons)))))

(defn limited-scoped?
  [sess]
  (boolean (some-> (:scope sess) (str/starts-with? limited-scope-prefix))))

(defn limited-reasons
  [sess]
  (if (limited-scoped? sess)
    (into #{} (comp (remove str/blank?) (map keyword))
          (str/split (subs (:scope sess) (count limited-scope-prefix)) #","))
    #{}))

(defn- kind [{:keys [scope client-id] :as sess}]
  (cond client-id :app (agent-scoped? sess) :agent (limited-scoped? sess) :limited
        scope :clone :else :login))

(defn max-logins
  []
  (or (some-> (env/getenv "SUTRA_MAX_SESSIONS") str/trim parse-long (max 1))
      (when (env/auth-hardening?) 10)))

(defn- plus-secs ^Date [^Date d secs] (Date. (+ (.getTime d) (* 1000 (long secs)))))

(defn- earliest
  ^Date [& ds]
  (reduce (fn [^Date a ^Date b] (if (.before b a) b a)) (remove nil? ds)))

(defn- deadline
  ^Date [{:keys [^Date expires-at ^Date created-at ^Date last-seen-at] :as row}]
  (let [k   (kind row)
        ttl (ttl-of k)]
    (earliest expires-at
              (when (and ttl created-at) (plus-secs created-at ttl))
              (when (= :login k)
                (some-> (or last-seen-at created-at) (plus-secs idle-seconds))))))

(defn- live? [^Date now row] (.after (deadline row) now))

(defn- view
  [current-sid row]
  (cond-> {:id (sid-of row) :kind (kind row)
           :created-at (:created-at row) :expires-at (deadline row)
           :last-seen-at (or (:last-seen-at row) (:created-at row))
           :current? (= current-sid (sid-of row))}
    (:provider row)   (assoc :provider (:provider row))
    (:user-agent row) (assoc :user-agent (:user-agent row))
    (:ip row)         (assoc :ip (:ip row))))

(defn- session-row
  [now token p cred-gen {:keys [scope ttl client-id secret-gen provider user-agent ip] :as opts}]
  (let [ttl (or ttl (ttl-of (kind opts)))]
    (cond-> {:token (token-key token) :sid (session-id token) :username (:username p)
             :user-id (:user-object-id p) :branch (:branch p) :scope scope
             :cred-gen cred-gen :created-at ctl/now :last-seen-at ctl/now
             :expires-at (plus-secs now ttl)}
      client-id  (assoc :client-id client-id :secret-gen secret-gen)
      provider   (assoc :provider (name provider))
      user-agent (assoc :user-agent (subs user-agent 0 (min 512 (count user-agent))))
      ip         (assoc :ip ip))))

(defn current?
  [{:keys [cred-gen client-id secret-gen]} p c]
  (and (or (nil? cred-gen) (= cred-gen (:cred-gen p)))
       (or (nil? client-id) (and c (nil? (:revoked-at c)) (= secret-gen (:secret-gen c))))))

(defn scoped-to?
  [sess repo-rid]
  (let [s (:scope sess)]
    (or (nil? s) (contains? (set (str/split s #" ")) (str repo-rid)))))

(defn- audit-entry [store {:keys [username provider outcome subject detail]}]
  (ctl/entry store :login-log {:id (str (UUID/randomUUID)) :at ctl/now :username username
                               :provider (some-> provider name) :outcome (some-> outcome name)
                               :subject subject :detail detail}))

(defn- auditing [entry]
  (fn [store _] {:records [(audit-entry store entry)]}))

(defn minting
  [token p opts]
  (fn [store _]
    (lc/decide lifecycle (session-id token) :login nil nil nil)
    {:records [(ctl/entry store :sessions
                          (session-row (ctl/clock store) token p
                                       (:cred-gen (ctl/get store :principals
                                                           {:username (:username p)}))
                                       opts))]
     :result token}))

(defn- ending
  [ks event]
  (fn [store _]
    (let [rows (ctl/list store :sessions {:token (set ks)})]
      (doseq [row rows] (lc/decide lifecycle (sid-of row) event :live row nil))
      (update (ctl/rewriting :sessions rows (constantly nil)) :records into
              (for [{:keys [username]} rows]
                (audit-entry store (merge (endings event) {:username username
                                                           :provider :session
                                                           :subject username})))))))

(defn- removing
  [ks]
  (fn [store _]
    (ctl/rewriting :sessions (ctl/list store :sessions {:token (set ks)}) (constantly nil))))

(defn- plan-batches!
  [store planner tokens]
  (transduce (comp (partition-all batch)
                   (map #(first (ctl/commit! store [[:plan :sessions (planner %)]]))))
             + 0 tokens))

(defn record-login!
  [store entry]
  (try
    (ctl/commit! store [[:plan :sessions (auditing entry)]])
    (catch Exception e
      (attempt/rethrow-fatal! e)
      (println "WARN login_log: append failed —" (.getMessage e))))
  nil)

(defn read-login-log
  ([store limit] (read-login-log store {} limit))
  ([store {:keys [username outcome provider]} limit]
   (ctl/list store :login-log
             (cond-> {}
               username (assoc :username username)
               outcome  (assoc :outcome  (name outcome))
               provider (assoc :provider (name provider)))
             {:limit (max 1 limit)})))

(defn mint!
  ([store p] (mint! store p {}))
  ([store p opts]
   (first (ctl/commit! store [[:plan :sessions (minting (new-token) p opts)]]))))

(defn mint-agent-token!
  [store p agent routes]
  (mint! store p {:scope (agent-scope agent routes)}))

(defn mint-clone-token!
  [store p repo-rids]
  (mint! store p {:scope (str/join " " (distinct (map str repo-rids))) :ttl clone-ttl-seconds}))

(defn- beyond-cap
  [store username keep]
  (when-let [cap (max-logins)]
    (let [now (ctl/clock store)]
      (->> (ctl/list store :sessions {:username username})
           (filter #(and (= :login (kind %)) (live? now %) (not= keep (:token %))))
           (sort-by #(or (:last-seen-at %) (:created-at %)) #(compare %2 %1))
           (drop (dec cap))
           (map :token)))))

(defn open!
  ([store p audit client] (open! store p audit client {}))
  ([store p audit client opts]
   (let [token (first (ctl/commit! store
                                   [[:plan :sessions
                                     (minting (new-token) p
                                              (merge (assoc (select-keys client [:user-agent :ip])
                                                            :provider (:provider audit))
                                                     (select-keys opts [:scope])))]
                                    [:plan :sessions
                                     (auditing (assoc audit :outcome :success
                                                      :username (:username p)))]]))]
     (when-not (:scope opts)
       (when-let [ks (seq (beyond-cap store (:username p) (token-key token)))]
         (plan-batches! store #(ending % :revoke) ks)))
     token)))

(defn end!
  [store token]
  (when token
    (ctl/commit! store [[:plan :sessions (ending [(token-key token)] :logout)]]))
  nil)

(defn of-user
  [store username current-token]
  (let [now (ctl/clock store)
        sid (some-> current-token session-id)]
    (->> (ctl/list store :sessions {:username username})
         (filter (partial live? now))
         (map (partial view sid))
         (sort-by :last-seen-at #(compare %2 %1))
         vec)))

(defn revoke!
  [store username {:keys [id except]}]
  (let [rows (mapcat #(ctl/list store :sessions (merge {:username username} %)
                                {:project [:token :sid]})
                     (if id [{:sid id} {:sid nil}] [{}]))]
    (plan-batches! store #(ending % :revoke)
                   (for [row rows
                         :let [s (sid-of row)]
                         :when (if id (= id s) (not= except s))]
                     (:token row)))))

(def ^:private seen-every-ms (* 5 60 1000))

(defonce ^:private last-seen (atom {}))

(defn seen!
  [store token]
  (let [token (token-key token)
        t  (System/currentTimeMillis)
        [old _] (swap-vals! last-seen
                            (fn [m] (if (< (- t (get m token 0)) seen-every-ms)
                                      m
                                      (assoc m token t))))]
    (when (>= (- t (get old token 0)) seen-every-ms)
      (when (> (count old) 10000) (reset! last-seen {token t}))
      (future
        (let [seen (fn [store at]
                     (ctl/updating store :sessions {:token token} {:last-seen-at ctl/now} at))]
          (noted/or-else "noting a session as seen" nil
            (ctl/commit! store [[:plan :sessions seen]])))))
    nil))

(defn fetch
  [store token]
  (when token
    (when-let [row (ctl/get store :sessions {:token (token-key token)})]
      (when (live? (ctl/clock store) row)
        (if (current? row
                      (ctl/get store :principals {:username (:username row)})
                      (some->> (:client-id row)
                               (hash-map :client-id) (ctl/get store :oauth-clients)))
          (select-keys row [:username :user-id :branch :scope])
          (do (ctl/commit! store [[:plan :sessions (ending [(token-key token)] :cred-change)]])
              nil))))))

(defn- epoch-s [^Date d] (when d (quot (.getTime d) 1000)))

(defn introspect
  [store token]
  (let [row (when (seq (str token)) (ctl/get store :sessions {:token (token-key token)}))
        now (ctl/clock store)]
    (if (and row (live? now row) (#{:app :login} (kind row))
             (current? row (ctl/get store :principals {:username (:username row)})
                       (some->> (:client-id row) (hash-map :client-id)
                                (ctl/get store :oauth-clients))))
      (cond-> {:active true :token_type "Bearer" :username (:username row)
               :sub (str (or (:user-id row) (:username row)))
               :iat (epoch-s (:created-at row)) :exp (epoch-s (deadline row))}
        (:client-id row) (assoc :client_id (:client-id row))
        (:scope row)     (assoc :scope (:scope row)))
      {:active false})))

(defn revoke-client-token!
  [store token client-id]
  (when (seq (str token))
    (let [k (token-key token)]
      (when (= client-id (:client-id (ctl/get store :sessions {:token k})))
        (ctl/commit! store [[:plan :sessions (ending [k] :revoke)]]))))
  nil)

(defn authenticated-at
  [store token]
  (when (seq (str token))
    (epoch-s (:created-at (ctl/get store :sessions {:token (token-key token)})))))

(defn sweep-expired!
  [store]
  (let [now (ctl/clock store)]
    (plan-batches! store removing
                   (->> (ctl/list store :sessions {}
                                  {:project [:token :scope :client-id :created-at :expires-at
                                             :last-seen-at]})
                        (remove #(and (stored-key? (:token %)) (live? now %)))
                        (map :token)))))
