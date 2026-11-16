(ns sutra.access.principals
  (:require [sutra.kit.refusal :as refusal]
            [sutra.kit.attempt :as attempt]
            [sutra.catalog.grants :as grants]
            [sutra.kit.anomaly :as anomaly]
            [sutra.access.password :as password]
            [sutra.access.password-policy :as policy]
            [clojure.string :as str]
            [sutra.store.control :as ctl]
            [sutra.foundation.env :as env]
            [sutra.store.run :as run]))

(set! *warn-on-reflection* true)
(def ^:private configured-admin-password
  (some-> (env/getenv "SUTRA_ADMIN_PASSWORD") str/trim not-empty))

(def default-admin-password
  (or configured-admin-password (password/initial-password)))

(def ^:private admin-user-id "admin")

(def case-insensitive-auth-sources #{"spnego"})

(def service-auth-source
  "oauth-client")

(defn service-user? [p] (= service-auth-source (:auth-source p)))

(defn- service-user-refusal [code username]
  (refusal/refusal code {:reason :service-user :principal username}
                   {:source service-auth-source}))

(defn- public
  [p]
  (when p
    (-> (dissoc p :ontology :created-at :cred-gen :mfa-seed :mfa-pending :mfa-last-step)
        (assoc :mfa (boolean (seq (:mfa-seed p)))))))

(defn- registering
  [{:keys [username password branch user-id auth-source external-subject initial?]}]
  (let [source (name (or auth-source :password))
        hash   (password/hash-or-nil password)]
    (fn [store at]
      (when (= source service-auth-source)
        (throw
         (service-user-refusal :refusal.access/service-user-by-hand username)))
      (when (service-user? (ctl/get store :principals {:username username}))
        (throw
         (service-user-refusal :refusal.access/service-user-sign-in username)))
      (when-let [holder (and external-subject
                             (some #(when (not= username (:username %)) (:username %))
                                   (ctl/list store :principals {:auth-source source
                                                             :external-subject external-subject})))]
        (refusal/refuse! :refusal.access/external-subject-taken
                         {:reason :external-subject-taken
                          :subject external-subject :principal holder}
                         {:source source}))
      (ctl/swapping store :principals {:username username}
                    (fn [old]
                      {:ontology         ""
                       :branch           (or branch "main")
                       :user-object-id   user-id
                       :pw-hash          hash
                       :auth-source      source
                       :external-subject external-subject
                       :created-at       (or (:created-at old) ctl/now)
                       :cred-gen         (if old (inc (long (or (:cred-gen old) 0))) 0)
                       :pw-must-change   (boolean (and hash initial?))
                       :pw-set-at        (if hash ctl/now (:pw-set-at old))
                       :mfa-seed         (:mfa-seed old)
                       :mfa-pending      (:mfa-pending old)
                       :mfa-last-step    (:mfa-last-step old)})
                    at))))

(defn registering-service-user
  [client-id]
  (fn [store at]
    (when (ctl/get store :principals {:username client-id})
      (throw (service-user-refusal :refusal.access/principal-exists client-id)))
    (ctl/swapping store :principals {:username client-id}
                  (fn [_]
                    {:ontology         ""
                     :branch           "main"
                     :user-object-id   client-id
                     :pw-hash          nil
                     :auth-source      service-auth-source
                     :external-subject nil
                     :created-at       ctl/now
                     :cred-gen         0})
                  at)))

(defn register-principal!
  [store m]
  (ctl/commit! store [[:plan :principals (registering m)]])
  {:username (:username m)})

(defn fetch
  [store username]
  (public (ctl/get store :principals {:username username})))

(defn fetch-by-external
  [store auth-source external-subject]
  (or (public (ctl/get store :principals
                       {:auth-source auth-source :external-subject external-subject}))
      (when (contains? case-insensitive-auth-sources auth-source)
        (let [s  (str/lower-case (str external-subject))
              rs (filter #(= s (some-> (:external-subject %) str/lower-case))
                         (ctl/list store :principals {:auth-source auth-source}))]
          (when (= 1 (count rs)) (public (first rs)))))))

(defn all-rows
  [store]
  (->> (ctl/list store :principals {})
       (sort-by :username)
       (mapv #(-> % public (dissoc :pw-hash) (assoc :has-password (boolean (seq (:pw-hash %))))))))

(defn federated-subjects
  [store auth-source]
  (into []
        (comp (filter :external-subject) (map #(select-keys % [:external-subject :user-object-id])))
        (ctl/list store :principals {:auth-source auth-source})))

(defn- password-row-planner
  [username f ends?]
  (fn [store at]
    (let [old (ctl/get store :principals {:username username})]
      (if-let [h (and old (f old))]
        (ctl/swapping store :principals {:username username}
                      (fn [row]
                        (cond-> (assoc row :pw-hash h)
                          ends? (assoc :cred-gen (inc (long (or (:cred-gen row) 0)))
                                       :pw-must-change false :pw-set-at ctl/now)))
                      at)
        {:result nil}))))

(defn set-password!
  [store username new-password]
  (ctl/commit! store
               [[:plan :principals
                 (password-row-planner username (constantly (password/encode new-password)) true)]])
  nil)

(defn rehash!
  [store username stored-hash new-hash]
  (ctl/commit! store
               [[:plan :principals
                 (password-row-planner username
                                       #(when (= stored-hash (:pw-hash %)) new-hash) false)]])
  nil)

(defn verify
  [store p password]
  (let [h (:pw-hash p)]
    (if (and (seq h) (password/verify? password h))
      (do (when (password/needs-rehash? h)
            (try (rehash! store (:username p) h (password/encode password))
                 (catch Exception e
                   (attempt/rethrow-fatal! e)
                   (println "WARN password rehash failed —" (.getMessage e)))))
          p)
      (when-not (seq h) (password/burn! password)))))

(defn- refuse-weak-admin!
  [pw]
  (when (or (= pw grants/admin-username)
            (policy/common? pw))
    (anomaly/raise! :fault (str "SUTRA_ADMIN_PASSWORD is a well-known password — refusing to seed '"
                                grants/admin-username "' with it; set a long random one")
                           {:reason :weak-admin-password})))

(defn ensure-default-admin!
  [store _opts]
  (when-not (fetch store grants/admin-username)
    (if configured-admin-password
      (refuse-weak-admin! configured-admin-password)
      (binding [*out* *err*]
        (println (str "[auth] SUTRA_ADMIN_PASSWORD is unset — seeding '" grants/admin-username
                      "' with a generated password, shown this once: "
                      default-admin-password))))
    (register-principal! store {:username grants/admin-username
                             :password default-admin-password
                             :branch   "main"
                             :user-id  admin-user-id})
    {:created grants/admin-username}))

(defmethod run/after! :register-principal
  [store {:keys [principal]}]
  (register-principal! store principal))
