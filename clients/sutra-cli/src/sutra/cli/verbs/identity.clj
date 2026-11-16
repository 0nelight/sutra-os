(ns sutra.cli.verbs.identity
  (:require [sutra.cli.auth :refer [bound-user load-creds save-creds!]]
            [sutra.cli.dispatch :refer [deref1 step!]]
            [sutra.cli.errors :refer [ok? req!]]
            [sutra.cli.live :refer [schema-sections]]
            [sutra.cli.out :refer [emit! mark paint read-visible say! warn!]]
            [sutra.i18n.core :as i18n]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]
            [sutra.cli.instance :as inst]))

(defn- project-by-name
  [schema nm]
  (some (fn [[rid spec]]
          (when (= (clojure.core/name (:project-name spec)) (clojure.core/name nm))
            (assoc spec :rid rid)))
        (:projects schema)))

(defn- organization-by-name
  [schema nm]
  (some (fn [[rid spec]]
          (when (or (= (str rid) (str nm))
                    (= (some-> (:organization-name spec) clojure.core/name) (str nm)))
            (assoc spec :rid rid)))
        (:organizations schema)))

(defmethod step! :org-guest [ctx [_ {:keys [op organization principal stage?]}]]
  (when-not (#{"add" "remove"} (str op))
    (throw (ex-info (str "org guest: add or remove, not " (pr-str op)) {:op op})))
  (let [org    (or (organization-by-name (schema-sections :organizations) organization)
                   (throw (ex-info (str "org guest: no organization " organization)
                                   {:organization organization})))
        before (vec (:guests org))
        guests (if (= "add" (str op))
                 (vec (sort (distinct (conj before (str principal)))))
                 (vec (remove #{(str principal)} before)))]
    (if (= guests before)
      (emit! ctx {:organization organization :guests guests :unchanged true})
      (do (req! (str "org guest " op)
                (api/mutate :update-organization
                            {:name (or (:organization-name org) (:rid org)) :guests guests}
                            {:stage? true}))
          (when-not stage? (req! "commit guest change" (api/post! :staging)))
          (emit! ctx {:organization organization :guests guests
                      (if (= "add" (str op)) :added :removed) principal
                      :staged (boolean stage?)})))))

(defmethod step! :grant [ctx [_ {:keys [project to role revoke?]}]]
  (let [role   (or role :ontology-viewer)
        given  (deref1 ctx to)
        prin   (if (and (string? given)
                        (not (re-find #"^ri\.|^[0-9a-f]{8}-[0-9a-f]{4}-" given)))
                 (or (:user-id (req! (str "access role " given)
                                     (api/get! :access-role {:user given})))
                     (throw (ex-info (str "grant: no user " given) {:user given})))
                 given)
        schema (schema-sections :projects :roles)
        known  (into #{:ontology-finder :ontology-viewer :ontology-editor :ontology-owner
                       :consumer}
                     (map keyword) (keys (:roles schema)))
        _      (when-not (contains? known (keyword role))
                 (throw (ex-info (str "grant: no role " (clojure.core/name role) " — known: "
                                      (str/join ", " (sort (map clojure.core/name known))))
                                 {:role role})))
        cur    (project-by-name schema project)
        grants0 (vec (:grants cur))
        g      {:principal prin :role role}
        grants (if revoke?
                 (vec (remove #(and (= (:principal %) prin) (= (:role %) role)) grants0))
                 (vec (distinct (conj grants0 g))))
        [action extra] (if cur
                         [:update-project {}]
                         [:create-project
                          {:description (str "Portal-Projekt " (clojure.core/name project))}])]
    (req! (str (if cur "update" "create") " project " (clojure.core/name project))
          (api/mutate action (merge {:name (keyword project) :grants grants} extra)
                      {:stage? true}))
    (req! "commit grant" (api/post! :staging))
    (emit! ctx {:project (clojure.core/name project) :principal prin :role role
                (if revoke? :revoked :granted) true})))

(defn- read-keytab-b64
  [path]
  (try
    (.encodeToString (java.util.Base64/getEncoder)
                     (java.nio.file.Files/readAllBytes (.toPath (io/file path))))
    (catch Exception e
      (throw (ex-info (str "keytab file not readable: " path) {:path path} e)))))

(defmethod step! :sso [ctx [_ {:keys [sub realm kdc spn keytab enable? disable?]}]]
  (if-not (= sub "config")
    (let [status (req! "sso status" (api/get! :identity-status {:provider "spnego"}))]
      (emit! ctx status)
      (when (and (:enabled status) (not (:ready status)))
        (warn! (mark :yellow "!" (i18n/t :cli/sso-incomplete)))))
    (let [realm (or realm (read-visible (i18n/t :cli/sso-ask-realm) nil))
          kdc   (or kdc   (read-visible (i18n/t :cli/sso-ask-kdc) nil))
          spn   (or spn   (read-visible (i18n/t :cli/sso-ask-spn) nil))
          ktb64 (when keytab (read-keytab-b64 keytab))
          body  (cond-> {}
                  (seq (str (or realm ""))) (assoc :realm realm)
                  (seq (str (or kdc "")))   (assoc :kdc kdc)
                  (seq (str (or spn "")))   (assoc :spn spn)
                  ktb64                     (assoc :keytab-b64 ktb64)
                  enable?                   (assoc :enabled true)
                  disable?                  (assoc :enabled false))
          status    (req! "set sso config" (api/post! :identity-config {:provider "spnego"} body))]
      (emit! ctx status)
      (when (and (:enabled status) (not (:ready status)))
        (let [missing (for [[k v] [["realm" (:realm status)] ["kdc" (:kdc status)]
                                   ["spn" (:spn status)] ["keytab" (:keytab? status)]]
                            :when (not (if (= k "keytab") v (seq (str (or v "")))))]
                        k)]
          (warn! (mark :yellow "!"
                       (i18n/t :cli/sso-incomplete-missing {:fehlt (str/join ", " missing)})))))
      nil)))

(defn- read-group-map-arg
  [s]
  (when (seq (str (or s "")))
    (let [f (io/file s)]
      (if (.isFile f) (slurp f) s))))

(defmethod step! :ldap [ctx [_ {:keys [sub url bind-dn bind-pw base-dn user-filter
                                       realm group-map enable? disable? dry-run? disable-gone?]}]]
  (case (or sub "status")
    "status"
    (emit! ctx (req! "ldap status" (api/get! :identity-status {:provider "ldap"})))

    "config"
    (let [gm   (read-group-map-arg group-map)
          body (cond-> {}
                 (seq (str (or url "")))         (assoc :url url)
                 (seq (str (or bind-dn "")))     (assoc :bind-dn bind-dn)
                 (seq (str (or bind-pw "")))     (assoc :bind-pw bind-pw)
                 (seq (str (or base-dn "")))     (assoc :base-dn base-dn)
                 (seq (str (or user-filter ""))) (assoc :user-filter user-filter)
                 (seq (str (or realm "")))       (assoc :realm realm)
                 gm                              (assoc :group-map gm)
                 enable?                         (assoc :enabled true)
                 disable?                        (assoc :enabled false))
          status   (req! "set ldap config" (api/post! :identity-config {:provider "ldap"} body))]
      (emit! ctx status)
      (when (and (:enabled status) (not (:ready status)))
        (warn! (mark :yellow "!" (i18n/t :cli/ldap-incomplete)))))

    "sync"
    (emit! ctx (req! "ldap sync" (api/post! :identity-sync {:provider "ldap"}
                                            {:dry-run? (boolean dry-run?)
                                             :disable-gone? (boolean disable-gone?)})))

    (warn! (mark :red "✗" (i18n/t :cli/ldap-unknown-command {:befehl sub})))))

(defn- idp-config-body
  [p settings enable? disable?]
  (let [known (into {} (for [[f s] (:settings p)] [(keyword (name (or (:input s) f))) s]))]
    (cond-> (into {}
                  (for [[k v] settings]
                    (if-let [{:keys [type]} (known (keyword k))]
                      [(keyword k) (if (= :boolean (keyword type))
                                     (contains? #{"true" "on" "yes" "1"} (str/lower-case (str v)))
                                     (str v))]
                      (throw (ex-info (str (:label p) " has no setting " (name k) " — it declares "
                                           (str/join ", " (sort (map name (keys known)))))
                                      {:setting k})))))
      enable?  (assoc :enabled true)
      disable? (assoc :enabled false))))

(defmethod step! :idp
  [ctx [_ {:keys [provider sub settings enable? disable? dry-run? disable-gone?]}]]
  (let [fs (inst/facts)]
    (if (nil? provider)
      (emit! ctx {:identity-providers (mapv #(select-keys % [:id :label :can])
                                            (:identity-providers fs))})
      (let [p   (or (inst/provider fs provider)
                    (throw (ex-info (str "no identity provider " provider " — this server has "
                                         (str/join ", "
                                                   (map (comp name :id) (:identity-providers fs))))
                                    {:provider provider})))
            arg {:provider (name (:id p))}
            can (set (map keyword (:can p)))]
        (case (or sub "status")
          "status"   (emit! ctx (req! (str (:label p) " status") (api/get! :identity-status arg)))
          "settings" (emit! ctx (:settings p))
          "config"   (emit! ctx (req! (str "set " (:label p))
                                      (api/post! :identity-config arg
                                                 (idp-config-body p settings enable? disable?))))
          "preview"  (if (can :directory)
                       (emit! ctx (req! (str (:label p) " preview")
                                        (api/get! :identity-preview arg)))
                       (throw (ex-info (str (:label p) " reads no directory") {})))
          "sync"     (if (can :directory)
                       (emit! ctx (req! (str (:label p) " sync")
                                        (api/post! :identity-sync arg
                                                   {:dry-run? (boolean dry-run?)
                                                    :disable-gone? (boolean disable-gone?)})))
                       (throw (ex-info (str (:label p) " reads no directory") {})))
          (throw (ex-info (str "idp " (name (:id p)) " " sub
                               " — status | settings | config k=v… | preview | sync")
                          {:sub sub})))))
    nil))

(defmethod step! :login [ctx [_ {:keys [as]}]]
  (let [creds (load-creds (:creds-path ctx))
        {u :user refused :refused} (bound-user creds (tx/current-base) (or as :admin))]
    (cond
      refused
      (warn! (mark :red "✗"
                   (i18n/t :cli/creds-other-instance (assoc refused :datei (:creds-path ctx)))))

      (nil? u)
      (say! ctx (mark :yellow "⏸" (i18n/t :cli/no-stored-creds {:wer (or as :admin)})))

      :else
      (let [r (api/post! :login (select-keys u [:username :password]))]
        (if (ok? r)
          (do (reset! (:token ctx) (:token (:body r)))
              (save-creds! (:creds-path ctx)
                           (assoc-in creds [:users (or as :admin) :token]
                                     (:token (:body r))))
              (say! ctx (mark :green "✓"
                              (i18n/t :cli/logged-in-as {:wer (paint :bold (:username u))}))))
          (say! ctx (mark :yellow "⏸"
                          (i18n/t :cli/login-failed {:wer (:username u) :status (:status r)}))))))))
