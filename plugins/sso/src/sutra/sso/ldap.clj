(ns sutra.sso.ldap
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.util Hashtable)
           (javax.naming AuthenticationException Context NamingEnumeration NamingException
                         PartialResultException)
           (javax.naming.directory Attributes DirContext SearchControls SearchResult)
           (javax.naming.ldap InitialLdapContext StartTlsRequest StartTlsResponse)
           (javax.net.ssl SSLContext SSLSocketFactory TrustManagerFactory)
           (java.security KeyStore)
           (java.security.cert CertificateFactory)))

(set! *warn-on-reflection* true)

(def settings
  {:enabled     {:key "ldap_enabled"     :type :boolean}
   :url         {:key "ldap_url"         :type :string}
   :bind-dn     {:key "ldap_bind_dn"     :type :string}
   :bind-pw     {:key "ldap_bind_pw"     :type :string :secret? true}
   :base-dn     {:key "ldap_base_dn"     :type :string}
   :user-filter {:key "ldap_user_filter" :type :string
                 :default "(&(objectCategory=person)(objectClass=user)(sAMAccountName=*))"}
   :realm       {:key "ldap_realm"       :type :string}
   :ca-file     {:key "ldap_ca_file"     :type :string}
   :ca-cert     {:key "ldap_ca_cert"     :type :string}
   :group-map   {:key "ldap_group_map"   :type :edn :default {}}})

(defn ready?
  [{:keys [enabled url bind-dn bind-pw base-dn]}]
  (boolean (and enabled (every? #(seq (str %)) [url bind-dn bind-pw base-dn]))))

(defn- ca-socket-factory
  ^SSLSocketFactory [open-in source]
  (let [certs (with-open [^java.io.InputStream in (open-in)]
                (.generateCertificates (CertificateFactory/getInstance "X.509") in))
        ks    (doto (KeyStore/getInstance (KeyStore/getDefaultType)) (.load nil nil))
        _     (when (empty? certs)
                (throw (ex-info (str "LDAP CA holds no certificate: " source) {:source source})))
        _     (doseq [[i c] (map-indexed vector certs)] (.setCertificateEntry ks (str "ca" i) c))
        tmf   (doto (TrustManagerFactory/getInstance (TrustManagerFactory/getDefaultAlgorithm))
                (.init ks))]
    (.getSocketFactory (doto (SSLContext/getInstance "TLS")
                         (.init nil (.getTrustManagers tmf) nil)))))

(defn- tls-negotiated!
  [^StartTlsResponse tls {:keys [ca-cert ca-file]}]
  (cond
    (not (str/blank? (str ca-cert)))
    (.negotiate tls (ca-socket-factory
                     #(java.io.ByteArrayInputStream. (.getBytes (str ca-cert) "US-ASCII"))
                     "ldap_ca_cert"))

    (not (str/blank? (str ca-file)))
    (.negotiate tls (ca-socket-factory #(io/input-stream (io/file (str ca-file))) ca-file))

    :else (.negotiate tls)))

(defn- with-dir-ctx
  ([s f] (with-dir-ctx s (str (:bind-dn s)) (str (:bind-pw s)) f))
  ([s dn pw f]
   (let [url (str (:url s))
         starttls? (str/starts-with? (str/lower-case url) "ldap://")
         env (doto (Hashtable.)
               (.put Context/INITIAL_CONTEXT_FACTORY "com.sun.jndi.ldap.LdapCtxFactory")
               (.put Context/PROVIDER_URL url)
               (.put Context/REFERRAL "ignore")
               (.put "com.sun.jndi.ldap.connect.timeout" "8000")
               (.put "com.sun.jndi.ldap.read.timeout" "12000"))]
     (if starttls?
       (let [_   (.put env Context/SECURITY_AUTHENTICATION "none")
             ctx (InitialLdapContext. env nil)
             tls ^StartTlsResponse (.extendedOperation ctx (StartTlsRequest.))]
         (try (tls-negotiated! tls s)
              (.addToEnvironment ctx Context/SECURITY_AUTHENTICATION "simple")
              (.addToEnvironment ctx Context/SECURITY_PRINCIPAL dn)
              (.addToEnvironment ctx Context/SECURITY_CREDENTIALS pw)
              (f ctx)
              (finally (try (.close tls) (catch java.io.IOException _ nil))
                       (try (.close ctx) (catch NamingException _ nil)))))
       (let [_   (doto env
                   (.put Context/SECURITY_AUTHENTICATION "simple")
                   (.put Context/SECURITY_PRINCIPAL dn)
                   (.put Context/SECURITY_CREDENTIALS pw)
                   (.put Context/SECURITY_PROTOCOL "ssl"))
             ctx (InitialLdapContext. env nil)]
         (try (f ctx) (finally (try (.close ctx) (catch NamingException _ nil)))))))))

(defn- attr1 [^Attributes attrs k]
  (when-let [a (.get attrs k)] (some-> (.get a) str)))

(defn- attr-all [^Attributes attrs k]
  (when-let [a (.get attrs k)]
    (let [e (.getAll a)]
      (loop [acc []] (if (.hasMore e) (recur (conj acc (str (.next e)))) acc)))))

(defn- cn-of
  [dn]
  (some-> dn (str/split #",") first (str/replace #"(?i)^CN=" "")))

(defn- search-users
  [s]
  (when-not (ready? s)
    (throw (ex-info "LDAP not configured — set url/bind-dn/bind-pw/base-dn and enable" {})))
  (with-dir-ctx
    s
    (fn [^DirContext c]
      (let [sc   (doto (SearchControls.)
                   (.setSearchScope SearchControls/SUBTREE_SCOPE)
                   (.setCountLimit 0)
                   (.setReturningAttributes
                    (into-array String ["sAMAccountName" "userPrincipalName"
                                        "displayName" "mail" "memberOf"])))
            filt (:user-filter s)
            ^NamingEnumeration res (.search c (str (:base-dn s)) ^String filt sc)]
        (loop [acc []]
          (if (try (.hasMore res) (catch PartialResultException _ false))
            (let [a (.getAttributes ^SearchResult (.next res))]
              (recur (conj acc {:sam     (attr1 a "sAMAccountName")
                                :upn     (attr1 a "userPrincipalName")
                                :display (attr1 a "displayName")
                                :mail    (attr1 a "mail")
                                :groups  (mapv cn-of (attr-all a "memberOf"))})))
            acc))))))

(defn- subject-for
  [s {:keys [upn sam]}]
  (let [realm (some-> (:realm s) str str/upper-case)]
    (str/lower-case
     (cond
       (seq (str (or upn ""))) upn
       (and (seq (str (or sam ""))) realm) (str sam "@" realm)
       :else (str sam)))))

(defn map-user
  [s subject {:keys [attributes groups]}]
  (let [lc    (fn [x] (some-> x str str/lower-case))
        gmap' (into {} (map (fn [[k v]] [(lc k) v])) (:group-map s))
        hits  (keep #(get gmap' (lc %)) (sort groups))]
    (when (seq hits)
      (let [grants (vec (distinct (for [{:keys [role projects]} hits
                                        p (or projects [])]
                                    {:project p :role role})))]
        {:subject  subject
         :sam      (:sam attributes)
         :display  (or (:display attributes) (:sam attributes))
         :mail     (:mail attributes)
         :role     (:role (first hits))
         :projects (vec (distinct (map :project grants)))
         :grants   grants
         :groups   (vec (sort groups))}))))

(def ^:private filter-escapes
  {"*" "\\2a", "(" "\\28", ")" "\\29", "\\" "\\5c"})

(defn- ldap-escape
  [s]
  (str/replace (str s) #"[*()\\\x00]"
               (fn [m] (get filter-escapes m "\\00"))))

(defn- find-user
  [s username]
  (let [u (ldap-escape username)]
    (with-dir-ctx
      s
      (fn [^DirContext c]
        (let [sc   (doto (SearchControls.)
                     (.setSearchScope SearchControls/SUBTREE_SCOPE)
                     (.setCountLimit 2)
                     (.setReturningAttributes
                      (into-array String ["sAMAccountName" "userPrincipalName"])))
              filt (str "(&(objectCategory=person)(objectClass=user)"
                        "(|(sAMAccountName=" u ")(userPrincipalName=" u ")))")
              ^NamingEnumeration res (.search c (str (:base-dn s)) ^String filt sc)]
          (when (try (.hasMore res) (catch PartialResultException _ false))
            (let [^SearchResult r (.next res)
                  a     (.getAttributes r)
                  more? (try (.hasMore res) (catch PartialResultException _ false))]
              (when-not more?
                {:dn  (.getNameInNamespace r)
                 :sam (attr1 a "sAMAccountName")
                 :upn (attr1 a "userPrincipalName")}))))))))

(defn verify-user
  [s username password]
  (when (and (ready? s)
             (seq (str/trim (str (or username ""))))
             (seq (str (or password ""))))
    (when-let [{:keys [dn] :as u} (find-user s username)]
      (try
        (with-dir-ctx s dn (str password) (fn [_] true))
        (subject-for s u)
        (catch AuthenticationException _ nil)
        (catch NamingException e
          (println "[ldap] bind of" dn "failed:" (ex-message e))
          nil)))))

(defn snapshot
  [s found]
  (let [users (into (sorted-map)
                    (for [u found
                          :let [subject (subject-for s u)]
                          :when (seq subject)]
                      [subject
                       {:attributes (into {} (filter val) (select-keys u [:sam :display :mail]))
                        :groups     (set (:groups u))}]))]
    {:users  users
     :groups (into (sorted-map) (for [[_ {gs :groups}] users g gs] [g {}]))}))

(defn directory
  [s]
  (snapshot s (search-users s)))

(defn preview
  [s snap have]
  (let [mapped (keep (fn [[subject u]] (map-user s subject u)) (:users snap))]
    {:realm   (:realm s)
     :users   (mapv (fn [e]
                      (let [uid (get have (:subject e))]
                        (assoc e :action (if uid :exists :create) :user-id uid)))
                    mapped)
     :skipped (- (count (:users snap)) (count mapped))}))

(def module
  {:name        :auth-ldap
   :contributes {:identity/providers
                 {:ldap {:label     "LDAP"
                         :source    "spnego"
                         :settings  settings
                         :ready?    ready?
                         :password  verify-user
                         :directory directory
                         :preview   preview}}}})
