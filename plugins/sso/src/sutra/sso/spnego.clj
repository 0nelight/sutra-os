(ns sutra.sso.spnego
  (:require [sutra.kit.err :as err])
  (:import (java.util Base64)
           (java.nio.file Files OpenOption)
           (java.nio.file.attribute FileAttribute PosixFilePermissions)
           (javax.security.auth Subject)
           (javax.security.auth.login LoginContext Configuration
                                      AppConfigurationEntry
                                      AppConfigurationEntry$LoginModuleControlFlag)
           (java.security PrivilegedExceptionAction)
           (org.ietf.jgss GSSCredential GSSManager GSSName Oid)))

(set! *warn-on-reflection* true)

(def settings
  {:enabled {:key "spnego_enabled" :type :boolean}
   :realm   {:key "spnego_realm"   :type :string}
   :kdc     {:key "spnego_kdc"     :type :string}
   :spn     {:key "spnego_spn"     :type :string}
   :keytab  {:key "spnego_keytab_b64" :type :base64 :secret? true :input :keytab-b64}})

(defn ready?
  [{:keys [enabled realm kdc spn keytab]}]
  (boolean (and enabled (every? #(seq (str %)) [realm kdc spn keytab]))))

(defonce ^:private svc (atom nil))

(defn- fingerprint [s]
  (hash (select-keys s [:realm :kdc :spn :keytab])))

(defn- write-keytab!
  [b64]
  (let [bytes (.decode (Base64/getDecoder) ^String b64)
        attr  (PosixFilePermissions/asFileAttribute
               (PosixFilePermissions/fromString "rw-------"))
        path  (Files/createTempFile "sutra-krb5-" ".keytab"
                                    (into-array FileAttribute [attr]))]
    (Files/write path bytes ^"[Ljava.nio.file.OpenOption;" (into-array OpenOption []))
    (.deleteOnExit (.toFile path))
    (.getAbsolutePath (.toFile path))))

(defn- jaas-config
  ^Configuration [spn keytab-path]
  (proxy [Configuration] []
    (getAppConfigurationEntry [_name]
      (into-array
       AppConfigurationEntry
       [(AppConfigurationEntry.
         "com.sun.security.auth.module.Krb5LoginModule"
         AppConfigurationEntry$LoginModuleControlFlag/REQUIRED
         {"useKeyTab"         "true"
          "keyTab"            keytab-path
          "principal"         spn
          "storeKey"          "true"
          "doNotPrompt"       "true"
          "isInitiator"       "false"
          "refreshKrb5Config" "true"})]))))

(defn- write-krb5-conf!
  [realm kdc]
  (let [content (str "[libdefaults]\n"
                     "  default_realm = " realm "\n"
                     "  allow_weak_crypto = true\n"
                     "  dns_lookup_kdc = false\n"
                     "  dns_lookup_realm = false\n"
                     "  permitted_enctypes   = aes256-cts-hmac-sha1-96 aes128-cts-hmac-sha1-96 "
                     "arcfour-hmac-md5\n"
                     "  default_tkt_enctypes = aes256-cts-hmac-sha1-96 aes128-cts-hmac-sha1-96 "
                     "arcfour-hmac-md5\n"
                     "  default_tgs_enctypes = aes256-cts-hmac-sha1-96 aes128-cts-hmac-sha1-96 "
                     "arcfour-hmac-md5\n"
                     "[realms]\n"
                     "  " realm " = {\n"
                     "    kdc = " kdc "\n"
                     "  }\n")
        path    (Files/createTempFile "sutra-krb5-" ".conf"
                                      (into-array FileAttribute []))]
    (Files/write path (.getBytes content "UTF-8")
                 ^"[Ljava.nio.file.OpenOption;" (into-array OpenOption []))
    (.deleteOnExit (.toFile path))
    (.getAbsolutePath (.toFile path))))

(defn- build-subject!
  [{:keys [realm kdc spn keytab]}]
  (let [ktb   (write-keytab! keytab)
        conf  (write-krb5-conf! realm kdc)]
    (System/setProperty "java.security.krb5.conf" conf)
    (System/setProperty "javax.security.auth.useSubjectCredsOnly" "false")
    (let [lc (LoginContext. "sutra-spnego" nil nil (jaas-config spn ktb))]
      (.login lc)
      {:subject (.getSubject lc) :keytab-path ktb})))

(defn- ensure!
  [settings]
  (let [fp (fingerprint settings)
        s  @svc]
    (if (and s (= fp (:fp s)))
      (:subject s)
      (let [{:keys [subject]} (build-subject! settings)]
        (reset! svc {:fp fp :subject subject})
        subject))))

(def ^:private krb5-oid  (Oid. "1.2.840.113554.1.2.2"))
(def ^:private spnego-oid (Oid. "1.3.6.1.5.5.2"))

(defn accept-negotiate
  [settings b64-token]
  (when (and (ready? settings) (seq b64-token))
    (try
      (let [^Subject subject (ensure! settings)
            in-bytes  (.decode (Base64/getDecoder) ^String b64-token)
            action    (reify PrivilegedExceptionAction
                        (run [_]
                          (let [mgr (GSSManager/getInstance)
                                ^GSSName no-name nil
                                cred (.createCredential
                                      mgr no-name GSSCredential/INDEFINITE_LIFETIME
                                      ^"[Lorg.ietf.jgss.Oid;" (into-array Oid [krb5-oid spnego-oid])
                                      GSSCredential/ACCEPT_ONLY)
                                ctx  (.createContext mgr cred)
                                out  (.acceptSecContext ctx in-bytes 0 (alength in-bytes))
                                done (.isEstablished ctx)]
                            {:complete? done
                             :subject   (when done (str (.getSrcName ctx)))
                             :out-token (when (and out (pos? (alength out)))
                                          (.encodeToString (Base64/getEncoder) out))})))]
        (Subject/doAs subject action))
      (catch Exception e
        (println "WARN spnego: accept failed —" (.getName (class e)) "|"
                 (pr-str (.getMessage e))
                 (when (instance? org.ietf.jgss.GSSException e)
                   (str " [major=" (.getMajorString ^org.ietf.jgss.GSSException e)
                        " | minor=" (.getMinorString ^org.ietf.jgss.GSSException e) "]")))
        (doseq [^Throwable c (rest (err/causes e))]
          (println "  caused by:" (.getName (class c)) "|" (pr-str (.getMessage c))))
        nil))))

(def module
  {:name        :auth-spnego
   :contributes {:identity/providers
                 {:spnego {:label     "Kerberos/SPNEGO"
                           :source    "spnego"
                           :settings  settings
                           :ready?    ready?
                           :negotiate accept-negotiate}}}})
