(ns sutra.cli.auth
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [sutra.cli.credential :as credential]
            [sutra.cli.errors :refer [error-hint ok? req!]]
            [sutra.cli.instance :as inst]
            [sutra.cli.out :refer [mark paint read-secret warn!]]
            [sutra.cli.seal :as seal]
            [sutra.cli.site :as site]
            [sutra.i18n.core :as i18n]
            [sutra.wire.api :as api]
            [sutra.wire.creds :as creds]
            [sutra.wire.transport :as tx]))

(def default-creds-path ".run/cli-creds.edn")

(defn load-creds [path]
  (let [f (io/file path)]
    (if (.exists f) (edn/read-string (slurp f)) {})))

(defn bound-user [creds base as]
  (creds/bound-user creds base as (site/prod-url)))

(defn- host-file-name [base]
  (-> (str (creds/normalize-base base))
      (str/replace #"^[a-z]+://" "")
      (str/replace #"[^A-Za-z0-9.-]" "_")))

(defn creds-path-for
  [base]
  (let [d     (load-creds default-creds-path)
        prod? (= (creds/normalize-base base) (some-> (site/prod-url) creds/normalize-base))]
    (if (and (not (:refused (bound-user d base nil)))
             (or prod? (seq (:users d)) (:base-url d)))
      default-creds-path
      (str ".run/creds/" (host-file-name base) ".edn"))))

(defn save-creds!
  [path m]
  (let [base    (creds/normalize-base (tx/current-base))
        refused (when base (:refused (bound-user (load-creds path) base nil)))]
    (if refused
      (do (warn! (str "  ✗ " (i18n/t :cli/creds-not-saved (assoc refused :datei path))))
          nil)
      (do (io/make-parents (io/file path))
          (credential/nur-fuer-mich! (io/file path))
          (spit path (with-out-str (pprint/pprint (cond-> m base (assoc :base-url base)))))
          true))))

(defn gen-password
  []
  (let [b (byte-array 16)]
    (.nextBytes (java.security.SecureRandom.) b)
    (.encodeToString (.withoutPadding (java.util.Base64/getUrlEncoder)) b)))

(def ^:private default-admin-user "admin")

(defn- env-admin-pass []
  (some-> (System/getenv "SUTRA_ADMIN_PASSWORD") str/trim not-empty))

(defn login-credential
  [creds base env-pass]
  (let [{stored :user refused :refused} (bound-user creds base :admin)
        p (or (:password stored) env-pass)]
    (cond-> {:username (or (:username stored) default-admin-user)
             :password p
             :token    (when-not p (:token stored))
             :saved    (:token stored)}
      (and refused (nil? p)) (assoc :refused refused))))

(defn- live-token?
  [token username]
  (let [r (binding [tx/*auth-token* token]
            (try (api/get! :me) (catch Exception _ nil)))]
    (and (ok? r) (= username (:username (:body r))))))

(defn- keep-token!
  [path token]
  (let [c (load-creds path)]
    (cond
      (get-in c [:users :admin]) (save-creds! path (assoc-in c [:users :admin :token] token))
      (and (seq (:users c)) (nil? (:base-url c))) (save-creds! path c))))

(defn ensure-prod-login!
  [ctx]
  (when (nil? @(:token ctx))
    (let [{u :username p :password cached :token saved :saved refused :refused}
          (login-credential (load-creds (:creds-path ctx)) (tx/current-base) (env-admin-pass))
          _ (when refused
              (warn! (mark :red "✗" (i18n/t :cli/creds-other-instance
                                            (assoc refused :datei (:creds-path ctx))))))
          reused? (and p saved (not refused) (live-token? saved u))
          _ (when reused?
              (reset! (:token ctx) saved)
              (warn! (mark :green "✓" (i18n/t :cli/auto-logged-in-reused
                                              {:wer (paint :bold u)}))))
          r (when (and p (not reused?))
              (try (api/post! :login {:username u :password p})
                   (catch Exception e {:status 0 :threw e})))
          unreachable (boolean (and r (not (ok? r))
                                    (let [s (:status r)]
                                      (or (not (integer? s)) (zero? s) (<= 500 s 599)))))]
      (cond
        reused? nil

        (ok? r)
        (do (reset! (:token ctx) (:token (:body r)))
            (keep-token! (:creds-path ctx) (:token (:body r)))
            (warn! (mark :green "✓" (i18n/t :cli/auto-logged-in {:wer (paint :bold u)}))))

        unreachable
        (do (warn! (mark :red "✗" (str (i18n/t :cli/signin-no-answer) ": "
                                       (or (:error (:body r))
                                           (some-> (:threw r) ex-message)
                                           (str "HTTP " (:status r))))))
            (warn! (paint :cyan (str "    → " (i18n/t :cli/not-a-credential-problem) " "
                                     (error-hint (:status r) (:body r)
                                                 (str (:error (:body r))
                                                      " " (some-> (:threw r) ex-message)))))))

        cached
        (do (reset! (:token ctx) cached)
            (warn! (mark :green "✓" (paint :dim "using cached session token"))))))))

(def secret-sentinel "PLAINTEXT-AT-AUTHOR-TIME-ONLY")

(defn fill-secrets
  [srcs ask]
  (into {} (for [[k src] srcs]
             [k (update src :secrets
                        (fn [m] (into {} (for [[f v] m]
                                           [f (if (= v secret-sentinel)
                                                (let [in (ask (str (name k) "." (name f)))]
                                                  (if (seq in) in v))
                                                v)]))))])))

(defn- source-types
  []
  (or (:source-types (inst/facts))
      @(requiring-resolve 'sutra.parser.spec/default-source-types)))

(defn- descriptor-egress
  [descr src]
  ((requiring-resolve 'sutra.parser.spec/descriptor-egress) descr src))

(defn seal-schema-sources
  [ctx schema]
  (let [srcs (:sources schema)]
    (if (empty? srcs)
      schema
      (let [srcs   (into {} (for [[k src] srcs]
                              [k (update src :secrets merge (get (:secrets ctx) k))]))
            srcs   (fill-secrets srcs read-secret)
            srcs   (into {}
                         (for [[k src] srcs]
                           [k (if (some #(= secret-sentinel %) (vals (:secrets src)))
                                (do (warn! (paint :yellow
                                                  (str "  ↳ "
                                                       (i18n/t :cli/credential-skipped
                                                               {:quelle (name k)}))))
                                    (dissoc src :secrets))
                                src)]))
            pubkey (memoize (fn [q]
                              (:public-key (req! "egress pubkey" (api/get! :egress-pubkey q)))))
            key-of (fn [k src] (pubkey (if (:agent src)
                                         {:agent (name (:agent src))}
                                         {:source (name k)})))]
        (assoc schema :sources
               (into {}
                     (for [[k src] srcs]
                       (let [descr  (get (source-types) (:type src))
                             xf?    (= "external-transform" (some-> (:capability descr) name))
                             egress (or (not-empty (:egress src))
                                        (when descr
                                          (descriptor-egress
                                           descr (merge (dissoc src :config) (:config src)))))]
                         [k (cond-> (dissoc src :secrets)
                              (seq (:secrets src))
                              (assoc :secret-cipher
                                     (seal/seal (key-of k src) (pr-str (:secrets src))))
                              xf?              (assoc :external-transform true)
                              (seq egress)     (assoc :egress (vec egress)))]))))))))
