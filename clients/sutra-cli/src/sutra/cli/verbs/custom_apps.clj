(ns sutra.cli.verbs.custom-apps
  (:require [sutra.cli.dispatch :refer [step!]]
            [sutra.cli.errors :refer [req!]]
            [sutra.cli.live :refer [backing* live-schema]]
            [sutra.cli.out :refer [emit! render show-lines]]
            [sutra.i18n.core :as i18n]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [sutra.types.rid :as rid]
            [sutra.types.shape :as tshape]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(defn- sdk-fingerprint          [& a]
  (apply @(requiring-resolve 'platform-sdk.schema/fingerprint) a))
(defn- sdk-write!               [& a] (apply @(requiring-resolve 'platform-sdk.package/write!) a))
(defn- sdk-stamped-fingerprint  [& a]
  (apply @(requiring-resolve 'platform-sdk.package/stamped-fingerprint) a))
(defn- sdk-generate             [& a] (apply @(requiring-resolve 'platform-sdk.gen/generate) a))

(defmethod step! :sdk-fingerprint [ctx [_ _]]
  (emit! ctx {:fingerprint (sdk-fingerprint (live-schema))}))

(defn- sdk-runtime
  [{:keys [runtime-git runtime-sha]}]
  (if (and runtime-git runtime-sha)
    {:git runtime-git :sha runtime-sha}
    {:root (let [f (io/file "libs/core")] (when (.isDirectory f) (.getCanonicalPath f)))}))

(defn- sdk-scope
  [schema app]
  (if-not app
    schema
    (let [find-app @(requiring-resolve 'platform-sdk.scope/find-app)
          narrow   @(requiring-resolve 'platform-sdk.scope/narrow)]
      (narrow schema (or (find-app schema app)
                         (throw (ex-info (str "no custom application " app) {:app app})))))))

(defmethod step! :sdk-generate [ctx [_ {:keys [ns out ontology package? app target] :as args}]]
  (let [schema (sdk-scope (live-schema) app)
        opts   {:ns (symbol ns)
                :target (keyword (or target "clj"))
                :ontology (or ontology "sutra")
                :backing (backing*)
                :package? (boolean package?)
                :runtime (sdk-runtime args)}]
    (if out
      (let [{:keys [fingerprint written version]} (sdk-write! schema opts out)]
        (emit! ctx {:out out :ns ns :fingerprint fingerprint :version version :written written}))
      (let [{:keys [files]} (sdk-generate schema opts)]
        (emit! ctx {:ns ns :files (into (sorted-map) files)} ::sdk-files)))))

(defmethod render ::sdk-files [_ {:keys [files]}]
  (for [[path src] files]
    [(str ";; ── " path " " (apply str (repeat (max 1 (- 70 (count path))) "─")))
     src]))

(defmethod render ::source [_ {:keys [source]}] (str source))

(defmethod render ::markdown [_ {:keys [markdown]}] (str markdown))

(defmethod step! :custom-app-sdk [ctx [_ {:keys [name out]}]]
  (let [r (req! (str "sdk " name) (api/get! :custom-app-sdk {:name name}))]
    (if out
      (let [written (vec (for [[path src] (sort (:files r))
                               :let [f (io/file out path)]]
                           (do (io/make-parents f)
                               (spit f (str src "\n"))
                               (.getPath f))))]
        (emit! ctx (-> r (dissoc :source :files) (assoc :written written))))
      (emit! ctx (select-keys r [:source]) ::source))))

(defmethod step! :custom-app-docs [ctx [_ {:keys [name out]}]]
  (let [r (req! (str "docs " name) (api/get! :custom-app-docs {:name name}))]
    (if out
      (do (spit out (str (:markdown r) "\n"))
          (emit! ctx {:out out}))
      (emit! ctx (select-keys r [:markdown]) ::markdown))))

(defmethod step! :custom-app-client-mint
  [ctx [_ {:keys [name grants redirect]}]]
  (let [body (cond-> {}
               grants       (assoc :grant-types
                                   (mapv keyword (str/split (str grants) #",")))
               redirect     (assoc :redirect-uris [redirect]))
        r (req! (str "mint client " name) (api/post! :custom-app-clients {:name name} body))]
    (emit! ctx r ::minted)))

(defmethod render ::minted [_ r]
  [(show-lines r)
   "  Das Geheimnis wird GENAU EINMAL gezeigt — ein verlorenes wird rotiert, nie wiedergeholt."
   (when-let [svc (:service-user r)]
     (str "  Dienstbenutzer " svc
          " angelegt, noch ohne Rechte: sutra access grant <projekt> --role <rolle> "
          "--to " svc))])

(defmethod step! :sdk-verify [ctx [_ {:keys [file]}]]
  (let [stamped (sdk-stamped-fingerprint file)
        live    (sdk-fingerprint (live-schema))
        ok?     (= stamped live)]
    (emit! ctx {:file file :ok ok? :stamped stamped :live live})
    (when-not ok?
      (throw (ex-info (i18n/t :cli/sdk-drift
                              {:datei file :stand (or stamped (i18n/t :cli/no-fingerprint))
                               :live live})
                      {:desc "sdk verify" :file file
                       :stamped stamped :live live})))))

(defn- custom-app-host
  [app]
  (let [sub (get-in app [:hosting :subdomain])]
    (when (seq (str sub))
      (let [base (or (tx/current-base) "")
            host (str/replace (str/replace base #"^https?://" "") #"/.*$" "")]
        (str sub "." host)))))

(defn- custom-app-findings
  [lint nm app]
  (let [cb (some-> (custom-app-host app) (as-> h (str "https://" h "/auth/callback")))]
    (vec
     (concat
      (filter #(and (= "custom-applications" (some-> (:entity-class %) clojure.core/name))
                    (str/starts-with? (str (:entity %)) (str nm ".")))
              lint)
      (when (and cb (not-any? #(= cb (str %)) (get-in app [:client :redirect-uris])))
        [{:rule :hosted-callback :severity :error :entity (str nm ".client")
          :message (str "the hosted callback " cb " is not one of :client :redirect-uris")}])))))

(defn- custom-app-decl
  [k app]
  (let [print-ent @(requiring-resolve 'sutra.parser.canonical-print/canonical-ent-str)]
    (str/trimr (print-ent {:custom-applications
                           {(keyword k) (tshape/authored-entry :custom-applications app)}}))))

(defmethod step! :custom-app [ctx [_ {:keys [name declaration?]}]]
  (let [sch   (rid/name-keyed (live-schema))
        lint  (memoize #(:findings (req! "lint" (api/get! :schema-lint))))
        decl  (or (:custom-applications sch) {})
        ->n   (fn [x] (cond (keyword? x) (clojure.core/name x) (some? x) (str x)))]
    (if name
      (let [app (or (get decl (keyword name)) (get decl name)
                    (throw (ex-info (str "no custom application named " (pr-str name)
                                         " — run `custom-app` to list them")
                                    {:desc "custom-app" :name name})))]
        (if declaration?
          (emit! ctx {:name name :declaration (custom-app-decl name app)} ::declaration)
          (emit! ctx {:name name :application app
                      :findings (custom-app-findings (lint) name app)})))
      (emit! ctx {:custom-applications
                  (vec (for [[k app] (sort-by clojure.core/key decl)]
                         {:name (->n k)
                          :display-name (:display-name app)
                          :status (->n (:status app))
                          :hosting (custom-app-host app)
                          :sdk (get-in app [:sdk :namespace])
                          :findings (count (custom-app-findings (lint) (->n k) app))}))})))
  nil)

(defmethod render ::declaration [_ {:keys [declaration]}] (str declaration))
