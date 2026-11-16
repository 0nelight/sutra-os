(ns sutra.cli.verbs.platform
  (:require [sutra.cli.dispatch :refer [step!]]
            [sutra.cli.errors :refer [ok? req!]]
            [sutra.cli.live :refer [live-schema parse-param-flags]]
            [sutra.cli.local :refer [authoring-read-file checkout-of resolve-code-files
                                     vom-aufrufer]]
            [sutra.cli.out :refer [emit! mark render warn!]]
            [sutra.cli.json :as json]
            [sutra.i18n.core :as i18n]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [sutra.types.submission :as csub]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]
            [sutra.cli.sandbox :as sandbox]))

(defn- checkout-text
  [live path]
  (let [{:keys [rel formats]} (checkout-of live path)
        r (rel (io/file ^String path))]
    (or (authoring-read-file formats r (slurp path))
        (throw (ex-info (str path ": no kind of text of this checkout's projection reads it")
                        {:file path})))))

(defmethod step! :blob [ctx [_ {:keys [hash out]}]]
  (let [{:keys [status bytes content-type]} (tx/get-bytes! (str "/api/blob/" hash))]
    (if (<= 200 status 299)
      (let [ext  (or (some-> content-type (str/split #";") first str/trim
                             {"application/pdf" ".pdf"
                              "application/vnd.oasis.opendocument.spreadsheet" ".ods"
                              "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
                              ".xlsx"
                              "text/csv" ".csv" "text/plain" ".txt" "image/png" ".png"
                              "image/jpeg" ".jpg" "application/zip" ".zip"
                              "application/json" ".json"})
                     ".bin")
            file (if (str/blank? (str out))
                   (sandbox/scratch-file (str hash ext))
                   (io/file out))
            path (.getPath file)]
        (with-open [o (io/output-stream file)] (.write o ^bytes bytes))
        (emit! ctx {:path path :bytes (count bytes)})
        {:kind :file :path path})
      (do (warn! (mark :red "✗" (str "blob " hash " → HTTP " status
                                     (when (= 401 status)
                                       (str " (" (i18n/t :cli/not-authorized) ")")))))
          nil))))

(def ^:private logo-mimes
  {"png" "image/png" "jpg" "image/jpeg" "jpeg" "image/jpeg" "gif" "image/gif"
   "webp" "image/webp" "svg" "image/svg+xml" "ico" "image/x-icon"})

(defn- logo-data-uri
  [path]
  (let [f   (io/file path)
        ext (str/lower-case (or (second (re-find #"\.([A-Za-z0-9]+)$" (.getName f))) ""))]
    (when-not (.exists f)
      (throw (ex-info (str "no such image: " path) {:file path})))
    (when-not (logo-mimes ext)
      (throw (ex-info (str "unsupported image type " (pr-str ext) " — one of: "
                           (str/join ", " (sort (keys logo-mimes))))
                      {:file path})))
    (str "data:" (logo-mimes ext) ";base64,"
         (.encodeToString (java.util.Base64/getEncoder)
                          (java.nio.file.Files/readAllBytes (.toPath f))))))

(defmethod step! :platform [ctx [_ {:keys [title logo file clear]}]]
  (when (and logo (not file))
    (throw (ex-info "`--logo <size>` needs `--file <image>` — use `--clear <size>` to drop one"
                    {:logo logo})))
  (let [payload (cond-> {}
                  (some? title) (assoc :title title)
                  logo          (assoc-in [:logo (keyword (name logo))] (logo-data-uri file))
                  clear         (assoc-in [:logo (keyword (name clear))] ""))
        {:keys [title logo uploaded]}
        (if (seq payload)
          (req! "set platform experience" (api/post! :platform-experience-set payload))
          (req! "platform experience" (api/get! :platform-experience)))]
    (emit! ctx {:title title :logo logo :uploaded uploaded})
    nil))

(defmethod step! :apps [ctx [_ _]]
  (let [appmap (:applications (req! "apps" (api/get! :applications)))
        rows   (sort-by :name
                        (for [[nm spec] appmap]
                          {:name        (clojure.core/name nm)
                           :published   (some-> (:published spec) str)
                           :releases    (count (:releases spec))
                           :auto        (true? (:auto-publish spec))
                           :object-type (some-> (:object-type spec) clojure.core/name)}))]
    (emit! ctx {:applications (vec rows)}))
  nil)

(defmethod step! :templates [ctx [_ _]]
  (let [rows (sort-by :name
                      (for [t (:templates (req! "templates" (api/get! :templates)))]
                        {:name    (some-> (:template-name t) clojure.core/name)
                         :title   (:title t)
                         :format  (some-> (:format t) clojure.core/name)
                         :status  (some-> (:status t) clojure.core/name)
                         :version (:version t)
                         :params  (count (:parameters t))}))]
    (emit! ctx {:templates (vec rows)}))
  nil)

(defmethod step! :template [ctx [_ {:keys [name version]}]]
  (let [t (req! "template" (api/get! :template-of {:name (clojure.core/name name)}))
        t (if version
            (assoc t :version version)
            t)]
    (emit! ctx t))
  nil)

(defn- draft-template
  [path]
  (let [p         (vom-aufrufer path)
        [nm spec] (-> (checkout-text (live-schema) p) :schema
                      (resolve-code-files p) :templates first)]
    (when-not nm
      (throw (ex-info (str path " declares no template") {:file path})))
    [nm (-> spec (dissoc :releases) (assoc :name nm))]))

(defmethod step! :render-template
  [ctx [_ {:keys [name version values pdf preview out file layout]}]]
  (let [[dname draft] (clojure.core/when file (draft-template file))
        name   (or dname name)
        raw    (parse-param-flags values)
        params (clojure.core/cond
                 draft       (:parameters draft)
                 (and name
                      (seq raw))
                 (:parameters (:body (api/get! :template-of {:name (clojure.core/name name)}))))
        body
        (cond-> {:values
                 (csub/decode-template-values params raw #(json/read-str % :key-fn keyword))
                 :targets (if pdf ["pdf"] ["html"])}
          draft   (assoc :draft draft)
          (and version (not draft)) (assoc :version version)
          layout  (assoc :layout (clojure.core/name layout))
          preview (assoc :preview true))
        r      (if name
                 (api/post! :template-render {:name (clojure.core/name name)} body)
                 {:status 400
                  :body {:error "render-template needs a template name or --file <x.entt>"}})
        b      (:body r)]
    (cond
      (not (ok? r))
      (warn! (mark :red "✗"
                   (str "render-template: " (or (:error b) (:error r) (i18n/t :cli/failed)))))

      (and draft (not (:draft b)))
      (warn!
       (mark :red "✗"
             "render-template --file: this server predates draft rendering and answered with the INSTALLED template — nothing of the checkout was rendered"))

      pdf
      (let [bytes (.decode (java.util.Base64/getDecoder) ^String (:pdf_b64 b))
            path  (or out (str (clojure.core/name name) ".pdf"))]
        (with-open [o (io/output-stream path)] (.write o ^bytes bytes))
        (emit! ctx {:template name :file path :bytes (count bytes)}))

      out
      (do (spit out (:html b))
          (emit! ctx {:template name :file out}))

      :else
      (emit! ctx (select-keys b [:html :format]) ::html)))
  nil)

(defmethod render ::html [_ {:keys [html]}] (str html))
