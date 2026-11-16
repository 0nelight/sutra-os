(ns sutra.cli.verbs.documents
  (:require [sutra.cli.dispatch :refer [step!]]
            [sutra.cli.errors :refer [ok? req!]]
            [sutra.cli.live :refer [live-schema parse-param-flags schema-sections]]
            [sutra.cli.local :refer [pe-document-template-spec pe-template-unit pe-unit-files
                                     vom-aufrufer]]
            [sutra.cli.out :refer [emit! kname mark paint render show-lines warn!]]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [sutra.cli.json :as json]
            [sutra.types.submission :as csub]
            [sutra.cli.view :refer [entity-names in-project]]
            [sutra.wire.api :as api]))

(defmethod step! :documents [ctx [_ {:keys [template project]}]]
  (let [rows (:documents (req! "documents" (api/documents template)))
        rows (if project
               (let [projects (:projects (schema-sections :projects))]
                 (when-not (some #{project} (entity-names projects))
                   (throw (ex-info (str "no project named " (pr-str project)
                                        " — `project list` names them")
                                   {:project project})))
                 (vals (in-project projects project (zipmap (range) rows))))
               rows)]
    (emit! ctx {:documents (vec rows)}))
  nil)

(defn- beim-aufrufer
  [p]
  (let [f   (io/file ^String p)
        pwd (System/getenv "SUTRA_CALLER_PWD")]
    (if (or (.isAbsolute f) (not (seq pwd))) p (.getPath (io/file pwd ^String p)))))

(defn- doc-of [id]
  (let [b (req! "document" (api/get! :document-of {:id (str id)}))]
    (or (:document b) b)))

(defn- doc-saved! [ctx d]
  (emit! ctx {:document (dissoc d :body)}))

(defmethod step! :document [ctx [_ {:keys [id out]}]]
  (let [d (doc-of id)]
    (if out
      (let [path (beim-aufrufer out)]
        (spit path (or (:body d) ""))
        (emit! ctx {:document (dissoc d :body) :file path}))
      (emit! ctx d ::document)))
  nil)

(defmethod render ::document [_ d]
  [(show-lines (dissoc d :body)) "" (or (:body d) "")])

(defn- format-of-file [path]
  (cond (re-find #"(?i)\.(md|markdown)$" path) "markdown"
        (re-find #"(?i)\.html?$" path)         "html"
        (re-find #"(?i)\.edn$" path)           "use-case"
        :else                                  "text"))

(defmethod step! :document-create [ctx [_ {:keys [file name format project]}]]
  (let [p    (vom-aufrufer file)
        body (cond-> {:name   (or name (str/replace (.getName (io/file ^String p)) #"\.[^.]+$" ""))
                      :format (or format (format-of-file p))
                      :body   (slurp p)}
               project (assoc :project project))
        b    (req! "document create" (api/post! :documents body))]
    (doc-saved! ctx (:document b)))
  nil)

(defmethod step! :document-update [ctx [_ {:keys [id file label endorse]}]]
  (if-not (or file label endorse)
    (warn!
     (mark :red "✗"
           "document update: --file und/oder --label/--endorse angeben — sonst gibt es nichts zu speichern"))
    (let [d    (doc-of id)
          body (cond-> (assoc (select-keys d
                                           [:name :format :body :template :template-version
                                            :parameters :page :links])
                              :expect-version (:version d))
                 file    (assoc :body (slurp (vom-aufrufer file)))
                 (or label endorse) (assoc :label (or label (if endorse "freigegeben" "Version")))
                 endorse (assoc :endorsed true))
          b    (req! "document update" (api/put! :document-of {:id (str id)} body))]
      (doc-saved! ctx (:document b))))
  nil)

(defmethod step! :document-generate [ctx [_ {:keys [template values version name]}]]
  (let [raw    (parse-param-flags values)
        params (clojure.core/when (seq raw)
                 (:parameters (req! "template"
                                    (api/get! :template-of {:name (clojure.core/name template)}))))
        body
        (cond-> {:values
                 (csub/decode-template-values params raw #(json/read-str % :key-fn keyword))}
          version (assoc :version version)
          name    (assoc :name name))
        b      (req! "document generate"
                     (api/post! :template-generate {:name (clojure.core/name template)} body))]
    (doc-saved! ctx (:document b)))
  nil)

(defmethod step! :document-refresh [ctx [_ {:keys [id latest]}]]
  (doc-saved! ctx (:document (req! "document refresh"
                                   (api/post! :document-regenerate {:id (str id)}
                                              (cond-> {} latest (assoc :latest true))))))
  nil)

(defmethod step! :document-restore [ctx [_ {:keys [id seq]}]]
  (if-not seq
    (warn! (mark :red "✗"
                 "document restore: --seq N fehlt (die Nummern zeigt `document versions <id>`)"))
    (let [b (req! "document restore" (api/post! :document-restore {:id (str id)} {:seq seq}))]
      (doc-saved! ctx (:document b))))
  nil)

(defmethod step! :document-export [ctx [_ {:keys [id html out]}]]
  (let [d    (clojure.core/when-not out (doc-of id))
        stem (str/replace (str (or (:name d) id)) #"[/\\:*?\"<>|]" "_")
        b    (req! "document export"
                   (api/post! :document-export {:id (str id)} {:targets [(if html "html" "pdf")]}))
        path (beim-aufrufer (or out (str stem (if html ".html" ".pdf"))))]
    (if html
      (do (spit path (:html b))
          (emit! ctx {:document id :file path}))
      (let [bytes (.decode (java.util.Base64/getDecoder) ^String (:pdf_b64 b))]
        (with-open [o (io/output-stream path)] (.write o ^bytes bytes))
        (emit! ctx {:document id :file path :bytes (count bytes)}))))
  nil)

(defmethod step! :document-templatize [ctx [_ {:keys [id template project repository]}]]
  (let [d       (doc-of id)
        src-tpl (clojure.core/when (:template d)
                  (let [r (api/get! :template-of {:name (name (:template d))})]
                    (clojure.core/when (ok? r) (:body r))))
        unit    (pe-template-unit
                 (pe-document-template-spec d src-tpl {:template (kname template)
                                                       :project (some-> project kname)
                                                       :repository (some-> repository kname)})
                 {})
        targets (or (seq (map (juxt :path :content)
                              (pe-unit-files (live-schema) :code-repository unit)))
                    (throw
                     (ex-info "document templatize: keine Textart der Projektion :code-repository nimmt eine Dokumentvorlage (:kind :doc-template)"
                              {:kind :undeclared-kind})))
        taken   (filter #(.exists (io/file ^String (beim-aufrufer (first %)))) targets)]
    (if (seq taken)
      (warn! (mark :red "✗" (str "document templatize: existiert schon — "
                                 (str/join ", " (map #(beim-aufrufer (first %)) taken))
                                 " (nichts überschrieben)")))
      (do
        (doseq [[p c] targets]
          (let [f (io/file ^String (beim-aufrufer p))]
            (io/make-parents f)
            (spit f c)))
        (emit! ctx {:files (mapv #(beim-aufrufer (first %)) targets)
                    :paths (mapv first targets)
                    :template (kname template)
                    :project (some-> project kname)
                    :from-template (:template d)}
               ::templatized))))
  nil)

(defmethod render ::templatized [_ {:keys [files paths project from-template]}]
  [(for [f files] (mark :cyan "◱" f))
   (clojure.core/when from-template
     (mark :yellow "!"
           (str "Parameter aus Vorlage " from-template
                " übernommen — der Rumpf ist aber der GERENDERTE Text: "
                "setze die {{ }}-Platzhalter wieder ein")))
   (clojure.core/when-not project
     (mark :yellow "!" "kein :project — mit --project <p> setzen oder in der .entt nachtragen"))
   (str "  " (paint :dim "Weiter:"))
   (str "    1. " (str/join " + " paths) " bearbeiten")
   (str "    2. bin/sutra render-template --file " (first paths) " --preview")
   (str "    3. bin/sutra --branch <b> projection push " (first files) " --preview"
        "   (im Checkout von `projection pull code-repository`)")
   (str "    4. … projection push " (first files)
        " → commit --yes → propose --source <b> --target main --yes")])

(defn- doc-intel-source [source kind]
  {:name (str source) :kind (or (some-> kind str) "dataset")})

(defn- strategy-of-file [file]
  (json/read-str (slurp (vom-aufrufer file)) :key-fn keyword))

(defmethod step! :doc-intel-run [ctx [_ {:keys [source kind address page strategy-file judge]}]]
  (let [body {:source   (doc-intel-source source kind)
              :address  (str address)
              :page     (parse-long (str (or page 0)))
              :strategy (strategy-of-file strategy-file)}
        run  (req! "document-intelligence run" (api/post! :doc-intel-extract body))
        ev   (clojure.core/when judge
               (req! "document-intelligence run"
                     (api/post! :doc-intel-evaluate
                                (assoc (dissoc body :strategy)
                                       :markdown (:markdown run) :model (str judge)))))]
    (emit! ctx (cond-> (dissoc run :blocks) ev (assoc :evaluation ev))))
  nil)

(defmethod step! :doc-intel-deploy
  [ctx [_ {:keys [name source kind strategy-file repository]}]]
  (let [d (req! "document-intelligence deploy"
                (api/post! :doc-intel-deploy
                           {:name       (clojure.core/name name)
                            :source     (doc-intel-source source kind)
                            :strategy   (strategy-of-file strategy-file)
                            :repository (str repository)}))]
    (emit! ctx (assoc (dissoc d :files) :files (vec (keys (:files d))))))
  nil)

(def ^:private op-glyph
  {"create" [:green "+"] "change" [:yellow "~"] "retire" [:red "−"] "keep" [:dim "="]
   "check" [:cyan "?"]})

(defmethod render ::use-case-plan [_ {:keys [steps hints summary preview staged retired]}]
  [(for [{:keys [op kind name auto why warn]} steps
         :let [[c g] (op-glyph (kname op) [:dim "·"])]]
     [(mark c g (str (paint :bold (str (kname kind) " " (kname name)))
                     (clojure.core/when auto (paint :green "  ✓ selbst"))
                     "  " why))
      (for [w warn] (str "      " (paint :yellow (str "⚠ " w))))])
   (clojure.core/when (seq hints)
     ["" (paint :bold "  offen:") (for [h hints] (str "    • " h))])
   ""
   (show-lines (cond-> {:summary summary}
                 preview (assoc :preview (select-keys preview [:added :updated :deleted
                                                               :refused? :problems]))
                 staged  (assoc :staged staged)
                 retired (assoc :retired retired)))])

(defn- use-case-steps [b]
  (mapv #(-> % (select-keys [:op :kind :name :auto? :why :warn])
             (set/rename-keys {:auto? :auto}))
        (:steps b)))

(defmethod step! :use-case-plan [ctx [_ {:keys [id preview]}]]
  (let [b (req! "use-case plan"
                (api/post! :use-case-plan {:id (str id)} {:preview? (boolean preview)}))]
    (emit! ctx {:document (get-in b [:document :name]) :steps (use-case-steps b)
                :hints (:hints b) :summary (:summary b) :preview (:preview b)}
           ::use-case-plan))
  nil)

(defmethod step! :use-case-apply [ctx [_ {:keys [id retire]}]]
  (let [b       (req! "use-case apply" (api/post! :use-case-apply {:id (str id)} {}))
        retired (clojure.core/when retire
                  (vec (for [[kind nm] (:retire b)
                             :let [k (keyword kind) n (kname nm)]]
                         (do (req! (str "delete " (kname k) " " n)
                                   (case k
                                     :object-type (api/delete-object-type n true)
                                     :link-type   (api/delete-conn-type n)
                                     :action-type (api/delete-action-type n)))
                             (str (kname k) " " n)))))]
    (emit! ctx {:branch (:branch b) :steps (use-case-steps b) :hints (:hints b)
                :summary (:summary b) :staged (:staged b) :retired retired}
           ::use-case-plan)
    (clojure.core/when (and (seq (:retire b)) (not retire))
      (warn! (mark :yellow "⚠" (str (count (:retire b)) " zurückzubauen — mit --retire löschen"))))
    (warn! (mark :cyan "→" "weiter: commit, dann propose --target main")))
  nil)

(defn- draft-views [body]
  (let [d ((requiring-resolve 'sutra.use-case.draft/read-draft) body)]
    (clojure.core/when-let [e (:sutra.use-case.draft/error d)]
      (throw (ex-info (str "kein Entwurf: " e) {})))
    (concat (clojure.core/when (:object-model d)
              [[(kname (or (get-in d [:object-model :id]) "objektmodell"))
                {:view "object-model"}]])
            (for [sc (:statecharts d) :let [c (kname (:id sc))]]
              [c {:view "statechart" :chart c}]))))

(defmethod step! :use-case-diagram [ctx [_ {:keys [id out]}]]
  (let [d     (doc-of id)
        dir   (io/file (beim-aufrufer (or out ".")))
        _     (.mkdirs dir)
        files (vec (for [[stem attrs] (draft-views (:body d))]
                     (let [html (:html (req! (str "use-case diagram " stem)
                                             (api/post! :document-widget
                                                        {:attrs (merge {:kind "use-case-draft"
                                                                        :draft (str id)}
                                                                       attrs)})))
                           b64  (second (re-find #"base64,([A-Za-z0-9+/=]+)" (str html)))
                           f    (io/file dir (str stem ".svg"))]
                       (if-not b64
                         (throw (ex-info (str stem ": " (str/replace (str html) #"<[^>]+>" ""))
                                         {}))
                         (do (with-open [o (io/output-stream f)]
                               (.write o ^bytes (.decode (java.util.Base64/getDecoder)
                                                         ^String b64)))
                             (.getPath f))))))]
    (emit! ctx {:document (:name d) :files files}))
  nil)
