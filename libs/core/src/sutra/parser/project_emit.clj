(ns sutra.parser.project-emit
  (:require [sutra.kit.refusal :as refusal]
            [sutra.types.languages :as langs]
            [sutra.kit.tree :as tree]
            [sutra.types.tree :as body-tree]
            [sutra.types.attribute :as attribute]
            [sutra.types.authority :as authority]
            [sutra.types.shape :as tshape]
            [sutra.types.entities :as ents]
            [sutra.types.versions :as vers]
            [sutra.types.world-rules :as world-rules]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [sutra.parser.ent :as ent]
            [sutra.parser.ent-view :as ent-view]
            [sutra.parser.ontology :as onto]
            [sutra.parser.sugar :as sugar]
            [sutra.capps.layout :as lay]
            [sutra.capps.refs :as refs]
            [sutra.parser.spec :as spec]
            [malli.util :as mu]
            [sutra.types.rid :as rid]
            [sutra.types.schema :as sch]
            [sutra.types.backing :as backing]))

(set! *warn-on-reflection* true)

(defn snake [nm] (str/replace (name nm) "-" "_"))
(defn py-filename  [nm] (str (snake nm) ".py"))
(defn clj-filename [nm] (str (snake nm) ".clj"))
(defn cljs-filename [nm] (str (snake nm) ".cljs"))

(defn- template-ext
  [layout]
  (or (some->> (get-in layout [:export :filename])
               (re-find #"\.([A-Za-z0-9]+)$")
               second
               (str "."))
      ".ods"))

(defn extract-app-template
  [app-name layout]
  (if-let [b64 (get-in layout [:export :template-b64])]
    (let [path  (str "exports/" (snake app-name) (template-ext layout))
          bytes (.decode (java.util.Base64/getDecoder) ^String b64)]
      {:layout (-> layout
                   (update :export dissoc :template-b64)
                   (assoc-in [:export :template-file] path))
       :file   {:path path :bytes bytes}})
    {:layout layout}))

(defn names-resolved
  [idx kinds-key section]
  (let [resolve (fn [_targets v] (get idx v v))]
    (update-vals section #(rid/rewrite-refs kinds-key % resolve))))

(defn- with-imported
  [projects rid]
  (filterv projects (tree/reach #(get-in projects [% :imports]) [rid])))

(defn- with-owner [terse spec]
  (into terse (filter val) (select-keys spec [:project :ignore-inherited :repository])))

(defn- project-rid
  [schema pname]
  (when pname
    (or (let [r (sch/rid-of (sch/index schema) :projects pname)]
          (when (contains? (:projects schema) r) r))
        (refusal/refuse! :refusal.parser/unknown-project
                        {:kind    ::unknown-project
                         :project (name pname)
                         :known   (into (sorted-set)
                                        (keep #(some-> (:project-name %) name))
                                        (vals (:projects schema)))}))))

(defn repository-rid
  [schema rname]
  (when rname
    (or (let [r (sch/rid-of (sch/index schema) :repositories rname)]
          (when (contains? (:repositories schema) r) r))
        (refusal/refuse! :refusal.parser/unknown-repository
                        {:kind       ::unknown-repository
                         :repository (name rname)
                         :known      (into (sorted-set)
                                           (keep #(some-> (:repository-name %) name))
                                           (vals (:repositories schema)))}))))

(defn- owned-by
  [section proj-rid]
  (into {} (filter (fn [[_ spec]] (= proj-rid (:project spec)))) section))

(defn- attr-tuples
  [attr-list]
  (mapv attribute/->tuple attr-list))

(defn- emit-object [spec ext vts sps]
  (let [attrs (sugar/raise-attrs (:attribute-list spec) vts sps)]
    (cond-> (sugar/raise-display-name
             (tshape/->author :object-types spec
                              (cond-> {:attributes attrs}
                                (seq (:implements spec))
                                (assoc :implements (vec (:implements spec))))))
      ext (assoc :external-source ext))))

(def ^:private endpoint-fk-producer? backing/endpoint-fk-producer?)

(defn- emit-input [in]
  (into {} (remove (comp nil? val))
        (select-keys in (mu/keys onto/InputSpec))))

(defn- emit-output [out]
  (cond-> (dissoc out :dataset)
    (and (:dataset out) (= :dataset (:kind out)))
    (assoc :dataset (:dataset out))))

(def ^:private code-opt-keys
  [:entry-point :incremental :semantic-version :done-key :max-per-run :allowed-run-seconds
   :prompt-key :profile :container
   :resources
   :timeout-ms :returns :version :status :deprecation :description :rationale :tests
   :ignore-inherited])

(defn- code-opts [spec m inline?]
  (cond-> (into m (filter val) (select-keys spec code-opt-keys))
    (and inline? (seq (:releases spec))) (assoc :releases (:releases spec))
    (seq (:external-systems spec)) (assoc :external-systems (vec (:external-systems spec)))))

(defn- wiring [spec {:keys [inputs]}]
  (merge (when-let [in (:input spec)]
           {:input (merge (emit-input in)
                          (select-keys (:source spec) [:table :query]))})
         (select-keys spec [:from-key-column :to-key-column :fk-attribute :match-column
                            :shared-property :strategy :sources :ops :board])
         (when (and (or (= :always inputs) (seq (:inputs spec)))
                    (or (seq (:inputs spec)) (nil? (:input spec))))
           {:inputs (update-vals (:inputs spec) emit-input)})
         (when-let [o (:output spec)] {:output (emit-output o)})
         (when-let [os (seq (:outputs spec))] {:outputs (mapv emit-output os)})
         (when-let [mo (not-empty (:media-outputs spec))] {:media-outputs mo})
         (when-let [oc (seq (:output-columns spec))] {:output-columns (attr-tuples oc)})
         (when-let [ps (seq (:parameters spec))] {:parameters (into {} ps)})
         (when-let [k (:primary-key spec)] {:primary-key k})
         (when (seq (:column-map spec)) {:column-map (:column-map spec)})))

(def ^:private transform-langs
  (let [files {:python py-filename :clojure clj-filename}]
    (into {nil {:inputs :always}}
          (for [lang langs/compute-language-names]
            [lang (cond-> {:inputs (langs/export-inputs lang)}
                    (files lang) (assoc :file (files lang)))]))))

(defn code-path
  [spec]
  (when-let [file (:file (transform-langs (:language spec)))]
    (or (:code-path spec) (str "transforms/" (file (:transform-name spec))))))

(defn emit-transform [spec {:keys [inline?]}]
  (when-let [{:keys [file] :as row} (transform-langs (:language spec))]
    (when-not (endpoint-fk-producer? spec)
      (let [nm        (:transform-name spec)
            lang      (:language spec)
            path      (code-path spec)
            body      (cond (nil? file) (or (when-let [q (:query spec)] {:query q})
                                            (when-let [c (:code spec)] {:code c}))
                            inline?     {:code (:code spec)}
                            :else       {:code-file path})
            declared? (and file (= :code (:spec-source spec)))]
        {:entry [nm (-> (code-opts (cond->> spec
                                     declared? (#(apply dissoc % ents/decorator-fields)))
                                   (merge (when lang {:language lang})
                                          body
                                          (if declared?
                                            (when-let [kept (not-empty
                                                             (select-keys
                                                              (:output spec)
                                                              ents/output-ontology-fields))]
                                              {:output kept})
                                            (wiring spec row)))
                                   inline?)
                        (with-owner spec))]
         :file  (when (and path (not inline?)) {:path path :content (or (:code spec) "")})
         :modules (when (and (= :python lang) (not inline?))
                    (for [[m src] (sort-by key (:modules spec))]
                      {:path (str "transforms/" m ".py") :content src}))}))))

(def migrate-banner
  (str ";; ══════════════════════════════════════════════════════════════════════════\n"
       ";; ⚠  DEPRECATED — SCI-.cljs-View. Wird durch ein declaratives apps/<name>.enta\n"
       ";;    abgelöst (Layout + :vars mit :expr/:derived/:function statt handgeschrie-\n"
       ";;    benem SCI-Code). Die FE rendert ein Daten-:layout VOR dieser Datei — eine\n"
       ";;    App, die beides trägt, liest sie nicht mehr und liefert sie trotzdem aus.\n"
       ";;    LÖSCHEN: erst das :layout autoren und pushen, dann in EINEM Commit das\n"
       ";;    :source-file aus der .enta nehmen UND diese Datei `git rm`en. Der Install\n"
       ";;    retrahiert dann das gespeicherte :source, die Projektion emittiert kein\n"
       ";;    .cljs mehr, und `normalized by install` nimmt es aus dem Baum. Nur das\n"
       ";;    :source-file zu streichen und die Datei stehen zu lassen (oder umgekehrt)\n"
       ";;    ist KEINE Rücknahme.\n"
       ";;    Doku: resources/presets/grammar.enta.\n"
       ";; ══════════════════════════════════════════════════════════════════════════\n"))

(defn with-migrate-banner
  [source]
  (if (str/starts-with? (str source) migrate-banner) source (str migrate-banner source)))

(def ^:private banner-rule ";; ═")
(def ^:private banner-mark "DEPRECATED")

(defn strip-migrate-banner
  [source]
  (let [s (str source)]
    (cond
      (str/starts-with? s migrate-banner) (subs s (count migrate-banner))

      (str/starts-with? s banner-rule)
      (let [lines (str/split-lines s)
            close (first (keep-indexed
                          (fn [i l] (when (and (pos? i)
                                               (str/starts-with? l ";;")
                                               (str/starts-with? l banner-rule))
                                      i))
                          (take-while #(str/starts-with? % ";;") lines)))]
        (if (and close (some #(str/includes? % banner-mark) (take (inc close) lines)))
          (str/join "\n" (drop (inc close) lines))
          source))

      :else source)))

(defn- emit-application [spec {:keys [inline?]}]
  (let [nm     (:application-name spec)
        file   (str "apps/" (cljs-filename nm))
        {:keys [layout tpl-file]}
        (if inline?
          {:layout (:layout spec)}
          (let [{:keys [layout file]} (extract-app-template nm (:layout spec))]
            {:layout layout :tpl-file file}))
        base   (cond-> (-> (vers/authored :applications (tshape/->author :applications spec {}))
                           (dissoc :layout :source)
                           (with-owner spec))
                 (seq layout) (assoc :layout layout))
        src?   (seq (:source spec))]
    {:entry [nm (cond-> base
                  (and inline?       src?) (assoc :source (:source spec))
                  (and (not inline?) src?) (assoc :source-file file))]
     :files (when-not inline?
              (cond-> []
                src?     (conj {:path file :content (with-migrate-banner (:source spec))})
                tpl-file (conj tpl-file)))}))

(defn- emit-view-file
  [ot-name views]
  (let [views   (mapv (fn [[k spec]]
                        (clojure.lang.MapEntry/create
                         k (refs/lift-application spec (constantly ot-name))))
                      views)
        proj    (some (comp :project val) views)
        sidebar (some (fn [[_ spec]] (when (= :full (keyword (:form-factor spec)))
                                       (not-empty (:sidebar spec))))
                      views)
        live    (some (comp :live val) views)
        auto?   (some (comp true? :auto-publish val) views)
        lifted  (fn [k] (some (comp not-empty k :layout val) views))]
    (into (cond-> {:object-type ot-name}
            proj    (assoc :project proj)
            auto?   (assoc :auto-publish true)
            sidebar (assoc :sidebar sidebar)
            live    (assoc :live live)
            (lifted :aggregate-types) (assoc :aggregate-types (lifted :aggregate-types))
            (lifted :functions) (assoc :functions (lifted :functions)))
          (map (fn [[_ spec]] [(:form-factor spec)
                               (-> (lay/lower-view-body (:layout spec))
                                   ent-view/without-derived-actions
                                   (dissoc :aggregate-types :functions))]))
          views)))

(defn app-unit
  [spec opts]
  (let [{[nm app-map] :entry files :files} (emit-application (refs/lift-application spec) opts)
        lay  (:layout app-map)
        body (some-> lay (as-> l (apply dissoc l (map first ent/app-lifted-keys))) not-empty)]
    {:kind     :app
     :name     (name nm)
     :nodes    [[:applications (name nm)]]
     :doc      (ent/fold-app-lifted
                (cond-> (-> (dissoc app-map :layout) (assoc :name nm))
                  body (assoc :layout body))
                lay)
     :siblings (vec files)}))

(def ^:private template-extension
  {:html "html.j2" :markdown "md.j2" :text "txt.j2"})

(defn template-filenames
  [nm]
  (mapv #(str (snake nm) "." %) (vals template-extension)))

(defn- template-body-path
  [spec]
  (str "doc-templates/" (snake (:template-name spec)) "."
       (template-extension (or (:format spec) :html) "html.j2")))

(defn template-decl
  [spec {:keys [inline?]}]
  (-> (tshape/->author :templates spec {})
      (dissoc :source)
      (cond-> (not inline?) (dissoc :releases))
      (merge {:name (:template-name spec)}
             (if inline? {:source (:source spec)} {:source-file (template-body-path spec)}))
      (with-owner spec)))

(defn template-unit
  [spec {:keys [inline?] :as opts}]
  (let [nm (:template-name spec)]
    {:kind     :doc-template
     :name     (name nm)
     :nodes    [[:templates (name nm)]]
     :doc      (template-decl spec opts)
     :siblings (if inline? []
                   [{:path (template-body-path spec) :content (or (:source spec) "")}])}))

(defn document-template-spec
  [doc src {:keys [template project repository]}]
  (cond-> {:template-name (keyword (name template))
           :title         (:name doc)
           :format        (keyword (name (or (:format doc) "markdown")))
           :engine        :jinja2
           :source        (or (:body doc) "")
           :parameters    (or (:parameters src) {})
           :targets       [:html :pdf]
           :status        :published
           :version       "0.1.0"}
    (:description src) (assoc :description (:description src))
    (seq (:page doc))  (assoc :page (:page doc))
    project            (assoc :project (keyword (name project)))
    repository         (assoc :repository (keyword (name repository)))))

(defn- pipeline-unit
  [spec members]
  (let [nm (:pipeline-name spec)]
    {:kind  :pipeline
     :name  (name nm)
     :nodes (into [[:pipelines (name nm)]] (map (fn [[t _]] [:transforms (name t)])) members)
     :doc   (cond-> {:name nm :kind (or (:kind spec) :batch)}
              (seq (:description spec)) (assoc :description (:description spec))
              (:status spec)            (assoc :status (:status spec))
              (:deprecation spec)       (assoc :deprecation (:deprecation spec))
              (:project spec)           (assoc :project (:project spec))
              (:repository spec)        (assoc :repository (:repository spec))
              (some? (:ignore-inherited spec)) (assoc :ignore-inherited (:ignore-inherited spec))
              (seq (:imports spec))     (assoc :imports (vec (:imports spec)))
              (seq (:outputs spec))     (assoc :outputs (vec (:outputs spec)))
              (seq members)             (assoc :transforms
                                               (into (sorted-map)
                                                     (map (fn [[t-nm entry]]
                                                            [t-nm (dissoc entry :project :repository
                                                                          :pipeline)]))
                                                     members)))}))

(defn- backing->external-source
  [spec]
  (let [nm         (some-> (:transform-name spec) name)
        out        (:output spec)
        datasource (get-in spec [:input :datasource])
        ext        (fn [] (cond-> {:strategy :simple :datasource datasource}
                            (:primary-key spec)      (assoc :primary-key (:primary-key spec))
                            (seq (:column-map spec))  (assoc :column-map (:column-map spec))
                            (get-in spec
                                    [:source :query]) (assoc :query (get-in spec [:source :query]))
                            (get-in spec
                                    [:source :table]) (assoc :table (get-in spec [:source :table]))
                            (get-in spec [:source :incremental])
                            (assoc :incremental (get-in spec [:source :incremental]))
                            (seq (:ops spec))         (assoc :transforms (vec (:ops spec)))))]
    (when datasource
      (cond
        (and (= :dataset (:kind out))
             (= (keyword nm) (backing/transform-name :dataset (:dataset out))))
        {:kind :dataset :entity (:dataset out) :ext (ext)}
        (and (= :object-type (:kind out))
             (= (keyword nm) (backing/transform-name :object-type (:object-type out))))
        {:kind :object-type :entity (:object-type out) :ext (ext)}))))

(defn- emit-dataset [spec ext]
  (cond-> (tshape/->author :datasets spec {})
    ext                                 (assoc :external-source ext)
    (= :materialization (:kind spec))   (dissoc :columns)))

(defn- emit-interface [spec vts sps]
  (tshape/->author :interfaces spec
                   (cond-> {:property-list (sugar/raise-attrs (:property-list spec) vts sps)}
                     (seq (:link-constraints spec))
                     (assoc :link-constraints (vec (:link-constraints spec))))))

(def ^:private source-author-fields
  (into [] (remove #{:secrets :secret-cipher :project :ignore-inherited})
        (mu/keys ent/SourceEntry)))

(defn- emit-source
  [spec]
  (-> (reduce (fn [acc k]
                (let [v (get spec k)]
                  (cond
                    (nil? v)                     acc
                    (and (coll? v) (empty? v))   acc
                    (and (sequential? v)
                         (#{:egress :allow-commands :markings :export-markings} k))
                    (assoc acc k (vec v))
                    :else                        (assoc acc k v))))
              {}
              source-author-fields)
      (with-owner spec)))

(defn- emit-action
  [idx spec]
  (let [->name (fn [x] (rid/rewrite-action-refs (fn [_targets v] (get idx v v)) x))]
    (cond-> (dissoc (sugar/raise-create-ids (->name spec)) :revision)
      (nil? (spec/function-rule spec)) (dissoc :edits)
      (some? (spec/function-rule spec)) (update :edits #(mapv ->name %)))))

(def ^:private prose-keys
  #{:description :rationale :title})

(defn- reflow-prose
  [s]
  (-> s (str/replace #"[ \t]*\n[ \t]*" " ") str/trim))

(defn reflow-prose-strings
  [m]
  (walk/postwalk
   (fn [x]
     (if (map? x)
       (reduce-kv (fn [acc k v]
                    (assoc acc k (if (and (contains? prose-keys k) (string? v))
                                   (reflow-prose v)
                                   v)))
                  {} x)
       x))
   m))

(defn- scoped-section
  [schema {:keys [idx proj-rid repo-rid all-projects?]} kinds-key]
  (->> (if (= :repo (:tree (ents/by-section kinds-key)))
         (cond
           repo-rid      (into {} (filter (fn [[_ s]] (= repo-rid (:repository s))))
                               (get schema kinds-key))
           all-projects? (get schema kinds-key)
           :else         (owned-by (get schema kinds-key) proj-rid))
         (cond-> (get schema kinds-key)
           (not all-projects?) (owned-by proj-rid)))
       (names-resolved idx kinds-key)))

(defn- container-section
  [schema {:keys [idx proj-rid]} kinds-key]
  (let [chain (when proj-rid (set (authority/container-chain schema proj-rid)))]
    (names-resolved idx kinds-key (cond-> (get schema kinds-key)
                                    chain (select-keys chain)))))

(def ^:private exported
  (filterv :export ents/entities))

(defn- selected
  [schema {:keys [idx proj-rid repo-rid] :as scope} section]
  (let [all (get schema section)]
    (case (get-in (ents/by-section section) [:export :scope])
      :project    (scoped-section schema scope section)
      :global     (names-resolved idx section all)
      :container  (container-section schema scope section)
      :imports    (names-resolved idx section
                                  (cond-> all proj-rid (select-keys (with-imported all proj-rid))))
      :repository (names-resolved idx section (cond repo-rid (select-keys all [repo-rid])
                                                    proj-rid (owned-by all proj-rid)
                                                    :else    all))
      :owned      (cond-> all proj-rid (owned-by proj-rid))
      :all        all
      :none       nil)))

(defn- export-sections
  [schema scope]
  (into {}
        (for [{:keys [section export]} exported
              :let [{:keys [kinds]} export
                    v (selected schema scope section)]
              :when (some? v)]
          [section (cond->> v kinds (into {} (filter (fn [[_ s]] (kinds (:kind s))))))])))

(defn- split-transforms
  [all-txs]
  (let [query-fn? (fn [[_ s]] (boolean (:function-name s)))
        lifted?   (fn [[_ s]]
                    (and (not (:repository s))
                         (= :dataset (get-in s [:output :kind]))
                         (= (keyword (:transform-name s))
                            (backing/transform-name :materialization
                                                    (get-in s [:output :dataset])))))
        build-txs (into {} (remove #(or (query-fn? %) (lifted? %))) all-txs)
        lowered   (into {} (keep (fn [[k s]]
                                   (when-let [r (backing->external-source s)] [k r])))
                        build-txs)
        ext-of    (fn [kind] (into {} (for [[_ r] lowered :when (= kind (:kind r))]
                                        [(:entity r) (:ext r)])))]
    {:query-fns  (into {} (filter query-fn?) all-txs)
     :txs        (apply dissoc build-txs (keys lowered))
     :ext-by-ds  (ext-of :dataset)
     :ext-by-obj (ext-of :object-type)}))

(defn- split-apps
  [apps]
  (let [view-app? (fn [[_ s]] (tshape/object-view? s))]
    {:view-apps (into {} (filter view-app?) apps)
     :nav-apps  (into {} (remove view-app?) apps)}))

(defn- collect
  [schema {:keys [project repository inline? all-projects?] :or {inline? false}}]
  (let [proj-rid   (project-rid schema project)
        scope      {:idx           (sch/rid-name-index schema)
                    :proj-rid      proj-rid
                    :repo-rid      (repository-rid schema repository)
                    :all-projects? all-projects?}
        sections   (export-sections schema scope)
        {:keys [txs query-fns] :as transforms}
        (split-transforms (scoped-section schema scope :transforms))
        pipe-names (into #{} (map (comp :pipeline-name val)) (:pipelines sections))
        in-pipe?   (fn [[_ s]] (pipe-names (:pipeline s)))
        pipe-txs   (into {} (filter in-pipe?) txs)
        emit       (fn [s] (emit-transform s {:inline? inline?}))
        index-by   (fn [section name-key]
                     (into {} (map (fn [[_ s]] [(name-key s) s])) (get sections section)))
        tx-emitted (keep (comp emit val) (remove in-pipe? txs))
        qf-emitted (keep (comp emit val) query-fns)]
    (merge scope transforms
           (split-apps (scoped-section schema scope :applications))
           {:schema       schema
            :inline?      inline?
            :sections     sections
            :vt-by-name   (index-by :value-types :value-type-name)
            :sp-by-name   (index-by :shared-properties :shared-property-name)
            :pipe-txs     pipe-txs
            :producers    (into {} (map (fn [[_ s]] [(:transform-name s) s])) txs)
            :tx-emitted   tx-emitted
            :pipe-emitted (into {} (keep (fn [[_ s]]
                                           (when-let [e (emit s)] [(:transform-name s) e])))
                                pipe-txs)
            :qf-emitted   qf-emitted
            :generated    {:transforms tx-emitted :query-functions qf-emitted}})))

(defn- link-tuple
  [{:keys [producers]} spec]
  (let [nm   (:link-type-name spec)
        from (:from-object-type spec)
        to   (:to-object-type spec)
        card (:cardinality spec)
        fk   (sugar/raise-fk-backing from to card
                                     (get producers (backing/link-producer-name nm)))
        opts (cond-> (dissoc (tshape/->author :link-types spec {}) :from :to :cardinality)
               (some? fk) (assoc :fk fk))]
    (cond-> [nm from to card] (seq opts) (conj opts))))

(def ^:private entry-of
  {:object-types      (fn [{:keys [ext-by-obj vt-by-name sp-by-name]} s]
                        (emit-object s (get ext-by-obj (:object-type-name s))
                                     vt-by-name sp-by-name))
   :interfaces        (fn [{:keys [vt-by-name sp-by-name]} s]
                        (emit-interface s vt-by-name sp-by-name))
   :datasets          (fn [{:keys [ext-by-ds]} s]
                        (emit-dataset s (get ext-by-ds (:dataset-name s))))
   :shared-properties (fn [_ s]
                        (sugar/raise-display-name (tshape/->author :shared-properties s {})))
   :sources           (fn [_ s] (emit-source s))
   :functions         (fn [{:keys [inline?]} s]
                        (cond-> (tshape/->author :functions s {}) (not inline?) (dissoc :releases)))
   :link-types        link-tuple
   :actions           (fn [{:keys [idx]} s] (dissoc (emit-action idx s) :id :action-name))
   :eval-suites       (fn [{:keys [idx]} s]
                        (let [p (some-> (:project s) (as-> r (get idx r r)) rid/->name keyword)]
                          (cond-> (tshape/authored-entry :eval-suites s) p (assoc :project p))))})

(defn- entry-key
  [section k spec]
  (if (= :keyword (:key (ents/by-section section)))
    k
    (if-some [n ((ents/name-field section) spec)]
      (if (contains? ents/normalized-name-sections section) (keyword n) n)
      k)))

(defn- emit-section
  [c {:keys [section export]}]
  (let [entry   (get entry-of section (fn [_ s] (tshape/authored-entry section s)))
        entries (get-in c [:sections section])]
    (if (= :tuples (:shape export))
      (some->> (not-empty entries) (map (fn [[_ s]] (entry c s))) (sort-by first) vec)
      (not-empty
       (into (into (sorted-map) (map (fn [[k s]] [(entry-key section k s) (entry c s)])) entries)
             (map :entry)
             (get-in c [:generated (:generated export)]))))))

(def ^:private text-sections
  (filterv #(not= :units (get-in % [:export :into])) exported))

(defn- pipeline-units
  [{:keys [sections pipe-txs pipe-emitted]}]
  (let [by-pipe (group-by (fn [[_ s]] (:pipeline s)) pipe-txs)]
    (for [[_ p] (:pipelines sections)]
      (pipeline-unit p (for [[_ s] (sort-by key (get by-pipe (:pipeline-name p)))
                             :let  [e (get pipe-emitted (:transform-name s))]
                             :when e]
                         (:entry e))))))

(defn- view-units
  [{:keys [view-apps]}]
  (for [[ot views] (group-by (fn [[_ s]] (some-> (:object-type s) name keyword)) view-apps)
        :when ot]
    {:kind  :view
     :name  (name ot)
     :nodes (mapv (fn [[k _]] [:applications (rid/->name k)]) views)
     :doc   (emit-view-file ot views)}))

(defn- export-units
  [{:keys [sections nav-apps inline?] :as c}]
  (vec (concat (pipeline-units c)
               (map #(template-unit (val %) {:inline? inline?}) (:templates sections))
               (map #(app-unit (val %) {:inline? inline?}) nav-apps)
               (view-units c))))

(defn- export-files
  [{:keys [tx-emitted qf-emitted pipe-emitted]}]
  (let [emitted (concat tx-emitted qf-emitted (vals pipe-emitted))
        own     (keep :file emitted)
        taken   (set (map :path own))]
    (->> (concat own (->> (mapcat :modules emitted)
                          (remove (comp taken :path))
                          (reduce (fn [m f] (assoc m (:path f) f)) {})
                          vals))
         (sort-by :path)
         vec)))

(defn- live-object-keys
  [schema]
  (into {} (for [[rid o] (:object-types schema)
                 :let [n (:object-type-name o)
                       pk (when n (world-rules/object-primary-key schema rid))]
                 :when n]
             [(keyword (name n))
              (cond-> {:attributes (mapv (juxt :attribute-name :attribute-type)
                                         (:attribute-list o))
                       :title      (:title o)
                       :match-keys (vec (:match-keys o))}
                (and pk (not (sequential? pk))) (assoc :primary-key (keyword (name pk)))
                (and (sequential? pk) (= 1 (count pk)))
                (assoc :primary-key (keyword (name (first pk)))))])))

(defn export
  [schema {:keys [project all-projects?] :as opts}]
  (when (and all-projects? project)
    (refusal/refuse! :refusal.parser/conflicting-export-scope
                     {:kind ::conflicting-scope :project (name project)}))
  (let [{:keys [sections txs tx-emitted] :as c} (collect schema opts)
        body (into {} (keep (fn [{:keys [section] :as e}]
                              (some->> (emit-section c e) (vector (ents/section->author section)))))
                   text-sections)]
    {:ent     {:schema (reflow-prose-strings (sugar/raise-actions body (live-object-keys schema)))}
     :files   (export-files c)
     :units   (export-units c)
     :skipped {:sources     0
               :credentials (count (filter (comp :secret-cipher val) (:sources sections)))
               :transforms  (- (count txs) (count tx-emitted))}}))

(def ^:private text-section-of
  (into {} (map (juxt :section :export)) text-sections))

(defn- transform-role
  [s]
  (cond
    (:function-name s) [:query]
    (and (not (:repository s))
         (= :dataset (get-in s [:output :kind]))
         (= (keyword (:transform-name s))
            (backing/transform-name :materialization (get-in s [:output :dataset]))))
    [:lifted]
    :else (if-let [r (backing->external-source s)] [:lowered r] [:build])))

(defn- resolved
  [idx section k spec]
  (case (get-in (ents/by-section section) [:export :scope])
    (:project :global :container :imports :repository)
    (get (names-resolved idx section {k spec}) k)
    (:owned :all) spec
    nil))

(defn- transform-export [c k spec]
  (let [s        (get (names-resolved (:idx c) :transforms {k spec}) k)
        [role r] (transform-role s)
        gen      (when (or (= :query role)
                           (and (= :build role) (not ((:pipeline? c) (:pipeline s)))))
                   (:entry (emit-transform s {:inline? (:inline? c)})))
        asec     (ents/section->author (if (= :query role) :functions :transforms))]
    (cond-> {:facts (cond-> #{}
                      (= :lowered role) (conj [:ext (:kind r) (:entity r)])
                      (= :build role)   (conj [:producer (:transform-name s)]))
             :nodes (if gen
                      {[asec (rid/->name (first gen))]
                       {:key (first gen) :value (second gen) :vector? false :rank 1}}
                      {})}
      (and (= :build role) ((:pipeline? c) (:pipeline s)))
      (-> (update :facts conj [:in-pipeline (:pipeline s)])
          (assoc :pipe (some-> (emit-transform s {:inline? (:inline? c)}) :entry))))))

(defn- application-export [c k spec]
  (let [s (get (names-resolved (:idx c) :applications {k spec}) k)]
    (if (tshape/object-view? s)
      {:facts (if-some [ot (some-> (:object-type s) name keyword)] #{[:view-of ot]} #{})
       :nodes {} :view-app s}
      {:facts #{} :nodes {} :units [(app-unit s {:inline? (:inline? c)})]})))

(defn- section-export
  [c section k spec]
  (let [export (get text-section-of section)
        s      (when export (resolved (:idx c) section k spec))]
    (if (and s (or (nil? (:kinds export)) ((:kinds export) (:kind s))))
      (let [entry ((get entry-of section (fn [_ s] (tshape/authored-entry section s))) c s)
            asec  (ents/section->author section)
            tuple (= :tuples (:shape export))
            ek    (if tuple (first entry) (entry-key section k s))]
        {:facts #{} :nodes {[asec (rid/->name ek)]
                            {:key ek :value entry :vector? tuple :rank 0}}})
      {:facts #{} :nodes {}})))

(def ^:private exports-of
  {:transforms   transform-export
   :pipelines    (fn [c k spec]
                   (let [s (resolved (:idx c) :pipelines k spec)]
                     {:facts #{[:pipeline (:pipeline-name s)]} :nodes {} :pipeline s}))
   :templates    (fn [c k spec]
                   {:facts #{} :nodes {}
                    :units [(template-unit (resolved (:idx c) :templates k spec)
                                           {:inline? (:inline? c)})]})
   :applications application-export})

(defn node-export
  [c section k spec]
  (if-let [f (exports-of section)]
    (f c k spec)
    (section-export c section k spec)))

(defn body-of
  [index live]
  (reflow-prose-strings
   (dissoc (sugar/raise-actions (assoc (body-tree/body index) :sutra.parser.sugar/live live))
           :sutra.parser.sugar/live)))

(defn external-source
  [s]
  (:ext (backing->external-source s)))

(defn live-lookup
  [f]
  (reify
    clojure.lang.ILookup
    (valAt [_ k] (f k))
    (valAt [_ k nf] (if-some [v (f k)] v nf))
    clojure.lang.Associative
    (containsKey [_ k] (some? (f k)))
    (entryAt [_ k] (when-some [v (f k)] (clojure.lang.MapEntry/create k v)))))

(defn view-unit
  [ot views]
  {:kind  :view
   :name  (name ot)
   :nodes (mapv (fn [[k _]] [:applications (rid/->name k)]) views)
   :doc   (emit-view-file ot views)})

(defn pipeline-unit*
  [spec members]
  (pipeline-unit spec members))
