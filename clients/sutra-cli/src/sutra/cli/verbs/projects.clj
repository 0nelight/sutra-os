(ns sutra.cli.verbs.projects
  (:require [sutra.cli.dispatch :refer [step!]]
            [sutra.cli.errors :refer [req!]]
            [sutra.cli.live :refer [commit-unless-staged! find-or-create-folder!
                                    kompass-folder-named kompass-nodes!]]
            [sutra.cli.out :refer [emit! mark say! warn!]]
            [sutra.cli.local :refer [vom-aufrufer]]
            [clojure.edn :as edn]
            [sutra.i18n.core :as i18n]
            [clojure.string :as str]
            [sutra.types.rid :as rid]
            [sutra.wire.api :as api]))

(defn- board-col->attr          [& a] (apply @(requiring-resolve 'parser.column/col->attr) a))

(defmethod step! :new-project [ctx [_ {:keys [name description stage?]}]]
  (req! "create project" (api/mutate :create-project
                                     (cond-> {:name name}
                                       description (assoc :description description))
                                     {:stage? true}))
  (emit! ctx {:staged-project name :description description})
  (commit-unless-staged! ctx stage?
                         (str "committed — `projects` to list, then `rename <resource> …` "
                              "or author resources with `:project " (clojure.core/name name) "`"))
  nil)

(defmethod step! :new-repository [ctx [_ {:keys [name project type description stage?]}]]
  (req! "create repository"
        (api/mutate :create-repository
                    (cond-> {:name name}
                      project     (assoc :project (keyword project))
                      type        (assoc :type (keyword type))
                      description (assoc :description description))
                    {:stage? true}))
  (emit! ctx {:staged-repository name :project project :type type :description description})
  (commit-unless-staged! ctx stage? (str "committed — `repositories` to list, then author "
                                         "transforms/functions with `:repository "
                                         (clojure.core/name name) "`"))
  nil)

(defmethod step! :new-application [ctx [_ {:keys [name project object-type description stage?]}]]
  (req! "create app" (api/mutate :create-application
                                 (cond-> {:name name}
                                   project     (assoc :project project)
                                   object-type (assoc :object-type object-type)
                                   description (assoc :description description))
                                 {:stage? true}))
  (emit! ctx {:staged-application name :project project :object-type object-type})
  (commit-unless-staged! ctx stage? (str "committed — `app show " (clojure.core/name name)
                                         "` to inspect, `app edit " (clojure.core/name name)
                                         "` to author its view"))
  nil)

(defn- folder-rid!
  [nodes fname]
  (or (:rid (kompass-folder-named nodes fname))
      (throw (ex-info (str "no folder named " fname
                           " — `project folder create " fname "` first")
                      {:folder fname}))))

(defn- resource-ref!
  [idx nm]
  (let [k (if (keyword? nm) nm (keyword nm))]
    (or (rid/resolve-name->rid idx rid/rid-kinds k)
        (throw (ex-info (str "no resource named " (clojure.core/name k)
                             " — check `describe` / `projects`")
                        {:resource nm})))))

(defmethod step! :project-files [ctx [_ _]]
  (let [nodes  (kompass-nodes!)
        annots (:annotations (req! "annotations" (api/get! :kompass-annotations)))]
    (emit! ctx {:nodes (vec nodes) :annotations annots}))
  nil)

(defmethod step! :place [ctx [_ {:keys [resources into as]}]]
  (let [idx     (rid/name->rid-index (req! "schema" (api/get! :schema)))
        parent  (when into (find-or-create-folder! (kompass-nodes!) (clojure.core/name into)))
        single? (= 1 (count resources))]
    (when (and as (not single?))
      (say! ctx (mark :yellow "!" (i18n/t :cli/as-single-placement-only))))
    (doseq [nm resources
            :let [ref (resource-ref! idx nm)]]
      (req! (str "place " nm)
            (api/post! :kompass-place (cond-> {:ref ref :parent-rid parent}
                                        (and as single?) (assoc :name (clojure.core/name as))))))
    (emit! ctx {:placed (vec resources) :into into})
    nil))

(defmethod step! :unplace [ctx [_ {:keys [resources]}]]
  (let [idx     (rid/name->rid-index (req! "schema" (api/get! :schema)))
        by-ref  (into {} (comp (filter #(= :placement (:node-kind %)))
                               (map (juxt :ref identity)))
                      (kompass-nodes!))
        removed (doall
                 (for [nm resources
                       :let [node (get by-ref (resource-ref! idx nm))]]
                   (if node
                     (do (req! (str "unplace " nm)
                               (api/delete! :kompass-node-of {:rid (:rid node)}))
                         nm)
                     (say! ctx (mark :yellow "!" (i18n/t :cli/not-filed-in-kompass {:name nm}))))))]
    (emit! ctx {:unplaced (vec (remove nil? removed))})
    nil))

(def ^:private repo-member-kinds
  {:transform   :update-transform
   :function    :update-function
   :application :update-application})

(defn- repo-membership!
  [label repository resources]
  (let [idx      (rid/name->rid-index (req! "schema" (api/get! :schema)))
        member-kinds (into #{} (map rid/tag->kinds-key) (keys repo-member-kinds))
        resolve1 (fn [nm]
                   (let [k (keyword nm)]
                     (or (rid/resolve-name->rid idx member-kinds k)
                         (resource-ref! idx nm))))
        touched  (doall
                  (for [nm resources
                        :let [ref  (resolve1 nm)
                              kind (rid/rid-kind ref)
                              verb (get repo-member-kinds kind)]]
                    (if verb
                      (do (req! (str label " " nm)
                                (api/mutate verb {:name (keyword nm) :repository repository}
                                            {:stage? true}))
                          [nm kind])
                      (throw (ex-info
                              (str nm " is a " (clojure.core/name (or kind :resource))
                                   " — a code repository holds "
                                   (str/join "/"
                                             (sort (map clojure.core/name
                                                        (keys repo-member-kinds))))
                                   ". Ontology types are imported, not committed.")
                              {:resource nm :kind kind})))))]
    (req! "commit" (api/post! :staging))
    touched))

(defmethod step! :repo-place [ctx [_ {:keys [repository resources]}]]
  (emit! ctx {:repository repository
              :placed (mapv (fn [[nm kind]] {:resource nm :kind kind})
                            (repo-membership! "place" repository resources))})
  nil)

(defmethod step! :repo-unplace [ctx [_ {:keys [resources]}]]
  (emit! ctx {:unplaced (mapv (fn [[nm kind]] {:resource nm :kind kind})
                              (repo-membership! "unplace" :none resources))})
  nil)

(defmethod step! :folder [ctx [_ {:keys [op name to into]}]]
  (case op
    :create
    (let [parent (when into (find-or-create-folder! (kompass-nodes!) (clojure.core/name into)))]
      (req! (str "folder " name) (api/post! :kompass-folders {:name name :parent-rid parent})))
    :rename
    (do (when-not to (throw (ex-info (i18n/t :cli/usage-folder-rename) {})))
        (let [rid (folder-rid! (kompass-nodes!) name)]
          (req! "rename folder" (api/post! :kompass-node-rename {:rid rid} {:to to}))))
    :move
    (let [nodes  (kompass-nodes!)
          rid    (folder-rid! nodes name)
          parent (when into (find-or-create-folder! nodes (clojure.core/name into)))]
      (req! "move folder" (api/post! :kompass-node-move {:rid rid} {:parent-rid parent})))
    :delete
    (let [rid (folder-rid! (kompass-nodes!) name)]
      (req! "delete folder" (api/delete! :kompass-node-of {:rid rid})))
    (throw (ex-info (i18n/t :cli/folder-unknown-op {:op (pr-str op)}) {:op op})))
  (emit! ctx (cond-> {:op op :folder name} to (assoc :to to) into (assoc :into into)))
  nil)

(defn- col->attr [c] (board-col->attr c))

(defn- humanize [kw]
  (str/join " " (map str/capitalize (str/split (clojure.core/name kw) #"[-_]"))))

(defn- sync-object-map
  [{:keys [datasource as cols pk-attr table sql]}]
  (let [attrs (mapv (fn [{col :name :keys [type ontology-type]}]
                      [(col->attr col) (or ontology-type :string)
                       (cond-> {} type (assoc :description (str type)))])
                    cols)
        query (or sql (str "SELECT " (str/join ", " (map :name cols)) " FROM " table))]
    {:objects
     {as {:name       (humanize as)
          :plural     (humanize as)
          :title      pk-attr
          :attributes attrs
          :external-source {:strategy    :simple
                            :datasource  datasource
                            :primary-key pk-attr
                            :on-null     :skip
                            :query       query}}}}))

(defmethod step! :sync [ctx [_ {:keys [name as table sql] pk-col :key}]]
  (when-not as
    (throw (ex-info "sync needs --as <object-type>" {})))
  (when (= (boolean table) (boolean sql))
    (throw (ex-info "sync needs exactly one of <table> or --sql \"SELECT …\"" {})))
  (let [cols (if table
               (:columns (req! "columns" (api/get! :datasource-columns {:name name :table table})))
               (let [row (first (:rows (req! "sample"
                                             (api/post! :datasource-query {:name name}
                                                        {:sql sql :limit 1}))))]
                 (when-not row
                   (throw (ex-info "the --sql query returned no rows to infer columns from"
                                   {:sql sql})))
                 (mapv (fn [k] {:name (clojure.core/name k) :ontology-type :string}) (keys row))))]
    (when (empty? cols)
      (throw (ex-info (str "no columns found for " (pr-str (or table sql))) {})))
    (let [pk    (if pk-col (col->attr pk-col) (col->attr (:name (first cols))))
          smap  (sync-object-map {:datasource (keyword name) :as as :cols cols
                                  :pk-attr pk :table table :sql sql})
          tally (req! "sync" (api/post! :install {:schema smap}))]
      (emit! ctx (assoc tally :object-type as :source (str name (when table (str "." table)))
                        :pk pk :attributes (count cols)))
      (say! ctx (i18n/t :cli/next-review-commit-import {:typ (clojure.core/name as)}))
      nil)))

(defmethod step! :projects [ctx [_ _]]
  (let [ps   (:projects (req! "projects" (api/get! :projects)))
        rows (sort-by :name
                      (for [p ps]
                        {:name (let [n (or (:project-name p) (:name p))]
                                 (if (keyword? n) (clojure.core/name n) (str n)))
                         :description (:description p)}))]
    (emit! ctx {:projects (vec rows)}))
  nil)

(defmethod step! :repositories [ctx [_ _]]
  (let [rs   (:repositories (req! "repositories" (api/get! :repositories)))
        rows (sort-by :name
                      (for [r rs]
                        (let [members (:members r)
                              n       (reduce + 0 (map (comp count val) members))]
                          {:name (let [nm (or (:repository-name r) (:name r))]
                                   (if (keyword? nm) (clojure.core/name nm) (str nm)))
                           :project (some-> (:project r) name)
                           :type    (some-> (:type r) name)
                           :members n
                           :description (:description r)})))]
    (emit! ctx {:repositories (vec rows)}))
  nil)

(defmethod step! :project-map [ctx [_ {:keys [project]}]]
  (let [r (api/get! :project-map {:project project})]
    (emit! ctx (if (= 400 (:status r))
                 (merge (into {} (remove (comp qualified-keyword? key)) (:data (:body r)))
                        (select-keys (:body r) [:error]))
                 (req! "project map" r))))
  nil)

(defmethod step! :source-adopt
  [ctx [_ {:keys [source table into dataset as key project preview? land? stage?]}]]
  (let [body (cond-> {:table table :preview? (boolean preview?)}
               into    (assoc :into into)
               dataset (assoc :dataset dataset)
               as      (assoc :as as)
               project (assoc :project project)
               key     (assoc :key (vec (remove str/blank? (str/split key #","))))
               land?   (assoc :land? true)
               stage?  (assoc :land? false))
        r    (api/post! :datasource-adopt {:name source} body)]
    (if (= 400 (:status r))
      (do (warn! (mark :red "✗" (get-in r [:body :error]))) nil)
      (let [b (req! (str "source adopt " source " " table) r)]
        (emit! ctx (select-keys b [:dataset :key :type :landed :land-refused :created :updated
                                   :skipped :preview?]))
        (when-not (or (:landed b) (:preview? b))
          (say! ctx "    staged — commit it to land it: sutra commit"))
        nil))))

(defn- package-body [{:keys [package preview? land? stage? mine? theirs?]}]
  (cond-> {:package package :preview? (boolean preview?)}
    land?   (assoc :land? true)
    stage?  (assoc :land? false)
    mine?   (assoc :resolve :mine)
    theirs? (assoc :resolve :theirs)))

(defn- package-answer [ctx what r]
  (if (= 409 (:status r))
    (do (emit! ctx (merge (select-keys (:body r) [:error]) (:data (:body r)))) nil)
    (let [b (req! what r)]
      (emit! ctx (select-keys b [:package :version :from :reinstall? :unchanged :landed
                                 :land-refused :created :updated :deleted :skipped :preview?]))
      (when-not (or (:landed b) (:preview? b) (:unchanged b))
        (say! ctx "    staged — commit it to land it: sutra commit"))
      nil)))

(defmethod step! :package-install [ctx [_ args]]
  (package-answer ctx (str "package install " (:package args))
                  (api/post! :package-install {} (package-body args))))

(defmethod step! :package-upgrade [ctx [_ args]]
  (package-answer ctx (str "package upgrade " (:package args))
                  (api/post! :package-upgrade {} (package-body args))))

(defn- strings
  [v]
  (into [] (comp (mapcat #(str/split (str %) #",")) (map str/trim) (remove str/blank?))
        (if (sequential? v) v [v])))

(defn- requires-map
  [vs]
  (into {} (for [v (strings vs) :let [[p r] (str/split v #"@" 2)]] [p (or r "*")])))

(defn- bindings-map [vs]
  (into {} (for [v (strings vs) :let [[k l] (str/split v #"=" 2)] :when l] [k l])))

(defn- window
  [{:keys [window-days window-from window-to window-zone]}]
  (when (or window-from window-to)
    (cond-> {:from window-from :to window-to}
      window-days (assoc :days (strings window-days))
      window-zone (assoc :zone window-zone))))

(defn- landing [{:keys [land? stage?]}]
  (cond land? {:land? true} stage? {:land? false} :else {}))

(defn- product-answer [ctx what r keep-keys]
  (if (= 409 (:status r))
    (do (emit! ctx (merge (select-keys (:body r) [:error]) (:data (:body r)))) nil)
    (let [b (req! what r)]
      (emit! ctx (select-keys b keep-keys))
      (when-not (or (:landed b) (:preview? b) (:unchanged b) (:published b) (:imported b))
        (say! ctx (str "    staged, not landed — an installation is recorded when it lands;"
                       " run it again with --land")))
      nil)))

(defmethod step! :product-publish [ctx [_ {:keys [product version projects channel
                                                  description requires release?]}]]
  (product-answer ctx (str "product publish " product " " version)
                  (api/post! :products {}
                             (cond-> {:product product :version version
                                      :projects (strings projects)
                                      :release? (boolean release?)}
                               channel     (assoc :channel channel)
                               description (assoc :description description)
                               requires    (assoc :requires (requires-map requires))))
                  [:published]))

(defmethod step! :product-install [ctx [_ {:keys [product version channel suffix prefix bind
                                                  lock? auto-upgrade? with-linked? preview?]
                                           :as args}]]
  (product-answer ctx (str "product install " product)
                  (api/post! :product-install {:product product}
                             (cond-> (merge {:preview? (boolean preview?)
                                             :lock? (boolean lock?)
                                             :auto-upgrade? (boolean auto-upgrade?)
                                             :with-linked? (boolean with-linked?)}
                                            (landing args))
                               version (assoc :version version)
                               channel (assoc :channel channel)
                               suffix  (assoc :suffix suffix)
                               prefix  (assoc :prefix prefix)
                               bind    (assoc :bindings (bindings-map bind))
                               (window args) (assoc :window (window args))))
                  [:product :version :install-id :state :projects :linked :landed :land-refused
                   :created :updated :deleted :preview?]))

(defmethod step! :product-upgrade [ctx [_ {:keys [installation version preview? mine? theirs?]
                                           :as args}]]
  (product-answer ctx (str "product upgrade " installation)
                  (api/post! :product-installation-upgrade {:id installation}
                             (cond-> (merge {:preview? (boolean preview?)} (landing args))
                               version (assoc :version version)
                               mine?   (assoc :resolve :mine)
                               theirs? (assoc :resolve :theirs)))
                  [:install-id :from :version :unchanged :landed :land-refused :created
                   :updated :deleted :preview?]))

(defmethod step! :product-uninstall [ctx [_ {:keys [installation preview?] :as args}]]
  (product-answer ctx (str "product uninstall " installation)
                  (api/post! :product-installation-uninstall {:id installation}
                             (merge {:preview? (boolean preview?)} (landing args)))
                  [:install-id :product :state :projects-stay :landed :land-refused :created
                   :updated :deleted :preview?]))

(defmethod step! :product-configure [ctx [_ {:keys [installation channel auto-upgrade? manual?
                                                    no-window?]
                                             :as args}]]
  (let [r (req! (str "product configure " installation)
                (api/post! :product-installation {:id installation}
                           (cond-> {}
                             channel       (assoc :channel channel)
                             auto-upgrade? (assoc :auto-upgrade true)
                             manual?       (assoc :auto-upgrade false)
                             no-window?    (assoc :window nil)
                             (window args) (assoc :window (window args)))))]
    (emit! ctx (:installation r))
    nil))

(defmethod step! :product-export [ctx [_ {:keys [product version out]}]]
  (let [b    (:bundle (req! (str "product export " product " " version)
                            (api/get! :product-bundle {:product product :version version})))
        path (or out (str product "-" version ".edn"))
        f    (if (.isAbsolute (java.io.File. ^String path))
               path
               (str (or (System/getenv "SUTRA_CALLER_PWD") ".") "/" path))]
    (spit f (pr-str b))
    (emit! ctx {:exported f :product product :version version :digest (:digest b)
                :signed-by (get-in b [:signature :issuer])})
    nil))

(defmethod step! :product-import [ctx [_ {:keys [file channel require-signature?]}]]
  (let [bundle (edn/read-string (slurp (vom-aufrufer file)))]
    (product-answer ctx (str "product import " file)
                    (api/post! :product-imports {}
                               (cond-> {:bundle bundle
                                        :require-signature? (boolean require-signature?)}
                                 channel (assoc :channel channel)))
                    [:imported])))

