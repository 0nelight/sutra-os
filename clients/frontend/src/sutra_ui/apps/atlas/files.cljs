(ns sutra-ui.apps.atlas.files
  (:require [clojure.string :as str]
            [sutra.i18n.core :as i18n]
            [sutra.types.rid :as rid]
            [sutra-ui.apps.atlas.model :as model]
            [sutra-ui.resource.core :as res]
            [sutra-ui.state.atlas :as ks]
            [sutra-ui.state.catalog :as catalog]
            [sutra.kit.remote :as rd]))

(defn project-label [pkey]
  (if (and pkey (not= pkey ks/default-project))
    (name pkey)
    (i18n/t :ui/kompass-default-scope)))

(defn- portfolio-of
  []
  (into {} (for [p (:portfolios @ks/kompass-index) r (:projects p)] [(str r) p])))

(defn- folder? [n] (= :folder (keyword (:node-kind n))))

(defn- placed-in
  []
  (into {} (for [n (ks/kompass-node-list) :when (:ref n)] [(str (:ref n)) (:parent-rid n)])))

(defn- folder-names
  [idx parent-rid]
  (loop [rid parent-rid acc ()]
    (if-let [f (get idx rid)]
      (recur (:parent-rid f) (cons (:name f) acc))
      (vec acc))))

(defn- file-project [rid-or-name]
  (some-> rid-or-name catalog/project-of name keyword))

(defn- schema-key [family spec] (str (name family) "/" (:id spec)))

(defn- base-row
  [family nm {:keys [nm-name project parent modified placed-ref]}]
  (let [handle [family nm]]
    {:key       (str (name family) "/" nm)
     :family    family :nm nm :handle handle
     :name      (or nm-name (res/display-name family nm))
     :project   project :parent parent :modified modified :placed-ref placed-ref
     :tags      (model/resource-tags handle)
     :promoted? (model/promoted? handle)
     :pinned?   (model/pinned? handle)
     :curated?  (model/curated? handle)
     :trashed?  (model/trashed? handle)}))

(defn- schema-rows
  [placed]
  (let [{modified :times floor :floor-at} (:modified @ks/kompass-index)]
    (for [{:keys [family]} (concat res/schema-families res/file-sections)
          [nm spec] (get @catalog/canonical-schema family)
          :when (not (and (= :datasets family)
                          (contains? (:object-types @catalog/canonical-schema) nm)))]
      (base-row family nm
                {:project    (file-project (:project spec))
                 :parent     (get placed (str (:id spec)))
                 :placed-ref (str (:id spec))
                 :modified   (get modified (schema-key family spec))
                 :older-than (when-not (get modified (schema-key family spec)) floor)}))))

(defn- document-rows
  [placed]
  (for [d (ks/rows :documents)
        :let [id (str (:id d)) ref (str "document/" id)]]
    (base-row :documents id {:project (file-project (:project d)) :parent (get placed ref)
                             :placed-ref ref :modified (:updated-at d) :nm-name (:name d)})))

(defn- exploration-rows
  [placed]
  (for [x (ks/rows :explorations)
        :let [id (str (:rid x)) ref (str "exploration/" id)]]
    (assoc (base-row :explorations id {:project (file-project (:project x))
                                       :parent (get placed ref) :placed-ref ref
                                       :modified (:updated-at x) :nm-name (:name x)})
           :private? (:private x) :mine? (:mine x))))

(defn- widget-set-rows
  []
  (for [w (ks/rows :widget-sets)]
    (base-row :widget-sets (:name w) {:project (file-project (:project w))
                                      :modified (:updated-at w) :nm-name (:name w)})))

(defn- graph-rows
  [placed]
  (for [[id v] @catalog/views
        :let [ref (rid/resource-ref :graphs id)]]
    (base-row :graphs id {:project (file-project (:project v)) :parent (get placed ref)
                          :placed-ref ref :modified (:updated-at v) :nm-name (:name v)})))

(defn- folder-rows
  []
  (for [n (ks/kompass-node-list) :when (folder? n)]
    (base-row :folders (:rid n) {:project (file-project (:project n)) :parent (:parent-rid n)
                                 :modified (:updated-at n) :nm-name (:name n)})))

(defn- uploaded-rows
  [placed]
  (for [{:keys [id] :as f} (ks/rows :files)
        :let [ref (str "file/" id)]]
    (base-row :files (str id) {:project (file-project (:project f)) :parent (get placed ref)
                               :placed-ref ref :modified (:updated-at f) :nm-name (:name f)})))

(defn- with-path
  [idx row]
  (assoc row :path
         (cond-> []
           (:private? row) (conj (i18n/t :ui/kompass-your-files))
           (and (:project row) (not (:private? row))) (conj (name (:project row)))
           true (into (folder-names idx (:parent row)))
           true (conj (:name row)))))

(defn- all-file-rows []
  (let [placed (placed-in)
        idx    (into {} (map (juxt :rid identity)) (ks/kompass-node-list))]
    (map #(with-path idx %)
         (concat (folder-rows) (document-rows placed) (exploration-rows placed)
                 (uploaded-rows placed)
                 (widget-set-rows) (graph-rows placed) (schema-rows placed)))))

(defn files-empty-text
  []
  (rd/fold (rd/all [@(ks/lists :documents) @(ks/lists :explorations) @(ks/lists :widget-sets)
                    @(ks/lists :files) @ks/kompass-nodes @catalog/rid->name])
           {:loading (fn [] (i18n/t :ui/kompass-files-loading))
            :failed  (fn [r] (i18n/t :ui/kompass-files-failed {:reason r}))
            :absent  (fn [] (i18n/t :ui/kompass-files-empty))
            :value   (fn [_] (i18n/t :ui/kompass-files-empty))}))

(defn- latest [times] (some->> times (remove nil?) (map str) sort last))

(defn project-rows
  [file-rows]
  (let [rights    (:rights @ks/kompass-index)
        by-folio  (portfolio-of)
        by-owner  (group-by :project file-rows)]
    (for [[pkey spec] (catalog/project-list)]
      (-> (base-row :projects pkey {:project pkey :nm-name (name pkey)
                                    :modified (latest (cons (get (get-in @ks/kompass-index
                                                                         [:modified :times])
                                                                 (str "projects/" (:id spec)))
                                                            (map :modified (get by-owner pkey))))})
          (assoc :path [(name pkey)] :description (:description spec) :spec spec
                 :may-read? (get-in rights [(name pkey) :read?] true)
                 :portfolio (get by-folio (str (:id spec))))))))

(defn live-rows
  []
  (let [rows    (all-file-rows)
        by-prid (portfolio-of)
        folio   (fn [pk] (get by-prid (str (get-in (catalog/project-list) [pk :id]))))
        files   (for [r rows :when (not (:trashed? r))] (assoc r :portfolio (folio (:project r))))]
    (concat (project-rows files) files)))

(defn project-files
  [pkey folder]
  (let [scope (when (not= pkey ks/default-project) pkey)]
    (for [r (all-file-rows)
          :when (and (not (:trashed? r)) (= scope (:project r)) (not (:private? r))
                     (= folder (:parent r)))]
      r)))

(defn project-pinned [pkey]
  (filter #(and (:pinned? %) (= pkey (:project %))) (all-file-rows)))

(defn project-trash [pkey]
  (let [scope (when (not= pkey ks/default-project) pkey)]
    (filter #(and (:trashed? %) (= scope (:project %))) (all-file-rows))))

(defn project-curated [pkey]
  (filter #(and (:curated? %) (not (:trashed? %)) (= pkey (:project %))) (all-file-rows)))

(defn project-files-anywhere
  [pkey]
  (filter #(and (not (:trashed? %)) (= pkey (:project %))) (all-file-rows)))

(defn project-count [pkey]
  (count (filter #(and (not (:trashed? %)) (= pkey (:project %))) (all-file-rows))))

(defn folder-chain
  [rid]
  (let [idx (into {} (map (juxt :rid identity)) (ks/kompass-node-list))]
    (loop [r rid acc ()]
      (if-let [f (get idx r)]
        (recur (:parent-rid f) (cons f acc))
        (vec acc)))))

(defn folders-of
  [pkey]
  (let [nodes (filter #(and (folder? %) (= pkey (file-project (:project %))))
                      (ks/kompass-node-list))
        idx   (into {} (map (juxt :rid identity)) nodes)]
    (sort-by second
             (for [n nodes]
               [(:rid n) (str/join " / " (conj (folder-names idx (:parent-rid n)) (:name n)))]))))

(defn fav-ref
  [{:keys [family nm spec]}]
  (case family
    :projects     (str "project/" (:id spec))
    :documents    (str "document/" nm)
    :explorations (str "exploration/" nm)
    :folders      (str "folder/" nm)
    :widget-sets  (str "widget-set/" nm)
    :files        (str "file/" nm)
    (str (name family) "/" nm)))

(defn shareable? [{:keys [family]}] (contains? #{:documents :explorations :files} family))

(defn- ot->project [sch]
  (into {} (for [[nm spec] (:object-types sch)] [nm (:project spec)])))

(defn references-for
  [pkey]
  (let [sch   @catalog/canonical-schema
        owner (ot->project sch)
        ext?  (fn [o] (and o (contains? owner o) (not= (owner o) pkey)))
        owns? (fn [spec] (= pkey (:project spec)))]
    (distinct
     (concat
      (for [[nm spec] (:link-types sch) :when (owns? spec)
            t [(:from-object-type spec) (:to-object-type spec)] :when (ext? t)]
        {:via nm :family :link-types :target t :owner (owner t)})
      (for [[nm spec] (:functions sch) :when (owns? spec)
            :let [t (:target-type spec)] :when (ext? t)]
        {:via nm :family :functions :target t :owner (owner t)})
      (for [[nm spec] (:transforms sch) :when (owns? spec)
            t (concat (res/transform-writes spec) (res/transform-reads spec)) :when (ext? t)]
        {:via nm :family :transforms :target t :owner (owner t)})))))

(defn readers-of
  [pkey]
  (distinct
   (for [other (keys (catalog/project-list))
         :when (and (not= other pkey)
                    (some #(= pkey (:owner %)) (references-for other)))]
     other)))

