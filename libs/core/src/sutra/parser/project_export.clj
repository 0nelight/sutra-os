(ns sutra.parser.project-export
  (:require [sutra.kit.refusal :as refusal]
            [sutra.parser.project-emit :as emit]
            [sutra.types.entities :as ents]
            [sutra.types.refs :as refs]
            [clojure.set :as set]
            [clojure.string :as str]
            [sutra.parser.ent-graph :as graph]
            [sutra.parser.authoring :as authoring]
            [sutra.parser.authoring.registry :as registry]
            [sutra.parser.checkout :as co]
            [sutra.parser.sugar :as sugar]
            [sutra.types.rid :as rid]
            [sutra.types.schema :as sch]
            [sutra.types.tree :as tree]
            [sutra.types.world-rules :as world-rules]))

(set! *warn-on-reflection* true)

(defn restated-keys
  [formats path]
  (not-empty (get-in formats [(authoring/default-format formats path) :envelope])))

(defn- read-form
  [formats path ^bytes bytes]
  (-> (authoring/read-file formats path (String. bytes "UTF-8"))
      (select-keys [:schema])
      emit/reflow-prose-strings
      (update-in [:schema :links] #(some->> % (sort-by pr-str) vec))))

(defn- sort-links [form]
  (update-in form [:schema :links] #(some->> % (sort-by pr-str) vec)))

(defn- ohne-default-status
  [schema]
  (let [weg   #(if (= :experimental (:status %)) (dissoc % :status) %)
        eintr (fn [m] (if (map? m) (update-vals m #(if (map? %) (weg %) %)) m))]
    (-> (reduce (fn [s k] (cond-> s (map? (get s k)) (update k eintr)))
                schema
                [:objects :actions :interfaces :shared-properties :value-types
                 :transforms :functions :datasets :media-sets :exports :applications
                 :templates])
        (update :links #(some->> % (mapv (fn [l] (if (map? (get l 4))
                                                   (let [o (weg (nth l 4))]
                                                     (if (seq o) (assoc l 4 o) (subvec l 0 4)))
                                                   l))))))))

(defn- lowered-form
  [formats path ^bytes bytes]
  (let [f (read-form formats path bytes)]
    (sort-links (cond-> f
                  (map? (:schema f)) (update :schema
                                             (comp ohne-default-status sugar/lower-schema))))))

(defn same-authored-content?
  [formats path ^bytes a ^bytes b]
  (or (java.util.Arrays/equals a b)
      (and (authoring/file? formats path)
           (or (try (= (read-form formats path a) (read-form formats path b))
                    (catch RuntimeException _ false))
               (try (= (lowered-form formats path a) (lowered-form formats path b))
                    (catch RuntimeException _ false))))))

(def derived-root-files
  #{".gitmodules"})

(defn plane-of
  [formats path]
  (let [p (str path)]
    (if (or (contains? derived-root-files p)
            (authoring/file? formats p)
            (authoring/sibling? formats p))
      :entity
      :file)))

(defn unit-file?
  [formats path]
  (some? (authoring/unit-kind formats path)))

(defn requirements-str
  ^String [deps]
  (when (seq deps)
    (str (str/join "\n" (sort (map str deps))) "\n")))

(def code-sections (ents/sections-of-tree :repo))

(def type-sections (ents/sections-of-tree :ontology))

(def container-sections (ents/sections-of-tree :container))

(def section->kinds-key
  (into {} (for [s type-sections
                 :let [section (ents/author->section s)]]
             [s (when (contains? ents/rid-sections section) section)])))

(defn partition-check! [sections where]
  (let [known (into code-sections (concat type-sections container-sections))]
    (when-let [orphans (not-empty (into (sorted-set) (remove known) sections))]
      (throw (ex-info (str "no git tree owns `.ent` section(s) "
                           (str/join ", " (map str orphans))
                           " — give them a :tree in sutra.types.entities (code-sections /"
                           " type-sections / container-sections)")
                      {:kind ::unowned-section :sections (vec orphans) :where where})))))

(def repo-sections
  (into code-sections container-sections))

(def ontology-sections
  (into type-sections container-sections))

(defn carried-sections
  [ontology?]
  (if ontology? (conj ontology-sections :applications) repo-sections))

(defn- entity-plane!
  [formats tree]
  (when-let [p (some #(when (= :file (plane-of formats %)) %) (keys tree))]
    (throw (ex-info (str "exporter emitted a path no text kind reads: " p)
                    {:kind ::plane-drift :path p})))
  tree)

(defn- keep-sections [ent sections]
  (update ent :schema #(into (sorted-map) (filter (comp sections key)) %)))

(defn- narrow
  [body fields]
  (reduce-kv (fn [b section fs]
               (cond-> b
                 (map? (get b section)) (update section update-vals #(select-keys % fs))))
             body
             fields))

(defn selected-body
  ([schema scope sections] (selected-body schema scope sections {}))
  ([schema scope sections {:keys [inline?]}]
   (let [scope (assoc scope :inline? (boolean inline?))]
     (:schema (keep-sections (:ent (sch/column schema [::export scope] #(emit/export % scope)))
                             sections)))))

(defn- placed-units
  ([formats memory units] (placed-units formats memory units nil))
  ([formats memory units only]
   (for [{:keys [kind nodes doc siblings] nm :name} units
         :let [path (when (some #(= kind (:kind %)) (vals formats))
                      (or (some #(get-in memory [:placement %]) nodes)
                          (authoring/unit-path formats kind nm)))]
         :when (and path (or (nil? only) (only path) (some (comp only :path) siblings)))]
     {:path     path
      :content  (authoring/print-file formats
                                      (or (get-in memory [:files path :format])
                                          (authoring/default-format formats path))
                                      doc {})
      :siblings siblings})))

(defn unit-files
  [schema projection unit]
  (when-let [[p] (seq (placed-units (registry/texts schema projection) {} [unit]))]
    (cons (dissoc p :siblings) (:siblings p))))

(defn- entity-files
  [files]
  (into {} (map (fn [{:keys [path content bytes]}]
                  [path (cond-> {:plane :entity}
                          bytes   (assoc :bytes bytes)
                          :always (assoc :content (or content "")))]))
        files))

(defn repo-tree
  [schema repo-name]
  (let [repo-rid  (emit/repository-rid schema repo-name)
        rspec     (get (:repositories schema) repo-rid)
        proj-name (some-> (get-in schema [:projects (:project rspec) :project-name]) name)
        {:keys [ent files units]} (emit/export schema {:repository repo-name
                                                  :project    proj-name
                                                  :inline?    false})
        _         (partition-check! (keys (:schema ent)) ::repo-tree)
        ent       (keep-sections ent repo-sections)
        repo      (registry/texts schema :code-repository)
        ent-path  (or (authoring/unit-path repo nil (emit/snake repo-name))
                      (throw (ex-info (str "no text kind of the projection :code-repository places "
                                           (emit/snake repo-name) ".ent")
                                      {:kind ::undeclared-root :repository repo-name})))
        reqs      (requirements-str (:python-deps rspec))
        placed    (placed-units repo {} units)]
    (entity-plane!
     repo
     (entity-files
      (concat [{:path ent-path
                :content (authoring/print-file repo (authoring/default-format repo ent-path)
                                               (:schema ent) {})}]
              (when reqs [{:path "transforms/requirements" :content reqs}])
              files
              (map #(dissoc % :siblings) placed)
              (mapcat :siblings placed))))))

(def ontology-ent-path
  "ontology.ent")

(def transform-ent-path
  "transform.ent")

(def trunk-branch
  "main")

(def owned-sections
  (into #{:projects}
        (filter #(contains? (ents/fields (ents/author->section %)) :project))
        type-sections))

(def repository-owned-sections
  (into #{}
        (filter #(contains? (ents/fields (ents/author->section %)) :repository))
        code-sections))

(defn- update-owned
  [section-val f]
  (if (map? section-val)
    (into (sorted-map) (map (fn [[k v]] [k (f (or v {}))])) section-val)
    (mapv (fn [e]
            (let [[nm from to card opts] e]
              (if (and (vector? e) (<= 4 (count e) 5) (or (nil? opts) (map? opts)))
                (let [o (f (or opts {}))]
                  (cond-> [nm from to card] (seq o) (conj o)))
                e)))
          section-val)))

(defn rename-types
  [schema m]
  (refs/rename ents/author-body-refs schema :object-types (update-keys m name)))

(defn owned-body
  ([path schema] (owned-body path schema nil))
  ([path schema facet]
   (let [fail  (fn [code params]
                 (refusal/refuse! code {:kind ::owned-text :path path} (assoc params :path path)))
         stray (sort (remove owned-sections (keys schema)))
         projs (:projects schema)]
     (when (seq stray)
       (fail :refusal.parser/stray-sections {:sections (str/join ", " stray)
                                             :central  ontology-ent-path}))
     (when-not (or (= 1 (count projs)) (and facet (empty? projs)))
       (fail :refusal.parser/project-count {:count (count projs)}))
     (let [[pname pspec] (first projs)
           own           (keyword (if pname (rid/->name pname) facet))]
       (when (and facet (not= facet (name own)))
         (fail :refusal.parser/facet-mismatch {:facet facet :project (name own)}))
       (when-let [clash (seq (filter #(and (contains? (:objects schema) %)
                                           (not
                                            (graph/extension-entry? (get-in schema [:objects %]))))
                                     (keys (:synonyms pspec))))]
         (fail :refusal.parser/synonym-clash {:synonyms (str/join ", " (map name clash))}))
       (into {}
             (for [[s v] (rename-types schema (:synonyms pspec))]
               [s (if (= :projects s)
                    {pname pspec}
                    (update-owned
                     v (fn [spec]
                         (let [p (:project spec)]
                           (when (and p (not= own (keyword (rid/->name p))))
                             (fail :refusal.parser/foreign-resource
                                   {:owner (rid/->name p) :project (name own)}))
                           (assoc spec :project own)))))]))))))

(defn- route-contributed
  [bodies project->path owner-of]
  (let [contributed (for [[path body] bodies
                          [k spec] (:objects body)
                          a (:attributes spec)
                          :let [p (some-> (:project (get a 2)) rid/->name)]
                          :when (and p (not= p (owner-of path)) (project->path p))]
                      [path k a p])
        strip       (fn [[n t o]] (let [o (dissoc o :project)] (cond-> [n t] (seq o) (conj o))))
        bodies      (reduce (fn [bs [path k a]]
                              (update-in bs [path :objects k :attributes]
                                         (fn [as] (vec (remove #(identical? a %) as)))))
                            bodies contributed)]
    (reduce (fn [bs [_ k a p]]
              (update-in bs [(project->path p) :objects k]
                         (fn [e] (update (or e {}) :attributes (fnil conj []) (strip a)))))
            bodies contributed)))

(defn- repository-body
  [schema repo]
  (into {}
        (for [[s v] schema]
          [s (if (repository-owned-sections s)
               (update-owned v
                             #(cond-> % (nil? (:repository %)) (assoc :repository (keyword repo))))
               v)])))

(defn- owned-schema
  [formats path {:keys [schema owner rule]}]
  (cond
    (nil? owner) schema
    (= :repository rule) (repository-body schema owner)
    (= :declared rule) (owned-body path schema (when (authoring/facet formats path)
                                                 (authoring/implied-project formats path)))
    :else (owned-body path schema owner)))

(defn- check-scope! [formats path {:keys [schema format]}]
  (when-let [bad (seq (remove #(authoring/allows? formats format %) (keys schema)))]
    (refusal/refuse! :refusal.parser/out-of-scope
                     {:kind ::out-of-scope :path path}
                     {:format   (name format)
                      :sections (str/join ", " (map str (sort bad)))
                      :allowed  (str/join ", " (map str (:sections (get formats format))))})))

(defn- implied-projects
  [read placement]
  (let [held (frequencies (vals placement))]
    (->> read
         (filter (fn [[_ r]] (and (:owner r) (#{:facet :file-name} (:rule r)))))
         (group-by (comp :owner second))
         (map (fn [[owner files]]
                [owner (ffirst (sort-by (fn [[p _]] [(- (get held p 0)) p]) files))])))))

(defn- with-implied-projects
  [g read]
  (reduce (fn [g [owner _path]]
            (if (some #(= owner (rid/->name %)) (keys (get-in g [:schema :projects])))
              g
              (assoc-in g [:schema :projects (keyword owner)] {})))
          g
          (implied-projects read (:placement g))))

(defn- merge-read
  [formats read]
  (doseq [[p r] read] (check-scope! formats p r))
  (-> (graph/merge-bodies (for [[p r] read] [p (owned-schema formats p r)]))
      (with-implied-projects read)
      (assoc :formats formats
             :files (into {} (for [[p r] read] [p (dissoc r :schema)])))))

(def ^:private readings (atom {}))

(defn- read-file* [formats p text]
  (let [rf authoring/read-file
        k  [rf formats p text]]
    (if-let [hit (get @readings k)]
      hit
      (let [r (rf formats p text)]
        (swap! readings #(assoc (if (< 1024 (count %)) {} %) k r))
        r))))

(defn- read-files
  [formats paths text-at]
  (into []
        (keep (fn [p]
                (when (authoring/file? formats p)
                  (some->> (read-file* formats p (text-at p)) (vector p)))))
        (sort paths)))

(defn read-texts
  [formats paths text-at]
  (merge-read formats (read-files formats paths text-at)))

(def git-projections
  {:ontology :ontologie :repo :code-repository})

(defn kinds-problems
  [formats]
  (authoring/problems formats {:project owned-sections :repository repository-owned-sections}))

(defn tree-formats
  [live nm paths text-at]
  (let [formats  (registry/texts live nm)
        central  (filter (fn [p] (let [d (get formats (authoring/default-format formats p))]
                                   (and d (empty? (:owner d)) (nil? (:kind d)))))
                         paths)
        declared (when (seq central)
                   (try (some-> (read-texts formats central text-at) :schema
                                (registry/projection nm) :filename)
                        (catch clojure.lang.ExceptionInfo _ nil)))]
    (if (seq declared) (registry/formats declared) formats)))

(defn checkout-formats
  ([schema paths] (checkout-formats schema paths nil))
  ([schema paths text-at]
   (let [git   (filter #(registry/projection schema %) (vals git-projections))
         names (sort (map name (if (seq git) git (keys (:projections schema)))))
         kinds (fn [nm]
                 (if text-at (tree-formats schema nm paths text-at) (registry/texts schema nm)))
         reads (fn [formats] (count (filter #(= :entity (plane-of formats %)) paths)))]
     (reduce (fn [best f] (if (>= (reads f) (reads best)) f best)) {} (map kinds names)))))

(defn unread-files
  [formats paths]
  (vec (remove #(or (authoring/file? formats %) (authoring/sibling? formats %)) paths)))

(defn- read-placement [formats paths text-at]
  (dissoc (read-texts formats paths text-at) :schema))

(defn placement-of
  [formats prior]
  (let [paths   (filter #(authoring/file? formats %) (keys prior))
        text-at #(force (get prior %))]
    (if (empty? paths)
      {}
      (try (read-placement formats paths text-at)
           (catch clojure.lang.ExceptionInfo _
             (apply merge-with merge {}
                    (for [p (sort paths)
                          :let [r (try (read-placement formats [p] text-at)
                                       (catch clojure.lang.ExceptionInfo _ nil))]
                          :when r]
                      (select-keys r [:placement :fields :attributes :files]))))))))

(defn without-layouts
  [schema]
  (cond-> schema
    (:projections schema) (update :projections update-vals #(cond-> % (map? %) (dissoc :layout)))))

(defn- own-file-body
  [body own implied? field]
  (let [[pname pspec] (first (:projects body))
        strip         #(if (= own (some-> (get % field) rid/->name)) (dissoc % field) %)
        body          (rename-types
                       (into {} (for [[s v] body]
                                  [s (if (= :projects s) {pname pspec} (update-owned v strip))]))
                       (set/map-invert (:synonyms pspec)))]
    (if (and implied? pname (= own (rid/->name pname)) (empty? pspec)
             (= 1 (count (:projects body))))
      (dissoc body :projects)
      body)))

(def ^:private prints (atom {}))

(defn- print-file* [formats fmt b]
  (let [pf authoring/print-file
        k  [pf formats fmt b]]
    (or (get @prints k)
        (let [t (pf formats fmt b {})]
          (swap! prints #(assoc (if (< 1024 (count %)) {} %) k t))
          t))))

(defn node-body
  [idx nodes]
  (tree/body (keep #(some->> (get idx %) (vector %)) (set nodes))))

(defn- entry-value
  [{:keys [value vector?]}]
  (if vector? (get value 4) value))

(defn- entry-owner
  [field [section nm] e]
  (if (and (= :projects section) (= :project field))
    (rid/->name nm)
    (let [v (entry-value e)] (some-> (when (map? v) (get v field)) rid/->name))))

(defn holding
  [schema export]
  (sch/with-views schema {::held export}))

(defn- held-export
  [schema scope]
  (when (:all-projects? scope) (sch/column schema ::held (constantly nil))))

(defn exports
  [schema scope]
  (let [col   (fn [k f] (delay (sch/column schema k f)))
        sc    (assoc scope :inline? false)
        held  (held-export schema scope)
        laid  (col [::lay-out sc] #(emit/export (without-layouts %) sc))
        index (fn [k sc] (col [k sc] #(tree/index (:schema (:ent (emit/export % sc))))))
        lidx  (or (:laid held)
                  (col [::lay-out-index sc] (fn [_] (tree/index (:schema (:ent @laid))))))
        owned (fn [field]
                @(col [::by-owner sc field]
                      (fn [_]
                        (reduce (fn [m [n e]]
                                  (update-in m [(entry-owner field n e) (first n)]
                                             (fnil conj (sorted-set)) n))
                                {} @lidx))))]
    {:laid     lidx
     :units    (or (:units held) (delay (:units @laid)))
     :authored (or (:authored held) (index ::export-index sc))
     :inlined  (or (:inlined held) (index ::export-index (assoc scope :inline? true)))
     :members  (delay (update-vals (dissoc (owned :project) nil)
                                   #(into (sorted-set) cat (vals %))))
     :by-owner owned
     :unit-of  (col [::unit-of sc]
                    (fn [_] (into {} (for [u (:units @laid) n (:nodes u)] [n u]))))
     :type     (let [by (col ::object-types-by-name
                             #(into {} (for [[r o] (:object-types %)
                                             :let [n (some-> (:object-type-name o) rid/->name)]]
                                         [n [r o]])))]
                 (fn [k] (get @by (rid/->name k))))
     :primary-key (fn [r] (world-rules/object-primary-key schema r))}))

(defn exported-nodes
  [ex sections nodes {:keys [inline?]}]
  (node-body @(if inline? (:inlined ex) (:authored ex)) (filter (comp sections first) nodes)))

(defn- placer
  [formats memory]
  (let [files-of (or (:files memory) {})
        in-text  (co/texts memory)
        fmt-of   (fn [path]
                   (or (get-in files-of [path :format]) (authoring/default-format formats path)))
        allowed? (fn [path section] (authoring/allows? formats (fmt-of path) section))
        owner-of (fn [path] (if-let [f (get files-of path)]
                              (:owner f)
                              (when (seq (:owner (get formats (fmt-of path))))
                                (authoring/implied-project formats path))))
        field    (authoring/owner-field formats)
        project  (fn [[section nm] v] (if (and (= :projects section) (= :project field))
                                        (rid/->name nm)
                                        (some-> (get v field) rid/->name)))
        fits?    (fn [path section pn]
                   (and path (authoring/file? formats path) (fmt-of path) (allowed? path section)
                        (let [o (owner-of path)] (or (nil? o) (= o pn)))))
        owned    (fn [pn] (sort-by (fn [p] [(or (:order (get formats (fmt-of p))) 100) p])
                                   (keep (fn [[p f]] (when (= pn (:owner f)) p)) files-of)))
        keeps?   (fn [o section path]
                   (and (= o (get-in files-of [path :owner]))
                        (some (fn [[part el]] (and (= :placement part) (= section (first el))))
                              (in-text path))))
        own-text (fn [pn section]
                   (or (some #(when (and (keeps? pn section %) (fits? % section pn)) %)
                             (owned pn))
                       (authoring/project-home formats pn section)
                       (some #(when (fits? % section pn) %) (owned pn))))
        place    (fn [[section :as node] v]
                   (let [pn (project node v)
                         m  (get-in memory [:placement node])]
                     (or (when (fits? m section pn) m)
                         (some-> pn (own-text section))
                         (authoring/home formats section))))
        home     (fn [[section nm :as node] v]
                   (or (place node v)
                       (refusal/refuse! :refusal.parser/homeless
                                        {:kind ::homeless :node node}
                                        {:section     (name section)
                                         :name        (rid/->name nm)
                                         :section-key (str section)})))]
    {:files-of files-of :fmt-of fmt-of :allowed? allowed? :owner-of owner-of :field field
     :project project :fits? fits? :own-text own-text :place place :home home
     :in-text in-text}))

(defn- place-of
  [formats memory p]
  (if (co/source? memory)
    (fn [n v] ((:place (placer formats (co/memory-for memory [n]))) n v))
    (:place p)))

(defn witness
  [ex formats memory paths pname]
  (let [members @(:members ex)
        {:keys [owner-of fits? project allowed? in-text] :as p} (placer formats memory)
        place   (place-of formats memory p)
        idx     @(:laid ex)
        home-of (fn [n] (place n (entry-value (get idx n))))
        its?    (fn [path] (and path (not (contains? paths path))
                                (= pname (some-> (owner-of path) rid/->name))))
        lands?  (fn [path [part el]]
                  (let [[[section :as n]] (if (= :placement part) [el] el)
                        e (get idx n)]
                    (when e
                      (case part
                        :placement  (= path (home-of n))
                        :fields     (fits? path section (project n (entry-value e)))
                        :attributes (and (authoring/file? formats path)
                                         (allowed? path section))))))]
    (or (first (filter (comp its? home-of) (get members pname)))
        (first (for [path (sort (distinct
                                 (concat (authoring/project-texts formats pname)
                                         (keep (fn [[path f]] (when (= pname (:owner f)) path))
                                               (:files memory))
                                         (when-not (co/source? memory)
                                           (keys (co/by-path memory))))))
                     :when (its? path)
                     [part el :as pe] (sort-by pr-str (in-text path))
                     :when (lands? path pe)]
                 (if (= :placement part) el (first el)))))))

(defn pulled-nodes
  [ex formats memory sections path]
  (let [{:keys [owner-of own-text field in-text] :as p} (placer formats memory)
        place    (place-of formats memory p)
        idx      @(:laid ex)
        by-owner ((:by-owner ex) field)
        home-of  (fn [n] (when-let [e (get idx n)] (place n (entry-value e))))
        o        (some-> (owner-of path) rid/->name)
        central  (filter #(= path (authoring/home formats %)) sections)
        cands    (concat (mapcat val (get by-owner o))
                         (for [s central
                               [pn by-sec] by-owner
                               :when (or (nil? pn) (nil? (own-text pn s)))
                               n (get by-sec s)]
                           n))]
    (-> #{}
        (into (for [[part el] (in-text path)] (if (= :placement part) el (first el))))
        (into (filter #(= path (home-of %))) cands))))

(declare unit-path)

(defn project-paths
  [ex formats memory pname]
  (let [{:keys [field]} (placer formats memory)
        unit-of @(:unit-of ex)]
    (-> (set (keep (fn [[p f]] (when (= pname (some-> (:owner f) rid/->name)) p))
                   (:files memory)))
        (into (authoring/project-texts formats pname))
        (into (for [[_ ns] (get ((:by-owner ex) field) pname)
                    n ns
                    :let [u (unit-of n)]
                    :when u]
                (unit-path formats (co/memory-for memory (:nodes u)) u))))))

(defn related-paths
  [ex formats memory nodes]
  (let [idx @(:laid ex)
        mem (co/memory-for memory nodes)
        {:keys [place]} (placer formats mem)]
    (into (set (for [part [:placement :fields :attributes] [_ p] (get mem part)] p))
          (keep (fn [n] (when-let [e (get idx n)]
                          (place n (entry-value e)))))
          nodes)))

(defn- unit-path
  [formats memory {:keys [kind nodes] nm :name}]
  (or (some #(get-in memory [:placement %]) nodes) (authoring/unit-path formats kind nm)))

(defn- fragments
  [ex nodes formats memory paths]
  {:ent   {:schema (node-body @(:laid ex) nodes)}
   :units (filter (fn [u] (or (some nodes (:nodes u))
                              (contains? paths (unit-path formats memory u))))
                  @(:units ex))})

(defn lay-out
  [schema {:keys [scope sections fields formats memory body only nodes paths]}]
  (let [scope'              (assoc scope :inline? false)
        held                (held-export schema scope)
        {:keys [ent units]} (cond
                              body  {:ent {:schema (without-layouts body)}}
                              nodes (fragments (exports schema scope) nodes formats memory
                                               (set paths))
                              held  {:ent   {:schema @(:laid-body held)}
                                     :units @(:units held)}
                              :else (sch/column schema [::lay-out scope']
                                                #(emit/export (without-layouts %) scope')))
        _        (partition-check! (keys (:schema ent)) ::lay-out)
        body     (narrow (:schema (keep-sections ent sections)) fields)
        {:keys [files-of fmt-of allowed? owner-of field project fits? own-text home]}
        (placer formats memory)
        node-of  (into {} (for [[section sv] body
                                :when (map? sv)
                                [k v] sv]
                            [[section (rid/->name k)] (when (map? v) (project [section k] v))]))
        memory   (-> memory
                     (update :fields #(into {} (filter (fn [[[[section :as node] _] path]]
                                                         (fits? path section (node-of node))))
                                            %))
                     (update :attributes
                             #(into {} (filter (fn [[[[section] _] path]]
                                                 (and (authoring/file? formats path)
                                                      (allowed? path section))))
                                    %)))
        central  (when (and (authoring/file? formats ontology-ent-path)
                            (empty? (:owner (get formats
                                                 (authoring/default-format formats
                                                                           ontology-ent-path)))))
                   ontology-ent-path)
        bodies   (cond-> (route-contributed (graph/distribute body home memory)
                                            #(own-text % :objects) owner-of)
                   central (update central #(or % {})))
        files    (into {}
                       (for [[path b] bodies
                             :let [b        (into {} (filter (comp seq val)) b)
                                   own      (owner-of path)
                                   implied?
                                   (or (#{:facet :file-name
                                          :repository} (get-in files-of [path :rule]))
                                       (and own (not (files-of path))))
                                   b        (if (and own (or implied? (= 1 (count (:projects b)))))
                                              (own-file-body b own implied? field)
                                              b)]
                             :when (and (or (nil? only) (only path))
                                        (or (seq b) (= path central)))]
                         [path
                          {:content (print-file* formats (or (fmt-of path) :zentral) b)
                           :plane   :entity}]))]
    (entity-plane! formats
                   (merge files
                          (cond->> (entity-files
                                    (mapcat #(cons (dissoc % :siblings) (:siblings %))
                                            (placed-units formats memory units only)))
                            only (into {} (filter (comp only key))))))))

(defn ontology-tree
  ([schema] (ontology-tree schema nil nil))
  ([schema prior] (ontology-tree schema prior nil))
  ([schema prior memory]
   (let [formats (registry/texts schema :ontologie)]
     (lay-out schema {:scope {:all-projects? true} :sections ontology-sections
                      :formats formats
                      :memory (co/memory-merge (placement-of formats prior) memory)}))))

(defn declared-type-names
  [schema-body]
  (into {}
        (keep (fn [section]
                (when-let [m (get schema-body section)]
                  [section (into (sorted-set)
                                 (map #(name (if (map-entry? %) (key %) (first %))))
                                 m)])))
        type-sections))

(defn declared-type-statuses
  [schema-body]
  (into {}
        (keep (fn [section]
                (when-let [m (get schema-body section)]
                  (let [pairs (into {}
                                    (keep (fn [x]
                                            (let [[nm entry] (if (map-entry? x)
                                                               [(key x) (val x)]
                                                               [(first x) (get x 4)])]
                                              (when-let [status (:status entry)]
                                                [(name nm) (keyword status)]))))
                                    m)]
                    (when (seq pairs) [section pairs])))))
        type-sections))
