(ns sutra.lineage.facts
  (:require [sutra.types.entities :as ents]
            [sutra.types.transform-io :as tio]
            [sutra.kit.refusal :as refusal]
            [sutra.types.kinds :as kinds]
            [clojure.string :as str]
            [sutra.kit.digest :as digest]
            [sutra.kit.time :as time]
            [sutra.parser.impact :as impact]
            [sutra.schedules.api :as schedules]
            [sutra.builds.api :as builds]
            [sutra.lineage.graph :as lineage]
            [sutra.catalog.retirement :as retirement]
            [sutra.catalog.llm-models :as llm-models]
            [sutra.catalog.lookup :as lookup]
            [sutra.types.project-graph :as pg]
            [sutra.types.rid :as rid]
            [sutra.types.schema :as sch]
            [sutra.types.virtual :as vtypes]
            [sutra.capps.catalog :as cat]
            [sutra.types.refs :as refs]))

(set! *warn-on-reflection* true)

(def ^:private nm rid/->name)

(defn node-id [kind n] (str (nm kind) "/" (nm n)))

(defn- tag-of [section] (some-> (rid/kinds-key->tag section) nm))

(def ^:private reader-rel
  {:applications :shows :functions :reads :exports :exports :pipelines :imports
   :object-sets :selects :templates :shows :custom-applications :shows
   :object-type-groups :groups :datasets :materializes :object-types :backs})

(defn named-nodes
  [schema]
  (let [idx  (sch/index schema)
        name* (fn [k] (nm (or (sch/name-of idx k) k)))
        of   (fn [section]
               (into {} (for [[k _] (get schema section)]
                          [(name* k) (node-id (tag-of section) (name* k))])))]
    {:name*        name*
     :object-types (of :object-types)
     :link-types   (of :link-types)
     :applications (of :applications)
     :datasets     (of :datasets)
     :transforms   (of :transforms)
     :actions      (into {} (for [[an _] (lookup/actions-by-name schema)]
                              [(nm an) (node-id :action an)]))
     :functions    (merge
                    (into {} (for [[k f] (:functions schema)
                                   :let [n (nm (or (:function-name f) (name* k)))]]
                               [n (node-id :function n)]))
                    (into {} (for [[k t] (:transforms schema) :when (:function-name t)]
                               [(nm (:function-name t))
                                (node-id :transform (or (:transform-name t) (name* k)))])))}))

(defn known-node
  [known section t]
  (when (or (keyword? t) (string? t))
    (get-in known [section ((:name* known) (if (keyword? t) (name t) t))])))

(defn- io-node
  [{:keys [kind name]}]
  (node-id (kinds/lineage-node (keyword (nm kind))) name))

(defn- io-edges
  [schema]
  (mapcat (fn [{tn :name :keys [inputs outputs]}]
            (let [t (node-id :transform tn)]
              (concat
               (for [i inputs :when (not= :language-model (keyword (nm (:kind i))))]
                 {:from (io-node i) :to t :rel :input})
               (for [o outputs]
                 {:from t :to (io-node o) :rel :output}))))
          (lineage/edges schema)))

(defn- written-dataset-edges
  [schema]
  (let [idx  (sch/index schema)
        read (into #{} (for [[_ t] (:transforms schema), [_ in] (:inputs t)
                             :when (= :dataset (:kind in))]
                         (:dataset in)))]
    (for [[_ t] (:transforms schema)
          {:keys [kind dataset]} (tio/outputs-of t)
          :when (and (= :object-type kind) (read dataset))]
      {:from (node-id :transform (:transform-name t))
       :to   (node-id :dataset (or (sch/name-of idx dataset) dataset))
       :rel  :output})))

(defn- reference-edges
  [schema]
  (let [idx (sch/index schema)
        id  (fn [section r] (node-id (tag-of section) (or (sch/name-of idx r) r)))]
    (for [{:keys [from-kind from to-kind to]} (pg/references schema)
          :when (and (not= :transforms from-kind)
                     (ents/in-role? :lineage-referenced to-kind))]
      {:from (id to-kind to) :to (id from-kind from)
       :rel (get reader-rel from-kind :references)})))

(defn- action-uses
  [a]
  (concat
   (for [[k t] (impact/action-targets a) :when (#{:function :action} k)]
     [(if (= :function k) :functions :actions) t :calls])
   (for [[_ p] (when (map? (:params a)) (:params a))
         t     [(:type p) (get-in p [:options-from :object-type])]]
     [:object-types t :param])
   [[:object-types (:on a) :param]
    [:actions (get-in a [:deprecation :replacement]) :replaces]]))

(defn- action-edges
  [schema known]
  (let [idx (sch/index schema)]
    (concat
     (for [[an a] (lookup/actions-by-name schema)
           [k t]  (impact/action-targets a)
           :when  (kinds/is? :typed-output? k)]
       {:from (node-id k (or (sch/name-of idx t) t)) :to (node-id :action an) :rel :edits})
     (for [[an a]           (lookup/actions-by-name schema)
           [section t rel]  (action-uses a)
           :let             [from (known-node known section t)]
           :when            from]
       {:from from :to (node-id :action an) :rel rel}))))

(defn- function-edges
  [schema known]
  (for [[k f] (concat (:functions schema) (filter (comp :function-name val) (:transforms schema)))
        :let  [refs (impact/function-references (:source f))
               to   (known-node known :functions (or (:function-name f) k))]
        :when to
        e     (concat
               (for [ot (:reads-types refs)]
                 {:from (node-id :object-type ot) :to to :rel :reads})
               (for [ot (:edits-types refs) :let [from (known-node known :object-types ot)]
                     :when from]
                 {:from from :to to :rel :edits})
               (for [a (:invokes-actions refs) :let [from (known-node known :actions a)]
                     :when from]
                 {:from from :to to :rel :calls})
               (for [lib (:uses f) :let [from (known-node known :functions lib)] :when from]
                 {:from from :to to :rel :uses}))]
    e))

(def ^:private app-reference-rel
  {:actions :calls :functions :calls :link-types :shows :applications :embeds})

(defn- app-edges
  [schema known]
  (concat
   (for [[app ots] (lineage/app-uses schema) ot ots]
     {:from (node-id :object-type ot) :to (node-id :application app) :rel :shows})
   (for [[k a]         (:applications schema)
         :let          [to (node-id :application
                                    (or (:application-name a) ((:name* known) k)))]
         [section ns*] (cat/app-references a)
         t             ns*
         :let          [from (known-node known section t)]
         :when         (and from (not= from to))]
     {:from from :to to :rel (app-reference-rel section)})))

(declare model-of)

(defn- workspace-edges
  [schema known]
  (for [[k w] (:workspaces schema)
        t     (distinct (concat (:apps w) (:anchored w) [(:home w) (:landing w)]))
        :let  [from (known-node known :applications t)]
        :when from]
    {:from from :to (node-id :workspace ((:name* known) k)) :rel :shelves}))

(defn- source-edges
  [schema]
  (concat
   (for [[_ t]            (:transforms schema)
         {:keys [provider]} (model-of schema t)
         :when provider]
     {:from (node-id :source provider) :to (node-id :transform (:transform-name t)) :rel :calls})
   (for [[_ ds] (:datasets schema)
         :let   [s (when (vtypes/virtual? ds) (vtypes/source-name ds))]
         :when  s]
     {:from (node-id :source s) :to (node-id :dataset (:dataset-name ds)) :rel :reads})
   (for [{tn :name ext :external-systems} (lineage/edges schema) s ext]
     {:from (node-id :source s) :to (node-id :transform tn) :rel :calls})))

(defn- eval-edges
  [schema]
  (mapcat (fn [[k s]]
            (let [e (node-id :eval-suite k)]
              (concat
               (for [{ot :object-type} (:cases-from s) :when ot]
                 {:from (node-id :object-type ot) :to e :rel :cases})
               (for [kind [:function :agent :action] :let [t (get s kind)] :when t]
                 {:from (node-id kind t) :to e :rel :evaluates}))))
          (:eval-suites schema)))

(defn- automation-edges
  [automations]
  (for [{:keys [name on]} automations :when on]
    {:from (node-id :object-type on) :to (node-id :automation name) :rel :watches}))

(defn edges
  [schema automations]
  (let [known (named-nodes schema)]
    (->> (concat (io-edges schema) (written-dataset-edges schema) (reference-edges schema)
                 (source-edges schema) (workspace-edges schema known)
                 (action-edges schema known)
                 (function-edges schema known) (app-edges schema known) (eval-edges schema)
                 (automation-edges automations))
         (remove #(= (:from %) (:to %)))
         distinct
         vec)))

(def ^:private node-sections
  [:transforms :datasets :object-types :link-types :sources :actions :applications
   :functions :exports])

(defn- project-names [schema]
  (into {} (for [[k p] (:projects schema)] [k (nm (or (:project-name p) k))])))

(defn nodes
  [schema es]
  (let [projects (project-names schema)
        declared (into {}
                       (for [section node-sections
                             [k v] (get schema section)
                             :let [n (or (sch/name-of (sch/index schema) k) k)
                                   kind (tag-of section)]]
                         [(node-id kind n)
                          (cond-> {:kind kind :name (nm n)}
                            (:project v) (assoc :project (get projects (:project v)
                                                              (nm (:project v)))))]))
        seen     (for [e es id [(:from e) (:to e)] :when (not (contains? declared id))]
                   (let [[k n] (str/split id #"/" 2)] [id {:kind k :name n}]))]
    (into declared seen)))

(defn- adjacency [es k-from k-to]
  (reduce (fn [m e] (update m (get e k-from) (fnil conj #{}) (get e k-to))) {} es))

(defn- walk
  [adj start depth]
  (loop [seen (set start) frontier (set start) d 0]
    (if (or (empty? frontier) (and depth (>= d depth)))
      seen
      (let [nxt (into #{} (comp (mapcat adj) (remove seen)) frontier)]
        (recur (into seen nxt) nxt (inc d))))))

(defn- depth-of [s default]
  (cond (nil? s) default
        (= "all" (str/lower-case (str s))) nil
        :else (or (parse-long (str s))
                  (refusal/refuse! :refusal.lineage/depth {:depth s}))))

(defn resolve-node
  [schema ns handle]
  (let [h   (str/trim (str handle))
        idx (sch/index schema)]
    (or (when (rid/rid? h)
          (when-let [section (sch/section-of idx h)]
            [(node-id (tag-of section) (sch/name-of idx h))]))
        (when (contains? ns h) [h])
        (not-empty (vec (sort (for [[id {n :name}] ns :when (= h n)] id))))
        (refusal/refuse! :refusal.lineage/no-node {:node h}))))

(defn resolve-nodes
  [schema ns handles]
  (vec (distinct (mapcat #(resolve-node schema ns %)
                         (remove str/blank? (map str/trim (str/split (str handles) #",")))))))

(def ^:private io-rels #{:input :output :backs})

(defn- duration-ms
  [s]
  (when-let [[_ n u] (re-matches #"(\d+)\s*([smhdw])" (str/trim (str s)))]
    (* (parse-long n) ({"s" 1000 "m" 60000 "h" 3600000 "d" 86400000 "w" 604800000} u))))

(defn- ms [v] (when (some? v) (time/->epoch-ms (str v))))

(def ^:private job-word
  {"COMPLETED" "succeeded" "FAILED" "failed" "RUNNING" "running" "DID_NOT_RUN" "skipped"
   "ABORTED" "aborted" "WAITING" "waiting" "RUN_PENDING" "waiting" "ABORT_PENDING" "running"})

(defn- schedules-by-target [schedules]
  (reduce (fn [m s] (reduce #(update %1 %2 (fnil conj []) s) m (schedules/targets s)))
          {} schedules))

(defn- run-schedule-actions
  [schema]
  (reduce (fn [m [an a]]
            (reduce (fn [m step]
                      (cond-> m
                        (and (= :run-schedule (:op step)) (:schedule step))
                        (update (nm (:schedule step)) (fnil conj []) (nm an))))
                    m (refs/steps (:rules a))))
          {} (lookup/actions-by-name schema)))

(defn- provider-sources
  [schema]
  (into #{} (keep #(some-> (llm-models/source-of schema %) nm)) (keys (:llm-models schema))))

(defn- model-of
  [schema t]
  (vec (for [[alias in] (:inputs t)
             :when (= :language-model (keyword (nm (:kind in))))
             :let  [m (:model in)]]
         {:alias    (nm alias)
          :model    (nm m)
          :provider (nm (or (:source in) (llm-models/source-of schema m)))
          :calls    (nm (llm-models/provider-model schema m))})))

(defn- declared-at
  [schema placements t tn]
  (let [repo (some->> (:repository t) (get (:repositories schema)) :repository-name nm)
        key  [:transforms (nm tn)]]
    {:repository (or repo "ohne Repository")
     :text       (or (get (:code-repository placements) key)
                     (get (:ontologie placements) key))}))

(defn- job-fact [j]
  (when j
    (cond-> {:at (str (or (:ended-at j) (:started-at j)))}
      (some? (:rows-out j)) (assoc :rows-out (:rows-out j))
      (:error j)            (assoc :error (str (:error j)))
      (:run-id j)           (assoc :run-id (str (:run-id j))))))

(def coverage
  {:counted     ["transform inputs" "object types backed by a dataset" "materializations"
                 "links (through the consumers of their end types)"
                 "actions (edits, parameter types)"
                 "apps (opening type, declared types, every object set their layout reads)"
                 "functions (OntologyClient/search and :edits in their source, :target-type)"
                 "eval suites (:cases-from)"
                 "automations (:on)" "exports" "object sets" "pipelines" "templates"
                 "custom apps" "object-type groups"]
   :not-counted ["code that queries the ontology or a dataset outside its declared inputs"
                 "a function naming a type only at run time"
                 "readers outside Sutra (a BI tool, a script against the API)"]})

(defn- index-edges [es]
  {:out (group-by :from es) :in (group-by :to es)})

(defn- endpoint-consumers
  [{:keys [out in]} link]
  (let [ends (for [e (get in link)
                   :when (and (= :references (:rel e)) (str/starts-with? (:from e) "object-type/"))]
               (:from e))]
    (vec (distinct (for [ot ends e (get out ot)
                         :when (not (#{:input :references} (:rel e)))]
                     (:to e))))))

(defn- changed-at
  [{:keys [txns ok rid-of eidx]} id]
  (let [[kind] (str/split id #"/" 2)]
    (or (when (= "dataset" kind) (some-> (get txns (rid-of id)) :ended_at ms))
        (some->> (get (:in eidx) id)
                 (keep #(when (= :output (:rel %))
                          (some-> (get ok (rid-of (:from %))) :ended-at ms)))
                 seq (reduce max)))))

(defn source-tables
  [t]
  (let [{:keys [table query]} (:source t)
        in-sql (map (comp #(str/replace % #"[\[\]\"]" "") second)
                    (re-seq #"(?i)\b(?:from|join)\s+([\[\]\"\w.]+)" (str query)))]
    (vec (sort (distinct (cond-> in-sql (seq table) (conj (nm table))))))))

(defn- read-time?
  [t]
  (let [outs (tio/outputs-of t)]
    (and (seq outs)
         (every? #(and (= :link-type (some-> (:kind %) keyword)) (:fk-attribute %)) outs))))

(defn transform-facts
  [schema {:keys [jobs placements now eidx sched-of providers] :as ctx} tid t]
  (let [tn       (second (str/split tid #"/" 2))
        last-job (get-in jobs [:any tid])
        ok       (get-in jobs [:ok tid])
        failed   (get-in jobs [:failed tid])
        outs-e   (filter #(= :output (:rel %)) (get (:out eidx) tid))
        outputs  (mapv :to outs-e)
        inputs   (vec (for [e (get (:in eidx) tid) :when (= :input (:rel e))] (:from e)))
        targets  (cons tn (map #(second (str/split % #"/" 2)) outputs))
        scheds   (distinct (mapcat sched-of targets))
        readers  (vec (distinct (for [o outputs e (get (:out eidx) o)] (:to e))))
        via      (into {} (for [o outputs
                                :when (str/starts-with? o "link-type/")
                                :let [c (endpoint-consumers eidx o)]
                                :when (seq c)]
                            [o c]))
        unread?  (fn [o] (and (not (get via o)) (empty? (get (:out eidx) o))))
        ok-ms    (some-> ok :ended-at ms)
        newer    (vec (for [i inputs
                            :let [c (changed-at ctx i)]
                            :when (and c (or (nil? ok-ms) (> c ok-ms)))]
                        i))
        models   (model-of schema t)
        declared (mapv nm (:external-systems t))
        called   (set (keep :provider models))]
    (cond-> {:transform  tn
             :status     (if (read-time? t)
                           "read-time"
                           (get job-word (some-> last-job :status str) "never"))
             :declared   (declared-at schema placements t tn)
             :triggers   (vec (for [s scheds]
                                (cond-> {:schedule (nm (:name s)) :paused (boolean (:paused s))
                                         :trigger (schedules/trigger-word s)}
                                  (seq (get (:run-by ctx) (nm (:name s))))
                                  (assoc :run-by-actions (get (:run-by ctx) (nm (:name s)))))))
             :last-run   (job-fact last-job)
             :last-success (job-fact ok)
             :last-failure (job-fact failed)
             :model      models
             :external-systems declared
             :inputs     inputs
             :outputs    outputs
             :readers    readers
             :read-via-link-ends via
             :dead?      (boolean (and (seq outputs) (every? unread? outputs)))
             :inputs-changed-since-success newer
             :since-success-ms (when ok-ms (- now ok-ms))}
      (empty? outputs) (assoc :no-outputs? true)
      (seq (source-tables t)) (assoc :source-tables (source-tables t))
      (seq (remove called (filter providers declared)))
      (assoc :declared-not-called (vec (remove called (filter providers declared)))))))

(def ^:private blockable
  {"transform" :transforms "dataset" :datasets "object-type" :object-types
   "link-type" :link-types})

(defn- in-no-text?
  [schema placements section n]
  (let [k [section (nm n)]
        v (get-in schema [section (lookup/rid-of schema section (keyword n))])]
    (and (not (get (:ontologie placements) k))
         (not (get (:code-repository placements) k))
         (not (:repository v)))))

(defn blocked-node
  [schema placements referrers id]
  (let [[kind n] (str/split id #"/" 2)
        section  (get blockable kind)
        ident    (keyword n)]
    (when (and section (lookup/rid-of schema section ident))
      (let [reasons (cond-> (mapv (fn [r]
                                    (if (= :referenced (:reason r))
                                      {:reason :referenced
                                       :by (vec (distinct
                                                 (for [{:keys [in entity]} (:referrers r)]
                                                   (node-id (tag-of in)
                                                            (or (lookup/name-of schema in entity)
                                                                entity)))))}
                                      (update r :repository #(some-> % nm))))
                                  (retirement/deletion-reasons schema section ident referrers))
                      (in-no-text? schema placements section n)
                      (conj {:reason :in-no-text
                             :message (str "installed but in no text — remove it with `"
                                           (nm kind) " delete` on a branch, not by "
                                           "editing a text")}))]
        (when (seq reasons) {:node id :reasons reasons})))))

(defn- graph-scope
  [spec]
  (into #{} (for [[section names] (:visible-types spec)
                  :let [kind (tag-of section)]
                  :when kind
                  n names]
              (node-id kind n))))

(defn- project-rid
  [schema project]
  (or (some (fn [[k p]] (when (= (nm project) (nm (or (:project-name p) k))) k))
            (:projects schema))
      (lookup/rid-of schema :projects (keyword (nm project)))
      (refusal/refuse! :refusal.lineage/no-project {:project project})))

(defn readers-by-kind
  [schema automations id]
  (let [es (edges schema automations)]
    (reduce (fn [m e]
              (let [[k n] (str/split (:to e) #"/" 2)]
                (update m (keyword k) (fnil conj []) {:name n :rel (:rel e)})))
            {} (filter #(= id (:from %)) es))))

(def ^:private built-sections
  [[:datasets :dataset :dataset-name] [:object-types :object-type :object-type-name]
   [:link-types :link-type :link-type-name]])

(defn- jobspec-nodes
  [schema js]
  (into {} (for [[section kind k] built-sections
                 [rid spec] (get schema section)
                 :let [{:keys [branch] s :spec} (get js [section (str rid)])]]
             [(node-id kind (get spec k))
              (if s
                {:jobspec branch :jobspec-id (:id s) :jobspec-transform (:transform s)}
                {:jobspec "none"})])))

(defn fact-ctx
  [schema es {:keys [jobs schedules] :as answers}]
  (let [js     (some-> (:jobspecs answers) force)
        t-rid  (into {} (for [[k t] (:transforms schema)]
                          [(node-id :transform (:transform-name t)) (str k)]))
        by-rid (fn [m] (into {} (for [[id r] t-rid :let [j (get m r)] :when j] [id j])))
        by-tgt (schedules-by-target schedules)]
    (assoc answers
           :spec-ids  (into {} (for [{s :spec} (vals js)]
                                 [(node-id :transform (:transform s)) (:id s)]))
           :jobspec-nodes (when js (jobspec-nodes schema js))
           :t-rid     t-rid
           :jobs      {:any (by-rid (:any jobs)) :ok (by-rid (:ok jobs))
                       :failed (by-rid (:failed jobs))}
           :ok        (:ok jobs)
           :rid-of    (fn [id]
                        (let [[kind n] (str/split id #"/" 2)]
                          (or (get t-rid id)
                              (when (= "dataset" kind)
                                (some-> (lookup/rid-of schema :datasets (keyword n)) str)))))
           :eidx      (index-edges es)
           :sched-of  #(get by-tgt %)
           :providers (provider-sources schema)
           :run-by    (run-schedule-actions schema))))

(def freshness-rank
  {"up-to-date" 0 "read-time" 0 "ancestor" 1 "parent" 2 "logic" 3 "never-built" 4})

(def stale-words (disj (set (keys freshness-rank)) "up-to-date" "read-time"))

(def ^:private root-words #{"parent" "logic"})

(def ^:private buildable #{"transform" "dataset" "object-type" "link-type"})

(defn- logic-changed?
  [t spec-id ok]
  (if-let [built (some-> (:jobspec-id ok) str not-empty)]
    (not= built spec-id)
    (let [ran  (some-> (:code-sha ok) str not-empty)
          code (:code t)]
      (boolean (and ran (string? code) (seq code) (not= ran (digest/sha256-hex code)))))))

(defn- own-freshness
  [schema ctx tid t]
  (let [ok    (get-in ctx [:jobs :ok tid])
        ok-ms (some-> ok :ended-at ms)
        newer (vec (for [e (get-in ctx [:eidx :in tid])
                         :when (= :input (:rel e))
                         :let [c (changed-at ctx (:from e))]
                         :when (and c ok-ms (> c ok-ms))]
                     (:from e)))
        logic (and ok (logic-changed? t (get (:spec-ids ctx) tid (builds/jobspec-id schema t)) ok))]
    (cond-> {:freshness (cond (read-time? t) "read-time"
                              (nil? ok)   "never-built"
                              logic       "logic"
                              (seq newer) "parent"
                              :else       "up-to-date")}
      logic       (assoc :logic? true)
      (seq newer) (assoc :changed newer))))

(defn- worst
  [fs]
  (let [changed (vec (distinct (mapcat :changed fs)))]
    (cond-> (select-keys (apply max-key (comp freshness-rank :freshness) fs) [:freshness])
      (some :logic? fs) (assoc :logic? true)
      (seq changed)     (assoc :changed changed))))

(defn- producer-freshness
  [{:keys [eidx]} tf id]
  (some->> (get (:in eidx) id)
           (keep #(when (= :output (:rel %)) (get tf (:from %))))
           seq
           worst))

(defn- behind-of
  [{:keys [eidx]} roots]
  (let [flow (adjacency (filter #(io-rels (:rel %)) (mapcat val (:out eidx))) :from :to)]
    (reduce (fn [m r] (reduce #(update %1 %2 (fnil conj []) r) m (disj (walk flow [r] nil) r)))
            {} roots)))

(defn freshness
  [schema ns ctx]
  (let [tf     (into {} (for [[_ t] (:transforms schema)
                              :let [tid (node-id :transform (:transform-name t))]]
                          [tid (own-freshness schema ctx tid t)]))
        own    (into tf (for [id (keys ns)
                              :when (not (contains? tf id))
                              :let [f (producer-freshness ctx tf id)]
                              :when f]
                          [id f]))
        behind (behind-of ctx (sort (for [[id f] own :when (root-words (:freshness f))] id)))]
    (into {} (for [[id {k :kind}] ns
                   :when (buildable k)
                   :let [f (get own id) b (get behind id)]]
               [id (cond (and f (not= "up-to-date" (:freshness f))) f
                         (seq b) {:freshness "ancestor" :behind b}
                         :else   (or f {:freshness "up-to-date"}))]))))

(defn- compute-word [t] (or (some-> (:language t) nm) "mapping"))

(defn- write-mode-word
  [t]
  (let [i (:incremental t)]
    (if (= "replace" (some-> (if (map? i) (:write-mode i) :modify) nm)) "snapshot" "append")))

(defn- dataset-resource
  [d]
  (cond (vtypes/virtual? d)                 "virtual table"
        (:external-source d)                "synced"
        (= :object-backing (:kind d))       "object backing"
        :else                               "derived"))

(defn- transform-description
  [schema t]
  (let [c    (compute-word t)
        repo (some->> (:repository t) (get (:repositories schema)) :repository-name nm)]
    (cond-> {:resource (str "transform · " c) :compute c :write-mode (write-mode-word t)}
      repo (assoc :repository repo))))

(defn- dataset-description
  [d]
  {:resource (dataset-resource d)
   :storage  (if (vtypes/virtual? d) (str "virtual · " (nm (vtypes/source-name d))) "sutra")})

(defn- sync-names
  [schema]
  (let [idx (sch/index schema)
        ->n #(nm (or (sch/name-of idx %) %))]
    (reduce (fn [m [k x]]
              (cond-> m
                (:dataset x) (update (node-id :dataset (->n (:dataset x))) (fnil conj []) (->n k))))
            {} (:exports schema))))

(defn- backing-description
  [es]
  (let [bare #(second (str/split % #"/" 2))]
    (reduce (fn [m {:keys [from to]}]
              (-> m
                  (update-in [from :defines] (fnil conj []) (bare to))
                  (update-in [to :backed-by] (fnil conj []) (bare from))))
            {}
            (filter #(and (= :backs (:rel %)) (str/starts-with? (:from %) "dataset/")) es))))

(defn describe-nodes
  [schema es]
  (merge-with merge
              (into {} (for [[_ t] (:transforms schema)]
                         [(node-id :transform (:transform-name t))
                          (transform-description schema t)]))
              (into {} (for [[_ d] (:datasets schema)]
                         [(node-id :dataset (:dataset-name d)) (dataset-description d)]))
              (into {} (for [[_ o] (:object-types schema)]
                         [(node-id :object-type (:object-type-name o))
                          {:resource "object type"}]))
              (into {} (for [[k s] (:sources schema)]
                         [(node-id :source (or (:source-name s) k))
                          {:resource (str "source · " (or (some-> (:type s) nm) "unknown"))}]))
              (update-vals (sync-names schema) #(hash-map :sync-names %))
              (backing-description es)))

(def ^:private status-kind {"source" "datasource"})

(defn status-key-of
  [id]
  (let [[k n] (str/split id #"/" 2)] (str (get status-kind k k) "/" n)))

(defn- schedule-facts-of
  [ctx n]
  (when-let [ss (seq ((:sched-of ctx) (:name n)))]
    {:schedule-names   (mapv #(nm (:name %)) ss)
     :paused-schedules (count (filter :paused ss))}))

(def ^:private build-keys
  [:build-status :last-build :started-at :duration-ms :rows :rows-in :rows-out :run-id])

(defn- built-status
  [status backs id]
  (let [own (get status (status-key-of id))
        via (when (= "never" (:build-status own))
              (some (fn [p]
                      (let [b (get status (status-key-of p))]
                        (when (and (:build-status b) (not= "never" (:build-status b)))
                          (assoc (select-keys b build-keys) :build-via p))))
                    (sort (disj (walk backs [id] nil) id))))]
    (merge own via)))

(defn node-facts
  [schema ns es ctx status]
  (let [desc  (describe-nodes schema es)
        fresh (freshness schema ns ctx)
        backs (let [b (filter #(= :backs (:rel %)) es)]
                (merge-with into (adjacency b :from :to) (adjacency b :to :from)))]
    (into (sorted-map)
          (for [[id n] ns
                :let [f (get fresh id)]]
            [id (cond-> (merge n (built-status status backs id) (get desc id) f
                               (get (:jobspec-nodes ctx) id)
                               (schedule-facts-of ctx n))
                  f (assoc :out-of-date? (contains? stale-words (:freshness f)))
                  (= "read-time" (:freshness f)) (assoc :build-status "read-time"))]))))

(defn canvas-status
  [schema answers status]
  (let [es (edges schema (:automations answers))
        ns (nodes schema es)
        fx (node-facts schema ns es (fact-ctx schema es answers) (:nodes status))]
    (update status :nodes merge (into {} (for [[id f] fx] [(status-key-of id) f])))))

(defn- column-key
  [x]
  (-> (nm x) str/trim str/lower-case (str/replace "-" "_")))

(defn- column-names [spec k] (keep #(some-> (:attribute-name %) column-key) (get spec k)))

(def column-note
  "same name, or named in a transform's :column-map: not a trace through the transforms' code")

(def ^:private column-carriers
  [[:datasets :dataset :dataset-name :columns]
   [:object-types :object-type :object-type-name :attribute-list]])

(defn column-hits
  [schema column]
  (let [want (column-key column)]
    (vec (sort (concat
                (for [[section kind name-k cols-k] column-carriers
                      [_ spec] (get schema section)
                      :when (some #{want} (column-names spec cols-k))]
                  (node-id kind (get spec name-k)))
                (for [[_ t] (:transforms schema)
                      :when (some #(= want (column-key %)) (flatten (seq (:column-map t))))]
                  (node-id :transform (:transform-name t))))))))

(defn- day [s] (some-> s str not-empty (as-> v (subs v 0 (min 10 (count v))))))

(def facets
  {"kind"         :kind
   "project"      :project
   "build-status" #(or (:build-status %) "never")
   "freshness"    :freshness
   "health"       #(some-> % :health :status nm)
   "repository"   :repository
   "resource"     :resource
   "compute"      :compute
   "write-mode"   :write-mode
   "storage"      :storage
   "jobspec"      :jobspec
   "schedules"    :schedules
   "issues"       :issues
   "last-built"   (comp day :last-build)
   "defines"      :defines
   "syncs"        :sync-names})

(defn- facet-of
  [f]
  (or (get facets f)
      (refusal/refuse! :refusal.lineage/by-facet {:by f}
                       {:facets (str/join ", " (sort (keys facets)))})))

(defn- facet-values
  [v]
  (cond (sequential? v) (if (seq v) (map str v) ["—"])
        (nil? v)        ["—"]
        :else           [(str v)]))

(defn- buckets
  [nodes get-f]
  (->> (for [[id n] nodes v (facet-values (get-f n))] [v id])
       (group-by first)
       (map (fn [[v ps]] {:value v :count (count ps) :nodes (mapv second ps)}))
       (sort-by (juxt (comp - :count) :value))
       vec))

(defn histogram
  [nodes by]
  (into (sorted-map)
        (for [f (map str/trim (str/split (str by) #","))
              :when (seq f)]
          [f (buckets nodes (facet-of f))])))

(defn schedule-facts
  [names schedules runs]
  (vec (for [s schedules
             :let [ts (schedules/targets s)]
             :when (or (empty? ts) (some names ts))]
         (schedules/schedule-fact runs s))))

(defn- out-of-date-entry
  [[id f]]
  (cond-> {:node id :freshness (:freshness f)}
    (:logic? f)  (assoc :logic true)
    (:changed f) (assoc :changed (:changed f))
    (:behind f)  (assoc :behind (:behind f))))

(defn- anchors-of
  [schema ns graph-spec {:keys [node column project graph]}]
  (cond node    (resolve-nodes schema ns node)
        column  (or (not-empty (column-hits schema column))
                    (refusal/refuse! :refusal.lineage/no-column {:column column}))
        project (let [pname (get (project-names schema) (project-rid schema project))]
                  (sort (for [[id n] ns :when (= pname (:project n))] id)))
        graph   (sort (filter ns (graph-scope graph-spec)))))

(defn- scope-of
  [ns es io-only anchors {:keys [node up down]}]
  (if (nil? anchors)
    (set (keys ns))
    (let [walk-es (if io-only (filter #(io-rels (:rel %)) es) es)
          default (if (and node (nil? up) (nil? down)) 1 0)]
      (into (walk (adjacency walk-es :to :from) anchors (depth-of up default))
            (walk (adjacency walk-es :from :to) anchors (depth-of down default))))))

(defn- shortest-path
  [adj from to]
  (loop [frontier [from] parent {from nil}]
    (cond
      (contains? parent to) (vec (reverse (take-while some? (iterate parent to))))
      (empty? frontier)     nil
      :else
      (let [found (reduce (fn [m n]
                            (reduce #(if (contains? %1 %2) %1 (assoc %1 %2 n))
                                    m (sort (adj n))))
                          parent frontier)]
        (recur (vec (sort (remove #(contains? parent %) (keys found)))) found)))))

(defn- path-between
  [es from to]
  (let [adj  (adjacency (filter #(io-rels (:rel %)) es) :from :to)
        down (shortest-path adj from to)
        up   (when-not down (shortest-path adj to from))]
    (cond-> {:from from :to to :found? (boolean (or down up)) :nodes (vec (or down up []))}
      down (assoc :direction "downstream")
      up   (assoc :direction "upstream"))))

(defn- path-of
  [schema ns es anchors to]
  (let [target (resolve-nodes schema ns to)]
    (when-not (and (= 1 (count anchors)) (= 1 (count target)))
      (refusal/refuse! :refusal.lineage/to-two-nodes {:to to}))
    (path-between es (first anchors) (first target))))

(defn- beyond-scope
  [scope tfacts]
  (vec (for [{:keys [transform inputs outputs]} tfacts
             :let [in  (count (remove scope inputs))
                   out (count (remove scope outputs))]
             :when (pos? (+ in out))]
         {:transform (node-id :transform transform) :in in :out out})))

(defn- stale-ms-of
  [stale]
  (or (duration-ms stale)
      (refusal/refuse! :refusal.lineage/stale-duration {:stale stale})))

(defn- stale-entries
  [tfacts stale-ms]
  (vec (for [f tfacts
             :when (and (seq (:inputs-changed-since-success f))
                        (or (nil? (:since-success-ms f)) (> (:since-success-ms f) stale-ms)))]
         {:transform      (:transform f)
          :last-success   (get-in f [:last-success :at])
          :changed-inputs (:inputs-changed-since-success f)})))

(defn- diff-live-of
  [ns es saved]
  (let [cut (filter #(not= (boolean (saved (:from %))) (boolean (saved (:to %)))) es)]
    {:gone            (vec (sort (remove (set (keys ns)) saved)))
     :live-not-shown  (vec (sort (distinct (for [e cut]
                                             (if (saved (:from e)) (:to e) (:from e))))))
     :edges-not-shown (vec cut)}))

(defn facts
  [schema {:keys [automations graph-spec] :as answers}
   {:keys [project graph dead stale blocked diff-live io-only out-of-date by schedules to column
           modified]
    :as q}]
  (let [es      (edges schema automations)
        ns      (nodes schema es)
        anchors (anchors-of schema ns graph-spec q)
        path    (when to (path-of schema ns es anchors to))
        scope   (if path
                  (set (or (not-empty (:nodes path)) [(:from path) (:to path)]))
                  (scope-of ns es io-only anchors q))
        ctx     (fact-ctx schema es answers)
        nfacts  (when-not io-only
                  (select-keys (node-facts schema ns es ctx (force (:status answers))) scope))
        tfacts  (vec (for [[_ t] (:transforms schema)
                           :let [id (node-id :transform (:transform-name t))]
                           :when (scope id)]
                       (cond-> (transform-facts schema ctx id t)
                         nfacts (assoc :freshness (get-in nfacts [id :freshness])))))
        tfacts  (vec (sort-by :transform tfacts))
        steps    (when path (set (map vec (partition 2 1 (:nodes path)))))
        in-scope (filter (if path
                           #(and (io-rels (:rel %)) (steps [(:from %) (:to %)]))
                           #(and (scope (:from %)) (scope (:to %))))
                         es)]
    (cond-> {:anchors    (vec anchors)
             :nodes      (or nfacts (into (sorted-map) (select-keys ns scope)))
             :edges      (vec in-scope)
             :transforms tfacts
             :beyond     (beyond-scope scope tfacts)}
      path        (assoc :path path)
      column      (assoc :column-note column-note)
      project     (assoc :project (get (project-names schema) (project-rid schema project)))
      dead        (assoc :dead (vec (for [f tfacts :when (:dead? f)]
                                      {:transform (:transform f) :outputs (:outputs f)}))
                         :no-outputs (vec (for [f tfacts :when (:no-outputs? f)] (:transform f)))
                         :coverage coverage)
      modified    (assoc :modified (filterv ns (force (:modified answers))))
      stale       (assoc :stale (stale-entries tfacts (stale-ms-of stale)))
      blocked     (assoc :blocked
                         (let [referrers (retirement/deletion-referrers schema)]
                           (vec (keep #(blocked-node schema (:placements ctx) referrers %)
                                      (sort scope)))))
      (and diff-live graph) (assoc :diff-live (diff-live-of ns es (graph-scope graph-spec)))
      (and out-of-date nfacts)
      (assoc :out-of-date (->> nfacts
                               (filter (comp :out-of-date? val))
                               (sort-by (juxt (comp - freshness-rank :freshness val) key))
                               (mapv out-of-date-entry)))
      (and by nfacts) (assoc :histogram (histogram nfacts by))
      schedules   (assoc :schedules
                         (schedule-facts (into #{} (map #(second (str/split % #"/" 2))) scope)
                                         (:schedules answers) (force (:runs answers)))))))
