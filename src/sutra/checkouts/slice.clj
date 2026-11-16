(ns sutra.checkouts.slice
  (:require [clojure.edn :as edn]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [sutra.authoring.api :as authoring]
            [sutra.branching.api :as branching]
            [sutra.kit.refusal :as refusal]
            [sutra.lineage.api :as lineage]
            [sutra.parser.project-export :as pe]
            [sutra.parser.project-emit :as pemit]
            [sutra.parser.projection :as proj]
            [sutra.repos.api :as repos]
            [sutra.store.access.scope :as scope]
            [sutra.governance.api :as governance]
            [sutra.store.seat :as seat]
            [sutra.store.world :as world]
            [sutra.types.rid :as rid])
  (:import (java.io ByteArrayInputStream)
           (org.eclipse.jgit.ignore IgnoreNode IgnoreNode$MatchResult)))

(set! *warn-on-reflection* true)

(def ^:private projection-order
  [(pe/git-projections :repo) (pe/git-projections :ontology)])

(defn- read-text [text] (edn/read-string {:default (fn [_ v] v)} text))

(defn- print-text [form] (with-out-str (pprint/pprint form)))

(defn- link-name [l] (rid/->name (if (map? l) (or (:name l) (:link-name l)) (first l))))

(defn- sectioned? [form] (map? (:schema form)))

(defn cut-text
  [text keep?]
  (let [form (read-text text)
        kept (fn [section v]
               (cond
                 (map? v)        (into (empty v)
                                       (filter #(keep? [section (rid/->name (key %))]))
                                       v)
                 (sequential? v) (filterv #(keep? [section (link-name %)]) v)))]
    (if-not (sectioned? form)
      text
      (print-text
       (assoc form :schema
              (into {}
                    (keep (fn [[s v]] (let [m (kept s v)] (when (seq m) [s m]))))
                    (:schema form)))))))

(defn splice-text
  [full mine held]
  (let [form  (if mine (read-text mine) {:schema {}})
        whole (read-text full)]
    (if-not (and (sectioned? form) (sectioned? whole))
      mine
      (let [held?   (set held)
            old     (:schema whole)
            spliced (fn [section]
                      (let [v (get old section)
                            m (get-in form [:schema section])]
                        (if (or (sequential? v) (sequential? m))
                          (into (vec (remove #(held? [section (link-name %)]) v)) m)
                          (merge (into {} (remove #(held? [section (rid/->name (key %))]) v))
                                 m))))]
        (print-text
         (assoc whole :schema
                (into {}
                      (keep (fn [s] (let [v (spliced s)] (when (seq v) [s v]))))
                      (distinct (concat (keys old) (keys (:schema form)))))))))))

(defn part-of
  [{:keys [files base shape]} in?]
  (let [held (into {}
                   (keep (fn [[path nodes]]
                           (let [mine (into {} (filter (comp in? key)) nodes)]
                             (when (seq mine) [path mine]))))
                   base)]
    (when (seq held)
      {:shape shape
       :base  held
       :files (into {} (for [path (keys held)] [path (cut-text (get files path) in?)]))})))

(defn full-texts
  [now files base]
  (into {}
        (for [[path text] files
              :let [path (rid/->name path)
                    full (get now path)]]
          [path (if full (splice-text full text (keys (get base path))) text)])))

(defn code-files
  [schema ids]
  (let [known (lineage/named-nodes schema)
        named (fn [section path-of]
                (for [[k spec] (get schema section)
                      :let [path (path-of spec)]
                      :when (and path (:repository spec))
                      :let [id (or (lineage/known-node known :functions (:function-name spec))
                                   (lineage/known-node known section (or (:transform-name spec) k)))]
                      :when (contains? ids id)]
                  [(:repository spec) path]))]
    (vec (distinct (concat (named :transforms pemit/code-path)
                           (named :functions :code-file))))))

(defn ignored-by
  [gitignore]
  (if (str/blank? gitignore)
    (constantly false)
    (let [node (doto (IgnoreNode.)
                 (.parse (ByteArrayInputStream. (.getBytes ^String gitignore "UTF-8"))))
          hit? (fn [^String p dir?] (= IgnoreNode$MatchResult/IGNORED (.isIgnored node p dir?)))]
      (fn [^String path]
        (let [parts (str/split path #"/")]
          (or (hit? path false)
              (some #(hit? (str/join "/" (take % parts)) true) (range 1 (count parts)))))))))

(defn code-changes
  [schema tree-of text-of code resolve]
  (vec
   (for [[repo files] code
         :let [r (or (repos/repository-rid schema repo)
                     (refusal/refuse! :refusal.checkouts/no-repository {}
                                      {:repo (rid/->name repo)}))]
         :let [ignored? (ignored-by (some->> (get-in (tree-of r) [".gitignore" :blob])
                                             (text-of r)))]
         [path {:keys [text blob]}] files
         :let [path   (rid/->name path)
               tip    (:blob (get (tree-of r) path))
               moved? (not= tip blob)]
         :when (not (ignored? path))
         :when (not (and moved? (= text (when tip (text-of r tip)))))
         :when (not (and moved? (= :theirs resolve)))]
     {:repo (rid/->name repo) :rid r :path path :text text
      :conflict? (and moved? (not= :mine resolve))})))

(defn- refuse-conflicts!
  [changes]
  (when-let [c (seq (filter :conflict? changes))]
    (refusal/refuse! :refusal.checkouts/code-changed-both
                     {:kind ::code-conflict
                      :conflicts (mapv #(select-keys % [:repo :path]) c)}
                     {:paths (str/join ", " (map #(str (:repo %) "/" (:path %)) c))})))

(defn imports-in
  [text tree]
  (into []
        (comp (map (fn [[_ a b]] (or a b)))
              (map #(str "transforms/" % ".py"))
              (filter #(contains? tree %)))
        (re-seq #"(?m)^\s*(?:from\s+(?:transforms\.)?(\w+)\s+import|import\s+(\w+))" (str text))))

(defn with-defined
  [schema absorbed]
  (reduce (fn [s [section name-field]]
            (reduce-kv (fn [s k spec] (assoc-in s [section k] (assoc spec name-field k)))
                       s (get absorbed section)))
          schema
          [[:functions :function-name] [:transforms :transform-name]]))

(defn neighbours
  [es ids]
  {:inputs (vec (sort (distinct (for [{:keys [from to]} es
                                      :when (and (ids to) (not (ids from)))]
                                  from))))
   :feeds  (vec (sort (distinct (for [{:keys [from to]} es
                                      :when (and (ids from) (not (ids to)))]
                                  to))))})

(defn- tree-at
  [r branch]
  (when-let [sha (some #(some->> (repos/branch->ref %) (repos/ref-sha (scope/repos) r))
                       (distinct [(str branch) pe/trunk-branch]))]
    (repos/read-tree (scope/repos) r sha)))

(defn- read-code
  [{:keys [schema branch auth]} files]
  (let [tree (memoize #(tree-at % (or branch pe/trunk-branch)))]
    (loop [todo (seq files) acc {}]
      (if-let [[r path] (first todo)]
        (let [repo (rid/->name (get-in schema [:repositories r :repository-name]))
              blob (when (and (repos/repo-readable? auth schema r)
                              (not (get-in acc [repo path])))
                     (:blob (get (tree r) path)))]
          (if blob
            (let [text (String. ^bytes (repos/blob-bytes (scope/repos) r blob) "UTF-8")]
              (recur (concat (rest todo) (map (fn [p] [r p]) (imports-in text (tree r))))
                     (assoc-in acc [repo path] {:text text :blob blob})))
            (recur (rest todo) acc)))
        acc))))

(defn- view-of [{:keys [schema] :as ctx} nm]
  (when (proj/declared schema nm)
    (proj/view schema nm (authoring/projection-memory ctx nm) nil)))

(defn pull
  [{:keys [schema] :as ctx} handles]
  (let [es    (lineage/edges schema nil)
        ids   (set (lineage/resolve-nodes schema (lineage/nodes schema es) handles))
        id-of (lineage/node-id-of schema)
        in?   (comp ids id-of)
        parts (into {}
                    (keep (fn [nm] (when-let [p (some-> (view-of ctx nm) (part-of in?))] [nm p])))
                    projection-order)
        bases (map :base (vals parts))
        held  (into #{} (comp (mapcat vals) (mapcat keys) (map id-of)) bases)]
    (merge
     {:nodes       (vec (sort ids))
      :projections parts
      :code        (read-code ctx (code-files schema ids))
      :around      (authoring/inputs-around ctx (apply merge bases))
      :not-held    (vec (sort (remove held ids)))}
     (neighbours es ids))))

(defn- projection-body
  [{:keys [schema] :as ctx} nm {:keys [files base shape]} opts]
  (let [base (update-keys base rid/->name)
        now  (proj/render schema nm (authoring/projection-memory ctx nm))]
    (merge opts {:files (full-texts now files base) :base base :shape shape})))

(defn- pushed-world
  [{:keys [store branch schema] :as ctx} nm body preview?]
  (if preview?
    (with-defined schema (:schema (authoring/projection-push-plan ctx nm (:files body) body)))
    (seat/branch-schema (world/at store branch))))

(defn- push-projections!
  [ctx projections {:keys [preview?] :as opts} around]
  (let [sent (update-keys projections rid/->name)]
    (reduce (fn [[acc ctx] nm]
              (if-let [part (get sent (rid/->name nm))]
                (let [body (projection-body ctx nm part
                                            (cond-> opts (empty? acc) (assoc :around around)))]
                  [(assoc acc nm (authoring/projection-push! ctx nm body))
                   (assoc ctx :schema (pushed-world ctx nm body preview?))])
                [acc ctx]))
            [{} ctx] projection-order)))

(defn ensure-proposal!
  [{:keys [store schema branch admin?]} message]
  (when-not (governance/changeset-offen? store branch)
    (when (governance/auto-land? store pe/trunk-branch)
      (refusal/refuse! :refusal.checkouts/trunk-needs-proposal {} {:trunk pe/trunk-branch}))
    (branching/propose! store schema
                        {:source (str branch) :target pe/trunk-branch :title message}
                        admin?)))

(defn commit-code!
  [{:keys [store branch author]} changes message]
  (reduce
   (fn [done [[repo r] cs]]
     (try
       (repos/project-branch! store (scope/repos) (scope/blobs) r (str branch))
       (conj done
             (assoc (select-keys
                     (branching/commit-files! store (seat/branch-schema (world/at store branch))
                                              r (str branch)
                                              {:files   (into {} (map (juxt :path :text)) cs)
                                               :message message
                                               :author  author})
                     [:commit :lines])
                    :repo repo))
       (catch clojure.lang.ExceptionInfo e
         (throw (ex-info (str repo ": " (ex-message e)
                              (when (seq done)
                                (str " · already committed: "
                                     (str/join ", " (map :repo done))
                                     "; the same push can be sent again")))
                         (ex-data e) e)))))
   []
   (group-by (juxt :repo :rid) changes)))

(defn- code-refusals
  [{:keys [store schema branch author]} changes message]
  (vec
   (for [[[repo r] cs] (group-by (juxt :repo :rid) changes)
         :let [why (branching/check-files store schema r (str branch)
                                          {:files   (into {} (map (juxt :path :text)) cs)
                                           :message message
                                           :author  author})]
         :when why]
     {:repo repo :why why})))

(defn push!
  [{:keys [schema branch] :as ctx} {:keys [projections code preview? around message resolve]}]
  (when (and (not preview?) (= (str (or branch pe/trunk-branch)) pe/trunk-branch))
    (refusal/refuse! :refusal.checkouts/slice-needs-branch {}))
  (let [resolve (some-> resolve keyword #{:mine :theirs})
        changes (code-changes schema (memoize #(tree-at % branch))
                              (fn [r blob]
                                (String. ^bytes (repos/blob-bytes (scope/repos) r blob) "UTF-8"))
                              code resolve)
        _       (refuse-conflicts! changes)
        message (or (not-empty (str message)) "slice")
        [results after]
        (push-projections! ctx projections
                           (cond-> {:preview? (boolean preview?) :resolve resolve
                                    :sent-code code}
                             (not preview?) (assoc :land? true))
                           around)]
    (when-not preview?
      (when-let [refused (some (comp :land-refused val) results)]
        (refusal/refuse! :refusal.checkouts/texts-staged {} {:refused refused})))
    (cond-> {:projections results
             :code        (mapv #(select-keys % [:repo :path]) changes)}
      (and (seq changes) (empty? results) around)
      (assoc :inputs-changed (authoring/inputs-changed ctx around))
      (and preview? (seq changes))
      (assoc :code-refused (code-refusals after changes message))
      (and (not preview?) (seq changes))
      (assoc :proposal (some-> (ensure-proposal! ctx message) (select-keys [:id]))
             :commits  (commit-code! ctx changes message)))))
