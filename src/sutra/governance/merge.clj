(ns sutra.governance.merge
  (:require [clojure.string :as str]
            [sutra.service.merge :as dmerge]
            [sutra.service.access :as zugang]
            [sutra.world.policy :as policy]
            [sutra.world.db :as kdb]
            [sutra.world.delta :as delta]
            [sutra.commits.branches :as zweige]
            [sutra.store.control :as ctl]
            [sutra.store.kept :as kept]
            [sutra.store.access.datasets :as dsets]
            [sutra.catalog.system-datasets :as sysds]
            [sutra.catalog.world-gates :as world-gates]
            [sutra.catalog.shape :as shape]
            [sutra.llm.api :as llm]
            [sutra.store.lib :as lib]
            [sutra.store.world :as world]
            [sutra.types.entities :as ents]
            [sutra.parser.checkout :as checkout]
            [sutra.types.review :as review]
            [sutra.types.authority :as authority]
            [sutra.types.ontologies :as ontologies]
            [sutra.catalog.grants :as grants]
            [sutra.types.rid :as rid]
            [sutra.types.shape :as tshape]
            [sutra.types.tree :as tree]
            [sutra.governance.merge-plan :as mp]))

(set! *warn-on-reflection* true)

(defn- sections [w] (select-keys w ents/sections))

(defn resolver
  [resolutions]
  (let [r (into {}
                (for [[k side] resolutions
                      :let [[c n] (if (string? k) (str/split k #"/" 2) k)
                            side  (some-> side rid/->name keyword)]
                      :when (#{:mine :theirs} side)]
                  [[(keyword (rid/->name c)) (rid/->name n)] side]))]
    (fn [node] (get r node))))

(def ^:private merge-opts {:cut tree/lists-of-maps :fold tshape/fold :first :theirs})

(defn- checked
  [conflicts held mine-diff]
  (let [clashes (vec (for [[[class :as n] cs] (sort-by (comp str key) (group-by :node conflicts))
                           :let [fields (vec (sort (keep (comp tree/label :field) cs)))]]
                       (cond-> (assoc (held n) :class class)
                         (seq fields) (assoc :fields fields))))]
    {:status    (if (seq clashes) :conflicts :clean)
     :conflicts clashes
     :mine      mine-diff}))

(defn- at-nodes
  [db nodes]
  (reduce (fn [W n] (if-some [e (kdb/entry db n)] (assoc-in W n e) W)) {} nodes))

(defn- heads-check
  [{:keys [base mine theirs nodes]}]
  (let [{:keys [conflicts]} (dmerge/merge3 base mine theirs nodes merge-opts)
        nodes   (remove delta/frame-node? nodes)
        node-of (into {} (map (fn [[s k :as n]] [[s (rid/->name k)] n])) nodes)
        held    (fn [n]
                  (let [[_ k :as node] (node-of n)
                        side #(if-some [e (kdb/entry % node)] e :removed)]
                    {:key k :mine (side mine) :theirs (side theirs)}))]
    (checked conflicts held (lib/diff-paths (at-nodes base nodes) (at-nodes mine nodes)))))

(defn heads-merge
  [{:keys [base mine theirs nodes onto]} resolutions]
  (let [{:keys [changes]} (dmerge/merge3 base mine theirs nodes
                                         (assoc merge-opts :resolve (resolver resolutions)))]
    {:before theirs :after (kdb/db-with theirs changes) :onto onto}))

(defn rebase-schema
  [{:keys [before after onto]}]
  (kept/made-by (kdb/world after) onto before after))

(defn- branch-states
  [store parent staging rev]
  (kept/at-head store parent nil
                #(assoc (zweige/states %1 %2 (str staging) (str parent) rev world-gates/views
                                       (kept/home store staging))
                        :onto (str parent))))

(defn- heads-worlds
  [{:keys [base mine theirs apart]}]
  (let [t (kdb/world theirs)]
    {:base         (delta/apply-to t (kept/restore-at theirs base apart))
     :mine         (or (not-empty (kdb/world mine)) shape/empty-schema-map)
     :theirs       (or (not-empty t) shape/empty-schema-map)
     :parent-world t}))

(defn staging-merge-check
  ([store parent staging]
   (staging-merge-check store parent staging (:base (world/branch store staging))))
  ([store parent staging {:keys [branch rev] :as fork-base}]
   (when-not (and branch rev (= branch parent))
     (throw (ex-info (str staging " was not forked from " parent)
                     {:precondition :no-fork-from-parent :fork-base fork-base
                      :parent (str parent)})))
   (let [heads (branch-states store parent staging rev)
         {:keys [base mine theirs parent-world]} (heads-worlds heads)]
     (assoc (heads-check heads)
            :base base :mine-schema mine :theirs-schema theirs :parent-world parent-world
            :heads heads))))

(def ^:private change-value-max 400)

(defn- show-value
  [v]
  (when (some? v)
    (let [s (pr-str v)]
      (if (> (count s) change-value-max) (str (subs s 0 change-value-max) " …") s))))

(defn field-changes
  [before after]
  (let [b (tree/elements tree/lists-of-maps nil (or before {}))
        a (tree/elements tree/lists-of-maps nil (or after {}))]
    (vec (for [[l e] (sort-by first (for [e (distinct (concat (keys b) (keys a)))
                                          :let [l (tree/label e)]
                                          :when (and l (not (and (vector? e) (nil? (second e)))))]
                                      [l e]))
               :when (not= (get b e) (get a e))]
           {:field l :before (show-value (get b e)) :after (show-value (get a e))}))))

(defn- touched-keys
  [d]
  (into #{} (for [[_ {:keys [added removed changed]}] d m [added removed changed] k (keys m)] k)))

(defn- widening
  [delta touched]
  (let [{:keys [subjects unattributed?]} (authority/expanding delta)
        orphan? (or unattributed? (boolean (some (complement touched) subjects)))]
    (fn [class rid]
      (or (contains? subjects rid)
          (and orphan? (contains? authority/access-sections class))))))

(defn- declared-lifts
  [base after ts org]
  (let [added (fn [spec old field a]
                (remove (set (get-in old [a field])) (get-in spec [:inputs a field])))]
    (into {}
          (for [t ts
                :let [spec (get-in after [:transforms t])
                      old  (get-in base [:transforms t :inputs])
                      as   (keys (:inputs spec))
                      ms   (into #{} (for [a as m (added spec old :stop-propagating a)]
                                       (keyword (name m))))
                      os   (into #{} (for [a as o (added spec old :stop-requiring a)] (org o)))]
                :when (or (seq ms) (seq os))]
            [t (cond-> {} (seq ms) (assoc :markings ms) (seq os) (assoc :organizations os))]))))

(defn- lifts [lifted declared] (merge-with #(merge-with into %1 %2) lifted declared))

(defn- world-access
  [d base after]
  (let [iks   (ents/union-keys (ents/instance-keys (or base {})) (ents/instance-keys (or after {})))
        org   (fn [o] (or (some (fn [[k v]] (when (or (= (str k) (str o))
                                                       (= (some-> (:organization-name v) name)
                                                          (name o)))
                                              k))
                                (:organizations after))
                          o))
        delta (when (and base after) (grants/access-delta base after))]
    {:base      base
     :after     after
     :widens?   (when delta (widening delta (touched-keys d)))
     :lifted    (lifts (when delta (authority/lifted delta (grants/schema-policy base)))
                       (declared-lifts base after (keys (:transforms after)) org))
     :applied   (if delta
                  (authority/applied (grants/schema-policy base) (grants/schema-policy after))
                  {})
     :instance? (fn [class k] (let [ik (get iks class)] (or (= :all ik) (contains? ik k))))
     :scope-of  #(ontologies/ontology-of (or after base) %)}))

(defn- heads-access
  [d {:keys [db-before db-after] :as step}]
  (let [base   (kdb/world db-before)
        after  (kdb/world db-after)
        moved  (zugang/access-delta step (grants/roles base) (grants/roles after))
        ts     (mapcat #(keys (get-in d [:transforms %])) [:added :changed])
        org    (fn [o] (or (policy/resolve-in db-after :organizations o) o))
        inst?  (fn [db class k] (or (ents/instance-entry? class (kdb/entry db [class k]))
                                    (seq (kdb/nodes db :instance [class k]))))]
    {:base      base
     :after     after
     :widens?   (widening moved (touched-keys d))
     :lifted    (lifts (zugang/lifted moved db-before) (declared-lifts base after ts org))
     :applied   (zugang/applied step)
     :instance? (fn [class k] (boolean (or (ents/instance-section? class)
                                           (inst? db-before class k)
                                           (inst? db-after class k))))
     :scope-of  #(or (kdb/scope-of db-after %) ontologies/default-ontology)}))

(defn- access-gates
  [base {:keys [markings organizations]}]
  (vec (concat
        (for [m (sort-by str markings)]
          {:gate    (str "marking/" (name m))
           :holders (vec (get-in base [:markings (keyword (name m)) :remove-access]))})
        (for [o (sort-by str organizations)]
          {:gate    (str "organization/" o)
           :holders (vec (get-in base [:organizations o :expand-access]))}))))

(defn- apply-gates
  [base after orgs]
  (vec (for [o (sort-by str orgs)]
         {:gate    (str "apply-organization/" o)
          :holders (vec (or (get-in base [:organizations o :apply])
                            (get-in after [:organizations o :apply])))})))

(defn- ontology-field
  [{:keys [base after scope-of]} class spec]
  (when-let [schema (or after base)]
    (when (and spec (some #{class} ontologies/sections))
      (let [s (scope-of spec)]
        {:ontology (str s) :ontology-name (ontologies/ontology-name schema s)}))))

(defn- entity-diff->rows
  [d {:keys [base widens? lifted applied instance?] :as access}
   {:keys [changes?] :or {changes? true}}]
  (let [after-w (:after access)]
    (into []
          (for [[class {:keys [added removed changed]}] d
                [status m] [["added" added] ["modified" changed] ["removed" removed]]
                [k v] m
                :let [before (when-not (= status "added") (get-in base [class k]))
                      after  (when-not (= status "removed") v)]]
            (cond-> {:table (str (name class) "/" (name k)) :staged true :status status
                     :rid   (str k) :section (name class)
                     :name  (some-> (get v (rid/name-field-of class)) name)
                     :project (if (ents/in-role? :container class)
                                (str k)
                                (some-> (:project v) name))
                     :fingerprint (checkout/fingerprint [status v])
                     :kind (review/change-kind status before after)
                     :expands-access (boolean (or (review/expands-access? status before after)
                                                  (and widens? (widens? class k))
                                                  (seq (get lifted k))))}
              (or (seq (get lifted k)) (review/expands-access? status before after))
              (assoc :access-gates
                     (access-gates base
                                   (update (get lifted k) :markings (fnil into #{})
                                           (remove (set (:markings after))
                                                   (:markings before)))))
              (seq (get applied k))
              (assoc :apply-gates (apply-gates base after-w (get applied k)))
              true (merge (ontology-field access class (or after before)))
              (and before after (not= (:project before) (:project after)) (:project before))
              (assoc :moved-from (name (:project before)))
              (some #(get-in % [:board :security-approvals]) [before after])
              (assoc :security-approvals true)
              changes? (assoc :changes
                              (field-changes (or before (when (= status "removed") v)) after))
              (instance? class k) (assoc :instance true))))))

(defn touched-rows
  [before after]
  (let [d (lib/diff-paths before after)]
    (entity-diff->rows d (world-access d before after) nil)))

(defn- heads-step
  [{:keys [base mine apart]}]
  (let [back (kdb/with mine (kept/restore-at mine base apart))]
    {:db-before (:db-after back) :db-after mine :delta (delta/invert (:delta back))}))

(defn- diff-rows
  [{:keys [mine heads]} opts]
  (entity-diff->rows mine (heads-access mine (heads-step heads)) opts))

(defn preview-of
  ([check] (preview-of check nil))
  ([{:keys [status conflicts] :as check} opts]
   {:status status :diff (diff-rows check opts) :conflicts conflicts}))

(defn commit-preview
  [store {:keys [parent staging live? base]}]
  (if-not live?
    {:status :empty :diff [] :conflicts []}
    (preview-of (staging-merge-check store parent staging base))))

(defn- worded
  [{:keys [ok reasons] :as v}]
  (if ok
    v
    (-> v
        (dissoc :reasons)
        (assoc :error (or reasons
                          ["schema failed validation without an explanation — it is most "
                           "likely structurally incomplete (a missing entity section, not "
                           "a bad entity)"])))))

(defn judged
  [{:keys [before after]}]
  (worded (world-gates/verdict before after)))

(defn authored-target
  [{:keys [heads]}]
  (world/without-retired (kdb/world (:theirs heads))))

(def ^:private secret-fields #{:secrets :secret-cipher})

(defn- side-value
  [field v]
  (cond (= :removed v)                              {:removed true}
        (nil? v)                                    {:absent true}
        (secret-fields (keyword field))             {:secret true}
        (map? v)
        {:value (show-value (apply dissoc v secret-fields))}
        :else                                       {:value (show-value v)}))

(defn- conflict-row
  [base {:keys [class key fields mine theirs]}]
  (let [nf   (rid/name-field-of class)
        b    (get-in base [class key])
        nm   (some (fn [e] (when (map? e) (some-> (get e nf) rid/->name))) [mine theirs b])
        top  (fn [f] (keyword (first (str/split f #"/"))))]
    {:table         (str (name class) "/" (name key))
     :section       (name class)
     :rid           (rid/->name key)
     :name          nm
     :num-conflicts 1
     :mine          (when (= :removed mine) "removed")
     :theirs        (when (= :removed theirs) "removed")
     :fields        (vec (for [f (or (seq fields) [nil])
                               :let [k (some-> f top)]]
                           {:field  f
                            :base   (if k (side-value k (get b k)) (side-value nil b))
                            :mine   (if (and k (map? mine)) (side-value k (get mine k))
                                        (side-value nil mine))
                            :theirs (if (and k (map? theirs)) (side-value k (get theirs k))
                                        (side-value nil theirs))}))}))

(defn branch-merge-preview
  [row check {:keys [changes?] :or {changes? true} :as opts}]
  (cond
    (nil? row)         {:status :gone    :diff [] :conflicts []}
    (nil? (:base row)) {:status :no-base :diff [] :conflicts []}
    :else
    (update (preview-of check opts) :conflicts
            (fn [cs] (if changes?
                       (mapv #(conflict-row (:base check) %) cs)
                       (mapv #(select-keys % [:class :key]) cs))))))

(defn preview-branch-schema-merge
  ([store source-name target-name] (preview-branch-schema-merge store source-name target-name nil))
  ([store source-name target-name opts]
   (let [row (world/branch store source-name)]
     (branch-merge-preview row
                           (when (:base row)
                             (staging-merge-check store target-name source-name (:base row)))
                           opts))))

(defn preview-tasks
  [preview]
  (->> (:diff preview)
       (mapv (fn [r]
               (cond-> {:task (:table r) :section (:section r) :rid (:rid r) :name (:name r)
                        :project (:project r)
                        :status (:status r) :kind (:kind r) :fingerprint (:fingerprint r)}
                 (:expands-access r) (assoc :expands-access true)
                 (seq (:access-gates r)) (assoc :access-gates (:access-gates r))
                 (seq (:apply-gates r)) (assoc :apply-gates (:apply-gates r))
                 (:ontology r) (merge (select-keys r [:ontology :ontology-name]))
                 (seq (:changes r)) (assoc :changes (:changes r)))))))

(defn proposal-tasks
  ([store source target] (proposal-tasks store source target nil))
  ([store source target opts]
   (preview-tasks (preview-branch-schema-merge store source target opts))))

(defn head-of
  [store]
  (let [unviewed-store (assoc store :view nil)]
    {:world (or (world/authored unviewed-store) {})
     :at    (some-> (first (world/history unviewed-store {:schema? true :limit 1})) :at
                    (as-> d (if (instance? java.util.Date d) (.toInstant ^java.util.Date d) d)))}))

(defn- kept-links
  [store l]
  (into {} (map (juxt :link-type :fingerprint))
        (ctl/list store :link-backings (cond-> {:branch (str (:branch store))}
                                         l (assoc :link-type l)))))

(defn- kept-versions
  [store t]
  (into {} (map (juxt :transform :semantic-version))
        (ctl/list store :transform-versions (cond-> {:branch (str (:branch store))}
                                              t (assoc :transform t)))))

(defn- keep-link! [store l fp]
  (ctl/upsert! store :link-backings
               {:branch (str (:branch store)) :link-type (mp/link-key l) :fingerprint fp}))

(defn- keep-version! [store t v]
  (ctl/upsert! store :transform-versions
               {:branch (str (:branch store)) :transform (mp/transform-key t)
                :semantic-version v}))

(defn after-commit-reads
  [store]
  (let [{:keys [world at]} (head-of store)]
    {:head          world
     :at            at
     :kept-links    (kept-links store nil)
     :kept-versions (kept-versions store nil)}))

(defn- reregister!
  [store reregister-link! l owed]
  (let [now    (:world (head-of store))
        now-fp (get (mp/link-backing-fingerprints now) l)
        k      (mp/link-key l)]
    (when (and now-fp (not= now-fp (get (kept-links store k) k)))
      (when reregister-link! (reregister-link! store now l owed))
      (keep-link! store l now-fp))))

(defn- start-over!
  [store reset-consumer! t outs]
  (let [k     (mp/transform-key t)
        spec  (get-in (:world (head-of store)) [:transforms t])
        v     (get spec :semantic-version 1)]
    (when (and spec (integer? v) (not= v (get (kept-versions store k) k)))
      (when reset-consumer! (reset-consumer! store t))
      (dsets/ensure-datasets! store
                              (into {}
                                    (for [[rid ds] outs
                                          :when (not (contains? sysds/system-datasets (str rid)))
                                          :when (not= :object-backing (keyword (:kind ds)))]
                                      [rid (:columns ds)])))
      (doseq [[dsr _] outs]
        (dsets/truncate! store dsr))
      (keep-version! store t v))))

(defn- run-job!
  [store {:keys [reregister-link! reset-consumer! guard-reset!]} seen [op x _ arg]]
  (let [guard (or guard-reset! (fn [store _ f] (f store)))]
    (case op
      :reregister      (guard store [(str x)] #(reregister! % reregister-link! x arg))
      :reset            (guard store (vec (keys arg)) #(start-over! % reset-consumer! x arg))
      :record-llm-rates (llm/record-llm-rates! store (:head seen) (:at seen)))))

(defn reconcile-after-commit!
  ([store callbacks base] (reconcile-after-commit! store callbacks base
                                                   (after-commit-reads store)))
  ([store callbacks base seen]
   (let [jobs  (mp/after-commit seen base)
         keeps (group-by first (filter (comp #{:keep-link :keep-version} first) jobs))
         b     (str (:branch store))]
     (when-let [ls (seq (:keep-link keeps))]
       (ctl/insert-many! store :link-backings
                         (for [[_ l fp] ls]
                           {:branch b :link-type (mp/link-key l) :fingerprint fp})
                         {:upsert? true}))
     (when-let [vs (seq (:keep-version keeps))]
       (ctl/insert-many! store :transform-versions
                         (for [[_ t v] vs]
                           {:branch b :transform (mp/transform-key t) :semantic-version v})
                         {:upsert? true}))
     (run! #(run-job! store callbacks seen %)
           (remove (comp #{:keep-link :keep-version} first) jobs)))))
