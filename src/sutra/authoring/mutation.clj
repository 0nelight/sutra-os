(ns sutra.authoring.mutation
  (:require [sutra.kit.refusal :as refusal]
            [sutra.foundation.ops :as op]
            [sutra.store.access.scope :as scope]
            [clojure.string :as str]
            [sutra.authoring.install.world :as onto-install]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.project-scope :as project-scope]
            [sutra.catalog.retirement :as retirement]
            [sutra.catalog.lookup :as lookup]
            [sutra.parser.project-emit :as pemit]
            [sutra.catalog.kernel :as kernel]
            [sutra.types.rid :as rid]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.governance.api :as governance]
            [sutra.store.protection :as protection]
            [sutra.types.entities :as ents]
            [sutra.store.world :as world]
            [sutra.store.seat :as seat]
            [sutra.authoring.install.live :as live]
            [sutra.store.kept :as kept]
            [sutra.capps.catalog :as cat]
            [sutra.ref.install :as ri]))

(set! *warn-on-reflection* true)

(defn- refuse-if-degraded!
  [store]
  (when-let [e (seat/schema-load-error)]
    (when (= (str (:branch e)) (str (:branch store)))
      (refusal/refuse! :refusal.authoring/branch-degraded
                       {:error :degraded :branch (:branch store)} {:why (:message e)}))))

(defn- protection-guard
  [store old new]
  (let [touched (governance/touched-rows old new)]
    (project-scope/require-schema-change! (grants/actor store) touched)
    (project-scope/require-unlocked! old (op/query {:store store} :authoring/locked-projects
                                                      {:branch (str (:branch store))
                                                       :schema old})
                                     touched)
    (protection/protection-guard store (:branch store) touched)))

(defn install!
  [store schema {:keys [project edge]}]
  (refuse-if-degraded! store)
  (let [store (governance/write-guard store edge (:branch store) nil)]
    {:schema (onto-install/install-schema! store schema {:guard   protection-guard
                                                         :project project})}))

(def ^:private empty-schema
  {:object-types {} :link-types {} :transforms {} :actions {} :object-sets {}})

(defn merge-and-install
  [store schema f opts]
  (install! store (f (or schema empty-schema)) opts))

(defn- deletion-conflict
  [schema kinds-key ident cascade? ix]
  (let [status-block (or (retirement/deletion-status-block schema kinds-key ident)
                         (when cascade?
                           (retirement/cascade-status-block schema kinds-key ident ix)))
        owned        (retirement/deletion-owner-block schema kinds-key ident)
        blockers     (seq (retirement/deletion-blockers schema kinds-key ident cascade? ix))]
    (cond
      status-block
      {:error (:message status-block) :status (:status status-block)}

      owned
      {:error      (:message owned)
       :reason     :declared-in-repository
       :entity     (:entity owned)
       :repository (:repository owned)}

      blockers
      (let [named (mapv (fn [{:keys [in entity] :as b}]
                          (cond-> (select-keys b [:in :entity])
                            (lookup/name-of schema in entity)
                            (assoc :name (lookup/name-of schema in entity))))
                        blockers)]
        {:error (str "cannot delete: entity is still referenced by "
                     (str/join ", " (map #(str (name (:in %)) " "
                                               (name (or (:name %) (:entity %))))
                                         named))
                     " — repoint or remove the referrers first"
                     (when-not cascade?
                       (str " (or pass --cascade to remove associated link-types and"
                            " unbind the :backing of the object types it backs)")))
         :referrers named}))))

(defn- deletion
  [schema kinds-key ident cascade? ix]
  (let [warnings (seq (retirement/deletion-warnings schema kinds-key ident ix))
        cascaded (when cascade?
                   (seq (retirement/associated-link-types schema kinds-key ident ix)))
        unbound  (when cascade? (seq (retirement/backing-referrers schema kinds-key ident ix)))]
    {:schema (cond-> schema
               cascade? (retirement/cascade-associated-links kinds-key ident ix)
               :always  (-> (retirement/unbind-soft-referrers kinds-key ident)
                            (retirement/remove-own-object-backing kinds-key ident ix)))
     :report (cond-> {}
               warnings (assoc :warnings
                               (mapv #(assoc (select-keys % [:in :entity]) :unbound true)
                                     warnings))
               cascaded (assoc :cascaded (mapv #(select-keys % [:entity]) cascaded))
               unbound  (assoc :unbound-backings
                               (mapv #(or (lookup/name-of schema :object-types %) %)
                                     (sort-by str unbound))))}))

(defn- applied
  [store k schema step]
  (kernel/apply-step store (scope/code) schema step #(kept/step @k schema step)))

(defn- step-plan
  [store schema0 {:keys [op path cascade?] :as step}]
  (let [[kinds-key ident] path
        k (delay (kept/db store (:branch store)))]
    (if (not= :dissoc op)
      {:schema (applied store k schema0 step) :report {}}
      (let [{:keys [conflict schema report]}
            (let [ix     (live/index-of (:after (kept/onto @k schema0)))
                  schema (retirement/remove-owned-link-producers
                          schema0 kinds-key ident cascade? ix)]
              (if-let [c (deletion-conflict schema kinds-key ident cascade? ix)]
                {:conflict c}
                (deletion schema kinds-key ident cascade? ix)))]
        (if conflict
          {:conflict conflict}
          {:schema (applied store k schema step) :report report})))))

(defn- instance-step?
  [schema {[section k] :path value :value}]
  (let [ik (get (ents/instance-keys (or schema {})) section)]
    (boolean (or (= :all ik)
                 (and value (ents/authored-instance? section value))
                 (when (and ik k) (contains? ik (or (lookup/rid-of schema section k) k)))))))

(defn- landing
  [store schema step]
  (let [inst? (instance-step? schema step)
        home  (world/instance-home store)]
    (if (and home inst?)
      (let [store (world/at store home)] [store (seat/branch-schema store) true])
      [store schema inst?])))

(defn run-step
  [store schema step {:keys [project edge]}]
  (let [[store schema inst?] (landing store schema step)
        _  (refuse-if-degraded! store)
        store (governance/write-guard store edge (:branch store) nil {:instance? inst?})
        {:keys [conflict report] next-schema :schema}
        (step-plan store (or schema empty-schema) step)]
    (if conflict
      {:conflict conflict}
      (merge {:schema (onto-install/install-schema!
                       store next-schema {:guard protection-guard :project project})}
             report))))

(defn- rename-object-views
  [schema kinds-key rid new]
  (if (not= :object-types kinds-key)
    schema
    (update schema :applications
            (fn [apps]
              (reduce-kv
               (fn [m k app]
                 (assoc m k (if (and (:form-factor app) (= rid (:object-type app)))
                              (assoc app :application-name
                                     (keyword (str (name new) "-view-" (name (:form-factor app)))))
                              app)))
               {} (or apps {}))))))

(def python-field
  {:object-types "object_type" :datasets "dataset" :link-types "link_type"})

(defn rename-in-python
  [code field old new]
  (if-not (string? code)
    code
    (str/replace code
                 (re-pattern (str "(\\b" field "\\s*=\\s*)([\"'])"
                                  (java.util.regex.Pattern/quote (name old)) "\\2"))
                 (str "$1$2" (str/re-quote-replacement (name new)) "$2"))))

(defn- rename-in-sql
  [query old new]
  (if-not (string? query)
    query
    (str/replace query
                 (re-pattern (str "(?<![\\w-])" (java.util.regex.Pattern/quote (name old))
                                  "(?![\\w-])"))
                 (str/re-quote-replacement (name new)))))

(defn- rename-in-transforms
  [schema kinds-key old new]
  (let [field (python-field kinds-key)]
    (cond-> schema
      (and field (map? (:transforms schema)))
      (update :transforms update-vals
              (fn [t]
                (cond-> t
                  (string? (:code t))
                  (update :code rename-in-python field old new)
                  (and (= :datasets kinds-key) (string? (:query t)))
                  (update :query rename-in-sql old new)))))))

(defn- rename-in-apps
  [schema kinds-key old new]
  (let [in-app (fn [app]
                 (cond-> app
                   (map? (:layout app))
                   (update :layout cat/rename-object-type old new)
                   (sequential? (:sidebar app))
                   (update :sidebar cat/rename-object-type old new)))]
    (cond-> schema
      (and (= :object-types kinds-key) (map? (:applications schema)))
      (update :applications update-vals #(cond-> % (map? %) in-app)))))

(defn- remember-former-name
  [schema kinds-key rid old new]
  (cond-> schema
    (= :datasets kinds-key)
    (update-in [:datasets rid :former-names]
               (fn [names]
                 (vec (remove #{new} (distinct (conj (vec names) old))))))))

(defn- keep-code-where-it-is
  [schema kinds-key rid]
  (let [spec (get-in schema [:transforms rid])
        path (when (and (= :transforms kinds-key) (:repository spec)) (pemit/code-path spec))]
    (cond-> schema
      path (assoc-in [:transforms rid :code-path] path))))

(defn renamed-schema
  [schema kinds-key old new]
  (let [rid (lookup/rid-of schema kinds-key old)]
    (when-not rid
      (refusal/refuse! :refusal.authoring/no-such-resource {:name old} {:kind (name kinds-key)}))
    (when (and (not= old new) (lookup/rid-of schema kinds-key new))
      (refusal/refuse! :refusal.authoring/name-in-use {:name new}))
    (-> schema
        ri/ensure-actions
        (keep-code-where-it-is kinds-key rid)
        (assoc-in [kinds-key rid (rid/name-field-of kinds-key)] new)
        (retirement/rename-own-object-backing kinds-key rid old new)
        (rename-object-views kinds-key rid new)
        (rename-in-apps kinds-key old new)
        (rename-in-transforms kinds-key old new)
        (remember-former-name kinds-key rid old new))))

(def ^:private layout-node-kind
  {:transforms :transform :object-types :object-type :datasets :dataset})

(defn carry-layout!
  ([store kinds-key from to]
   (when-let [k (layout-node-kind kinds-key)]
     (op/run! {:store store} :canvas/move-layout-node
              {:branch (:branch store) :node-kind k :from (name from) :to (name to)}))))

(defn rename-entity
  [store schema kinds-key path-name to opts]
  (let [old   (keyword path-name)
        block (retirement/rename-status-block (or schema {}) kinds-key old)]
    (if block
      {:conflict {:error (:message block) :status (:status block)}}
      (let [r (merge-and-install
               store schema (fn [s] (renamed-schema s kinds-key old (->kw to))) opts)]
        (carry-layout! store kinds-key old (->kw to))
        r))))

(defn rename-transform
  [store schema from to opts]
  (let [old    (->kw from)
        to     (->kw to)
        s      (or schema {})
        ds-rid (get-in s [:transforms (lookup/rid-of s :transforms old) :dataset])
        carry? (and ds-rid
                    (= old (get-in s [:datasets ds-rid :dataset-name]))
                    (nil? (lookup/rid-of s :datasets to)))
        r      (rename-entity store schema :transforms from to opts)]
    (if (and carry? (not (:conflict r)))
      (let [r (merge-and-install store (:schema r)
                                 (fn [s] (assoc-in s [:datasets ds-rid :dataset-name] to))
                                 opts)]
        (carry-layout! store :datasets old to)
        r)
      r)))
