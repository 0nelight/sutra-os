(ns sutra.actions.staging
  (:require [sutra.kit.refusal :as refusal]
            [sutra.catalog.grants :as grants]
            [sutra.types.branch-kinds :as branch-kinds]
            [sutra.actions.staged-rows :as staged]
            [sutra.store.access.scope :as ctx]
            [sutra.store.access.substrate :as sub]
            [sutra.catalog.actions :as actions]
            [sutra.catalog.lookup :as lookup]
            [sutra.store.memo :as memo]
            [sutra.foundation.encoding :as enc]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.kit.lifecycle :as lc]
            [sutra.store.kept :as kept]
            [sutra.store.world :as world]
            [sutra.types.rid :as rid]
            [sutra.types.schema :as sch]))

(set! *warn-on-reflection* true)

(defn current-staging-branch
  [store]
  (branch-kinds/staging-branch (:branch store) (grants/current-user-id (grants/actor store))))

(defn staging-branch-of
  [parent user-id]
  (or (branch-kinds/staging-branch parent user-id)
      (refusal/refuse! :refusal.actions/stage-no-parent {:current parent})))

(defn fork-staging!
  [store parent staging]
  (world/fork! store parent staging {:kind :staging}))

(def lifecycle
  (lc/table {:name   :staging
             :states #{:empty :staged :committed :discarded}
             :final  #{:committed :discarded}
             :edges  {:stage   {:from #{:empty :staged} :to :staged}
                      :unstage {:from #{:staged}
                                :to (fn [_ {:keys [left]}] (if (pos? left) :staged :discarded))}
                      :rebase  {:from #{:staged} :to :staged}
                      :commit  {:from #{:staged} :to :committed}
                      :discard {:from #{:empty :staged} :to :discarded}}}))

(defonce ^:private memo-tables (atom {}))

(def ^:private memo-spec {::schema {:cap 32 :clears #{::derived}} ::derived {:cap 128}})

(defn- basis-key
  [store]
  (when-let [rev (:rev (world/basis store))]
    [(:db store) (str (:branch store)) rev
     (when-let [h (and (:instance-keys store) (world/instance-home store))]
       (:rev (world/basis (world/at store h))))]))

(defn staging-schema
  [store]
  (if-let [k (basis-key store)]
    (memo/through memo-tables memo-spec ::schema k
                  #(sch/indexed (kept/staging store (:branch store))))
    (kept/staging store (:branch store))))

(defn of-staging-schema
  [store k f]
  (if-let [b (basis-key store)]
    (memo/through memo-tables memo-spec ::derived [b k] #(f (staging-schema store)))
    (f (staging-schema store))))

(defn staging-view
  [store]
  (let [parent  (:branch store)
        staging (current-staging-branch store)
        row     (when staging (world/branch store staging))
        live?   (some? row)]
    {:parent parent :staging staging :live? live? :base (:base row)
     :read (if live? staging parent)}))

(defn decide!
  [store event input state]
  (lc/decide lifecycle (current-staging-branch store) event state nil input))

(defn staged-rows
  [store staging]
  (if staging (vec (staged/list-staged store staging)) []))

(defn clear-staged!
  [store {:keys [staging live?]}]
  (if-not live?
    0
    (let [n (count (staged-rows store staging))]
      (decide! store :discard nil :staged)
      (staged/clear-staged! store staging)
      (world/retire! store staging)
      n)))

(def ^:private instance-verbs
  {:create :create :create-object :create :update :update :update-object :update
   :delete-object :delete :create-link :link :delete-link :unlink})

(defn row-kind
  [schema a]
  (or (actions/schema-action-kinds a)
      (let [{:keys [op object-type link-type]} (first (:rules (lookup/action schema a)))]
        (when-let [verb (instance-verbs op)]
          {:verb verb :kind :instance :type (or object-type link-type)}))))

(defn- read-object*
  [store obj-type id]
  (:values (first (sub/read-objects store obj-type [id]))))

(defn- staged-before-values
  [store schema staged-rows]
  (into {}
        (keep (fn [{:keys [seq action args]}]
                (let [{:keys [verb kind type]} (row-kind schema action)]
                  (when (and (= [:update :instance] [verb kind]) type (:id args))
                    [seq (read-object* store type (:id args))]))))
        staged-rows))

(def ^:private row-extra
  {:object-type (fn [_ args] {:type-name (:name args) :attributes (:attributes args)})

   :link-type   (fn [_ args] {:type-name   (:name args)
                              :from        (:from args)
                              :to          (:to args)
                              :cardinality (:cardinality args)})

   :interface   (fn [_ args] {:type-name        (:name args)
                              :property-list    (or (:property-list args) (:properties args))
                              :link-constraints (or (:link-constraints args) (:links args))
                              :extends          (:extends args)})

   :create      (fn [_ args] {:values (:values args)})

   :update      (fn [before args]
                  (let [after (:values args)]
                    {:before-values before
                     :after-values  after
                     :changes (vec (for [[k v] after]
                                     {:attr k :before (get before k) :after v}))}))

   :link        (fn [_ args] {:from-id    (:from-id args)
                              :to-id      (:to-id args)
                              :from-short (some-> (:from-id args) str (subs 0 8))
                              :to-short   (some-> (:to-id   args) str (subs 0 8))})})

(defn- enrich-row
  [schema before {:keys [seq action args created-at]}]
  (let [{:keys [verb kind type section]} (row-kind schema action)
        base {:seq        seq
              :action     action
              :verb       (some-> verb name)
              :kind       (or kind :instance)
              :section    section
              :type       (some-> (or type (when (not= :instance kind) kind)) name)
              :target-id  (:id args)
              :short-id   (enc/clip 8 (:id args))
              :created-at created-at
              :args       args}]
    (if-let [f (row-extra (if (= :instance kind) verb kind))]
      (merge base (f before args))
      base)))

(defn staged-creates
  [store]
  (let [{:keys [staging live?]} (staging-view store)]
    (if-not live?
      {}
      (into {}
            (for [{:keys [seq action args]} (staged/list-staged store staging)
                  :let [{:keys [verb kind]} (actions/schema-action-kinds action)
                        nm (:name args)]
                  :when (and (= :create verb) nm)]
              [[kind (->kw (rid/->name nm))] seq])))))

(defn enrich-rows
  [stage-store schema rows]
  (let [befores (staged-before-values stage-store schema rows)]
    (mapv #(enrich-row schema (get befores (:seq %)) %) rows)))

(defn enriched-rows
  [store {:keys [staging live?]} rows]
  (let [stage-store (if live? (ctx/at store staging) store)]
    (enrich-rows stage-store (staging-schema stage-store) rows)))

(defn list-staged-enriched
  [store]
  (let [{:keys [staging] :as v} (staging-view store)]
    (if staging (enriched-rows store v (staged-rows store staging)) [])))

(defn open-transactions
  [store]
  (let [me     (grants/current-user-id (grants/actor store))
        parent (:branch store)]
    (into []
          (keep (fn [{:keys [branch n author origin origin-ref opened-at latest-at]}]
                  (when-let [base (branch-kinds/staging-parent branch)]
                    (when (or (nil? parent) (= (str parent) base))
                      {:branch     branch
                       :parent     base
                       :owner      author
                       :mine?      (= author me)
                       :origin     (some-> origin keyword)
                       :origin-ref origin-ref
                       :n          n
                       :opened-at  opened-at
                       :latest-at  latest-at}))))
          (staged/open-batches store))))

(defn open-transaction
  [store]
  (first (filter :mine? (open-transactions store))))
