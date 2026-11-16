(ns sutra.actions.proposals
  (:require [sutra.kit.anomaly :as anomaly]
            [sutra.kit.refusal :as refusal]
            [sutra.kit.noted :as noted]
            [sutra.store.control :as ctl]
            [sutra.store.served :as served]
            [sutra.kit.tree :as tree]
            [sutra.kit.digest :as digest]
            [clojure.string :as str]
            [sutra.catalog.actions :as actions]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.lookup :as lookup]
            [sutra.actions.dispatch :as c]
            [sutra.objects.api :as objects]
            [sutra.actions.action :as action]
            [sutra.store.seat :as seat]
            [sutra.store.access.scope :as ctx]
            [sutra.store.access.substrate :as sub]
            [sutra.store.api :as api]))

(set! *warn-on-reflection* true)

(declare object-params)

(defn- read-versions
  [store schema action args]
  (into {}
        (for [[p rid] (object-params schema action)
              :let [id (get args p)]
              :when (some? id)
              o (sub/read-objects (ctx/as-system store) rid [(str id)])]
          [[rid (str (:id o))] (api/object-version o)])))

(defn- canonical [x]
  (tree/canonical {:leaf #(if (uuid? %) (str %) %)} x))

(defn digest [args] (digest/sha256-hex (pr-str (canonical args))))

(defn- identity-of [branch {:keys [automation-id object-id action args]}]
  {:branch branch :automation-id automation-id :object-id (some-> object-id str)
   :action action :args-digest (digest args)})

(defn- held-back? [store branch {:keys [source-key]} key]
  (if source-key
    (pos? (ctl/count store :agent-proposals {:branch branch :source-key source-key}))
    (pos? (ctl/count store :agent-proposals (assoc key :status "open")))))

(defn propose!
  [store branch {:keys [action args reason decision-log source-key provenance] :as p}]
  (let [key (identity-of branch p)]
    (when-not (held-back? store branch p key)
      (try
        (ctl/insert! store :agent-proposals
                     (cond-> (assoc key :id (str (random-uuid)) :args (canonical args)
                                    :reason reason
                                    :decision-log decision-log :status "open"
                                    :versions
                                    (try
                                      (read-versions
                                       store (seat/branch-schema (ctx/at store branch))
                                       action args)
                                      (catch clojure.lang.ExceptionInfo e
                                        (noted/note (str "versions read by " action) e))))
                       source-key (assoc :source-key (str source-key))
                       provenance (assoc :provenance (canonical provenance))))
        (catch Exception e
          (if (and source-key (held-back? store branch p key)) nil (throw e)))))))

(def list-max 1000)

(defn list-for
  ([store branch automation-id status] (list-for store branch automation-id status nil))
  ([store branch automation-id status {:keys [limit offset]}]
   (ctl/list store :agent-proposals
             (cond-> {:branch branch :automation-id automation-id}
               status (assoc :status (name status)))
             (cond-> {:limit (max 1 (min list-max (or limit 200)))}
               (and offset (pos? offset)) (assoc :offset offset)))))

(defn- object-params
  [schema action]
  (into {} (keep (fn [[p spec]]
                   (when-let [rid (and (:type spec)
                                       (lookup/rid-of schema :object-types (:type spec)))]
                     [p rid])))
        (:params (actions/resolve-action schema action))))

(defn- wanted
  [schema proposals]
  (->> (for [{:keys [action args accepted-args]} proposals
             [p rid] (object-params schema action)
             v [(get args p) (get accepted-args p)]
             :when (some? v)]
         [rid (str v)])
       (group-by first)
       (into {} (map (fn [[rid ids]] [rid (vec (distinct (map second ids)))])))))

(defn- object-ref [schema rid r]
  (let [nm (get-in schema [:object-types rid :object-type-name])
        tk (get-in schema [:object-types rid :title-key])]
    {:id (str (:id r)) :type (some-> nm name)
     :title (some-> (or (and tk (get-in r [:values (keyword (name tk))])) (:id r)) str)}))

(defn- attach-refs
  [schema found proposals]
  (mapv (fn [{:keys [action args accepted-args] :as pr}]
          (let [refs (into {} (for [[p rid] (object-params schema action)
                                    v [(get accepted-args p) (get args p)]
                                    :let [hit (and (some? v) (get found [rid (str v)]))]
                                    :when hit]
                                [p hit]))]
            (cond-> pr (seq refs) (assoc :refs refs))))
        proposals))

(defn with-refs
  [query schema proposals]
  (let [found (into {}
                    (for [[rid ids] (wanted schema proposals)
                          r (try (objects/objects-by-ids query schema rid ids)
                                 (catch clojure.lang.ExceptionInfo _ nil))]
                      [[rid (str (:id r))] (object-ref schema rid r)]))]
    (attach-refs schema found proposals)))

(defn counts-for
  [store branch automation-id]
  (into {"open" 0 "applied" 0 "rejected" 0 "failed" 0}
        (ctl/counts store :agent-proposals {:branch branch :automation-id automation-id} :status)))

(defn- one! [store branch automation-id id]
  (let [p (ctl/get store :agent-proposals {:branch branch :automation-id automation-id :id id})]
    (cond
      (nil? p) (refusal/refuse! :refusal.actions/proposal-not-found {:id id})
      (not= "open" (:status p)) (refusal/refuse! :refusal.actions/proposal-already-decided
                                                 {:id id :status (:status p)})
      :else p)))

(defn- decide! [store branch id status extra]
  (pos? (ctl/update! store :agent-proposals {:branch branch :id id :status "open"}
                     (merge {:status status :decided-at ctl/now
                             :decided-by (grants/acting-username (grants/actor store))} extra))))

(defn- edited-args
  [schema action args edits]
  (let [declared (set (keys (:params (actions/resolve-action schema action))))
        allowed  (into declared (keys args))
        bad      (remove allowed (keys edits))]
    (when (seq bad)
      (refusal/refuse! :refusal.actions/proposal-unknown-parameter
                      {:action action :unknown (vec bad)}
                      {:names (str/join ", " (map name bad)) :action (name action)}))
    (reduce-kv (fn [m k v] (if (nil? v) (dissoc m k) (assoc m k v))) args edits)))

(defn accept!
  ([store branch schema automation-id id] (accept! store branch schema automation-id id nil))
  ([store branch schema automation-id id edits]
   (let [{:keys [action args versions provenance]} (one! store branch automation-id id)
         run-args (if (seq edits) (edited-args schema action args edits) args)
         changed? (not= (canonical run-args) (canonical args))
         where    {:branch branch :id id}
         applied  (:records
                   (ctl/swapping store :agent-proposals where
                                 (fn [old]
                                   (when-not (= "open" (:status old))
                                     (refusal/refuse! :refusal.actions/proposal-decided-concurrently
                                       {:id id}))
                                   (cond-> (assoc old :status "applied" :decided-at ctl/now
                                                  :decided-by
                                                  (grants/acting-username (grants/actor store)))
                                     changed? (assoc :accepted-args (canonical run-args))))
                                 (ctl/clock store)))
         fenced   (update store :db served/fenced
                          (for [[[rid oid] v] versions] [:object rid oid v]))]
     (try
       (let [r      (c/dispatch-action! fenced schema action run-args {:records applied
                                                                       :provenance provenance})
             edited (action/edited-objects schema (:tx-vector r))]
         (ctl/update! store :agent-proposals where {:edited edited})
         (cond-> {:id id :status "applied" :action action :edited edited}
           changed? (assoc :accepted-args run-args)))
       (catch InterruptedException e (throw e))
       (catch Exception e
         (if (and (= :conflict (anomaly/category (ex-data e)))
                  (not= :stale-object (:store/refusal (ex-data e))))
           (throw e)
           (do (decide! store branch id "failed" {:error (ex-message e)})
               {:id id :status "failed" :action action :error (ex-message e)})))))))

(defn reject! [store branch automation-id id]
  (one! store branch automation-id id)
  (if (decide! store branch id "rejected" nil)
    {:id id :status "rejected"}
    (refusal/refuse! :refusal.actions/proposal-decided-concurrently {:id id})))
