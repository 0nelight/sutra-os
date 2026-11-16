(ns sutra.automation.runs
  (:require [sutra.kit.coll :as coll]
            [sutra.kit.refusal :as refusal]
            [sutra.store.control :as ctl]
            [sutra.store.world :as world]
            [clojure.edn :as edn]
            [clojure.set]
            [sutra.catalog.grants :as grants]
            [sutra.types.opid :as opid]
            [sutra.foundation.encoding :as enc]
            [sutra.store.control.cadence :as cadence]))

(set! *warn-on-reflection* true)

(def ^:private table "automations")

(defn- unwrap-spec
  [text]
  (if-let [inner (and (string? text)
                      (let [v (try (edn/read-string text) (catch RuntimeException _ nil))]
                        (when (string? v) v)))]
    (recur inner)
    text))

(defn- row->automation [row]
  (-> row
      (clojure.set/rename-keys {:on-type :on})
      (cond-> (contains? row :when-spec)   (update :when-spec unwrap-spec)
              (contains? row :effect-spec) (update :effect-spec unwrap-spec))))

(defn- spec-form
  [field spec]
  (if-not (string? spec)
    spec
    (let [v (try (edn/read-string (unwrap-spec spec))
                 (catch RuntimeException e
                   (refusal/refuse! :refusal.automation/field-not-edn
                                    {:field field}
                                    {:field (name field) :error (.getMessage e)})))]
      (when-not (or (nil? v) (map? v) (vector? v) (keyword? v))
        (refusal/refuse! :refusal.automation/field-shape
                         {:field field} {:field (name field) :spec spec}))
      v)))

(defn list-all [store branch]
  (mapv row->automation (cadence/list-all store branch table)))

(defn get-one [store branch id]
  (some-> (cadence/get-one store branch table id) row->automation))

(defn upsert!
  [store branch {:keys [id name on when-spec effect-spec every-ms paused live auto-mute]
                     :as auto}]
  (let [when-spec   (spec-form :when   (or when-spec   (:when auto)))
        effect-spec (spec-form :effect (or effect-spec (:effect auto)))
        id (or id (opid/name-id :automation name))
        owner (grants/acting-username (grants/actor store))]
    (when-not owner
      (refusal/refuse! :refusal.automation/needs-owner
                       {:rule :automation/no-owner :automation name}))
    (row->automation
     (cadence/upsert! store branch table
                      {:id id :name name :on-type (some-> on clojure.core/name)
                       :when-spec (when when-spec (enc/->payload when-spec))
                       :effect-spec (enc/->payload effect-spec)
                       :every-ms every-ms :paused (boolean paused) :owner owner
                       :live (boolean live) :auto-mute (boolean auto-mute)}))))

(defn set-muted!
  [store branch id muted? who]
  (ctl/commit! store [[:update :automations
                       {:branch branch :id id}
                       {:muted (boolean muted?) :muted-by (when muted? (str who))
                        :muted-at (when muted? (ctl/clock store))}]]))

(defn subscribers [store branch id]
  (mapv :username (ctl/list store :automation-subscriptions {:branch branch :automation-id id})))

(defn subscribe! [store branch id username]
  (ctl/commit! store [[:upsert :automation-subscriptions
                       [{:branch branch :automation-id id :username (str username)}]]]))

(defn unsubscribe! [store branch id username]
  (ctl/commit! store [[:delete :automation-subscriptions
                       {:branch branch :automation-id id :username (str username)}]]))

(defn delete! [store branch id]
  (cadence/delete! store branch table id))

(defn mark-fired! [store branch id matched]
  (cadence/mark-fired! store branch table id :last-matched matched))

(def ^:private definition
  [:name :on-type :when-spec :effect-spec :every-ms :paused])

(defn own-rows
  [rows parent-of row-on]
  (let [inherited? (fn [{:keys [branch id] :as r}]
                     (loop [b (parent-of branch) seen #{branch}]
                       (cond
                         (or (nil? b) (seen b)) false
                         :else (if-let [p (row-on b id)]
                                 (= (select-keys r definition) (select-keys p definition))
                                 (if (= b ctl/trunk) false (recur (parent-of b) (conj seen b)))))))]
    (filterv #(or (= ctl/trunk (:branch %)) (not (inherited? %))) rows)))

(defn list-due-on [store branches]
  (mapv row->automation
        (own-rows (cadence/list-due-on store branches table)
                  #(or (world/parent store %) ctl/trunk)
                  #(ctl/get store :automations {:branch %1 :id %2}))))

(defn effect-entry
  [store branch id run-id object version]
  (ctl/entry store :automation-effects {:branch branch :automation-id id
                                      :run-id (str run-id) :object (str object) :at ctl/now
                                      :version (str (or version ""))}))

(defn mark-versions!
  [store branch id run-id object->version]
  (when (seq object->version)
    (doseq [rows (partition-all 500 (for [[o v] object->version]
                                      {:branch branch :automation-id id :run-id (str run-id)
                                       :object (str o) :version (str v) :at ctl/now}))]
      (ctl/commit! store [(fn [s at] (ctl/upserting s :automation-effects rows at))]))))

(defn applied-versions
  [store branch id]
  (into #{} (map (juxt :object :version))
        (ctl/list store :automation-effects {:branch branch :automation-id id})))

(defn record-run!
  [store branch id {:keys [matched applied affected-ids status detail proposed decisions run-id]}]
  (ctl/commit! store
               [[:insert :automation-runs
                 [(cond-> {:branch branch :automation-id id :matched matched :applied applied
                           :affected-ids (vec affected-ids) :status (or status "ok") :detail detail
                           :proposed (or proposed 0)}
                    run-id          (assoc :run-id (str run-id))
                    (seq decisions) (assoc :decisions (vec decisions)))]]]))

(defn list-runs
  [store branch id limit]
  (let [where    {:branch branch :automation-id id}
        recorded (ctl/list store :automation-runs where {:limit (or limit 50)})
        known    (into #{} (keep :run-id) recorded)
        folded   (for [[run-id
                        es] (group-by :run-id
                                      (ctl/list store :automation-effects where {:limit 500}))
                       :let [es (vals (coll/index-by :object es))]
                       :when (not (known run-id))]
                   {:branch branch :automation-id id :run-id run-id
                    :matched nil :applied (count es) :affected-ids (mapv :object es)
                    :status "unrecorded" :proposed 0
                    :detail "the run's effects committed, its record did not"
                    :fired-at (reduce #(if (pos? (compare %2 %1)) %2 %1) (map :at es))})]
    (vec (take (or limit 50)
               (sort-by #(if-let [t (:fired-at %)] (.getTime ^java.util.Date t) 0) >
                        (concat recorded folded))))))
