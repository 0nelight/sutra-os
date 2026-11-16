(ns sutra.automation.logic-events
  (:require [sutra.kit.refusal :as refusal]
            [sutra.store.control :as ctl]
            [sutra.kit.tree :as tree]
            [sutra.kit.digest :as digest]
            [sutra.kit.err :as err]
            [sutra.types.run-limits :as limits]))

(set! *warn-on-reflection* true)

(defn- canonical [x]
  (tree/canonical {:leaf #(if (or (uuid? %) (inst? %)) (str %) %)} x))

(defn fingerprint
  [once inputs stored-values]
  (if (= :object once)
    (digest/sha256-hex "object")
    (digest/sha256-hex (pr-str (canonical {:inputs inputs :values stored-values})))))

(defn- ms-ago ^java.util.Date [store ms]
  (java.util.Date. (- (ctl/millis store) (long ms))))

(defn- before? [a ^java.util.Date b] (and a (< (inst-ms a) (.getTime b))))

(defn claim!
  [store branch automation-id object-id fp {:keys [manual? rerun-since stale-ms]}]
  (let [take (fn [row]
               (assoc row :status "running" :claim-id (str (random-uuid)) :started-at ctl/now))]
    (some-> (ctl/swap! store :logic-events
                       {:branch branch :automation-id automation-id :object-id (str object-id)
                        :fingerprint fp}
                       (fn [{:keys [status next-at started-at] :as old}]
                         (cond
                           (nil? old) (take {:event-id (str (random-uuid)) :attempts 0})
                           (and rerun-since (= "done" status)
                                (before? (:updated-at old) rerun-since))
                           (take (assoc old :attempts 0))
                           (and (= "failed" status)
                                (or manual?
                                    (and next-at (not (before? (ctl/clock store) next-at)))))
                           (take old)
                           (and (= "running" status) started-at
                                (not (before? (ms-ago store (or stale-ms 600000)) started-at)))
                           (take old))))
            (select-keys [:event-id :claim-id :attempts]))))

(defn- where
  ([branch automation-id object-id fp]
   {:branch branch :automation-id automation-id :object-id (str object-id) :fingerprint fp})
  ([branch automation-id object-id fp claim-id]
   (cond-> (where branch automation-id object-id fp)
     claim-id (assoc :claim-id claim-id))))

(defn held
  [store branch automation-id object-id fp claim-id]
  (when claim-id
    (let [w (assoc (where branch automation-id object-id fp claim-id) :status "running")]
      (or (ctl/expect store :logic-events w)
          (refusal/refuse! :refusal.automation/claim-superseded w)))))

(defn done!
  ([store branch automation-id object-id fp
    outcome] (done! store branch automation-id object-id fp outcome nil))
  ([store branch automation-id object-id fp outcome claim-id]
   (pos? (or (ctl/update! store :logic-events
                          (cond-> (where branch automation-id object-id fp claim-id)
                            claim-id (assoc :status "running"))
                          {:status "done" :outcome outcome :error nil :transient false :next-at nil
                           :finished-at ctl/now})
             0))))

(defn cool-down-ms
  [attempts]
  (let [{:keys [first-ms max-ms]} limits/logic-event-cool-down]
    (long (min max-ms (* first-ms (Math/pow 4 (max 0 (dec attempts))))))))

(defn failed!
  [store branch automation-id object-id fp {:keys [attempts error transient? claim-id]}]
  (let [n        (inc (or attempts 0))
        retry-ms (when (and transient? (< n limits/logic-event-attempts-max)) (cool-down-ms n))
        updated  (ctl/update! store :logic-events
                              (cond-> (where branch automation-id object-id fp claim-id)
                                claim-id (assoc :status "running"))
                              {:status "failed" :attempts n
                               :error (some-> error str (as-> e (subs e 0 (min 4000 (count e)))))
                               :transient (boolean transient?)
                               :next-at
                               (when retry-ms
                                 (java.util.Date. (long (+ (ctl/millis store) retry-ms))))
                               :finished-at ctl/now})]
    (when (or (nil? claim-id) (pos? (or updated 0)))
      {:attempts n :retry-in-ms retry-ms})))

(defn fallback!
  ([store branch automation-id object-id fp
    result] (fallback! store branch automation-id object-id fp result nil))
  ([store branch automation-id object-id fp result claim-id]
   (ctl/update! store :logic-events (where branch automation-id object-id fp claim-id)
                {:fallback result})))

(defn state-of
  [store branch automation-id object-ids]
  (if (empty? object-ids)
    {}
    (group-by :object-id
              (ctl/list store :logic-events
                        {:branch branch :automation-id automation-id
                         :object-id (into #{} (map str) object-ids)}))))

(defn list-for
  ([store branch automation-id status] (list-for store branch automation-id status nil))
  ([store branch automation-id status {:keys [limit offset]}]
   (ctl/list store :logic-events
             (cond-> {:branch branch :automation-id automation-id}
               status (assoc :status (name status)))
             (cond-> {:limit (max 1 (min 1000 (or limit 100)))}
               (and offset (pos? offset)) (assoc :offset offset)))))

(defn counts-for
  [store branch automation-id]
  (into {"running" 0 "done" 0 "failed" 0}
        (ctl/counts store :logic-events {:branch branch :automation-id automation-id} :status)))

(def ^:private transient-status #{408 425 429 500 502 503 504})

(def deterministic-limits
  #{:objects :edits :transcript :media})

(defn transient?
  [e]
  (let [chain (err/causes e)
        datas (mapcat #(let [d (ex-data %)] [d (:cause-ex-data d)]) chain)]
    (boolean
     (and
      (not (some (comp deterministic-limits :sutra/run-limit) datas))
      (or (some #(transient-status (:status %)) datas)
          (some #(#{:timeout} (:sutra/run-limit %)) datas)
          (some #(instance? java.io.IOException %) chain)
          (some #(instance? java.util.concurrent.TimeoutException %) chain)
          (some
           #(re-find #"language-model call refused \((429|5\d\d)\)|did not finish within|deadline"
                     (str (ex-message %)))
           chain))))))
