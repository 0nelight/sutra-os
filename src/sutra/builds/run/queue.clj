(ns sutra.builds.run.queue
  (:require [clojure.edn :as edn]
            [clojure.walk :as walk]
            [sutra.builds.run.claim :as claim]
            [sutra.foundation.wire :as wire]
            [sutra.kit.lifecycle :as lc]
            [sutra.builds.ports :as ports]
            [sutra.store.access.scope :as ctx]
            [sutra.store.control :as ctl]
            [sutra.store.control.lifecycle :as lcs]
            [sutra.kit.noted :as noted]))

(set! *warn-on-reflection* true)

(def claim-ms
  (* 3 60 1000))

(def ^:private await-poll-ms 1000)

(def ^:private keep-ms
  (* 7 24 60 60 1000))

(def request-table
  (lc/table
   {:name   :build-request
    :states #{:queued :running :done :failed :cancelled}
    :final  #{:done :failed :cancelled}
    :edges  {:take   {:from #{:queued} :to :running}
             :cancel {:from #{:queued} :to :cancelled}
             :settle {:from  #{:running}
                      :to    (fn [_ {:keys [status]}] (keyword status))
                      :guard (fn [r {:keys [claim-id]}]
                               (when-not (claim/mine? r claim-id) "another claim holds it"))}
             :lapse  {:from  #{:running} :to :failed
                      :guard (fn [r {:keys [now]}]
                               (when-not (claim/free? r now) "its claim still holds"))}}}))

(def ^:private settled-statuses
  (into #{} (map name) (:final request-table)))

(defn route
  [role mode]
  (if (#{:all :worker} role)
    (if (= :detach mode) :thread :inline)
    (if (= :await mode) :queue-await :queue)))

(defn- edn-value?
  [v]
  (or (nil? v) (string? v) (number? v) (boolean? v) (keyword? v) (symbol? v)
      (uuid? v) (instance? java.util.Date v) (coll? v) (map-entry? v)))

(defn wire-safe
  [x]
  (walk/postwalk
   (fn [v]
     (cond (record? v) (into {} v)
           (edn-value? v) v
           :else (str v)))
   x))

(defn crossed
  [preset]
  (-> (walk/postwalk
       (fn [v]
         (cond (record? v) (into {} v)
               (edn-value? v) v
               :else (throw (ex-info (str "a build request carries values, not "
                                          (.getName (class v)))
                                     {:value (str v)}))))
       preset)
      pr-str
      edn/read-string))

(def ^:private result-keys
  [:counts :skipped :run-id :snapshot-id :timings :phase-timings :trace :scope
   :index-reconcile :publish-failures])

(defn done
  [result]
  {:status "done" :result (wire-safe (select-keys result result-keys))})

(defn failed
  [^Throwable t]
  (if (:handed-back (ex-data t))
    {:status "queued"}
    {:status     "failed"
     :error      (or (ex-message t) (.getName (class t)))
     :error-data (wire-safe (or (ex-data t) {}))}))

(def ^:private lapsed-text
  "build failed: the worker running it stopped beating — it died before the build ended")

(defn answer
  [{:keys [status result error error-data] :as request} now]
  (case status
    nil         {:error "no such build request" :data {}}
    "queued"    nil
    "running"   (when (claim/free? request now) {:error lapsed-text :data {}})
    "done"      {:result result}
    "cancelled" {:error "build cancelled while queued" :data {:cancelled true}}
    {:error (or error "build failed") :data (or error-data {})}))

(defn request-run
  [{:keys [run-id status error] :as request} now]
  (let [a (answer request now)
        base {:id run-id :kind "import" :status status}]
    (cond
      (nil? a)                base
      (contains? a :result)   nil
      (= "cancelled" status) (assoc base :error-message (:error a))
      :else                   (assoc base :status "failed" :error-message (or error (:error a))))))

(defn stale
  [requests now]
  (filterv #(and (settled-statuses (:status %))
                 (some-> (:updated-at %) inst-ms (< (- (long now) keep-ms))))
           requests))

(defn- declared
  [column v]
  (when-let [p (wire/problem [:rows :build-requests column] v)]
    (println (str "[builds] " column " of a build request breaks its wire definition: " p)))
  v)

(defn request-context
  [store branch]
  (select-keys (ctx/context (ctx/at store branch)) [:branch]))

(defn submit!
  [store branch preset]
  (let [run-id (str (or (:run-id preset) (random-uuid)))]
    (ctl/insert! store :build-requests
                 {:run-id run-id :branch (some-> branch str) :status "queued"
                  :context (declared :context (request-context store branch))
                  :preset (declared :preset (crossed (dissoc preset :run-id)))
                  :origin (:origin preset)})
    (when-let [broadcast (ports/cluster-broadcast store)]
      (broadcast :build/requested {:run-id run-id}))
    run-id))

(defn requested-run
  [store run-id]
  (some-> (ctl/get store :build-requests {:run-id (str run-id)}) (request-run (ctl/millis store))))

(defn take!
  [store owner]
  (some (fn [{:keys [run-id]}]
          (ctl/swap! store :build-requests {:run-id run-id}
                     (lcs/stepping request-table :take
                                   (fn [old _]
                                     (-> (claim/claimed old (str (random-uuid))
                                                        (ctl/millis store) claim-ms)
                                         (assoc :owner owner))))))
        (ctl/list store :build-requests {:status "queued"})))

(defn renew!
  [store run-id claim-id]
  (ctl/swap! store :build-requests {:run-id run-id}
             #(claim/renewed % claim-id (ctl/millis store) claim-ms)))

(defn finish!
  [store run-id claim-id settlement]
  (ctl/swap! store :build-requests {:run-id run-id}
             (lcs/stepping request-table :settle
                           {:claim-id claim-id :status (:status settlement)}
                           (fn [old _]
                             (merge (claim/released old)
                                    (cond-> settlement
                                      (:result settlement) (update :result #(declared :result %))
                                      (:error-data settlement)
                                      (update :error-data #(declared :error-data %))))))))

(defn cancel-queued!
  [store run-id]
  (some? (ctl/swap! store :build-requests {:run-id (str run-id)}
                    (lcs/stepping request-table :cancel (fn [_ _] nil)))))

(defn open
  [store]
  (noted/or-else "build queue" []
    (into (vec (ctl/list store :build-requests {:status "queued"}))
          (ctl/list store :build-requests {:status "running"}))))

(defn tidy!
  [store]
  (let [t       (ctl/millis store)
        lapsed  (into []
                      (comp (filter #(claim/free? % t)) (map :run-id))
                      (ctl/list store :build-requests {:status "running"}))
        settled (into #{}
                      (comp (mapcat #(stale (ctl/list store :build-requests {:status %}
                                                      {:limit 50})
                                            t))
                            (map :run-id))
                      settled-statuses)
        fail    (lcs/stepping request-table :lapse {:now t}
                              (fn [old _] (assoc (claim/released old) :error lapsed-text)))]
    (when (or (seq lapsed) (seq settled))
      (ctl/commit! store
                   (concat
                    (for [id lapsed]
                      (fn [s at] (ctl/swapping s :build-requests {:run-id id} fail at)))
                    (when (seq settled)
                      [(fn [s _] (ctl/deleting s :build-requests {:run-id settled}))]))))))

(defn await!
  [store run-id]
  (loop []
    (let [a (answer (ctl/get store :build-requests {:run-id run-id}) (ctl/millis store))]
      (cond (nil? a)              (do (Thread/sleep (long await-poll-ms)) (recur))
            (contains? a :result) (:result a)
            :else (throw (ex-info (:error a) (assoc (:data a) :run-id run-id)))))))

(defn detach!
  [what f]
  (let [run-id (random-uuid)
        go     (bound-fn []
                 (try (f run-id)
                      (catch Throwable e
                        (binding [*out* *err*]
                          (println (str "[run] " what " " run-id " failed: " (ex-message e)))))))]
    (doto (Thread. ^Runnable go (str "sutra-run-" run-id)) (.start))
    run-id))
