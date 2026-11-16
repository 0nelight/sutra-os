(ns sutra.sources.connector
  (:require [sutra.types.plan :as cp]
            [clojure.string :as str]
            [sutra.sources.protocols :as api]
            [sutra.sources.policy :as policy]
            [sutra.kit.refusal :as refusal]
            [sutra.foundation.scan-progress :as scan-progress]
            [sutra.kit.effects :as effects]
            [sutra.types.source :as source])
  (:import (java.time LocalDateTime)))

(set! *warn-on-reflection* true)

(def ^:private capability-fns
  {:catalog [:tables :columns] :scan [:scan] :write [:write] :invoke [:invoke] :token [:token]})

(defn problems
  [entry]
  (let [caps (get-in entry [:profile :capabilities])]
    (vec (concat
          (for [c caps, f (capability-fns c) :when (not (fn? (get entry f)))]
            {:problem :capability-without-fn :capability c :fn f})
          (for [[c fs] capability-fns, f fs
                :when (and (fn? (get entry f)) (not (contains? caps c)))]
            {:problem :fn-without-capability :capability c :fn f})))))

(defn assert-connectors!
  []
  (when-let [cs (seq (source/collisions (source/connectors)))]
    (refusal/refuse! :refusal.foreign/connector-collides {:types (vec cs)}
                     {:types (str/join ", " (map name cs))}))
  (doseq [[t entry] (source/connectors)
          :when (and (source/connector t) (:open entry))
          :let [ps (problems entry)]]
    (cp/check! (cp/of-findings (cp/problems :foreign/connector-malformed ps))
               {:code   :refusal.foreign/connector-malformed
                :data   {:type t}
                :params {:type (name t) :problems (pr-str ps)}}))
  :ok)

(defn ctx
  ([spec] (ctx spec nil))
  ([spec socket-factory]
   (cond-> {:source-name     (:source-name spec)
            :progress!       (let [work effects/*work*]
                               (fn [m]
                                 (effects/check! work)
                                 (scan-progress/note! (some-> (:source-name spec) name) m)))
            :egress-allowed? (fn [host port] (policy/egress-permits? spec host port))
            :assert-egress!  (fn [host port] (policy/assert-egress-permits! spec host port))}
     socket-factory (assoc :socket-factory socket-factory))))

(defn lower-row
  [row]
  (persistent! (reduce-kv (fn [m k v] (assoc! m (keyword (str/lower-case (name k))) v))
                          (transient {}) row)))

(defn- call [entry k spec & args]
  (if-let [f (get entry k)]
    (apply f args)
    (api/unsupported k spec)))

(defn plan
  [spec write-spec]
  (let [p (policy/write-plan (LocalDateTime/now) spec write-spec)]
    (when (:log p)
      (refusal/refuse! :refusal.foreign/connector-log
                       {:table (:table write-spec) :type (:type spec)}
                       {:type (name (:type spec))}))
    (cond-> p
      (and (seq (:pk-columns p)) (contains? p :pk-value))
      (assoc :key (zipmap (:pk-columns p) (policy/pk-parts (:pk-columns p) (:pk-value p)))))))

(defn written
  [plan r]
  (if (:conflict r)
    (refusal/refuse! :refusal.foreign/edit-conflict
                     {:conflict true :table (:table plan) :pk-value (:pk-value plan)
                      :expect (:expect plan)})
    r))

(defrecord ConnectorSource [spec entry handle]
  api/Source
  (-profile [_] (:profile entry))
  (-probe [_]
    (cond (:probe entry)  ((:probe entry) handle)
          (:tables entry) ((:tables entry) handle))
    nil)
  (-close [_]
    (scan-progress/end! (some-> (:source-name spec) name))
    (when-let [f (:close entry)] (f handle))
    nil)
  api/Catalog
  (-tables [_] (vec (sort (call entry :tables spec handle))))
  (-columns [_ table] (vec (call entry :columns spec handle table)))
  (-relations [_] [])
  api/Scan
  (-scan-profile [_ _] (or (get-in entry [:profile :scan]) {:preds {}}))
  (-scan [_ table pushed] (mapv lower-row (call entry :scan spec handle table pushed)))
  api/Write
  (-write! [_ write-spec]
    (let [p (plan spec write-spec)]
      (written p (call entry :write spec handle p))))
  api/Invoke
  (-invoke! [_ op params opts] (call entry :invoke spec handle op params opts))
  api/Token
  (-token [_ table] (call entry :token spec handle table)))

(defn open
  ([spec] (open spec nil))
  ([spec socket-factory]
   (let [entry (or (some-> (source/connector (:type spec)) (#(when (:open %) %)))
                   (refusal/refuse! :refusal.foreign/connector-not-loaded {:type (:type spec)}
                                    {:type (some-> (:type spec) name)}))]
     (when (:endpoint (:profile entry)) (policy/assert-egress-allowed! spec))
     (->ConnectorSource spec entry ((:open entry) spec (ctx spec socket-factory))))))

(defn run-op
  [spec {:keys [op table scan write-spec webhook-id params opts]}]
  (api/with-source [s open spec]
    (case op
      :probe        (api/probe s)
      :tables       (api/-tables s)
      :columns      (api/-columns s table)
      :scan-profile (api/-scan-profile s table)
      :scan         (api/-scan s table scan)
      :write        (api/-write! s write-spec)
      :invoke       (api/-invoke! s webhook-id params opts)
      :token        (api/-token s table)
      (refusal/refuse! :refusal.sources/unknown-connector-op {:op op}))))
