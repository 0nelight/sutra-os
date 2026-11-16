(ns sutra.builds.run
  (:require [sutra.catalog.lookup :as lookup]
            [sutra.builds.run.rows :as run-rows]
            [sutra.catalog.grants :as grants]
            [sutra.store.tx-vector :as tv]
            [sutra.kit.digest :as digest]
            [clojure.string :as str]
            [sutra.store.seat :as seat]
            [sutra.builds.ledger :as ledger]
            [sutra.builds.run.lock :as build-lock]
            [sutra.builds.import.reconcile :as reconcile]
            [sutra.builds.run.monitor :as bm]
            [sutra.foundation.encoding :as enc]
            [sutra.store.storage.command :as cmd]
            [sutra.store.access.scope :as ctx]
            [sutra.store.storage.node :as node]
            [sutra.store.access.datasets :as dsets]
            [sutra.store.access.watch :as watch]
            [sutra.types.rid :as rid]
            [sutra.types.schema :as sch]
            [sutra.store.control :as ctl]
            [sutra.kit.attempt :as attempt]
            [sutra.kit.noted :as noted])
  (:import (java.util UUID)))

(set! *warn-on-reflection* true)

(defn inputs-hash
  [inputs]
  (digest/sha256-hex (pr-str inputs)))

(def sensitive-keys
  #{:password :secret :token :api-key :api-token :auth-token})

(defn- redact-inputs
  [x]
  (cond
    (map? x)        (reduce-kv (fn [m k v]
                                 (assoc m k (if (contains? sensitive-keys k)
                                              "[REDACTED]"
                                              (redact-inputs v))))
                               {} x)
    (vector? x)     (mapv redact-inputs x)
    (sequential? x) (mapv redact-inputs x)
    (set? x)        (into #{} (map redact-inputs) x)
    :else           x))

(defn- fresh-run-id [] (UUID/randomUUID))

(defn- run-row
  [store branch {:keys [id kind inputs working-branch actor options targets]}]
  (merge {:id (or id (fresh-run-id)) :kind kind
          :actor (or actor (grants/current-user-id (grants/actor store)))
          :inputs-hash (inputs-hash inputs) :inputs (redact-inputs inputs)
          :parent-run-id (run-rows/latest-succeeded-run-id store branch)
          :working-branch working-branch
          :owner (node/id) :lease-ms node/lease-ms}
         (when targets {:targets targets})
         (select-keys options [:retry-count :retry-backoff-ms :abort-on-failure])))

(defn- start-run!
  [store branch run]
  (run-rows/start-run! store branch (run-row store branch run)))

(defn- finish-facts [{:keys [counts summary snapshot-id]}]
  {:objects-count (or (:objects counts) 0)
   :links-count   (or (:links counts) 0)
   :summary       (some-> summary redact-inputs)
   :commit-hash   snapshot-id})

(defn- finish-run!
  [store branch run-id facts]
  (run-rows/finish-run! store branch run-id (finish-facts facts)))

(defn- finished-run-entry
  [store opts summary]
  (run-rows/finished-run-entry store (:branch store) (run-row store (:branch store) opts)
                               (finish-facts {:summary summary
                                              :snapshot-id (dsets/current-version store)})))

(defn name-rids
  [schema s]
  (attempt/or-answer s (rid/name-rids-in (sch/rid-name-index schema) s)))

(declare describe-throwable*)

(defn describe-throwable
  [schema ^Throwable t]
  (name-rids schema (describe-throwable* t)))

(defn- describe-throwable*
  [^Throwable t]
  (let [msg (or (.getMessage t) (str (type t)))
        {:keys [cause url status attempts stderr]} (ex-data t)
        cause-s (when cause
                  (if (instance? Throwable cause)
                    (or (.getMessage ^Throwable cause) (str (type cause)))
                    (str cause)))
        stderr-s (some-> stderr str str/trim not-empty
                         (->> (enc/elide 400)))
        bits (cond-> []
               (and cause-s (not= cause-s msg)) (conj (str "cause: " cause-s))
               url    (conj (str "url: " url))
               status (conj (str "worker status: " status))
               (and attempts (> (long attempts) 1)) (conj (str "after " attempts " attempts"))
               stderr-s (conj (str "stderr: " stderr-s)))]
    (if (seq bits)
      (str msg " (" (str/join "; " bits) ")")
      msg)))

(defmethod cmd/plan :run/fail
  [store {:keys [run-id error-message working-branch summary]} at]
  (run-rows/failing-run store (:branch store) run-id
                        {:error-message error-message :working-branch working-branch
                         :summary (some-> summary redact-inputs)}
                        at))

(defn settle-failed-run!
  [store run-effects facts]
  (cmd/run-command! store
                    #(ledger/failed-job-count % (:run-id facts))
                    #(into run-effects (ledger/settle-failed-run % facts))))

(defn list-recent
  ([store] (list-recent store 50 nil nil))
  ([store limit] (list-recent store limit nil nil))
  ([store limit kind] (list-recent store limit kind nil))
  ([store limit kind within-seconds]
   (run-rows/recent-runs store (:branch store) limit kind within-seconds)))

(defn get-run
  [store run-id]
  (or (run-rows/fetch-run store (:branch store) run-id)
      (run-rows/fetch-run-anywhere store run-id)))

(defn- retract-open-jobs!
  [store branch run-id]
  (let [branch-store    (ctx/at store branch)
        schema (seat/branch-schema branch-store)]
    (doseq [{:keys [status snapshot_id transform_rid]} (ledger/jobs-of-run branch-store run-id)
            :when (contains? #{"RUNNING" "ABORT_PENDING" "RUN_PENDING"} status)
            :when snapshot_id]
      (ledger/retract-outputs! branch-store snapshot_id
                               (ledger/declared-outputs
                                schema (some #(when (= (str %) (str transform_rid)) %)
                                             (keys (:transforms schema))))))))

(defn reap-orphans!
  ([store] (reap-orphans! store 120000))
  ([store min-age-ms]
   (let [reaped (bm/reap-zombies! store min-age-ms)
         at     (ctl/stamp store)]
     (doseq [{:keys [id branch reason]} reaped]
       (noted/quietly "reap orphans" (retract-open-jobs! store branch id))
       (noted/quietly "reap orphans"
         (settle-failed-run! (ctx/at store branch) []
                             {:run-id id :branch branch :error reason :at at})))
     (count reaped))))

(defn- touched-object-types
  [schema body-result]
  (into []
        (comp (keep identity)
              (keep #(lookup/->type-rid schema :object-types %))
              (distinct))
        (concat (:touched-types body-result)
                (keys (:rows body-result))
                (keep :type (filter tv/object-write?
                                    (or (get-in body-result [:result :tx-vector])
                                        (get-in body-result [:action-result :tx-vector])
                                        (:tx-vector body-result)))))))

(defn- written-object-ids
  [schema body-result]
  (reduce (fn [m {:keys [type payload]}]
            (if-let [t (lookup/->type-rid schema :object-types type)]
              (update m t (fnil conj []) (str (:id payload)))
              m))
          {}
          (filter tv/object-write?
                  (or (get-in body-result [:result :tx-vector])
                      (get-in body-result [:action-result :tx-vector])
                      (:tx-vector body-result)))))

(defn- announce!
  [branch schema body-result payload]
  (let [written (written-object-ids schema body-result)]
    (doseq [t (touched-object-types schema body-result)]
      (watch/notify! branch t (cond-> (assoc payload :at (str (java.time.Instant/now)))
                                (seq (get written t)) (assoc :ids (vec (get written t))))))))

(defn- record-edits!
  [branch-store branch result]
  (when-let [ops (seq (:tx-vector result))]
    (try
      (ledger/record-edit-transactions!
       branch-store
       {:branch branch :snapshot-id (dsets/current-version branch-store)
        :ops ops :run-id (str (:action-id result))
        :actor (some-> (:actor branch-store) str) :at (ctl/stamp branch-store)})
      (catch InterruptedException e (throw e))
      (catch Exception e (noted/note "edit-txn ledger record" e)))))

(defn announce-action!
  [branch-store branch schema result]
  (record-edits! branch-store branch result)
  (announce! branch schema {:result result} {:action-id (some-> (:action-id result) str)})
  (watch/wake!))

(defn- dead-jobs
  [store run-id]
  (into #{}
        (comp (filter #(#{"FAILED" "ABORTED" "ABORT_PENDING"} (:status %)))
              (map (comp str :job_rid)))
        (ledger/jobs-of-run store run-id)))

(defn- completed-writes
  [dead run-id body-result]
  (let [dead-job? (fn [jr] (contains? dead (str jr)))]
    {:dataset-writes (vec (remove (comp dead-job? :job-rid) (:dataset-writes body-result)))
     :consumed       (vec (remove #(dead-job? (ledger/job-rid run-id (:consumer-rid %)))
                                  (:consumed body-result)))}))

(defn- outcome
  [branch {:keys [id in-commit?]} {:keys [counts summary failure run-committed?]} writes
   snapshot-id at]
  (let [ends? (not (or failure run-committed?))]
    {:start?       (boolean (and in-commit? ends?))
     :finish       (when ends? {:counts counts :summary summary :snapshot-id snapshot-id})
     :transactions (when (seq (:dataset-writes writes))
                     {:branch branch :run-id id :snapshot-id snapshot-id
                      :writes (:dataset-writes writes) :consumed (:consumed writes) :at at})}))

(defn- run-result
  [body-result id snapshot-id ms-commit]
  (-> body-result
      (assoc :run-id id :snapshot-id snapshot-id)
      (update :phase-timings (fnil into []) [{:phase :run-commit :ms ms-commit}])))

(defn- failure-facts
  [schema branch {:keys [id]} ^Throwable t at]
  (let [error (describe-throwable schema t)]
    [[{:op            :run/fail
       :run-id        id
       :error-message error
       :summary       (not-empty (select-keys (ex-data t)
                                              [:publish-failures :failed-jobs :aborted-jobs]))}]
     {:run-id id :branch branch :error error :at at
      :attributed? (boolean (:jobs-attributed? (ex-data t)))}]))

(defn- run-body!
  [store schema {:keys [in-commit?] :as run} body-fn]
  (let [r (if in-commit?
            (body-fn store schema #(finished-run-entry store run %))
            (body-fn store schema))]
    (when-not (map? r)
      (throw (ex-info "with-run! body must return a map" {:got (type r)})))
    r))

(defn- end-run!
  [store branch schema {:keys [id] :as run} body-result]
  (let [t-cm        (System/nanoTime)
        snapshot-id (dsets/current-version store)
        ms-commit   (Math/round (double (/ (- (System/nanoTime) t-cm) 1e6)))
        writes      (if (:failure body-result)
                      (completed-writes (dead-jobs store id) id body-result)
                      body-result)
        {:keys [start? finish transactions]}
        (outcome branch run body-result writes snapshot-id (ctl/stamp store))]
    (when start? (start-run! store branch run))
    (when finish (finish-run! store branch id finish))
    (when transactions (ledger/commit-transactions! store transactions))
    (announce! branch schema body-result {:run-id id})
    (run-result body-result id snapshot-id ms-commit)))

(defn- fail-run!
  [store branch schema {:keys [id in-commit?] :as run} t]
  (when (and in-commit? (nil? (run-rows/fetch-run store branch id)))
    (noted/quietly "fail run" (start-run! store branch run)))
  (noted/quietly "fail run" (retract-open-jobs! store branch id))
  (noted/quietly "fail run"
    (apply settle-failed-run! store
           (failure-facts schema branch run t (ctl/stamp store)))))

(defn do-run!
  [store {:keys [id kind inputs options targets in-commit? schema]} body-fn]
  (when-not kind
    (throw (ex-info "run!: missing :kind" {})))
  (when (and in-commit? (not id))
    (throw (ex-info "run!: :in-commit? needs the run's :id" {})))
  (let [id     (or id (fresh-run-id))
        ended? (volatile! false)
        branch (:branch store)
        schema (or schema (seat/branch-schema store))
        run    {:id id :kind kind :inputs inputs :options options :targets targets
                :in-commit? in-commit?}]
    (bm/run-began! id)
    (when-not in-commit? (start-run! store branch run))
    (try
      (let [body-result (run-body! store schema run body-fn)
            answer      (end-run! store branch schema run body-result)]
        (vreset! ended? true)
        (some-> (:failure body-result) throw)
        answer)
      (catch Throwable t
        (noted/quietly "run note" (bm/note-final! id))
        (fail-run! store branch schema run t)
        (throw t))
      (finally
        (bm/unregister! id)
        (bm/run-ended! id)
        (when @ended? (watch/wake!))))))

(defmacro with-run!
  {:arglists '([[[store-sym schema-sym] store opts] & body])}
  [bindings & body]
  (let [[[store-sym schema-sym] store opts] bindings]
    `(do-run! ~store ~opts (fn [~store-sym ~schema-sym] ~@body))))

(defn- sweep-orphans!
  [store]
  (let [n (reap-orphans! store)]
    (when (pos? n) (println (str "[runs] closed " n " run(s) whose process died")))))

(def module
  {:name        :runs
   :jobs        [{:name :orphan-sweep :every "SUTRA_ORPHAN_SWEEP_MS" :tick sweep-orphans!
                  :default 15000}]
   :provides
   {:builds/landing
    {:run-fn           do-run!
     :announce-action! announce-action!
     :reset-consumer!  ledger/reset-consumer!
     :guard-reset!     (fn [store writes f]
                         (build-lock/with-claim
                           store {:run-id (random-uuid) :branch (:branch store)
                                  :reads [] :writes writes :origin "reconcile"}
                           f))
     :reregister-link! reconcile/reregister-link-backing!}}
   :contributes
   {:cluster/listeners {:build/cancel (fn [{:keys [run-id]}] (bm/request-cancel! run-id))}}})
