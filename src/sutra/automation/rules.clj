(ns sutra.automation.rules
  (:require [sutra.foundation.workers :as workers]
            [sutra.foundation.periodic :as periodic]
            [sutra.foundation.settings :as settings]
            [sutra.kit.effects :as effects]
            [sutra.foundation.ops :as op]
            [sutra.store.access.scope :as ctx]
            [sutra.store.world :as world]
            [sutra.kit.anomaly :as anomaly]
            [sutra.kit.refusal :as refusal]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [sutra.actions.api :as actions]
            [sutra.objects.api :as objects]
            [sutra.store.seat :as seat]
            [sutra.automation.runs :as autos]
            [sutra.automation.executions :as executions]
            [sutra.store.storage.node :as node]
            [sutra.store.api :as api]
            [sutra.automation.logic-events :as logic-events]
            [sutra.automation.notify :as notify]
            [sutra.automation.activity :as activity]
            [sutra.store.access.watch :as watch]
            [sutra.schedules.api :as schedules]
            [sutra.catalog.functions :as functions]
            [sutra.catalog.problems :as problems]
            [sutra.catalog.lookup :as lookup]
            [sutra.catalog.object-ref :as oref]
            [sutra.ref.expr :as rexpr]
            [sutra.governance.api :as governance]
            [sutra.types.branch-kinds :as branch-kinds]
            [sutra.access.api :as access]
            [sutra.types.rid :as rid]
            [sutra.types.run-limits :as limits]
            [sutra.foundation.encoding :as enc])
  (:import (java.util.concurrent ExecutorService Future TimeUnit TimeoutException
                                 ExecutionException)))

(set! *warn-on-reflection* true)

(defn- objects-of-type
  [store schema type-kw]
  (objects/objects-of-type store schema (lookup/rid-of schema :object-types type-kw)
                           {:derived? true}))

(defn- domain-of [s]
  (let [a (some-> s str str/trim str/lower-case (str/replace #"\.+$" ""))]
    (when (seq a) (if (str/includes? a "@") (last (str/split a #"@")) a))))

(defn- reg-domain [s]
  (when-let [d (domain-of s)]
    (let [parts (str/split d #"\.")
          two #{"co.uk" "eu.com" "com.au" "co.at"}]
      (cond
        (and (>= (count parts) 3) (two (str/join "." (take-last 2 parts))))
        (str/join "." (take-last 3 parts))
        (>= (count parts) 2) (str/join "." (take-last 2 parts))
        :else d))))

(defn- literal
  [v]
  (if (keyword? v) (name v) v))

(defn- when->pred
  [w]
  (cond
    (or (nil? w) (= w :always)) true
    (vector? w) w
    (:and w) (into [:and] (map when->pred) (:and w))
    (:or  w) (into [:or]  (map when->pred) (:or w))
    (:not w) [:not (when->pred (:not w))]
    (:eq  w) (let [{:keys [attr value]} (:eq w)] [:= [:get attr] (literal value)])
    (:contains-any w) (let [{:keys [attr values]} (:contains-any w)]
                        (into [:includes-any [:get attr]] (map literal) values))
    :else (refusal/refuse! :refusal.automation/unrecognized-when {:when w})))

(declare ev)

(defn- domain-match
  [against {:keys [array-attr]} candidates]
  (when-let [fd (domain-of against)]
    (->> candidates
         (keep (fn [{:keys [id values]}]
                 (some (fn [e]
                         (when-let [ed (domain-of e)]
                           (when (or (= fd ed) (str/ends-with? fd (str "." ed)))
                             [(count ed) id])))
                       (get values array-attr))))
         (sort-by first >) first second)))

(defn- type-rid [schema t] (lookup/rid-of schema :object-types (keyword (name t))))

(defn- lift
  [f [v creates]]
  [(f v) creates])

(defn- resolve-ref
  [{:keys [schema rows] :as env} o {:keys [type match by create-if-missing]}]
  (let [rid      (type-rid schema type)
        cands    (get rows rid)
        [hit cs] (cond
                   by    (lift
                          (fn [target]
                            (some (fn [r]
                                    (when (rexpr/same-value? (get-in r [:values (:attr by)]) target)
                                      (:id r)))
                                  cands))
                          (ev env o (:eq by)))
                   match (lift #(domain-match % match cands) (ev env o (:against match)))
                   :else [nil []])]
    (if (or hit (not create-if-missing))
      [hit cs]
      (let [[new-id cs1] (ev env o (:id create-if-missing))
            [values cs2] (ev env o (:values create-if-missing))]
        [new-id (-> cs (into cs1) (into cs2)
                    (conj {:type rid :action (:action create-if-missing)
                           :args {:id new-id :values values}}))]))))

(defn- ref-id
  [{:keys [schema rows]} t pk]
  (when (some? pk)
    (let [rid  (type-rid schema t)
          cand (get rows rid)]
      (some-> (oref/resolve-ref schema rid pk
                                (fn [{[k v] :key id :id}]
                                  (if k
                                    (filterv #(= v (get-in % [:values k])) cand)
                                    (filterv #(= (str id) (str (:id %))) cand))))
              :object :id str))))

(def ^:private template-var #"\{\{\s*([^}\s]+)\s*\}\}")

(defn- ev
  [env o expr]
  (let [sub  #(ev env o %)
        vals (:values o)]
    (cond
      (vector? expr) (let [rs (mapv sub expr)] [(mapv first rs) (into [] (mapcat second) rs)])
      (not (map? expr)) [expr []]
      (contains? expr :self)       [(let [a (:self expr)] (if (= a :id) (:id o) (get vals a))) []]
      (contains? expr :domain-of)  (lift domain-of (sub (:domain-of expr)))
      (contains? expr :reg-domain) (lift reg-domain (sub (:reg-domain expr)))
      (contains? expr :slug)       (lift #(name (rid/slug-kw %)) (sub (:slug expr)))
      (contains? expr :pk-uuid)    (let [{:keys [type pk]} (:pk-uuid expr)]
                                     (lift #(enc/object-id (type-rid (:schema env) type) %)
                                           (sub pk)))
      (contains? expr :resolve)    (resolve-ref env o (:resolve expr))
      (contains? expr :ref)        (lift #(ref-id env (:ref expr) %) (sub (:pk expr)))
      (contains? expr :failure)    [(get (:failure env) (:failure expr)) []]
      (contains? expr :template)   [(str/replace (str (:template expr)) template-var
                                                 (fn [[_ p]]
                                                   (let [k (keyword p)]
                                                     (str (if (= k :id) (:id o) (get vals k ""))))))
                                    []]
      :else (let [rs (mapv (fn [[k v]] [k (sub v)]) expr)]
              [(into {} (map (fn [[k [v]]] [k v])) rs)
               (into [] (mapcat (comp second second)) rs)]))))

(defn- types-read
  [schema effect]
  (into #{}
        (comp (keep (fn [x] (when (map? x)
                              (cond (:resolve x) (type-rid schema (get-in x [:resolve :type]))
                                    (:ref x)     (type-rid schema (:ref x))))))
              (remove nil?))
        (tree-seq coll? seq effect)))

(defn- planned
  [env creates]
  (reduce (fn [env {:keys [type args]}] (update-in env [:rows type] (fnil conj []) args)) env
          creates))

(defn- create!
  [store schema creates]
  (doseq [{:keys [action args]} creates]
    (actions/dispatch-action! store schema action args)))

(def parallelism-default limits/logic-concurrency-default)

(defn parallelism-cap [] (settings/value :logic-parallelism-max))

(defonce ^:private logic-executor
  (delay (workers/pool "automate-logic-" (parallelism-cap))))

(defn- sized-executor
  ^ExecutorService []
  (workers/sized! @logic-executor (parallelism-cap)))

(defn- default-inputs
  [schema fn-name on]
  (let [spec (get (:functions schema) (lookup/rid-of schema :functions fn-name))
        ps   (for [[p t] (:param-types spec) :when (= (name t) (name on))] p)]
    (when-not spec
      (refusal/refuse! :refusal.automation/no-edit-function {:function fn-name}))
    (if (= 1 (count ps))
      {(first ps) {:self :id}}
      (refusal/refuse! :refusal.automation/ambiguous-object-parameter
                       {:function fn-name :candidates (vec ps)}
                       {:n (count ps) :on (name on)}))))

(defn- retry-policy
  [{:keys [retries retry]}]
  (let [{:keys [attempts-max delay-ms max-delay-ms jitter]} limits/logic-retry]
    {:attempts     (min attempts-max (or (:attempts retry) retries 0))
     :backoff      (or (:backoff retry) :exponential)
     :delay-ms     (or (:delay-ms retry) delay-ms)
     :max-delay-ms (or (:max-delay-ms retry) max-delay-ms)
     :jitter       (or (:jitter retry) jitter)}))

(defn backoff-ms
  [{:keys [backoff delay-ms max-delay-ms jitter]} i r]
  (let [base (min max-delay-ms (if (= :constant backoff)
                                 delay-ms
                                 (* delay-ms (Math/pow 2 (dec i)))))]
    (long (max 0 (+ base (* base jitter (dec (* 2 r))))))))

(defn- backoff-waits
  [policy draw]
  (map #(backoff-ms policy % (draw)) (range 1 (inc (:attempts policy)))))

(defn- backoff-bound-ms [{:keys [jitter] :as policy}]
  (reduce + 0 (map #(long (* (inc jitter) %))
                   (backoff-waits (assoc policy :jitter 0) (constantly 0.5)))))

(defn- sleep! [ms] (when (pos? ms) (Thread/sleep (long ms))))

(defn- with-retries
  [waits sleep! f]
  (loop [waits waits]
    (let [{:keys [ok error reached?]} (effects/attempt f)]
      (cond
        (not error) ok
        (and (seq waits) (not reached?)
             (not (logic-events/deterministic-limits (anomaly/run-limit error))))
        (do (sleep! (first waits))
            (recur (rest waits)))
        :else (throw error)))))

(defn- await-run
  [^Future f deadline-ms object-id wait-ms on-timeout]
  (try (.get f (max 0 (- deadline-ms (System/currentTimeMillis))) TimeUnit/MILLISECONDS)
       (catch TimeoutException _
         (.cancel f true)
         (let [msg (str "the Logic run did not finish within " wait-ms
                        " ms (its time limit per attempt,"
                        " plus margin) and was cancelled")]
           (merge {:object object-id :error msg} (on-timeout object-id msg))))
       (catch ExecutionException e
         {:object object-id :error (ex-message (or (.getCause e) e))})))

(defn- stored-values
  [schema on o]
  (let [rid     (lookup/rid-of schema :object-types (keyword on))
        derived (into #{} (map first) (when rid (:derived (lookup/attrs-index schema rid))))]
    (apply dissoc (:values o) derived)))

(defn- run-fallback!
  [store {:keys [action args]} o {:keys [schema] :as env} failure]
  (try
    (let [[resolved creates] (ev (assoc env :failure failure) o args)]
      (create! store schema creates)
      (actions/dispatch-action! store schema action resolved)
      {:action action :status "applied"})
    (catch InterruptedException e (throw e))
    (catch Exception t
      (println (str "[automate] fallback " action " on " (:id o) " failed: " (.getMessage t)))
      {:action action :status "failed" :error (.getMessage t)})))

(defn- logic-outcome [function mode proposals decisions applied]
  {:function  function
   :mode      mode
   :proposed  (count proposals)
   :applied   (mapv #(select-keys % [:action :args]) applied)
   :decisions (mapv #(-> (select-keys % [:model :source :prompt-sha :media])
                         (assoc :request-ids (vec (keep :request-id (:log %)))
                                :output (some :output (:log %))))
                    decisions)})

(defn- logic-run
  [store branch {auto-id :id auto-name :name}
   {:keys [function inputs mode once fallback] :as effect}
   schema on env
   {:keys [dry? manual? rerun-since with-model?] sleep :sleep! :or {sleep sleep!}}]
  (let [policy (retry-policy effect)
        spec   (:spec (functions/function-by-name schema function))
        t-ms   (:timeout-ms (actions/run-policy schema {:spec spec}))]
    {:store store :branch branch :auto-id auto-id :auto-name auto-name
     :schema schema :on on :env env
     :function function
     :mode     (or mode :apply)
     :inputs   (or inputs (default-inputs schema function on))
     :once once :fallback fallback
     :policy   policy
     :pin      (select-keys effect [:version :auto-upgrade])
     :wait     (+ (* (inc (:attempts policy)) t-ms) (backoff-bound-ms policy)
                  limits/await-margin-ms)
     :dry? dry? :manual? manual? :rerun-since rerun-since :with-model? with-model? :sleep sleep
     :claims   (atom {})}))

(defn- settle-failure!
  [{:keys [store branch auto-id auto-name env fallback]} {:keys [o fp claim]} msg transient]
  (let [{:keys [attempts claim-id event-id]} claim
        base {:object (:id o) :error msg :event-id event-id :transient transient}]
    (if-let [{:keys [retry-in-ms]}
             (logic-events/failed! store branch auto-id (:id o) fp
                                   {:attempts attempts :error msg
                                    :transient? transient :claim-id claim-id})]
      (let [fb (when (and fallback (zero? (or attempts 0)))
                 (let [r (run-fallback! store fallback o env
                                        {:message msg :event-id event-id
                                         :automation auto-name :automation-id auto-id})]
                   (logic-events/fallback! store branch auto-id (:id o) fp r claim-id)
                   r))]
        (cond-> base
          retry-in-ms (assoc :retry-in-ms retry-in-ms)
          fb          (assoc :fallback fb)))
      (assoc base :settled true))))

(defn- model-preview
  [{:keys [store schema function mode pin]} in]
  (try (let [{:keys [proposals decisions]}
             (actions/run-logic! store schema function in (assoc pin :mode :propose))]
         {:would-propose proposals
          :decisions     (:decisions (logic-outcome function mode proposals decisions []))})
       (catch InterruptedException e (throw e))
       (catch Exception t {:error (.getMessage t)})))

(defn- logic-preview [{:keys [store branch auto-id with-model?] :as lr} o in fp]
  (let [state (first (get (logic-events/state-of store branch auto-id [(:id o)]) (str (:id o))))
        seen  (when (= fp (:fingerprint state)) state)]
    (cond-> {:object (:id o) :inputs in
             :event  (if seen
                       (select-keys seen [:status :attempts :error :next-at])
                       {:status "new"})}
      with-model? (merge (model-preview lr in)))))

(defn- run-claimed!
  [{:keys [store branch auto-id auto-name schema function mode policy pin sleep]}
   {:keys [o fp claim]} in creates]
  (let [{:keys [claim-id event-id]} claim
        _          (create! store schema creates)
        held       (logic-events/held store branch auto-id (:id o) fp claim-id)
        provenance {:automation auto-name :automation-id auto-id
                    :event-id event-id :object (str (:id o))}
        {:keys [proposals decisions applied]}
        (with-retries (backoff-waits policy rand) sleep
          #(actions/run-logic! store schema function in
                         (assoc pin :mode mode :expect held :provenance provenance)))
        staged     (doall (keep #(actions/propose!
                                  store branch (assoc % :automation-id auto-id
                                                      :object-id (:id o)
                                                      :decision-log decisions))
                                proposals))
        outcome    (logic-outcome function mode staged decisions applied)]
    (logic-events/done! store branch auto-id (:id o) fp (assoc outcome :event-id event-id) claim-id)
    {:object    (:id o)
     :proposed  (count staged)
     :event-id  event-id
     :decisions (mapv #(select-keys % [:model :request-ids :output :prompt-sha])
                      (:decisions outcome))}))

(defn- logic-one
  [{:keys [store branch auto-id schema on env inputs once dry? manual? rerun-since wait claims
           function]
    :as lr}
   o]
  (let [[in creates] (ev env o inputs)]
    (when (every? some? (vals in))
      (let [fp (logic-events/fingerprint (or once :version) in (stored-values schema on o))]
        (if dry?
          (logic-preview lr o in fp)
          (if-let [claim (logic-events/claim! store branch auto-id (:id o) fp
                                              {:manual? manual? :rerun-since rerun-since
                                               :stale-ms wait})]
            (let [event {:o o :fp fp :claim claim}]
              (swap! claims assoc (:id o) event)
              (try
                (run-claimed! lr event in creates)
                (catch InterruptedException e (throw e))
                (catch Exception t
                  (println (str "[automate] logic effect " function " on " (:id o)
                                " failed: " (.getMessage t)))
                  (settle-failure! lr event (.getMessage t) (logic-events/transient? t)))))
            {:object (:id o) :skipped true}))))))

(defn- on-timeout
  [{:keys [claims] :as lr} oid msg]
  (if-let [event (get @claims oid)]
    (select-keys (settle-failure! lr event msg true)
                 [:event-id :transient :retry-in-ms :fallback])
    {}))

(defn- run-chunked
  [{:keys [wait] :as lr} matched n]
  (let [exec (sized-executor)]
    (reduce (fn [acc chunk]
              (let [until (+ (System/currentTimeMillis) wait)
                    futs  (mapv #(let [^Callable task (bound-fn [] (logic-one lr %))]
                                   (.submit exec task))
                                chunk)]
                (into acc (map (fn [o f] (await-run f until (:id o) wait (partial on-timeout lr)))
                               chunk futs))))
            [] (partition-all n matched))))

(defn- logic-summary
  [outs dry?]
  (let [outs (filterv some? outs)
        done (remove #(or (:error %) (:skipped %)) outs)]
    (cond-> {:affected  (if dry? [] (mapv :object done))
             :errors    (vec (keep :error outs))
             :proposed  (reduce + 0 (keep :proposed done))
             :skipped   (count (filter :skipped outs))
             :decisions (vec (keep #(when (seq (:decisions %))
                                      (select-keys % [:object :decisions]))
                                   done))
             :failures  (vec (keep #(when (:error %)
                                      (select-keys % [:object :error :event-id :transient
                                                      :retry-in-ms :fallback]))
                                   outs))}
      dry? (assoc :previews (vec done)))))

(defn- run-logic-effect
  [store branch auto {:keys [parallelism] :as effect} schema on matched env
   {:keys [dry? object-id] :as opts}]
  (let [lr      (logic-run store branch auto effect schema on env opts)
        matched (if object-id (filterv #(= (str object-id) (str (:id %))) matched) matched)
        n       (max 1 (min (parallelism-cap)
                            (or (:parallelism opts) parallelism parallelism-default)))]
    (logic-summary (run-chunked lr matched n) dry?)))

(defn- key-of
  [env key o]
  (let [vals (:values o)
        vars (when (and (map? key) (contains? key :template))
               (map (comp keyword second) (re-seq template-var (str (:template key)))))]
    (when (every? #(some? (if (= :id %) (:id o) (get vals %))) vars)
      (some-> (first (ev env o key)) str not-empty))))

(defn- staged-proposal
  [env auto-id {:keys [action reason provenance key]} o resolved]
  (let [ev* #(when (some? %) (first (ev env o %)))]
    (cond-> {:automation-id auto-id :object-id (:id o) :action action
             :args (if key (into {} (remove (comp nil? val)) resolved) resolved)}
      reason     (assoc :reason (some-> (ev* reason) str))
      provenance (assoc :provenance (into {} (remove (comp nil? val)) (ev* provenance)))
      key        (assoc :source-key (key-of env key o)))))

(defn- object-plan
  [env auto-id {:keys [args mode key] :as effect} o]
  (let [propose? (= :propose mode)
        replay?  (and propose? (some? key))
        [resolved creates] (ev env o args)]
    (cond
      (and (not replay?) (not (every? some? (vals resolved)))) {:object (:id o) :skip true}
      (and replay? (nil? (key-of env key o)))
      {:object (:id o) :error (str "object " (:id o) " has no value for the proposal :key")}
      :else
      (cond-> {:object (:id o) :args resolved :creates creates}
        propose? (assoc :proposal (staged-proposal env auto-id effect o resolved))))))

(defn- effect-plan
  [env auto-id effect matched]
  (first (reduce (fn [[plan env] o]
                   (let [p (try (object-plan env auto-id effect o)
                                (catch InterruptedException e (throw e))
                                (catch Exception t {:object (:id o) :error (.getMessage t)}))]
                     [(conj plan p) (planned env (:creates p))]))
                 [[] env] matched)))

(defn- effect-one!
  [store branch schema action propose? batched? dry? {:keys [auto-id run-id versions]} acc
   {:keys [object skip error args creates proposal]}]
  (cond
    skip  acc
    error (update acc :errors conj error)
    :else
    (try
      (when-not dry? (create! store schema creates))
      (if propose?
        (cond-> (update acc :affected conj object)
          (and (not dry?) (actions/propose! store branch proposal)) (update :proposed inc))
        (do (when-not (or dry? batched?)
              (actions/dispatch-action! store schema action args
                                  {:records [(autos/effect-entry store branch auto-id run-id
                                                                 object
                                                                 (get versions (str object)))]}))
            (-> acc (update :affected conj object) (update :batch conj args))))
      (catch InterruptedException e (throw e))
      (catch Exception t
        (println (str "[automate] effect on " object " failed: " (.getMessage t)))
        (update acc :errors conj (.getMessage t))))))

(defn- dispatch-batch!
  [store branch schema action batch affected {:keys [auto-id run-id versions]}]
  (try (actions/dispatch-action-batch!
        store schema action batch
        {:records (mapv #(autos/effect-entry store branch auto-id run-id %
                                             (get versions (str %)))
                        affected)})
       nil
       (catch InterruptedException e (throw e))
       (catch Exception t
         (println (str "[automate] batched effect over " (count batch)
                       " object(s) failed: " (.getMessage t)))
         (.getMessage t))))

(defn- planned-outcome
  [{:keys [affected errors proposed]} batch-error]
  {:affected (if batch-error [] affected)
   :errors   (cond-> errors batch-error (conj batch-error))
   :proposed proposed})

(defn- run-planned!
  [store branch schema {:keys [action mode]} plan batched? dry? run]
  (let [acc (reduce (partial effect-one! store branch schema action (= :propose mode)
                             batched? dry? run)
                    {:affected [] :errors [] :batch [] :proposed 0} plan)]
    (planned-outcome acc (when (and batched? (not dry?) (seq (:batch acc)))
                           (dispatch-batch! store branch schema action (:batch acc)
                                            (:affected acc) run)))))

(defn- run-action-effect
  [store branch auto-id {:keys [action mode] :as effect} schema matched env dry? run-id]
  (let [batched? (and (not= :propose mode) (functions/batched-action? schema action))
        store    (if (or dry? (= :propose mode)) store (governance/data-guarded store))]
    (run-planned! store branch schema effect (effect-plan env auto-id effect matched) batched? dry?
                  {:auto-id auto-id :run-id run-id
                   :versions (into {} (map (fn [o] [(str (:id o)) (api/object-version o)]))
                                   matched)})))

(defn scale-refusal
  [schema effect n manual?]
  (let [{:keys [scheduled-per-object-max manual-object-set-max batch-max]} limits/automate
        batched? (and (not (:function effect)) (not= :propose (:mode effect))
                      (functions/batched-action? schema (:action effect)))]
    (cond
      (and batched? (> n batch-max))
      (str n " objects matched, more than the batch size of " batch-max
           " an automation may hand one batched action — nothing was run")

      (and (not batched?) (not manual?) (> n scheduled-per-object-max))
      (str n " objects matched, more than the " scheduled-per-object-max
           " a scheduled automation may evaluate per object — nothing was run;"
           " narrow :when, or run it manually")

      (and (not batched?) (> n manual-object-set-max))
      (str n " objects matched, more than the " manual-object-set-max
           " a manual execution may take — nothing was run"))))

(declare run-one*)

(defn owner-authority
  [store owner branch]
  (when-not owner
    (refusal/refuse! :refusal.automation/no-owner {:rule :automation/no-owner}))
  (or (access/of-principal store branch owner)
      (refusal/refuse! :refusal.automation/owner-gone {:owner owner})))

(defn run-one!
  ([store branch auto] (run-one! store branch auto {}))
  ([store branch {:keys [owner] :as auto} opts]
   (let [{:keys [authority refusal]
          :as owner*} (try {:authority (owner-authority store owner branch)}
                           (catch InterruptedException e (throw e))
                           (catch Exception t
                             {:refusal (.getMessage t)}))]
     (if (contains? owner* :refusal)
       (do (when-not (:dry? opts)
             (autos/record-run! store branch (:id auto)
                                {:matched 0 :applied 0 :status "error" :detail refusal}))
           {:matched 0 :applied 0 :affected-ids [] :errors [refusal] :proposed 0})
       (run-one* (ctx/act-for store authority) branch auto opts)))))

(defn- skips-handled?
  [effect {:keys [dry? manual?]}]
  (not (or manual? dry? (:function effect) (= :propose (:mode effect)))))

(defn- matching
  [objs pred done]
  (filterv #(and (problems/eval-pred (:values %) pred)
                 (not (contains? done [(str (:id %)) (str (api/object-version %))])))
           objs))

(defn object-set
  [objs {:keys [ids where]}]
  (let [ids  (some->> ids (into #{} (map str)))
        pred (some-> where when->pred)]
    (filterv #(and (or (nil? ids) (contains? ids (str (:id %))))
                   (or (nil? pred) (problems/eval-pred (:values %) pred)))
             objs)))

(defn- effect-kind
  [effect refusal dry? muted?]
  (cond (and muted? (not dry?)) :muted
        refusal            :refused
        (:function effect) :function
        (:notify effect)   (if dry? :notify-preview :notify)
        :else              :action))

(defn- run-summary
  [matched {:keys [affected errors proposed decisions previews skipped failures]}]
  (cond-> {:matched (count matched) :applied (count affected)
           :affected-ids affected :errors errors :proposed proposed}
    (seq decisions) (assoc :decisions decisions)
    (seq failures)  (assoc :failures failures)
    skipped         (assoc :skipped skipped)
    previews        (assoc :previews previews)))

(defn- mark-handled!
  [store branch id run-id schema on affected]
  (let [hit (into #{} (map str) affected)]
    (autos/mark-versions! store branch id run-id
                          (into {} (keep (fn [o] (when (hit (str (:id o)))
                                                   [(str (:id o)) (api/object-version o)])))
                                (objects-of-type store schema (keyword on))))))

(defn- run-one*
  [store branch {:keys [id on when-spec effect-spec] :as auto}
   {:keys [dry? manual? objects label] :as opts}]
  (let [store   (ctx/at store branch)
        schema  (seat/branch-schema store)
        pred    (when->pred (when (seq when-spec) (edn/read-string when-spec)))
        effect  (edn/read-string effect-spec)
        objs    (objects-of-type store schema (keyword on))
        env     {:schema schema
                 :rows   (into {} (map (fn [rid]
                                         [rid (objects/objects-of-type
                                               store schema rid {:derived? true})]))
                               (types-read schema effect))}
        matched (if objects
                  (object-set objs objects)
                  (matching objs pred
                            (when (skips-handled? effect opts)
                              (autos/applied-versions store branch id))))
        run-id  (str (random-uuid))
        refusal (when-not (:notify effect)
                  (scale-refusal schema effect (count matched) (boolean manual?)))
        outcome (case (effect-kind effect refusal dry? (:muted auto))
                  :muted          {:affected [] :errors [] :proposed 0
                                   :skipped {:muted (count matched)}}
                  :refused        {:affected [] :errors [refusal] :proposed 0}
                  :function       (run-logic-effect store branch auto effect schema on matched
                                                    env opts)
                  :notify-preview {:affected [] :errors [] :proposed 0
                                   :previews (notify/previews effect matched)}
                  :notify         (notify/notify! store branch auto effect schema on matched
                                                  run-id)
                  :action         (let [r (run-action-effect store branch id effect schema
                                                             matched env (boolean dry?) run-id)]
                                    (when (and (not dry?) (not= :propose (:mode effect))
                                               (seq (:affected r)))
                                      (mark-handled! store branch id run-id schema on
                                                     (:affected r)))
                                    r))
        result  (run-summary matched outcome)]
    (when-not dry?
      (autos/mark-fired! store branch id (count matched))
      (let [run (assoc result
                       :run-id run-id
                       :status (cond (seq (:errors outcome)) "error"
                                     (:muted auto)           "muted"
                                     :else                   "ok")
                       :detail (not-empty (str/join "; " (cond->> (:errors outcome)
                                                           label (cons label)))))]
        (autos/record-run! store branch id run)
        (when-not (:muted auto) (activity/after-run! store branch auto run))))
    result))

(def execution-batch-default
  100)

(defn- read-where [w] (if (string? w) (edn/read-string w) w))

(defn execute!
  [store branch {:keys [id on] :as _auto} {:keys [ids where batch-size parallelism]}]
  (let [spec   (cond-> {}
                 (seq ids)     (assoc :ids ids)
                 (some? where) (assoc :where (read-where where)))
        _      (when (empty? spec)
                 (refusal/refuse! :refusal.automation/manual-no-objects {}))
        at     (ctx/at store branch)
        chosen (mapv (comp str :id)
                     (object-set (objects-of-type at (seat/branch-schema at) (keyword on)) spec))
        size   (max 1 (or batch-size execution-batch-default))
        x-id   (str (random-uuid))]
    (executions/create! store branch {:id x-id :automation-id id :pending chosen
                                      :batch-size size :parallelism parallelism})
    {:execution-id x-id :objects (count chosen) :batches (count (partition-all size chosen))
     :batch-size size}))

(defn- batch-label [{:keys [id total batches batch-size]}]
  (str "Ausführung " id " · Stapel " (inc (or batches 0)) "/"
       (long (Math/ceil (/ (or total 0) (double (max 1 batch-size)))))))

(defn run-batch!
  [store branch {:keys [id automation-id]}]
  (when-let [c (executions/claim! store branch id)]
    (if-let [auto (autos/get-one store branch automation-id)]
      (let [batch (executions/next-batch c)]
        (periodic/renewing
         "sutra-claim-heartbeat" (quot executions/claim-ms 3)
         #(executions/renew! store branch id (:claim-id c))
         #(try (run-one! store branch auto
                         {:manual? true :rerun-since (:created-at c) :objects {:ids batch}
                          :parallelism (:parallelism c) :label (batch-label c)})
               (catch InterruptedException e (throw e))
               (catch Exception t
                 (println (str "[automate] execution " id " batch " (inc (or (:batches c) 0))
                               " failed: " (.getMessage t))))))
        (executions/advance! store branch id (:claim-id c) (count batch)))
      (executions/cancel! store branch id))))

(defn- fire-and-log! [store branch a]
  (let [{:keys [matched applied]} (run-one! store branch a)]
    (println (str "[automate] fired '" (:name a) "' (" (:id a) ") → "
                  matched " matched, " applied " applied"))))

(defn live-branches
  [store]
  (let [ended (governance/ended-branches store)]
    (into [] (comp (filter branch-kinds/projectable-branch?) (remove ended))
          (map :name (world/branches store)))))

(defn executions-due
  [store]
  (vec (for [b (live-branches store)
             x (executions/running (ctx/at store b) b)]
         (assoc x :branch b))))

(defn live-due
  [store]
  (vec (for [b (live-branches store)
             :let [store  (ctx/at store b)
                   schema (delay (seat/branch-schema store))]
             {:keys [on last-fired-at paused] :as a} (autos/list-all store b)
             :when (and (:live a) (not paused))
             :let [t (some->> on keyword (type-rid @schema) (watch/changed-at b))]
             :when (or (nil? last-fired-at)
                       (and t (> t (.getTime ^java.util.Date last-fired-at))))]
         (assoc a :branch b))))

(op/defop :automation/list [{:keys [store]} {:keys [branch]}]
  (autos/list-all store branch))

(def module
  {:name :automate
   :jobs [{:name  :automate :every :automate-ms
           :tick  (schedules/each-due-across "automate" #(autos/list-due-on % (live-branches %))
                                          fire-and-log!
                                          #(str (:name %) " (" (:id %) ")"))}
          {:name    :automate-live :every "SUTRA_AUTOMATE_LIVE_MS" :default 30000
           :tick    (schedules/each-due-across "automate-live" live-due fire-and-log!
                                            #(str (:name %) " (" (:id %) ")"))}
          {:name    :automate-executions :every "SUTRA_AUTOMATE_EXECUTIONS_MS" :default 5000
           :tick    (let [tick (schedules/each-due-across "automate-executions" executions-due
                                                       run-batch! #(str "execution " (:id %)))]
                      (fn [store] (when (= :all node/role) (tick store))))}]
   :contributes {:watch/hooks  {:automate-live (fn [_ _] (schedules/poke-job! :automate-live))}
                 :repos/riders {:automations {:upsert! activity/saved! :delete! autos/delete!}}}})
