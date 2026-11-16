(ns sutra.actions.action
  (:require [sutra.foundation.side-effect :as fx]
            [sutra.kit.effects :as effects]
            [sutra.store.access.scope :as scope]
            [sutra.store.read-only :as read-only]
            [sutra.foundation.ops :as op]
            [sutra.kit.anomaly :as anomaly]
            [sutra.kit.refusal :as refusal]
            [malli.core :as m]
            [sutra.catalog.signature :as signature]
            [sutra.store.world :as world]
            [sutra.store.tx-vector :as tv]
            [sutra.actions.ports :as ports]
            [sutra.sources.api :as sources]
            [sutra.types.media :as mt]
            [sutra.blobs.api :as blobs]
            [sutra.media.api :as media]
            [sutra.types.rid :as rid]
            [sutra.types.object-set :as os]
            [sutra.ref.action :as ra]
            [sutra.ref.kernel :as rk]
            [sutra.ref.lower :as lower]
            [sutra.ref.objectset :as ros]
            [sutra.types.run-limits :as limits]
            [sutra.foundation.contracts :as ct]
            [sutra.catalog.actions :as actions]
            [sutra.catalog.functions :as functions]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.lookup :as lookup]
            [sutra.governance.api :as governance]
            [sutra.catalog.branch-policy :as policy]
            [sutra.actions.logic :as logic]
            [sutra.actions.execution :as x]
            [sutra.store.access.run-log :as run-log]
            [sutra.catalog.kernel :as kernel]
            [sutra.store.kept :as kept]
            [sutra.actions.operations :as ops]
            [sutra.types.branch-kinds :as branch-kinds]
            [sutra.store.served :as served]
            [sutra.catalog.backing-checks :as backing]
            [sutra.store.storage.instance-config :as ic]
            [sutra.types.attribute :as attribute]))

(set! *warn-on-reflection* true)

(def seal-action-name
  :sys/scenario-sealed)

(defn- within-load-limit
  [run ir]
  (let [left (x/objects-left run)
        q    (when (and left (os/flat? ir)) (os/ir->qt ir))]
    (if (and q (not (:aggregate q)))
      (os/qt->ir (update q :limit #(min (or % Long/MAX_VALUE) (inc left))))
      ir)))

(defn- read-fn [ctx io query-name args]
  (let [schema      (:schema ctx)
        spec        (os/by-name (:object-sets schema) query-name)
        _ (when-not spec
            (refusal/refuse! :refusal.actions/unknown-object-set {:query-name query-name}))
        _ (if (:id spec)
            (grants/authorize-read! (:authority ctx) spec (:id spec))
            (doseq [rid (distinct (keep :root (os/nodes (os/definition spec))))]
              (grants/authorize-read! (:authority ctx) (get-in schema [:object-types rid])
                                      rid)))
        qt (ros/instantiate (os/definition spec) (or args {}))
        result (x/read-store ctx io [:select (within-load-limit (:pass io) qt)])]
    (x/loaded! io (count (if (map? result) (:data result) result)))
    result))

(def ^:private write-targets
  {:create         {:section :object-types :marked :object}
   :update         {:section :object-types :marked :object}
   :delete-object  {:section :object-types :marked :object}
   :apply-marking  {:section :object-types :marked :object}
   :remove-marking {:section :object-types :marked :object}
   :compute-on     {:section :object-types}
   :create-link    {:section :link-types :marked :link}
   :delete-link    {:section :link-types :marked :link}})

(def ^:private edits
  #{:create :update :delete-object :compute-on :create-link :delete-link})

(defn declared-action?
  [schema action]
  (boolean (and action (not (contains? actions/platform-actions action))
                (some? (lookup/action schema action)))))

(defn- make-authorizer
  [blocked? schema auth declared?]
  (let [denied-markings (grants/denied-markings auth)
        hidden          (grants/hidden-object-types auth)]
    (fn [op target _bindings]
      (let [project-mgmt? (= :projects (:type target))
            section (and (not project-mgmt?) (:type target)
                         (get-in write-targets [op :section]))
            spec    (when section (get-in schema [section (:type target)]))
            proj    (if project-mgmt? (:id target) (:project spec))
            ignore? (boolean (:ignore-inherited spec))
            via?    (and declared? (contains? edits op))
            needed  (cond project-mgmt? :manage-permissions
                          via?          :read
                          :else         (grants/op->operation op))]
        (when (and via? (= :object-types section) (contains? hidden (:type target)))
          (refusal/refuse! :refusal.actions/acl-write-hidden-type
                           {:sutra/acl-denied true :op op :target target}))
        (when-not (or (nil? needed) (grants/allows? auth proj ignore? needed))
          (refusal/refuse! :refusal.actions/rbac-operation-denied
                          {:sutra/acl-denied true :op op :target target
                           :operation needed :project proj
                           :operations
                           (some-> (grants/effective-operations auth proj ignore?) vec)})))
      (let [{:keys [type id]} target
            kind (get-in write-targets [op :marked])]
        (when (and kind type id (seq denied-markings)
                   (blocked? kind type id))
          (refusal/refuse! :refusal.actions/acl-markings-denied
                          {:sutra/acl-denied true :op op :target target
                           :denied (vec denied-markings)}))))))

(def ^:private run-modes
  {:dispatch {}
   :dry-run  {:dry-run? true}
   :stage    {:sources-at-merge? true}
   :propose  {:mode :propose}})

(def ^:private run-ctx-schema
  [:map
   [:read-fn fn?]
   [:reads? :boolean]
   [:dry-run? {:optional true} :boolean]
   [:mode {:optional true} [:enum :propose :collect]]
   [:prior {:optional true} [:maybe [:sequential map?]]]
   [:installed {:optional true} [:maybe map?]]])

(defn- run-mode
  [as]
  (or (run-modes as) (anomaly/raise! :fault (str "no run mode " as) {:as as})))

(defn- installer
  [schema installed {:keys [sources-at-merge?]} project]
  (cond-> {:persisted (or installed schema)
           :accept!   (when-not sources-at-merge? backing/validate-external-sources!)}
    (kept/held schema)
    (assoc :via (fn [W st checked]
                  (kept/staged-step W schema st (force project) checked)))))

(defn- check-ctx!
  [fields as]
  (when (ct/checking?)
    (when-let [e (m/explain run-ctx-schema fields)]
      (anomaly/raise! :fault "run context contract violation" {:explain e :as as}))))

(defn store-io
  [store code]
  (assoc (x/source store)
         :sql-gate       (kernel/sql-gate store code)
         :derived-reader (ops/derived-reader store)
         :lm-service     (delay (ic/lm-service-config store))))

(defn- run-io
  [{:keys [store trace project]} pass {:keys [schema installed authority declared-action?] :as ctx}
   mode]
  (let [io (x/io (merge (some-> store (store-io (:code ctx)))
                        {:pass        pass
                         :trace       trace
                         :project     project
                         :installer   (installer schema installed mode project)}))]
    (assoc io :authorize (make-authorizer (x/blocking (x/reader ctx io) authority)
                                          schema authority declared-action?))))

(defn run-for
  [req pass & {:keys [as] :or {as :dispatch} :as extra}]
  (let [mode  (run-mode as)
        store (:store req)
        ctx   (merge {:read-fn read-fn :reads? true :code (scope/code)
                      :authority (:authority store) :actor (some-> store grants/acting-for)}
                     {:branch (or (:branch req) (:branch store))}
                     (select-keys req [:schema :effect-gate :asks-models? :action-id :provenance])
                     {:action-name (:action req)}
                     (dissoc mode :sources-at-merge?)
                     (dissoc extra :as))
        ctx   (assoc ctx :declared-action? (declared-action? (:schema ctx) (:action req)))]
    (check-ctx! ctx as)
    [ctx (run-io req pass ctx mode)]))

(defn- step-verbs []
  (merge logic/verbs (ports/action-verbs)))

(defn rule-ops []
  (-> lower/surface-ops (into lower/entity-ops) (into (keys (step-verbs)))))

(def ^:private carried
  [:branch :code :reads? :installed :prior :read-fn :schema :action-type :declared-action?
   :mode :authority :actor :asks-models? :action-id :action-name :provenance])

(defn interp-ctx
  [ctx]
  (cond-> (assoc (select-keys ctx carried)
                 :staged?     (boolean (:staged? ctx))
                 :dry-run?    (boolean (:dry-run? ctx))
                 :verbs       (step-verbs)
                 :sigma-fn    ops/sigma
                 :render      ops/render
                 :effect-gate (or (:effect-gate ctx) (policy/request-gate (:branch ctx))))
    (= :propose (:mode ctx)) logic/proposing))

(defn guard-tx-vector!
  [txv]
  (when (ct/checking?)
    (when-let [problem (tv/explain-tx-vector txv)]
      (anomaly/raise! :fault
                      "write-seam contract violation (tx-vector)" {:explain problem :txv txv})))
  txv)

(defn effect-opts
  [{:keys [actor authority]}]
  {:actor actor :authority authority})

(defn- held-back
  [gate txv]
  (policy/held-back
   (keep (fn [e]
           (or (:side-effect e)
               (when (= :notify (:op e))
                 (let [fx (fx/side-effect gate :notifications {:channel (:channel e)})]
                   (when-not (= :allow (:verdict fx)) fx)))))
         txv)))

(defn- commit-actor
  [{:keys [actor action-name action-id provenance]}]
  (cond-> {:principal (some-> actor str)}
    action-name         (assoc :action (name action-name))
    action-id           (assoc :action-id (str action-id))
    (:agent provenance) (assoc :agent (str (:agent provenance)))
    (:run-id provenance) (assoc :agent-run (str (:run-id provenance)))))

(defn- run-commit
  [{:keys [commit-on records records-fn] :as env} {:keys [installed]} txv logs]
  (let [ops (filterv tv/store-write? txv)
        {:keys [read-set] :as on} (or commit-on (x/commit-basis nil))]
    (signature/installing
     (merge (dissoc on :read-set)
            {:fences  (into (vec (:fences on)) (x/write-fences read-set ops))
             :ops     ops
             :log     (into (run-log/effect-entries (remove tv/store-op? txv)
                                                    (effect-opts env))
                            logs)
             :records (into (vec records) (some-> records-fn (apply [txv installed])))
             :actor   (commit-actor env)})
     installed)))

(defn world-before
  [store {:keys [prior]}]
  (if prior (:authored prior) (world/authored (world/at store (:branch store)))))

(defn judge-end-world!
  [{:keys [world] :as opts} before store]
  (when world
    (let [v (kept/verdict (kept/db store (:branch store)) world before)]
      (when-not (:ok v) (throw (kernel/schema-refusal (:reasons v) (:findings v))))))
  opts)

(defn try-save!
  [store {:keys [world ops log records] :as opts}]
  (when (or world (seq ops) (seq log) (seq records))
    (let [opts (assoc (dissoc opts :world) :allow-schema-wipe? (scope/schema-wipe-allowed?))]
      (if world
        (kept/commit! store (:branch store) world nil #(world/try-commit! store (merge opts %)))
        (world/try-commit! store opts)))))

(defn sweep-graphs!
  [store before head]
  (op/run! {:store store :auth grants/system-authority} :canvas/sweep-dropped-graphs
           {:branch (:branch store) :before (:graphs before) :after (:graphs head)}))

(defn save!
  [store {:keys [world] :as opts} before]
  (anomaly/then (try-save! store opts)
                (fn [r]
                  (when (and r world)
                    (sweep-graphs! store before (world/authored (assoc store :view nil))))
                  r)))

(defn commit-run!
  [store {:keys [world ops log records] :as opts}]
  (when (or world (seq ops) (seq log) (seq records))
    (let [before (when world (world-before store opts))]
      (save! store (judge-end-world! opts before store) before))))

(defn- commit-env
  [store ictx]
  (assoc (select-keys ictx [:commit-on :actor :effect-gate :records :records-fn
                            :action-name :action-id :provenance])
         :authority (:authority store)))

(defn save-unit
  [store ictx result]
  (let [txv  (guard-tx-vector! (into (vec (:ops result)) (:side-effects result)))
        logs (some-> (:log-fn ictx)
                     (apply [txv (cond-> result (:oracle ictx) (assoc ::oracle (:oracle ictx)))]))]
    (assoc result :tx-vector txv
           :commit-opts (run-commit (commit-env store ictx) result txv logs))))

(defn finish!
  [store ictx result]
  (let [{:keys [commit-opts] :as r} (save-unit store ictx result)
        c (commit-run! store commit-opts)]
    (anomaly/then c #(cond-> (dissoc r :commit-opts) % (assoc :commit %)))))

(defn- refused-as-value
  [f]
  (try (f)
       (catch clojure.lang.ExceptionInfo e
         (or (when-not (x/need e) (anomaly/of e)) (throw e)))))

(defn- evaluated
  [ctx io f]
  (let [ictx (interp-ctx ctx)]
    (refused-as-value
     #(hash-map :ictx   ictx
                :io     io
                :schema (:schema ctx)
                :result (kernel/run-ref (:schema ctx) (or (:bindings ctx) {}) ictx io f)))))

(defn eval-rules
  [ctx io rules]
  (evaluated ctx io #(rk/ev (lower/lower-seq rules) %1 %2)))

(defn eval-action
  [ctx io action-name args]
  (evaluated ctx io #(ra/invoke %1 %2 action-name args)))

(def ^:private logged-objects-max 25)

(defn edited-objects
  [schema tx-vector]
  (into []
        (comp (filter tv/object-write?)
              (map (fn [{:keys [type payload]}]
                     {:type (some-> (or (when (rid/rid? type)
                                          (lookup/name-of schema :object-types type))
                                        type)
                                    name)
                      :id   (str (:id payload))}))
              (filter :type)
              (distinct)
              (take logged-objects-max))
        tx-vector))

(defn- action-function-spec
  [schema action]
  (let [step (some-> (actions/resolve-action schema action) functions/function-rule)]
    (when (keyword? (:function step))
      (:spec (functions/function-at-version schema (:function step)
                                            (select-keys step [:version :auto-upgrade]))))))

(defn run-policy
  [schema {:keys [spec action label]}]
  (let [spec (or spec (when action (action-function-spec schema action)))
        t    (limits/function-timeout-ms spec)]
    {:label      label
     :timeout-ms (-> t (max (:min limits/timeout-ms)) (min (:max limits/timeout-ms)))}))

(defn action-policy
  [schema action]
  (run-policy schema {:action action :label (str "action " (some-> action name))}))

(defn logic-policy
  [schema fn-name]
  (run-policy schema {:spec  (:spec (functions/function-by-name schema fn-name))
                      :label (str "function " (name fn-name))}))

(defn performer
  [store session]
  (let [ps (merge logic/performers (ports/run-performers store))]
    (fn [{:keys [kind] :as request} remaining-ms]
      (let [f (or (get ps kind)
                  (anomaly/raise! :fault
                                  (str "no performer answers a " kind " request") {:kind kind}))]
        (effects/call! #(f request
                           {:store store :write-session session :timeout-ms remaining-ms}))))))

(defn evaluating
  ([req policy evaluate] (evaluating req policy evaluate nil))
  ([req policy evaluate session]
   (let [run (x/run (merge policy (select-keys (:oracle req) [:now :seed]) {:trace (:trace req)})
                    (performer (or (:performer-store req) (:store req)) session))
         {:keys [result facts]} (x/replaying run evaluate)]
     (cond-> result
       (and (map? result) (not (anomaly/refusal? result)))
       (assoc :facts facts :run run :store (:store req))))))

(defn run-held
  [req action args extra]
  (let [{:keys [ictx result]}
        (anomaly/unwrap!
         (evaluating req (action-policy (:schema req) action)
                     #(let [[ctx io] (run-for req % (assoc extra :as :stage))]
                        (eval-action ctx io action {:arg0 args}))))]
    (save-unit (:store req) ictx result)))

(defn add-unit
  [acc {:keys [ops log records]}]
  (-> acc (update :ops into ops) (update :log into log) (update :records into records)))

(def ^:private commit-attempts 3)

(defn- retryable?
  [ev r]
  (and (anomaly/refusal? r)
       (not (anomaly/refusal? ev))
       (not (:released? (:facts ev)))
       (contains? #{:conflict :stale-object} (:store/refusal r))))

(defn- reauthorize!
  [schema actx io txv]
  (let [writes    (filterv tv/store-write? txv)
        hidden    (x/blocked-among (x/reader actx io) (:authority actx)
                                   (for [{:keys [type payload] :as w} writes :when (:id payload)]
                                     [(if (tv/link-write? w) :link :object) type
                                      (str (:id payload))]))
        authorize (make-authorizer (fn [kind T id] (contains? hidden [kind T (str id)])) schema
                                   (:authority actx) (:declared-action? actx))]
    (doseq [{:keys [type payload] :as w} writes
            op (tv/authorized-as w)]
      (authorize op {:type type :id (:id payload)} nil))))

(defn applying
  [ictx io schema facts]
  (let [actx (-> ictx
                 (dissoc :mode)
                 (assoc :staged? false
                        :commit-on (x/commit-basis facts)
                        :verbs (step-verbs)))
        io   (assoc io :pass nil)]
    [actx (assoc io :authorize (make-authorizer (x/blocking (x/reader actx io) (:authority actx))
                                                schema (:authority actx)
                                                (:declared-action? actx)))]))

(defn- apply-collected
  [schema actx io result collected]
  (reduce (fn [acc {:keys [action args]}]
            (let [r (kernel/run-ref schema {} (assoc actx :ops (:ops acc)) io
                                    #(ra/invoke %1 %2 action {:arg0 args}))]
              (-> acc
                  (update :ops (fnil into []) (:ops r))
                  (update :side-effects into (:side-effects r)))))
          result collected))

(defn settle!
  [schema {:keys [ictx io store result collected facts run]}
   {:keys [recheck log-fn records records-fn]}]
  (let [[actx io] (applying ictx io schema facts)
        actx   (assoc actx :log-fn log-fn
                      :records (vec (remove nil? records))
                      :records-fn records-fn :oracle (x/oracle run))
        result (if (seq collected)
                 (:result (x/replaying (x/continued run)
                                       #(apply-collected
                                         schema actx (assoc io :pass %) result collected)))
                 result)]
    (x/edits-within-limit! result)
    (when (:released? facts)
      (reauthorize! schema actx io (into (vec (:ops result)) (:side-effects result)))
      (some-> recheck (apply [actx io])))
    (finish! store actx result)))

(defn run-provenance
  [{:keys [provenance authority]} result]
  (let [llm (for [e (:side-effects result) :when (= :use-llm (:op e))]
              (select-keys e [:model :source :request-ids :prompt-sha :media :turns]))]
    (not-empty (cond-> (or provenance {})
                 (:username authority) (assoc :actor (:username authority))
                 (seq llm)             (assoc :llm (vec llm))))))

(defn- logged
  [{:keys [schema action] :as req} log-args {:keys [recheck expect]}]
  {:recheck (when recheck #(recheck schema %1 %2))
   :log-fn  (fn [txv r]
              [(run-log/action-entry action log-args (edited-objects schema txv)
                                     (cond-> (or (run-provenance req r) {})
                                       (::oracle r) (assoc :oracle (::oracle r)))
                                     (:action-id req))])
   :records (into [expect] (:records req))})

(defn- with-held-log
  [settle session]
  (if-let [held (not-empty (sources/held-log session))]
    (let [log-fn (:log-fn settle)]
      (assoc settle :log-fn (fn [txv r] (into (vec (some-> log-fn (apply [txv r]))) held))))
    settle))

(defn- decided-and-settled
  [req policy evaluate settle session]
  (let [ev (evaluating req policy evaluate session)]
    [ev (anomaly/then ev #(settle! (:schema %) % (with-held-log settle session)))]))

(defn settled-run
  [req policy evaluate settle]
  (second (decided-and-settled req policy evaluate settle nil)))

(defn- with-write-session
  [f]
  (let [session (sources/write-session)
        r       (try (f session)
                     (catch Throwable e
                       (try (sources/end-write-session! session false)
                            (catch Throwable rollback (.addSuppressed e rollback)))
                       (throw e)))]
    (sources/end-write-session! session (not (anomaly/refusal? r)))
    r))

(defn- committed-run
  [req policy evaluate settle]
  (with-write-session
    (fn [session]
      (loop [attempt 1]
        (let [[ev r] (decided-and-settled req policy evaluate settle session)]
          (if (and (< attempt commit-attempts) (retryable? ev r))
            (recur (inc attempt))
            r))))))

(defn run-with-log!
  [{:keys [schema] :as req} policy evaluate log-args opts]
  (committed-run req (or policy (run-policy schema {:label "the rules"}))
                 #(apply evaluate (run-for req %))
                 (logged req log-args opts)))

(defn log-action!
  [store action-name payload]
  (run-log/append! store [(run-log/action-entry action-name payload nil nil)])
  nil)

(defn guard-action-request!
  [{:keys [authority branch action dry-run?]} args]
  (ct/check! :sutra.foundation.contracts/action-request
             {:authority authority
              :branch    branch
              :dry-run?  (boolean dry-run?)
              :action    action
              :args      {:arg0 args}})
  args)

(defn refuse-sys!
  [action]
  (when (and (keyword? action) (= "sys" (namespace action)))
    (refusal/refuse! :refusal.actions/sys-action-not-callable {:action action})))

(defn scenario-sealed?
  [store]
  (not= :open (ports/scenario-state store (:branch store))))

(defn- sealed-guarded
  [{:keys [branch replaying? store] :as req}]
  (if (and (not replaying?) (branch-kinds/scenario-branch? branch))
    (update-in req [:store :db] served/fenced [(ports/scenario-append-fence store branch)])
    req))

(defn on-branch-allowances
  [schema action-name]
  (into #{} (keep (fn [[k v]] (when (true? v) k)))
        (:on-branch (actions/resolve-action schema action-name))))

(defn- submission-failed!
  [{:keys [store action action-id]} e]
  (try (run-log/append! store
                        [(run-log/action-failed-entry
                          action action-id (anomaly/metric e) (anomaly/message e))])
       (catch Exception logged
         (.addSuppressed ^Throwable e logged)
         (println "[actions] failure of" action "not logged:" (ex-message logged)))))

(defn- effect-gate
  [schema action branch]
  (let [recips (get-in (actions/resolve-action schema action)
                       [:on-branch :notification-recipients])]
    (cond-> (policy/request-gate branch (on-branch-allowances schema action))
      recips (assoc :notification-recipients recips))))

(defn- admitted
  [req arg-maps dry-run?]
  (refuse-sys! (:action req))
  (doseq [a arg-maps]
    (guard-action-request! (-> (select-keys req [:authority :branch :action])
                               (assoc :dry-run? dry-run?))
                           a))
  (cond-> req (not dry-run?) sealed-guarded))

(defn- b64->bytes
  [b64]
  (.decode (java.util.Base64/getDecoder) (str b64)))

(defn- written-attribute-set
  [schema spec param]
  (some (fn [{:keys [object-type values] :as rule}]
          (let [attrs (into (set (map name (filter #{param} (:set rule))))
                            (keep (fn [[a v]] (when (= v [:get param]) (name a))))
                            values)]
            (when (and object-type (seq attrs))
              (let [ot (get-in schema [:object-types
                                       (lookup/rid-of schema :object-types object-type)])]
                (some #(when (attrs (name (:attribute-name %))) (mt/set-of %))
                      (mt/media-attributes (attribute/rows ot)))))))
        (:rules spec)))

(def ^:private media-param-codes
  {:not-a-media-value :refusal.actions/media-param-not-a-value
   :required          :refusal.actions/media-param-required
   :too-large         :refusal.actions/media-param-too-large})

(defn- with-media-addresses
  [schema action arg-maps]
  (let [spec (actions/resolve-action schema action)]
    (if (empty? (mt/media-params spec))
      {:arg-maps arg-maps}
      (reduce
       (fn [acc args]
         (when-let [p (mt/problem spec args mt/max-upload-bytes)]
           (refusal/refuse! (media-param-codes (:reason p))
                            (assoc (dissoc p :message) :action action)))
         (let [landed (for [[param env] (mt/uploads spec args)
                            :let [bs    (b64->bytes (:bytes-b64 env))
                                  pspec (get-in spec [:params param])
                                  mset  (or (mt/set-of pspec)
                                            (written-attribute-set schema spec param))]]
                        (do
                          (when mset
                            (when-let [p (media/land-problem schema mset (:mime env))]
                              (media/land-refusal! p {:parameter param :action action})))
                          {:parameter param :address (blobs/address-of bs) :bytes bs
                           :media-set mset :size (alength ^bytes bs) :mime (:mime env)
                           :path (or (not-empty (str (:filename env)))
                                     (blobs/address-of bs))}))]
           (-> acc
               (update :arg-maps conj
                       (reduce (fn [a {:keys [parameter address]}] (assoc a parameter address))
                               args landed))
               (update :pending into landed))))
       {:arg-maps [] :pending []}
       arg-maps))))

(defn- land-media!
  [{:keys [store branch]} pending]
  (doseq [{:keys [^bytes bytes media-set path address size mime]} pending]
    (blobs/put! (scope/blobs) bytes)
    (when media-set
      (media/record! (world/at store branch)
                     {:media-set media-set :path path :address address
                      :size size :mime mime}))))

(defn- submitted
  [{:keys [schema action branch action-id] :as req} arg-maps batch? pending]
  (let [gate   (effect-gate schema action branch)
        req    (admitted (assoc req :effect-gate gate) arg-maps false)
        invoke (if batch?
                 #(ra/invoke-batch %1 %2 action arg-maps)
                 #(ra/invoke %1 %2 action {:arg0 (first arg-maps)}))
        settle (logged req
                       (if batch? {:batch (count arg-maps) :args arg-maps} (first arg-maps))
                       {:recheck (fn [schema actx io]
                                   (doseq [a arg-maps]
                                     (ops/check-submission! schema actx io action {:arg0 a})))})]
    (anomaly/then
     (committed-run req (action-policy schema action)
                    #(let [[ctx io] (run-for req %)] (evaluated ctx io invoke))
                    settle)
     (fn [result]
       (land-media! req pending)
       (merge result (held-back gate (:tx-vector result)) {:action-id action-id})))))

(defn run-action!
  [{:keys [action args requests schema] :as req}]
  (when-not (governance/guarded? (:store req))
    (anomaly/raise! :fault
                    "an action runs on a store that passed governance/write-guard; this one did not"
                    {:action action :branch (:branch req)}))
  (let [req      (update req :action-id #(or % (random-uuid)))
        landed   (with-media-addresses schema action (or requests [args]))
        r        (try (submitted req (:arg-maps landed) (some? requests) (:pending landed))
                      (catch Throwable e
                        (submission-failed! req e)
                        (throw e)))]
    (when (anomaly/refusal? r) (submission-failed! req r))
    r))

(defn side-effect-failures
  [result]
  (vec (for [e (:side-effects result)
             :when (= :side-effect-failed (:op e))]
         (select-keys e [:rule-op :error]))))

(defn plan-action!
  [{:keys [action args schema] :as req}]
  (let [args (first (:arg-maps (with-media-addresses schema action [args])))
        req  (-> (cond-> (assoc req :dry-run? true :args args)
                   (:store req) (-> (assoc :performer-store (:store req))
                                    (update :store read-only/of)))
                 (admitted [args] true))]
    (try
      (evaluating req (action-policy schema action)
                  #(let [[ctx io] (run-for req % :as :dry-run)]
                     (:side-effects
                      (:result (anomaly/unwrap! (eval-action ctx io action {:arg0 args}))))))
      (catch clojure.lang.ExceptionInfo e
        (if-let [model (some #(:plan/model-not-asked (ex-data %))
                             (take-while some? (iterate ex-cause e)))]
          [{:op :use-llm :model model
            :planned "the model is asked when the action runs — its edits follow its answer"}]
          (throw e))))))
