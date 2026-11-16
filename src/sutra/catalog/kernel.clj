(ns sutra.catalog.kernel
  (:require [sutra.kit.anomaly :as anomaly]
            [sutra.types.forms :as forms]
            [sutra.types.membership :as member]
            [clojure.string :as str]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.types.languages :as langs]
            [sutra.kit.refusal :as refusal]
            [sutra.kit.cache :as cache]
            [sutra.types.entities :as ents]
            [sutra.types.rid :as rid]
            [sutra.types.schema :as sch]
            [sutra.types.backing :as backing]
            [sutra.types.stream :as strm]
            [sutra.types.sql-traps :as traps]
            [sutra.foundation.contracts :as ct]
            [sutra.catalog.constraints :as cstr]
            [sutra.catalog.actions :as actions]
            [sutra.catalog.problems :as problems]
            [sutra.catalog.structure :as structure]
            [sutra.catalog.lookup :as lookup]
            [sutra.catalog.publishing :as publishing]
            [sutra.catalog.system-datasets :as sysds]
            [sutra.types.world-rules :as rules]
            [sutra.catalog.shape :as shape]
            [sutra.parser.lint :as lint]
            [sutra.parser.spec :as spec]
            [sutra.types.finding :as finding]
            [sutra.types.plan :as cp]
            [sutra.ref.expr :as rexpr]
            [sutra.ref.action]
            [sutra.ref.install :as ri]
            [sutra.ref.kernel :as rk]
            [sutra.ref.lower :as lower]
            [sutra.ref.clock :as clock]
            [sutra.ref.sigma :as sigma]
            [sutra.ref.value :as rv]))

(set! *warn-on-reflection* true)

(defn- introspection-failure
  [body ^Throwable e]
  (let [d (ex-data e)]
    {:transform (:name body)
     :state  (if (:exit d) :refused :unknown)
     :detail (or (not-empty (str (:stderr d))) (.getMessage e))}))

(def ^:private python-entries
  {:functions  {:entry :function  :refusal :refusal.foreign/function-transform-decorated}
   :transforms {:entry :transform :refusal :refusal.foreign/transform-function-decorated}})

(defn decorator-refusal
  [body entry]
  (when-let [{want :entry code :refusal} (python-entries (:sutra.code/section body))]
    (when (and entry (not= want entry))
      [code {:transform (:name body) :entry entry}])))

(defn- as-declared
  [inferred body]
  (let [stated (dissoc inferred :sutra.code/entry)
        target #(select-keys % [:kind :dataset :object-type])
        output (:output body)
        kept   (when (or (empty? (target output)) (= (target output) (target (:output stated))))
                 (select-keys output ents/output-ontology-fields))]
    (if (= :transform (:sutra.code/entry inferred))
      (cond-> (assoc (merge (apply dissoc body ents/decorator-fields) stated) :spec-source :code)
        (and (seq kept) (:output stated)) (update :output merge kept))
      (merge stated body))))

(defonce ^:private introspected (cache/lru 256))

(defn- introspected-spec
  [introspect {:keys [name code modules entry-point] :as body}]
  (cache/through introspected [introspect name code modules entry-point] #(introspect body)))

(defn- introspect-python-transform
  [code {:keys [language] :as body}]
  (if-let [introspect (and (= :python (some-> language ->kw)) (seq (:code body))
                           (:introspect-python code))]
    (let [[inferred failure]
          (try [(introspected-spec introspect body) nil]
               (catch Exception e
                 (if (contains? body :inputs)
                   [nil (introspection-failure body e)]
                   (refusal/refuse! :refusal.catalog/introspection-without-inputs
                                    {:transform (:name body)} nil e))))]
      (some->> (decorator-refusal body (:sutra.code/entry inferred)) (apply refusal/refuse!))
      [(as-declared inferred body) failure])
    [body nil]))

(defn- enrich-clojure-transform
  [code {:keys [language] :as body}]
  (if-let [introspect (and (= :clojure (some-> language ->kw)) (seq (:code body))
                           (:introspect-clojure code))]
    (let [inferred (introspect body)
          stated   (into {} (remove (comp nil? val)) body)]
      (if (seq inferred)
        (assoc (merge inferred stated) :spec-source :code)
        body))
    body))

(defn introspect-code-transform
  [code body]
  (let [[b failure] (introspect-python-transform code body)]
    [(enrich-clojure-transform code (dissoc b :sutra.code/section)) failure]))

(defn- transform-named
  [world nm]
  (let [r (rid/->name nm)]
    (if (rid/rid? r)
      (or (get-in world [:transforms r]) (get-in world [:transforms (keyword r)]))
      (some (fn [[_ t]] (when (= (->kw nm) (->kw (:transform-name t))) t))
            (:transforms world)))))

(def ^:private sql-body-keys [:language :query :inputs :parameters :incremental])

(defn- sql-refusals
  [store code old-world new-world k]
  (let [t  (transform-named new-world k)
        nm (or (:transform-name t) k)]
    (when (and (langs/query-language? (some-> (:language t) ->kw))
               (not= (select-keys t sql-body-keys)
                     (select-keys (transform-named old-world k) sql-body-keys)))
      (let [idx (rid/name->rid-index new-world)
            t*  (rid/rewrite-refs :transforms t
                                  (fn [targets v] (or (rid/resolve-name->rid idx targets v) v)))
            v   (when-let [explain (and (seq (str (:query t))) (:sql-explain code))]
                  (assoc (explain store new-world t*) :transform (->kw nm)))]
        (seq (filter #(= :error (:severity %))
                     (concat (when (= :postgres (langs/dialect (->kw (:language t))))
                               (traps/char-cast-findings (->kw nm) (:query t)))
                             (traps/verdict-findings (when v [v])))))))))

(defn sql-gate
  [store code]
  (partial sql-refusals store code))

(defn transform-origin
  [world {:keys [transform-name output outputs project repository]}]
  (let [idx  (rid/rid->name-index world)
        nm   #(rid/resolve-rid->name idx %)
        made (for [{:keys [kind] :as o} (cond-> (vec outputs) output (conj output))
                   :let [target (nm (get o kind))]
                   :when target]
               (str "produces " (name kind) " " (pr-str target)
                    (when (contains? (backing/generated kind target) (->kw transform-name))
                      (str " under the name the platform generates for its backing transform"))))
        owner (for [[k v] [[:project project] [:repository repository]] :when v]
                (str "in " (name k) " " (pr-str (nm v))))]
    (str/join ", " (concat made owner))))

(def install-cx
  {:sigma (sigma/check
           (merge clock/system
                  {:stream             strm/normalize
                   :transform-contract #(ct/check! :sutra.foundation.contracts/transform-contract %)
                   :unpack             shape/unpack-schema
                   :migrate-external   shape/migrate-external-source-to-transforms
                   :system-datasets    (constantly sysds/system-datasets)
                   :structure          problems/structural-findings
                   :structure-ok?      structure/valid-schema-structure?
                   :gate-problems      problems/gate-problems
                   :lint               #(lint/gate-violations % (spec/vocabulary))
                   :index              sch/indexed}))})

(def ^:private value-cx
  (assoc-in install-cx [:sigma :lint] #(lint/value-gate-violations % (spec/vocabulary))))

(defn install-cx-of
  [code]
  (assoc-in install-cx [:sigma :introspect] #(first (introspect-code-transform code %))))

(defn accept?
  ([W] (ri/accept? value-cx W nil))
  ([W old] (ri/accept? install-cx W old)))

(defn findings
  ([W] (finding/worst-first (mapv problems/word (ri/report value-cx W nil))))
  ([W old] (finding/worst-first (mapv problems/word (ri/report install-cx W old)))))

(defn reasons
  ([W] (problems/reasons (ri/report value-cx W nil)))
  ([W old] (problems/reasons (ri/report install-cx W old))))

(defn assessment
  [W]
  (let [report (ri/report value-cx W nil)]
    {:reasons  (problems/reasons report)
     :findings (finding/worst-first (mapv problems/word report))}))

(defn verdict
  [W old]
  (if (ri/accept? install-cx W old)
    {:ok W}
    (let [report (ri/report install-cx W old)]
      {:schema   W
       :reasons  (problems/reasons report)
       :findings (finding/worst-first (mapv problems/word report))})))

(defn step
  [code verb tag body]
  (ri/build (install-cx-of code) verb tag body))

(defn prepare
  [world step]
  (ri/prepare install-cx world step))

(defn- problem-line
  [{:keys [entity-class entity message]}]
  (str (str/join " " (remove nil? [(some-> entity-class name) (some-> entity pr-str)]))
       ": " message))

(defn schema-refusal
  [reasons findings]
  (or (cp/refusal (cp/of-findings findings)
                  {:code   :refusal.catalog/plan-problems
                   :data   {:sutra/gate :plan-problems}
                   :params (fn [bs] {:detail (str (count bs) " problem(s) — "
                                                  (str/join "; " (map problem-line bs)))})})
      (refusal/refusal :refusal.catalog/invalid-schema {:reasons reasons})))

(defn- sql-gate-refusal
  [fs data]
  (cp/refusal (cp/of-findings fs)
              {:code   :refusal.catalog/sql-gate
               :data   (merge {:sutra/gate :sql} data)
               :params (fn [bs] {:detail (str/join "\n" (map :message bs))})}))

(defn- worded-refusal [W e]
  (let [{k :ref/kind :keys [problems reasons existing] :as data} (ex-data e)]
    (case k
      :plan-problems    (schema-refusal problems (mapv problems/word problems))
      :invalid-schema   (schema-refusal reasons (mapv problems/word reasons))
      :sql-gate         (or (sql-gate-refusal problems (dissoc data :problems)) e)
      :transform-exists (refusal/refusal :refusal.catalog/transform-exists
                                         (assoc data :op :set :kind :transforms)
                                         {:origin (let [o (transform-origin W existing)]
                                                    (if (seq o) o "already exists"))}
                                         e)
      e)))

(defn apply-step
  [store code world step stepped]
  (try
    (let [W1      (stepped)
          [sec k] (:path step)]
      (when-let [r (and (= :transforms sec)
                        (sql-gate-refusal (sql-refusals store code world W1 k) nil))]
        (throw r))
      W1)
    (catch clojure.lang.ExceptionInfo e (throw (worded-refusal world e)))))

(defn judge!
  [code world old]
  (try (ri/judge (install-cx-of code) world old)
       (catch clojure.lang.ExceptionInfo e (throw (worded-refusal world e)))))

(defn- install-via
  [{:keys [persisted accept! via]}]
  (fn [W {:keys [verb tag body installed]} cx]
    (let [live (fn [sec k]
                 (when-let [r (lookup/rid-of W sec k)]
                   ((publishing/ref-names W sec) (get-in W [sec r]))))
          body (cond->> body (#{:create :update} verb) (publishing/versioned-body live tag))
          cx  (cond-> cx persisted (assoc-in [:sigma :persisted] (constantly persisted)))
          ref #(let [rs (ri/install {:world W :events [] :installed installed} cx verb tag body)]
                 {:world (:world rs) :events (:events rs)})
          sql (fn [W1 k]
                (when (= :transform tag)
                  (when-let [r (sql-gate-refusal (vec (sigma/call cx :sql-gate W W1 k)) nil)]
                    (throw r))))
          staged #(let [st (ri/build cx verb tag body)
                        [sec k] (:path st)]
                    (ri/authorize-step! cx (:op st) sec k)
                    (via W st sql))
          r   (try (if via (staged) (ref))
                   (catch clojure.lang.ExceptionInfo e (throw (worded-refusal W e))))]
      (some-> accept! (apply [(:world r)]))
      r)))

(def module
  {:name     :catalog
   :requires #{:code/introspect-python :code/introspect-clojure :sql/explain}})

(defn- trace-snapshot
  [bindings]
  (update-vals bindings
               #(cond
                  (sequential? %) {:count (count %) :type :collection}
                  (map? %)        {:id (:id %) :type (:object-type-ref %)}
                  :else           %)))

(defn- verb-ctx
  [ctx cx run-state]
  (let [{:keys [action batch phase]} (:kappa cx)]
    (cond-> ctx
      action          (assoc :action action :inside-action? true)
      batch           (assoc :batch batch)
      phase           (assoc :phase phase)
      true            (assoc :ops (:ops run-state))
      (:installed run-state) (assoc :installed (:installed run-state)))))

(defn- verb [ctx op]
  (or (get-in ctx [:verbs op])
      (refusal/refuse! :refusal.catalog/unknown-step-op
                       {:op op :known (vec (sort (keys (:verbs ctx))))})))

(defn edited-arrays
  [world log-type]
  (let [lt (or (lookup/rid-of world :object-types log-type) log-type)]
    (into {}
          (for [lk (keys (:link-types world))
                :let [{:keys [fk-type pk-type fk-attribute]} (rules/fk-link-spec world lk)]
                :when (and fk-attribute
                           (= lt (or (lookup/rid-of world :object-types fk-type) fk-type))
                           (str/starts-with? (name fk-attribute) "edited-objects"))]
            [fk-attribute pk-type]))))

(defn log-stamps
  [world log-type]
  (let [lt (or (lookup/rid-of world :object-types log-type) log-type)]
    (into #{} (filter #{:action-rid :action-type-rid :action-type-version :timestamp})
          (map (comp keyword :attribute-name) (get-in world [:object-types lt :attribute-list])))))

(def ^:private schema-sigma
  {:validate-row         (fn [W T vals complete?] ((cstr/row-validator W T) vals complete?))
   :rid-of               lookup/rid-of
   :object-key           rules/object-key
   :interface?           lookup/iface?
   :implementers         rules/implementers
   :interface-properties (fn [W i] (into #{} (keep :attribute-name) (lookup/attrs W i)))
   :pk-attribute         rules/object-primary-key
   :action               actions/resolve-action
   :param-object-type    (fn [W t] (lookup/rid-of W :object-types t))
   :attr-types           lookup/attr-type-map
   :edited-arrays        edited-arrays
   :log-stamps           log-stamps})

(defn- authority-of
  [ctx]
  (:authority ctx))

(def ^:private run-out
  [:proposals :collected :decisions])

(defn- handed-back
  [run-state r]
  (reduce (fn [run-state k] (cond-> run-state (seq (get r k)) (update k (fnil into []) (get r k))))
          (cond-> run-state (:installed r) (assoc :installed (:installed r)))
          run-out))

(defn- reads
  [ctx io world]
  (merge (when (:read-fn ctx)
           {:query (fn [W os args installed]
                     ((:read-fn ctx) (cond-> (assoc ctx :schema W)
                                       installed (assoc :installed installed))
                                     io os args))})
         (some-> (:sigma-fn ctx) (apply [ctx io world]))))

(defn- sigma
  [ctx io world]
  (sigma/check
   (merge
    (:sigma (install-cx-of (:code ctx)))
    schema-sigma
    (cond-> {:place    (fn [W W1]
                         (if-let [p (force (:project io))]
                           (member/place-new forms/installed W W1 p) W1))
             :user     (fn [] (:user (authority-of ctx)))
             :username (fn [] (:username (authority-of ctx)))
             :foreign (fn [op step run-state cx]
                        (let [r ((verb ctx op)
                                 step (:world run-state) (:bindings run-state)
                                 (verb-ctx ctx cx run-state) io)]
                          (-> run-state
                              (assoc :world (:world r) :bindings (:bindings r))
                              (update :ops into (:ops r))
                              (cond-> (seq (:steps r))
                                (as-> run-state
                                      (rk/ev {:k :seq :body (mapv lower/lower (:steps r))} run-state
                                             cx)))
                              (update :events into (:side-effects r))
                              (handed-back r)
                              (cond-> (:trace-extra r) (assoc ::trace-extra (:trace-extra r))))))
             :expand  (fn [via step run-state cx]
                        (let [r ((verb ctx via)
                                 step (:world run-state) (:bindings run-state)
                                 (verb-ctx ctx cx run-state) io)]
                          {:steps  []
                           :ops    (:ops r)
                           :events (:side-effects r)
                           :out    (select-keys r run-out)}))}
      (:installer io)  (assoc :install (install-via (:installer io)))
      (:sql-gate io)   (assoc :sql-gate (:sql-gate io))
      (:verbs ctx)     (assoc :machine-op? #(contains? (:verbs ctx) %))
      (:authorize io)  (assoc :authorize (fn [op t] ((:authorize io) op t nil)))
      (:checkpoint io) (assoc :checkpoint (fn [run-state] ((:checkpoint io) io run-state))))
    (reads ctx io world)
    {:under (fn [R] (reads (assoc ctx :ops R) io world))})))

(defn- kappa [ctx]
  (cond-> {:mode           (case (:mode ctx)
                             :propose :propose
                             :collect (if (:staged? ctx) :apply :collect)
                             :apply)
           :actor          (:actor ctx)
           :action-type    (:action-type ctx)
           :staged?        (boolean (:staged? ctx))
           :inside-action? (boolean (:inside-action? ctx))}
    (:action-id ctx) (assoc :action-id (str (:action-id ctx)))
    (:action ctx) (assoc :action (:action ctx))
    (:batch ctx)  (assoc :batch (:batch ctx))
    (:phase ctx)  (assoc :phase (:phase ctx))))

(defn- step-extra [{:keys [k over body step]} B run-state']
  (case k
    :for-each (let [items (if (contains? over :binding) (get B (:binding over))
                              (rexpr/eval B (:expr over)))]
                {:collection (or (:binding over) (:expr over))
                 :iterations (count (or items []))
                 :sub-steps  (count (:body body))})
    :query    (let [r (get-in run-state' [:bindings (:as step)])]
                {:object-set (:object-set step)
                 :result-count (count (if (map? r) (:data r) r))})
    nil))

(defn- frame
  [{:keys [render] :as ctx} {:keys [trace]}]
  (fn [{:keys [op i src body]} run-state run _cx]
    (let [B   (:bindings run-state)
          t0 (System/nanoTime)
          ms #(/ (- (System/nanoTime) t0) 1e6)
          run-state' (try (run run-state)
                          (catch InterruptedException e (throw e))
                          (catch Throwable t
                            (let [d (ex-data t)]
                              (when (= :need (:ref/kind d)) (throw t))
                              (when trace
                                (swap! trace conj {:step i :op op :rule src
                                                   :bindings-snapshot (trace-snapshot B)
                                                   :elapsed-ms (ms)
                                                   :error (ex-message t)
                                                   :error-class (.getName (class t))
                                                   :ex-data d}))
                              (when (:user-facing? d) (throw t))
                              (let [msg (or (when (and render (:ref/kind d) (not (:step d)))
                                              (render ctx t))
                                            (ex-message t))]
                                (throw
                                 (ex-info (str "Function step " i " (" op ") failed: " msg)
                                          (merge {:step i :rule src
                                                  :bindings-snapshot (trace-snapshot B)
                                                  :known-bindings (vec (sort (keys B)))
                                                  :cause-class (.getName (class t))
                                                  :cause-ex-data d}
                                                 (cond-> (select-keys d [:ref/kind])
                                                   (anomaly/category d)
                                                   (assoc ::anomaly/category
                                                          (anomaly/category d))))
                                          t))))))]
      (when trace
        (swap! trace conj
               (merge {:step i :op op :bindings-snapshot (trace-snapshot B) :elapsed-ms (ms)}
                      (step-extra body B run-state')
                      (::trace-extra run-state'))))
      (dissoc run-state' ::trace-extra))))

(defn- handle->id [x] (if (rv/handle? x) (:id x) x))

(defn run-ref
  [world bindings ctx io f]
  (let [seed (vec (:ops ctx))
        cx   {:sigma (sigma ctx io world) :kappa (kappa ctx) :frame (frame ctx io)}
        run-state (try (f (cond-> {:world world :bindings bindings :events [] :ops seed}
                            (:installed ctx) (assoc :installed (:installed ctx)))
                          cx)
                       (catch clojure.lang.ExceptionInfo e
                         (let [d (ex-data e)
                               r (:render ctx)]
                           (throw (if-let [msg (and r (:ref/kind d) (not= :need (:ref/kind d))
                                                    (not (:step d))
                                                    (not (::rendered d)) (r ctx e))]
                                    (ex-info msg (assoc d ::rendered true) e)
                                    e)))))]
    (merge {:world (:world run-state) :bindings (:bindings run-state)
            :side-effects (:events run-state)
            :ops (subvec (:ops run-state) (count seed))}
           (select-keys run-state [:installed])
           (cond-> (select-keys run-state run-out)
             (:proposals run-state)
             (update :proposals #(mapv (fn [p] (update p :args update-vals handle->id)) %))
             (:collected run-state)
             (update :collected #(mapv (fn [c] (update c :args update-vals handle->id)) %))))))

(defn eval-step
  ([step world]              (eval-step step world {} {} nil))
  ([step world bindings]     (eval-step step world bindings {} nil))
  ([step world bindings ctx] (eval-step step world bindings ctx nil))
  ([step world bindings ctx io]
   (run-ref world bindings ctx io #(rk/ev (lower/lower step) %1 %2))))

(defn eval-steps
  ([steps world]              (eval-steps steps world {} {} nil))
  ([steps world bindings]     (eval-steps steps world bindings {} nil))
  ([steps world bindings ctx] (eval-steps steps world bindings ctx nil))
  ([steps world bindings ctx io]
   (run-ref world bindings ctx io #(rk/ev (lower/lower-seq steps) %1 %2))))

(defn event
  ([op] (event op {}))
  ([op fields]
   (merge {:op op :target nil :before nil :after nil :actor nil :provenance nil}
          fields)))
