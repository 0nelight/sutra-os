(ns sutra.ref.action
  (:require [sutra.ref.err :as err :refer [fail!]]
            [sutra.ref.expr :as expr]
            [sutra.ref.kernel :as k]
            [sutra.ref.lower :as lower]
            [sutra.ref.objects :as obj]
            [sutra.ref.params :as p]
            [sutra.ref.sigma :as s]
            [sutra.ref.term :as term]
            [sutra.ref.value :as v]
            [sutra.types.refs :as refs]))

(set! *warn-on-reflection* true)

(def sys-actions
  {:sys/create-object {:rules [{:op :create :id [:get :arg0 :id]
                                :object-type [:get :arg0 :object-type]
                                :values [:get :arg0 :values]}]}
   :sys/update-object {:rules [{:op :update :target [:get :arg0 :id] :values [:get :arg0 :values]}]}
   :sys/delete-object {:rules [{:op :delete-object :target [:get :arg0 :id]}]}
   :sys/create-link   {:rules [{:op :create-link :id [:get :arg0 :id]
                                :link-type [:get :arg0 :link-type]
                                :from-id [:get :arg0 :from-id] :to-id [:get :arg0 :to-id]}]}
   :sys/delete-link   {:rules [{:op :delete-link :target [:get :arg0 :id]}]}
   :sys/compute-on    {:rules [{:op :compute-on :target [:get :arg0 :id] :expr [:get :arg0 :expr]
                                :result-attr [:get :arg0 :result-attr]}]}
   :sys/notify        {:rules [{:op :notify :channel [:get :arg0 :channel]
                                :payload [:get :arg0 :payload]}]}})

(defn- resolve-action! [W cx a]
  (let [sys? (and (keyword? a) (= "sys" (namespace a)))]
    (when (and sys? (not (get-in cx [:kappa :inside-action?])))
      (fail! :sys-denied (str "Direct sys/* access denied: " a)))
    (or (when sys? (sys-actions a))
        (s/call cx :action W a)
        (fail! :unknown-action (str "Unknown action: " a) {:action a}))))

(defn function-rule [action]
  (first (filter #(= :apply-edit-function (:op %)) (refs/steps (:rules action)))))

(defn- multiple-params [action]
  (into #{} (keep (fn [[q spec]] (when (= :list (first (term/of spec))) q))) (:params action)))

(defn- log-rule? [r] (= [:get :log-id] (get-in r [:values :log-id])))
(defn- side-effect-rule? [r] (= :side-effect (:phase r)))
(defn- writeback-rule? [r] (= :writeback (:phase r)))

(defn- writeback-inputs
  [action]
  (let [rules (vec (:rules action))
        i     (first (keep-indexed (fn [i r] (when (writeback-rule? r) i)) rules))]
    (if i (filterv #(= :invoke-function (:op %)) (subvec rules 0 i)) [])))

(defn- after-writeback
  [action]
  (let [early (set (writeback-inputs action))]
    (vec (remove #(or (side-effect-rule? %) (writeback-rule? %) (early %)) (:rules action)))))

(defn- object-types-of
  [W cx t]
  (or (some-> (s/call cx :param-object-type W t) vector)
      (when (keyword? t)
        (some->> (s/call cx :rid-of W :interfaces t) (s/call cx :implementers W) seq vec))))

(defn param-terms
  [W cx specs]
  (let [ref-types (when (s/can-read? cx) #(object-types-of W cx %))]
    (update-vals specs #(term/of % ref-types))))

(defn- hydrate [W cx action an B]
  (if-not (s/can-read? cx)
    B
    (reduce-kv
     (fn [b q spec]
       (if-let [ts (object-types-of W cx (:type spec))]
         (let [one (fn [x]
                     (when (some? x)
                       (if (v/handle? x)
                         x
                         (let [raw  (str (if (and (map? x) (contains? x :id)) (:id x) x))
                               hits (for [ot ts
                                          :let
                                          [{:keys [object
                                                   problem]} (obj/resolve-ref cx W ot raw)]]
                                      {:ot ot :object object :problem problem})
                               found (filter :object hits)]
                           (case (count found)
                             1 (let [{:keys [ot object]} (first found)]
                                 {:id (str (:id object)) :object-type-ref ot
                                  :values (:values object)})
                             0 (fail! :unresolved-param (str "parameter " q " does not resolve")
                                      (merge (:problem (first hits))
                                             {:action an :param q :unresolved raw}))
                             (fail! :ambiguous
                                    (str "parameter " q " names " (count found)
                                         " objects")
                                    {:action an :param q :unresolved raw}))))))
               x (get b q)]
           (assoc b q (if (sequential? x) (mapv one x) (one x))))
         b))
     B (:params action))))

(defn- override-holds? [B when-expr]
  (try (expr/pred B (v/resolve-deep B when-expr) nil)
       (catch Throwable e
         (if (= :unknown-field (err/kind e)) false (throw e)))))

(defn- hidden? [spec]
  (= :hidden (some-> (:exposure spec) name keyword)))

(defn- effective [B spec]
  (if-let [hit (first (filter #(override-holds? B (:when %)) (:overrides spec)))]
    (merge spec (dissoc hit :when))
    spec))

(defn- prefill [cx B specs]
  (reduce-kv
   (fn [b q e]
     (case (:prefill e)
       :current-user (assoc b q (some-> (if (= :user (:type e))
                                          (s/call cx :user)
                                          (or (s/call cx :username) (s/call cx :user)))
                                        str))
       :uuid (cond-> b (nil? (get b q)) (assoc q (str (s/call cx :fresh-uuid))))
       :now  (cond-> b
               (or (nil? (get b q)) (p/implicit-context-param? q))
               (assoc q
                      (let [i (s/call cx :now)]
                        (if (= :date (:type e))
                          (str
                           (.toLocalDate (.atZone ^java.time.Instant i
                                                  ^java.time.ZoneId (s/call cx :zone))))
                          (str i)))))
       b))
   B specs))

(defn- param-write-types
  [W cx action]
  (let [p-of  (fn [x]
                (cond
                  (keyword? x) x
                  (and (vector? x) (= :get (some-> (first x) name keyword)))
                  (let [[a b & more] (rest x)]
                    (cond (and (= :arg0 (some-> a name keyword)) b (empty? more)) (keyword (name b))
                          (and a (nil? b)) (keyword (name a))))))
        ot-of (fn [r]
                (let [t (or (:object-type r)
                            (some->> (p-of (:target r)) (get (:params action)) :type))]
                  (cond (nil? t) nil
                        (get-in W [:object-types t]) t
                        :else (s/call cx :rid-of W :object-types (keyword (name t))))))]
    (reduce (fn [m r]
              (let [types (some->> (ot-of r) (s/call cx :attr-types W))]
                (reduce-kv (fn [m prop x]
                             (let [q (p-of x)
                                   t (when q (get types (keyword (name prop))))]
                               (if (and t (contains? (:params action) q))
                                 (update m q (fnil conj #{}) t) m)))
                           m (:values r))))
            {}
            (filter (comp map? :values) (refs/steps (:rules action))))))

(defn- coerce-numbers [W cx action specs B]
  (let [wt (delay (param-write-types W cx action))]
    (reduce-kv
     (fn [[b probs] q spec]
       (let [x    (get b q)
             kind (when (or (number? x) (and (sequential? x) (some number? x)))
                    (p/number-kind spec (get @wt q)))]
         (if-not kind
           [b probs]
           (let [c  (if (sequential? x) (mapv #(p/coerce-number kind %) x) (p/coerce-number kind x))
                 ok (not (if (sequential? c) (some #{::p/unreadable} c) (= ::p/unreadable c)))]
             (if ok
               [(cond-> (assoc b q c) (contains? (:arg0 b) q) (assoc-in [:arg0 q] c)) probs]
               [b
                (conj probs
                      {:parameter q :expected (if (= :integral kind) :integer kind) :value x})])))))
     [B []] specs)))

(defn- fill
  [B specs f]
  (reduce-kv (fn [b q e] (if (nil? (get b q)) (if-some [x (f b e)] (assoc b q x) b) b)) B specs))

(defn- fill-computed
  [B specs]
  (let [f #(when-let [c (:compute %2)] (try (expr/eval %1 c) (catch RuntimeException _ nil)))]
    (loop [b B n (count specs)]
      (let [b' (fill b specs f)]
        (if (or (= b' b) (zero? n)) b' (recur b' (dec n)))))))

(defn- property-default
  [current b {d :default}]
  (when-let [from (and (map? d) (:property d)
                       (or (some-> (:from d) name keyword) current))]
    (let [vals (:values (get b from))
          kk   (keyword (name (:property d)))]
      (when (map? vals)
        (if (contains? vals kk) (get vals kk) (get vals (name kk)))))))

(defn- settle
  [W cx action an params B env]
  (let [
        specs  (if params (update-vals params #(effective env %)) {})
        B      (reduce-kv (fn [b q e]
                            (cond-> b (and (hidden? e) (not (hidden? (get params q))))
                                    (assoc q nil)))
                          B specs)
        B      (reduce-kv (fn [b q e]
                            (cond-> b (and (= :string (:type e)) (uuid? (get b q))) (update q str)))
                          B specs)
        B      (prefill cx B specs)
        B      (fill B specs #(let [d (:default %2)] (when-not (map? d) d)))
        B      (hydrate W cx action an B)
        current (some (fn [[q e]] (when (= :current-object (:prefill e)) q)) specs)
        B0     B
        B      (fill B specs (partial property-default current))
        B      (reduce (fn [b q]
                         (try (hydrate W cx {:params {q (get params q)}} an b)
                              (catch clojure.lang.ExceptionInfo e
                                (if (#{:unresolved-param :ambiguous} (err/kind e))
                                  (assoc b q nil)
                                  (throw e)))))
                       B (filter #(and (nil? (get B0 %)) (some? (get B %))) (keys specs)))
        B      (fill-computed B specs)]
    [B specs]))

(defn param-evaluation
  [W cx action an B args]
  (let [action (p/with-section-overrides action)
        params (:params action)
        args   (cond-> args (map? (:arg0 args)) (update :arg0 #(p/absent-blanks params %)))
        B      (merge B args
                      (when params (into {} (for [[q _] params] [q (get-in args [:arg0 q])]))))
        B      (hydrate W cx action an B)
        [B specs] (loop [[b sp] (settle W cx action an params B B) n 2]
                    (let [[_ sp' :as nxt] (settle W cx action an params B b)]
                      (if (or (= sp' sp) (zero? n)) nxt (recur nxt (dec n)))))
        B      (p/absent-blanks specs B)
        zone   (s/call cx :zone)
        missing (seq (p/missing-required specs B))
        bad     (when-not missing (seq (p/type-problems zone specs B)))
        [B num-bad] (if (or missing bad) [B nil] (coerce-numbers W cx action specs B))
        bad     (or bad (seq num-bad))
        B       (p/normalise-dates zone specs B)
        B       (reduce (fn [b q]
                          (update b q #(cond (nil? %) [] (sequential? %) (vec %) :else [%])))
                        B (multiple-params action))
        broken  (when-not (or missing bad) (seq (p/param-constraint-problems specs B)))
        failed  (when-not (or missing bad broken)
                  (first (remove #(expr/pred (v/resolve-bindings B (:test %)) (:pred %) nil)
                                 (:submission-criteria action))))]
    {:bindings B :params specs
     :missing (vec missing) :invalid (vec bad) :violated (vec broken) :criterion failed}))

(defn call-bindings [W cx action an B args]
  (let [{:keys [bindings params missing invalid violated
                criterion]} (param-evaluation W cx action an B args)
        refusal {:action an :params params}]
    (cond
      (seq missing)  (fail! :missing-required (str "Aktion " (name an) " abgewiesen")
                            (assoc refusal :missing missing))
      (seq invalid)  (fail! :invalid-type "type refusal"
                            (assoc refusal :invalid invalid))
      (seq violated) (fail! :constraint-refusal "constraint refusal"
                            (assoc refusal :violated violated))
      criterion      (fail! :criterion-failed (or (:message criterion) "criterion failed")
                            (assoc refusal :failed-criterion criterion))
      :else bindings)))

(defn evaluate-params
  [W cx an args]
  (let [cx     (assoc-in cx [:kappa :inside-action?] true)
        action (resolve-action! W cx an)
        {:keys [bindings params missing invalid violated
                criterion]} (param-evaluation W cx action an {} {:arg0 args})]
    {:action an :params params :criterion criterion :bindings bindings
     :terms (param-terms W cx params)
     :missing missing :invalid invalid :violated violated
     :blocked? (boolean (or (seq missing) (seq invalid) (seq violated) criterion))}))

(defn check-submission
  [W cx an args]
  (let [cx     (assoc-in cx [:kappa :inside-action?] true)
        action (resolve-action! W cx an)]
    (call-bindings W cx action an {} args)
    nil))

(defn- action-rules
  [action]
  (let [rules (after-writeback action)
        qs    (multiple-params action)]
    (cond->> rules
      (seq qs) (mapv (fn [r] (let [e (get-in r [:values :edited])]
                               (cond-> r
                                 (and (log-rule? r) (vector? e) (= :get (first e))
                                      (contains? qs (second e)))
                                 (update :values dissoc :edited))))))))

(defn- edited-keys [W cx events pk-type]
  (let [rid     #(or (s/call cx :rid-of W :object-types %) %)
        pk-type (rid pk-type)
        pk      (first (:properties (s/call cx :object-key W pk-type)))
        writes  (filter #(and (#{:create :update :delete-object} (:op %))
                              (= pk-type (rid (get-in % [:target :type]))))
                        events)
        created (into {} (keep #(when (= :create (:op %)) [(get-in % [:target :id]) (:after %)]))
                      writes)
        key-of  (fn [id] (cond (nil? pk) (str id)
                               (contains? created id) (get-in created [id pk])
                               (s/can-read? cx)
                               (get-in (s/call cx :object pk-type (str id)) [:values pk])))]
    (into [] (comp (map #(get-in % [:target :id])) (distinct) (keep key-of) (distinct)) writes)))

(defn- run-rules [rules run-state cx]
  (k/ev (lower/lower-seq rules) run-state cx))

(defn- writeback
  [action run-state cx]
  (if-let [r (first (filter writeback-rule? (:rules action)))]
    (try (run-rules [r] (run-rules (writeback-inputs action) run-state cx)
                    (assoc-in cx [:kappa :phase] :writeback))
         (catch InterruptedException e (throw e))
         (catch Exception e
           (when (= :need (err/kind e)) (throw e))
           (fail! :writeback-failed
                  (str "the writeback failed, nothing was changed: " (ex-message e))
                  {:cause (ex-data e)})))
    run-state))

(defn- log-stamp-values
  [W action rule cx]
  (select-keys (cond-> {:action-type-rid     (:id action)
                        :action-type-version (or (:revision action) 1)
                        :timestamp           (str (s/call cx :now))}
                 (get-in cx [:kappa :action-id])
                 (assoc :action-rid (get-in cx [:kappa :action-id])))
               (s/call cx :log-stamps W (:object-type rule))))

(defn- eval-action-rules [action run-state cx]
  (let [{logs true edits false} (group-by (comp boolean log-rule?) (action-rules action))
        run-state (writeback action run-state cx)]
    (reduce (fn [run-state rule]
              (let [W    (:world run-state)
                    vals (into (log-stamp-values W action rule cx)
                               (for [[attr
                                      pk-type] (s/call cx :edited-arrays W (:object-type rule))]
                                 [attr
                                  (edited-keys W (s/under cx run-state) (:events run-state)
                                               pk-type)]))]
                (run-rules [(update rule :values merge vals)] run-state cx)))
            (run-rules (vec edits) run-state cx)
            logs)))

(defn provenance-violations [W cx action events]
  (if-not (function-rule action)
    []
    (let [rid      #(or (s/call cx :rid-of W %1 %2) %2)
          canon    #(cond-> %
                      (:object-type %) (update :object-type (partial rid :object-types))
                      (:link-type %)   (update :link-type (partial rid :link-types)))
          declared (into #{} (map canon)
                         (concat (:edits action)
                                 (map #(select-keys % [:object-type])
                                      (filter log-rule? (:rules action)))))
          ifaces   (into #{} (comp (keep :interface)
                                   (mapcat #(s/call cx :implementers W %))
                                   (map (partial rid :object-types)))
                         (:edits action))
          edit-of  (fn [{:keys [op target]}]
                     (case op
                       (:create :update
                                :delete-object) (when (:type target) {:object-type (:type target)})
                       (:create-link :delete-link) (when (:type target) {:link-type (:type target)})
                       nil))]
      (into [] (comp (keep edit-of)
                     (remove #(let [c (canon %)]
                                (or (contains? declared c) (contains? ifaces (:object-type c)))))
                     (distinct))
            events))))

(defn- enforce-provenance! [W cx an action events]
  (when-let [vs (seq (provenance-violations W cx action events))]
    (fail! :provenance (str "Action " an " edited outside its declared :edits")
           {:action an :violations (vec vs) :declared (vec (:edits action))})))

(defn- side-effects [action run-state cx]
  (reduce (fn [run-state rule]
            (try (run-rules [rule] run-state (assoc-in cx [:kappa :phase] :side-effect))
                 (catch InterruptedException e (throw e))
                 (catch Exception t
                   (when (#{:side-effect-edit :need} (err/kind t)) (throw t))
                   (v/emit run-state {:op :side-effect-failed :rule-op (:op rule)
                                      :error (ex-message t) :ex-data (ex-data t)}))))
          run-state
          (filter side-effect-rule? (:rules action))))

(defn- enter [run-state cx an]
  (let [cx     (assoc-in cx [:kappa :inside-action?] true)
        action (resolve-action! (:world run-state) cx an)]
    [(:world run-state) (assoc-in cx [:kappa :action] action) action]))

(defn- leave
  [run-state W cx an action r]
  (enforce-provenance! W cx an action (:events r))
  (update (side-effects action r cx) :events #(into (:events run-state) %)))

(defn invoke
  [run-state cx an args]
  (let [[W cx action] (enter run-state cx an)
        call-B (call-bindings W (s/under cx run-state) action an (:bindings run-state) args)
        r      (eval-action-rules action (assoc run-state :bindings call-B :events []) cx)]
    (leave run-state W cx an action
           (update r :bindings merge (select-keys call-B (keys (:params action)))))))

(defn invoke-batch
  [run-state cx an arg-maps]
  (let [[W cx action] (enter run-state cx an)
        entries (mapv #(call-bindings W (s/under cx run-state) action an (:bindings run-state)
                                      {:arg0 %})
                      arg-maps)
        r       (if (:batched (function-rule action))
                  (let [run-state (writeback action
                                             (assoc run-state
                                                    :bindings (or (first entries)
                                                                  (:bindings run-state))
                                                    :events []) cx)]
                    (run-rules (after-writeback action)
                               run-state
                               (assoc-in cx [:kappa :batch] entries)))
                  (reduce (fn [r B] (eval-action-rules action (assoc r :bindings B) cx))
                          (assoc run-state :events [])
                          entries))]
    (leave run-state W cx an action r)))

(defmethod k/action-step :evaluate-params [_ world cx a args] (evaluate-params world cx a args))

(defmethod k/action-step :invoke [_ run-state cx a args] (invoke run-state cx a args))
