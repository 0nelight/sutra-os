(ns sutra.ref.kernel
  (:require [sutra.ref.err :refer [fail!] :as err]
            [sutra.ref.expr :as expr]
            [sutra.ref.install :as install]
            [sutra.ref.lower :as lower]
            [sutra.ref.objects :as obj]
            [sutra.ref.objectset :as os]
            [sutra.ref.sigma :as s]
            [sutra.ref.value :as v]))

(set! *warn-on-reflection* true)

(def ^:private entity-classes (set (map :section (vals install/class-by-tag))))

(defn- class-of! [op path]
  (or (entity-classes (first path))
      (fail! :bad-path (str op " path must start with an entity class") {:path path})))

(defn- term [B [kind & args]]
  (case kind
    :expr      (expr/eval B (first args))
    :target-id (let [t (obj/target-id B (first args))]
                 (when-not (and (sequential? t) (empty? t)) t))
    :coll      (let [[from [cop & cargs] default] args
                     raw (v/coll-op cop (get B from) (first cargs))]
                 (if (and (nil? raw) default)
                   {:id :_default :object-type-ref :_default :values default}
                   raw))))

(defn- env [B e]
  (cond
    (= :bindings e)        B
    (:resolve-bindings e)  (v/resolve-bindings B (:resolve-bindings e))
    (contains? e :resolve-bindings) {}
    (:terms e)             (update-vals (:terms e) #(term B %))))

(declare ev)

(defmulti action-step
  (fn [op & _] op))

(defn- invoke [{a :fn :keys [args as effect?]} run-state cx]
  (let [B     (:bindings run-state)
        args* (v/resolve-deep B args)
        mode  (get-in cx [:kappa :mode])
        arg0  (fn [] (let [a (or (:arg0 args*) args*)] (if (map? a) a {})))]
    (cond
      (and effect? (= mode :propose))
      (update run-state :proposals (fnil conj []) {:action a :args (arg0)})

      (and (not effect?) (= mode :propose))
      (fail! :propose-refused "propose mode: :call is refused" {:op :call})

      (and effect? (= mode :collect) (not (get-in cx [:kappa :staged?])))
      (let [check
              (action-step :evaluate-params (:world run-state) cx a (arg0))]
        (when (:blocked? check)
          (fail! :collect-blocked "collected action is blocked" check))
        (update run-state :collected (fnil conj []) {:action a :args (arg0)}))

      :else
      (let [r (action-step :invoke run-state cx a args*)]
        (assoc r :bindings (if as (assoc B as (:bindings r)) B))))))

(defn- compute-on [{:keys [target expr result-attr as]} run-state cx]
  (let [B   (:bindings run-state)
        o   (v/resolve-binding B target)
        e   (v/resolve-deep B expr)
        new (expr/eval (:values o) e
                       {:self o :traverse (get-in (s/under cx run-state) [:sigma :traverse])})
        o'  (assoc-in o [:values result-attr] new)]
    (-> run-state
        (update :bindings #(cond-> %
                             as (assoc as o')
                             (and (keyword? target) (contains? % target)) (assoc target o')))
        (v/emit {:op :compute-on
                 :target (when (or (:object-type-ref o) (:id o))
                           {:type (:object-type-ref o) :id (:id o)})
                 :result-attr result-attr :after new}))))

(defn- refuse-in-propose! [cx op]
  (when (= :propose (get-in cx [:kappa :mode]))
    (fail! :propose-refused
           (str "propose mode: a " (name op) " step edits the ontology directly. "
                "Only Actions can be proposed for review — wrap the edit in an Action "
                "and apply it with :apply-action (or an :apply-action tool).")
           {:op op})))

(defn- refuse-in-side-effect! [cx op]
  (when (= :side-effect (get-in cx [:kappa :phase]))
    (fail! :side-effect-edit
           (str "side-effect phase: a " (name op) " step edits the ontology. "
                "A side effect only sends (:notify, :invoke-webhook, a writeback push); "
                "move the edit to the action's rules.")
           {:op op})))

(defn- loaded
  [run-state n cx]
  (let [total (+ (:loaded run-state 0) n)
        limit (get-in cx [:kappa :objects-loaded-max])]
    (when (and limit (> total limit))
      (fail! :limit (str "the run loaded " total " objects, more than " limit)
             {:loaded total :limit limit}))
    (assoc run-state :loaded total)))

(def ^:private list-slots
  {:update [:target] :delete-object [:target]
   :update-of-interface [:target] :delete-of-interface [:target]
   :apply-marking [:target] :remove-marking [:target]
   :create-link [:from-id :to-id] :delete-link [:target :from-id :to-id]
   :create-link-of-interface [:from-id :to-id]
   :delete-link-of-interface [:target :from-id :to-id]})

(defn- list-axes
  [B op step]
  (reduce (fn [[step axes] slot]
            (let [x (get step slot)
                  k (cond (and (keyword? x) (contains? B x)) x
                          (and (vector? x) (= :get (first x)) (keyword? (second x))) (second x))]
              (cond
                (nil? x) [step axes]
                (and k (sequential? (get B k)))
                [step (cond-> axes (not-any? #(= k (first %)) axes) (conj [k (vec (get B k))]))]
                :else
                (let [r (v/resolve-arg B x)]
                  (if (sequential? r)
                    (let [k' (keyword (str "_list-" (name slot)))]
                      [(assoc step slot [:get k']) (conj axes [k' (vec r)])])
                    [step axes])))))
          [step []]
          (list-slots op)))

(defn- write-each
  [f op step run-state cx]
  (let [B (:bindings run-state)
        [step axes] (list-axes B op step)]
    (if (empty? axes)
      (f step run-state cx)
      (let [combos (reduce (fn [cs [k items]] (for [c cs i items] (assoc c k i))) [{}] axes)
            as     (:as step)
            [run-state outs] (reduce (fn [[run-state outs] c]
                                (let [run-state' (f step (update run-state :bindings merge c) cx)]
                                  [(assoc run-state' :bindings B)
                                   (cond-> outs as (conj (get-in run-state' [:bindings as])))]))
                              [run-state []]
                              combos)]
        (cond-> run-state as (assoc-in [:bindings as] outs))))))

(defn- write [{:keys [op step]} run-state cx]
  (refuse-in-propose! cx op)
  (refuse-in-side-effect! cx op)
  (let [f (case op
            :create obj/create-object  :update obj/update-object  :delete-object obj/delete-object
            :create-link obj/create-link  :delete-link obj/delete-link
            :apply-marking (partial obj/reclassify :add)
            :remove-marking (partial obj/reclassify :remove)
            :create-of-interface (partial obj/of-interface :create)
            :update-of-interface (partial obj/of-interface :update)
            :delete-of-interface (partial obj/of-interface :delete)
            :create-link-of-interface (partial obj/link-of-interface :create)
            :delete-link-of-interface (partial obj/link-of-interface :delete))]
    (write-each f op step run-state cx)))

(defn ev
  [n run-state cx]
  (let [{W :world B :bindings} run-state]
    (case (:k n)
      :at (do (s/call cx :checkpoint run-state)
              (cond
                (not (:frame? n)) (ev (:body n) run-state cx)
                (:frame cx)       ((:frame cx) n run-state #(ev (:body n) % cx) cx)
                :else
                (try (ev (:body n) run-state cx)
                     (catch clojure.lang.ExceptionInfo e
                       (when (or (:user-facing? (ex-data e)) (= :need (err/kind e))) (throw e))
                       (throw (ex-info (ex-message e)
                                       (update (ex-data e) :ref/ops (fnil conj ()) (:op n))
                                       e)))
                     (catch InterruptedException e (throw e))
                     (catch Exception e
                       (throw (ex-info (str "step " (:op n) " failed: " (ex-message e))
                                       {:ref/kind :host-error :ref/ops (list (:op n))
                                        :cause-class (.getName (class e))}
                                       e))))))

      :seq (reduce (fn [run-state m] (ev m run-state cx)) run-state (:body n))

      (:set :dissoc)
      (let [{op :k :keys [path value]} n
            c (class-of! op path)]
        (s/call cx :authorize op {:type c :id nil})
        (-> run-state
            (assoc :world (if (= op :set) (assoc-in W path value) (v/dissoc-in W path)))
            (v/emit (cond-> {:op op :target {:type c :id nil} :path path :before (get-in W path)}
                      (= op :set) (assoc :after value)))))

      :if (let [branch (if (expr/pred (env B (:env n)) (:pred n)) (:then n) (:else n))]
            (if branch (ev branch run-state cx) run-state))

      :for-each
      (let [{:keys [over as body collect-ids]} n
            items (if (contains? over :binding) (get B (:binding over)) (expr/eval B (:expr over)))
            items (or items [])
            run-state'   (reduce (fn [run-state item]
                                   (ev body (assoc-in run-state [:bindings as] item) cx)) run-state
                                 items)]
        (cond-> run-state'
          collect-ids (assoc-in [:bindings collect-ids] (vec (keep :id items)))))

      :bind (cond-> (assoc-in run-state [:bindings (:as n)] (term B (:term n)))
              (:event n) (v/emit {:op :compute :compute-op (first (nth (:term n) 2))}))

      :compute-on (compute-on (:step n) run-state cx)

      :scope (assoc (ev (:body n) run-state cx) :bindings B)

      :subst (ev (lower/lower-seq (mapv #(v/resolve-deep B %) (:steps n))) run-state cx)

      :write (write n run-state cx)

      :install (do (refuse-in-propose! cx (keyword (str (name (:verb n)) "-" (name (:tag n)))))
                   (let [body (into {} (map (fn [[k x]] [k (v/resolve-arg B x)]))
                                    (dissoc (:step n) :op))
                         r    (when (s/provides? cx :install)
                                (s/call cx :install W {:verb (:verb n) :tag (:tag n) :body body
                                                       :installed (:installed run-state)} cx))]
                     (if r
                       (-> run-state (assoc :world (:world r) :installed (:world r))
                           (update :events into (:events r)))
                       (install/install run-state cx (:verb n) (:tag n) body))))

      :invoke (invoke n run-state cx)

      :query (let [{:keys [object-set arguments as]} (:step n)
                   args (v/resolve-bindings B arguments)
                   own? (not (s/provides? cx :query))
                   r    (if own?
                          (os/query (s/under cx run-state) W object-set args)
                          (s/call (s/under cx run-state) :query W object-set args
                                  (:installed run-state)))
                   rows (if (map? r) (:data r) r)]
               (-> run-state
                   (cond-> own? (loaded (count rows) cx))
                   (assoc-in [:bindings as] r)
                   (v/emit {:op :query :object-set object-set :result-count (count rows)})))

      :notify (v/emit run-state {:op :notify
                          :channel (v/resolve-arg B (:channel n))
                          :payload (v/resolve-arg B (:payload n))
                          :actor (get-in cx [:kappa :actor])})

      :foreign (s/call cx :foreign (:op n) (:step n) run-state cx)

      :expand (let [{:keys [steps bindings world events out
                            ops]} (s/call cx :expand (:via n) (:step n) run-state cx)
                    inner (ev (lower/lower-seq steps)
                              (-> run-state
                                  (assoc :world (or world W) :bindings (or bindings {}))
                                  (update :ops into ops))
                              cx)]
                (reduce-kv (fn [run-state k xs]
                             (cond-> run-state (seq xs) (update k (fnil into []) xs)))
                           (-> inner
                               (assoc :world W :bindings B)
                               (update :events into events))
                           out))

      :fail (if (and (= :unknown-step-op (:kind n)) (:op n)
                     (s/provides? cx :machine-op?) (s/call cx :machine-op? (:op n)))
              (s/call cx :foreign (:op n) (:step n) run-state cx)
              (fail! (:kind n) (or (:message n) (str "Unknown step op: " (:op n)))
                     {:op (:op n)})))))
