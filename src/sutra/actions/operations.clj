(ns sutra.actions.operations
  (:require [sutra.catalog.actions :as actions]
            [sutra.store.access.scope :as scope]
            [sutra.store.served :as served]
            [sutra.store.read-only :as read-only]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.lookup :as lookup]
            [sutra.catalog.constraints :as cstr]
            [sutra.catalog.kernel :as kernel]
            [sutra.objects.api :as objects]
            [sutra.store.access.substrate :as sub]
            [sutra.types.submission :as csub]
            [sutra.actions.execution :as x]
            [sutra.ref.action :as ra]
            [sutra.ref.term :as term]))

(set! *warn-on-reflection* true)

(defn derived-reader
  [store]
  (let [store (read-only/of store)]
    (fn [answer]
      (sub/reader (update store :db served/reading (:branch store) answer)))))

(defn sigma
  [ctx io world]
  (let [read (x/reader ctx io)]
    (cond-> {}
      (:pass io)
      (assoc :fresh-uuid #(x/fresh-uuid io)
             :now        #(x/now io))
      (x/can-read? ctx)
      (assoc :object
             (fn [T id] (x/object read grants/system-authority T id))
             :lookup
             (fn [W T q]
               (objects/attach-derived ((:derived-reader io) read) W T (x/lookup read T q)))
             :link-ends
             (fn [_W L id]
               (when-let [l (first (read grants/system-authority [:links L {:ids [(str id)]}]))]
                 {:from-id (:from-object-id l) :to-id (:to-object-id l)})))
      (x/traverse-fn ctx io world)
      (assoc :traverse (let [tf (x/traverse-fn ctx io world)]
                         (fn [self over] (tf nil (:id self) over (:object-type-ref self))))))))

(defn- shown
  [W an]
  (or (when (and W an)
        (not-empty (str (:display-name (actions/resolve-action W (keyword (name an)))))))
      an))

(defn- refused-param
  [W an {:keys [param unresolved] :as problem}]
  (csub/unresolved-param-refusal (shown W an) param
                                 (get-in (actions/resolve-action W an) [:params param])
                                 unresolved problem))

(defn- violations-worded [params violated]
  (mapv #(cstr/with-message (get-in params [(:parameter %) :constraints]) % (:value %)) violated))

(defn- blocked-reason [W {:keys [params missing invalid violated criterion] :as d}]
  (let [action (shown W (:action d))]
    (cond
      (seq missing)  (csub/required-refusal action params missing)
      (seq invalid)  (csub/type-refusal action params invalid)
      (seq violated) (csub/constraint-refusal action params (violations-worded params violated))
      criterion      (csub/criterion-refusal action criterion))))

(defn- blocked-code
  [{:keys [missing invalid violated criterion]}]
  (cond
    (seq missing)  :refusal.actions/params-missing
    (seq invalid)  :refusal.actions/params-invalid
    (seq violated) :refusal.actions/params-violate-constraints
    criterion      :refusal.actions/criterion-not-met
    :else          :refusal.actions/submission-blocked))

(defn render
  [ctx e]
  (let [W (:schema ctx)
        {k :ref/kind :keys [params missing invalid violated failed-criterion]
         :as d} (ex-data e)
        action (shown W (:action d))
        tl #(or (lookup/name-of W :object-types %) %)]
    (case k
      :missing-required   (csub/required-refusal action params missing)
      :invalid-type       (csub/type-refusal action params invalid)
      :constraint-refusal
      (csub/constraint-refusal action params (violations-worded params violated))
      :criterion-failed   (csub/criterion-refusal action failed-criterion)
      :collect-blocked    (blocked-reason W d)
      :unresolved-param   (refused-param W (:action d) d)
      :rule-target-unresolved
      (let [act (:action-type ctx) tname (tl (:type d))]
        (str "„" (if act (name (shown W act)) "(ohne Namen)") "“ nicht ausgeführt — die Regel "
             (pr-str (:op d)) " auf " (name tname) " zielt mit " (pr-str (:target d))
             " auf " (pr-str (:unresolved d)) ", und das ist kein bestehendes " (name tname)
             ". Nichts wurde geschrieben. Eine Regel ändert oder löscht nur"
             " ein Objekt, das als Objektparameter übergeben wird."))
      :provenance
      (let [nm (fn [e] (or (some->> (:object-type e) (lookup/name-of W :object-types))
                           (some->> (:link-type e) (lookup/name-of W :link-types))
                           (some->> (:interface e) (lookup/name-of W :interfaces))
                           (first (vals e))))]
        (str "Action " action " edited outside its declared :edits provenance: "
             (pr-str (mapv nm (:violations d))) " (declared: " (pr-str (mapv nm (:declared d)))
             "). The backing function must either stay within the declaration or the action"
             " must widen it."))
      nil)))

(def machine
  {:sigma-fn          sigma
   :render            render})

(defn- run [world bindings ctx io f]
  (kernel/run-ref world bindings (merge machine {:code (scope/code)} ctx) io f))

(defn execute-action
  [world bindings ctx io action-name args]
  (run world bindings ctx io #(ra/invoke %1 %2 action-name args)))

(defn execute-action-batch
  [world bindings ctx io action-name arg-maps]
  (run world bindings ctx io #(ra/invoke-batch %1 %2 action-name arg-maps)))

(defn check-submission!
  [world ctx io action-name args]
  (run world {} ctx io
       (fn [run-state cx] (ra/check-submission (:world run-state) cx action-name args) run-state))
  nil)

(defn form-values
  [terms bindings]
  (reduce-kv
   (fn [acc p t]
     (let [x    (get bindings p)
           {w :value problems :problems} (term/render t x)
           objs (term/references t x)]
       (cond-> acc
         (seq problems)
         (update :unencodable conj {:parameter p :problems problems})
         (and (empty? problems) (some? w) (not (and (vector? w) (empty? w))))
         (assoc-in [:values p] w)
         (seq objs)
         (assoc-in [:objects p] (mapv #(select-keys % [:id :object-type-ref :values]) objs)))))
   {:values {} :objects {} :unencodable []}
   terms))

(defn evaluate-action-params
  [world ctx io action-name args]
  (let [v (volatile! nil)
        _ (run world {} ctx io
               (fn [run-state cx]
                 (vreset! v (ra/evaluate-params (:world run-state) cx action-name args)) run-state))
        {:keys [params bindings terms missing invalid violated blocked?]} @v
        {:keys [values objects unencodable]} (form-values terms bindings)]
    {:params   (update-vals params #(csub/param-descriptor (dissoc % :overrides)))
     :values   values
     :objects  objects
     :unencodable unencodable
     :missing  (vec missing)
     :invalid  (vec invalid)
     :violated (mapv #(select-keys % [:parameter :constraint :value :allowed])
                     (violations-worded params violated))
     :blocked? blocked?
     :reason      (blocked-reason (:schema ctx) @v)
     :reason-code (blocked-code @v)}))
