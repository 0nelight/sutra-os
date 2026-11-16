(ns sutra.actions.dispatch
  (:require [sutra.kit.anomaly :as anomaly]
            [sutra.kit.refusal :as refusal]
            [sutra.store.access.run-log :as run-log]
            [sutra.catalog.world-columns :as kc]
            [sutra.catalog.structure :as structure]
            [sutra.governance.api :as governance]
            [sutra.catalog.shape :as shape]
            [sutra.actions.action :as action]
            [sutra.actions.publish :as publish]
            [sutra.catalog.program :as prog]
            [sutra.actions.request :as request]
            [sutra.parser.spec :as spec]
            [sutra.ref.expr :as rexpr]
            [sutra.store.tx-vector :as tv]
            [sutra.types.schema :as sch]))

(set! *warn-on-reflection* true)

(defn- array-cols-for
  [schema table-name]
  (when schema
    (get (sch/column schema ::kc/array-cols kc/array-cols-index) table-name)))

(def empty-schema shape/empty-schema-map)

(defn function [opts]
  (assoc opts ::function true))

(defn- function? [x]
  (and (map? x) (::function x)))

(defn- run-on-branch!
  [store req]
  (anomaly/unwrap!
   (action/run-action! (request/request-on (governance/data-guarded store)
                                           (assoc req :branch (:branch store))))))

(defn dispatch-action!
  ([store schema action-name args] (dispatch-action! store schema action-name args nil))
  ([store schema action-name args {:keys [records provenance]}]
   (run-on-branch! store (cond-> {:schema schema :action action-name :args args}
                           (seq records) (assoc :records (vec records))
                           provenance    (assoc :provenance provenance)))))

(defn dispatch-action-batch!
  ([store schema action-name
    arg-maps] (dispatch-action-batch! store schema action-name arg-maps nil))
  ([store schema action-name arg-maps {:keys [records]}]
   (run-on-branch! store (cond-> {:schema schema :action action-name :requests (vec arg-maps)}
                           (seq records) (assoc :records (vec records))))))

(defn- propose-logic
  [req policy rule inputs fn-name]
  (-> (action/evaluating
       req policy
       (fn [run]
         (let [{:keys [result]}
               (let [[ctx io] (action/run-for req run :as :propose :bindings inputs)]
                 (anomaly/unwrap! (action/eval-rules ctx io rule)))
               txv (into (vec (:ops result)) (:side-effects result))]
           (when-let [w (some #(when (tv/store-write? %) %) txv)]
             (refusal/refuse! :refusal.actions/propose-direct-write
                             {:function fn-name :op (:op w)}
                             {:fn (name fn-name) :op (name (:op w))}))
           {:proposals (vec (:proposals result)) :decisions (vec (:decisions result))})))
      (select-keys [:proposals :decisions])))

(defn- apply-logic!
  [req policy rule inputs fn-name expect]
  (let [r (anomaly/unwrap!
           (action/run-with-log!
            req policy
            (fn [ctx io]
              (anomaly/then
               (action/eval-rules (assoc ctx :bindings inputs :mode :collect) io rule)
               #(assoc % :collected (:collected (:result %)))))
            {:function fn-name :inputs inputs}
            {:expect expect}))]
    {:proposals [] :decisions (vec (:decisions r)) :applied (vec (:collected r))}))

(defn run-logic!
  [store schema fn-name inputs {:keys [mode expect] :as opts}]
  (let [rule   [(merge {:op spec/function-rule-op :function fn-name}
                       (select-keys opts [:version :auto-upgrade]))]
        req    (request/request-on store
                                   (merge {:branch (:branch store) :schema schema :action fn-name}
                                          (select-keys opts [:provenance :trace])))
        policy (action/logic-policy (:schema req) fn-name)]
    (if (= :propose mode)
      (propose-logic req policy rule inputs fn-name)
      (apply-logic! req policy rule inputs fn-name expect))))

(defn- assert-known-entities!
  [schema compiled]
  (let [violations (structure/function-schema-violations schema compiled)]
    (when (seq violations)
      (refusal/refuse! :refusal.actions/function-unknown-entities
                      {:violations violations}
                      {:refs (pr-str (mapv (juxt :kind :ref) violations))}))))

(defn compile-edit-function
  [schema {:keys [language source types inputs target-type] :as wf}]
  (let [parsed   (prog/parse-source {:language language :source source :schema schema})
        params   (reduce-kv (fn [m k v] (assoc-in m [k :type] v)) (or (:params parsed) {}) types)
        compiled (prog/compile-function schema (:steps parsed) params)]
    (assert-known-entities! schema compiled)
    {:compiled       compiled
     :input-bindings (prog/bind-inputs (:param-bindings compiled) (or inputs {}))
     :target-type    target-type
     :original       wf}))

(defn- run-rules!
  [store schema rules]
  (anomaly/unwrap!
   (action/run-with-log! (request/request-on store {:action nil :schema schema})
                         nil
                         (fn [ctx io] (action/eval-rules ctx io rules))
                         {:rule-count (count rules)}
                         nil)))

(defn- run-function!
  [store schema wf]
  (let [{:keys [compiled input-bindings target-type]} (compile-edit-function schema wf)
        req        (request/request-on (governance/data-guarded store) {:schema schema})
        run-schema (update (:schema req) :object-sets merge (:object-sets compiled))
        req        (assoc req :schema run-schema)]
    (anomaly/unwrap!
     (action/settled-run
      req
      (action/run-policy run-schema
                         {:spec  wf
                          :label (str "function " (some-> (:function-name wf) name))})
      (fn [run]
        (let [[ctx io] (action/run-for req run
                                       :bindings input-bindings
                                       :action-type target-type
                                       :staged? (boolean (:staged-writes wf)))]
          (action/eval-rules ctx io (:steps compiled))))
      {:log-fn (fn [_ _] [(run-log/action-entry :function
                                                {:target     target-type
                                                 :step-count (count (:steps compiled))}
                                                nil nil)])}))
    {:compiled compiled
     :result   {:branch        (:branch req)
                :schema        run-schema}}))

(defn submit!
  [store schema batch]
  (if-let [wf (some #(when (function? %) %) batch)]
    (run-function! store schema wf)
    (do (run-rules! store schema batch) nil)))

(def module
  {:name     :ontology
   :provides {:schema/array-cols array-cols-for
              :objects/publish-index publish/publish!
              :rules/known-ops   action/rule-ops
              :expr/field-refs   rexpr/field-refs
              :expr/unknown-ops  rexpr/unknown-ops}})
