(ns sutra.catalog.actions
  (:require [sutra.types.rid :as rid]
            [sutra.types.schema :as sch]
            [sutra.types.entities :as ents]
            [sutra.types.lifecycle :as lifecycle]
            [sutra.types.submission :as csub]
            [sutra.parser.spec :as spec]
            [sutra.parser.lint :as lint]
            [sutra.catalog.lookup :as lookup]))

(set! *warn-on-reflection* true)

(defn- passthrough-action
  [op & fields]
  {:rules [(into {:op op} (map (fn [f] [f [:get :arg0 f]])) fields)]})

(def entity-fields
  (into {} (map (juxt :tag (comp ents/forwarded-fields :tag))) ents/mutable))

(def schema-action-kinds
  (into {} (for [{:keys [tag section]} ents/mutable
                 verb [:create :update :delete]]
             [(keyword (str (name verb) "-" (name tag))) {:verb verb :kind tag :section section}])))

(def schema-actions
  (into {} (for [[op {:keys [verb kind]}] schema-action-kinds]
             [op
              (apply passthrough-action op (if (= :delete verb) [:name] (entity-fields kind)))])))

(def platform-actions
  (merge
   schema-actions
   {:apply-marking
    {:rules [{:op :apply-marking  :target [:get :arg0 :id]
              :object-type [:get :arg0 :object-type] :marking [:get :arg0 :marking]}]}
    :remove-marking
    {:rules [{:op :remove-marking :target [:get :arg0 :id]
              :object-type [:get :arg0 :object-type] :marking [:get :arg0 :marking]}]}}))

(defn app-lint-findings
  [nm spec rules]
  (filterv (comp rules :rule) (lint/run-lint {:applications {nm spec}} (spec/vocabulary))))

(defn- with-lifecycle
  [schema action-name action]
  (let [types (for [[_ ot] (:object-types schema) :when (:lifecycle ot)]
                [(:object-type-name ot) ot])]
    (if-let [tr (and (seq types) (lifecycle/transition-for types action-name))]
      (let [trid (lookup/rid-of schema :object-types (:object-type tr))]
        (lifecycle/expand-action action (assoc tr :object-type trid)
                                 #(and (some? %) (= trid (lookup/rid-of schema :object-types %)))))
      action)))

(defn named-action-params
  [schema action]
  (let [idx (sch/rid-name-index schema)]
    (rid/rewrite-action-param-refs (fn [_ v] (or (rid/resolve-rid->name idx v) v)) action)))

(defn resolve-action
  [schema name]
  (or (get platform-actions name)
      (some-> (->> (lookup/action schema name)
                   (named-action-params schema)
                   (with-lifecycle schema name)
                   csub/with-section-overrides)
              (csub/with-on-prefill
                (fn [t]
                  (let [t (if (and (keyword? t) (rid/rid? (subs (str t) 1))) (subs (str t) 1) t)]
                    (lookup/rid-of schema :object-types t)))))))

(defn executable-actions
  [schema]
  (merge platform-actions (lookup/actions-by-name schema)))
