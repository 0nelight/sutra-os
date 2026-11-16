(ns sutra.actions.options
  (:require [sutra.store.access.scope :as ctx]
            [sutra.store.access.substrate :as sub]
            [sutra.catalog.actions :as actions]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.lookup :as lookup]
            [sutra.types.world-rules :as rules]
            [clojure.string :as str]
            [sutra.foundation.wire :as wire]
            [sutra.actions.action :as action]
            [sutra.actions.operations :as ops]
            [sutra.actions.request :as request]
            [sutra.store.seat :as seat]
            [sutra.objects.api :as objects]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.types.object-set :as os]
            [sutra.types.submission :as csub]))

(set! *warn-on-reflection* true)

(def action-option-max 500)

(defn options-page
  [label rows]
  {:rows      (mapv #(csub/option-row label %) (take action-option-max rows))
   :truncated (> (count rows) action-option-max)})

(defn- text-attrs
  [schema t]
  (->> (lookup/attrs schema t)
       (filter #(#{:string :text} (:attribute-type %)))
       (mapv :attribute-name)))

(defn- option-shows
  [sch rid label property]
  (when rid
    (let [base {:label      label
                :property   property
                :match-keys (:properties (rules/object-key sch rid))}
          kws  #(some->> % (into [] (comp (map ->kw) (distinct))) not-empty)]
      [(kws (csub/option-row-show (assoc base :text (text-attrs sch rid))))
       (kws (csub/option-row-show base))])))

(defn- search-hits
  [reads rights schema needle types limit]
  (let [text-attrs (partial text-attrs schema)]
    (take limit
          (for [t types
                :let [s     (get-in schema [:object-types t])
                      attrs (text-attrs t)]
                :when (and (seq attrs)
                           (grants/read-allowed? rights (:project s)
                                                 (:ignore-inherited s)))
                row (sub/select reads {:root  t
                                       :where (into [:or]
                                                    (map (fn [a] [:contains-ci a needle]))
                                                    attrs)
                                       :limit limit})]
            [t row]))))

(defn- param-type-rid
  [sch t]
  (let [s (if (keyword? t) (str (symbol t)) (some-> t str))]
    (when-not (str/blank? s)
      (let [r (lookup/->type-rid sch :object-types s)]
        (when (get-in sch [:object-types r]) r)))))

(defn- from-source-query
  [sch of]
  (let [set-spec (when (:object-set of) (os/by-name (:object-sets sch) (:object-set of)))
        paged?   (boolean (or (:limit set-spec) (:offset set-spec) (:order-by set-spec)))
        src-rid  (param-type-rid sch (:object-type of))
        src      (or (some-> src-rid (->> (get (:object-types sch))) :object-type-name)
                     (->kw (:object-type of)))
        prop     (->kw (:property of))
        label    (or (:label of)
                     (when (and src-rid (not (:property of)))
                       (get-in sch [:object-types src-rid :title-key])))
        [show keep] (option-shows sch src-rid label prop)]
    {:query    (cond-> (if src {:find src} {:object-set (:object-set of)})
                 (not paged?) (assoc :limit (inc action-option-max))
                 show         (assoc :show show))
     :rid      src-rid
     :label    label
     :keep     keep
     :property prop
     :args     (:args of)}))

(defn- object-type-query
  [sch pspec]
  (when-let [rid (param-type-rid sch (:type pspec))]
    (when-let [ot (get (:object-types sch) rid)]
      (let [title   (:title-key ot)
            stored? (some #(= title (:attribute-name %)) (:attribute-list ot))
            [show keep] (option-shows sch rid title nil)]
        {:query (cond-> {:find (:object-type-name ot) :limit (inc action-option-max)}
                  (and title stored?) (assoc :order-by title)
                  show                (assoc :show show))
         :rid   rid
         :label title
         :keep  keep}))))

(defn- interface-query
  [sch t]
  (let [s (if (keyword? t) (str (symbol t)) (some-> t str))]
    (when-not (str/blank? s)
      (let [rid (lookup/->type-rid sch :interfaces s)]
        (when-let [iface (get-in sch [:interfaces rid])]
          (let [impls   (rules/implementers sch rid)
                titles  (into #{} (map #(get-in sch [:object-types % :title-key])) impls)
                title   (when (= 1 (count titles)) (first titles))
                stored? (and title
                             (every? (fn [ot]
                                       (some #(= title (:attribute-name %))
                                             (get-in sch [:object-types ot :attribute-list])))
                                     impls))
                show    (not-empty (into [] (comp (map ->kw) (distinct))
                                         (csub/option-row-show {:label title})))]
            {:query (cond-> {:find (:interface-name iface) :limit (inc action-option-max)}
                      stored? (assoc :order-by title)
                      show    (assoc :show show))
             :label title
             :keep  show}))))))

(defn- user-editable?
  [pspec]
  (not (or (#{:hidden :disabled} (->kw (:exposure pspec)))
           (false? (:editable pspec)))))

(defn param-option-query
  [sch pspec]
  (if-let [of (:options-from pspec)]
    (from-source-query sch of)
    (when (user-editable? pspec)
      (or (object-type-query sch pspec)
          (interface-query sch (:type pspec))))))

(defn- searched-ids
  [reads rights sch rid term]
  (into [] (comp (map second) (keep :id) (map str) (distinct))
        (search-hits reads rights sch (str/lower-case (str/trim term)) [rid]
                     (inc action-option-max))))

(defn- search-term
  [t]
  (when (and (string? t) (not (str/blank? t))) t))

(defn- param-options
  [query ids-of values term {:keys [label args rid] q :query}]
  (let [set-args (into {} (keep (fn [[slot param]]
                                  (when-some [v (get values param)] [slot v])))
                       args)
        ids      (when (and term rid (:find q))
                   (ids-of rid term))]
    (options-page
     label
     (if (and ids (empty? ids))
       []
       (:rows (query (cond-> q
                       (seq set-args) (assoc :object-set-args set-args)
                       ids            (assoc :ids ids)
                       (and term label (not rid) (:find q))
                       (update :where merge {label {:contains-ci term}}))))))))

(defn- action-param-options
  [store sch branch action-name values search]
  (let [params (:params (actions/resolve-action sch action-name))]
    (when (some #(param-option-query sch %) (vals params))
      (let [live   (seat/rebuild-schema-from-storage (ctx/at store branch))
            query  (objects/object-query store live)
            rights (grants/read-rights store)
            ids-of #(searched-ids (sub/reader store) rights live %1 %2)]
        (reduce (fn [[options errors] [p pspec]]
                  (if-let [src (param-option-query live pspec)]
                    (try
                      [(assoc options
                              p (param-options query ids-of values
                                               (search-term (get search p)) src))
                       errors]
                      (catch InterruptedException e (throw e))
                      (catch Exception e
                        [options (assoc errors p (or (ex-message e) (str (class e))))]))
                    [options errors]))
                [{} {}]
                params)))))

(defn evaluate-action-params!
  ([store schema action-name args] (evaluate-action-params! store schema action-name args nil))
  ([store schema action-name args search]
   (action/refuse-sys! action-name)
   (let [{req-store :store req-schema :schema :keys [branch] :as req}
         (request/request-on store {:action action-name :branch (:branch store) :schema schema})]
     (action/guard-action-request! (-> (select-keys req [:authority :branch :action])
                                       (assoc :dry-run? true))
                                   args)
     (let [[ctx io]  (action/run-for req nil)
           evaluated (ops/evaluate-action-params req-schema (action/interp-ctx ctx) io
                                                 action-name args)
           [options
            errors] (action-param-options req-store req-schema branch action-name args search)
           answer   (cond-> (assoc evaluated :options options)
                      (seq errors) (assoc :option-errors errors))]
       (when-let [p (wire/problem [:http :action-params] answer)]
         (println (str "[wire] action-params of " action-name
                       " is not what a browser reads: " p)))
       answer))))
