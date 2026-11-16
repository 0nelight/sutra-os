(ns sutra.apps.http.query
  (:require [sutra.builds.api :as builds]
            [sutra.catalog.lookup :as lookup]
            [sutra.kit.tree :as tree]
            [clojure.string :as str]
            [sutra.objects.api :as objects]
            [sutra.kit.anomaly :as anomaly]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.http.responses :refer [negotiated object-set-rows]]
            [sutra.catalog.functions :as functions]
            [sutra.store.world :as world]
            [sutra.types.link-side :as ls]
            [sutra.types.rid :as rid]
            [sutra.types.transform-io :as tio]
            [sutra.actions.api :as actions]
            [sutra.apps.http.scope :refer [app-type-name in-scope not-in-app]]
            [sutra.apps.http.session :refer [with-app]]
            [sutra.apps.http.view :refer [app-read app-view-rids scoped-around view-layouts]]))

(set! *warn-on-reflection* true)

(def ^:private app-aggregate-spec-keys
  #{:find :where :aggs :group-by :order-by :sort-dir :limit :offset})

(defn- ->spec-kw
  [x]
  (if (and (string? x) (not (rid/rid? x))) (keyword x) x))

(defn- ->group-term
  [t]
  (cond
    (sequential? t) (mapv ->spec-kw t)
    (map? t)        (update-vals t ->spec-kw)
    :else           (->spec-kw t)))

(defn- spec-kws
  [body ks kw-ks]
  (reduce (fn [m k] (cond-> m (some? (get m k)) (update k ->spec-kw)))
          (select-keys body ks) kw-ks))

(defn- app-aggregate-spec
  [{:keys [aggs group-by] :as body}]
  (cond-> (spec-kws body app-aggregate-spec-keys [:find :order-by :sort-dir])
    (some? aggs)  (assoc :aggs (if (sequential? aggs)
                                 (mapv #(if (map? %) (update-vals % ->spec-kw) %) aggs)
                                 (->spec-kw aggs)))
    (seq group-by) (assoc :group-by (mapv ->group-term group-by))))

(defn- query-refusal
  [{:keys [keys required what missing]} {:keys [rid schema scope auth]} body spec root]
  (let [extra       (into (sorted-set) (remove keys) (clojure.core/keys (or body {})))
        link-of     (fn [k] (or (lookup/->type-rid schema :link-types k)
                                (when root
                                  (try (:link (ls/resolve-side schema root k))
                                       (catch clojure.lang.ExceptionInfo _ nil)))))
        stray-links (into (sorted-set)
                          (keep (fn [k]
                                  (when-let [l (link-of k)]
                                    (when-not (in-scope auth schema scope :link l)
                                      k))))
                          (clojure.core/keys (:where spec)))]
    (cond
      (seq extra)
      [400 {:error   (str "not part of " what ": " (str/join ", " (map pr-str extra)))
            :allowed (vec (sort keys))}]

      (nil? (required spec))
      [400 {:error (str required " is required — this endpoint " missing
                        "; the row read is GET /api/my-application/" rid)}]

      (nil? root)         [404 (not-in-app :type (:find spec))]
      (seq stray-links)   [404 (apply not-in-app :link stray-links)])))

(defn- app-query
  [req {:keys [spec run] :as q}]
  (with-app req {:seg :query :preview? true}
    (fn [{:keys [schema branch seg sb body scope store auth] :as app}]
      (let [spec (spec (or body {}))
            root (in-scope auth schema scope :type (:find spec))]
        (if-let [[status answer] (query-refusal q (select-keys app [:rid :schema :scope :auth])
                                                body spec root)]
          (negotiated req status answer)
          (let [spec (assoc spec :find (app-type-name schema root))]
            (app-read store branch sb
                      (fn [store sch]
                        (negotiated req (assoc (run store sch spec) :scenario seg))))))))))

(defn my-application-aggregate
  [req]
  (app-query req {:keys     app-aggregate-spec-keys
                  :spec     app-aggregate-spec
                  :required :aggs
                  :what     "an app aggregation"
                  :missing  "answers a question about the bound objects"
                  :run      objects/run-object-query}))

(def ^:private app-objects-spec-keys
  #{:find :where :order-by :sort-dir :limit :offset})

(defn my-application-objects
  [req]
  (app-query req {:keys     app-objects-spec-keys
                  :spec     #(spec-kws % app-objects-spec-keys [:find :order-by :sort-dir])
                  :required :find
                  :what     "an app's object read"
                  :missing  "reads objects of a type this app binds"
                  :run      objects/run-object-query}))

(defn- function-input-types
  [schema spec]
  (into #{} (keep #(lookup/->type-rid schema :object-types %)) (tio/object-type-inputs spec)))

(defn my-application-function
  [req]
  (with-app req {:seg :query :preview? true}
    (fn [{:keys [rid schema branch seg sb body scope a store auth]}]
      (let [fn-name     (some-> (:function body) ->kw)
            fn-spec     (when fn-name (:spec (functions/function-by-name schema fn-name)))
            args        (into {} (map (fn [[k v]] [(keyword k) v])) (:args body))
            stray-types (into (sorted-set) (remove (:type scope))
                              (function-input-types schema fn-spec))
            unbound     (tio/unbound-scalars fn-spec args)
            unfit       (tio/unfit-scalars fn-spec args)]
        (cond
          (nil? fn-name) (negotiated req 400 {:error ":function is required"})

          (nil? (in-scope auth schema scope :function fn-name))
          (negotiated req 404 (not-in-app :function (:function body)))

          (seq stray-types)
          (negotiated req 404 {:error (str "function " (pr-str (:function body))
                                           " reads a type not in this application")
                               :types (mapv #(app-type-name schema %) stray-types)})

          (seq unbound)
          (negotiated req 400 {:error   (str "function " (pr-str (:function body))
                                             " declares the :scalar input(s) "
                                             (str/join ", " (map pr-str unbound))
                                             " and this call binds nothing to them — bind each"
                                             " in the variable's :args, e.g. {:function "
                                             (pr-str fn-name) " :args {" (pr-str (first unbound))
                                             " {:val …}}}")
                               :unbound (vec unbound)})

          (seq unfit)
          (negotiated req 400 {:error (str "function " (pr-str (:function body))
                                           " takes one value for the :scalar input(s) "
                                           (str/join ", " (map pr-str unfit))
                                           " and this call hands them a map or rows — bind"
                                           " a variable holding the value, e.g. {:val …} on"
                                           " {:object-property … :of :object}")
                               :unfit unfit})

          :else
          (try
            (app-read store branch sb
                      (fn [store sch]
                        (let [rows (actions/run-query-function
                                    store sch fn-name args
                                    {:call-site   :app-variable
                                     :application (:application-name a)})]
                          (negotiated req {:function fn-name
                                           :rows     (object-set-rows rows)
                                           :scenario seg}))))
            (catch clojure.lang.ExceptionInfo e
              (let [d (ex-data e)]
                (println (str "[app-function] " rid " :function " (pr-str fn-name)
                              " failed — " (.getMessage e)))
                (if (and (nil? (anomaly/of e)) (or (:function d) (= fn-name (:transform d))))
                  (negotiated req 400 {:error (.getMessage e)})
                  (throw e))))))))))

(defn my-application-edit-history
  [req]
  (with-app req {:seg :query :preview? true}
    (fn [{:keys [schema branch seg sb body scope store auth]}]
      (let [t    (:type body)
            root (when t (in-scope auth schema scope :object-type t))]
        (cond
          (nil? t)
          (negotiated req 400 {:error ":type is required — the object type whose edits to read"})

          (nil? root)
          (negotiated req 404 (not-in-app :object-type t))

          :else
          (app-read store branch sb
                    (fn [store _]
                      (let [limit (some-> (:limit body) long)
                            {:keys [history total next-token]}
                            (objects/edit-history-page
                             store root (assoc (select-keys body [:pk :token :sort :kinds
                                                                  :editors :props])
                                               :limit limit :authority auth))]
                        (negotiated
                         req
                         (cond-> {:object-type
                                  (get-in schema [:object-types root :object-type-name])
                                  :history         (mapv objects/edit-history-row history)
                                  :total           total
                                  :next-page-token next-token
                                  :scenario        seg}
                           (and (:timeline body) (:pk body))
                           (assoc :timeline (objects/edit-timeline store schema root
                                                                   {:pk (:pk body)
                                                                    :authority auth}))
                           (:provenance body)
                           (assoc :provenance
                                  (world/edit-provenance
                                   (world/at store (or (:branch store) "main")) root
                                   {:pk (:pk body) :limit limit :authority auth}))))))))))))

(defn my-application-data-freshness
  [req]
  (with-app req {:seg :query :preview? true}
    (fn [{:keys [schema branch sb a store auth]}]
      (let [items (into []
                        (comp (mapcat tree/nodes)
                              (filter #(= :data-freshness (->kw (:widget %))))
                              (mapcat :items))
                        (cons (:layout a)
                              (view-layouts schema (app-view-rids auth schema a))))]
        (app-read store branch sb
                  (fn [store _]
                    (negotiated req {:updates (builds/last-updated store schema auth items)})))))))

(defn my-application-around
  [req]
  (with-app req {:seg :query :preview? true}
    #(scoped-around req % "application")))

(def ^:private app-series-spec-keys
  #{:find :where :property :from :until :bucket :rollup :order-by :sort-dir
    :limit :offset})

(defn- app-series-spec
  [body]
  (spec-kws body app-series-spec-keys [:find :property :bucket :rollup :order-by :sort-dir]))

(defn my-application-series
  [req]
  (app-query req {:keys     app-series-spec-keys
                  :spec     app-series-spec
                  :required :property
                  :what     "an app series read"
                  :missing  "reads a time-series property"
                  :run      objects/run-series-query}))
