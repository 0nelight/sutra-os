(ns sutra.authoring.http.sources
  (:require [sutra.store.access.scope :as ctx]
            [sutra.kit.redact :as redact]
            [sutra.catalog.kernel :as kernel]
            [sutra.sources.api :as sources]
            [sutra.sources.http :as source]
            [sutra.parser.adopt :as adopt]
            [sutra.actions.api :as actions]
            [sutra.authoring.http.oms :as oms]
            [sutra.authoring.http.mutation :as mutation]
            [sutra.authoring.mutation :refer [run-step]]
            [sutra.http.endpoint :as ep]
            [sutra.kit.refusal :as refusal]))

(set! *warn-on-reflection* true)

(def upsert-datasource
  (ep/handler :datasources
              {:respond
               (fn [{:keys [store branch schema body edge] project :sutra/project}]
                 (let [{src :ok error :error} (source/request->source-spec body)]
                   (if error
                     (refusal/refuse! :refusal.authoring/source-spec (redact/redact body)
                                      {:error error})
                     (do
                       (when (seq (:password body))
                         (println "WARN upsert-datasource: ignoring :password for"
                                  (:name src) "— store connector secrets in the egress agent"))
                       (source/datasources-after
                        (mutation/answer
                         (run-step (ctx/at store branch) schema
                                   (kernel/step (ctx/code) :create :source src)
                                   {:project project :edge edge})))))))}))

(def delete-datasource
  (ep/handler :datasource
              {:respond (fn [{:keys [store branch schema name edge]}]
                          (if (contains? (source/datasources schema) (keyword name))
                            (source/datasources-after
                             (mutation/answer
                              (run-step (ctx/at store branch) schema
                                        (kernel/step (ctx/code) :delete :source {:name name})
                                        {:edge edge})))
                            (refusal/refuse! :refusal.authoring/datasource-not-found
                                             {:name name})))}))

(defn- live-names [schema section name-key]
  (set (keep name-key (vals (get schema section)))))

(defn adopt-table
  [{{nm :name} :path-params {:keys [table preview?] land-asked :land? :as body} :body-params
    auth :sutra/auth schema :sutra/schema branch :sutra/branch :as req}]
  (if-not table
    (refusal/refuse! :refusal.authoring/adopt-table {})
    (source/with-datasource-spec schema (or branch "main") nm
      (fn [spec]
        (let [cols (sources/with-source [s (ctx/connect auth) spec]
                     (sources/columns s table))
              p    (adopt/plan (merge (select-keys body [:key :into :dataset :as :project])
                                      {:source   nm :table table :columns cols
                                       :existing
                                         {:objects
                                            (live-names schema :object-types :object-type-name)
                                          :datasets (live-names schema :datasets :dataset-name)}}))]
          (if-let [_problem (:problem p)]
            (refusal/refuse! :refusal.authoring/adopt-refused (dissoc p :message)
                             {:message (:message p)})
            (let [resp
                    (oms/install-preset (assoc req
                                               :body-params
                                                 {:schema (:schema p)
                                                  :preview? (boolean preview?)}))
                  go?  (and (<= 200 (:status resp) 299) (not preview?)
                            (actions/land? req land-asked))]
              (cond->> (update resp :body merge (select-keys p [:dataset :key :type]))
                go? (actions/land! req)))))))))

(def module
  {:name        :source-authoring
   :contributes {:http/routes
                 {:datasource          {:delete delete-datasource}
                  :datasource-adopt    {:post adopt-table}
                  :datasources         {:post upsert-datasource}}}})
