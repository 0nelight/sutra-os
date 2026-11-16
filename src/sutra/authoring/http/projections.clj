(ns sutra.authoring.http.projections
  (:require [sutra.kit.anomaly :as anomaly]
            [sutra.parser.project-export :as pe]
            [sutra.parser.projection :as proj]
            [sutra.authoring.projections :refer [pull push!]]
            [sutra.http.responses :refer [ok]]
            [sutra.kit.refusal :as refusal]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)

(defn list-projections
  [{schema :sutra/schema}]
  (ok {:projections (vec (for [[nm p] (sort-by (comp str key) (:projections schema))]
                           (-> (select-keys p [:filename :select :edits :description])
                               (assoc :name (name nm)))))}))

(defn- ctx-of
  [{store :sutra/store schema :sutra/schema branch :sutra/branch project :sutra/project
    e :sutra/edge}]
  {:store store :schema schema :branch (or branch pe/trunk-branch) :project project :edge e})

(def ^:private shown
  [:kind :files :sections :formats :projection :conflicts :problems :edits :nodes :errors
   :tally :planned])

(def ^:private renderings
  (atom []))

(defn- answered
  [f]
  (try (ok (binding [proj/*rendering-cache* renderings] (f)))
       (catch clojure.lang.ExceptionInfo e
         (let [r    (or (anomaly/of e) (throw e))
               data (anomaly/data r)
               c    (anomaly/category r)]
           (anomaly/refused c (anomaly/message r)
                            (merge (select-keys data [:refusal/code :refusal/params])
                                   (select-keys data (cond (= :not-found c) [:name]
                                                           (and (= :conflict c)
                                                                (not= ::proj/conflict (:kind data)))
                                                           [:status :conflicts]
                                                           :else shown))))))))

(defn- query [req k] (not-empty (str (get-in req [:query-params k]))))

(defn projection-files
  [{{nm :name} :path-params :as req}]
  (answered #(pull (ctx-of req) nm
                   {:texts   (some-> (query req "texts") (str/split #",") (->> (remove str/blank?))
                                     seq)
                    :project (query req "project")}
                   (= "true" (query req "rehome")))))

(defn write-projection-files
  [{{nm :name} :path-params body :body-params :as req}]
  (if-not (map? (:files body))
    (refusal/refuse! :refusal.authoring/projection-files {:name nm})
    (answered #(push! (ctx-of req) nm body))))

(def module
  {:name        :projections
   :contributes {:http/routes
                 {:projections      {:get list-projections}
                  :projection-files {:get projection-files :post write-projection-files}}}})
