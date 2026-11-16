(ns sutra.apps.http.files
  (:require [sutra.kit.refusal :as refusal]
            [sutra.foundation.encoding :as enc]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.parser.ent :as ent]
            [sutra.parser.project-export :as pe]
            [sutra.parser.project-emit :as pemit]
            [sutra.http.endpoint :as ep]
            [sutra.authoring.http.mutation :as mutation]
            [sutra.authoring.api :as authoring]
            [sutra.authoring.http.oms :as oms]
            [sutra.http.responses :refer [ok]]
            [sutra.store.access.scope :as ctx]
            [sutra.catalog.lookup :as lookup]
            [sutra.types.rid :as rid]
            [sutra.types.schema :as sch]))

(set! *warn-on-reflection* true)

(defn rename-application
  [{:keys [path-params body-params] store :sutra/store schema :sutra/schema branch :sutra/branch
    edge :sutra/edge}]
  (let [nm   (->kw (:name path-params))
        spec (get (:applications (rid/name-keyed (or schema {}))) nm)]
    (if-let [ff (:form-factor spec)]
      (refusal/refused :refusal.apps/cannot-rename-application-it-is {} {:nm (name nm) :ff (name ff)})
      (mutation/answer
       (authoring/rename-entity (ctx/at store branch) schema :applications (:name path-params)
                                (:to body-params) {:edge edge})))))

(def list-applications
  (ep/handler :applications
              {:call (fn [{:keys [schema]}]
                       {:applications (:applications (rid/name-keyed schema))})}))

(defn named-entry
  [schema kk nm]
  (when-let [r (when-not (rid/rid? nm) (lookup/rid-of schema kk nm))]
    (some->> (get-in schema [kk r]) (vector r))))

(def application-file
  (ep/handler :application-file
              {:respond (fn [{:keys [schema name]}]
                          (if-let [[rid spec] (named-entry schema :applications name)]
                            (let [named (-> (pemit/names-resolved (sch/rid-name-index schema)
                                                               :applications {rid spec})
                                            (get rid))
                                  unit (pemit/app-unit named {:inline? true})]
                              (if-let [[{:keys [path
                                                content]}]
                                       (pe/unit-files schema :code-repository unit)]
                                (ok {:name name :path path :content content :file (:doc unit)})
                                (refusal/refused :refusal.apps/no-text-kind-of-the {})))
                            (refusal/refused :refusal.apps/no-application-named {} {:name name})))}))

(defn- view-doc
  [{:keys [object-type project layout sidebar live auto-publish]} ff]
  (let [ff (keyword (clojure.core/name ff))]
    (cond-> {:object-type (keyword (clojure.core/name object-type))
             ff           (or layout {:kind :rows :items []})}
      project      (assoc :project (keyword (clojure.core/name project)))
      (and (= :full ff) (seq sidebar)) (assoc :sidebar sidebar)
      live         (assoc :live live)
      auto-publish (assoc :auto-publish true))))

(defn- sibling-views
  [schema object-type ff auto?]
  (let [idx (sch/rid-name-index schema)
        nm  #(some-> % clojure.core/name)]
    (for [[rid spec] (:applications schema)
          :when (:form-factor spec)
          :let  [named (get (pemit/names-resolved idx :applications {rid spec}) rid)]
          :when (and (= (nm object-type) (nm (:object-type named)))
                     (not= (nm ff) (nm (:form-factor named)))
                     (not= auto? (true? (:auto-publish named))))]
      (:doc (pemit/app-unit named {:inline? true})))))

(defn- parse-view
  [schema file ff path]
  (let [auto? (true? (:auto-publish file))
        docs  (cons (assoc file :auto-publish auto?)
                    (map #(assoc % :auto-publish auto?)
                         (sibling-views schema (:object-type file) ff auto?)))]
    {:schema {:applications
              (into {} (mapcat #(get-in (ent/parse-entv (view-doc % (:form-factor %)) path)
                                        [:schema :applications]))
                    docs)}}))

(defn save-application-file
  [{{:keys [name]} :path-params :keys [body-params] schema :sutra/schema :as req}]
  (let [{:keys [file content]} body-params
        file (or file (enc/edn-or-nil content))]
    (cond
      (not (map? file)) (refusal/refused :refusal.apps/save-needs-a-file-map body-params)
      :else
      (let [named  (assoc file :name (keyword (or (some-> (:name file) clojure.core/name) name)))
            parsed (try {:ok (if-let [ff (:form-factor file)]
                               (parse-view schema file ff (str name ".entv"))
                               (ent/parse-enta named (str name ".enta")))}
                        (catch clojure.lang.ExceptionInfo e {:msg (ex-message e)}))]
        (if-let [msg (:msg parsed)]
          (refusal/refused :refusal.apps/passed-msg {:app name} {:message msg})
          (oms/install-preset
           (assoc req :body-params {:schema (:schema (:ok parsed))})))))))
