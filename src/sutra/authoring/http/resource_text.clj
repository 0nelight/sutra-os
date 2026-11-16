(ns sutra.authoring.http.resource-text
  (:require [clojure.edn :as edn]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [sutra.parser.project-emit :as pemit]
            [sutra.parser.sugar :as sugar]
            [sutra.authoring.http.oms :as oms]
            [sutra.http.responses :refer [ok]]
            [sutra.kit.refusal :as refusal]
            [sutra.catalog.lookup :as lookup]
            [sutra.types.entities :as ents]
            [sutra.types.schema :as sch]
            [sutra.types.shape :as tshape]
            [sutra.types.versions :as vers]))

(set! *warn-on-reflection* true)

(defn text
  [schema section rid spec]
  (let [named (get (pemit/names-resolved (sch/rid-name-index schema) section {rid spec}) rid)]
    (with-out-str (pprint/pprint (tshape/reading-order
                                  (sugar/raise-name-sugar
                                   section
                                   (vers/authored section (tshape/->author section named {}))))))))

(defn- section-of [{:keys [section]}]
  (let [s (keyword section)] (when (contains? ents/text-sections s) s)))

(defn read-text
  [{:keys [path-params] schema :sutra/schema}]
  (let [{nm :name} path-params
        s   (section-of path-params)
        rid (when s (lookup/rid-of schema s (keyword nm)))]
    (cond
      (nil? s)    (refusal/refuse! :refusal.authoring/section-not-text
                                   {:section (:section path-params)}
                                   {:sections (str/join ", " (map name (sort ents/text-sections)))})
      (nil? rid)  (refusal/refuse! :refusal.authoring/not-declared {:section (name s) :name nm})
      :else       (ok {:section (name s) :name nm
                       :content (text schema s rid (get-in schema [s rid]))}))))

(defn save-text
  [{:keys [path-params body-params] :as req}]
  (let [{nm :name} path-params
        s    (section-of path-params)
        spec (try (edn/read-string (str (:content body-params))) (catch Exception e e))]
    (cond
      (nil? s)                   (refusal/refuse! :refusal.authoring/section-not-edited
                                                  {:section (:section path-params)})
      (instance? Exception spec) (refusal/refuse! :refusal.authoring/not-edn {:name nm}
                                                  {:reason (ex-message spec)})
      (not (map? spec))          (refusal/refuse! :refusal.authoring/entry-not-map {:name nm}
                                                  {:section-name (name s)})
      :else
      (let [r    (oms/stage-schema req {:schema   {s {(keyword nm) spec}}
                                        :preview? (boolean (:preview body-params))}
                                   nil)
            errs (get-in r [:body :data :errors])]
        (cond-> r
          (seq errs) (update-in [:body :error] str "\n" (str/join "\n" (map str errs))))))))

(def module
  {:name        :resource-text
   :contributes {:http/routes {:resource-file {:get read-text :post save-text}}}})
