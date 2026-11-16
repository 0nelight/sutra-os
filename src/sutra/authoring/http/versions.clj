(ns sutra.authoring.http.versions
  (:require [sutra.store.control :as ctl]
            [clojure.pprint :as pprint]
            [sutra.parser.project-emit :as pemit]
            [sutra.authoring.http.oms :as oms]
            [sutra.http.responses :refer [ok]]
            [sutra.kit.refusal :as refusal]
            [sutra.catalog.functions :as functions]
            [sutra.catalog.branch-policy :as policy]
            [sutra.catalog.lookup :as lookup]
            [sutra.types.schema :as sch]
            [sutra.types.semver :as semver]
            [sutra.types.shape :as tshape]
            [sutra.types.versions :as vers]))

(set! *warn-on-reflection* true)

(defn- resource
  ([schema path-params] (resource schema path-params vers/of))
  ([schema {:keys [section name]} keeps?]
   (let [sec (keyword section)
         rid (when (keeps? sec) (lookup/rid-of schema sec (keyword name)))]
     (some->> (when rid (get-in schema [sec rid])) (vector sec rid)))))

(defn- authored [schema sec rid spec]
  (tshape/reading-order
   (vers/authored sec
                  (tshape/->author sec
                                   (get
                                    (pemit/names-resolved (sch/rid-name-index schema) sec
                                                          {rid spec})
                                    rid) {}))))

(defn- text [m] (with-out-str (pprint/pprint m)))

(defn- missing
  ([path-params] (missing path-params "keeps"))
  ([{:keys [section name]} verb]
   (refusal/refuse! :refusal.authoring/no-versioned {:section section :name name}
                    {:verb verb})))

(defn list-versions
  [{:keys [path-params] schema :sutra/schema}]
  (if-let [[sec _ spec] (resource schema path-params)]
    (ok (assoc (vers/listing (cond-> spec (not (vers/publishes? sec))
                                     (assoc :published (:version spec))))
               :mode (vers/mode sec) :section (:section path-params) :name (:name path-params)))
    (missing path-params)))

(defn read-version
  [{:keys [path-params] schema :sutra/schema}]
  (if-let [[sec rid spec] (resource schema path-params)]
    (let [at (authored schema sec rid (vers/at-version spec (:version path-params)))]
      (ok {:version (:version path-params) :spec at :content (text at)
           :head (text (authored schema sec rid spec))}))
    (missing path-params)))

(defn publish
  [{:keys [path-params body-params] store :sutra/store schema :sutra/schema :as req}]
  (if-let [[sec rid spec] (resource schema path-params vers/publishes?)]
    (let [published (vers/publish spec {:version (:version body-params)
                                        :note    (not-empty (str (:note body-params)))
                                        :by      (some-> (policy/actor-name store) str)
                                        :at      (ctl/stamp store)})
          head      (merge (authored schema sec rid spec)
                           (select-keys published [:releases :release-notes :published]))]
      (oms/stage-schema req {:schema   {sec {(keyword (:name path-params)) head}}
                             :preview? (boolean (:preview body-params))}
                        nil))
    (missing path-params "publishes")))

(defn- next-patch
  [spec]
  (when-let [{:keys [major minor patch]} (semver/parse (peek (functions/released-versions spec)))]
    (str major "." minor "." (inc patch))))

(defn restore
  [{:keys [path-params body-params] schema :sutra/schema :as req}]
  (if-let [[sec rid spec] (resource schema path-params #(= :release (vers/mode %)))]
    (let [v  (some-> (:version body-params) str not-empty)
          to (or (some-> (:as body-params) str not-empty) (next-patch spec))]
      (if-not (and v (contains? (:releases spec) v))
        (refusal/refuse! :refusal.authoring/name-release
                         {:version v :versions (functions/released-versions spec)})
        (oms/stage-schema
         req
         {:schema   {sec {(keyword (:name path-params))
                          (-> (authored schema sec rid (vers/at-version spec v))
                              (assoc :version to)
                              (dissoc :releases :release-notes :published :libraries))}}
          :preview? (boolean (:preview body-params))}
         nil)))
    (missing path-params "releases")))

(def module
  {:name        :versions
   :contributes {:http/routes {:resource-versions         {:get list-versions}
                               :resource-version          {:get read-version}
                               :resource-versions-publish {:post publish}
                               :resource-versions-restore {:post restore}}}})
