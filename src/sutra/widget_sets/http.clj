(ns sutra.widget-sets.http
  (:require [sutra.http.endpoint :as ep]
            [sutra.kit.refusal :as refusal]
            [clojure.string :as str]
            [sutra.apps.http.session :refer [with-app]]
            [sutra.calx.io.zip :as zip]
            [sutra.capps.custom-widget :as cw]
            [sutra.catalog.project-scope :as scope]
            [sutra.http.responses :refer [negotiated ok]]
            [sutra.kit.tree :as tree]
            [sutra.widget-sets.records :as records])
  (:import [java.util Base64]))

(set! *warn-on-reflection* true)

(defn- ctx
  [req]
  {:store  (:sutra/store req)
   :auth   (:sutra/auth req)
   :schema (:sutra/schema req)
   :nm     (get-in req [:path-params :name])
   :body   (:body-params req)})

(defn- username [req] (ep/signed-in-user req))

(defn list-sets
  [req]
  (let [{:keys [store auth]} (ctx req)]
    (ok {:widget-sets (records/list-sets store auth)})))

(defn create-set
  [req]
  (let [{:keys [store auth schema body]} (ctx req)
        project (scope/resolve-project schema (:project body))]
    (scope/require-home! project)
    (ok {:widget-set
         (records/create! store auth {:name        (some-> (:name body) name str/trim)
                                      :description (:description body)
                                      :project     project
                                      :author      (username req)})})))

(defn get-set
  [req]
  (let [{:keys [store auth nm]} (ctx req)]
    (ok {:widget-set (records/details store auth nm)})))

(defn delete-set
  [req]
  (let [{:keys [store auth nm]} (ctx req)]
    (records/delete! store auth nm)
    (ok)))

(defn- unzipped
  [b64]
  (let [files  (try
                 (update-vals (zip/read-entries
                               (.decode (Base64/getDecoder) ^String b64)
                               {:max-total-bytes cw/max-release-bytes
                                :max-entry-bytes cw/max-release-bytes
                                :max-entries     2000})
                              #(String. ^bytes % "UTF-8"))
                 (catch clojure.lang.ExceptionInfo e
                   (if (:zip/refused (ex-data e))
                     (refusal/refuse! :refusal.widget-sets/ein-release-darf-bytes-gross (select-keys (ex-data e) [:reason :entry]) {:value cw/max-release-bytes})
                     (throw e))))
        common (let [tops (distinct (map #(first (str/split % #"/" 2)) (keys files)))]
                 (when (and (= 1 (count tops)) (every? #(str/includes? % "/") (keys files)))
                   (first tops)))]
    (if common
      (update-keys files #(subs % (inc (count common))))
      files)))

(defn publish
  [req]
  (let [{:keys [store auth nm body]} (ctx req)
        files (if-let [z (:zip-b64 body)] (unzipped z) (:files body))]
    (ok {:release (records/publish! store auth nm
                                    (assoc body :files files :username (username req)))})))

(defn drop-dev
  [req]
  (let [{:keys [store auth nm]} (ctx req)]
    (records/drop-dev! store auth nm (username req))
    (ok)))

(defn get-release
  [req]
  (let [{:keys [store auth nm]} (ctx req)
        version (get-in req [:path-params :version])]
    (if-let [r (records/release store auth nm version)]
      (ok {:release (select-keys r [:version :manifest :files :notes :author :created-at])})
      (refusal/refused :refusal.widget-sets/keine-version {:name nm :version version} {:version version}))))

(defn playground-bundle
  [req]
  (let [{:keys [store auth nm]} (ctx req)
        _ (records/details store auth nm)]
    (if-let [b (records/bundle store nm (get-in req [:query-params "version"]) (username req))]
      (ok {:bundle-edn (pr-str b)})
      (refusal/refused :refusal.widget-sets/dieses-widget-set-hat-noch {:name nm}))))

(defn- sets-in-layout
  [app]
  (into #{}
        (comp (filter #(= :custom-widget (some-> (:widget %) keyword)))
              (keep #(some-> (:widget-set %) name)))
        (tree/nodes (:layout app))))

(defn my-application-widget-set
  [req]
  (with-app req {:preview? true}
    (fn [{:keys [a store]}]
      (let [nm (get-in req [:path-params :name])]
        (if-not (contains? (sets-in-layout a) nm)
          (negotiated req 404 {:error (str "diese App zeigt kein Widget Set " nm)})
          (if-let [b (records/bundle store nm (get-in req [:query-params "version"])
                                     (username req))]
            (negotiated req 200 {:bundle-edn (pr-str b)})
            (negotiated req 404 {:error (str "keine passende Version von " nm)})))))))

(def module
  {:name        :widget-sets
   :contributes {:kompass/movers {:widget-set records/move!}
                 :http/routes
                 {:widget-sets               {:get list-sets :post create-set}
                  :widget-set                {:get get-set :delete delete-set}
                  :widget-set-releases       {:post publish}
                  :widget-set-dev            {:delete drop-dev}
                  :widget-set-bundle         {:get playground-bundle}
                  :widget-set-release        {:get get-release}
                  :my-application-widget-set {:get my-application-widget-set}}}})

