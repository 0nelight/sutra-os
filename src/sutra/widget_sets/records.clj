(ns sutra.widget-sets.records
  (:require [sutra.kit.coll :as coll]
            [sutra.types.plan :as cp]
            [clojure.edn :as edn]
            [sutra.kit.refusal :as refusal]
            [clojure.string :as str]
            [sci.core :as sci]
            [sutra.capps.custom-widget :as cw]
            [sutra.catalog.project-scope :as scope]
            [sutra.store.control :as ctl]))

(set! *warn-on-reflection* true)

(def ^:private what "Dieses Widget Set")
(def ^:private dev-prefix "dev/")

(defn- dev-version [username] (str dev-prefix username))
(defn- dev? [version] (str/starts-with? (str version) dev-prefix))

(defn- readable? [auth row] (scope/may-unplaced? auth :read (:project row)))

(defn- row-of [store nm] (ctl/get store :widget-sets {:name (str nm)}))

(defn- release-rows
  [store nm]
  (ctl/list store :widget-set-releases {:set-name (str nm)}))

(defn- release-summary
  [{:keys [version manifest notes author created-at]}]
  {:version version :notes notes :author author :created-at created-at
   :widgets (update-vals (:widgets manifest) #(dissoc % :main :css))
   :csp (:csp manifest)})

(defn- summary
  [row releases]
  (let [published (remove (comp dev? :version) releases)]
    (assoc (select-keys row [:name :description :project :author :created-at :updated-at])
           :latest (cw/latest (map :version published))
           :versions (mapv :version published))))

(defn list-sets
  [store auth]
  (let [by-set (group-by :set-name (ctl/list store :widget-set-releases {}))]
    (into []
          (comp (filter #(readable? auth %))
                (map #(summary % (get by-set (:name %)))))
          (ctl/list store :widget-sets {}))))

(defn- set-row!
  [store auth nm op]
  (let [row (row-of store nm)]
    (when-not row
      (refusal/refuse! :refusal.widget-set/unknown {:name nm}))
    (scope/require! auth op (:project row) what)
    row))

(defn details
  [store auth nm]
  (let [row      (set-row! store auth nm :read)
        releases (release-rows store nm)]
    (assoc (summary row releases)
           :releases (->> releases
                          (remove (comp dev? :version))
                          (sort-by :created-at #(compare %2 %1))
                          (mapv release-summary)))))

(defn create!
  [store auth {:keys [name description project author]}]
  (when-not (cw/set-name? name)
    (refusal/refuse! :refusal.widget-set/name-shape {:name name}))
  (when (row-of store name)
    (refusal/refuse! :refusal.widget-set/exists {:name name}))
  (scope/require! auth :update project what)
  (ctl/commit! store [[:insert :widget-sets
                       [{:name name :description description :project project
                         :author author}]]])
  (details store auth name))

(defn delete!
  [store auth nm]
  (set-row! store auth nm :update)
  (ctl/commit! store [[:delete :widget-set-releases {:set-name (str nm)}]])
  (ctl/commit! store [[:delete :widget-sets {:name (str nm)}]]))

(defn move!
  [store auth nm project]
  (let [row (set-row! store auth nm :read)]
    (scope/require-move! auth (:project row) project what)
    (ctl/commit! store [[:update :widget-sets
                         {:name (str nm)}
                         {:project project}]])
    (row-of store nm)))

(defn- parse-problem
  [path source]
  (try
    (let [ctx (sci/init {})
          rdr (sci/source-reader source)]
      (loop []
        (when-not (= ::eof (sci/parse-next ctx rdr {:eof ::eof}))
          (recur))))
    nil
    (catch Exception e
      (str path ": " (ex-message e)))))

(defn- source? [path] (re-find #"\.clj[sc]$" (str path)))

(defn- parsed-manifest
  [files]
  (let [text (get files cw/manifest-file)]
    (when-not text
      (refusal/refuse! :refusal.widget-set/manifest-missing {:files (vec (keys files))}
                       {:manifest cw/manifest-file}))
    (try (edn/read-string text)
         (catch Exception e
           (refusal/refuse! :refusal.widget-set/manifest-unreadable {}
                            {:manifest cw/manifest-file :reason (ex-message e)} e)))))

(defn- require-releasable!
  [manifest files]
  (let [size     (reduce + 0 (map (comp count val) files))
        problems (concat
                  (when (> size cw/max-release-bytes)
                    [(str "Ein Release darf " cw/max-release-bytes " Zeichen groß sein, dieses hat "
                          size)])
                  (cw/manifest-problems manifest files)
                  (keep (fn [[p s]] (when (source? p) (parse-problem p s))) files))]
    (cp/check! (cp/of-findings (cp/problems :widget-set/not-releasable problems))
               {:code   :refusal.widget-set/not-releasable
                :params (fn [bs] {:problems-text (str/join "\n" (map :message bs))})})
    nil))

(defn publish!
  [store auth nm {:keys [version files notes dev? username]}]
  (set-row! store auth nm :update)
  (let [files    (update-keys files #(if (keyword? %) (subs (str %) 1) (str %)))
        manifest (parsed-manifest files)
        _        (require-releasable! manifest files)
        version  (if dev? (dev-version username) version)
        row      {:set-name (str nm) :version version :manifest manifest
                  :files files :notes notes :author username}]
    (when-not dev?
      (cond
        (not (cw/version? version))
        (refusal/refuse! :refusal.widget-set/version-shape {} {:version version})

        (ctl/get store :widget-set-releases {:set-name (str nm) :version version})
        (refusal/refuse! :refusal.widget-set/version-published {:version version})))
    (ctl/commit! store
                 [(if dev?
                    (fn [s at] (ctl/upserting s :widget-set-releases [row] at))
                    (fn [s _] (ctl/inserting s :widget-set-releases [row])))
                  [:touch :widget-sets {:name (str nm)}]])
    (release-summary row)))

(defn drop-dev!
  [store auth nm username]
  (set-row! store auth nm :read)
  (ctl/commit! store [[:delete :widget-set-releases
                       {:set-name (str nm) :version (dev-version username)}]]))

(defn release
  [store auth nm version]
  (set-row! store auth nm :read)
  (ctl/get store :widget-set-releases {:set-name (str nm) :version (str version)}))

(defn bundle
  [store nm pin username]
  (let [rows     (release-rows store nm)
        by       (coll/index-by :version rows)
        draft    (get by (dev-version username))
        version  (cw/resolve-version (remove dev? (keys by)) pin)
        row      (or draft (get by version))]
    (when row
      {:set-name nm
       :version  (if draft "dev" version)
       :dev?     (some? draft)
       :stamp    (str (:updated-at row) (:created-at row))
       :manifest (:manifest row)
       :files    (:files row)})))
