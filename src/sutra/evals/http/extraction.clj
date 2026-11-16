(ns sutra.evals.http.extraction
  (:require [sutra.http.endpoint :as ep]
            [sutra.kit.refusal :as refusal]
            [clojure.string :as str]
            [sutra.catalog.grants :as grants]
            [sutra.evals.extraction :as extraction]
            [sutra.http.responses :refer [ok fault]]
            [sutra.kit.anomaly :as anomaly]
            [sutra.media.api :as media]
            [sutra.repos.api :as repos]
            [sutra.store.access.scope :as ctx]
            [sutra.store.control :as ctl]))

(set! *warn-on-reflection* true)

(defn- admin? [req] (= (ep/signed-in-user req) grants/admin-username))

(defn- source-of [m]
  (let [{:keys [kind name]} (or (:source m) m)]
    (when-not (str/blank? (str name)) {:kind (str kind) :name (str name)})))

(defn- entry
  [req store schema {:keys [kind name]} address]
  (case kind
    "media-set" (media/set-item-entry store schema name address)
    "dataset"   (when (admin? req) (media/media-item-entry store schema {:dataset name} address))
    nil))

(def ^:private transform-name #"[a-z][a-z0-9-]*")

(defn- failure
  [^Throwable e]
  (let [err (some-> (ex-data e) :stderr str str/split-lines)]
    (or (some->> err (remove str/blank?) last str/trim not-empty)
        (ex-message e) (str (class e)))))

(defn- run [f]
  (try (f)
       (catch InterruptedException e (throw e))
       (catch Exception e
         (if (anomaly/of e)
           (fault e)
           (refusal/refused :refusal.evals/passed-failure-e {} {:message (failure e)})))))

(defn sources
  [{store :sutra/store schema :sutra/schema branch :sutra/branch :as req}]
  (ok {:sources (vec (media/media-sources (ctx/at store branch) schema (admin? req)))
       :models  (extraction/models schema)}))

(defn items
  [{:keys [query-params] store :sutra/store schema :sutra/schema branch :sutra/branch :as req}]
  (let [src (source-of {:kind (get query-params "kind") :name (get query-params "name")})]
    (cond
      (nil? src) (refusal/refused :refusal.evals/name-a-media-source-kind {})
      (not (some #{src} (media/media-sources (ctx/at store branch) schema (admin? req))))
      (refusal/refused :refusal.evals/no-media-source src {:name (:name src)})
      :else (ok {:items (media/source-items (ctx/at store branch) schema src)}))))

(defn- with-entry
  [{:keys [body-params] store :sutra/store schema :sutra/schema branch :sutra/branch :as req} f]
  (let [store (ctx/at store branch)
        src   (source-of body-params)
        addr  (str (:address body-params))
        page  (:page body-params)]
    (cond
      (nil? src)                 (refusal/refused :refusal.evals/name-the-media-source-source {})
      (not (nat-int? page))      (refusal/refused :refusal.evals/name-the-page-page-from {})
      :else
      (run #(if-let [e (entry req store schema src addr)]
              (ok (f store schema e page body-params))
              (refusal/refused :refusal.evals/no-media-item-in src {:addr addr :name (:name src)}))))))

(defn extract
  [req]
  (let [{:keys [strategy model entities]} (:strategy (:body-params req))]
    (cond
      (not (extraction/strategies (str strategy)))
      (refusal/refused :refusal.evals/strategy-one-of {} {:list (str/join ", " (sort extraction/strategies))})
      (and (extraction/needs-model? strategy) (str/blank? (str model)))
      (refusal/refused :refusal.evals/the-strategy-needs-a-model {} {:strategy strategy})
      (and (= "entities" strategy) (empty? entities))
      (refusal/refused :refusal.evals/the-entities-strategy-needs-its {})
      :else
      (with-entry req (fn [store schema e page {:keys [strategy]}]
                        (extraction/extract! store schema e page strategy))))))

(defn evaluate
  [req]
  (let [{:keys [model markdown]} (:body-params req)]
    (if (or (str/blank? (str model)) (str/blank? (str markdown)))
      (refusal/refused :refusal.evals/an-evaluation-needs-the-judge {})
      (with-entry req (fn [store schema e page _]
                        (extraction/evaluate! store schema e page markdown model))))))

(defn- deploy-refusal
  [schema {:keys [strategy model entities]} {:keys [name repository]} known-source? branch]
  (cond
    (not (extraction/strategies (str strategy)))
    (str "strategy — one of " (str/join ", " (sort extraction/strategies)))
    (and (extraction/needs-model? strategy)
         (not (some #{(str model)} (extraction/models schema))))
    (str "model — one of " (str/join ", " (extraction/models schema)))
    (and (= "entities" strategy) (empty? entities))
    "entities — name → type: what the transform extracts"
    (not (and (string? name) (re-matches transform-name name)))
    "name — a transform's name is hyphen-cased"
    (str/blank? (str repository)) "repository — the code repository the transform goes to"
    (nil? (repos/repository-rid schema (str repository)))
    (str "repository — no repository " repository " on this branch: create it first")
    (not known-source?) "source — no media source of that name"
    (or (nil? branch) (= ctl/trunk (str branch)))
    "branch — a deployment commits to a branch, never to main"))

(defn- taken
  [repos rr branch files]
  (let [tree (some->> (repos/ref-sha repos rr (repos/branch->ref branch))
                      (repos/read-tree repos rr))]
    (seq (filter #(contains? tree %) (keys files)))))

(defn- commit-deployment!
  [store repos blobs rr branch {:keys [files readme name]}]
  (let [tree    (repos/read-tree repos rr (repos/ref-sha repos rr (repos/branch->ref branch)))
        old     (some-> (get tree "README.md") :blob (#(repos/blob-bytes repos rr %))
                        (String. "UTF-8"))]
    (repos/commit-files-to! store repos blobs rr branch
                            (assoc files "README.md" (str old readme))
                            (str "Document Intelligence: deploy " name))))

(defn deploy
  [{:keys [body-params] store :sutra/store schema :sutra/schema branch :sutra/branch :as req}]
  (let [src     (source-of body-params)
        known?  (or (extraction/layout-source? schema src (:strategy body-params))
                    (some #{src} (media/media-sources (ctx/at store branch) schema (admin? req))))
        refusal (deploy-refusal schema (:strategy body-params) body-params known? branch)]
    (if refusal
      (refusal/refused :refusal.evals/passed-refusal {} {:message refusal})
      (let [d     (extraction/deployment (assoc (select-keys body-params [:name :strategy])
                                                :source src))
            rr    (repos/repository-rid schema (str (:repository body-params)))
            b     (ctx/at store branch)]
        (repos/project-branch! b (ctx/repos) (ctx/blobs) rr branch)
        (if-let [t (taken (ctx/repos) rr branch (:files d))]
          (refusal/refused :refusal.evals/already-in-the-repository {} {:list (str/join ", " t)})
          (ok {:path       (:path d)
               :files      (:files d)
               :repository (str (:repository body-params))
               :branch     (str branch)
               :commit     (commit-deployment! b (ctx/repos) (ctx/blobs) rr branch
                                               (assoc d :name (:name body-params)))}))))))

(def module
  {:name        :document-intelligence
   :contributes {:http/routes {:doc-intel-sources  {:get sources}
                               :doc-intel-items    {:get items}
                               :doc-intel-extract  {:post extract}
                               :doc-intel-evaluate {:post evaluate}
                               :doc-intel-deploy   {:post deploy}}}})
