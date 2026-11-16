(ns sutra.http.endpoint
  (:require [clojure.string :as str]
            [sutra.catalog.grants :as grants]
            [sutra.kit.refusal :as refusal]
            [sutra.store.access.scope :as ctx]
            [sutra.store.seat :as seat]
            [sutra.store.world :as world]
            [sutra.types.ctx :as tctx]
            [sutra.catalog.lookup :as lookup]
            [sutra.foundation.encoding :as enc :refer [->uuid]]
            [sutra.types.paths :as paths]
            [sutra.http.responses :refer [ok]]))

(set! *warn-on-reflection* true)

(def truthy #{"1" "true" "yes" "on"})

(defn truthy? [v]
  (contains? truthy (some-> v str str/trim str/lower-case)))

(defn- csv [s]
  (into [] (comp (map str/trim) (remove str/blank?)) (str/split (str s) #",")))

(defn coerce-value [kind s]
  (let [[k & [a b d]] (if (vector? kind) kind [kind])]
    (case k
      :raw      s
      :str      (some-> s not-empty)
      :trim     (some-> s str str/trim not-empty)
      :kw       (some-> s not-empty keyword)
      :uuid     (->uuid s)
      :long     (or (enc/safe-long s) a)
      :double   (some-> s not-empty parse-double)
      :clamp    (some-> (or (enc/safe-long s) d) (max a) (min b))
      :bool     (truthy? s)
      :csv      (some-> s not-empty csv)
      :csv-kw   (some->> s not-empty csv (mapv #(keyword (str/replace % "_" "-"))))
      :repeated (cond (nil? s) [] (sequential? s) s :else [s])
      :json     (some-> s not-empty (enc/json-or-nil :key-fn keyword)))))

(defn coerce [decl m]
  (into {}
        (keep (fn [[k kind]]
                (let [v (coerce-value kind (get m (name k)))]
                  (when (some? v) [k v]))))
        decl))

(defn request-branch
  [req]
  (or (:sutra/branch req) (not-empty (get-in req [:query-params "branch"])) tctx/default-branch))

(defn request-schema
  [{store :sutra/store e :sutra/edge :as req}]
  (or (:sutra/schema req)
      (if e
        (:schema e)
        (let [b (request-branch req)]
          (when (world/exists? store b)
            (seat/branch-schema (ctx/at store b)))))))

(defn acting-username
  [id store]
  (if (= :public (get-in paths/endpoints [id :auth]))
    (grants/acting-username (grants/actor store))
    (grants/username-or-admin store)))

(defn signed-in-user
  [req]
  (get-in req [:principal :username]))

(defn acting-user
  [req]
  (or (signed-in-user req) (some-> (:sutra/store req) grants/username-or-admin)))

(defn args [id {:keys [path]} req]
  (let [pp    (:path-params req)
        store (:sutra/store req)]
    (-> (coerce (paths/query id) (:query-params req))
        (into (map (fn [[k v]] [k (if-let [kind (get path k)] (coerce-value kind v) v)])) pp)
        (assoc :body (:body-params req) :req req :store store :auth (:sutra/auth req)
               :user (acting-username id store)
               :host (:sutra/host req)
               :schema (request-schema req)
               :branch (:sutra/branch req) :sutra/project (:sutra/project req)
               :edge (:sutra/edge req)))))

(defn found
  [x code d]
  (if (some? x) x (refusal/refuse! code d)))

(defn- answer [{:keys [wrap or-404]} req a v]
  (cond
    (and (nil? v) or-404)
    (let [[code data] (if (vector? or-404) or-404 [or-404 nil])]
      (found nil code (if data (data a) (or (:path-params req) {}))))

    wrap  (ok {wrap v})
    :else (ok v)))

(defn- resolved
  [{:keys [needs-user with]} a]
  (when (and needs-user (nil? (:user a)))
    (refusal/refuse! :refusal.http/no-user {}))
  (reduce-kv (fn [m k [f code]]
               (assoc m k (found (f a) code (select-keys a [:id :name]))))
             a with))

(defn handler [id {:keys [call respond] :as spec}]
  (with-meta
    (fn [req]
      (let [a (resolved spec (args id spec req))]
        (cond
          respond (respond a)
          :else   (answer spec req a (call a)))))
    {:sutra/endpoint id}))

(defn entry
  [id section code]
  (handler id {:call (fn [{:keys [schema name]}]
                       (get-in schema [section (lookup/rid-of schema section (keyword name))]))
               :or-404 [code #(select-keys % [:name])]}))

(defn endpoint-of [h]
  (:sutra/endpoint (meta h)))

(defn branch-read
  [store f]
  (f store (seat/rebuild-schema-from-storage store)))
