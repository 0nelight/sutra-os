(ns sutra.http.openapi
  (:require [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [malli.json-schema :as js]
            [sutra.foundation.contracts :as c]
            [sutra.foundation.boundaries :as bnd]
            [sutra.types.paths :as paths]))

(def openapi-file "boundaries/http-api/openapi.json")

(def operations-file "boundaries/operations/operations.schema.json")

(def media-types ["application/json" "application/transit+json"])

(defn- component-name
  [k]
  (str/replace (str (namespace k) "." (name k)) #"[^a-zA-Z0-9._-]" "_"))

(defn- schema-of
  [schema ref-path]
  (let [s     (if (qualified-keyword? schema) [:ref schema] schema)
        out   (js/transform s (assoc c/opts ::js/definitions-path "#/$defs/"))
        defs  (:definitions out)
        fix   (fn [r] (let [k (str/replace (subs r (count "#/$defs/")) "~1" "/")]
                        (str ref-path (component-name (keyword k)))))
        walkr (fn [x] (walk/postwalk
                       (fn [v] (if (and (map? v) (string? (:$ref v)))
                                 (update v :$ref fix)
                                 v))
                       x))]
    [(walkr (dissoc out :definitions))
     (into {} (for [[k v] defs] [(component-name (keyword k)) (walkr v)]))]))

(defn- query-schema
  [t]
  (let [[tag & args] (if (vector? t) t [t])]
    (case tag
      :long     (cond-> {:type "integer"} (seq args) (assoc :default (first args)))
      :clamp    (let [[lo hi d] args] {:type "integer" :minimum lo :maximum hi :default d})
      :double   {:type "number"}
      :bool     {:type "boolean"}
      :uuid     {:type "string" :format "uuid"}
      :json     {:type "string" :contentMediaType "application/json"}
      :csv      {:type "string" :description "comma-separated values"}
      :csv-kw   {:type "string" :description "comma-separated names"}
      :repeated {:type "array" :items {:type "string"}}
      {:type "string"})))

(defn- path-params [url] (map second (re-seq #":([\w\-]+)" url)))

(defn- openapi-path [url] (str/replace url #":([\w\-]+)" "{$1}"))

(def security-schemes
  {:bearer  {:type "http" :scheme "bearer"
             :description "An OAuth 2.0 access token or API token (B-2)."}
   :session {:type "apiKey" :in "cookie" :name "sutra-session"
             :description "The session of a browser login (B-2)."}})

(defn- security [auth]
  (case auth
    :public []
    [{:bearer []} {:session []}]))

(defn- body [schema-json]
  {:content (into {} (for [mt media-types] [mt {:schema schema-json}]))})

(defn- operation
  [id {:keys [url service auth reason query branch? encode] :as row} m mr]
  (let [req          (or (:request mr) (:request row))
        res          (or (:response mr) (:response row))
        [rq rq-defs] (when req (schema-of req "#/components/schemas/"))
        [rs rs-defs] (when res (schema-of res "#/components/schemas/"))
        params       (concat
                      (for [p (path-params url)]
                        {:name p :in "path" :required true :schema {:type "string"}})
                      (for [[q t] (sort-by key query)]
                        {:name (name q) :in "query" :required false :schema (query-schema t)})
                      (when branch?
                        [{:name "branch" :in "query" :required false
                          :schema {:type "string"}
                          :description "The branch to read or write; the trunk when absent."}]))
        op           (cond-> {:operationId (str (name id) "." (name m))
                              :summary     (str/replace (name id) "-" " ")
                              :tags        [(name service)]
                              :security    (security auth)
                              :x-sutra-effects (vec (sort (map name (:effects mr))))
                              :x-sutra-service (name service)
                              :responses
                              {"200" (cond-> {:description "The answer."}
                                       rs (merge (body rs)))
                               "default" {:description "A refusal or fault."}}}
                       (seq params)          (assoc :parameters (vec params))
                       (:requires mr)        (assoc :x-sutra-requires
                                                    (walk/postwalk #(if (keyword? %) (str %) %)
                                                                   (:requires mr)))
                       auth                  (assoc :x-sutra-auth (name auth))
                       reason                (assoc :description reason)
                       encode                (assoc :x-sutra-encode (str encode))
                       (and rq (not= :get m)) (assoc :requestBody (merge {:required true}
                                                                         (body rq))))]
    [op (merge rq-defs rs-defs)]))

(defn document
  ([] (document (bnd/register) paths/endpoints))
  ([reg endpoints]
   (let [ops (for [[id {:keys [url methods] :as row}] endpoints
                   [m mr] methods]
               [(openapi-path url) m (operation id row m mr)])]
     {:openapi "3.1.0"
      :info    {:title       "Sutra HTTP API"
                :version     (get-in reg [:boundaries :boundary/http-api :version])
                :description (str "Boundary B-1. Generated from "
                                  "sutra.types.paths; do not edit by hand.")
                :license     {:name (:spec-license reg)
                              :identifier (:spec-license reg)}}
      :paths   (reduce (fn [acc [p m [op]]] (assoc-in acc [p (name m)] op)) {} ops)
      :components {:securitySchemes security-schemes
                   :schemas (apply merge {} (map (comp second #(nth % 2)) ops))}})))

(defn operations
  ([] (operations (bnd/register)
                  (edn/read-string (slurp (io/resource "sutra/ops.edn")))))
  ([reg table]
   (let [rows (for [[op {:keys [service kind in out doc branch?]}] table
                    :let [[i i-defs] (schema-of in "#/$defs/")
                          [o o-defs] (schema-of out "#/$defs/")]]
                [(str (namespace op) "/" (name op))
                 {:kind (name kind) :service (name service) :branch (boolean branch?)
                  :doc (str/join " " (map str/trim (str/split-lines (str doc))))
                  :in i :out o}
                 (merge i-defs o-defs)])]
     {:$schema     "https://json-schema.org/draft/2020-12/schema"
      :$id         "https://sutra.dev/boundaries/operations/operations.schema.json"
      :title       "Sutra operations between modules"
      :description (str "Boundary B-14, version "
                        (get-in reg [:boundaries :boundary/operations :version])
                        ". Generated from resources/sutra/ops.edn; do not edit by hand.")
      :x-sutra-operations (into {} (map (juxt first second)) rows)
      :$defs (apply merge {} (map #(nth % 2) rows))})))

(defn- text [k] (if (keyword? k) (subs (str k) 1) (str k)))

(defn- stable
  [x]
  (walk/postwalk (fn [v]
                   (cond (map? v)     (into (sorted-map) (map (fn [[k x]] [(text k) x])) v)
                         (keyword? v) (text v)
                         :else        v))
                 x))

(defn render
  [doc]
  (with-out-str (json/pprint (stable doc) :escape-slash false)))

(defn -main [& _]
  (doseq [[f doc] [[openapi-file (document)] [operations-file (operations)]]]
    (io/make-parents f)
    (spit f (render doc))
    (println "wrote" f)))
