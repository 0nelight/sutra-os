(ns sutra.sources.cloudevents
  (:require [clojure.string :as str]))

(def spec-version "1.0")

(def types
  {"com.sutra.actions.webhook"        "an action called a source's webhook"
   "com.sutra.sources.webhook"        "a webhook called outside an action"
   "com.sutra.sources.writeback-create" "a writeback created a record through a webhook"})

(defn- seg [x] (str/replace (name x) #"[^A-Za-z0-9._~-]" "_"))

(defn attributes
  [{:keys [type source-name webhook-id id time subject]}]
  (cond-> {:specversion spec-version
           :id          (str id)
           :source      (str "/sources/" (seg (or source-name "unknown")))
           :type        (or type "com.sutra.sources.webhook")
           :time        (str time)
           :subject     (some-> (or subject webhook-id) name)}
    (nil? (or subject webhook-id)) (dissoc :subject)))

(defn headers
  [attrs]
  (when-not (and (seq (:id attrs)) (seq (:source attrs)) (types (:type attrs)))
    (throw (ex-info "a CloudEvent needs id, source and a known type" {:attributes attrs})))
  (into {} (for [[k v] attrs :when (some? v)] [(str "ce-" (name k)) (str v)])))
