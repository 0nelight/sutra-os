(ns sutra.types.llm-providers
  (:require [sutra.kit.refusal :as refusal]
            [clojure.string :as str]
            #?(:clj [clojure.data.json :as json])))

(defn- ->json
  [body]
  (if (string? body)
    #?(:clj  (json/read-str body :key-fn keyword)
       :cljs (js->clj (js/JSON.parse body) :keywordize-keys true))
    body))

(defn- host-of
  [base-url]
  (or (some-> base-url str str/lower-case
              (str/replace #"^\w+://" "") (str/split #"/") first
              (str/split #":") first)
      ""))

(defn spec-base-url
  [spec]
  (or (:base-url spec) (get-in spec [:config :base-url])))

(defn adapter-for
  [adapters spec]
  (let [h (host-of (spec-base-url spec))]
    (some (fn [[_ a]] (when (str/includes? h (:host a)) a)) adapters)))

(defn parse-catalog
  [adapters spec body]
  (if-let [a (adapter-for adapters spec)]
    (vec ((:parse a) (->json body)))
    (refusal/refuse! :refusal.types/no-llm-catalog-adapter {:base-url (spec-base-url spec)}
                     {:source (:source-name spec)})))
