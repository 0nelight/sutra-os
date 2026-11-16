(ns conformance.kits.platform-sdk
  (:require [clojure.string :as str]
            [conformance.http :as http]
            [conformance.json :as json]
            [conformance.report :as r]))

(defn sdk-endpoints
  [text]
  (into (sorted-map)
        (for [[_ id body] (re-seq #"(?m)^\s*\"([^\"]+)\":\s*(\{.*\}),?\s*$" (str text))
              :let [path    (second (re-find #"\"path\":\s*\"([^\"]+)\"" body))
                    methods (some->> (re-find #"\"methods\":\s*\[([^\]]*)\]" body) second
                                     (re-seq #"\"([a-z]+)\"") (map second) set)
                    auth    (second (re-find #"\"auth\":\s*\"([^\"]+)\"" body))]
              :when path]
          [id {:path path :methods (or methods #{}) :auth auth}])))

(defn- shape
  [p]
  (str/replace p #"\{[^}]*\}" "{}"))

(defn- served-ops
  [doc]
  (into {} (for [[p ms] (get doc "paths")] [(shape p) ms])))

(defn missing
  [endpoints doc]
  (let [served (served-ops doc)]
    (vec (for [[id {:keys [path methods]}] endpoints
               m (sort methods)
               :when (not (map? (get-in served [(shape path) m])))]
           [id path m]))))

(defn auth-mismatches
  [endpoints doc]
  (let [served (served-ops doc)]
    (vec (for [[id {:keys [path methods auth]}] endpoints
               m (sort methods)
               :let [op (get-in served [(shape path) m])]
               :when (map? op)
               :let [public? (= [] (get op "security"))]
               :when (not= (= "public" auth) public?)]
           [id path m]))))

(defn- show [xs]
  (str (count xs) ": " (str/join ", " (map pr-str (take 8 xs))) (when (< 8 (count xs)) " …")))

(defn checks
  [{:keys [base openapi sdk-text]}]
  (let [eps (sdk-endpoints sdk-text)
        doc (delay (or openapi
                       (json/parse-or-nil
                        (:body (http/get-url (str (str/replace base #"/+$" "")
                                                  "/api/openapi.json")
                                             {"Accept" "application/json"})))))]
    [(r/check "sdk.table" "the SDK declares an endpoint table"
              #(when (empty? eps) "no ENDPOINTS entries found in the SDK paths file"))
     (r/check "sdk.openapi" "the instance serves its OpenAPI document"
              #(when-not (get @doc "paths") "no OpenAPI document at /api/openapi.json"))
     (r/check "sdk.paths" "every path and method the SDK calls is served"
              #(when (get @doc "paths")
                 (let [m (missing eps @doc)] (when (seq m) (show m)))))
     (r/check "sdk.auth" "the SDK's public endpoints are public on the instance, and only those"
              #(when (get @doc "paths")
                 (let [m (auth-mismatches eps @doc)] (when (seq m) (show m)))))]))
