(ns conformance.kits.http-api
  (:require [clojure.string :as str]
            [conformance.http :as http]
            [conformance.json :as json]
            [conformance.report :as r]))

(defn- major [v] (first (str/split (str v) #"\.")))

(defn- operations
  [doc]
  (sort-by (juxt first second)
           (for [[p ms] (get doc "paths")
                 [m op] ms
                 :when (map? op)]
             [p m op])))

(defn- parameterless-gets
  [doc pred]
  (for [[p m op] (operations doc)
        :when (and (= "get" m) (not (str/includes? p "{"))
                   (not-any? #(get % "required") (get op "parameters"))
                   (pred op))]
    p))

(defn- public? [op] (= [] (get op "security")))

(defn missing-operations
  [published served]
  (let [have (set (map (fn [[p m]] [p m]) (operations served)))]
    (vec (remove have (map (fn [[p m]] [p m]) (operations published))))))

(defn- show [xs] (str (count xs) ": " (str/join ", " (map pr-str (take 10 xs)))
                      (when (< 10 (count xs)) " …")))

(defn checks
  [{:keys [base published sample] :or {sample 20}}]
  (let [base     (str/replace base #"/+$" "")
        url      #(str base %)
        ask      {"Accept" "application/json"
                  "Sutra-API-Version" (str (get-in published ["info" "version"]))}
        served   (delay (http/get-url (url "/api/openapi.json") ask))
        doc      (delay (json/parse-or-nil (:body @served)))
        pubs     (delay (vec (remove #{"/api/openapi.json"}
                                     (parameterless-gets published public?))))
        guarded  (delay (vec (take sample (parameterless-gets published (complement public?)))))
        probe    (delay (into [["/api/openapi.json" @served]]
                              (for [p (concat @pubs @guarded)]
                                [p (http/get-url (url p) ask)])))
        want     (major (get-in published ["info" "version"]))]
    [(r/check "openapi-served" "GET /api/openapi.json answers an OpenAPI 3.1 document"
              #(cond (not= 200 (:status @served)) (str "status " (:status @served))
                     (not (map? @doc)) "the body is not a JSON object"
                     (not (str/starts-with? (str (get @doc "openapi")) "3.1"))
                     (str "openapi is " (pr-str (get @doc "openapi")))))
     (r/check "version-major" "the served API has the published major version"
              #(let [v (get-in @doc ["info" "version"])]
                 (when (not= want (major v))
                   (str "served " (pr-str v) ", published major " want))))
     (r/check "operations-present" "every published operation is served"
              #(let [m (missing-operations published @doc)]
                 (when (seq m) (str "missing " (show m)))))
     (r/check "version-header" "every /api answer carries Sutra-API-Version of that major"
              #(let [bad (for [[p resp] @probe
                               :let [v (get-in resp [:headers "sutra-api-version"])]
                               :when (not= want (major v))]
                           [p v])]
                 (when (seq bad) (str "without the header or of another major " (show bad)))))
     (r/check "json-negotiation"
              "a public GET asked for JSON, naming the API version, answers JSON"
              #(let [ok (for [[p resp] @probe
                              :when (and (some #{p} @pubs) (<= 200 (:status resp) 299))]
                          [p resp])
                     bad (for [[p resp] ok
                               :when (not (and (http/json-content? resp)
                                               (some? (json/parse-or-nil (:body resp)))))]
                           p)]
                 (cond (empty? ok)  (r/skip "no public parameterless GET answered 2xx")
                       (seq bad)    (str "not JSON " (show bad)))))
     (r/check "anonymous-refused" "a protected operation refuses an anonymous call (401/403)"
              #(let [bad (for [[p resp] @probe
                               :when (and (some #{p} @guarded) (not (#{401 403} (:status resp))))]
                           [p (:status resp)])]
                 (cond (empty? @guarded) (r/skip "no protected parameterless GET published")
                       (seq bad)         (str "answered otherwise " (show bad)))))
     (r/check "no-server-error" "no probe answered 5xx"
              #(let [bad (for [[p resp] @probe :when (<= 500 (:status resp))]
                           [p (:status resp)])]
                 (when (seq bad) (show bad))))]))
