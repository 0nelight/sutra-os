(ns grammar.http-refusals-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def ^:private sep java.io.File/separator)

(defn- handler-file? [^java.io.File f]
  (let [p (.getPath f)]
    (and (str/ends-with? p ".clj")
         (str/starts-with? p (str "src" sep "sutra" sep))
         (or (= "http.clj" (.getName f))
             (str/includes? p (str sep "http" sep))
             (str/includes? p (str sep "serve" sep))))))

(defn- src-files []
  (filter #(re-find #"\.cljc?$" (.getName ^java.io.File %)) (file-seq (io/file "src"))))

(defn- path [^java.io.File f] (str/replace (.getPath f) sep "/"))

(def ^:private wire-formats
  {"src/sutra/apps/http/custom.clj"
   [7 "OAuth 2.0 token endpoint: RFC 6749 §5.2 error bodies (invalid_scope, invalid_grant,
       unauthorized_client, unsupported_grant_type), returned from inside the issuing commit"]
   "src/sutra/http/origin.clj"
   [1 "the WebSocket origin check runs outside the router, before the error middleware"]
   "src/sutra/serve/server.clj"
   [2 "the router's default handler: no route matched, so no endpoint and no middleware"]
   "src/sutra/serve/platform.clj"
   [1 "the inter-process RPC answers in EDN to another process of the deployment"]
   "src/sutra/sources/http.clj"
   [1 "the egress hub's RPC answers its peer in plain text"]
   "src/sutra/llm/http.clj"
   [3 "the LLM proxy's admission results: internal values with :logged, answered in the
       OpenAI-compatible shape of the proxy"]
   "src/sutra/branching/http/git.clj"
   [1 "git smart HTTP: a plain-text 404 a git client prints"]})

(def ^:private categorised #"\{:status\s+(400|403|404|409|413|422)\b")

(deftest handlers-build-no-4xx-by-hand
  (let [counts (into (sorted-map)
                     (for [f (src-files)
                           :when (handler-file? f)
                           :let [n (count (re-seq categorised (slurp f)))]
                           :when (pos? n)]
                       [(path f) n]))]
    (is (= (update-vals wire-formats first) counts)
        "answer with (refusal/refused code data) or refuse!; a wire format of its own goes
         into wire-formats with why")))

(deftest the-response-helpers-are-gone
  (doseq [f (src-files)
          :let [t (slurp f)]]
    (is (not (re-find #"\((?:bad-request|branch-not-found)[\s)]" t)) (path f))
    (is (not (re-find #"responses[^\]]*:refer \[[^\]]*\b(?:bad-request|not-found)\b" t))
        (path f))))

(deftest handlers-raise-no-free-text-refusal
  (doseq [f (src-files)
          :when (handler-file? f)]
    (is (not (re-find #"\(anomaly/raise!\s+:(?:incorrect|unprocessable|not-found|forbidden|conflict|too-large)\b"
                      (slurp f)))
        (path f))))

(def ^:private reads-the-principal
  #{"src/sutra/http/endpoint.clj" "src/sutra/http/security_log.clj"
    "src/sutra/serve/errors.clj"})

(deftest the-acting-user-is-read-through-the-endpoint
  (doseq [f (src-files)
          :let [p (path f)]
          :when (not (reads-the-principal p))]
    (is (not (re-find #":principal\s+:username|\(:username\s+\(:principal" (slurp f)))
        (str p ": read (:user args), ep/signed-in-user or ep/acting-user"))))

(deftest every-handler-code-is-in-the-catalog
  (let [en  (edn/read-string (slurp "libs/core/resources/i18n/refusal.en.edn"))
        de  (edn/read-string (slurp "libs/core/resources/i18n/refusal.de.edn"))
        cat (edn/read-string (slurp "libs/core/resources/refusal/category.edn"))
        used (into (sorted-set)
                   (for [f (src-files)
                         :when (handler-file? f)
                         [_ c] (re-seq #":(refusal\.[a-z\-]+/[a-z0-9\-]+)" (slurp f))]
                     (keyword c)))]
    (is (seq used))
    (testing "an English and a German text and a category"
      (is (= [] (vec (remove #(and (en %) (de %) (cat %)) used)))))))
