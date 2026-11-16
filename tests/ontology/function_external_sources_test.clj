(ns ontology.function-external-sources-test
  (:require [sutra.kit.anomaly :as anomaly]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [sutra.compute.external :as ext]
            [sutra.compute.sandbox :as sandbox]
            [sutra.compute.runner :as runner]
            [sutra.actions.effects :as action-effects]
            [sutra.authoring.install :as install]
            [sutra.authoring.install.plan :as plan]
            [support.replay :as replay]
            [support.system :as tsys]
            [sutra.types.rid :as rid])
  (:import [com.sun.net.httpserver HttpExchange HttpHandler HttpServer]
           [java.net InetSocketAddress]))

(def ^:private fn-rid (rid/mint-for :transforms (random-uuid)))
(def ^:private src-rid (rid/mint-for :sources (random-uuid)))

(defn- world
  ([code] (world code {}))
  ([code source]
   {:transforms {fn-rid {:id fn-rid :transform-name :suche :function-name :suche
                         :language :python :code code
                         :inputs {:ziel {:kind :scalar}}
                         :external-systems [:stub]}}
    :sources    {src-rid (merge {:id src-rid :source-name :stub :type :rest
                                 :external-transform true}
                                source)}}))

(defn- refusal [store w]
  (try (action-effects/run-query-function store w :suche {:ziel "VIE"}) nil
       (catch clojure.lang.ExceptionInfo e e)))

(deftest ^:unit a-query-function-reaches-only-a-released-cleared-exportable-source
  (with-redefs [runner/run-code! (fn [_ compute _]
                                   {:rows [{:sources (:external-systems compute)}]})]
    (testing "a released source is granted with its spec, for the egress agent"
      (let [[{:keys [sources]}]
              (action-effects/run-query-function nil (world "") :suche {:ziel "VIE"})]
        (is (= [:stub] (map :source-id sources)))
        (is (= :stub (:source-name (:spec (first sources)))))))

    (testing "an unknown source"
      (let [e (refusal nil (update (world "") :sources dissoc src-rid))]
        (is (= :stub (:source (ex-data e))))
        (is (anomaly/client-error? (ex-data e)) "the author's mistake — a 400")
        (is (re-find #"no source" (ex-message e)))))

    (testing "a source not released to code"
      (let [e (refusal nil (world "" {:external-transform false}))]
        (is (re-find #":external-transform true" (ex-message e)))))

    (testing "a marked source the caller is not cleared for"
      (let [e (refusal {:authority {:clearances #{}}}
                       (world "" {:markings [:pii] :export-markings [:pii]}))]
        (is (= :suche (:function (ex-data e))) "named, so the REST door answers 400, not 500")
        (is (anomaly/client-error? (ex-data e)))))

    (testing "a marked source whose markings may not leave"
      (let [e (refusal {:authority {:clearances #{:pii}}} (world "" {:markings [:pii]}))]
        (is (some? e) "a call sends its arguments out, so export permission is required")))

    (testing "cleared and exportable"
      (is (seq (action-effects/run-query-function {:authority {:clearances #{:pii}}}
                                                  (world ""
                                                         {:markings [:pii] :export-markings [:pii]})
                                                  :suche {:ziel "VIE"}))))))

(deftest ^:unit inside-an-action-a-function-still-may-not-egress
  (tsys/ensure!)
  (let [verb (get (tsys/verbs) :invoke-function)
        e    (try (replay/evaluate {}
                                   #(verb {:function :suche :inputs {:ziel "VIE"}} (world "")
                                          {} %1 %2))
               nil
                  (catch clojure.lang.ExceptionInfo ex ex))]
    (is (= [:stub] (:external-systems (ex-data e))))
    (is (re-find #":invoke-webhook" (ex-message e)))))

(deftest ^:unit the-install-names-an-unknown-or-unreleased-source
  (let [desired (fn [sources]
                  {:sources   sources
                   :functions {:suche {:language :clojure :code "(fn [_] [])"
                                       :inputs {:ziel {:kind :scalar}}
                                       :external-systems [:stub]}}})
        rules   (fn [d] (set (map :rule (plan/plan-problems d {} (install/plan-schema d {} {})))))]
    (is (contains? (rules (desired {})) :external-system-unknown))
    (is (contains? (rules (desired {:stub {:type :rest :config {:base-url "http://x"}
                                           :egress ["x:80"]}}))
                   :external-system-not-released))
    (is (not-any? #{:external-system-unknown :external-system-not-released}
                  (rules (desired {:stub {:type :rest :config {:base-url "http://x"}
                                          :egress ["x:80"] :external-transform true}}))))))

(defn- stub-server!
  []
  (doto (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)
    (.createContext "/flights"
                    (reify HttpHandler
                      (handle [_ ex]
                        (let [^HttpExchange ex ex
                              ok?  (= "geheim" (.getFirst (.getRequestHeaders ex) "X-Key"))
                              to
                                (second (re-find #"to=([^&]+)"
                                                 (str (.getRawQuery (.getRequestURI ex)))))
                              body (.getBytes (if ok?
                                                (json/write-str {:flights [{:nr "OS1" :to to}
                                                                           {:nr "OS2" :to to}]})
                                                "{\"error\":\"no key\"}")
                                              "UTF-8")]
                          (.add (.getResponseHeaders ex) "Content-Type" "application/json")
                          (.sendResponseHeaders ex (if ok? 200 401) (alength body))
                          (with-open [o (.getResponseBody ex)] (.write o body))))))
    (.start)))

(def ^:private search-code
  "from functions.api import function, String
from functions.sources import get_source

@function(sources=['stub'], api_name='suche')
def suche(ziel: String):
    src = get_source('stub')
    client = src.get_https_connection().get_client()
    r = client.get('/flights', params={'to': ziel}, headers={'X-Key': src.get_secret('api-key')})
    r.raise_for_status()
    return [{'nummer': f['nr'], 'ziel': f['to']} for f in r.json()['flights']]
")

(deftest ^:integration a-python-query-function-calls-its-source-in-the-sandbox
  (if-not (sandbox/available?)
    (println "  (function-external-sources SKIPPED — bwrap unavailable)")
    (let [server (stub-server!)
          port   (.getPort (.getAddress server))
          base   (str "http://127.0.0.1:" port)
          asked  (atom nil)
          fields (fn [egress] {"stub" {:base-url base :api-key "geheim" :egress egress}})
          run    (fn [code egress]
                   (with-redefs-fn {#'ext/fetch-secrets!
                                      (fn [refs] (reset! asked refs) (fields egress))}
                     #(action-effects/run-query-function nil (world code) :suche {:ziel "VIE"})))]
      (try
        (testing "the scalar arg, the url and the secret reach the code; the rows come back"
          (is (= [{:nummer "OS1" :ziel "VIE"} {:nummer "OS2" :ziel "VIE"}]
                 (run search-code [(str "127.0.0.1:" port)])))
          (is (= [:stub] (map :source-id @asked))
              "the egress agent is asked for exactly the declared source"))

        (testing "the client refuses a host outside the source's egress allowlist"
          (let [e (try (run search-code ["elsewhere.example:443"]) nil
                       (catch clojure.lang.ExceptionInfo ex ex))]
            (is (some? e))
            (is (str/includes? (ex-message e) "egress allowlist"))))

        (testing "an HTTP error surfaces as the function's failure"
          (let [e (try (run (str/replace search-code "src.get_secret('api-key')" "'falsch'")
                            [(str "127.0.0.1:" port)]) nil
                       (catch clojure.lang.ExceptionInfo ex ex))]
            (is (some? e))
            (is (str/includes? (ex-message e) "HTTP 401"))))
        (finally (.stop server 0))))))
