(ns ^:api platform.llm-audit-test
  (:require [sutra.ref.install :as ri]
            [sutra.access.http.host :as host]
            [sutra.store.access.scope :as ctx]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [clojure.data.json :as json]
            [sutra.wire.api :as api]
            [support.pod :as pod]
            [sutra.store.seat :as seat]
            [sutra.store.storage.instance-config :as ic]
            [sutra.catalog.llm-models :as llm-models]
            [sutra.types.llm-cost :as llm-cost]
            [sutra.foundation.egress :as egress]
            [sutra.compute.external]
            [sutra.serve.server :as server]
            [sutra.llm.http :as llm]
            [sutra.catalog.values :as cvalues]
            [sutra.compute.runner :as runner]))

(defn- with-grok-4
  [f]
  (api/mutate :create-llm-model {:name "grok-4" :in 3.0 :out 15.0 :currency "USD"} {:stage? true})
  (api/post! :staging)
  (f))

(use-fixtures :once pod/with-mem-pod with-grok-4)

(def COLS
  [{:attribute-name :ts                :attribute-type :string}
   {:attribute-name :request_id        :attribute-type :string}
   {:attribute-name :caller            :attribute-type :string}
   {:attribute-name :run_id            :attribute-type :string}
   {:attribute-name :source            :attribute-type :string}
   {:attribute-name :model             :attribute-type :string}
   {:attribute-name :input_json        :attribute-type :text}
   {:attribute-name :output_json       :attribute-type :text}
   {:attribute-name :prompt_tokens     :attribute-type :integer}
   {:attribute-name :completion_tokens :attribute-type :integer}
   {:attribute-name :latency_ms        :attribute-type :integer}
   {:attribute-name :error             :attribute-type :text}
   {:attribute-name :input_sha         :attribute-type :string}])

(defn- ds-rid []
  (->> (:datasets (:schema (:body (api/get! :snapshot))))
       (some (fn [[rid d]] (when (= :llm_call_log (:dataset-name d)) rid)))))

(defn- rows [] (-> (api/get! :dataset-preview {:id (ds-rid)}) :body :rows))

(defn- rate-rid []
  (->> (:datasets (:schema (:body (api/get! :snapshot))))
       (some (fn [[rid d]] (when (= :llm_model_rates (:dataset-name d)) rid)))))

(defn- rate-rows []
  (when-let [rid (rate-rid)] (mapv :values (-> (api/get! :dataset-preview {:id rid}) :body :rows))))

(defn- read-shape
  [row]
  (update-keys row #(keyword (str/replace (name %) "_" "-"))))

(defn- json-req [m]
  {:body (java.io.ByteArrayInputStream. (.getBytes ^String (json/write-str m) "UTF-8"))
   :headers {"authorization" (str "Bearer " (ic/relay-token (ctx/store)))}})

(defn- call-chat [m]
  (llm/llm-chat (assoc (json-req m)
                       :sutra/branch "main"
                       :sutra/host host/capabilities
                       :sutra/store (ctx/store)
                       :sutra/schema (seat/branch-schema (ctx/system-store "main")))))

(deftest llm-call-audit
  (testing "llm_call_log is a SYSTEM dataset — present from bootstrap, no .ent"
    (is (some? (ds-rid)) "system dataset is in the schema on a fresh ontology")
    (is (= (mapv :attribute-name COLS)
           (mapv :attribute-name (:columns (get-in (:body (api/get! :snapshot))
                                                   [:schema :datasets (ds-rid)]))))
        "carries the audit-ledger columns"))

  (testing "a successful call returns the provider response AND logs a row"
    (with-redefs [egress/chat! (fn [_spec _payload _opts]
                             {:status 200 :latency-ms 55
                              :body (json/write-str
                                     {:choices [{:message {:content "hallo"}}]
                                      :usage {:prompt_tokens 10000 :completion_tokens 10000}})})
                  cvalues/sources->datasources (constantly {:xai {:external-transform true}})]
      (let [resp (call-chat {:source "xai" :caller "parse_invoices"
                             :request {:model "grok-4"
                                       :messages [{:role "user" :content "was kostet das?"}]}})
            body (json/read-str (:body resp) :key-fn keyword)]
        (is (= 200 (:status resp)))
        (is (= 10000 (get-in body [:usage :prompt_tokens])) "provider response echoed verbatim")))
    (let [rs (mapv :values (rows))]
      (is (= 1 (count rs)) "exactly one row logged")
      (let [r (first rs)]
        (is (= "parse_invoices" (:caller r)))
        (is (= "grok-4" (:model r)))
        (is (= 10000 (:prompt_tokens r)))
        (is (= 10000 (:completion_tokens r)))
        (is (nil? (:compute_seconds r)) "no stored compute-seconds — that metric is gone")
        (is (nil? (:cost r)) "cost is NOT frozen on the row; it is a derived view")
        (is (= 55 (:latency_ms r)))
        (is (= 64 (count (:input_sha r))) "sha256 hex of input_json")
        (is (nil? (:error r)))
        (is (re-find #"was kostet das" (:input_json r)) "the exact prompt is recoverable"))))

  (testing "cost is a VIEW — tokens × the rate in force at the call's ts"
    (let [call  (read-shape (first (mapv :values (rows))))
          priced (llm-cost/price-call (mapv read-shape (rate-rows)) call)]
      (is (== 0.18 (double (:cost priced))) "derived cost = (10k·3 + 10k·15)/1e6 USD")
      (is (= "USD" (:currency priced)))))

  (testing "a FAILED call still logs an error row (and 502s)"
    (with-redefs [egress/chat! (fn [_ _ _]
                             (throw (ex-info "grok-4 rejected application/pdf" {:latency-ms 12})))
                  cvalues/sources->datasources (constantly {:xai {:external-transform true}})]
      (let [resp (call-chat {:source "xai" :caller "parse_receipts"
                             :request {:model "grok-4" :messages []}})]
        (is (= 502 (:status resp)))))
    (let [rs (mapv :values (rows))]
      (is (= 2 (count rs)) "the failure appended a second row")
      (let [err (first (filter :error rs))]
        (is (some? err) "an error row exists")
        (is (re-find #"rejected application/pdf" (:error err)))
        (is (= "parse_receipts" (:caller err))))))

  (testing "an unknown source is refused AND logged (so the miss is auditable)"
    (with-redefs [cvalues/sources->datasources (constantly {})]
      (let [resp (call-chat {:source "nope" :caller "x" :request {:model "grok-4"}})]
        (is (= 400 (:status resp)))))
    (is (= 3 (count (rows))) "the refused call was still logged"))

  (testing "an UNREGISTERED model is refused (registered-models-only) AND logged"
    (with-redefs [cvalues/sources->datasources (constantly {:xai {:external-transform true}})]
      (let [resp (call-chat {:source "xai" :caller "z" :request {:model "grok-imaginary-9"}})]
        (is (= 400 (:status resp)))
        (is (re-find #"unregistered model" (:body resp)))))
    (is (= 4 (count (rows))) "the refused unregistered-model call was still logged")
    (is (some #(re-find #"unregistered model" (str (:error %))) (mapv :values (rows)))
        "the miss carries an auditable reason")))

(deftest llm-model-registry
  (testing "register-model commits a PRICED model into the :llm-models section"
    (is (= 200 (:status (api/mutate :create-llm-model
                                    {:name "grok-x" :in 5.0 :out 10.0} {:stage? true}))))
    (is (= 200 (:status (api/post! :staging))))
    (let [schema (:schema (:body (api/get! :snapshot)))]
      (is (= {:in 5.0 :out 10.0} (get-in schema [:llm-models "grok-x"]))
          "the rate is committed, string-keyed by model-id")
      (is (llm-models/registered? schema "grok-x")        "the newly-registered model is usable")
      (is (not (llm-models/registered? schema "nope-9"))
          "an unregistered model stays unusable — no code floor, the register is the only source")))
  (testing "a transform naming the just-registered model passes the whole-schema gate"
    (let [schema (:schema (:body (api/get! :snapshot)))
          tr {:transform-name :uses-x
              :id "ri.sutra.main.transform.aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa"
              :language :python :code "x"
              :inputs {:m {:kind :language-model :source :xai :model "grok-x"}}
              :output {:kind :object-type :object-type :ev :column-map {} :primary-key :id}}]
      (is (empty? ((ri/stated-gates :models-registered?)
                   (assoc-in schema [:transforms (:id tr)] tr)))
          "no unregistered-model reason once grok-x is in the registry"))))

(deftest llm-transform-refuses-without-lm-service
  (testing "run-transform! on a language-model transform FAILS LOUD when the Language Model Service is off"
    (with-redefs [ic/lm-service-url (constantly nil)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo #"Language Model Service is NOT configured"
           (runner/run-code! :python {:transform-name :fake-lm :language :python :code "x"
                                :inputs {:lm {:kind :language-model :source :xai :model "grok-4"}}}
                             nil))))))

(deftest llm-chat-over-http
  (with-redefs [egress/chat! (fn [_ _ _]
                           {:status 200 :latency-ms 3
                            :body (json/write-str
                                   {:choices [{:message {:content "hi"}}]
                                    :usage {:prompt_tokens 5 :completion_tokens 2}})})
                cvalues/sources->datasources (constantly {:xai {:external-transform true}})]
    (let [req  {:request-method :post
                :uri "/api/llm/chat"
                :headers {"content-type" "application/json"
                          "authorization" (str "Bearer " (ic/relay-token (ctx/store)))}
                :body (java.io.ByteArrayInputStream.
                       (.getBytes (json/write-str
                                   {:source "xai" :caller "http_probe"
                                    :request {:model "grok-4"
                                              :messages [{:role "user" :content "ping"}]}})
                                  "UTF-8"))}
          resp (server/app req)
          body (json/read-str (if (string? (:body resp)) (:body resp) (slurp (:body resp)))
                              :key-fn keyword)]
      (is (= 200 (:status resp))
          "reached the handler (not 401 public-path / 404 route / 406 muuntaja)")
      (is (= 5 (get-in body [:usage :prompt_tokens]))
          "JSON response passed through muuntaja untouched")
      (is (some #(= "http_probe" (:caller %)) (mapv :values (rows)))
          "logged over the HTTP path too"))))
