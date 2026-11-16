(ns ^:unit platform.foreign-connector-sidecar-test
  (:require [sutra.foundation.egress :as egress-agents]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [org.httpkit.server :as http-kit]
            [sutra.sources.protocols :as api]
            [sutra.sources.conformance :as conformance]
            [sutra.sources.datasources :as ds]
            [sutra.sources.egress :as egress]
            [sutra.sources.policy :as policy]
            [sutra.types.source :as source]))

(def ^:private seen (atom []))

(defn- lower [row] (update-keys row (comp keyword str/lower-case)))

(defn- rows [t] (mapv lower (get-in conformance/fixture [:tables t :rows])))

(defn- answer
  [{:keys [op table plan] :as body}]
  (swap! seen conj body)
  (case op
    "tables"       {:result (sort (keys (:tables conformance/fixture)))}
    "columns"      {:result [{:name "ID" :type "INTEGER" :ontology-type "integer"}]}
    "scan-profile" {:result {:preds {"=" "inexact"} :project? false :page? false :compound? true}}
    "scan"         {:result (map #(update-keys % (comp str/upper-case name)) (rows table))}
    "token"        {:result ["v" 7]}
    "write"        {:result {:update-count 1 :set (:set plan)}}
    "probe"        {:result nil}
    {:error {:code "unknown-op" :message op}}))

(defn- handler [{:keys [uri body headers]}]
  (let [b (json/read-str (slurp body) :key-fn keyword)
        r (cond
            (not= "Bearer s3cret"
                  (get headers "authorization")) {:error {:code "unauthorized" :message ""}}
            (= uri
               "/describe")
              {:types {:mailbox {:profile {:capabilities ["catalog" "scan" "write" "token"]
                                               :route ["direct"]
                                               :needs ["host"]
                                               :scan {:preds {"=" "exact"} :compound? false}}
                                     :form {:label "Mailbox"}}}}
            (= uri "/op") (answer b))]
    {:status 200 :headers {"Content-Type" "application/json"} :body (json/write-str r)}))

(use-fixtures :each
  (fn [f]
    (reset! seen [])
    (let [stop  (http-kit/run-server #'handler
                                     {:port 0 :ip "127.0.0.1" :legacy-return-value? false})
          url   (str "http://127.0.0.1:" (http-kit/server-port stop))
          own   @#'egress/running*
          agent (fn [path payload]
                  (if (= "/status" path)
                    {:connectors (#'egress/connectors-here)}
                    ((get @#'egress/routes path) payload)))]
      (swap! own assoc :agent egress-agents/default-agent)
      (reset! @#'egress/described-at 0)
      (try
        (with-redefs-fn {#'egress/sidecars      (delay {:mailbox url})
                         #'egress/sidecar-token (delay "s3cret")
                         #'egress-agents/post              agent
                         #'egress-agents/post-to
                           (fn [_ path payload] (agent path payload))}
          f)
        (finally
          (http-kit/server-stop! stop)
          (reset! ds/announced {}))))))

(deftest the-agent-says-what-its-sidecar-runs-and-the-server-knows-the-type
  (ds/refresh-announced!)
  (testing "the type, as the sidecar described it, behind the agent whatever it said"
    (is (= #{:catalog :scan :write :token} (:capabilities (source/profile :mailbox))))
    (is (= #{:agent} (:route (source/profile :mailbox))))
    (is (= :connector (policy/read-mode {:type :mailbox})))
    (is (= :agent (policy/route {:type :mailbox}))))
  (testing "its needs judge a spec of it"
    (is (= [:host] (source/needs {:type :mailbox})))))

(deftest a-sidecar-answers-the-port-through-the-agent
  (ds/refresh-announced!)
  (let [spec {:type :mailbox :host "h" :source-name "box"
              :access :read-write :writeback {"ITEMS" {:pk "ID" :writable-columns ["NAME"]}}}]
    (api/with-source [s #(ds/connect nil %) spec]
      (is (= ["ITEMS" "ITEM_LOG"] (api/tables s)))
      (testing "rows come back keyed by lower-case keywords, the residual computed by the host"
        (is (= [{:id 3 :name "gamma" :status 2}]
               (api/scan s "ITEMS" {:where [[:= :status 2] [:> :id 1]]})))
        (is (= [["=" "status" 2]] (:where (:scan (last (filter #(= "scan" (:op %)) @seen)))))
            "the sidecar got only what its profile takes, as JSON"))
      (is (= ["v" 7] (api/token s "ITEMS")))
      (testing "a write reaches the sidecar as the plan the agent made against the allowlist"
        (is (= 1
               (:update-count (api/write! s
                                          {:table "ITEMS" :pk-value 1 :set {"NAME" "a2"}
                                           :expect {"NAME" "alpha"}}))))
        (is (thrown? clojure.lang.ExceptionInfo
                     (api/write! s
                                 {:table "ITEMS" :pk-value 1 :set {"STATUS" 9}
                                  :expect {"STATUS" 1}})))
        (is (= 1 (count (filter #(= "write" (:op %)) @seen))))))
    (testing "the sidecar is given the resolved spec"
      (is (= "h" (:host (:spec (first @seen))))))))

(deftest a-sidecar-error-is-a-refusal
  (ds/refresh-announced!)
  (api/with-source [s #(ds/connect nil %) {:type :mailbox :host "h"}]
    (is (= :refusal.foreign/connector-failed
           (:refusal/code (try (api/invoke! s :nothing {}) nil
                               (catch clojure.lang.ExceptionInfo e (ex-data e))))))))
