(ns ^:unit platform.foreign-connector-plugin-test
  (:require [sutra.foundation.egress :as egress-agents]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [malli.core :as m]
            [sutra.sources.protocols :as api]
            [sutra.sources.conformance :as conformance]
            [sutra.sources.connector :as connector]
            [sutra.sources.datasources :as ds]
            [sutra.sources.egress :as egress]
            [sutra.sources.policy :as policy]
            [sutra.sources.writeback :as wb]
            [sutra.kit.system :as sys]
            [sutra.parser.spec :as spec]
            [sutra.types.source :as source]))

(defn- lower [row] (update-keys row (comp keyword str/lower-case)))

(def ^:private state (atom nil))

(defn- held
  [route]
  {:profile {:capabilities #{:catalog :scan :write :token} :route route
             :needs [:host]
             :scan {:preds {:= :exact} :project? false :page? false :compound? false}}
   :config  [:map [:host :string]]
   :form    {:label "Held" :fields [{:key :host :label "Host" :kind :string :required true}]}
   :open    (fn [spec ctx] (swap! state update :opened (fnil conj []) (:source-name ctx)) spec)
   :close   (fn [_] (swap! state update :closed (fnil inc 0)))
   :tables  (fn [_] (keys (:tables @state)))
   :columns (fn [_ t] (mapv (fn [{:keys [name]}] {:name name :type "TEXT" :ontology-type :string})
                            (get-in @state [:tables t :columns])))
   :scan    (fn [_ t pushed]
              (swap! state update :pushed (fnil conj []) pushed)
              (api/run-local (mapv lower (get-in @state [:tables t :rows])) pushed))
   :write   (fn [_ plan]
              (swap! state update :plans (fnil conj []) plan)
              (if (:stale @state) {:conflict true} {:update-count 1}))
   :token   (fn [_ t] [t (:version @state)])})

(def ^:private module
  {:name :held-connector
   :contributes {:foreign/connectors {:held (held #{:direct})
                                      :held-agent (held #{:agent})
                                      :held-endpoint (assoc-in (held #{:direct})
                                                               [:profile :endpoint] :host)}}})

(use-fixtures :each
  (fn [f]
    (let [current @#'sys/installed
          prev    @current]
      (reset! state (assoc (select-keys conformance/fixture [:tables]) :version 1))
      (sys/install! (sys/assemble [module]))
      (try (f) (finally (reset! current prev))))))

(def ^:private base
  (assoc conformance/writeback :type :held :host "h"
         :writeback {"ITEMS" {:pk "ID" :writable-columns ["NAME" "STATUS"]}}))

(deftest a-plugin-type-is-a-connector-type
  (testing "its profile, read mode and route come from its entry"
    (is (= #{:catalog :scan :write :token} (:capabilities (source/profile :held))))
    (is (= :connector (policy/read-mode {:type :held})))
    (is (= :direct (policy/route {:type :held})))
    (is (= :agent (policy/route {:type :held-agent})))
    (is (contains? (source/all-profiles) :held)))
  (testing "a spec of it is judged by its :needs and its :config"
    (is (m/validate source/SourceConn {:type :held :host "h"}))
    (is (m/validate source/SourceConn {:type :held :config {:host "h"}}))
    (is (= "healthy" (:status (policy/spec-health {:type :held :config {:host "h"}}))))
    (is (= ["h" 445] (policy/spec-endpoint {:type :held-endpoint
                                            :config {:host "h" :port 445}})))
    (is (not (m/validate source/SourceConn {:type :held})))
    (is (not (m/validate source/SourceConn {:type :held :host 7})))
    (is (not (m/validate source/SourceConn {:type :nothing-brings-this :host "h"}))))
  (testing "its form joins the platform's"
    (is (= "Held" (:label (get (spec/source-types (source/connectors)) :held))))
    (is (contains? (spec/source-types (source/connectors)) :rest))))

(deftest a-connector-is-a-foreign-source
  (let [connect (fn [spec] (ds/connect nil (assoc spec :source-name "held-1")))]
    (api/with-source [s connect base]
      (testing "catalog, rows keyed by lower-case keywords"
        (is (= ["ITEMS" "ITEM_LOG"] (api/tables s)))
        (is (= #{:id :name :status} (set (keys (first (api/scan s "ITEMS")))))))
      (testing "what the profile takes is pushed, the rest computed by the host"
        (is (= [{:id 3 :name "gamma" :status 2}]
               (api/scan s "ITEMS" {:where [[:= :status 2] [:> :id 1]]})))
        (is (= [[:= :status 2]] (:where (last (:pushed @state))))))
      (testing "the token stands for the table's rows"
        (is (= ["ITEMS" 1] (api/token s "ITEMS")))
        (swap! state update :version inc)
        (is (= ["ITEMS" 2] (api/token s "ITEMS"))))
      (testing "a write reaches the connector as the host's plan against the allowlist"
        (is (= {:update-count 1}
               (api/write! s
                           {:table "ITEMS" :pk-value 1 :set {"NAME" "a2"}
                            :expect {"NAME" "alpha"}})))
        (is (= {"NAME" "a2"} (:set (last (:plans @state)))))
        (is (thrown? clojure.lang.ExceptionInfo
                     (api/write! s {:table "ITEMS" :pk-value 1 :set {"ID" 9} :expect {"ID" 1}})))
        (is (= 1 (count (:plans @state))) "a column off the allowlist never reaches it")))
    (is (= ["held-1"] (:opened @state)))
    (is (= 1 (:closed @state)))))

(deftest a-table-is-a-path-only-for-a-connector-that-says-so
  (let [scan (fn [paths?]
               (api/scan (reify
                           api/Source
                           (-profile [_] {:paths? paths? :scan {:preds {}}})
                           api/Scan
                           (-scan-profile [_ _] {:preds {}})
                           (-scan [_ table _] [{:table table}]))
                         "a b/c-d"))]
    (is (= [{:table "a b/c-d"}] (scan true)))
    (is (thrown? Exception (scan false)))))

(deftest a-connector-keeps-no-change-log-in-its-source
  (api/with-source [s connector/open (assoc base :writeback (:writeback conformance/writeback))]
    (is (= :refusal.foreign/connector-log
           (:refusal/code (try (api/write! s
                                           {:table "ITEMS" :pk-value 1 :set {"NAME" "x"}
                                            :expect {"NAME" "alpha"} :actor "ann"})
                               nil
                               (catch clojure.lang.ExceptionInfo e (ex-data e))))))
    (is (empty? (:plans @state)))))

(deftest a-connector-passes-the-scan-contract
  (conformance/check-scan (fn [_ spec] (connector/open (assoc spec :type :held :host "h")))))

(deftest an-agent-routed-connector-runs-in-the-agent
  (let [own    @#'egress/running*
        before @own
        route  (get @#'egress/routes "/connector")]
    (swap! own assoc :agent egress-agents/default-agent)
    (try
      (with-redefs-fn {#'egress-agents/post (fn [path payload]
                                              (is (= "/connector" path))
                                              (route payload))}
        (fn []
          (api/with-source [s #(ds/connect nil %) (assoc base :type :held-agent)]
            (is (instance? sutra.sources.datasources.ConnectorAgentSource s))
            (is (= ["ITEMS" "ITEM_LOG"] (api/tables s)))
            (is (= #{"alpha" "beta"}
                   (set (map :name (api/scan s "ITEMS" {:where [[:= :status 1]]})))))
            (is (= ["ITEMS" 1] (api/token s "ITEMS"))))))
      (finally (reset! own before)))))

(deftest a-plugin-may-not-replace-a-connector-the-platform-has
  (is (= [] (source/collisions (source/connectors))))
  (is (= [:postgres] (source/collisions {:postgres {} :held {}})))
  (testing "but it may bring the connector of a type the platform only names"
    (is (= [] (source/collisions {:imap {}})))))

(deftest an-inconsistent-entry-is-named
  (is (= [{:problem :capability-without-fn :capability :token :fn :token}]
         (connector/problems (dissoc (held #{:direct}) :token))))
  (is (= [{:problem :fn-without-capability :capability :invoke :fn :invoke}]
         (connector/problems (assoc (held #{:direct}) :invoke (fn [& _]))))))

(deftest a-connector-answering-conflict-is-an-edit-conflict
  (swap! state assoc :stale true)
  (api/with-source [s connector/open base]
    (let [d (try (api/write! s
                             {:table "ITEMS" :pk-value 1 :set {"NAME" "a2"}
                              :expect {"NAME" "alpha"}}) nil
                 (catch clojure.lang.ExceptionInfo e (ex-data e)))]
      (is (= :refusal.foreign/edit-conflict (:refusal/code d)))
      (is (true? (:conflict d))))))

(deftest a-composite-key-reaches-the-connector-in-parts
  (api/with-source [s connector/open
                    (assoc base :writeback {"ITEMS" {:pk-columns ["ID" "NAME"]
                                                     :writable-columns ["STATUS"]}})]
    (api/write! s {:table "ITEMS" :pk-value (policy/row-key [:id :name] {:id 2 :name "beta"})
                   :set {"STATUS" 3} :expect {"STATUS" 1}})
    (is (= {"ID" "2" "NAME" "beta"} (:key (last (:plans @state)))))))

(deftest an-object-writeback-reads-the-live-row-through-a-connector
  (let [tgt {:source (assoc base :source-name "held-1") :connect ds/connect :authority nil
             :table "ITEMS" :pk-result "ID" :column-map {:id "ID" :name "NAME" :status "STATUS"}}]
    (is (= {:id 2 :name "beta" :status 1} (wb/read-live-row tgt 2))
        "no SQL: a scan by the key, pushed as far as the connector takes it")
    (is (= [[:= :id 2]] (:where (last (:pushed @state)))))
    (is (nil? (wb/read-live-row tgt 99)))))
