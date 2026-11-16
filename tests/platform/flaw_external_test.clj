(ns ^:integration
    ^{:clj-kondo/config
      '{:linters {:consistent-alias {:aliases {sutra.sources.writeback-dataset wbd}}}}}
    platform.flaw-external-test
  (:require [sutra.catalog.grants :as grants]
            [sutra.kit.anomaly :as anomaly]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.sources.writeback-dataset :as wbd]
            [sutra.sources.datasources :as ds]
            [sutra.foundation.egress :as eg]
            [sutra.sources.writeback :as wb]
            [sutra.store.control :as ctl]
            [sutra.store.access.run-log :as run-log]
            [sutra.actions.action :as action]
            [sutra.actions.effects :as action-effects]
            [sutra.store.core]
            [sutra.store.core.commit]
            [sutra.store.access.scope :as ctx]
            [sutra.access.principals :as pr]
            [support.aviation :refer [ontologie]]
            [support.in-project :refer [in-project]]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(def ^:private quelle
  {:type :mysql :jdbc-url "jdbc:mysql://127.0.0.1:9/mro?user=root" :egress ["127.0.0.1"]
   :access :read-write :project :werft
   :writeback {"MECH_ERP" {:object-type :mechaniker :pk "LIZENZ" :identity "LIZENZ"
                           :match-key "LIZENZ"
                           :writable-columns ["NAME" "MUSTER"] :insertable-columns ["NAME" "MUSTER"]
                           :column-map {"lizenz" "LIZENZ" "name" "NAME" "muster" "MUSTER"}
                           :log :none}}
   :webhooks {:umbenennen {:kind :table :table "MECH_ERP" :op :update}
              :anlegen    {:kind :table :table "MECH_ERP" :op :insert}}})

(def ^:private aktionen
  {:extern-umbenennen
   {:params {:lizenz {:name "Lizenz" :type :string :required true}
             :name   {:name "Name" :type :string :required true}}
    :rules  [{:op :invoke-webhook :phase :writeback :source :erp :webhook :umbenennen
              :params {:lizenz [:get :lizenz] :name [:get :name]}}
             {:op :create :object-type :mechaniker
              :values {:lizenz [:get :lizenz] :name [:get :name] :muster "A320"}}]
    :edits  [{:object-type :mechaniker}]}
   :extern-anlegen
   {:params {:lizenz {:name "Lizenz" :type :string :required true}}
    :rules  [{:op :invoke-webhook :phase :writeback :source :erp :webhook :anlegen
              :params {:lizenz [:get :lizenz] :name "Neu" :muster "A320"}}]}})

(defn- als-admin [f]
  (binding [tx/*auth-token* (-> (api/post! :login {:username grants/admin-username
                                                   :password pr/default-admin-password})
                                :body :token)]
    (f)))

(defn- mit-welt [f]
  (let [r (api/post! :install {:schema (in-project :werft (-> ontologie
                                                         (assoc :sources {:erp quelle})
                                                         (update :actions merge aktionen)))})]
    (assert (ok? r) (pr-str (:body r))))
  (assert (ok? (api/post! :staging)))
  (f))

(use-fixtures :once pod/with-mem-pod-auth als-admin mit-welt)

(defn- mit-schluessel
  [k f]
  (let [orig @#'tx/builder]
    (with-redefs-fn {#'tx/builder (fn [& args]
                                    (doto ^java.net.http.HttpRequest$Builder (apply orig args)
                                      (.header "Idempotency-Key" k)))}
      f)))

(defn- log-eintraege [lizenz]
  (count (filter #(= lizenz (get-in % [:payload :lizenz]))
                 (run-log/action-log (ctx/store "main")))))

(deftest double-submit
  (testing "a retried submission with the same Idempotency-Key is applied once"
    (let [anlegen #(api/mutate :mechaniker-anlegen {:lizenz "DS-1" :name "Dina" :muster "A320"}
                               {:publish? true})
          [a b]   (mit-schluessel "ds-1" #(vector (anlegen) (anlegen)))]
      (is (ok? a) (pr-str (:body a)))
      (is (ok? b) (pr-str (:body b)))
      (is (true? (:replayed (:body b))) "the retry got the stored answer")
      (is (= (str (:action-id (:body a))) (str (:action-id (:body b)))))
      (is (= 1 (log-eintraege "DS-1")))))
  (testing "two at once with the same key: one applies, the other gets its answer"
    (let [anlegen #(api/mutate :mechaniker-anlegen {:lizenz "DS-2" :name "Detlef" :muster "A320"}
                               {:publish? true})
          rs      (mit-schluessel "ds-2"
                                  #(mapv deref
                                         [(future (anlegen)) (future (anlegen))
                                          (future (anlegen))]))]
      (is (every? ok? rs) (pr-str (map :body rs)))
      (is (= 1 (log-eintraege "DS-2"))))))

(defn- pushes []
  (filter #(= "writeback-push" (:action-name %)) (run-log/action-log (ctx/store "main"))))

(defn- live-stub [tgt pk]
  (assoc (zipmap (keys (:column-map tgt)) (repeat "alt")) (:pk-attr tgt) pk))

(deftest phantom-log
  (with-redefs [wb/read-live-row live-stub
                wb/push!         (fn [& _] {:rows 1})]
    (testing "the local commit fails after the push: no log entry of the writeback"
      (let [vorher (count (pushes))
            orig   @#'sutra.store.core.commit/apply!
            r      (with-redefs-fn
                     {#'sutra.store.core.commit/apply! (fn [& args]
                                              (apply orig args)
                                              (throw
                                               (ex-info "injected after the commit's writes" {})))}
       #(api/mutate :extern-umbenennen {:lizenz "PL-1" :name "Neu"} {:publish? true}))]
        (is (not (ok? r)))
        (is (= vorher (count (pushes))) "the rolled-back writeback left no log entry")))
    (testing "the local commit holds: the entry is in it"
      (let [vorher (count (pushes))]
        (is (ok? (api/mutate :extern-umbenennen {:lizenz "PL-2" :name "Neu"} {:publish? true})))
        (is (= (inc vorher) (count (pushes))))))))

(deftest duplicate-foreign
  (let [pushed (atom 0)]
    (with-redefs [wb/find-by-match-key (constantly nil)
                  wb/push!             (fn [_ spec & _] (swap! pushed inc)
                                         {:identity (str "DF-" @pushed) :spec spec})]
      (testing "two creates with the same match value: one reaches the source"
        (let [rs (mapv deref
                       [(future (api/mutate :extern-anlegen {:lizenz "DF-1"} {:publish? true}))
                        (future (api/mutate :extern-anlegen {:lizenz "DF-1"} {:publish? true}))])]
          (is (= 1 (count (filter ok? rs))) (pr-str (map :body rs)))
          (is (= 1 @pushed))))
      (testing "a later one, its row still not found, does not create it again"
        (is (not (ok? (api/mutate :extern-anlegen {:lizenz "DF-1"} {:publish? true}))))
        (is (= 1 @pushed)))
      (testing "a claim whose creator failed lapses"
        (with-redefs [wbd/claim-ttl-ms -1]
          (is (ok? (api/mutate :extern-anlegen {:lizenz "DF-1"} {:publish? true}))))
        (is (= 2 @pushed))))))

(deftest partial-foreign-commit
  (let [posts (atom [])]
    (with-redefs-fn {#'eg/post (fn [path payload]
                                 (swap! posts conj [path (get-in payload [:spec :source-name])])
                                 (if (= "/write-begin" path) {:tx (str "tx-" (count @posts))}
                                   {:ok true}))}
      (fn []
        (let [session (ds/write-session)]
          (ds/write! {:source-name "a"} {:set {}} session)
          (ds/write! {:source-name "a"} {:set {}} session)
          (testing "a second source in one write session is refused before it is written"
            (let [e (try (ds/write! {:source-name "b"} {:set {}} session) nil
                         (catch clojure.lang.ExceptionInfo e (ex-data e)))]
              (is (= :refusal.foreign/multi-source (:refusal/code e)) (pr-str e))
              (is (= 422 (anomaly/status e)))))
          (is (not-any? #(= "b" (second %)) @posts) "source b saw nothing"))))))

(deftest lost-side-effect
  (let [store      (ctx/store "main")
        calls   (atom 0)
        fail?   (atom true)
        letters #(ctl/list store :outbox-dead {:branch "main"})]
    (run-log/append! store
                     [(run-log/outbox-entry {:kind :webhook :source :erp :webhook :melden
                                             :params {:x 1}} nil)])
    (with-redefs [action/performer (fn [& _] (fn [_ _] (swap! calls inc)
                                               (if @fail? (throw (ex-info "receiver down" {}))
                                                 {:status 200})))]
      (testing "an effect failing all its attempts is kept as :dead, not given up"
        (action-effects/dispatch-outbox! store)
        (is (= 3 @calls))
        (is (= ["dead"] (map :state (letters)))))
      (testing "it waits before the next round"
        (action-effects/dispatch-outbox! store)
        (is (= 3 @calls)))
      (testing "a later round delivers it and it leaves the dead letters"
        (reset! fail? false)
        (ctl/update! store :outbox-dead {:branch "main"} {:next-at (java.util.Date. 0)})
        (action-effects/dispatch-outbox! store)
        (is (= 4 @calls))
        (is (empty? (letters)))))))

(deftest foreign-overwrite
  (is (ok? (api/mutate :mechaniker-anlegen {:lizenz "FO-1" :name "Neu" :muster "A320"}
                       {:publish? true})))
  (let [id     (some #(when (= "FO-1" (get-in % [:values :lizenz])) (:id %))
                     (:rows (:body (api/list-objects :mechaniker))))
        quell  (atom "Alt")
        pushed (atom nil)]
    (with-redefs [wb/read-live-row (fn [tgt pk] {(:pk-attr tgt) pk :name @quell :muster "A320"})
                  wb/push!         (fn [_ spec & _] (reset! pushed spec) {:rows 1})]
      (let [pv (:body (api/post! :writeback-preview {:object-type :mechaniker :object-id id}))]
        (is (= "Alt" (get-in pv [:seen :name])) (pr-str pv))
        (reset! quell "Fremd")
        (testing "the push expects what the user saw, not the row read just before it"
          (is (ok? (api/post! :writeback-confirm
                              {:object-type :mechaniker :object-id id :seen (:seen pv)})))
          (is (= {"NAME" "Alt"} (:expect @pushed)) (pr-str @pushed)))
        (testing "a confirm that says nothing of what it saw is refused, unless forced"
          (reset! pushed nil)
          (is (= 400
                 (:status (api/post! :writeback-confirm {:object-type :mechaniker :object-id id}))))
          (is (nil? @pushed)))))))
