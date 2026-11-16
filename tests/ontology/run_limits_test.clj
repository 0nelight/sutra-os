(ns ^:integration ontology.run-limits-test
  (:require [sutra.ref.install :as ri]
            [sutra.ref.clock :as clock]
            [support.system :as tsys]
            [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.store.access.scope :as ctx]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.lookup :as lookup]
            [sutra.actions.dispatch :as c]
            [support.pod :as pod]
            [support.store :as store]
            [sutra.types.run-limits :as limits])
  (:import (com.zaxxer.hikari HikariDataSource)))

(use-fixtures :once pod/with-stores-only)

(def ^:private open-tickets
  {:op :search :type :ticket :filter {:eq {:path :status :value "open"}} :as :open})

(def ^:private open-tickets-2
  (assoc open-tickets :as :open2))

(def ^:private touch
  {:op :update :target :tk :values {:status "open"}})

(defn- ask [& {:as extra}]
  (merge {:op :use-llm :model "gpt-test" :source :lms :prompt "?" :output :string :as :answer}
         extra))

(defn- kernel-fn [nm steps & {:as extra}]
  (merge {:function-name nm :target-type :ticket :language :kernel
          :param-types {:tk :ticket}
          :source {:params {:tk {:binding :tk :type :ticket}} :steps steps}}
         extra))

(def ^:private schema
  {:object-types
   {:ticket {:object-type-name :ticket
             :attribute-list [{:attribute-name :title  :attribute-type :string}
                              {:attribute-name :status :attribute-type :string}]
             :plural "Tickets"}}
   :link-types  {}
   :object-sets {}
   :llm-models  {"gpt-test" {:in 1 :out 1}}
   :functions
   {:slow       (kernel-fn :slow [{:op :create :type :ticket :values {:title "neu" :status "open"}}
                                  (ask :tools [{:query-objects :ticket :properties [:title]}])]
                           :timeout-ms 1000)
    :reader     (kernel-fn :reader [open-tickets touch])
    :creator    (kernel-fn :creator [{:op :create :type :ticket :values {:title "a" :status "x"}}
                                     {:op :create :type :ticket :values {:title "b" :status "x"}}
                                     {:op :create :type :ticket :values {:title "c" :status "x"}}])
    :twice      (kernel-fn :twice [open-tickets open-tickets-2 touch])
    :twice-ask  (kernel-fn :twice-ask [open-tickets (ask) open-tickets-2 touch])
    :staged     (kernel-fn :staged
                           [{:op :create :type :ticket :values {:title "neu" :status "open"}}
                            open-tickets open-tickets-2]
                           :staged-writes true)}})

(def ^:private world (ri/derived {:sigma clock/system} schema))

(def ^:private ticket (lookup/rid-of world :object-types :ticket))

(defn- active-connections []
  (-> ^javax.sql.DataSource (tsys/ds :control)
      ^HikariDataSource (.unwrap HikariDataSource)
      .getHikariPoolMXBean
      .getActiveConnections))

(defn- system-store [] (ctx/store "main" grants/system-authority))

(defn- with-branch [f]
  (let [branch (str "limits-" (subs (str (random-uuid)) 0 8))]
    (try
      (store/fork! branch)
      (store/install! branch world)
      (store/insert-object! branch ticket {:id "t1" :values {:title "alt" :status "open"}})
      (tsys/ensure!)
      (f branch)
      (finally (try (store/retire! branch) (catch Exception _ nil))))))

(defn- insert-ticket! [branch id]
  (store/insert-object! branch ticket {:id id :values {:title id :status "open"}}))

(defn- ticket-count [branch]
  (count (store/objects branch ticket)))

(defn- counts-read [trace]
  (mapv :result-count (filter #(= :query (:op %)) @trace)))

(defn- with-concurrent-write-after-first-read
  [branch f]
  (let [t    (atom [])
        done (atom false)]
    (add-watch t ::poke (fn [_ _ _ v]
                          (when (and (some #(= :query (:op %)) v)
                                     (compare-and-set! done false true))
                            @(future (insert-ticket! branch (str "c-" (random-uuid)))))))
    (f t)
    t))

(defn- limit-of [^Throwable e]
  (some #(:sutra/run-limit (ex-data %)) (take-while some? (iterate #(.getCause ^Throwable %) e))))

(defn- thrown-limit [f]
  (try (f) nil (catch Throwable e (limit-of e))))

(defn- final-answer [v]
  {:tool_calls [{:id "c1" :type "function"
                 :function {:name "final_answer" :arguments (json/write-str {"value" v})}}]})

(def ^:private query-call
  {:tool_calls [{:id "q1" :type "function" :function {:name "query_ticket" :arguments "{}"}}]})

(deftest a-deadline-mid-tool-loop-discards-the-run-and-frees-its-connections
  (with-branch
    (fn [branch]
      (let [baseline (active-connections)
            budgets  (atom [])]
        (tsys/with-provided {:llm/complete (fn [req]
                                                (swap! budgets conj (:timeout-ms req))
                                                (Thread/sleep 400)
                                                {:request-id "r" :message query-call})}
          (is (= :timeout
                 (thrown-limit #(c/run-logic! (ctx/at (system-store) branch) world :slow {:tk "t1"}
                                              {:mode :apply})))))
        (is (<= 2 (count @budgets) 3) "the loop stopped at the deadline, not at its turn cap")
        (is (every? #(<= % 1000) @budgets)
            "each model call was given only what was left of the limit")
        (is (apply > @budgets) "and that shrinks turn by turn")
        (is (= 1 (ticket-count branch)) "the create it staged is discarded")
        (is (= baseline (active-connections)) "no connection is left checked out")))))

(deftest object-and-edit-caps-throw-and-apply-nothing
  (with-branch
    (fn [branch]
      (insert-ticket! branch "t2")
      (insert-ticket! branch "t3")
      (testing "a read past the objects-loaded limit"
        (let [t (atom [])]
          (with-redefs [limits/objects-loaded-max 2]
            (is (= :objects
                   (thrown-limit #(c/run-logic! (ctx/at (system-store) branch) world :reader
                                                {:tk "t1"}
                                                {:mode :apply :trace t})))))))
      (testing "more edits than one run may apply"
        (with-redefs [limits/edits-max 2]
          (is (= :edits
                 (thrown-limit #(c/run-logic! (ctx/at (system-store) branch) world :creator
                                              {:tk "t1"}
                                              {:mode :apply})))))
        (is (= 3 (ticket-count branch)) "nothing of the refused run landed")))))

(deftest a-run-reads-one-state-and-misses-a-concurrent-commit
  (with-branch
    (fn [branch]
      (let [t (with-concurrent-write-after-first-read branch
                #(c/run-logic! (ctx/at (system-store) branch) world :twice {:tk "t1"}
                               {:mode :apply :trace %}))]
        (is (= [1 1] (counts-read t)) "both reads of the run see its S₀")))))

(deftest a-model-call-does-not-move-the-runs-state
  (with-branch
    (fn [branch]
      (let [baseline (active-connections)
            held     (atom nil)]
        (tsys/with-provided {:llm/complete (fn [_]
                                                (reset! held (active-connections))
                                                {:request-id "r" :message (final-answer "ok")})}
          (let [t (with-concurrent-write-after-first-read branch
                    #(c/run-logic! (ctx/at (system-store) branch) world :twice-ask {:tk "t1"}
                                   {:mode :apply :trace %}))]
            (is (= [1 1] (counts-read t)) "the read after the model call still sees the run's S₀")
            (is (= baseline @held) "and no connection is held through the call")))))))

(deftest staged-writes-replay-on-top-of-the-snapshot
  (with-branch
    (fn [branch]
      (let [t (with-concurrent-write-after-first-read branch
                #(c/run-logic! (ctx/at (system-store) branch) world :staged {:tk "t1"}
                               {:mode :apply :trace %}))]
        (is (= [2 2] (counts-read t)) "its own create is seen, the concurrent one is not")
        (is (= 3 (ticket-count branch)) "and both land")))))
