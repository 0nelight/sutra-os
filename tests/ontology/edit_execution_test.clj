(ns ^:integration ontology.edit-execution-test
  (:require [sutra.ref.install :as ri]
            [sutra.ref.clock :as clock]
            [support.system :as tsys]
            [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.store.access.scope :as ctx]
            [sutra.catalog.grants :as grants]
            [sutra.governance.guards :as guards]
            [sutra.catalog.lookup :as lookup]
            [sutra.actions.dispatch :as c]
            [sutra.actions.request :as request]
            [sutra.actions.action :as action]
            [support.pod :as pod]
            [support.store :as store])
  (:import (com.zaxxer.hikari HikariDataSource)))

(use-fixtures :once pod/with-stores-only)

(def ^:private open-tickets
  {:op :search :type :ticket :filter {:eq {:path :status :value "open"}} :as :open})

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
   {:ask      (kernel-fn :ask [{:op :use-llm :model "gpt-test" :source :lms
                                :prompt "Ticket {{tk.title}}" :output :string :as :answer}
                               {:op :update :target :tk :values {:status [:ref :answer]}}])
    :regular  (kernel-fn :regular [{:op :create :type :ticket :values {:title "neu" :status "open"}}
                                   open-tickets])
    :staged   (kernel-fn :staged [{:op :create :type :ticket :values {:title "neu" :status "open"}}
                                  open-tickets]
                         :staged-writes true)
    :inner    (kernel-fn :inner [open-tickets
                                 {:op :create :type :ticket
                                  :values {:title "innen" :status "open"}}])
    :outer    (kernel-fn :outer
                         [{:op :create :type :ticket :values {:title "aussen" :status "open"}}
                          {:op :effect :fn :inner-act :args {:arg0 {:tk [:ref :tk :id]}}}
                          open-tickets]
                         :staged-writes true)
    :doomed   (kernel-fn :doomed [{:op :create :type :ticket :values {:title "weg" :status "open"}}
                                  open-tickets
                                  {:op :use-llm :model "gpt-test" :source :lms :prompt "?"
                                   :output :string :as :answer}]
                         :staged-writes true)}
   :actions
   {:inner-act {:params {:tk {:name "Ticket" :type :ticket}}
                :rules  [{:op :apply-edit-function :function :inner}]
                :edits  [{:object-type :ticket}]}
    :outer-act {:params {:tk {:name "Ticket" :type :ticket}}
                :rules  [{:op :apply-edit-function :function :outer}]
                :edits  [{:object-type :ticket}]}}})

(defn- final-answer [v]
  {:tool_calls [{:id "c1" :type "function"
                 :function {:name "final_answer" :arguments (json/write-str {"value" v})}}]})

(def ^:private world (ri/derived {:sigma clock/system} schema))

(def ^:private ticket (lookup/rid-of world :object-types :ticket))

(defn- active-connections []
  (-> ^javax.sql.DataSource (tsys/ds :control)
      ^HikariDataSource (.unwrap HikariDataSource)
      .getHikariPoolMXBean
      .getActiveConnections))

(defn- system-store [] (ctx/store "main" grants/system-authority))

(defn- with-branch [f]
  (let [branch (str "editexec-" (subs (str (random-uuid)) 0 8))]
    (try
      (store/fork! branch)
      (store/install! branch world)
      (store/insert-object! branch ticket {:id "t1" :values {:title "alt" :status "open"}})
      (tsys/ensure!)
      (f branch)
      (finally (try (store/retire! branch) (catch Exception _ nil))))))

(defn- ticket-count [branch]
  (count (store/objects branch ticket)))

(defn- counts-read [trace]
  (mapv :result-count (filter #(= :query (:op %)) @trace)))

(defn- traced [f]
  (let [t (atom [])]
    (f t)
    t))

(deftest a-logic-run-holds-no-connection-while-the-model-thinks
  (with-branch
    (fn [branch]
      (let [baseline (active-connections)
            during   (atom nil)]
        (tsys/with-provided {:llm/complete (fn [_]
                                                (reset! during (active-connections))
                                                {:request-id "r1"
                                                 :message (final-answer "erledigt")})}
          (c/run-logic! (ctx/at (system-store) branch) world :ask {:tk "t1"} {:mode :apply}))
        (is (= baseline @during) "not one connection is checked out during the model call")
        (is (= "erledigt" (get-in (store/object branch ticket "t1")
                                  [:values :status]))
            "and the edit it decided on is applied afterwards")))))

(deftest regular-reads-see-the-ontology-before-the-run
  (with-branch
    (fn [branch]
      (let [t (traced #(c/run-logic! (ctx/at (system-store) branch) world :regular {:tk "t1"}
                                     {:mode :apply :trace %}))]
        (is (= [1] (counts-read t)) "the search does not see the ticket the run just created")
        (is (= 2 (ticket-count branch)) "the create is applied at the end")))))

(deftest staged-reads-see-their-own-edits-including-nested-calls
  (with-branch
    (fn [branch]
      (testing "read-after-write in one staged function"
        (let [t (traced #(c/run-logic! (ctx/at (system-store) branch) world :staged {:tk "t1"}
                                       {:mode :apply :trace %}))]
          (is (= [2] (counts-read t)))
          (is (= 2 (ticket-count branch)))))
      (testing "an Action running a staged function that calls another Action's function"
        (let [t (traced
                 #(action/run-action!
                   (request/action-request {:action :outer-act :args {:tk "t1"} :trace %
                                            :store (guards/data-guard (ctx/at (system-store) branch))
                                            :schema world})))]
          (is (= [3 4] (counts-read t))
              "the inner search sees the outer create; the outer search sees the inner one")
          (is (= 4 (ticket-count branch)) "one apply commits both functions' edits"))))))

(deftest a-throw-after-staged-edits-leaves-the-ontology-unchanged
  (with-branch
    (fn [branch]
      (let [t (traced (fn [t]
                        (is (thrown? Exception
                                     (tsys/with-provided
                                       {:llm/complete (fn [_] (throw (ex-info "model down" {})))}
                     (c/run-logic! (ctx/at (system-store) branch) world :doomed {:tk "t1"}
                                   {:mode :apply :trace t}))))))]
        (is (= [2] (counts-read t)) "the staged ticket was visible inside the run")
        (is (= 1 (ticket-count branch)) "and is gone with the run")))))
