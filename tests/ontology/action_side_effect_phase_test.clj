(ns ^:unit ontology.action-side-effect-phase-test
  (:require [support.system :as tsys]
            [clojure.test :refer [deftest is testing]]
            [sutra.actions.action :as action]
            [sutra.actions.operations :as ops]))

(defn- logged [res] (mapv (juxt :op :type :payload) (:ops res)))

(defn- boom-verb
  [_step _world _bindings _ctx _io]
  (throw (ex-info "the foreign system said no" {:sutra.kit.anomaly/category :upstream})))

(defn- ctx []
  (tsys/ensure!)
  {:verbs (merge (tsys/verbs) {:boom boom-verb})})

(defn- world-with [rules & [params]]
  {:object-types {:ticket {:object-type-name :ticket
                           :attribute-list [{:attribute-name :ticket-id :attribute-type :string}
                                            {:attribute-name :title :attribute-type :string}]
                           :plural "Tickets"}}
   :link-types {}
   :actions {:touch {:rules rules
                     :params (or params {:ticket {:type :ticket}})}}})

(def ^:private a-ticket
  {:id "t-1" :object-type-ref :ticket :values {:ticket-id "t-1" :title "before"}})

(deftest a-failing-side-effect-does-not-retract-the-action
  (let [res (ops/execute-action
             (world-with [{:op :update :target :ticket :values {:title "after"}}
                          {:op :boom :phase :side-effect}
                          {:op :notify :phase :side-effect :channel "ops" :payload "still ran"}])
             {} (ctx) nil :touch {:arg0 {:ticket a-ticket}})
        evs (:side-effects res)]
    (is (= [:update-object] (mapv first (logged res)))
        "the object change stands — the user was already told it succeeded")
    (is (some #(= :side-effect-failed (:op %)) evs)
        "the failure is RECORDED")
    (let [failed (first (filter #(= :side-effect-failed (:op %)) evs))]
      (is (= :boom (:rule-op failed))
          "naming the rule that failed")
      (is (re-find #"the foreign system said no" (:error failed))
          "with the reason the foreign system gave"))
    (is (some #(= :notify (:op %)) evs)
        "and the next side effect still runs — no guaranteed order means no chaining")))

(deftest a-side-effect-sees-the-whole-action-once
  (let [res (ops/execute-action
             (world-with [{:op :update :target :ticket :values {:title "after"}}
                          {:op :notify :phase :side-effect :channel "ops" :payload [:get :ticket]}]
                         {:ticket {:type :ticket :multiple true}})
             {} (ctx) nil :touch {:arg0 {:ticket [a-ticket a-ticket]}})
        notifies (filter #(= :notify (:op %)) (:side-effects res))]
    (is (= 2 (count (filter #(= :update-object (first %)) (logged res))))
        "the writeback phase fans out over the multiple parameter")
    (is (= 1 (count notifies))
        "the side effect does NOT")
    (is (= 2 (count (:payload (first notifies))))
        "and that list is what it receives")))

(deftest der-fehlgeschlagene-seiteneffekt-landet-in-der-lauf-summary
  (let [res (ops/execute-action
             (world-with [{:op :update :target :ticket :values {:title "after"}}
                          {:op :boom :phase :side-effect}])
             {} (ctx) nil :touch {:arg0 {:ticket a-ticket}})
        failed (action/side-effect-failures res)]
    (testing "was der Lauf aufzeichnet, benennt Regel und Grund"
      (is (= [:boom] (mapv :rule-op failed)))
      (is (re-find #"the foreign system said no" (:error (first failed)))))
    (testing "ein Lauf ohne Ausfall traegt keine Summary-Last"
      (is (empty? (action/side-effect-failures
                   (ops/execute-action
                    (world-with [{:op :update :target :ticket :values {:title "after"}}
                                 {:op :notify :phase :side-effect :channel "ops" :payload "ok"}])
                    {} (ctx) nil :touch {:arg0 {:ticket a-ticket}})))))))

(deftest an-edit-in-the-side-effect-phase-is-refused-at-run-time
  (is (thrown-with-msg? Exception #"side-effect phase"
                        (ops/execute-action
                         (world-with [{:op :update :target :ticket :values {:title "after"}
                                       :phase :side-effect}])
                         {} (ctx) nil :touch {:arg0 {:ticket a-ticket}}))))
