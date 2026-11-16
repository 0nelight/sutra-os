(ns ^:unit ontology.agent-proposals-test
  (:require [clojure.test :refer [deftest is testing]]
            [malli.core :as m]
            [sutra.parser.operational :as op]
            [sutra.actions.proposals :as proposals]
            [sutra.automation.rules :as rules]
            [sutra.automation.runs :as autos]
            [sutra.automation.activity :as activity]
            [sutra.catalog.actions :as actions]
            [sutra.catalog.grants :as grants]
            [support.logic-ledger :as ledger]
            [sutra.actions.dispatch :as c]
            [sutra.objects.reads :as objects]
            [sutra.store.seat :as seat]
            [sutra.actions.action :as action]
            [sutra.store.control :as ctl]
            [sutra.store.access.scope :as ctx]))

(defn- valid? [effect]
  (m/validate op/EntAutomation {:name "kostenstelle" :on :invoice :effect effect}))

(defn- fake-table []
  (let [rows (atom [])
        match? (fn [where r] (every? (fn [[k v]] (= v (get r k))) where))]
    [rows
     {#'ctx/store       (constantly nil)
      #'seat/branch-schema (constantly {})
      #'ctl/count   (fn [_ _ where] (count (filter #(match? where %) @rows)))
      #'ctl/insert! (fn [_ _ row] (swap! rows conj row) row)}]))

(deftest the-same-proposal-is-not-staged-twice-while-open
  (let [[rows redefs] (fake-table)
        branch "main"
        p  {:automation-id "a1" :object-id "i1" :action :assign}]
    (with-redefs-fn redefs
      (fn []
        (testing "a first proposal is staged"
          (is (some? (proposals/propose! nil branch (assoc p :args {:invoice "i1" :cc "4711"})))))
        (testing "the next tick proposes the same thing — key order and id spelling do not make it new"
          (is (nil? (proposals/propose! nil branch (assoc p :args {:cc "4711" :invoice "i1"})))))
        (testing "different arguments are a different proposal"
          (is (some? (proposals/propose! nil branch (assoc p :args {:invoice "i1" :cc "0815"})))))
        (testing "once decided, the same proposal may come again"
          (swap! rows (fn [rs] (mapv #(assoc % :status "rejected") rs)))
          (is (some? (proposals/propose! nil branch (assoc p :args {:invoice "i1" :cc "4711"})))))))
    (is (= 3 (count @rows)))
    (is (= #{"open" "rejected"} (set (map :status @rows))))))

(deftest digest-is-canonical
  (let [u (random-uuid)]
    (is (= (proposals/digest {:a 1 :b {:c u}}) (proposals/digest {:b {:c (str u)} :a 1})))
    (is (not= (proposals/digest {:a 1}) (proposals/digest {:a 2})))))

(def ^:private schema
  {:object-types {}
   :functions {:ri.fn {:function-name :classify :source {} :target-type :invoice
                       :param-types {:invoice :invoice}}}})

(defn- run-logic-automation [effect run-logic]
  (let [staged (atom []) recorded (atom nil) calls (atom [])]
    (with-redefs-fn (second (ledger/fake))
      (fn [] (with-redefs [ctx/store            (constantly nil)
                           seat/branch-schema   (constantly schema)
                           objects/objects-of-type (fn [& _] [{:id "i1" :values {:nr "R-1"}}
                                                        {:id "i2" :values {:nr "R-2"}}])
                           c/run-logic!
                             (fn [_ _ f in opts] (swap! calls conj [f in (:mode opts)])
                               (run-logic in))
                           proposals/propose!    (fn [_ _ p] (swap! staged conj p) p)
                           autos/mark-fired! (fn [& _])
rules/owner-authority (constantly {:user "u-owner" :username "owner"})
                           autos/record-run! (fn [_ _ _ r] (reset! recorded r))
                           activity/after-run! (fn [& _])]
               (let [r (rules/run-one! nil "main"
                                       {:id "a1" :owner "owner" :on "invoice"
                                        :effect-spec (pr-str effect)})]
                 {:result r :staged @staged :recorded @recorded :calls @calls}))))))

(deftest logic-effect-runs-the-function-once-per-object
  (let [{:keys [result staged recorded calls]}
        (run-logic-automation {:function :classify :mode :propose}
                              (fn [in]
                                {:proposals [{:action :assign
                                              :args {:invoice (:invoice in) :cc "4711"}}]
                                 :decisions [{:model "m"
                                              :log [{:request-id "r1"} {:output "4711"}]}]}))]
    (testing "the object reaches the function's one parameter of the automation's type"
      (is (= #{[:classify {:invoice "i1"} :propose] [:classify {:invoice "i2"} :propose]}
             (set calls))))
    (testing "every proposed Action is staged against its object, with the decision log"
      (is (= #{"i1" "i2"} (set (map :object-id staged))))
      (is (every? #(= "a1" (:automation-id %)) staged))
      (is (every? :decision-log staged)))
    (testing "the run records what was proposed and which model calls decided it"
      (is (= 2 (:proposed result) (:proposed recorded)))
      (is (= ["r1"] (-> recorded :decisions first :decisions first :request-ids))))))

(deftest a-failing-object-does-not-stop-the-others
  (let [{:keys [result]}
        (run-logic-automation {:function :classify}
                              (fn [in] (if (= "i1" (:invoice in))
                                         (throw (ex-info ":use-llm — nope" {}))
                                         {:proposals [] :decisions []})))]
    (is (= ["i2"] (:affected-ids result)))
    (is (= 1 (count (:errors result))))))

(deftest propose-mode-action-effect-stages-instead-of-dispatching
  (let [staged (atom [])]
    (with-redefs [ctx/store            (constantly nil)
                  seat/branch-schema   (constantly {:actions {:assign {:rules []}}})
                  objects/objects-of-type (fn [& _] [{:id "i1" :values {:nr "R-1"}}])
                  c/dispatch-action! (fn [& _] (throw (ex-info "must not dispatch" {})))
                  proposals/propose!    (fn [_ _ p] (swap! staged conj p) p)
                  autos/mark-fired! (fn [& _])
                  rules/owner-authority (constantly {:user "u-owner" :username "owner"})
                  autos/record-run! (fn [& _])
                  activity/after-run! (fn [& _])]
      (let [r (rules/run-one! nil "main"
                              {:id "a1" :owner "owner" :on "invoice"
                               :effect-spec (pr-str {:action :assign :mode :propose
                                                     :args {:invoice {:self :id}
                                                            :nr {:self :nr}}})})]
        (is (= 1 (:proposed r)))
        (is (= [{:automation-id "a1" :object-id "i1" :action :assign
                 :args {:invoice "i1" :nr "R-1"}}]
               @staged))))))

(deftest a-replay-effect-needs-propose-mode
  (is (valid? {:action :erfassen :mode :propose :key {:self :id} :reason "gelesen"
               :provenance {:model {:self :modell}}}))
  (is (not (valid? {:action :erfassen :key {:self :id}})))
  (is (not (valid? {:action :erfassen :mode :apply :reason "x"}))))

(deftest a-recorded-reading-is-staged-once-ever
  (let [[rows redefs] (fake-table)
        branch "main"
        p  {:automation-id "a1" :object-id "r1" :action :erfassen :args {:nr "R-1"}
            :source-key "grok_email_attached:77"}]
    (with-redefs-fn redefs
      (fn []
        (is (some? (proposals/propose! nil branch p)))
        (testing "re-running the migration while it is open stages nothing"
          (is (nil? (proposals/propose! nil branch p))))
        (testing "decided — rejected or applied — it is never staged again, not even with other values"
          (swap! rows (fn [rs] (mapv #(assoc % :status "rejected") rs)))
          (is (nil? (proposals/propose! nil branch (assoc p :args {:nr "R-1 korrigiert"})))))
        (testing "another reading is another proposal"
          (is (some? (proposals/propose! nil branch
                                         (assoc p :source-key "grok_email_attached:78")))))))
    (is (= 2 (count @rows)))
    (is (every? :source-key @rows))))

(deftest a-replay-effect-carries-reason-provenance-and-key
  (let [staged (atom [])]
    (with-redefs [ctx/store            (constantly nil)
                  seat/branch-schema   (constantly {:actions {:erfassen {:rules []}}})
                  objects/objects-of-type (fn [& _]
                                            [{:id "o1"
                                              :values {:nr "R-1" :betrag 118.4 :modell "grok-4"
                                                 :gelesen-am "2026-08-02" :pk "77"}}
                                       {:id "o2" :values {:nr "R-2" :modell "grok-4" :pk "78"}}
                                       {:id "o3" :values {:nr "R-3"}}])
                  c/dispatch-action! (fn [& _] (throw (ex-info "must not dispatch" {})))
                  proposals/propose!    (fn [_ _ p] (swap! staged conj p) p)
                  autos/mark-fired! (fn [& _])
                  rules/owner-authority (constantly {:user "u-owner" :username "owner"})
                  autos/record-run! (fn [& _])
                  activity/after-run! (fn [& _])]
      (let [r (rules/run-one! nil "main"
                              {:id "a1" :owner "owner" :on "rechnung"
                               :effect-spec (pr-str {:action :erfassen :mode :propose
                                                     :args {:nr {:self :nr} :betrag {:self :betrag}}
                                                     :key {:template "grok_email_attached:{{pk}}"}
                                                     :reason
                                                       {:template
                                                          "{{modell}} las {{nr}} am {{gelesen-am}}"}
                                                     :provenance {:model {:self :modell}
                                                                  :read-at {:self :gelesen-am}
                                                                  :source-dataset
                                                                    "grok_email_attached"
                                                                  :source-pk {:self :pk}}})}
                              {:manual? true})
            [s1 s2] @staged]
        (testing "every reading with a key becomes one proposal"
          (is (= 2 (:proposed r)))
          (is (= ["grok_email_attached:77" "grok_email_attached:78"] (mapv :source-key @staged))))
        (testing "the reading's values are the Action's parameters; a missing one is left to the reviewer"
          (is (= {:nr "R-1" :betrag 118.4} (:args s1)))
          (is (= {:nr "R-2"} (:args s2))))
        (testing "reason and provenance are read from the same object"
          (is (= "grok-4 las R-1 am 2026-08-02" (:reason s1)))
          (is (= {:model "grok-4" :read-at "2026-08-02" :source-dataset "grok_email_attached"
                  :source-pk "77"}
                 (:provenance s1)))
          (is (= {:model "grok-4" :source-dataset "grok_email_attached" :source-pk "78"}
                 (:provenance s2))))
        (testing "a reading without a key value is an error, never an unkeyed proposal"
          (is (= 1 (count (:errors r)))))))))

(deftest edit-then-accept-runs-the-action-with-the-reviewers-values
  (let [row      (atom {:id "p1" :branch "main" :automation-id "a1" :action :erfassen
                        :args {:nr "R-1" :betrag 118.4} :status "open"})
        updates  (atom [])
        ran      (atom nil)
        schema   {:actions {:erfassen {:params {:nr {:type :string} :betrag {:type :double}
                                                :lieferant {:type :string}}}}}]
    (with-redefs [ctx/store                  (constantly nil)
                  ctl/clock              (fn [_] (java.util.Date.))
                  ctl/get                (fn [& _] @row)
                  ctl/update!            (fn [_ _ where sets]
                                            (swap! updates conj sets)
                                            (if (and (= "open" (:status where))
                                                     (not= "open" (:status @row)))
                                              0
                                              (do (swap! row merge sets) 1)))
                  ctl/swapping           (fn [_ _ _ f _] {:records [{:value (f @row)}]})
                  c/dispatch-action!      (fn [_ _ a args & [{:keys [records]}]]
                                            (reset! ran [a args])
                                            (doseq [r records] (reset! row (:value r)))
                                            {:tx-vector []})
                  action/edited-objects        (constantly ["o9"])
                  actions/resolve-action     (fn [s a] (get-in s [:actions a]))
                  grants/acting-username    (constantly "edda")]
      (testing "an unknown parameter is refused before anything is decided"
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"no parameter gibtsnicht"
                              (proposals/accept! nil "main" schema "a1" "p1" {:gibtsnicht 1})))
        (is (= "open" (:status @row))))
      (let [r (proposals/accept! nil "main" schema "a1" "p1"
                                 {:betrag 181.4 :lieferant "Teile Maier"})]
        (is (= [:erfassen {:nr "R-1" :betrag 181.4 :lieferant "Teile Maier"}] @ran))
        (is (= "applied" (:status r)))
        (testing "both the proposed and the accepted values stay on the proposal"
          (is (= {:nr "R-1" :betrag 118.4} (:args @row)))
          (is (= {:nr "R-1" :betrag 181.4 :lieferant "Teile Maier"} (:accepted-args @row))))))))
