(ns ^:unit ontology.logic-effect-test
  (:require [clojure.test :refer [deftest is testing]]
            [malli.core :as m]
            [sutra.access.authority :as authority]
            [sutra.store.access.scope :as ctx]
            [sutra.automation.logic-events :as logic-events]
            [sutra.actions.proposals :as proposals]
            [sutra.automation.rules :as rules]
            [sutra.automation.runs :as autos]
            [sutra.automation.activity :as activity]
            [sutra.actions.dispatch :as c]
            [sutra.objects.reads :as objects]
            [sutra.store.seat :as seat]
            [sutra.parser.operational :as op]
            [sutra.access.principals :as pr]
            [support.logic-ledger :as ledger]))

(def ^:private schema
  {:object-types {}
   :actions {:lesung-fehlgeschlagen {:rules []}}
   :functions {:ri.fn {:function-name :beleg-lesen :source {} :target-type :lesung
                       :param-types {:dokument :eingangsdokument} :timeout-ms 1000}}})

(defn- doc [id & {:as vals}] {:id id :values (merge {:einordnung "offen" :pdf (str "h-" id)} vals)})

(def ^:private owner-authority rules/owner-authority)

(defn- harness
  [objects reply]
  (let [[rows redefs] (ledger/fake)
        calls (atom []) staged (atom []) runs (atom []) dispatched (atom []) sleeps (atom [])
        seen  (atom [])]
    {:rows rows :calls calls :staged staged :runs runs :dispatched dispatched :sleeps sleeps
     :seen seen
     :tick!
     (fn tick!
       ([effect] (tick! effect {} {}))
       ([effect opts] (tick! effect opts {}))
       ([effect opts auto]
        (with-redefs-fn
          (merge redefs
                 {#'rules/owner-authority (fn [store owner b]
                                            (if (= ::owner owner)
                                              {:user "u-owner" :username "owner"}
                                                (owner-authority store owner b)))
                  #'ctx/store              (constantly nil)
                  #'seat/branch-schema     (constantly schema)
                  #'objects/objects-of-type   (fn [& _] @objects)
                  #'c/run-logic!        (fn [store _ f in o]
                                          (swap! calls conj [f in (:mode o)])
                                          (swap! seen conj {:provenance (:provenance o)
                                                            :authority (:authority store) :opts o})
                                          (reply in o))
                  #'c/dispatch-action!  (fn [_ _ a args] (swap! dispatched conj [a args]) {})
                  #'proposals/propose!      (fn [_ _ p] (swap! staged conj p) p)
                  #'autos/mark-fired!   (fn [& _])
                  #'autos/record-run!   (fn [_ _ _ r] (swap! runs conj r))
                  #'activity/after-run! (fn [& _])})
          #(rules/run-one! nil "main"
                           (merge {:id "a1" :name "beleg-lesen" :on "eingangsdokument"
                                   :when-spec (pr-str [:= [:get :einordnung] "offen"])
                                   :effect-spec (pr-str effect) :owner ::owner}
                                  auto)
                           (assoc opts :sleep! (fn [ms] (swap! sleeps conj ms)))))))}))

(defn- reading [in _]
  {:proposals [{:action :lesung-anlegen :args {:dokument (:dokument in) :rechnungsnr "RE-1"}}]
   :decisions [{:model "grok-test" :prompt-sha "p1"
                :log [{:request-id "r1"} {:output {:rechnungsnr "RE-1"}}]}]
   :applied   [{:action :lesung-anlegen :args {:dokument (:dokument in)}}]})

(def ^:private effect {:function :beleg-lesen :inputs {:dokument {:self :id}}})

(deftest apply-mode-reads-each-object-once
  (let [objs (atom [(doc "d1") (doc "d2")])
        {:keys [tick! calls rows]} (harness objs reading)]
    (let [r (tick! effect)]
      (is (= 2 (count @calls)) "one model run per object")
      (is (= #{"d1" "d2"} (set (:affected-ids r)))))
    (testing "the next tick finds both events done: no model call, nothing applied again"
      (let [r (tick! effect)]
        (is (= 2 (count @calls)))
        (is (= 2 (:skipped r)))
        (is (= [] (:affected-ids r)))))
    (testing "the outcome is on the event: what was applied, which model call decided it"
      (let [row (first (filter #(= "done" (:status %)) (vals @rows)))]
        (is (= "grok-test" (-> row :outcome :decisions first :model)))
        (is (= ["r1"] (-> row :outcome :decisions first :request-ids)))
        (is (= 1 (count (-> row :outcome :applied))))))
    (testing "a changed object is a new version, and a new event"
      (swap! objs assoc 0 (doc "d1" :betreff "korrigiert"))
      (tick! effect)
      (is (= 3 (count @calls)))
      (is (= "d1" (get-in (last @calls) [1 :dokument]))))
    (testing ":once :object reads an object once, whatever changes"
      (let [objs (atom [(doc "d9")])
            {:keys [tick! calls]} (harness objs reading)
            e (assoc effect :once :object)]
        (tick! e)
        (swap! objs assoc 0 (doc "d9" :betreff "neu"))
        (tick! e)
        (is (= 1 (count @calls)))))))

(deftest propose-mode-stages-once-with-the-decision-log
  (let [{:keys [tick! staged calls]} (harness (atom [(doc "d1")]) reading)]
    (tick! (assoc effect :mode :propose))
    (tick! (assoc effect :mode :propose))
    (is (= 1 (count @calls)))
    (is (= 1 (count @staged)) "the next tick stages nothing — the event is done")
    (let [p (first @staged)]
      (is (= :lesung-anlegen (:action p)))
      (is (= "d1" (:object-id p)))
      (is (= "grok-test" (-> p :decision-log first :model))))
    (is (= :propose (last (first @calls))))))

(deftest the-run-carries-its-event-as-provenance-and-pins-the-version
  (let [{:keys [tick! seen]} (harness (atom [(doc "d1")]) reading)]
    (tick! (assoc effect :version "1.2.0" :auto-upgrade true))
    (let [{:keys [provenance opts]} (first @seen)]
      (is (= {:automation "beleg-lesen" :automation-id "a1" :object "d1"}
             (dissoc provenance :event-id)))
      (is (string? (:event-id provenance)))
      (is (= {:mode :apply :version "1.2.0" :auto-upgrade true}
             (select-keys opts [:mode :version :auto-upgrade]))))))

(defn- refused [status]
  (ex-info (str "language-model call refused (" status "): daily LLM budget reached")
           {:status status}))

(deftest a-transient-failure-is-recorded-and-tried-again-when-due
  (let [down? (atom true)
        {:keys [tick! calls rows staged runs]}
        (harness (atom [(doc "d1")]) (fn [in o] (if @down? (throw (refused 429)) (reading in o))))]
    (let [r (tick! (assoc effect :mode :propose))]
      (is (= [] (:affected-ids r)) "no edit")
      (is (empty? @staged) "no proposal")
      (is (= "error" (:status (last @runs))) "the run records the failure")
      (let [f (first (:failures r))]
        (is (:transient f))
        (is (= 60000 (:retry-in-ms f)) "the first cool-down is a minute")))
    (testing "before the cool-down has passed, the next tick does not call the model"
      (tick! (assoc effect :mode :propose))
      (is (= 1 (count @calls))))
    (testing "when it is due, the SAME event runs again — and succeeds"
      (reset! down? false)
      (swap! rows update-vals #(assoc % :due? true))
      (tick! (assoc effect :mode :propose))
      (is (= 2 (count @calls)))
      (is (= 1 (count @staged)))
      (is (= #{"done"} (set (map :status (vals @rows)))))
      (is (= 1 (count @rows)) "one event, not two"))))

(deftest a-deterministic-failure-waits-for-a-person-or-a-change
  (let [{:keys [tick! calls rows]}
        (harness (atom [(doc "d1")])
                 (fn [_ _] (throw (ex-info "Parameter netto: must be >= 0"
                                           {:sutra.kit.anomaly/category :unprocessable}))))]
    (let [r (tick! effect)]
      (is (false? (:transient (first (:failures r)))))
      (is (nil? (:retry-in-ms (first (:failures r))))))
    (swap! rows update-vals #(assoc % :due? false))
    (tick! effect)
    (is (= 1 (count @calls)) "no automatic retry of an answer the Action refused")
    (tick! effect {:manual? true})
    (is (= 2 (count @calls)) "a manual run retries the failed event")
    (is (= 2 (:attempts (first (vals @rows)))))))

(deftest retries-back-off-within-the-event
  (let [n (atom 0)
        {:keys [tick! calls sleeps]}
        (harness (atom [(doc "d1")])
                 (fn [in o] (if (< (swap! n inc) 3) (throw (refused 503)) (reading in o))))
        r (tick! (assoc effect
                        :retry {:attempts 2 :backoff :exponential :delay-ms 1000 :jitter 0.0}))]
    (is (= 3 (count @calls)))
    (is (= [1000 2000] @sleeps))
    (is (= ["d1"] (:affected-ids r))))
  (testing "constant backoff, and jitter stays within its band"
    (let [p {:backoff :constant :delay-ms 1000 :max-delay-ms 30000 :jitter 0.2}]
      (is (= 1000 (rules/backoff-ms (assoc p :jitter 0.0) 3 0.9)))
      (is (<= 800 (rules/backoff-ms p 2 0.0) (rules/backoff-ms p 2 0.99) 1200))
      (is (= 30000 (rules/backoff-ms (assoc p :backoff :exponential :jitter 0.0) 10 0.5))
          "capped")))
  (testing "a run limit is not retried"
    (let [{:keys [tick! calls]}
          (harness (atom [(doc "d1")])
                   (fn [_ _] (throw (ex-info "too many edits" {:sutra/run-limit :edits}))))]
      (tick! (assoc effect :retries 3))
      (is (= 1 (count @calls))))))

(deftest the-fallback-runs-once-per-failed-event
  (let [{:keys [tick! dispatched rows]}
        (harness (atom [(doc "d1") (doc "d2")])
                 (fn [in o] (if (= "d1" (:dokument in)) (throw (refused 502)) (reading in o))))
        e (assoc effect :fallback {:action :lesung-fehlgeschlagen
                                   :args {:dokument {:self :id} :fehler {:failure :message}
                                          :ereignis {:failure :event-id}}})]
    (tick! e)
    (is (= 1 (count @dispatched)) "only the failed object reaches the fallback")
    (let [[a args] (first @dispatched)]
      (is (= :lesung-fehlgeschlagen a))
      (is (= "d1" (:dokument args)))
      (is (re-find #"502" (:fehler args)))
      (is (string? (:ereignis args))))
    (swap! rows update-vals #(assoc % :due? true))
    (tick! e)
    (is (= 1 (count @dispatched)) "the event's second failure does not run it again")
    (is (= "applied" (some (fn [[[_ o] r]] (when (= "d1" o) (:status (:fallback r)))) @rows)))))

(deftest the-effect-runs-as-the-automations-owner
  (testing "the owner's authority, not the poller's"
    (let [{:keys [tick! seen]} (harness (atom [(doc "d1")]) reading)]
      (with-redefs [pr/fetch (fn [_ u] (when (= u "edda") {:username u :user-object-id "u-eli"}))
                    ctx/store (constantly {})
                    authority/context-for (fn [_ {:keys [user-id username]}]
                                            {:user-id user-id :username username :ops #{:read}})]
        (tick! effect {} {:owner "edda"}))
      (is (= {:user "u-eli" :username "edda"}
             (select-keys (:authority (first @seen)) [:user :username])))))
  (testing "no owner at all: nothing runs — not as the poller, not as whoever fires it"
    (let [{:keys [tick! calls runs]} (harness (atom [(doc "d1")]) reading)
          r (tick! effect {} {:owner nil})]
      (is (empty? @calls))
      (is (re-find #"has no owner" (first (:errors r))))
      (is (= "error" (:status (last @runs))))))
  (testing "an owner who no longer exists: nothing runs, the run says why"
    (let [{:keys [tick! calls runs]} (harness (atom [(doc "d1")]) reading)]
      (with-redefs [pr/fetch (constantly nil)]
        (let [r (tick! effect {} {:owner "weg"})]
          (is (empty? @calls))
          (is (re-find #"owner weg no longer exists" (first (:errors r))))
          (is (= "error" (:status (last @runs)))))))))

(deftest a-preview-shows-one-object-and-writes-nothing
  (let [{:keys [tick! calls rows staged runs]} (harness (atom [(doc "d1") (doc "d2")]) reading)]
    (let [r (tick! (assoc effect :mode :propose) {:dry? true :object-id "d2" :with-model? true})
          [p] (:previews r)]
      (is (= 1 (count (:previews r))))
      (is (= "d2" (:object p)))
      (is (= {:status "new"} (:event p)))
      (is (= [:lesung-anlegen] (mapv :action (:would-propose p))))
      (is (= :propose (last (first @calls))) "the model runs in propose mode: nothing is applied"))
    (is (empty? @rows) "no event claimed")
    (is (empty? @staged))
    (is (empty? @runs))
    (testing "without :with-model? no model is called"
      (tick! effect {:dry? true})
      (is (= 1 (count @calls))))))

(deftest transient-failures-are-told-apart
  (is (logic-events/transient? (refused 429)))
  (is (logic-events/transient? (ex-info "Function step 0 failed" {:cause-ex-data {:status 502}})))
  (is (logic-events/transient? (ex-info "wrapped" {} (java.net.ConnectException. "egress down"))))
  (is (logic-events/transient? (ex-info "exceeded its time limit" {:sutra/run-limit :timeout})))
  (is (not (logic-events/transient? (ex-info ":use-llm — the model's answer does not fit" {}))))
  (is (not (logic-events/transient? (ex-info "language-model call refused (400): unregistered model"
                                             {:status 400}))))
  (is (not (logic-events/transient? (ex-info "too many" {:sutra/run-limit :edits :status 503})))))

(deftest fingerprints
  (is (= (logic-events/fingerprint :version {:d "1"} {:a 1 :b 2})
         (logic-events/fingerprint :version {:d "1"} {:b 2 :a 1})))
  (is (not= (logic-events/fingerprint :version {:d "1"} {:a 1})
            (logic-events/fingerprint :version {:d "1"} {:a 2})))
  (is (= (logic-events/fingerprint :object {:d "1"} {:a 1})
         (logic-events/fingerprint :object {:d "2"} {:a 2})))
  (is (= [60000 240000 960000] (map logic-events/cool-down-ms [1 2 3])))
  (is (= 86400000 (logic-events/cool-down-ms 30))))

(deftest logic-effect-grammar
  (let [ok? #(m/validate op/EntAutomation {:name "lesen" :on :eingangsdokument :effect %})]
    (is (ok? {:function :beleg-lesen :version "1.0.0" :auto-upgrade true :once :object
              :retry {:attempts 3 :backoff :constant :delay-ms 500 :jitter 0.1}
              :fallback {:action :lesung-fehlgeschlagen :args {:fehler {:failure :message}}}}))
    (is (not (ok? {:function :beleg-lesen :retry {:attempts 9}})))
    (is (not (ok? {:function :beleg-lesen :once :always})))
    (is (not (ok? {:function :beleg-lesen :fallback {:args {}}})))))
