(ns ^:unit ontology.build-retry-test
  (:require [sutra.builds.plan.producers :as producers]
            [sutra.builds.ledger.jobs :as jl]
            [sutra.builds.plan.freshness :as fresh]
            [sutra.builds.plan.dag :as dag]
            [clojure.test :refer [deftest is testing]]
            [sutra.store.access.scope :as ctx]
            [sutra.store.seat :as seat]
            [sutra.builds.api :as builds]
            [sutra.builds.import :as importer]
            [sutra.builds.import.sources :as import-sources]
            [sutra.builds.ledger :as ledger]
            [sutra.schedules.dispatch :as dispatch]
            [sutra.schedules.schedules :as schedules]
            [sutra.store.control.cadence :as cadence]
            [sutra.types.duration :as dur]))

(def ^:private schema
  {:object-types {"ri.email" {:object-type-name :email :plural "Emails"}
                  "ri.pos"   {:object-type-name :pos :plural "Positionen"}
                  "ri.other" {:object-type-name :other :plural "Others"}}
   :transforms
   {:pull  {:transform-name :pull
            :input  {:kind :datasource :datasource :imap}
            :output {:kind :object-type :object-type "ri.email"}}
    :parse {:transform-name :parse :language :python
            :inputs {:mail {:kind :object-type :object-type "ri.email"}}
            :output {:kind :object-type :object-type "ri.pos"}}
    :other {:transform-name :other
            :input  {:kind :datasource :datasource :erp}
            :output {:kind :object-type :object-type "ri.other"}}}})

(def ^:private run-id #uuid "00000000-0000-0000-0000-000000000001")

(defn- rid [t] (ledger/job-rid run-id t))

(defn- node [t run] {:kind :producer :key t :t-kw t :spec {} :run run})

(defn- ok-run [_ _] {:facts []})

(defn- flaky [n e]
  (let [calls (atom 0)]
    (fn [_ _] (if (<= (swap! calls inc) n) (throw e) {:facts []}))))

(def ^:private transient-fault
  (ex-info "transform worker unreachable" {:cause (java.net.ConnectException. "refused")}))

(def ^:private code-fault
  (ex-info "transform worker refused: TypeError" {:status 400}))

(defn- fresh-ledger []
  (atom (into {} (for [t (keys (:transforms schema))]
                   [(rid t) {:status "WAITING" :attempt 1 :t t}]))))

(defn- record-effect!
  [jobs {:keys [op state facts job-rids]}]
  (let [put! (fn [jr k v] (swap! jobs update jr assoc k v))]
    (case [op state]
      [:job/transition :RUNNING]   (do (put! (:job-rid facts) :status "RUNNING")
                                       (put! (:job-rid facts) :attempt (:attempt facts)))
      [:job/transition :COMPLETED] (put! (:job-rid facts) :status "COMPLETED")
      [:job/transition :FAILED]    (put! (:job-rid facts) :status "FAILED")
      [:job/sweep :ABORTED]        (doseq [[jr {:keys [status]}] @jobs
                                           :when (and (#{"WAITING" "RUNNING"} status)
                                                      (or (nil? job-rids) (some #{jr} job-rids)))]
                                     (put! jr :status "ABORTED"))
      nil)))

(defn- build!
  ([options nodes] (build! options nodes (fresh-ledger)))
  ([options nodes jobs]
   (let [runs    (atom [])
         record! (fn [effects] (doseq [fx effects] (record-effect! jobs fx)))]
     (with-redefs [fresh/freshness (fn [& _] {:runs? (constantly true)})
                   producers/producer-job (fn [env acc node] ((:run node) env acc))
                   jl/ledger!   (fn [_ effects] (record! effects))
                   ledger/snapshot-requests (fn [_ _] #{})
                   dag/with-job! (fn [_ noted t attempt run]
                                        (record!
                                         [(ledger/open-job {:job-rid (rid t) :attempt attempt})])
                                        (swap! runs conj [t attempt])
                                        (try (let [r (run nil)]
                                               (record! [(ledger/finish-job {:job-rid (rid t)})])
                                               {:r r :noted noted})
                                             (catch Throwable e {:e e :noted noted})))]
       (let [result
               (try {:build-state (#'dag/run-dag! {:schema schema :datasources {} :run-id run-id
                                               :options (importer/build-options options)}
                                              nodes {})}
                    (catch clojure.lang.ExceptionInfo e {:thrown e}))]
         (assoc result
                :jobs (into {} (for [[_ j] @jobs] [(:t j) (select-keys j [:status :attempt])]))
                :runs @runs))))))

(deftest a-transient-failure-is-retried-in-place-until-it-succeeds
  (let [{:keys [build-state jobs runs]}
        (build! {:retry-count 2} [(node :pull (flaky 2 transient-fault)) (node :parse ok-run)])]
    (is (empty? (:failures build-state)))
    (is (= {:status "COMPLETED" :attempt 3} (:pull jobs)))
    (is (= [[:pull 1] [:pull 2] [:pull 3] [:parse 1]] (filterv (comp #{:pull :parse} first) runs))
        "the downstream job ran once, after the third attempt's verdict")
    (is (= {:status "COMPLETED" :attempt 1} (:parse jobs)))))

(deftest downstream-stays-waiting-while-its-parent-retries
  (let [ledger (fresh-ledger)
        seen   (atom [])
        pull   (flaky 1 transient-fault)]
    (build! {:retry-count 1}
            [(node :pull (fn [env acc]
                           (swap! seen conj [(get-in @ledger [(rid :pull) :attempt])
                                             (get-in @ledger [(rid :parse) :status])])
                           (pull env acc)))
             (node :parse ok-run)]
            ledger)
    (is (= [[1 "WAITING"] [2 "WAITING"]] @seen))
    (is (= "COMPLETED" (get-in @ledger [(rid :parse) :status])))))

(deftest without-retries-a-failure-is-final-at-attempt-1-and-its-dependents-abort
  (let [{:keys [build-state jobs]} (build! {:retry-count 0}
                                  [(node :pull (flaky 1 transient-fault))
                                   (node :parse ok-run)
                                   (node :other ok-run)])]
    (is (= {:status "FAILED" :attempt 1} (:pull jobs)))
    (is (= "ABORTED" (:status (:parse jobs))) "the dependent never runs")
    (is (= "COMPLETED" (:status (:other jobs))) "an independent branch keeps running")
    (is (= [:pull] (mapv :job (:failures build-state))))))

(deftest a-deterministic-failure-is-final-even-with-retries-left
  (let [{:keys [jobs runs]} (build! {:retry-count 3} [(node :pull (flaky 5 code-fault))
                                                      (node :parse ok-run)])]
    (is (= {:status "FAILED" :attempt 1} (:pull jobs)))
    (is (= [[:pull 1]] runs))
    (is (= "ABORTED" (:status (:parse jobs))))))

(deftest retries-run-out-and-the-last-attempt-is-the-verdict
  (let [{:keys [jobs runs]} (build! {:retry-count 2} [(node :pull (flaky 9 transient-fault))])]
    (is (= {:status "FAILED" :attempt 3} (:pull jobs)))
    (is (= [[:pull 1] [:pull 2] [:pull 3]] runs))))

(deftest abort-on-failure-cancels-every-other-job
  (let [{:keys [thrown jobs runs]} (build! {:abort-on-failure true}
                                           [(node :pull (flaky 1 code-fault))
                                            (node :other ok-run)
                                            (node :parse ok-run)])]
    (is (some? thrown))
    (is (:aborted-non-dependents (ex-data thrown)))
    (is (= "FAILED" (:status (:pull jobs))))
    (is (= "ABORTED" (:status (:other jobs))) "the independent sibling is cancelled")
    (is (= "ABORTED" (:status (:parse jobs))))
    (is (not-any? (comp #{:other :parse} first) runs))))

(deftest backoff-waits-between-attempts
  (let [t0 (System/nanoTime)
        {:keys [jobs]} (build! {:retry-count 2 :retry-backoff "60ms"}
                               [(node :pull (flaky 2 transient-fault))])]
    (is (= "COMPLETED" (:status (:pull jobs))))
    (is (<= 120 (/ (- (System/nanoTime) t0) 1e6)))))

(deftest which-failures-a-retry-can-change
  (is (dag/retryable-failure? transient-fault))
  (is (dag/retryable-failure? (ex-info "x" {:status 503})))
  (is (dag/retryable-failure? (ex-info "x" {} (java.sql.SQLException. "serialization" "40001"))))
  (is (dag/retryable-failure? (ex-info "x" {} (java.sql.SQLTransientConnectionException. "pool"))))
  (is (not (dag/retryable-failure? code-fault)))
  (is (not (dag/retryable-failure? (ex-info "x" {} (java.sql.SQLException. "no table" "42P01")))))
  (is (not (dag/retryable-failure? (ex-info "restricted" {:violations [{}]}))))
  (is (not (dag/retryable-failure? (ArithmeticException. "/ by zero")))))

(deftest build-options-follow-the-documented-defaults
  (is (= {:retry-count 0 :retry-backoff-ms 0 :abort-on-failure false}
         (select-keys (importer/build-options nil)
                      [:retry-count :retry-backoff-ms :abort-on-failure])))
  (is (= 4 (:retry-count (importer/build-options {:retry-count 9}))) "capped at five attempts")
  (is (= 90000 (:retry-backoff-ms (importer/build-options {:retry-backoff "90s"}))))
  (is (= 30000
         (:retry-backoff-ms (importer/build-options {:retry-backoff {:unit "SECONDS" :value 30}}))))
  (is (thrown? clojure.lang.ExceptionInfo (importer/build-options {:retry-count -1})))
  (is (thrown? clojure.lang.ExceptionInfo (importer/build-options {:retry-backoff "soon"})))
  (is (= "2m" (dur/render 120000))))

(deftest a-schedule-carries-the-options-to-every-build-it-fires
  (let [stored (atom nil)
        fired  (atom nil)]
    (with-redefs [cadence/upsert!           (fn [_ _ _ row] (reset! stored row) row)
                  ctx/store                    (fn [& _] {})
                  seat/branch-schema           (fn [_] schema)
                  import-sources/read-spec  (fn [_] {:datasources {} :import {}})
                  builds/build!
                    (fn [_ _ preset _] (reset! fired preset) {:run-id "r"})
                  schedules/mark-fired!     (fn [& _] nil)]
      (let [s (schedules/upsert! nil "main"
                                 {:name "mail" :target "pull" :cron "*/5 * * * *"
                                  :retry-count 3 :retry-backoff "1m" :abort-on-failure true})]
        (is (= {:retry-count 3 :retry-backoff-ms 60000 :abort-on-failure true}
               (select-keys s [:retry-count :retry-backoff-ms :abort-on-failure])))
        (dispatch/fire! nil "main" s)
        (is (= {:retry-count 3 :retry-backoff-ms 60000 :abort-on-failure true}
               (select-keys @fired [:retry-count :retry-backoff-ms :abort-on-failure])))
        (is (= 60000 (:retry-backoff-ms (importer/build-options @fired))))))
    (testing ""
      (with-redefs [cadence/upsert! (fn [_ _ _ row] row)]
        (is (= {:retry-count 0 :retry-backoff-ms 0 :abort-on-failure false}
               (select-keys (schedules/upsert! nil "main" {:name "x" :target "pull" :paused true})
                            [:retry-count :retry-backoff-ms :abort-on-failure])))))))

