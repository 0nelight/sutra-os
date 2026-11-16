(ns ^:unit apps.lineage-build-status-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.lineage.graph :as lineage]))

(def ^:private t-parse "ri.sutra.main.transform.1111")
(def ^:private t-acars  "ri.sutra.main.transform.2222")
(def ^:private t-multi "ri.sutra.main.transform.3333")
(def ^:private d-raw   "ri.sutra.main.dataset.aaaa")
(def ^:private d-out   "ri.sutra.main.dataset.bbbb")
(def ^:private o-inv   "ri.sutra.main.object-type.cccc")

(def ^:private schema
  {:transforms
   {t-parse {:transform-name :befunde-parsen
             :outputs [{:kind :object-type :object-type o-inv}]}
    t-acars  {:transform-name :acars-abholen
             :outputs [{:kind :dataset :dataset d-raw}]}
    t-multi {:transform-name :maengel-verteilen
             :outputs [{:kind :dataset :dataset d-out}
                       {:kind :object-type :object-type o-inv}]}}
   :datasets    {d-raw {:dataset-name :acars_roh}
                 d-out {:dataset-name :befunde}}
   :object-types {o-inv {:object-type-name :arbeitsauftrag :plural "Arbeitsaufträge"}}})

(defn- job [rid status started ended & {:as extra}]
  (merge {:transform_rid rid :status status
          :started_at started :ended_at ended}
         extra))

(deftest producers-registers-a-multi-output-transform-against-every-output
  (let [p (lineage/producers schema)]
    (is (= #{t-acars} (get p "dataset/acars_roh")))
    (is (= #{t-multi} (get p "dataset/befunde")))
    (testing "a node produced by two transforms lists both"
      (is (= #{t-parse t-multi} (get p "object-type/arbeitsauftrag"))))
    (testing "a node nothing produces is absent, not empty"
      (is (nil? (get p "dataset/nowhere"))))))

(defn- state-of [s] (:build-status (lineage/job->build {:status s})))

(deftest every-job-state-maps-to-its-own-colour-token
  (is (= "succeeded" (state-of "SUCCEEDED")))
  (is (= "failed"    (state-of "FAILED")))
  (is (= "running"   (state-of "RUNNING")))
  (is (= "running"   (state-of "ABORT_PENDING"))
      "an abort that the execution environment has not confirmed is still work in flight")
  (is (= "waiting"   (state-of "WAITING")))
  (is (= "skipped"   (state-of "DID_NOT_RUN")))
  (is (= "aborted"   (state-of "ABORTED")))
  (is (= "never"     (state-of nil)) "no Job row at all")
  (is (= "never"     (state-of "SOMETHING-NEW"))
      "an unknown state is NOT silently read as success"))

(deftest a-row-written-under-the-retired-spelling-still-colours
  (is (= "skipped" (state-of "SKIPPED"))))

(deftest duration-is-measured-from-the-rows-own-timestamps
  (let [done (lineage/job->build (job t-parse "SUCCEEDED"
                                      "2026-08-16T10:00:00Z" "2026-08-16T10:00:12.500Z"))
        live (lineage/job->build (job t-parse "RUNNING" "2026-08-16T10:00:00Z" nil))]
    (is (= 12500 (:duration-ms done)))
    (is (nil? (:duration-ms live)))
    (is (= "2026-08-16T10:00:12.500Z" (:last-build done)))
    (is (= "2026-08-16T10:00:00Z" (:last-build live))
        "a running job's last-build falls back to when it STARTED — it has no end yet")))

(deftest an-unparseable-timestamp-costs-a-duration-not-the-read
  (let [b (lineage/job->build (job t-parse "SUCCEEDED" "not-a-time" "also-not"))]
    (is (nil? (:duration-ms b)))
    (is (= "succeeded" (:build-status b)))))

(deftest the-log-TEXT-never-rides-in-the-status-payload
  (let [b (lineage/job->build (job t-parse "SUCCEEDED" "2026-08-16T10:00:00Z"
                                   "2026-08-16T10:00:01Z"
                                   :job_log "skipped 40 rows: no PDF"))]
    (is (true? (:log? b)))
    (is (nil? (:job-log b)))
    (is (nil? (:log b))))
  (is (false? (:log? (lineage/job->build (job t-parse "SUCCEEDED" nil nil))))))

(deftest a-transaction-carries-the-size-and-the-age
  (let [d (lineage/txn->data {:type "SNAPSHOT" :row_count 1415 :row_delta 12
                              :created "2026-01-04T08:00:00Z" :txns 87
                              :ended_at "2026-08-16T09:00:00Z"
                              :snapshot_id "42"})]
    (is (= 1415 (:rows d)))
    (is (= "2026-01-04T08:00:00Z" (:created-at d)))
    (is (= 87 (:transactions d)))
    (is (= "SNAPSHOT" (:last-write d)))
    (is (= "42" (:commit d)))))

(deftest a-node-wears-its-own-producers-job-not-the-branchs-newest
  (let [jobs {t-acars  (job t-acars "FAILED" "2026-08-16T10:00:00Z" "2026-08-16T10:00:05Z"
                           :error "acars: connection refused")
              t-multi (job t-multi "SUCCEEDED" "2026-08-16T11:00:00Z" "2026-08-16T11:00:30Z"
                           :rows_out 200)}
        idx  (lineage/build-index schema jobs {})]
    (is (= "failed" (:build-status (get idx "dataset/acars_roh"))))
    (is (= "acars: connection refused" (:error (get idx "dataset/acars_roh"))))
    (is (= "succeeded" (:build-status (get idx "dataset/befunde"))))
    (is (= 200 (:rows (get idx "dataset/befunde"))))
    (testing "the transform nodes themselves carry their own Job"
      (is (= "failed" (:build-status (get idx "transform/acars-abholen"))))
      (is (= "succeeded" (:build-status (get idx "transform/maengel-verteilen")))))))

(deftest a-node-with-several-producers-takes-the-newest-of-their-jobs
  (let [jobs {t-parse (job t-parse "FAILED" "2026-08-16T09:00:00Z" "2026-08-16T09:00:10Z")
              t-multi (job t-multi "SUCCEEDED" "2026-08-16T11:00:00Z" "2026-08-16T11:00:30Z")}
        idx  (lineage/build-index schema jobs {})]
    (is (= "succeeded" (:build-status (get idx "object-type/arbeitsauftrag"))))
    (testing "…and the other way round when the older one is the green one"
      (let [flipped {t-parse (job t-parse "SUCCEEDED" "2026-08-16T09:00:00Z" "2026-08-16T09:00:10Z")
                     t-multi (job t-multi "FAILED" "2026-08-16T11:00:00Z" "2026-08-16T11:00:30Z")}]
        (is (= "failed" (:build-status (get (lineage/build-index schema flipped {})
                                            "object-type/arbeitsauftrag"))))))))

(deftest a-datasets-transaction-supplies-rows-but-never-a-status
  (let [jobs {t-acars (job t-acars "FAILED" "2026-08-16T10:00:00Z" "2026-08-16T10:00:05Z")}
        txns {d-raw {:dataset_rid d-raw :type "APPEND" :row_count 900
                     :created "2026-01-04T08:00:00Z" :txns 12
                     :started_at "2026-08-09T10:00:00Z" :ended_at "2026-08-09T10:00:02Z"}}
        node (get (lineage/build-index schema jobs txns) "dataset/acars_roh")]
    (is (= "failed" (:build-status node)) "the producer's verdict, not the rows' age")
    (is (= 900 (:rows node)) "…while the row count still comes from the transaction")
    (is (= "2026-01-04T08:00:00Z" (:created-at node)))))

(deftest a-node-nothing-has-ever-built-is-ABSENT-from-the-index
  (let [idx (lineage/build-index schema {} {})]
    (is (empty? idx))
    (is (nil? (get idx "object-type/arbeitsauftrag")))))

(deftest model-usage-reaches-the-object-type-its-producer-fills
  (let [usage {"transform/befunde-parsen"
               {:attempted 10 :successful 8 :rate-limited 2 :failed 0
                :tokens 1000 :cost 0.5 :currency "USD" :models ["grok-4"]}
               "transform/maengel-verteilen"
               {:attempted 5 :successful 5 :rate-limited 0 :failed 0
                :tokens 500 :cost nil :currency "USD" :models ["grok-3"]}}
        got (lineage/spread-usage schema usage)]
    (testing "a transform keeps its own figure verbatim"
      (is (= 10 (:attempted (get got "transform/befunde-parsen")))))
    (testing "a type with TWO producers sums them"
      (let [u (get got "object-type/arbeitsauftrag")]
        (is (= 15 (:attempted u)))
        (is (= 13 (:successful u)))
        (is (= 2 (:rate-limited u)))
        (is (= 1500 (:tokens u)))
        (is (= ["grok-3" "grok-4"] (:models u)))
        (is (= 0.5 (:cost u)) "the priced half sums; the unpriced half is not counted as free")))
    (testing "a node whose producers made no calls is ABSENT — not zero"
      (is (nil? (get got "dataset/acars_roh"))))))

(deftest a-dataset-with-transactions-but-no-producer-job-still-reports-its-rows
  (let [txns {d-out {:dataset_rid d-out :type "SNAPSHOT" :row_count 5
                     :created "2026-02-01T00:00:00Z" :txns 1
                     :started_at "2026-02-01T00:00:00Z" :ended_at "2026-02-01T00:00:01Z"}}
        node (get (lineage/build-index schema {} txns) "dataset/befunde")]
    (is (= 5 (:rows node)))
    (is (nil? (:build-status node)) "no Job ⇒ no status; the caller renders `never`")))
