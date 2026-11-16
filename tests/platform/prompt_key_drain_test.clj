(ns ^:api platform.prompt-key-drain-test
  (:require [support.wire :refer [ok?]]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.java.io :as io]
            [sutra.wire.api :as api]
            [sutra.compute.sandbox :as sandbox]
            [sutra.builds.ledger :as ledger]
            [sutra.store.access.scope :as ctx]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-pod)

(defn- py [f] (slurp (io/resource (str "support/fixtures/python/" f))))
(defn- col [n t] {:attribute-name n :attribute-type t})

(def ^:private hash-Alpha "ee428e506fc4e2c28064aa3b1dc4f14d")
(def ^:private hash-Aleph "b8793a98bdad51284dc025e15f0a5aa6")

(defn- schema []
  {:schema
   {:transforms
    {:mk-widget
     {:project :pk :language :python :code (py "gated_prompt_key.py")
      :incremental {:read-mode :current :write-mode :modify}
      :semantic-version 1
      :done-key {:input :seed :key :tid}
      :max-per-run 2
      :prompt-key {:input :seed :columns [:label] :column :prompt-hash}
      :inputs {:seed {:kind :dataset :dataset :seed_ai}}
      :primary-key :pk
      :output {:kind :dataset :dataset :widget_out}}}}})

(defn- dataset-rid [nm]
  (->> (:datasets (:schema (:body (api/get! :snapshot))))
       (some (fn [[rid d]] (when (= nm (:dataset-name d)) rid)))))

(defn- out-rows
  []
  (mapv :values (:rows (:body (api/get! :dataset-preview {:id (dataset-rid :widget_out)})))))

(defn- row-of [pk] (first (filter #(= pk (:pk %)) (out-rows))))

(defn- observed [import-response]
  (->> (:jobs (:body (api/get! :run-jobs {:id (get-in import-response [:body :run-id])})))
       (keep :pending_observed)
       first))

(deftest the-gate-is-measured-and-the-prompt-key-re-asks
  (if-not (sandbox/available?)
    (println "  (prompt-key-drain SKIPPED — python sandbox unavailable)")
    (do
      (testing "install: 4 seed rows, of which the body's gate lets 2 through"
        (is (ok? (api/mutate :create-project {:name :pk} {:stage? true})))
        (is (ok? (api/mutate :create-dataset
                             {:name :seed_ai :project :pk
                              :columns [(col :tid :string) (col :label :string)]}
                             {:stage? true})))
        (is (ok? (api/mutate :create-dataset
                             {:name :widget_out :project :pk
                              :columns [(col :pk :string) (col :label :string)
                           (col :prompt_hash :string)]}
                             {:stage? true})))
        (is (ok? (api/post! :staging)) "commit datasets")
        (is (ok? (api/post! :install (schema))) "install stages the transform")
        (is (ok? (api/post! :staging)) "commit schema")
        (is (ok? (api/post! :dataset-write {:name "seed_ai"}
                            {:rows [{:values {:tid "1" :label "Alpha"}}
{:values {:tid "2" :label "beta"}}
{:values {:tid "3" :label "Aleph"}}
{:values {:tid "4" :label "gamma"}}]
:mode :append :message "seed"}))))

      (testing "build 1 — the gate decides the work, and the body REPORTS how much that was"
        (let [r (api/post! :import-run)]
          (is (ok? r) "import 1")
          (is (= 2 (observed r))
              "2 of 4, measured past the gate — the anti-join alone would have said 4")
          (is (= 2 (count (out-rows))) "…and exactly those 2 landed")))

      (testing "each output row carries the prompt hash the platform will anti-join on"
        (is (= hash-Alpha (:prompt_hash (row-of "1")))
            "python's md5 over (semantic_version, label) — pinned, because SQL must match it")
        (is (= hash-Aleph (:prompt_hash (row-of "3")))))

      (testing "build 2 — nothing changed, so nothing is asked again"
        (let [r (api/post! :import-run)]
          (is (ok? r) "import 2")
          (is (= 0 (observed r)) "measured ZERO: an answer, not a missing measurement")
          (is (= 2 (count (out-rows))))))

      (testing "the PROMPT INPUT changes ⇒ that row is work again (done_key could not see this)"
        (is (ok? (api/post! :dataset-write {:name "seed_ai"}
                            {:rows [{:values {:tid "1" :label "Alpha II"}}
{:values {:tid "2" :label "beta"}}
{:values {:tid "3" :label "Aleph"}}
{:values {:tid "4" :label "gamma"}}]
:mode :snapshot :message "corrected label"})))
        (let [r (api/post! :import-run)]
          (is (ok? r) "import 3")
          (is (= 1 (observed r)) "exactly one question is open again — the one that changed")
          (is (= "Alpha II" (:label (row-of "1"))) "row 1 was answered again, with the new input")
          (is (not= hash-Alpha (:prompt_hash (row-of "1")))
              "…and carries the hash of the NEW question, not the old one")
          (is (= hash-Aleph (:prompt_hash (row-of "3")))
              "the row whose prompt did NOT change was not re-asked")))

      (testing "and the estimate can READ that measurement back — the half a green build hides"
        (let [m (ledger/last-observed-pending (ctx/store "main") "main")]
          (is (= 1 (count m)) "one transform has filed a measurement")
          (is (= 1 (:pending (first (vals m))))
              "…and it is the LAST one, not the first: 2 → 0 → 1")
          (is (string? (:run (first (vals m)))) "carrying the build that measured it"))))))
