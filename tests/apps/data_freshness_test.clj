(ns ^:unit apps.data-freshness-test
  (:require [clojure.test :refer [deftest is testing]]
            [malli.core :as m]
            [sutra.builds.last-updated :as lu]
            [sutra.builds.ledger :as ledger]
            [sutra.capps.ctx :as cctx]
            [sutra.capps.format :as fmt]
            [sutra.catalog.grants :as grants]
            [sutra.parser.ent-view :as ent-view]
            [sutra.parser.lint :as lint]
            [sutra.parser.spec :as spec]
            [sutra.projects.api :as projects]
            [sutra.store.access.scope :as ctx]))

(def ^:private now 1752760320000)

(defn- shown
  [ago-ms locale]
  (fmt/temporal-text (str (java.time.Instant/ofEpochMilli (- now ago-ms)))
                     {:style :relative :brief? true :timezone "UTC"}
                     (assoc (cctx/of {:locale locale :zone "UTC"}) :now now)))

(deftest last-updated-reads-as-counterpart-documents-it
  (is (= "30 min ago" (shown (* 30 60000) "en")))
  (is (= "2 hours ago" (shown (* 2 3600000) "en")))
  (is (= "vor 30 Min." (shown (* 30 60000) "de")))
  (testing "past 24 hours it turns absolute"
    (is (= "Tue, Jul 15, 2025, 1:52 PM" (shown (* 2 86400000) "en")))))

(def ^:private schema
  {:object-types {"ri.ot.1" {:object-type-name :rechnung}
                  "ri.ot.2" {:object-type-name :geheim}}
   :datasets     {"ri.ds.1" {:dataset-name :rechnung_raw}
                  "ri.ds.2" {:dataset-name :ohne_txn}}
   :transforms   {"ri.tf.1" {:transform-name :rechnung-bau
                             :output {:dataset "ri.ds.2"}}}})

(defn- updates
  [branch auth items]
  (with-redefs [projects/index-times
                (fn [s] (if (= "main" (:branch s))
                          {"ri.ot.1" "2025-07-17T10:00:00Z"}
                          {}))
                ledger/last-txn-per-dataset
                (fn [_ b] (when (= "main" b)
                            {"ri.ds.1" {:ended_at "2025-07-17T09:00:00Z"}}))
                ledger/last-jobs-per-transform
                (fn [_ b] (if (= "main" b)
                            {:ok {"ri.tf.1" {:ended-at "2025-07-17T08:00:00Z"}}}
                            {:ok {"ri.tf.1" {:ended-at "2025-07-18T08:00:00Z"}}}))
                ctx/at (fn [s b] (assoc s :branch b))]
    (into {} (map (juxt (juxt :kind :name) :at))
          (lu/of-items {:branch branch} schema auth items))))

(def ^:private items
  [{:object-type :rechnung
    :sources [{:resource :rechnung_raw :override-name "SAP"}
              {:resource :rechnung-bau}
              {:resource :ohne_txn}]}
   {:object-type :geheim}])

(deftest each-node-answers-with-its-own-time
  (let [u (updates "main" nil items)]
    (is (= "2025-07-17T10:00:00Z" (get u ["object-type" "rechnung"])))
    (is (= "2025-07-17T09:00:00Z" (get u ["source" "rechnung_raw"])))
    (is (= "2025-07-17T08:00:00Z" (get u ["source" "rechnung-bau"])))
    (is (= "2025-07-17T08:00:00Z" (get u ["source" "ohne_txn"]))
        "a dataset without a transaction of its own is dated by its producer")
    (is (contains? u ["object-type" "geheim"]) "known but never indexed answers nil")
    (is (nil? (get u ["object-type" "geheim"])))))

(deftest a-rename-keeps-the-time
  (let [renamed (assoc-in schema [:object-types "ri.ot.1" :object-type-name] :beleg)]
    (with-redefs [projects/index-times        (fn [_] {"ri.ot.1" "2025-07-17T10:00:00Z"})
                  ledger/last-txn-per-dataset (fn [_ _] nil)
                  ledger/last-jobs-per-transform (fn [_ _] nil)
                  ctx/at                      (fn [s b] (assoc s :branch b))]
      (is (= [{:kind "object-type" :name "beleg" :at "2025-07-17T10:00:00Z"}]
             (lu/of-items {:branch "main"} renamed nil [{:object-type :beleg}]))))))

(deftest a-branch-reads-through-what-it-has-not-built
  (let [u (updates "fork" nil items)]
    (is (= "2025-07-17T10:00:00Z" (get u ["object-type" "rechnung"])))
    (is (= "2025-07-18T08:00:00Z" (get u ["source" "rechnung-bau"])))))

(deftest what-auth-may-not-see-is-left-out
  (with-redefs [grants/hidden-object-types (fn [_] #{"ri.ot.2"})
                grants/blocked-resources   (fn [_] #{"ri.ds.1"})]
    (let [u (updates "main" :someone items)]
      (is (not (contains? u ["object-type" "geheim"])))
      (is (not (contains? u ["source" "rechnung_raw"])))
      (is (contains? u ["source" "rechnung-bau"])))))

(deftest config-is-validated
  (let [valid? (fn [its] (m/validate @@#'ent-view/Widget* {:widget :data-freshness :items its}))]
    (is (valid? [{:object-type :rechnung
                  :sources [{:resource :rechnung_raw :override-name "SAP"}]}]))
    (is (not (valid? [{:sources []}])) "an item names its object type")
    (is (not (valid? [{:object-type :rechnung :sources [{:override-name "SAP"}]}]))
        "a source names its resource")))

(deftest a-name-that-resolves-to-nothing-is-a-finding
  (let [app   (fn [its] {"ri.app" {:name :a :layout {:widget :data-freshness :items its}}})
        found (fn [its] (filter #(= :data-freshness-names-nothing (:rule %))
                                (lint/run-lint (assoc schema :applications (app its))
                                               (spec/vocabulary))))]
    (is (empty? (found [{:object-type :rechnung :sources [{:resource :rechnung-bau}]}])))
    (is (= 2 (count (found [{:object-type :weg :sources [{:resource :auch_weg}]}]))))))
