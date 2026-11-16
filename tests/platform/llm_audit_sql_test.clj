(ns ^:integration platform.llm-audit-sql-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.llm.cost :as cost-log]
            [sutra.store.access.datasets :as dsets]
            [sutra.store.access.scope :as ctx]
            [sutra.catalog.lookup :as lookup]
            [sutra.store.seat :as seat]
            [support.pod :as pod]))

(use-fixtures :once pod/with-stores-only)

(def ^:private calls
  [{:ts "2026-10-01T08:00:00Z" :caller "parse_invoices" :model "grok-4" :error nil}
   {:ts "2026-10-02T08:00:00Z" :caller "parse_receipts" :model "grok-4" :error "boom"}
   {:ts "2026-10-03T08:00:00Z" :caller "x" :model "grok-4" :error "unknown source"}
   {:ts "2026-10-04T08:00:00Z" :caller "z" :model "grok-imaginary-9" :error "unregistered"}
   {:ts "2026-10-05T08:00:00Z" :caller "parse_invoices" :model "grok-4" :error ""}])

(defn- logged! [store schema]
  (let [rid (lookup/rid-of schema :datasets :llm_call_log)]
    (dsets/ensure-datasets! store {rid (get-in schema [:datasets rid :columns])})
    (dsets/write-rows! store rid :append
                       (map-indexed (fn [i c]
                                      {:id (str "r" i)
                                       :values (assoc c :request_id (str "r" i)
                                                      :prompt_tokens 1 :completion_tokens 1
                                                      :latency_ms 1)})
                                    calls))))

(deftest audit-is-answered-by-the-log-store
  (let [store  (ctx/at (ctx/system-store) "main")
        schema (seat/branch-schema (ctx/system-store "main"))
        _      (logged! store schema)
        audit  #(cost-log/audit store schema %)
        all    (audit {})]
    (testing "counts are over every match, rows newest first and capped"
      (is (= [5 5 5 3] ((juxt :logged :calls :shown :errors) all)) "an empty error is no error")
      (is (= (map :ts (reverse calls)) (map :ts (:rows all))))
      (is (= "parse_invoices" (:resource (first (:rows all)))))
      (is (= [5 2] ((juxt :calls :shown) (audit {:limit 2})))))
    (testing "filters"
      (is (= [3 3] ((juxt :calls :errors) (audit {:errors? true}))))
      (is (= ["parse_receipts"] (map :caller (:rows (audit {:caller "RECEIPTS"})))))
      (is (= ["z"] (map :caller (:rows (audit {:model "Imaginary"})))))
      (is (= [0 0 5] ((juxt :calls :shown :logged) (audit {:caller "nobody-like-this"}))))
      (is (= ["2026-10-03T08:00:00Z" "2026-10-02T08:00:00Z"]
             (map :ts (:rows (audit {:since "2026-10-02" :until "2026-10-03"}))))))
    (testing "the filters agree with reading every row"
      (is (every? #(str/includes? (:model %) "grok") (:rows (audit {:model "grok"})))))))
