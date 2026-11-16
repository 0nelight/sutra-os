(ns ^:unit platform.writeback-check-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.i18n.catalog :as catalog]
            [sutra.kit.anomaly :as anomaly]
            [sutra.sources.writeback-check :as wc]))

(def ^:private allow
  {:pk "ORDER_NO" :object-type :auftrag :writable-columns ["STATUS" "TERMIN"]
   :insertable-columns ["MISSING"] :column-map {"auf-nr" "ORDER_NO"} :log :none})

(def ^:private columns
  [{:name "ORDER_ID" :type "int"} {:name "ORDER_NO" :type "varchar"}
   {:name "STATUS" :type "varchar"} {:name "TERMIN" :type "datetime"}
   {:name "OPEN_USER" :type "varchar"}])

(defn- rules [fs] (set (map :rule fs)))

(deftest the-findings-name-a-code-and-the-params-to-say-it
  (let [fs (wc/findings {:table "ORDER_HEAD" :allow allow :columns columns
                         :pk-cols ["ORDER_ID"]
                         :triggers [{:name "trg" :def "UPDATE x" :disabled false}
                                    {:name "ok" :def "SET NOCOUNT ON;" :disabled false}]
                         :syncs [{:transform "auftrag-sync"
                                  :query "SELECT ISNULL(a.STATUS,'') FROM ORDER_HEAD a"}]
                         :attributes ["auf-nr" "status"]})]
    (is (= #{:refusal.writeback-check/column-missing
             :refusal.writeback-check/pk-not-primary-key
             :refusal.writeback-check/trigger-row-count
             :refusal.writeback-check/datetime-in-condition
             :refusal.writeback-check/sync-masks-value
             :refusal.writeback-check/log-none
             :refusal.writeback-check/actor-column-unset}
           (rules fs)))
    (is (= [:error] (distinct (map :severity (filter #(= :refusal.writeback-check/column-missing
                                                         (:rule %))
                                                     fs)))))
    (testing "the message is English, the params say it in German"
      (let [f (some #(when (= :refusal.writeback-check/pk-not-primary-key (:rule %)) %) fs)]
        (is (= ":pk ORDER_NO is not the primary key of ORDER_HEAD (ORDER_ID)" (:message f)))
        (is (= ":pk ORDER_NO ist nicht der Primärschlüssel von ORDER_HEAD (ORDER_ID)"
               (catalog/text :de (:rule f) (:params f))))))))

(deftest a-pk-no-attribute-reaches-is-an-error
  (let [fs (wc/findings {:table "T" :allow {:pk "ID" :object-type :ding}
                         :columns [{:name "ID" :type "int"}] :attributes ["name"]})]
    (is (= [:refusal.writeback-check/pk-no-attribute] (map :rule fs)))
    (is (= :error (:severity (first fs))))))

(deftest an-unknown-catalog-checks-nothing-about-the-key
  (is (empty? (wc/findings {:table "T" :allow {:pk "ID"} :columns [{:name "ID" :type "int"}]
                            :pk-cols nil}))))

(deftest the-table-is-found-on-the-allowlist-or-refused
  (let [spec {:writeback {"order_head" {:pk "ORDER_NO"} "bad name" {:pk "X"}}}]
    (is (= ["order_head" {:pk "ORDER_NO"}] (wc/allowlisted :mro spec "ORDER_HEAD")))
    (let [d (try (wc/allowlisted :mro spec "NOPE") (catch Exception e (ex-data e)))]
      (is (= :refusal.writeback-check/table-not-allowlisted (:refusal/code d)))
      (is (= 404 (anomaly/status d))))
    (let [d (try (wc/allowlisted :mro spec "bad name") (catch Exception e (ex-data e)))]
      (is (= :refusal.writeback-check/table-not-plain (:refusal/code d))))))
