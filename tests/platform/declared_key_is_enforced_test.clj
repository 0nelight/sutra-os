(ns ^:integration platform.declared-key-is-enforced-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.store.access.datasets :as dsets]
            [sutra.store.access.scope :as ctx]
            [sutra.types.naming :as naming]
            [sutra.foundation.encoding :as enc]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-stores-only)

(def ^:private d :key-enforced)

(defn- installed! []
  (dsets/ensure-tables! (ctx/store)
                        {(naming/dataset-table d) [{:attribute-name :k :attribute-type :string}
                                                {:attribute-name :v :attribute-type :string}]}))

(defn- landen! [rows]
  (installed!)
  (dsets/land-rows! (ctx/as-system (ctx/store)) d :snapshot #(enc/pk-uuid d (get % "k")) rows))

(deftest ein-doppelter-deklarierter-schluessel-kippt-den-job
  (testing "zwei Zeilen unter demselben Schluessel werden abgewiesen"
    (let [e (is (thrown? clojure.lang.ExceptionInfo
                         (landen! [{"k" "a" "v" "erste"}
                                   {"k" "a" "v" "zweite"}
                                   {"k" "b" "v" "dritte"}])))]
      (is (= :duplicate-primary-key (:error (ex-data e))))
      (is (= 1 (:duplicates (ex-data e))))
      (is (= {"k" "a" "v" "erste"} (:sample (ex-data e)))
          "die Meldung zeigt eine der kollidierenden Zeilen — sonst sucht man sie
           in Zehntausenden")))
  (testing "eindeutige Schluessel gehen durch"
    (is (= {:written 2 :deleted 0}
           (landen! [{"k" "a" "v" "eine"} {"k" "b" "v" "andere"}]))))
  (testing "ohne deklarierten Schluessel greift nichts — jede Zeile ist fuer sich"
    (is (= {:written 2 :deleted 0}
           (do (installed!)
               (dsets/land-rows! (ctx/as-system (ctx/store)) d :snapshot nil
                              [{"k" "a" "v" "eine"} {"k" "a" "v" "andere"}]))))))
