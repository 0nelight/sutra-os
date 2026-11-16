(ns ^:integration platform.retention-expiry-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [sutra.datasets.api :as datasets]
            [sutra.store.access.datasets :as dsets]
            [sutra.store.access.scope :as ctx]
            [sutra.types.naming :as naming]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-stores-only)

(def ^:private D "ri.sutra.main.dataset.55555555-5555-5555-5555-555555555555")

(defn- write! [store k]
  (dsets/write-rows! (ctx/as-system store) D :append [{:id k :values {:k k}}]))

(deftest an-expiry-is-looked-past-a-deletion-is-not
  (let [store (ctx/store)
        t     (naming/dataset-table D)
        _     (write! store "a")
        low   (dsets/current-version store)
        _     (write! store "b")
        _     (dsets/expire! store D low)
        high  (dsets/current-version store)]
    (is (false? (dsets/only-added? store t low high)) "to an ordinary consumer rows went")
    (is (true? (dsets/only-added-past-retention? store t low high))
        "to an allow_retention one nothing happened but b")
    (dsets/delete-rows! store D ["b"])
    (is (false? (dsets/only-added-past-retention? store t low (dsets/current-version store)))
        "and a real deletion still ends the increment")))
