(ns ^:integration platform.staged-row-rewrite-pg-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [sutra.actions.staged-rows :as staged]
            [sutra.store.access.scope :as ctx]
            [sutra.store.control :as ctl]
            [support.pod :as pod]))

(use-fixtures :once pod/with-stores-only)

(deftest a-staged-row-takes-its-delta-afterwards
  (let [store (ctx/store "main")
        b     (str "rewrite-" (random-uuid))
        s     (staged/stage! store b :a "1")]
    (try
      (ctl/update! store :staged-actions {:branch b :seq s} {:delta {:k 1}})
      (is (= [[s {:k 1}]] (map (juxt :seq :delta) (staged/list-staged store b))))
      (finally (ctl/delete! store :staged-actions {:branch b :seq s})))))
