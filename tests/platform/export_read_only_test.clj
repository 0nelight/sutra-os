(ns ^:api platform.export-read-only-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [sutra.authoring.projections :as projections]
            [sutra.catalog.world-export :as world-export]
            [sutra.store.access.scope :as scope]
            [sutra.store.kept :as kept]
            [sutra.store.read-only :as read-only]
            [sutra.store.seat :as seat]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-pod-auth)

(use-fixtures :each pod/as-admin)

(deftest a-pull-through-a-store-that-only-reads-renders-from-the-held-export
  (let [ro    (read-only/of (scope/store "main"))
        ctx   {:store ro :schema (seat/branch-schema ro) :branch "main"}
        held  world-export/held
        kept  kept/export
        calls (atom 0)
        reads (atom 0)]
    (with-redefs [world-export/held (fn [d] (swap! calls inc) (held d))
                  kept/export      (fn [s b] (swap! reads inc) (kept s b))]
      (let [whole  (projections/pull ctx "ontologie" nil false)
            rehome (projections/pull ctx "ontologie" nil true)]
        (is (seq (:files whole)) "a whole pull renders")
        (is (seq (:files rehome)) "a --rehome pull renders")
        (is (= 2 @reads) "both read the held export")
        (is (<= @calls 1) "the second pull reads the export the first one held")))))
