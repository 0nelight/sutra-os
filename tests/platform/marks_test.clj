(ns ^:integration platform.marks-test
  (:require [sutra.store.access.scope :as ctx]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.builds.ledger.marks :as marks]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-stores-only)

(deftest a-consumer-acts-once-per-move-and-survives-a-restart
  (let [owner [:test (str (random-uuid))]
        acted (atom 0)
        act   #(do (swap! acted inc) :acted)]
    (is (= ::marks/first (marks/consume! (ctx/store) "main" owner {:v 1} act))
        "the first sight is only the baseline")
    (is (= ::marks/same (marks/consume! (ctx/store) "main" owner {:v 1} act)))
    (is (= :acted (marks/consume! (ctx/store) "main" owner {:v 2} act)))
    (is (= ::marks/same (marks/consume! (ctx/store) "main" owner {:v 2} act))
        "acted once for the move")
    (testing "the mark lives in the database, not in the process: a move seen after a restart still acts"
      (is (marks/moved? (marks/get (ctx/store) "main" owner) {:v 3})))
    (testing "an act that throws leaves the mark, so the move is still pending"
      (is (thrown? clojure.lang.ExceptionInfo
                   (marks/consume! (ctx/store) "main" owner {:v 3}
                                   #(throw (ex-info "build failed" {})))))
      (is (= :acted (marks/consume! (ctx/store) "main" owner {:v 3} act))))
    (is (= 2 @acted))))

(deftest an-advance-keeps-a-mark-written-meanwhile
  (let [owner [:test (str (random-uuid))]]
    (marks/record! (ctx/store) "main" owner {:v 1})
    (let [seen (marks/get (ctx/store) "main" owner)]
      (marks/put! (ctx/store) "main" owner "written")
      (is (false? (marks/advance! (ctx/store) "main" owner seen {:v 2}))
          "a latch set during the fire is not overwritten")
      (is (marks/moved? (marks/get (ctx/store) "main" owner) {:v 2})
          "so the next pass still sees it"))))
