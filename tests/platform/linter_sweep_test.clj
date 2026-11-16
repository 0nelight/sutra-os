(ns ^:unit platform.linter-sweep-test
  (:require [clojure.test :refer [deftest is]]
            [sutra.linter.sweep :as sweep]
            [sutra.schedules.api :as schedules]
            [sutra.store.access.scope :as ctx]
            [support.mem :as mem]))

(defn- answers [] {:jobs {} :schedules [] :automations [] :txns {} :placements {}
                   :status (delay {}) :runs (delay {}) :jobspecs (delay nil)
                   :now (System/currentTimeMillis)})

(def ^:private t {:transform-name :a :language :pg-sql :query "SELECT 1" :inputs {}
                  :output {:kind :dataset :dataset "ri.d"}})

(def ^:private schema {:datasets {"ri.d" {:dataset-name :d :kind :standalone}}
                       :transforms {"ri.t" t}})

(deftest a-sweep-keeps-a-recommendation-and-a-snooze-survives-the-next
  (let [store (ctx/at (mem/store) "main")
        rule  "transform-never-built"
        one   {:rule rule :resource "transform/a"}
        open  #(map :rule (:recommendations (sweep/listed store "main" {})))]
    (with-redefs [schedules/list-all (constantly [])]
      (is (pos? (:found (sweep/sweep! store schema "main" (answers)))))
      (is (some #{rule} (open)))
      (sweep/snooze! store "main" (assoc one :days 3 :reason "später"))
      (is (not-any? #{rule} (open)) "a snoozed one is hidden")
      (sweep/sweep! store schema "main" (answers))
      (is (not-any? #{rule} (open)) "the next sweep keeps the snooze")
      (sweep/unsnooze! store "main" one)
      (is (some #{rule} (open)))
      (sweep/assign! store "main" (assoc one :assignee "anna"))
      (is (= ["transform/a"]
             (map :resource (:recommendations
                             (sweep/listed store "main" {:assignee "anna"})))))
      (sweep/sweep! store (dissoc schema :transforms) "main" (answers))
      (is (not-any? #{rule} (open)) "no longer found: resolved")
      (is (pos? (:resolved (sweep/listed store "main" {})))))))
