(ns ^:unit platform.datalog-test
  (:require [clojure.test :refer [deftest is]]
            [sutra.kit.datalog :as dl]))

(def ^:private kette
  [[{:id :pfad/kante :head '[:pfad ?a ?b] :body '[[:kante ?a ?b]]}
    {:id :pfad/weiter :head '[:pfad ?a ?c] :body '[[:kante ?a ?b] [:pfad ?b ?c]]}]])

(deftest recursion-reaches-the-fixpoint
  (let [db (dl/run {:kante #{[1 2] [2 3] [3 4]}} kette)]
    (is (dl/holds? db :pfad [1 4]))
    (is (not (dl/holds? db :pfad [4 1])))
    (is (= 6 (count (:pfad db))))))

(deftest a-cycle-terminates
  (let [db (dl/run {:kante #{[1 2] [2 1]}} kette)]
    (is (= #{[1 2] [2 1] [1 1] [2 2]} (:pfad db)))))

(deftest constants-and-repeated-variables-constrain
  (let [db (dl/run {:r #{[:a :a] [:a :b]}}
                   [[{:id :gleich :head '[:gleich ?x] :body '[[:r ?x ?x]]}
                     {:id :von-a :head '[:von-a ?y] :body '[[:r :a ?y]]}]])]
    (is (= #{[:a]} (:gleich db)))
    (is (= #{[:a] [:b]} (:von-a db)))))

(deftest negation-reads-an-earlier-stratum
  (let [db (dl/run {:knoten #{[1] [2] [3]} :kante #{[1 2]}}
                   [[{:id :hat-aus :head '[:hat-aus ?a] :body '[[:kante ?a ?b]]}]
                    [{:id :senke :head '[:senke ?a] :body '[[:knoten ?a] [:not [:hat-aus ?a]]]}]])]
    (is (= #{[2] [3]} (:senke db)))))

(deftest a-derived-fact-explains-itself
  (let [db (dl/run {:kante #{[1 2] [2 3]}} kette)
        e  (dl/explain db [:pfad [1 3]])]
    (is (= :pfad/weiter (:rule e)))
    (is (= [:kante [1 2]] (:fact (first (:from e)))))
    (is (= :pfad/kante (:rule (second (:from e)))))
    (is (nil? (dl/explain db [:pfad [3 1]])) "what does not hold has no explanation")))

(deftest query-binds-the-free-terms
  (let [db (dl/run {:kante #{[1 2] [1 3] [2 3]}} kette)]
    (is (= #{2 3} (set (map '?b (dl/query db '[:pfad 1 ?b])))))))
