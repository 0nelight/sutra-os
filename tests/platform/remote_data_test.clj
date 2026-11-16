(ns ^:unit platform.remote-data-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.kit.remote :as rd]))

(deftest every-state-is-said
  (is (= :loading (rd/state rd/loading)))
  (is (= :absent (rd/state rd/absent)))
  (is (= :failed (rd/state (rd/failed "kaputt"))))
  (is (= :value (rd/state {:id 1})))
  (testing "falsy values are values, not missing ones"
    (is (= :value (rd/state false)))
    (is (= :value (rd/state [])))
    (is (= :value (rd/state 0))))
  (testing "nil says nothing: it is refused, not read as loading or absent"
    (is (thrown? clojure.lang.ExceptionInfo (rd/state nil)))
    (is (false? (rd/value? nil)))))

(deftest fold-picks-the-branch-of-the-state
  (let [branches {:loading (constantly "laedt") :absent (constantly "keins")
                  :failed (fn [r] (str "Fehler: " r)) :value (fn [v] (str "Wert " v))}]
    (is (= "laedt" (rd/fold rd/loading branches)))
    (is (= "keins" (rd/fold rd/absent branches)))
    (is (= "Fehler: x" (rd/fold (rd/failed "x") branches)))
    (is (= "Wert 3" (rd/fold 3 branches)))
    (is (nil? (rd/fold 3 {})) "a missing branch answers nil")))

(deftest fmap-and-value-or-touch-only-values
  (is (= 4 (rd/fmap inc 3)))
  (is (= rd/loading (rd/fmap inc rd/loading)))
  (is (= rd/absent (rd/fmap inc rd/absent)))
  (is (= 3 (rd/value-or 3 :x)))
  (is (= :x (rd/value-or rd/loading :x)))
  (is (= "x" (rd/reason (rd/failed "x"))))
  (is (some? (rd/reason (rd/failed nil))) "a failure always has something to show"))

(deftest all-waits-for-every-input
  (is (= [1 2] (rd/all [1 2])))
  (is (= rd/loading (rd/all [1 rd/loading])))
  (is (= rd/absent (rd/all [1 rd/absent])))
  (is (= rd/loading (rd/all [rd/absent rd/loading])) "loading wins over absent: not yet known")
  (is (= (rd/failed "a") (rd/all [rd/loading (rd/failed "a") (rd/failed "b")])))
  (is (= [] (rd/all [])))
  (is (thrown? clojure.lang.ExceptionInfo (rd/all [1 nil]))))

(deftest of-answer-turns-an-answer-into-a-state
  (is (= {:id 1} (rd/of-answer {:id 1})))
  (is (= rd/absent (rd/of-answer nil)))
  (is (= [] (rd/of-answer [])) "no rows is an answer about rows"))

(deftest a-list-view-tells-loading-from-empty
  (is (= {:show :loading} (rd/list-view rd/loading)))
  (is (= {:show :empty} (rd/list-view [])))
  (is (= {:show :empty} (rd/list-view rd/absent)))
  (is (= {:show :failed :reason "x"} (rd/list-view (rd/failed "x"))))
  (is (= {:show :list :rows [1 2]} (rd/list-view '(1 2)))))

(deftest a-failed-refresh-keeps-what-is-shown
  (is (= [1] (rd/after-failure [1] "x")))
  (is (= (rd/failed "x") (rd/after-failure rd/loading "x")))
  (is (= (rd/failed "y") (rd/after-failure (rd/failed "x") "y"))))
