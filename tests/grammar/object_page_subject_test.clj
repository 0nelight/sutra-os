(ns ^:unit grammar.object-page-subject-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.capps.standard-view :as sv]
            [sutra.kit.remote :as rd]))

(defn- show [m] (:show (sv/subject m)))

(deftest without-a-type-nothing-is-selected
  (is (= :none (show {:catalog rd/loading}))))

(deftest a-type-the-catalog-does-not-know
  (testing "solange der Katalog laedt, wartet die Seite"
    (is (= :loading (show {:asked "ri.x" :catalog rd/loading}))))
  (testing "ist er da, gibt es den Typ nicht — kein Spinner fuer immer"
    (is (= :unknown-type (show {:asked "ri.x" :catalog :loaded}))))
  (testing "scheiterte er, sagt die Seite warum"
    (is (= {:show :failed :error "HTTP 500"}
           (sv/subject {:asked "ri.x" :catalog (rd/failed "HTTP 500")})))))

(deftest a-known-type-follows-its-entry
  (let [m {:asked "ri.x" :resolved "flug" :catalog :loaded :id "1"}]
    (is (= :loading (show m)) "noch kein Eintrag")
    (is (= :loading (show (assoc m :entry {:loading? true}))))
    (is (= :page (show (assoc m :entry {:loading? true :instance {:id "1"}})))
        "ein schon gezeigtes Objekt bleibt beim Neuladen stehen")
    (is (= {:show :failed :error "x"} (sv/subject (assoc m :entry {:error "x"}))))
    (is (= :not-found (show (assoc m :entry {}))))
    (is (= :page (show (assoc m :entry {:instance {:id "1"}}))))))
