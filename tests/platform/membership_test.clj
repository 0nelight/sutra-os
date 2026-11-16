(ns ^:unit platform.membership-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.types.forms :as forms]
            [sutra.types.membership :as member]))

(def ^:private welt
  {:projects   {:lager {}}
   :datasets   {:roh {:project :lager :kind :standalone}
                :sauber {:kind :transform-output}}
   :transforms {:bereinigen {:language :pg-sql :query "SELECT 1"
                             :inputs {:a {:kind :dataset :dataset :roh}}
                             :output {:kind :dataset :dataset :sauber}}
                :ohne-alles {:language :pg-sql :query "SELECT 1"
                             :output {:kind :dataset :dataset :nirgends}}}
   :graphs     {:skizze {:title "Skizze"}}})

(defn- project [tag nm]
  (let [lens (forms/author welt)]
    (member/project-of tag (some #(when (= nm (first %)) %) ((:entries lens) tag)) lens)))

(deftest a-transform-sits-where-its-data-is
  (testing "no project of its own, its output has none either: its input dataset decides"
    (is (= :lager (project :transform :bereinigen)))))

(deftest a-dataset-sits-where-its-producer-is
  (testing "a dataset a transform outputs takes that transform's project, also a derived one"
    (is (= :lager (project :dataset :sauber)))))

(deftest what-nothing-places-stays-without-and-is-named
  (let [ohne (set (map (fn [[tag [nm]]] [tag nm]) (member/without-project (forms/author welt))))]
    (is (contains? ohne [:transform :ohne-alles]) "nothing to derive from: the gate names it")
    (is (not (contains? ohne [:graph :skizze])) "a diagram is a view — its place is optional")
    (is (not (contains? ohne [:transform :bereinigen])))
    (is (not (contains? ohne [:dataset :sauber])))))
