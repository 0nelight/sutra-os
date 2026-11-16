(ns ^:unit grammar.lint-view-audiences-test
  (:require [clojure.test :refer [deftest testing is]]
            [sutra.parser.lint :as lint]
            [sutra.parser.spec :as spec]))

(def ^:private split-tabs
  {:kind :tabs :select :t
   :tabs [{:id :a :label "Einkauf" :visibility {:roles [:einkauf]} :body {:widget :attributes}}
          {:id :b :label "Leer" :visibility {:roles [:chef]} :body {:kind :rows :items []}}
          {:id :c :label "Chef" :visibility {:roles [:chef]}
           :body {:kind :tabs :select :u
                  :tabs [{:id :x :visibility {:roles [:einkauf]} :body {:widget :attributes}}]}}]})

(defn- rules-of [app]
  (->> (lint/run-lint {:applications {"r1" (merge {:name :v} app)}} (spec/vocabulary))
       (map :rule)
       frequencies))

(deftest an-audience-that-sees-no-pane-and-an-empty-pane-are-named
  (let [found (rules-of {:form-factor :full :object-type :x :layout split-tabs})]
    (is (= 1 (:pane-blind-for-audience found)) "chef sees none of the inner tabs")
    (is (= 1 (:pane-shows-nothing found)) "tab \"Leer\" has no body")
    (testing "an Object View splits its tabs per profile — no workspace"
      (is (nil? (:app-pages-serve-disjoint-audiences found))))))

(deftest a-view-with-more-than-ten-profiles-is-named
  (let [panes (vec (for [i (range 11)]
                     {:id (keyword (str "p" i)) :label (str i)
                      :visibility {:profiles [(keyword (str "prof" i))]}
                      :body {:widget :attributes}}))
        found (rules-of {:form-factor :full :object-type :x
                         :layout {:kind :tabs :select :t :tabs panes}})]
    (is (= 1 (:view-too-many-profiles found)))
    (is (nil? (:pane-blind-for-audience found)))))
