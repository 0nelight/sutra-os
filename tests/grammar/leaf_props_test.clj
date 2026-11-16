(ns ^:unit grammar.leaf-props-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.capps.layout :as lay]
            [sutra.capps.leaf-props :as lp]))

(def ^:private rid "ri.sutra.main.object-type.1")

(def ^:private red {:color "#c62828"})

(def ^:private page
  {:descriptor {:object-names     {rid :auftrag}
                :property-display {rid {:summe {:name "Summe" :type "decimal"
                                                :conditional-format
                                                [{:when [:< [:get :summe] 0] :color "#c62828"}]}
                                        :stand {:name "Stand" :type "string"}}}
                :object           {:id "a1" :object-type-ref rid
                                   :values {:summe -5 :stand "offen"}}}
   :data       {:link/positionen [{:id "p1" :object-type-ref rid :values {:summe 2}}
                                  {:id "p2" :object-type-ref rid :values {:summe 3}}]}
   :ctx        {}})

(deftest the-descriptor-is-read-by-name-or-rid
  (is (= :auftrag (lp/viewed-type (:descriptor page) {:object-type-ref rid})))
  (is (= "Summe" (:name (lp/display-entry (:descriptor page) :auftrag :summe))))
  (is (= "Summe" (:name (lp/display-entry (:descriptor page) rid :summe)))))

(deftest a-kpi-takes-what-the-author-left-open-from-the-ontology
  (let [src {::lay/source {:prop :summe :values {:summe -5} :type rid}}]
    (testing "no tone: the property's conditional format colours it"
      (let [p (lp/leaf->props page (merge {:widget :kpi :label "S" :value -5} src))]
        (is (= red (:look p)))
        (is (nil? (:accent p)))
        (is (not (contains? p ::lay/source)) "the source is consumed")
        (is (string? (:value p)) "formatted")))
    (testing "an authored tone wins and nothing is looked up"
      (let [p (lp/leaf->props page (merge {:widget :kpi :value -5 :accent :pos} src))]
        (is (= :pos (:accent p)))
        (is (nil? (:look p)))))
    (testing "a secondary value is formatted beside"
      (is (= {:value 3} (:secondary (lp/leaf->props page {:widget :kpi :value 1
                                                          :secondary 3})))))
    (testing "a kpi-row decides every item alike"
      (is (= 2 (count (:items (lp/leaf->props page {:widget :kpi-row
                                                    :items [{:value 1} {:value 2}]}))))))))

(deftest a-badge-reads-its-tone-like-a-kpi
  (let [p (lp/leaf->props page {:widget :badge :text "offen" :tone :auto})]
    (is (nil? (:tone p)) ":auto is no tone")
    (is (= "offen" (:text p)))))

(deftest property-cards-show-the-object-or-say-there-is-none
  (testing "a card per property and per aggregation over a link"
    (let [{:keys [items]} (lp/leaf->props page {:widget :property-cards
                                                :cards [:summe
                                                        {:link :positionen :agg :count}]})]
      (is (= ["Summe" "Positionen · Anzahl"] (mapv :label items)))
      (is (= red (:look (first items))) "the property's colour for this object")
      (is (= "2" (str (:value (second items)))))))
  (testing "no object: nothing to draw, and the view says so"
    (is (= {:no-object? true}
           (lp/leaf->props (update page :descriptor dissoc :object)
                           {:widget :property-cards :cards [:summe]})))))

(deftest a-scenario-comparison-is-the-charts-group-axis
  (let [cfg (lp/compare-chart {:compare {:rows [{:base {:id 1} :by {"s1" {:id 1}}}]
                                         :scenarios [{:scenario "s1" :name "Plan"}]}
                               :x :monat})]
    (is (= :szenario (:group cfg)))
    (is (= ["Basis" "Plan"] (mapv :szenario (:rows cfg))))
    (is (not (contains? cfg :compare)))))

(deftest scenarios-are-named-strings
  (is (= ["a" "b"] (lp/seg-list ["a" "" nil "b" "a"])))
  (is (= ["a"] (lp/seg-list "a")))
  (is (= [] (lp/seg-list nil)))
  (is (= "Plan" (lp/scenario-name [{:scenario "s1" :name "Plan"}] "s1")))
  (is (= "s2" (lp/scenario-name [] "s2"))))

(deftest a-widget-without-a-decision-gets-its-cfg
  (is (= {:rows [1]} (lp/leaf->props page {:widget :object-table :rows [1]}))))

(deftest a-scenario-comparison-table-adds-a-column-where-a-scenario-differs
  (let [cmp {:rows      [{:key 1 :base {:id 1 :values {:summe 2 :stand "offen"}}
                          :by {"s1" {:id 1 :values {:summe 3 :stand "offen"}}}}
                         {:key 2 :base {:id 2 :values {:summe 1 :stand "zu"}}
                          :by {"s1" {:id 2 :values {:summe 1 :stand "zu"}}}}]
             :scenarios [{:scenario "s1" :name "Plan"}]}
        cfg (lp/compare-table {:compare cmp :limit 1 :sort [[:summe :desc]]
                               :cols [{:prop :summe :label "Summe"}
                                      {:prop :stand :label "Stand"}]}
                              {})]
    (testing "a changed column gets one beside it per scenario, an unchanged one none"
      (is (= [[:summe nil] [:summe "s1"] [:stand nil]]
             (mapv (juxt (comp :prop :col) :seg) (:cols cfg))))
      (is (= "Summe · Plan" (:label (second (:cols cfg))))))
    (is (= 1 (count (:rows cfg))) "limited")
    (is (not-any? cfg [:compare :sort :limit :foot])))
  (testing "while the comparison is not there, the foot says why"
    (is (= [:loading] (:foot (lp/compare-table {:compare {:loading? true} :cols []} {}))))
    (let [cfg (lp/compare-table {:compare {:error "kaputt" :rows [{:key 1}]} :cols []} {})]
      (is (= [:error "kaputt"] (:foot cfg)))
      (is (= [] (:rows cfg))))))
