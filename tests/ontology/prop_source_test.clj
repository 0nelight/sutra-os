(ns ^:unit ontology.prop-source-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.capps.prop-source :as ps]))

(def tree
  {:kind :page
   :vars {:flights {:from :flight :where [[:aktiv := 1]]}
          :fast    {:from :flights}
          :ring-a  {:from :ring-b}
          :ring-b  {:from :ring-a}
          :airports {:source :airport}
          :sel     {:default nil}
          :total   {:agg [:sum :netto] :of :flights}
          :n       {:aggregate {:find :flight}}
          :vergl   {:compare {:of :airports :scenarios {:var :sel}}}}
   :body [{:widget :object-table :rows {:from :flight}
           :cols [{:prop :number}]}
          {:widget :chart :rows {:val :fast} :x :tag :y :netto}
          {:widget :chart :rows {:val :ring-a} :x :tag}
          {:widget :chart :rows {:val :sel} :x :tag}
          {:widget :chart :rows {:val :nirgends} :x :tag}
          {:widget :chart :rows {:val :vergl} :x :tag}
          {:widget :object-table :rows nil :cols [{:prop nil}]}]})

(def index (ps/vars-index tree))

(defn- widget [i] (get-in tree [:body i]))

(deftest vars-index-collects-every-scope
  (is (= #{:flights :fast :ring-a :ring-b :airports :sel :total :n :vergl}
         (set (keys index))))
  (testing "an inner scope layers over the root"
    (is (= :b (get-in (ps/vars-index {:vars {:x {:source :a}}
                                      :body {:kind :section :vars {:x {:source :b}}}})
                      [:x :source])))))

(deftest ref-type-resolves-the-chain
  (testing "a direct {:from <type>}"
    (is (= :flight (ps/ref-type index {:from :flight})))
    (is (= :flight (ps/ref-type index {:from "flight"}))))
  (testing "a var chain: {:val v} → {:from <var>} → {:from <type>}"
    (is (= :flight (ps/ref-type index {:val :fast})))
    (is (= :flight (ps/ref-type index {:val :flights})))
    (is (= :airport (ps/ref-type index {:val :airports})))
    (is (= :airport (ps/ref-type index {:val :vergl}))))
  (testing "a cycle terminates instead of recurring"
    (is (nil? (ps/ref-type index {:val :ring-a})))
    (is (nil? (ps/ref-type index {:val :ring-b}))))
  (testing "unresolvable refs are nil, never a guess"
    (is (nil? (ps/ref-type index {:val :nirgends})))
    (is (nil? (ps/ref-type index {:val :sel})))
    (is (nil? (ps/ref-type index {:val :total})))
    (is (nil? (ps/ref-type index {:val :n})))
    (is (nil? (ps/ref-type index nil)))
    (is (nil? (ps/ref-type index "flight")))))

(deftest node-object-type-reads-the-nodes-object-set
  (is (= :flight (ps/node-object-type index (widget 0) :cols)))
  (is (= :flight (ps/node-object-type index (widget 0))))
  (is (= :flight (ps/node-object-type index (widget 1) :x)))
  (is (nil? (ps/node-object-type index (widget 2) :x)))
  (is (nil? (ps/node-object-type index (widget 6) :cols)))
  (is (nil? (ps/node-object-type index nil :cols)))
  (testing "a :value-kind set param carries a prop picker too"
    (is (= :airport (ps/node-object-type index
                                         {:widget :select-input :sel {:var :sel}
                                          :items {:val :airports}}
                                         :item-label))))
  (testing "the tree-level convenience arity"
    (is (= :flight (ps/prop-type tree (widget 0) :cols)))))

(deftest properties-come-from-schema-and-module
  (let [objects {:flight {:fields [{:name :number :type :string}
                                   {:name :netto :type :double}]}
                 :airport [{:name :code}]}
        module  {:flight {:marge {:column-math [:div :netto :brutto]}
                          :netto {:column-math [:+ 1]}}}]
    (is (= [:number :netto] (ps/native-properties objects :flight)))
    (is (= [:code] (ps/native-properties objects "airport")))
    (is (= [] (ps/native-properties objects :unbekannt)))
    (is (= [{:group :native :label "Eigenschaften" :props [:number :netto]}
            {:group :derived :label "abgeleitet" :props [:marge]}]
           (ps/prop-options objects module :flight)))
    (is (= [:number :netto :marge] (ps/prop-names objects module :flight)))
    (is (= [{:group :native :label "Eigenschaften" :props [:code]}]
           (ps/prop-options objects module :airport)))
    (is (nil? (ps/prop-options objects module nil)))))
