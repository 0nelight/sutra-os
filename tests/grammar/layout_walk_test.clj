(ns ^:unit grammar.layout-walk-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.capps.catalog :as cat]
            [sutra.capps.walk :as walk]))

(deftest a-reference-is-a-kind-of-node
  (is (= :reference (walk/kind {:val :x})))
  (is (= :reference (walk/kind {:item :x})))
  (is (= :map (walk/kind {:object-property :p :of {:val :s}})) "an inline definition is walked")
  (is (= :map (walk/kind {:kind :rows})))
  (is (= :seq (walk/kind [1])))
  (is (= :value (walk/kind :x)))
  (is (walk/children? [{:widget :text}]))
  (is (not (walk/children? {:val :abschnitte})) "a loop's :items that reads a variable"))

(deftest a-pass-never-sees-a-reference-as-its-entries
  (let [seen (volatile! [])
        out  (walk/postwalk (fn [x] (vswap! seen conj x) x)
                            {:kind :loop :items {:val :abschnitte} :body {:widget :text}})]
    (is (= {:kind :loop :items {:val :abschnitte} :body {:widget :text}} out))
    (is (some #{{:val :abschnitte}} @seen) "the reference reaches the pass whole")
    (is (not-any? #(and (vector? %) (= :val (first %))) @seen)
        "and never as a [:val :abschnitte] entry")))

(deftest filtering-children-leaves-a-reference-alone
  (let [keep-shown (fn [x]
                     (if (and (map? x) (walk/children? (:items x)))
                       (update x :items #(filterv (complement :hidden?) %))
                       x))]
    (is (= {:kind :loop :items {:val :abschnitte}}
           (walk/postwalk keep-shown {:kind :loop :items {:val :abschnitte}}))
        "28b5821de: {:val :abschnitte} became [[:val :abschnitte]]")
    (is (= {:kind :rows :items [{:widget :a}]}
           (walk/postwalk keep-shown {:kind :rows :items [{:widget :a} {:hidden? true}]})))))

(deftest where-a-row-is-filled-is-said-once
  (is (= :item (walk/fills-below {:kind :loop} :body)))
  (is (= :item (walk/fills-below {:kind "loop"} :group-head)))
  (is (= :arg (walk/fills-below {:kind :overlay} :body)))
  (is (nil? (walk/fills-below {:kind :rows} :items)))
  (is (= [[[:vars :g] #{}] [[:items 0 :body :vars :f] #{:item}]]
         (mapv (juxt :path :fills)
               (cat/function-vars
                {:kind  :rows
                 :items [{:kind :loop :items {:val :s}
                          :body {:vars {:f {:function :fn :args {:a {:item :x}}}}}}]
                 :vars  {:g {:function :fn}}})))))

(deftest the-walk-keeps-shapes
  (is (= '(1 2) (walk/postwalk identity '(1 2))))
  (is (list? (walk/postwalk identity '(1 2))))
  (is (= #{1 2} (walk/postwalk identity #{1 2})))
  (is (= {:m 1} (meta (walk/walk-tree (constantly nil) (with-meta [1] {:m 1})))))
  (testing "a hit stops the descent"
    (is (= {:a :hit :b [:hit]}
           (walk/walk-tree #(when (= 1 %) [:hit]) {:a 1 :b [1]})))))

(deftest visit-carries-the-lexical-scope
  (let [tree {:vars  {:a {:default 1}}
              :items [{:vars {:b {:from :a}} :widget :text :value {:val :b}}
                      {:widget :text :value {:val :c}}]}
        seen (walk/visit (fn [x {:keys [path scope decl?]}]
                           (when (walk/leaf-ref? x) [[path (set (keys scope)) decl?]]))
                         tree {:references? true :scope {:o {}}})]
    (is (= [[[:items 0 :vars :b] #{:o :a :b} true]
            [[:items 0 :value] #{:o :a :b} false]
            [[:items 1 :value] #{:o :a} false]]
           seen)
        "a sibling's :vars is not in scope; a declaration is marked as such"))
  (is (= [] (walk/visit (fn [x _] (when (walk/leaf-ref? x) [x])) {:value {:val :x}}))
      "without :references? a reference is not visited")
  (is (= [{:from {}}]
         (walk/visit (fn [x {:keys [decl?]}] (when (and decl? (contains? x :from)) [x]))
                     {:vars {:from {}}}))
      "the map under :vars is declarations, even with an entry named like a form key"))

(deftest visit-carries-what-a-pass-adds
  (is (= [[[:body] 1] [[:body :items 0] 1] [[:head] 0]]
         (walk/visit (fn [_ {:keys [path depth]}] (when (seq path) [[path depth]]))
                     {:body {:items [{:x 1}]} :head {}}
                     {:ctx  {:depth 0}
                      :down (fn [ctx _ k] (cond-> ctx (= :body k) (update :depth inc)))}))))

(deftest an-object-view-fills-its-object-below-its-root
  (is (= [[[:layout :vars :g] #{}] [[:layout :items 0 :vars :f] #{:item}]]
         (mapv (juxt :path :fills)
               (cat/function-vars
                {:form-factor :full
                 :layout      {:vars  {:g {:function :fn}}
                               :items [{:vars {:f {:function :fn}}}]}})))))
