(ns ^:unit grammar.bind-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.capps.bind :as bind]
            [sutra.capps.catalog :as cat]
            [sutra.capps.events :as ce]
            [sutra.capps.layout-refs :as lr]
            [sutra.capps.layout :as lay]
            [sutra.capps.variables :as vars]))

(defn- read-values
  [f form nm entry _scope]
  (case (:id f)
    :item-not      {:value (not (get entry nm))}
    (:item :arg :row) {:value (get entry nm)}
    :spec          {:value [:spec form entry]}
    {:value entry}))

(def ^:private values
  {:vars {:a 1 :b nil} :types #{:projekt} :specs true :read read-values})

(defn- rule [form scope] (get-in (bind/resolve form scope) [:problem :rule]))

(deftest every-form-is-in-the-grammar
  (is (= #{:literal :row :item :item-not :arg :val :var :from :source :linked :spec}
         (set (map :id bind/forms))))
  (doseq [f bind/forms]
    (is (and (:label f) (:hint f)) (str (:id f) " has a label and a hint"))
    (when (:key f) (is (:env f) (str (:id f) " says where it is read")))))

(deftest a-literal-is-its-own-value
  (doseq [x [1 "a" :k nil [1 2] {:a 1 :b 2}]]
    (is (= {:value x} (bind/resolve x {})) (pr-str x))))

(deftest a-variable-reads-from-the-variables-in-scope
  (is (= {:value 1} (bind/resolve {:val :a} values)))
  (is (= {:value 1} (bind/resolve {:val "a"} values)) "a string names the same")
  (is (= {:value nil} (bind/resolve {:val :b} values)) "a declared nil is a value")
  (is (= {:value 1} (bind/resolve {:var :a} values)))
  (is (= {:value 1} (bind/resolve {:from :a} values)))
  (is (= :undeclared (rule {:val :c} values)))
  (is (= :undeclared (rule {:var :c} values)))
  (is (= :no-name (rule {:val ""} values)))
  (is (= :not-filled (rule {:val :a} (dissoc values :vars)))))

(deftest var-needs-a-state
  (let [s (assoc values :stateful? #(= 1 %))]
    (is (= {:value 1} (bind/resolve {:var :a} s)))
    (is (= :no-state (rule {:var :b} s)) "a computed variable has no handle")
    (is (= {:value nil} (bind/resolve {:val :b} s)) "its value is read through {:val}")))

(deftest a-type-reads-from-the-types-in-scope
  (is (= {:value :projekt} (bind/resolve {:source :projekt} values)))
  (is (= :unknown-type (rule {:source :auftrag} values)))
  (is (= :not-filled (rule {:linked :projekt} values)) "an app has no neighbours")
  (is (= {:value :projekt} (bind/resolve {:linked :projekt} (assoc values :linked true)))))

(deftest a-row-field-is-read-only-where-the-row-is-filled
  (doseq [[env form] [[:item {:item :x}] [:arg {:arg :x}] [:row {:row :x}]]]
    (testing (pr-str form)
      (is (= :not-filled (rule form values)))
      (is (= {:value 7} (bind/resolve form (bind/with-fill values env {:x 7}))))
      (is (= :not-filled (rule form (-> (bind/with-fill values env {:x 7})
                                        (bind/without-fill env)))))))
  (is (= {:value false} (bind/resolve {:item-not :x} (bind/with-fill values :item {:x 7})))
      "a loop's row fills {:item-not} too")
  (is (= :not-filled (rule {:item-not :x} (bind/with-fill values :arg {:x 7})))))

(deftest an-inline-spec-is-a-form
  (is (= :spec (bind/form-id {:object-property :p :of :s})))
  (is (= :literal (bind/form-id {:object-property :p :widget :kpi})) "a widget is no spec")
  (is (= {:value [:spec {:object-property :p :of :s} true]}
         (bind/resolve {:object-property :p :of :s} values))))

(deftest the-grammar-is-the-one-the-others-read
  (is (= (mapv :id ce/value-refs) [:literal :row :item :arg :val :var])
      "an event's value takes the effect forms, in the editor's order")
  (is (bind/variable-ref? {:val :a}))
  (is (bind/variable-ref? {:source :t}))
  (is (not (bind/variable-ref? {:item :a})) "a row field is no variable reference")
  (is (not (bind/variable-ref? {:val :a :x 1})) "a reference has ONE key")
  (is (bind/fill-ref? {:item-not :a}))
  (is (= :a (bind/form-name {:val "a"}))))

(deftest a-problem-is-worded
  (is (re-find #"nur im :body einer :loop"
               (bind/message (:problem (bind/resolve {:item :x} values)))))
  (is (re-find #"\{:val :b\}"
               (bind/message (:problem (bind/resolve {:var :b}
                                                     (assoc values :stateful? #{1})))))))

(deftest the-renderer-reads-through-the-grammar
  (let [h (atom 5)]
    (is (identical? h (vars/resolve-ref {:var :a} {:a h} {} {})) "{:var} is the handle")
    (is (nil? (vars/resolve-ref {:var :b} {} {:b {:expr "1"}} {}))
        "a computed variable has no handle to hand out")
    (is (= 5 (vars/resolve-ref {:val :a} {:a h} {} {})) "{:val} its value"))
  (is (= [{:id 1}] (vars/resolve-ref {:source :t} {} {} {:t [{:id 1}]})))
  (is (= {:item :x} (vars/resolve-ref {:item :x} {} {} {}))
      "a row field not filled here stays for the pass that fills it")
  (is (= {:x 1} (vars/resolve-ref {:x 1} {} {} {})) "a literal map is no reference"))

(deftest a-call-takes-references-only
  (let [h (atom 5)
        args {:a {:val :a} :b {:object-property :p :of {:val :a}} :c {:item :x} :d [{:val :a}]}]
    (is (= {:a 5 :b {:object-property :p :of 5} :c {:item :x} :d [5]}
           (vars/resolve-call-args args {:a h} {} {}))
        "an inline definition and an unfilled row field reach the Function as written")))

(deftest a-row-is-filled-by-the-same-reader
  (is (= {:widget :text :value 3}
         (lay/resolve-item {:widget :text :value {:item :n}} {:n 3})))
  (is (= {:a true :b {:val :v}}
         (lay/resolve-item {:a {:item-not :n} :b {:val :v}} {:n nil}))
      "{:item-not} too; every other form stays")
  (is (= {:a 1 :b {:item :k}} (lay/fill-arg {:a {:arg :k} :b {:item :k}} {:k 1}))))

(deftest the-check-refuses-what-the-renderer-cannot-read
  (testing "{:item} in :args where no row is filled"
    (is (= [{:item :x}] (cat/unresolved-arg-forms {:item :x} #{})))
    (is (= {:item :x} (vars/resolve-call-args {:item :x} {} {} {}))
        "the renderer sends it as written — what the check refuses")
    (is (= [] (cat/unresolved-arg-forms {:item :x} #{:item})) "inside a loop it is filled")
    (is (= [{:object-property :p :of :s}]
           (cat/unresolved-arg-forms {:object-property :p :of :s} #{}))
        "an inline definition is no reference in a call")
    (is (= [] (cat/unresolved-arg-forms [{:val :a} 1] #{}))))
  (testing "{:var} of a computed variable in a custom widget"
    (let [layout {:vars {:n {:expr "1"} :s {:default 1}}
                  :widget :custom-widget :variables {:p {:var :n} :q {:var :s}}}]
      (is (= [[:p]]
             (keep #(when (re-find #"keinen Zustand" (:error %)) (subvec (:path %) 1))
                   (cat/reference-problems layout)))
          "only the computed one, by the same rule")
      (is (nil? (vars/resolve-ref {:var :n} {:s (atom 1)} {:n {:expr "1"}} {}))
          "the renderer hands out no handle for it")))
  (testing "{:val} in a loop's :items reads the declaration"
    (is (= [[:item-field :nope]]
           (mapv (juxt :rule :field)
                 (lr/problems {:vars  {:s {:from :block}}
                               :kind  :loop :items {:val :s}
                               :body  {:widget :text :value {:item :nope}}}
                              {:types {:block #{:id}}})))))
  (testing "an undeclared variable"
    (is (= ["Variable :x ist nicht deklariert"]
           (map :error (cat/reference-problems {:widget :text :value {:val :x}}))))))

(deftest a-param-takes-the-binding-its-kind-says
  (let [layout {:kind  :rows
                :vars  {:s {:default :a} :v {:default "x"}}
                :items [{:kind :tabs :select {:val :s} :tabs []}
                        {:kind :overlay :overlay "o" :visible {:var :v} :body {:widget :text}}
                        {:kind :section :collapse true :body {:widget :text}}
                        {:kind :loop :items [1 2] :body {:widget :text}}]}]
    (is (= [[[:items 0 :select] :select] [[:items 3 :items] :items]]
           (mapv (juxt :path :param) (cat/binding-kind-problems layout)))
        "{:val} where a handle is wanted and a literal where a set is; true collapses")
    (is (= "select braucht eine Variable {:var …}, nicht {:val :s}"
           (:error (first (cat/binding-kind-problems layout)))))
    (is (nil? (cat/explain-node {:kind :section :collapse true :body {:widget :text}}))
        "the editor reads the same rule: :collapse takes true")
    (is (= :select (:param (cat/explain-node {:kind :tabs :select {:val :s} :tabs [{}]}))))))
