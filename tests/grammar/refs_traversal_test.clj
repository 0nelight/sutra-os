(ns ^:unit grammar.refs-traversal-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [grammar.ref-rename-test :as rename]
            [sutra.types.entities :as ents]
            [sutra.types.refs :as refs]))

(defn- plant
  [m spec next-v]
  (reduce
   (fn [m {:keys [at many each each-val steps]}]
     (let [put (fn [v] (if (seq at) (assoc-in m at v) v))]
       (cond
         steps    (let [step (plant (if (seq at) {} m) steps next-v)]
                    (put (assoc step :rules [(plant {} steps next-v)])))
         each     (put [(plant {} each next-v) (plant {} each next-v)])
         each-val (put {:a (plant {} each-val next-v) :b (plant {} each-val next-v)})
         many     (put [(next-v) (next-v)])
         :else    (put (next-v)))))
   m spec))

(defn- planted
  [spec]
  (let [n      (atom 0)
        minted (atom [])
        m      (plant {} spec #(let [v (keyword (str "v" (swap! n inc)))] (swap! minted conj v) v))]
    [m (set @minted)]))

(def ^:private specs
  (merge ents/refs
         {::action-param ents/action-param-refs
          ::action-rule  ents/action-rule-refs
          ::action       ents/action-refs}))

(deftest fold-and-rewrite-are-one-walk
  (doseq [[section spec] specs
          :when (seq spec)
          :let [[m minted] (planted spec)
                seen (refs/ref-values spec m)]]
    (testing (str section)
      (is (= minted (set (map second seen))) "the fold finds every planted reference")
      (is (= (count minted) (count seen)) "and each once")
      (is (= m (refs/rewrite-refs spec m (fn [_ v] v))) "an identity rewrite changes nothing")
      (let [tagged (refs/rewrite-refs spec m (fn [t v] [t v]))]
        (is (= seen (map second (refs/ref-values spec tagged)))
            "the rewrite replaces exactly what the fold sees, in its order")))))

(deftest an-action-states-its-references-in-every-step
  (let [a {:on      :auftrag
           :params  {:kunde {:type :kunde :options-from {:object-type :kunde-alt}}
                     :n     {:type :integer}
                     :x     "not a map"}
           :rules   [{:op :create :object-type :auftrag
                      :then [{:op :create-link :link-type :auftrag-kunde}]}]
           :edits   [{:object-type :rechnung :rules [{:interface :buchbar}]}]}]
    (is (= [[#{:object-types :interfaces} :kunde]
            [#{:object-types :interfaces} :kunde-alt]
            [#{:object-types :interfaces} :integer]
            [#{:object-types :interfaces} :auftrag]
            [#{:object-types :interfaces} :auftrag]
            [#{:link-types} :auftrag-kunde]
            [#{:object-types :interfaces} :rechnung]
            [#{:interfaces} :buchbar]]
           (refs/ref-values ents/action-refs a)))
    (is (= [[#{:object-types :interfaces} :auftrag] [#{:link-types} :auftrag-kunde]]
           (refs/ref-values ents/action-rule-refs (dissoc a :params :on)))
        "the rule tree alone leaves :edits out")
    (let [out (refs/rewrite-refs ents/action-rule-refs (list {:on :x}) (fn [_ _] :a))]
      (is (and (vector? out) (= :a (get-in out [0 :on])))
          "a rule tree given as a sequence comes back a vector"))))

(deftest the-author-form-reads-as-it-rewrites
  (doseq [[p body] (rename/corpus)
          :let [seen (refs/ref-values ents/author-body-refs body)]]
    (testing p
      (is (= body (refs/rewrite-refs ents/author-body-refs body (fn [_ v] v)))
          "an identity rewrite changes nothing")
      (let [tagged (refs/rewrite-refs ents/author-body-refs body (fn [t v] [::ref t v]))]
        (is (= seen (map #(subvec (second %) 1) (refs/ref-values ents/author-body-refs tagged)))
            "the rewrite replaces exactly what the fold sees, in its order")))))

(deftest a-slot-holds-where-its-condition-does
  (let [spec [{:steps [{:at [:name] :targets #{:actions} :if {:eq {:op #{:action}}}}]}
              {:at [:objects] :each-key {:targets #{:object-types}
                                         :if {:has :attributes :only-keys #{:attributes}}}}]
        m    {:rules   [{:op :action :name :a} {:op :create :name :b}]
              :objects {:x {:attributes [[:n :string]]} :y {:attributes []} :z {:title :t}}}]
    (is (= [[#{:actions} :a] [#{:object-types} :x]] (refs/ref-values spec m)))
    (let [out (refs/rewrite-refs spec m (fn [_ v] (keyword (str/upper-case (name v)))))]
      (is (= {:X {:attributes [[:n :string]]} :y {:attributes []} :z {:title :t}} (:objects out))
          "a key that is a reference is renamed with its entry, the others stay")
      (is (= [{:op :action :name :A} {:op :create :name :b}] (:rules out))))))
