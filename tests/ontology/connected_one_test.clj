(ns ^:unit ontology.connected-one-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.types.expr :as expr]
            [sutra.parser.ent :as ent]
            [sutra.parser.sugar :as sugar]
            [sutra.parser.resolve :as res]))

(defn- ctx
  [neighbours]
  {:state :world :object-id "child-1"
   :traverse-fn (fn [_state _id _conn _type] neighbours)})

(deftest connected-one-selects-across-the-edge
  (testing "the neighbour's attribute is selected, not aggregated"
    (is (= "Muster GmbH"
           (expr/eval {} [:connected-one :email-lieferant :name]
                      (ctx [{:id "l-1" :values {:name "Muster GmbH" :ort "Villach"}}])))))

  (testing "no neighbour ⇒ nil, matching :connected-max on an empty set"
    (is (nil? (expr/eval {} [:connected-one :email-lieferant :name] (ctx [])))))

  (testing "a missing attribute on a present neighbour is nil, not an error"
    (is (nil? (expr/eval {} [:connected-one :email-lieferant :ust-id]
                         (ctx [{:id "l-1" :values {:name "Muster GmbH"}}])))))

  (testing "TWO neighbours throw — the declaration promised at-most-one"
    (let [e (try (expr/eval {} [:connected-one :email-lieferant :name]
                            (ctx [{:id "l-1" :values {:name "A"}}
                                  {:id "l-2" :values {:name "B"}}]))
                 (catch clojure.lang.ExceptionInfo ex ex))]
      (is (instance? clojure.lang.ExceptionInfo e) "it throws rather than pick")
      (is (= 2 (:found (ex-data e))) "the error carries how many it found")
      (is (re-find #"at-most-one" (ex-message e))
          "and says which promise was broken, not just that something is wrong")))

  (testing "it needs a ctx, like every other link-traversing op"
    (is (thrown? clojure.lang.ExceptionInfo
                 (expr/eval {} [:connected-one :email-lieferant :name])))))

(defn- lower [rollup]
  (-> {:objects {:partner-email
                 {:attributes [[:lieferant-name :string {:derived {:rollup rollup}}]]
                  :plural "Partner emails"}}}
      sugar/lower-rollups
      (get-in [:objects :partner-email :attributes 0 2 :derived :expression])))

(deftest one-lowers-to-the-connected-one-ast
  (testing "{:agg :one :pick p} lowers to [:connected-one over p]"
    (is (= [:connected-one :partner-email-lieferant :name]
           (lower {:over :partner-email-lieferant :agg :one :pick :name}))))

  (testing "the sugar is gone after lowering — downstream sees ONE canonical form"
    (let [schema (sugar/lower-rollups
                  {:objects {:partner-email
                             {:attributes
                              [[:lieferant-name :string
                                {:derived {:rollup {:over :partner-email-lieferant
                                                    :agg :one :pick :name}}}]]
                              :plural "Partner emails"}}})]
      (is (nil? (get-in schema [:objects :partner-email :attributes 0 2 :derived :rollup]))))))

(deftest one-rejects-the-wrong-riders
  (let [ent-of (fn [rollup]
                 {:schema {:objects {:partner-email
                                     {:attributes [[:lieferant-name :string
                                                    {:derived {:rollup rollup}}]]
                                      :plural "Partner emails"}}}})
        bad?   (fn [rollup] (some? (ent/explain (ent-of rollup))))]
    (testing ":one needs :pick"
      (is (bad? {:over :l :agg :one})))
    (testing ":one takes neither :of nor :by — those belong to the aggregates"
      (is (bad? {:over :l :agg :one :pick :name :of :netto}))
      (is (bad? {:over :l :agg :one :pick :name :by :doc-date})))
    (testing "the well-formed one passes"
      (is (not (bad? {:over :l :agg :one :pick :name}))))))

(defn- schema-with [cardinality expr]
  {:objects {:partner-email {:attributes [[:lieferant-name :string
                                             {:derived {:expression expr}}]]
                               :plural "Partner emails"}
             :lieferant       {:attributes [[:name :string]] :plural "Lieferanten"}}
   :links   [[:partner-email-lieferant :partner-email :lieferant cardinality]]})

(defn- errors-for [cardinality]
  (:errors (res/problems
            (schema-with cardinality [:connected-one :partner-email-lieferant :name]))))

(deftest connected-one-is-refused-over-a-to-many-edge
  (testing "many-to-one from the FROM side is the legal case — no error"
    (is (nil? (errors-for :many-to-one))))

  (testing "one-to-one is legal from either side"
    (is (nil? (errors-for :one-to-one))))

  (testing "one-to-many from the FROM side can reach many ⇒ refused at the door"
    (let [errs (errors-for :one-to-many)]
      (is (= 1 (count errs)))
      (is (re-find #"can reach MANY" (first errs)))
      (is (re-find #"arg-max" (first errs))
          "and it names the operator that DOES handle a to-many edge")))

  (testing "many-to-many is refused too"
    (is (seq (errors-for :many-to-many))))

  (testing "the aggregating operators are untouched by the gate"
    (is (nil? (:errors (res/problems
                        (schema-with :one-to-many
                                     [:connected-sum :partner-email-lieferant :netto])))))))

(deftest the-gate-reads-the-side-not-just-the-link
  (let [s {:objects {:partner-email {:attributes
                                       [[:lieferant-name :string
                                         {:derived {:expression
                                                    [:connected-one :partner-emails :name]}}]]
                                       :plural "Partner emails"}
                     :lieferant       {:attributes [[:name :string]] :plural "Lieferanten"}}
           :links   [[:partner-emails :lieferant :partner-email :one-to-many]]}]
    (is (nil? (:errors (res/problems s)))
        "one-to-many walked from the TO side reaches exactly one — legal")))

(deftest derived-over-derived-cycle-across-links-is-refused
  (let [errs   #(:errors (res/problems %))
        cyclic {:objects {:a {:attributes [[:x :integer
                                            {:derived {:expression [:connected-sum :a-b :y]}}]]
                              :plural "As"}
                          :b {:attributes [[:y :integer
                                            {:derived {:expression [:connected-sum :b-a :x]}}]]
                              :plural "Bs"}}
                :links   [[:a-b :a :b :one-to-many]
                          [:b-a :b :a :one-to-many]]}
        chain  {:objects {:a {:attributes [[:x :integer
                                            {:derived {:expression [:connected-sum :a-b :y]}}]]
                              :plural "As"}
                          :b {:attributes [[:y :integer
                                            {:derived {:expression [:connected-sum :b-c :wert]}}]]
                              :plural "Bs"}
                          :c {:attributes [[:wert :integer]] :plural "Cs"}}
                :links   [[:a-b :a :b :one-to-many]
                          [:b-c :b :c :one-to-many]]}]
    (testing "a cycle a.x → b.y → a.x is refused, and the message names it"
      (let [es (errs cyclic)]
        (is (seq es))
        (is (some #(re-find #"derived cycle" %) es))))
    (testing "an ACYCLIC aggregate-over-derived (a.x over b.y over c.wert stored) is allowed"
      (is (not-any? #(re-find #"derived cycle" %) (or (errs chain) []))))))
