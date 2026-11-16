(ns ^:unit platform.checkout-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.parser.checkout :as co]))

(def ^:private elements {:objects {:attributes :by-name}})

(def ^:private body
  {:objects {:a {:title "A" :attributes [[:nr :string] [:name :string]] :plural "As"}}
   :links   [[:a-b :a :b :many-to-one]]})

(deftest a-base-is-fingerprints-in-the-format-sidecars-keep
  (let [base (co/base-of body elements)]
    (is (= (co/fingerprint "A") (get-in base [[:objects "a"] :title])))
    (is (= (co/fingerprint [:nr :string]) (get-in base [[:objects "a"] [:attributes "nr"]]))
        "a declared cut: [field part-name] per part")
    (is (= #{:sutra.parser.checkout/whole} (set (keys (get base [:links "a-b"]))))
        "the element of a tuple keeps the keyword older sidecars hold")
    (is (contains? (get (co/base-of body) [:objects "a"]) :attributes)
        "without the declaration the field is one element")))

(deftest a-base-from-before-the-list-marker-still-merges
  (let [old    (update (co/base-of body elements) [:objects "a"] dissoc [:attributes])
        mine   (update-in body [:objects :a :attributes] conj [:farbe :string])
        theirs (update-in body [:objects :a :attributes] conj [:gewicht :double])
        {merged :body conflicts :conflicts} (co/merge3 old mine theirs {:elements elements})]
    (is (empty? conflicts))
    (is (= [[:nr :string] [:name :string] [:farbe :string] [:gewicht :double]]
           (get-in merged [:objects :a :attributes])))))

(deftest a-push-merges-over-its-pull
  (let [base (co/base-of body elements)]
    (testing "a conflict names its element"
      (is (= [{:node [:objects "a"] :field [:attributes "name"]}]
             (:conflicts (co/merge3 base
                                    (assoc-in body [:objects :a :attributes 1] [:name :text])
                                    (assoc-in body [:objects :a :attributes 1] [:name :long])
                                    {:elements elements})))))
    (testing ":resolve decides it"
      (is (= [:name :text]
             (get-in (:body (co/merge3 base
                                       (assoc-in body [:objects :a :attributes 1] [:name :text])
                                       (assoc-in body [:objects :a :attributes 1] [:name :long])
                                       {:elements elements :resolve :mine}))
                     [:objects :a :attributes 1]))))))

(deftest a-memory-is-laid-over-entry-by-entry
  (let [before {:placement {[:objects "a"] "ontology.ent" [:objects "b"] "ontology.ent"}}
        after  (co/memory-merge before {:placement {[:objects "b"] "p.objects.ent"}})]
    (is (= "ontology.ent" (get-in after [:placement [:objects "a"]])))
    (is (= [[[:objects "b"] "ontology.ent" "p.objects.ent"]] (co/moved before after)))))

(deftest a-fingerprint-ignores-map-and-set-order
  (is (= (co/fingerprint {:a 1 :b #{1 2 3}}) (co/fingerprint (array-map :b #{3 2 1} :a 1))))
  (is (not= (co/fingerprint [1 2]) (co/fingerprint [2 1]))))
