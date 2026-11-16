(ns ^:unit grammar.value-type-lowering-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.types.value-types :as vt]
            [sutra.parser.ent :as ent]))

(def ^:private vts
  {:eur-amount    {:type :decimal :description "Geld in EUR"}
   :import-status {:type :string :constraints {:one-of ["a" "b"]}
                   :description "Lebenszyklus"
                   :render-hints [:selectable :low-cardinality]
                   :format {:kind :id}}})

(defn- lower [attrs]
  (get-in (ent/validate-schema! {:value-types vts :objects {:o {:attributes attrs :plural "Os"}}}
                                "value-type-lowering-test")
          [:objects :o :attributes]))

(deftest name-slot-freezes-base-and-records-ref
  (testing "a value-type NAME in the type slot → field type = its base, ref recorded"
    (is (= [[:kosten :decimal {:value-type :eur-amount}]]
           (lower [[:kosten :eur-amount]]))))
  (testing "nothing beyond the physical type is frozen — not constraints, not
            :format, not :description, not :render-hints; all resolve live"
    (is (= [[:s :string {:value-type :import-status}]]
           (lower [[:s :import-status]])))))

(deftest explicit-field-type-plus-ref
  (testing "compatible explicit field type + :value-type opt → kept, both survive"
    (is (= [[:kosten :decimal {:value-type :eur-amount}]]
           (lower [[:kosten :decimal {:value-type :eur-amount}]]))))
  (testing "INCOMPATIBLE explicit field type throws (no silent storage change)"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"wraps its base type"
         (lower [[:kosten :string {:value-type :eur-amount}]])))))

(deftest plain-types-and-typos
  (testing "bare scalar + the parametric BASE types pass through unchanged"
    (is (= [[:a :string] [:b :decimal] [:c [:array :string]]]
           (lower [[:a :string] [:b :decimal] [:c [:array :string]]]))))
  (testing "an unknown keyword type is a typo → throws (closedness guard)"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"Unknown type"
         (lower [[:x :not-a-type]])))))

(deftest the-registry-is-the-only-copy
  (let [attrs [{:attribute-name :s :attribute-type :string
                :opts {:value-type :import-status}}
               {:attribute-name :own :attribute-type :string
                :opts {:value-type :import-status
                       :description "eigene Erklärung"
                       :render-hints [:searchable]
                       :format {:kind :id :decimals 3}}}]
        eff   (vt/resolve-value-types attrs vts)
        by    (fn [nm] (first (filter #(= nm (:attribute-name %)) eff)))]
    (testing "a bare ref inherits constraints, :format, :description, :render-hints"
      (is (= ["a" "b"] (get-in (by :s) [:opts :constraints :one-of])))
      (is (= {:kind :id} (get-in (by :s) [:opts :format])))
      (is (= "Lebenszyklus" (get-in (by :s) [:opts :description])))
      (is (= [:selectable :low-cardinality] (get-in (by :s) [:opts :render-hints]))))
    (testing "the attribute's OWN values win, per key"
      (is (= "eigene Erklärung" (get-in (by :own) [:opts :description])))
      (is (= [:searchable] (get-in (by :own) [:opts :render-hints]))
          "own hints win WHOLESALE — a union would make an inherited hint undroppable")
      (is (= {:kind :id :decimals 3} (get-in (by :own) [:opts :format]))))
    (testing "editing the registry moves every referencing property"
      (let [eff2 (vt/resolve-value-types
                  attrs (assoc-in vts [:import-status :format :kind] :currency))]
        (is (= :currency (get-in (first eff2) [:opts :format :kind]))
            "no re-install, no second copy — the ref reads through")))))
