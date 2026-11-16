(ns ^:unit ontology.expectations-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.types.value-types :as vt]
            [sutra.catalog.constraints :as cstr]
            [sutra.types.expr :as expr]))

(def ^:private brutto-stimmt
  {:name "brutto-stimmt"
   :expr [:approx= [:get :brutto]
          [:+ [:get :netto] [:get :zuschlag] [:get :steuer]]]
   :inputs [:brutto :netto :zuschlag :steuer]
   :on-error :warn})

(deftest approx=-tolerates-double-noise
  (testing ":approx= is cent-equal, immune to 1e-9 float noise; := is not"
    (is (expr/pred {} [:approx= 122.8 (+ 100.0 0.0 22.8)]))
    (is (expr/pred {} [:approx= 122.80 122.804]))
    (is (not (expr/pred {} [:approx= 122.80 122.9])))
    (is (expr/pred {} [:approx= "A" "A"]))))

(deftest holds-and-violates
  (testing "a satisfied check yields no violation"
    (is (empty? (cstr/expectation-violations
                 [brutto-stimmt]
                 {:brutto 122.80 :netto 100.0 :zuschlag 0.0 :steuer 22.80}))))
  (testing "a violated check yields one violation carrying name + on-error"
    (let [vs (cstr/expectation-violations
              [brutto-stimmt]
              {:brutto 130.0 :netto 100.0 :zuschlag 0.0 :steuer 22.80})]
      (is (= 1 (count vs)))
      (is (= "brutto-stimmt" (:check (first vs))))
      (is (= :warn (:on-error (first vs)))))))

(deftest skips-uncomputable-rows
  (testing "absent declared :input ⇒ skipped (partial update, not a violation)"
    (is (empty? (cstr/expectation-violations
                 [brutto-stimmt]
                 {:brutto 130.0 :netto 100.0}))))
  (testing "present-but-nil operand ⇒ eval throws ⇒ skipped (left to :required)"
    (is (empty? (cstr/expectation-violations
                 [brutto-stimmt]
                 {:brutto 130.0 :netto nil :zuschlag 0.0 :steuer 22.80})))))

(deftest on-error-defaults-to-fail
  (testing "an omitted :on-error is :fail (fail-closed, like a constraint)"
    (let [vs (cstr/expectation-violations
              [{:name "x" :expr [:= [:get :a] 1] :inputs [:a]}]
              {:a 2})]
      (is (= :fail (:on-error (first vs)))))))

(def ^:private vt-attrs
  [{:attribute-name :status :attribute-type :string :opts {:value-type :import-status}}
   {:attribute-name :note   :attribute-type :string
    :opts {:value-type :import-status :constraints {:max-length 4}}}
   {:attribute-name :plain  :attribute-type :string :opts {}}])

(deftest resolve-value-types-merges-live-constraints
  (let [vts {"ri.vt.1" {:value-type-name :import-status :constraints {:one-of ["a" "b"]}}}
        eff (vt/resolve-value-types vt-attrs vts)]
    (testing "a referencing attr gains the value-type's constraints"
      (is (= {:one-of ["a" "b"]} (get-in (first eff) [:opts :constraints]))))
    (testing "the attribute's OWN constraints win / union (attr tightens, VT supplies rest)"
      (is (= {:one-of ["a" "b"] :max-length 4} (get-in (second eff) [:opts :constraints]))))
    (testing "an attr with no ref is untouched"
      (is (= {} (:opts (nth eff 2)))))
    (testing "empty registry ⇒ no-op"
      (is (= vt-attrs (vt/resolve-value-types vt-attrs {}))))))

(deftest value-type-constraints-are-enforced-and-versioned
  (let [vts1 {"ri.vt.1" {:value-type-name :import-status :constraints {:one-of ["a" "b"]}}}
        vts2 {"ri.vt.1" {:value-type-name :import-status :constraints {:one-of ["a" "b" "x"]}}}]
    (testing "value-type :one-of is enforced at the write chokepoint"
      (is (= [:one-of]
             (map :constraint (cstr/violations (vt/resolve-value-types vt-attrs vts1)
                                               {:status "x"})))))
    (testing "editing the value-type propagates (versioning) — same attrs, now valid"
      (is (empty? (cstr/violations (vt/resolve-value-types vt-attrs vts2) {:status "x"}))))))
