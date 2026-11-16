(ns ^:unit platform.cli-action-param-types-test
  (:require [clojure.test :refer [deftest is testing are]]
            [sutra.types.submission :as csub]
            [sutra.ref.params :as rp]))

(def ^:private params
  {:jahr       {:type :integer :name "Jahr"}
   :verkauft   {:type :decimal :name "Umsatz verkauft"}
   :quote      {:type :number}
   :aktiv?     {:type :boolean}
   :datum      {:type :date}
   :stichtag-id {:type :string}
   :ziele      {:type :integer :multiple true}})

(deftest text-is-read-against-the-declared-type
  (testing "numbers arrive as numbers, not as strings"
    (let [out (csub/decode-params-text params {:jahr "2026" :verkauft "1333333.33" :quote "0.75"})]
      (is (= 2026 (:jahr out)))
      (is (= 1333333.33 (:verkauft out)))
      (is (= 0.75 (:quote out)))
      (is (every? number? (vals out)) "nothing left as text")))

  (testing "booleans spell out in both languages"
    (are [txt v] (= v (:aktiv? (csub/decode-params-text params {:aktiv? txt})))
      "true" true "false" false "ja" true "nein" false "1" true "0" false))

  (testing "a :multiple parameter decodes element-wise"
    (is (= [1 2 3] (:ziele (csub/decode-params-text params {:ziele ["1" "2" "3"]})))))

  (testing "an integer parameter refuses a fractional text — it is not an integer"
    (is (= "1.5" (:jahr (csub/decode-params-text params {:jahr "1.5"})))
        "left as it came, for the server to refuse in one wording")))

(deftest types-that-legitimately-travel-as-text-are-untouched
  (testing "a date is an ISO string on the wire"
    (is (= "2026-08-31" (:datum (csub/decode-params-text params {:datum "2026-08-31"})))))

  (testing "a uuid-shaped value on a :string parameter STAYS a string"
    (let [u "4c25d8e9-1f1a-4a5e-9a0f-2b7c3d4e5f60"]
      (is (= u (:stichtag-id (csub/decode-params-text params {:stichtag-id u})))
          "declared :string, so it stays text")))

  (testing "a parameter the action does not declare passes through untouched"
    (is (= {:unbekannt "17"} (csub/decode-params-text params {:unbekannt "17"})))))

(deftest the-door-names-what-it-refuses
  (testing "a string on a numeric parameter is a problem, listed per parameter"
    (let [probs (rp/type-problems (java.time.ZoneId/systemDefault) params
                                  {:jahr "2026" :verkauft 1.0})]
      (is (= [{:parameter :jahr :expected :integer :value "2026"}] probs))))

  (testing "the refusal names the parameter, the type it wanted and what it got"
    (let [probs (rp/type-problems (java.time.ZoneId/systemDefault) params {:verkauft "1470000"})
          msg   (csub/type-refusal :stichtag-festschreiben params probs)]
      (is (re-find #"Umsatz verkauft" msg) "uses the parameter's label")
      (is (re-find #"decimal" msg) "says the type it wanted")
      (is (re-find #"1470000" msg) "shows the value it got")))

  (testing "typed values raise no problem at all"
    (is (empty? (rp/type-problems (java.time.ZoneId/systemDefault) params
                                  {:jahr 2026 :verkauft 1470000.0
         :aktiv? true :datum "2026-08-31"
         :ziele [1 2]})))))
