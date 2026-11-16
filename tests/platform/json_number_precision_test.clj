(ns ^:unit platform.json-number-precision-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.data.json :as json]
            [sutra.http.responses :as responses]))

(defn- wire
  [v]
  (-> (responses/json-response 200 {:v v}) :body (json/read-str) (get "v")))

(deftest documented-examples-row-by-row
  (testing "Byte 31, Short 8739, Integer 238940 — number"
    (is (= 31 (wire (byte 31))))
    (is (number? (wire (byte 31))))
    (is (= 8739 (wire (short 8739))))
    (is (number? (wire (short 8739))))
    (is (= 238940 (wire (int 238940))))
    (is (number? (wire (int 238940)))))
  (testing "Long 58319870951433 — string, though it is far below 2^53−1"
    (is (= "58319870951433" (wire 58319870951433))))
  (testing "Decimal 2.718281828 — string, though a double carries it exactly"
    (is (= "2.718281828" (wire 2.718281828M))))
  (testing "Double / Float 3.14159265 — number"
    (is (number? (wire 3.14159265)))
    (is (= 3.14159265 (wire 3.14159265)))
    (is (number? (wire (float 3.14159265))))))

(deftest exact-types-are-strings-at-every-magnitude
  (testing "the value decides nothing — a Long is a string from 0 upward"
    (doseq [v [0 1 -1 42 1000000 9007199254740991 9007199254740993]]
      (is (= (str v) (wire v)) (str v " is a Long and must ride as a string"))))
  (testing "BigInteger and BigInt keep every digit"
    (is (= "99999999999999999999" (wire (biginteger 99999999999999999999N))))
    (is (= "99999999999999999999" (wire 99999999999999999999N))))
  (testing "BigDecimal, integral or fractional, near or far"
    (is (= "1234.56" (wire 1234.56M)))
    (is (= "0.1" (wire 0.1M)))
    (is (= "12345678901234567.89" (wire 12345678901234567.89M)))
    (is (= "9007199254740993" (wire 9007199254740993M))))
  (testing "the declared scale survives — a price does not lose its cents column"
    (is (= "10.00" (wire (BigDecimal. "10.00")))))
  (testing "no scientific notation — a consumer parses the digits it was sent"
    (is (= "100000000000000000000" (wire (bigdec 1e20M))))))

(deftest inexact-types-stay-bare-numbers
  (testing "Double and Float have nothing left to lose to a JSON number"
    (doseq [v [3.5 -0.25 0.0 1.0e20 (float 1.5)]]
      (is (number? (wire v)) (str v " must not change shape"))))
  (testing "Integer/Short/Byte are provably double-safe (Integer-Max = 2^31−1)"
    (is (= 2147483647 (wire (int 2147483647))))
    (is (= -2147483648 (wire (int -2147483648))))))

(deftest the-rule-reaches-every-position-not-just-map-values
  (testing "inside vectors and nested maps — a data.json :value-fn would miss these"
    (let [body {:rows [{:sum 12345678901234567.89M :n 9007199254740993}
                       {:sum 1234.56M              :n 7}]
                :deep {:a [[9007199254740993]]}
                :int  (int 238940)}
          out  (json/read-str (:body (responses/json-response 200 body)))]
      (is (= "12345678901234567.89" (get-in out ["rows" 0 "sum"])))
      (is (= "9007199254740993"     (get-in out ["rows" 0 "n"])))
      (is (= "1234.56"              (get-in out ["rows" 1 "sum"])))
      (is (= "7"                    (get-in out ["rows" 1 "n"])))
      (is (= "9007199254740993"     (get-in out ["deep" "a" 0 0])))
      (is (= 238940                 (get out "int"))))))

(deftest everything-that-is-not-a-number-is-left-alone
  (let [body {:error "no branch" :ok true :nothing nil :items ["a" "b"] :kw :x}
        out  (json/read-str (:body (responses/json-response 409 body)))
        resp (responses/json-response 409 body)]
    (is (= {"error" "no branch" "ok" true "nothing" nil "items" ["a" "b"] "kw" "x"} out))
    (testing "status and Content-Type are untouched by the number rule"
      (is (= 409 (:status resp)))
      (is (= "application/json" (get-in resp [:headers "Content-Type"]))))))
