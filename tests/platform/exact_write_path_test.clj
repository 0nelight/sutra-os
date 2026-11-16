(ns ^:unit platform.exact-write-path-test
  (:require [sutra.builds.import.coerce :as coerce]
            [clojure.test :refer [deftest testing is]]
            [sutra.store.pg.landing :as landing]
            [sutra.types.coltype :as ctype]))

(defn- store-type-of [v]
  (ctype/render (ctype/for-store (landing/storable (landing/type-of v))) :postgres))
(defn- col-types-sql [rows]
  (update-vals (landing/col-types rows)
               #(ctype/render (ctype/for-store (landing/storable %)) :postgres)))

(deftest an-integral-amount-gets-no-scale-nobody-declared
  (is (= "numeric(38,0)" (store-type-of 40M))
      "the source of the \"40.0000000000\" that reached the JSON wire")
  (is (= {"betrag" "numeric(38,0)"} (col-types-sql [{"betrag" 40M}]))))

(deftest more-than-ten-decimals-are-not-truncated
  (testing "the constant cut a value's tail off in the write path, in silence"
    (is (= "numeric(38,12)" (store-type-of 0.123456789012M)))
    (is (= "numeric(38,18)" (store-type-of 1.234567890123456789M)))))

(deftest the-scale-is-the-widest-in-the-BATCH-not-the-first-rows
  (testing "a column is one type over every row, so row 1 must not round row 2"
    (is (= {"betrag" "numeric(38,3)"}
           (col-types-sql [{"betrag" 40M} {"betrag" 40.125M} {"betrag" 7.5M}])))
    (testing "and the order of the rows does not change the answer"
      (is (= {"betrag" "numeric(38,3)"}
             (col-types-sql [{"betrag" 40.125M} {"betrag" 40M}]))))))

(deftest an-exponent-is-a-shift-and-not-a-fraction
  (is (= "numeric(38,0)" (store-type-of (BigDecimal. "1E+3"))))
  (is (= "numeric(38,1)" (store-type-of (BigDecimal. "0.5")))
      "a value below 1 still needs its leading zero counted"))

(deftest the-integer-half-wins-when-38-digits-are-not-enough
  (testing "a value that lost its low-order digits is still the amount it was;
            one that lost the high-order ones is a different number"
    (let [wide (BigDecimal. (apply str (repeat 38 "9")))]
      (is (= "numeric(38,0)" (store-type-of wide)))
      (is (= {"c" "numeric(38,0)"} (col-types-sql [{"c" wide} {"c" 1.55M}]))))))

(deftest every-other-column-still-takes-the-FIRST-non-nil-value
  (testing "nothing here may retype what an earlier batch already wrote"
    (is (= {"a" "character varying"} (col-types-sql [{"a" "x"} {"a" 1.5M}]))
        "a string first means a VARCHAR column, decimal or no decimal")
    (is (= {"a" "numeric(38,1)"} (col-types-sql [{"a" 1.5M} {"a" "x"}])))
    (is (= {"a" "bigint" "b" "boolean"} (col-types-sql [{"a" 7 "b" true}])))
    (is (= {"a" "character varying"} (col-types-sql [{"a" nil} {"a" nil}]))
        "an all-nil column still gets a column — the Binder Error `col-types`
         documents")))

(def ^:private coercion-fn @#'coerce/coercion-fn)

(def ^:private a-string-double-cannot-hold
  "12345678901234567.89")

(deftest an-amount-arriving-as-a-STRING-keeps-its-cent
  (doseq [t [:decimal :number [:decimal 12 2] [:decimal 38 10]]]
    (testing (str "declared " (pr-str t))
      (let [f (coercion-fn t)]
        (is (instance? BigDecimal (f a-string-double-cannot-hold)))
        (is (= a-string-double-cannot-hold (str (f a-string-double-cannot-hold)))
            "every digit — the old cast answered 1.2345678901234568E16")
        (is (= "1234.56" (str (f "1234.56"))) "and the ordinary case is exact too")))))

(deftest a-DOUBLE-column-still-gets-a-double
  (testing "the change is by TARGET type; :float/:double declare inexactness and
            'repairing' them would trade a real type for a heavier one"
    (doseq [t [:float :double]]
      (let [f (coercion-fn t)]
        (is (instance? Double (f a-string-double-cannot-hold)))
        (is (= (Double/parseDouble a-string-double-cannot-hold)
               (f a-string-double-cannot-hold)))))))

(deftest an-INTEGER-column-still-gets-a-long
  (doseq [t [:byte :short :integer :long]]
    (is (= 42 ((coercion-fn t) "42")))
    (is (instance? Long ((coercion-fn t) "42")))))

(deftest a-cast-never-turns-a-BAD-value-into-a-plausible-one
  (testing "the contract `coerce-number` already had, kept on the exact arm"
    (let [f (coercion-fn :decimal)]
      (is (= "n/a" (f "n/a")))
      (is (= "1.234,56" (f "1.234,56")) "a locale format is the transform's job")
      (is (nil? (f nil))))))

(deftest a-value-that-is-ALREADY-a-number-passes-through
  (testing "what a double lost it lost before it got here; re-parsing its `str`
            would dress the loss up as a decision"
    (let [f (coercion-fn :decimal)]
      (is (= 1.5 (f 1.5)))
      (is (instance? Double (f 1.5)))
      (is (= 7 (f 7))))))

(deftest the-cast-TABLE-is-unchanged-for-every-non-numeric-type
  (is (nil? (coercion-fn :uuid)))
  (is (nil? (coercion-fn :keyword)))
  (is (some? (coercion-fn :boolean)))
  (is (some? (coercion-fn :date))))

(deftest a-numeric-column-holds-every-value-the-batch-carried
  (testing "row ORDER no longer decides the column type"
    (doseq [[a b] [[7 1.5] [7 1.5M] [1.5 1.25M]]]
      (is (= (get (landing/col-types [{"a" a} {"a" b}]) "a")
             (get (landing/col-types [{"a" b} {"a" a}]) "a"))
          (str "widening " (pr-str a) " with " (pr-str b) " must not depend on order"))))
  (testing "and it widens rather than keeping the first"
    (is (= (ctype/col-type :double) (get (landing/col-types [{"a" 7} {"a" 1.5}]) "a"))
        "a double in a bigint column was the value this used to write")
    (is (= :decimal (:kind (get (landing/col-types [{"a" 7} {"a" 1.5M}]) "a"))))
    (is (= (ctype/col-type :double) (get (landing/col-types [{"a" 1.25M} {"a" 1.5}]) "a"))
        "double is wider than decimal — only it holds every value in the batch"))
  (testing "a NON-numeric first value still wins outright"
    (is (= (ctype/col-type :varchar) (get (landing/col-types [{"a" "x"} {"a" 1.5M}]) "a")))
    (is (= :decimal (:kind (get (landing/col-types [{"a" 1.5M} {"a" "x"}]) "a"))))))
