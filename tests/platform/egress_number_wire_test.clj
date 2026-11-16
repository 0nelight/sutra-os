(ns ^:unit platform.egress-number-wire-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.data.json :as json]
            [sutra.sources.egress :as egress]))

(defn- body
  [tmpl params]
  (json/write-str (egress/render tmpl params)))

(deftest an-exact-value-leaves-with-every-digit-and-its-scale
  (testing "a decimal amount keeps the cents a double would have eaten"
    (is (= "{\"betrag\":12345678901234567.89}"
           (body {"betrag" "{amount}"} {:amount 12345678901234567.89M}))))
  (testing "the DECLARED SCALE survives — a NUMERIC(10,2) holding ten euro"
    (is (= "{\"betrag\":10.00}" (body {"betrag" "{amount}"} {:amount 10.00M}))))
  (testing "a big integer id is not rounded to the nearest double"
    (is (= "{\"id\":9007199254740993}"
           (body {"id" "{n}"} {:n (biginteger 9007199254740993)}))))
  (testing "a Long id likewise"
    (is (= "{\"id\":58319870951433}" (body {"id" "{n}"} {:n 58319870951433})))))

(deftest we-do-not-restate-our-own-encoding-at-a-door-we-do-not-own
  (testing "an exact value stays a JSON NUMBER — json-by-type's string would be
            a 400 against a schema that declares a number"
    (is (number? (get (json/read-str (body {"t" "{v}"} {:v 0.2M})) "t")))
    (is (number? (get (json/read-str (body {"t" "{v}"} {:v (biginteger 42)})) "t"))))
  (testing "and an inexact one is untouched, as everywhere else"
    (is (= "{\"t\":0.2}" (body {"t" "{v}"} {:v 0.2})))))

(deftest a-lone-placeholder-yields-the-param-as-the-ontology-typed-it
  (is (= 42 (egress/render "{id}" {:id 42})))
  (is (= 12.50M (egress/render "{amount}" {:amount 12.50M})))
  (is (= "acme" (egress/render "{name}" {:name "acme"})))
  (testing "and anything AROUND the placeholder makes it a JSON string, which is
            also the gap: there is no template that spells the value's own
            digits as a string, so 'send this number as a string' is not
            something an author can say — see render's docstring"
    (is (= "{\"id\":42}"        (body {"id" "{id}"}      {:id 42})))
    (is (= "{\"id\":\"id-42\"}" (body {"id" "id-{id}"}   {:id 42})))))

(deftest an-exact-value-spliced-into-a-string-spells-its-digits-plainly
  (testing "str would mint an exponent the author never wrote"
    (is (= "1E+20" (str (java.math.BigDecimal. "1E+20"))))
    (is (= "/api/x/100000000000000000000"
           (egress/render "/api/x/{id}" {:id (java.math.BigDecimal. "1E+20")}))))
  (testing "and the declared scale still survives the splice"
    (is (= "betrag=10.00" (egress/render "betrag={a}" {:a 10.00M}))))
  (testing "a BigInteger too"
    (is (= "/n/9007199254740993"
           (egress/render "/n/{n}" {:n (biginteger 9007199254740993)}))))
  (testing "a Double keeps str — its exponent is the host's honest spelling of
            an inexact value, and its .0 says the same"
    (is (= "x=1.0"    (egress/render "x={v}" {:v 1.0})))
    (is (= "x=1.0E20" (egress/render "x={v}" {:v 1.0E20}))))
  (testing "everything else is spelled as before"
    (is (= "a-1-b" (egress/render "a-{x}-b" {:x 1})))
    (is (= "a--b"  (egress/render "a-{x}-b" {:x nil})))
    (is (= "a-s-b" (egress/render "a-{x}-b" {:x "s"})))))

(deftest render-still-walks-the-whole-template
  (is (= {"outer" {"inner" [1 "x-2"]}}
         (egress/render {"outer" {"inner" ["{a}" "x-{b}"]}} {:a 1 :b 2})))
  (testing "a template with no placeholder is left exactly as authored"
    (is (= {"mode" "replace"} (egress/render {"mode" "replace"} {})))))
