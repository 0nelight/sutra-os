(ns ^:unit grammar.coltype-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.types.coltype :as ctype]))

(deftest the-same-type-parses-the-same-from-every-dialect
  (testing "a decimal, however the store spells it"
    (is (= (ctype/parse "DECIMAL(38,10)") (ctype/parse "numeric(38,10)"))
        "the store's spelling and the one Postgres reads it back as")
    (is (= {:kind :decimal :precision 38 :scale 10} (ctype/parse "DECIMAL(38,10)"))))
  (testing "the other kinds"
    (is (= (ctype/parse "BIGINT") (ctype/parse "bigint") (ctype/parse "int8")))
    (is (= (ctype/parse "VARCHAR") (ctype/parse "character varying")))
    (is (= {:kind :varchar :length 36} (ctype/parse "VARCHAR(36)")))
    (is (= (ctype/parse "DOUBLE") (ctype/parse "double precision") (ctype/parse "float8")))
    (is (= {:kind :timestamp :precision 6} (ctype/parse "TIMESTAMP(6)")))))

(deftest an-unknown-type-parses-to-nil-rather-than-a-guess
  (is (nil? (ctype/parse "GEOGRAPHY")))
  (is (nil? (ctype/parse nil)))
  (is (nil? (ctype/parse "")))
  (is (false? (ctype/holds? nil (ctype/col-type :bigint)))
      "not knowing a type is not a reason to assume it fits"))

(deftest holds-crosses-the-spellings
  (testing "the case the old regex could not see"
    (is (true? (ctype/holds-text? "numeric(38,10)" "DECIMAL(38,2)"))
        "a column standing wider than the batch needs still holds it")
    (is (false? (ctype/holds-text? "numeric(38,2)" "DECIMAL(38,10)"))
        "narrower is a real mismatch — that one rounds the value on the way in"))
  (testing "kinds, across the two spellings"
    (is (true?  (ctype/holds-text? "bigint" "BIGINT")))
    (is (true?  (ctype/holds-text? "character varying" "VARCHAR")))
    (is (false? (ctype/holds-text? "bigint" "VARCHAR")))))

(deftest render-is-the-only-place-that-knows-a-spelling
  (let [dec (ctype/col-type :decimal {:precision 38 :scale 3})]
    (is (= "numeric(38,3)" (ctype/render dec :postgres))))
  (testing "postgres is spelled the way postgres answers, so a readback compares"
    (is (= "timestamp without time zone"
           (ctype/render (ctype/col-type :timestamp) :postgres)))
    (is (= (ctype/parse (ctype/render (ctype/col-type :timestamp) :postgres))
           (ctype/col-type :timestamp))
        "and it parses back to the same value it was rendered from")
    (testing "a FLOAT stays a float — widening is a declaration, not a spelling"
      (is (= "real" (ctype/render (ctype/col-type :float) :postgres)))
      (is (= "double precision" (ctype/render (ctype/col-type :double) :postgres)))))
  (testing "the store's shape drops a length and a precision — never a decimal's scale"
    (let [vc (ctype/col-type :varchar {:length 255})
          ts (ctype/col-type :timestamp {:precision 6})
          dc (ctype/col-type :decimal {:precision 38 :scale 3})]
      (is (= "character varying(255)" (ctype/render vc :postgres)))
      (is (= "character varying" (ctype/render (ctype/for-store vc) :postgres)))
      (is (= "timestamp without time zone" (ctype/render (ctype/for-store ts) :postgres)))
      (is (= "character varying" (ctype/render (ctype/for-store (ctype/col-type :text)) :postgres)))
      (is (= "numeric(38,3)" (ctype/render (ctype/for-store dc) :postgres)))))
  (testing "an unrenderable value is loud"
    (is (thrown? clojure.lang.ExceptionInfo (ctype/render {:kind :geography} :postgres)))
    (is (thrown? clojure.lang.ExceptionInfo (ctype/render (ctype/col-type :bigint) :oracle)))))
