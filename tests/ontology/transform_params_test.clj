(ns ^:unit ontology.transform-params-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.parser.params :as params]
            [sutra.parser.board :as board]
            [sutra.parser.spec :as spec]))

(def ^:private ps
  {:cutoff {:type :string :default "2026-01-01"}
   :minimum {:type :number :default 100}
   :live?  {:type :boolean :default true}})

(deftest a-string-parameter-cannot-escape-its-own-quotes
  (is (= "'O''Brien'" (params/sql-literal :string "O'Brien")))
  (is (= "'; DROP TABLE invoices; --'"
         (params/sql-literal :string "; DROP TABLE invoices; --"))
      "an injection attempt lands INSIDE a quoted literal, not beside it"))

(deftest a-number-parameter-refuses-a-non-number
  (is (= "42" (params/sql-literal :number 42)))
  (is (= "42" (params/sql-literal :number "42")) "a value off a query param is a string")
  (is (= "-1.5" (params/sql-literal :number "-1.5")))
  (is (thrown? clojure.lang.ExceptionInfo (params/sql-literal :number "1; DROP TABLE x")))
  (is (thrown? clojure.lang.ExceptionInfo (params/sql-literal :number "abc"))))

(deftest a-boolean-renders-as-sql-and-an-absent-value-as-NULL
  (is (= "TRUE" (params/sql-literal :boolean true)))
  (is (= "FALSE" (params/sql-literal :boolean "false")))
  (is (= "NULL" (params/sql-literal :string nil))
      "NULL, not '' — an empty string is a value and NULL is the absence of one"))

(deftest substitution-fills-every-reference-with-a-typed-literal
  (is
   (= "SELECT * FROM t WHERE d >= '2026-01-01' AND n >= 100 AND live = TRUE"
      (params/substitute ps
                         "SELECT * FROM t WHERE d >= ${cutoff} AND n >= ${minimum} AND live = ${live?}")))
  (testing "the same name twice is filled twice"
    (is (= "a '2026-01-01' b '2026-01-01'"
           (params/substitute ps "a ${cutoff} b ${cutoff}"))))
  (testing "whitespace inside the braces is not part of the name"
    (is (= "'2026-01-01'" (params/substitute ps "${ cutoff }")))))

(deftest a-transform-that-declares-nothing-is-untouched
  (let [sql "SELECT '$5.00' AS price, ${not-a-param} FROM t"]
    (is (= sql (params/substitute {} sql)))
    (is (= sql (:query (params/substitute-transform {:query sql}))))))

(deftest an-undeclared-reference-throws-instead-of-blanking
  (let [e (try (params/substitute ps "SELECT * FROM t WHERE x = ${typo}") nil
               (catch clojure.lang.ExceptionInfo ex ex))]
    (is (some? e))
    (is (= [:typo] (:unknown (ex-data e))))
    (is (re-find #"\$\{typo\}" (ex-message e)) "the message names the token")))

(deftest a-parameter-without-a-value-parses-but-cannot-be-built
  (let [placeholder {:tenant {:type :string}}]
    (is (= [:tenant] (params/valueless placeholder)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"no value"
                          (params/substitute placeholder "SELECT ${tenant}")))
    (testing "…and a declared-but-unused placeholder blocks nothing"
      (is (= "SELECT 1" (params/substitute placeholder "SELECT 1"))))))

(deftest only-the-sql-body-is-substituted-never-code
  (let [t {:language :python :code "x = '${cutoff}'" :parameters ps}]
    (is (= "x = '${cutoff}'" (:code (params/substitute-transform t))))))

(deftest an-override-changes-the-value-without-changing-the-transform
  (let [t {:query "SELECT ${minimum}" :parameters ps}]
    (is (= "SELECT 500"
           (:query (params/substitute-transform (params/with-overrides t {:minimum 500})))))
    (is (= "SELECT 100" (:query (params/substitute-transform t)))
        "the original is untouched — a preview must not mutate the pipeline"))
  (testing "an override for a name the transform does not declare is IGNORED"
    (let [t {:query "SELECT ${minimum}" :parameters ps}]
      (is (= ps (:parameters (params/with-overrides t {:invented 1})))))))

(deftest an-override-is-still-checked-against-the-declared-type
  (let [t {:query "SELECT ${minimum}" :parameters ps}]
    (is (thrown? clojure.lang.ExceptionInfo
                 (params/substitute-transform (params/with-overrides t {:minimum "'; DROP"}))))))

(deftest errors-names-both-halves-that-can-disagree
  (testing "an unknown type"
    (is (seq (params/errors :t {:parameters {:x {:type :date}}}))))
  (testing "a default that does not fit its type"
    (is (seq (params/errors :t {:parameters {:x {:type :number :default "abc"}}}))))
  (testing "a body referencing what nobody declared"
    (is (seq (params/errors :t {:query "SELECT ${nope}" :parameters {}}))))
  (testing "a well-formed pair is silent"
    (is (empty? (params/errors :t {:query "SELECT ${cutoff}" :parameters ps})))))

(deftest the-validator-refuses-a-transform-whose-body-references-a-typo
  (let [good {:transform-name :t :language :pg-sql :output {:kind :dataset :dataset :d}
              :inputs {:a {:kind :dataset :dataset :src}}
              :query "SELECT * FROM a WHERE d >= ${cutoff}"
              :parameters {:cutoff {:type :string :default "2026-01-01"}}}
        bad  (assoc good :query "SELECT * FROM a WHERE d >= ${cutof}")]
    (is (spec/transform-valid? good))
    (is (not (spec/transform-valid? bad)))
    (is (re-find #"cutof" (spec/transform-error bad))
        "the rejection names the offending parameter, not 'malformed transform'")))

(deftest a-board-param-op-lowers-to-the-token-not-to-the-value
  (let [{:keys [query parameters]}
        (board/board->sql {:inputs {:a {:kind :dataset :dataset :invoices}}
                           :parameters ps
                           :ops [{:op :from :input :a}
                                 {:op :filter :where [:>= :netto [:param :minimum]]}]}
                          (constantly [{:name :netto :type :long}]))]
    (is (re-find #"\$\{minimum\}" query) "the compiled SQL carries the reference")
    (is (= ps parameters) "…and the declarations ride out with it, or nothing resolves it")
    (testing "and the shared substituter finishes the job — one meaning, two authors"
      (is (re-find #">= 100" (params/substitute parameters query))))))

(deftest the-param-op-is-in-the-editors-catalogue
  (is (some? (board/expr-op-spec :param)))
  (is (= :param (:kind (first (:args (board/expr-op-spec :param)))))))
