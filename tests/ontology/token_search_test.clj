(ns ^:unit ontology.token-search-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.ref.expr :as expr]
            [sutra.ref.tokens :as tokens]
            [clojure.string :as str]
            [sutra.store.pg.select :as select]
            [sutra.store.pg.sql :as sql]
            [sutra.store.pg.tokens :as pg-tokens]
            [sutra.types.object-set :as os]
            [sutra.types.sql-ident :as sqli]
            [sutra.types.query :as q]))

(deftest a-value-is-cut-at-whitespace-and-punctuation-and-folded
  (is (= ["a" "2024" "0042"] (tokens/tokens "A-2024-0042")))
  (is (= ["müller" "gmbh"] (tokens/tokens "MÜLLER, GmbH"))))

(deftest a-word-is-a-token-or-its-start
  (is (tokens/match? ["The Quick Brown Fox"] "quick"))
  (is (tokens/match? ["The Quick Brown Fox"] "qui"))
  (is (tokens/match? ["The Quick" "Brown Fox"] "fox the") "over all attrs, in any order")
  (is (not (tokens/match? ["The Quick Brown Fox"] "rown")) "the middle of a token is no match"))

(deftest words-are-searched-independently-as-in-the-global-search-bar
  (is (tokens/match? ["yellow car"] "yellow cab"))
  (is (tokens/match? ["red cab"] "yellow cab"))
  (is (not (tokens/match? ["red car"] "yellow cab")))
  (testing "AND, OR, NOT and brackets change that"
    (is (not (tokens/match? ["yellow car"] "yellow AND cab")))
    (is (tokens/match? ["yellow car"] "yellow NOT cab"))
    (is (not (tokens/match? ["yellow car"] "NOT yellow")))
    (is (tokens/match? ["red car"] "(yellow OR red) AND car")))
  (testing "a word of several tokens asks for all of them"
    (is (tokens/match? ["A-2024-0042"] "a-2024"))
    (is (not (tokens/match? ["A-2023-0042"] "a-2024")))))

(deftest a-quoted-phrase-stands-in-order
  (is (tokens/match? ["yellow cab company"] "\"yellow cab\""))
  (is (not (tokens/match? ["cab yellow"] "\"yellow cab\"")))
  (is (not (tokens/match? ["yellow big cab"] "\"yellow cab\""))))

(deftest text-without-a-token-matches-nothing
  (is (not (tokens/match? ["100 %"] "%")))
  (is (not (tokens/match? ["x"] ""))))

(deftest the-kernel-judges-the-predicate
  (is (q/valid-predicate? [:matches-tokens [:a :b] "x"]))
  (is (not (q/valid-predicate? [:matches-tokens :a "x"])))
  (is (expr/match? {:values {:a "Stücklisten Zeile" :b nil}} [:matches-tokens [:a :b] "stück"]))
  (is (not (expr/match? {:values {:a "Zeile"}} [:matches-tokens [:a] "eile"]))))

(deftest the-search-text-cannot-leave-its-quotes
  (is (= "('a':* & 'b':*)" (pg-tokens/tsquery "a'b")))
  (is (= "(('a' <-> 'b') | ('c':*))" (pg-tokens/tsquery "\"a b\" c")))
  (is (= "(('a':*) & !('b':*))" (pg-tokens/tsquery "a AND NOT b")))
  (testing "no token, no query"
    (is (nil? (pg-tokens/tsquery "%'\\")))))

(deftest a-read-writes-the-indexs-expression
  (let [sig {:types {"T" {:attrs [{:name :a :type :string} {:name :derived-b :type :string}]}}}
        cx  {:sig sig :denied #{} :rel (comp sql/frag sqli/quote-ident)
             :edges (fn [_] {:from (sql/frag "e") :derived? false})}
        ir  (os/qt->ir {:root "T" :where [:matches-tokens [:a] "x"]})
        got (:sql (select/compile-plan cx ir))]
    (is (str/includes? got (pg-tokens/document [(str "t." (sqli/quote-ident "a"))
                                                (str "t." (sqli/quote-ident "derived_b"))]))
        "over every text column of the type, though the predicate leaves the derived one out")))

(deftest a-field-search-reads-only-its-fields
  (is (q/valid-predicate? [:matches-tokens-in [:a] "x"]))
  (is (expr/match? {:values {:a "BA26.0342" :b "Lager"}} [:matches-tokens-in [:a :b] "0342 lag"]))
  (let [sig {:types {"T" {:attrs [{:name :a :type :string} {:name :b :type :string}]}}}
        cx  {:sig sig :denied #{} :rel (comp sql/frag sqli/quote-ident)
             :edges (fn [_] {:from (sql/frag "e") :derived? false})}
        ir  (os/qt->ir {:root "T" :where [:matches-tokens-in [:a] "x"]})
        got (:sql (select/compile-plan cx ir))]
    (is (str/includes? got (pg-tokens/document [(str "t." (sqli/quote-ident "a"))])))
    (is (not (str/includes? got (pg-tokens/document [(str "t." (sqli/quote-ident "a"))
                                                     (str "t." (sqli/quote-ident "b"))]))))))

(deftest a-search-within-a-type-asks-for-every-word
  (is (= "(anna AND großmann)" (tokens/all-words "anna großmann")))
  (is (= "anna" (tokens/all-words "anna")))
  (is (nil? (tokens/all-words "  ")))
  (is (not (tokens/match? ["anna berger"] (tokens/all-words "anna großmann"))))
  (is (tokens/match? ["großmann" "anna"] (tokens/all-words "anna großmann")))
  (testing "quotes and operators keep their meaning"
    (is (tokens/match? ["yellow cab berlin"] (tokens/all-words "\"yellow cab\" berlin")))
    (is (not (tokens/match? ["cab yellow berlin"] (tokens/all-words "\"yellow cab\" berlin"))))
    (is (tokens/match? ["red car"] (tokens/all-words "yellow OR red")))
    (is (not (tokens/match? ["yellow cab"] (tokens/all-words "yellow NOT cab"))))))

(deftest relevance-orders-by-the-rank-of-the-querys-text
  (let [sig {:types {"T" {:attrs [{:name :a :type :string} {:name :n :type :integer}]}}}
        cx  {:sig sig :denied #{} :rel (comp sql/frag sqli/quote-ident)
             :edges (fn [_] {:from (sql/frag "e") :derived? false})}
        doc (pg-tokens/document [(str "t." (sqli/quote-ident "a"))])
        plan (fn [m] (select/compile-plan cx (os/qt->ir (merge {:root "T" :limit 5} m))))
        got (plan {:where [:matches-tokens [:a] "x"] :order-by q/relevance :sort-dir :desc})]
    (is (str/includes? (:sql got)
                       (str "ORDER BY ts_rank_cd(" doc ", to_tsquery('simple'::regconfig, ?), 2) DESC")))
    (testing "without a text there is nothing to rank by"
      (is (not (str/includes? (:sql (plan {:order-by q/relevance})) "ts_rank_cd"))))))

(deftest the-schema-says-which-tables-are-indexed
  (let [sig {:types {"T" {:attrs [{:name :z :type :text} {:name :a :type :string}
                                  {:name :n :type :integer}]}
                     "N" {:attrs [{:name :n :type :integer}]}}}]
    (is (= [["d_T" [:a :z]]] (pg-tokens/searchable-tables sig))
        "attrs in the order a keyword read names them, no table without text"))
  (let [sig {:types {}}
        t0  1000000000]
    (testing "the first read looks"
      (is (pg-tokens/sweep-due? nil sig t0)))
    (testing "the same schema waits for the recheck"
      (is (not (pg-tokens/sweep-due? {:sig sig :at t0} sig (+ t0 120000))))
      (is (not (pg-tokens/sweep-due? {:sig sig :at t0} {:types {}} (+ t0 120000))) "equal is same")
      (is (pg-tokens/sweep-due? {:sig sig :at t0} sig (+ t0 pg-tokens/recheck-ms 1))))
    (testing "a changed schema is seen within a minute"
      (is (not (pg-tokens/sweep-due? {:sig sig :at t0} {:types {"T" {}}} (+ t0 1000))))
      (is (pg-tokens/sweep-due? {:sig sig :at t0} {:types {"T" {}}} (+ t0 61000))))))

(deftest a-tables-indexes-become-the-one-wanted
  (let [wanted (pg-tokens/wanted-index "d_t" [:a :b])
        nm     (:name wanted)]
    (testing "it stands and pg knows its statistics: nothing to do"
      (is (empty? (pg-tokens/statements "d_t" wanted {nm true "d_t_kw_old" true} true))))
    (testing "it stands without statistics: analyze only"
      (is (= ["ANALYZE main.\"d_t\""] (pg-tokens/statements "d_t" wanted {nm true} false))))
    (testing "a crash left it invalid: drop, build, analyze"
      (is (= 3 (count (pg-tokens/statements "d_t" wanted {nm false} false)))))
    (testing "the old one goes after the new one, other indexes stay, statistics come last"
      (let [[create drop-old analyze & more] (pg-tokens/statements
                                              "d_t" wanted
                                              {"d_t_kw_old" true "d_t_key_x" true} true)]
        (is (= (:create wanted) create))
        (is (re-find #"DROP .*d_t_kw_old" drop-old))
        (is (re-find #"^ANALYZE" analyze))
        (is (nil? more))))))

(deftest the-index-and-the-read-write-one-expression
  (testing "the document names its columns as it was handed them, and a change of columns is a
            new index"
    (let [a (pg-tokens/document ["\"x\"" "\"y\""])
          b (pg-tokens/document ["\"x\""])]
      (is (not= (pg-tokens/index-name "d_t" a) (pg-tokens/index-name "d_t" b)))
      (is (= (pg-tokens/index-name "d_t" a) (pg-tokens/index-name "d_t" a))))))
