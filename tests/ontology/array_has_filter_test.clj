(ns ^:unit ontology.array-has-filter-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [support.array-has :as h]
            [support.select-sql :as sel]
            [sutra.store.core]
            [sutra.store.mem :as mem]
            [sutra.types.query :as q]))

(deftest has-is-a-surface-op
  (is (q/surface-ops :has))
  (is (q/valid-predicate? [:has :anhang "x"])))

(deftest the-reference-answers-membership-never-a-substring
  (let [db (mem/store)]
    (h/seed! db)
    (doseq [[where codes] h/cases]
      (is (= codes (h/codes db where)) (pr-str where)))))

(deftest pg-asks-jsonb-containment
  (let [[sql params] (sel/compile-sql h/world {:op :filter :where [:has :anhang "abc"]
                                               :in {:op :scan :root h/mail}})]
    (is (str/includes? sql "::jsonb @> ?::jsonb"))
    (is (some #{"[\"abc\"]"} params))))
