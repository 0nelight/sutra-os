(ns ^:unit grammar.fn-params-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [sutra.parser.fn-params :as fn-params]))

(def ^:private ps22 (str/join " " (map #(str "p" %) (range 22))))

(deftest every-arity-over-twenty-is-named
  (is (= [{:name "big" :params 22} {:name "multi" :params 22} {:name nil :params 22}]
         (fn-params/over-limit
          (str "(defn ok [a b] a) (defn big \"doc\" [" ps22 "] 1)"
               " (defn multi ([a] a) ([" ps22 " & more] 1)) (fn [" ps22 "] 2)")))))

(deftest a-rest-param-does-not-count-and-unreadable-code-is-not-judged
  (is (= [] (fn-params/over-limit "(defn x [a & more] ::kw #?(:clj 1 :cljs 2))")))
  (is (nil? (fn-params/over-limit "(defn x [a"))))
