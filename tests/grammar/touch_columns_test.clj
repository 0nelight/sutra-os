(ns grammar.touch-columns-test
  (:require [clojure.java.io :as io]
            [clojure.string]
            [clojure.test :refer [deftest is]]
            [sutra.store.control.tables :as cc]))

(defn touch-only-columns
  []
  (let [seen (for [t cc/tables [c _ & flags] (:cols t)] [c (boolean (some #{:touch} flags))])]
    (into (sorted-set)
          (for [[c flags] (group-by first seen)
                :when (every? second flags)]
            c))))

(defn hand-held
  []
  (let [col-of #(if (vector? %) (first %) %)]
    (apply merge-with into
           (for [{:keys [coll index order]} cc/ledger
                 c (concat (map col-of index) (map first order))]
             {c #{(keyword (name coll))}}))))

(defn offenders
  []
  (let [cols (touch-only-columns)
        held (hand-held)
        re   (re-pattern (str "(" (clojure.string/join "|" (map #(java.util.regex.Pattern/quote
                                                                 (str %)) cols))
                              ")\\s+(?:ctl/now|at)\\b"))]
    (into (sorted-map)
          (for [^java.io.File f (file-seq (io/file "src"))
                :let  [p (.getPath f)]
                :when (re-find #"\.clj$" (.getName f))
                :let  [text  (slurp f)
                       keeps (fn [col] (some #(clojure.string/includes? text (str %))
                                             (get held (keyword (subs col 1)))))
                       hits  (for [[m col] (re-seq re text) :when (not (keeps col))] m)]
                :when (seq hits)]
            [p (vec hits)]))))

(deftest touch-columns-are-not-stamped-by-hand
  (is (= {} (offenders))))
