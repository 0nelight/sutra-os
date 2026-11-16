(ns ent-lsp.fixture
  (:require [clojure.string :as str]
            [ent-lsp.spec :as spec]
            [ent-lsp.workspace :as ws]))

(def root "test/fixtures/ws")

(def analyzed (delay (ws/analyze (ws/open-workspace (spec/load-spec) root))))

(defn pos-of
  ([w rel needle] (pos-of w rel needle 0 1))
  ([w rel needle nth offset]
   (let [text (get-in w [:files rel :text])
         idx  (loop [from 0 n nth]
                (let [i (str/index-of text needle from)]
                  (if (zero? n) i (recur (inc i) (dec n)))))
         before (subs text 0 idx)
         line (count (re-seq #"\n" before))
         col  (- idx (inc (or (str/last-index-of before "\n") -1)))]
     [line (+ col offset)])))
