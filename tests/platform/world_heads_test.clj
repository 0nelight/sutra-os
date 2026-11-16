(ns ^:unit platform.world-heads-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [platform.guard-held-test :as guard-held]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as p]))

(def ^:private roots
  ["src" "libs/core/src" "libs/dialect/src" "libs/kernel/src" "libs/lib/src" "libs/store/src"
   "libs/store-pg/src" "plugins"])

(def baseline
  2096)

(def ^:private world-params '#{W world schema})

(defn- params [node]
  (when (= :vector (n/tag node))
    (tree-seq coll? seq (try (n/sexpr node) (catch Exception _ nil)))))

(defn- heads-in
  [node]
  (let [kids (when (n/inner? node) (remove n/whitespace-or-comment? (n/children node)))
        here (when (and (= :list (n/tag node)) (#{'defn 'defn-} (some-> (first kids) n/sexpr)))
               (let [arities (concat (filter #(= :vector (n/tag %)) kids)
                                     (keep #(when (= :list (n/tag %))
                                              (first (remove n/whitespace-or-comment?
                                                             (n/children %))))
                                           kids))]
                 (when (some #(some world-params (params %)) arities)
                   [(str (n/sexpr (second kids)))])))]
    (into (vec here) (mapcat heads-in kids))))

(defn heads
  [dirs]
  (vec (for [d dirs
             ^java.io.File f (file-seq (io/file d))
             :let [nm (.getName f)]
             :when (and (re-find #"\.clj[cs]?$" nm)
                        (not (re-find #"(^|_)orakel\.clj[cs]?$" nm)))
             h (heads-in (p/parse-file-all f))]
         [(str f) h])))

(deftest the-old-code-takes-no-more-whole-worlds
  (let [hs  (heads roots)
        now (count (remove (set (keys guard-held/held)) hs))]
    (is (every? (set hs) (keys guard-held/held))
        "every function the guard test holds is one that takes a world in name")
    (is (<= now baseline)
        (str now " functions take a whole world, " baseline " did — read through an index"))))
