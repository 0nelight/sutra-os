(ns grammar.free-text-refusals-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def ^:private roots
  ["src" "libs/core/src" "libs/dialect/src" "libs/kernel/src" "libs/world/src" "libs/commits/src"
   "libs/service/src" "plugins"])

(def ^:private kit
  #{"libs/dialect/src/sutra/kit/anomaly.cljc" "libs/core/src/sutra/kit/refusal.cljc"})

(def ^:private head #"\(anomaly/(?:raise!|refused)(?=[\s)])")

(defn- skip-ws [^String s i]
  (let [n (count s)]
    (loop [j i]
      (cond (>= j n) j
            (Character/isWhitespace (.charAt s j)) (recur (inc j))
            (= \; (.charAt s j)) (recur (or (str/index-of s "\n" j) n))
            :else j))))

(defn- string-end [^String s i]
  (let [n (count s)]
    (loop [k (inc i)]
      (cond (>= k n)             n
            (= \\ (.charAt s k)) (recur (+ k 2))
            (= \" (.charAt s k)) (inc k)
            :else                (recur (inc k))))))

(defn- token-end
  [^String s i]
  (let [n (count s)
        c (.charAt s i)]
    (cond
      (#{\( \[ \{} c)
      (loop [j i depth 0]
        (if (>= j n)
          n
          (let [c (.charAt s j)]
            (case c
              \" (recur (string-end s j) depth)
              \\ (recur (+ j 2) depth)
              (\( \[ \{) (recur (inc j) (inc depth))
              (\) \] \}) (if (= 1 depth) (inc j) (recur (inc j) (dec depth)))
              (recur (inc j) depth)))))

      (= \" c) (string-end s i)

      :else
      (loop [j i]
        (if (or (>= j n) (Character/isWhitespace (.charAt s j)) (#{\( \) \[ \]} (.charAt s j)))
          j
          (recur (inc j)))))))

(defn by-hand?
  [^String s i]
  (let [ci       (skip-ws s (+ i (count (re-find head (subs s i)))))
        category (subs s ci (token-end s ci))
        mi       (skip-ws s (token-end s ci))
        message  (subs s mi (min (count s) (+ mi 20)))]
    (not (or (= ":fault" category)
             (re-find #"^\((?:anomaly|refusal)/message[\s)]" message)))))

(defn- source-files []
  (for [r     roots
        ^java.io.File f (file-seq (io/file r))
        :when (re-find #"\.clj[cs]?$" (.getName f))]
    f))

(defn offenders
  []
  (vec (for [^java.io.File f (source-files)
             :let  [p (str/replace (.getPath f) "\\" "/")]
             :when (not (kit p))
             :let  [s (slurp f)
                    m (re-matcher head s)]
             i     (loop [acc []] (if (.find m) (recur (conj acc (.start m))) acc))
             :when (by-hand? s i)]
         (str p ":" (inc (count (re-seq #"\n" (subs s 0 i))))))))

(deftest no-refusal-is-worded-by-hand
  (testing "raise with a code: (refusal/refuse! :refusal.<module>/<name> data params)"
    (is (= [] (offenders)))))

(deftest the-rule-sees-what-it-refuses
  (testing "a literal category, a computed one and a sentence are by hand"
    (is (by-hand? "(anomaly/raise! :incorrect \"no\" {})" 0))
    (is (by-hand? "(anomaly/raise! :conflict (str \"x \" y) {})" 0))
    (is (by-hand? "(anomaly/raise! (or (anomaly/category d) :upstream) msg d)" 0))
    (is (by-hand? "(anomaly/refused\n  :upstream\n  msg {})" 0)))
  (testing "a fault and a re-shaped refusal are not"
    (is (not (by-hand? "(anomaly/raise! :fault \"bug\" {})" 0)))
    (is (not (by-hand? "(anomaly/refused c (anomaly/message r) {})" 0)))
    (is (not (by-hand? "(anomaly/refused :incorrect (refusal/message :x d) d)" 0)))))
