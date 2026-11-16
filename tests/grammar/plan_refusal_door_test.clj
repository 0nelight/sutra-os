(ns grammar.plan-refusal-door-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def ^:private roots
  ["src" "libs/core/src" "libs/dialect/src" "libs/kernel/src" "libs/world/src" "libs/commits/src"
   "libs/service/src" "plugins"])

(def ^:private allow-file "tests/grammar/plan_refusal_door.edn")

(def ^:private pipeline
  #{"libs/core/src/sutra/types/finding.cljc" "libs/core/src/sutra/types/plan.cljc"})

(def ^:private head
  #"\((?:anomaly/raise!|anomaly/refused|anomaly/ex|refusal/refuse!|refusal/refusal|refuse!|err/fail!|fail!|ex-info)[\s)]")

(defn- form-end
  [^String s i]
  (let [n (count s)]
    (loop [j i depth 0]
      (if (>= j n)
        n
        (let [c (.charAt s j)]
          (case c
            \" (recur (loop [k (inc j)]
                        (cond (>= k n)               n
                              (= \\ (.charAt s k))   (recur (+ k 2))
                              (= \" (.charAt s k))   (inc k)
                              :else                  (recur (inc k))))
                      depth)
            \\ (recur (+ j 2) depth)
            \; (recur (or (str/index-of s "\n" j) n) depth)
            (\( \[ \{) (recur (inc j) (inc depth))
            (\) \] \}) (if (= 1 depth) j (recur (inc j) (dec depth)))
            (recur (inc j) depth)))))))

(defn- by-hand?
  [form]
  (and (or (str/includes? form ":problems") (str/includes? form ":plan-problems"))
       (not (re-find #"^\(\S+\s+:fault\b" form))))

(defn- re-seq-pos [re s]
  (let [m (re-matcher re s)]
    (loop [acc []]
      (if (.find m) (recur (conj acc (.start m))) acc))))

(defn offenders
  []
  (into (sorted-map)
        (for [r     roots
              ^java.io.File f (file-seq (io/file r))
              :let  [p (.getPath f)]
              :when (and (re-find #"\.clj[cs]?$" (.getName f)) (not (pipeline p)))
              :let  [s (slurp f)
                     n (count (for [m (re-seq-pos head s)
                                    :when (by-hand? (subs s m (min (count s)
                                                                   (inc (form-end s m)))))]
                                m))]
              :when (pos? n)]
          [p n])))

(deftest gates-refuse-through-the-plan-pipeline
  (let [allowed (into {} (map (fn [[p {:keys [n]}]] [p n]))
                      (edn/read-string (slurp allow-file)))
        now     (offenders)]
    (testing "a new hand-raised problems refusal: build a plan (sutra.types.plan) and check! it"
      (is (= {} (into (sorted-map)
                      (for [[p c] now :when (> c (get allowed p 0))]
                        [p {:raises c :allowed (get allowed p 0)}])))))
    (testing (str "fewer than allowed: lower or remove the entry in " allow-file)
      (is (= {} (into (sorted-map)
                      (for [[p c] allowed :when (< (get now p 0) c)]
                        [p {:raises (get now p 0) :allowed c}])))))))

(deftest every-allowance-says-why
  (doseq [[p {:keys [why]}] (edn/read-string (slurp allow-file))]
    (is (not (str/blank? why)) p)))
