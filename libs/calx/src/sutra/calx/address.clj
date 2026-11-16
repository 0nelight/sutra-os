(ns sutra.calx.address
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

(def max-rows 1048576)
(def max-cols 16384)

(defn col->letters
  [c]
  (loop [n (inc c) acc ""]
    (if (pos? n)
      (let [m (mod (dec n) 26)]
        (recur (quot (dec n) 26) (str (char (+ 65 m)) acc)))
      acc)))

(defn letters->col
  [s]
  (let [n (reduce (fn [acc ch] (+ (* acc 26) (- (int (Character/toUpperCase (char ch))) 64))) 0 s)]
    (when (<= 1 n max-cols) (dec n))))

(defn quote-sheet-name
  [name]
  (if (and (re-matches #"[A-Za-z_][A-Za-z0-9_.]*" name)
           (not (re-matches #"(?i)[A-Z]{1,3}\d+" name))
           (not (#{"TRUE" "FALSE"} (str/upper-case name))))
    name
    (str "'" (str/replace name "'" "''") "'")))

(defn cell->a1
  ([r c] (str (col->letters c) (inc r)))
  ([r c row-abs? col-abs?]
   (str (when col-abs? "$") (col->letters c) (when row-abs? "$") (inc r))))

(def ^:private cell-re #"(?i)\$?([A-Z]{1,3})\$?(\d+)")

(defn parse-cell
  [s]
  (when-let [[_ letters digits] (re-matches cell-re s)]
    (let [c (letters->col letters)
          r (dec (parse-long digits))]
      (when (and c (< -1 r max-rows)) [r c]))))

(defn- unquote-sheet [s]
  (if (and (str/starts-with? s "'") (str/ends-with? s "'") (> (count s) 1))
    (str/replace (subs s 1 (dec (count s))) "''" "'")
    s))

(defn split-sheet
  [s]
  (if-let [i (str/last-index-of s "!")]
    [(unquote-sheet (subs s 0 i)) (subs s (inc i))]
    [nil s]))

(defn parse-area
  [s]
  (let [[a b] (str/split s #":" 2)
        b (or b a)]
    (if-let [[r1 c1] (parse-cell a)]
      (when-let [[r2 c2] (parse-cell b)]
        {:r1 (min r1 r2) :c1 (min c1 c2) :r2 (max r1 r2) :c2 (max c1 c2)})
      (cond
        (and (re-matches #"(?i)\$?[A-Z]{1,3}" a) (re-matches #"(?i)\$?[A-Z]{1,3}" b))
        (let [c1 (letters->col (str/replace a "$" "")) c2 (letters->col (str/replace b "$" ""))]
          (when (and c1 c2)
            {:r1 0 :r2 (dec max-rows) :c1 (min c1 c2) :c2 (max c1 c2)}))
        (and (re-matches #"\$?\d+" a) (re-matches #"\$?\d+" b))
        (let [r1 (dec (parse-long (str/replace a "$" "")))
              r2 (dec (parse-long (str/replace b "$" "")))]
          (when (and (< -1 r1 max-rows) (< -1 r2 max-rows))
            {:r1 (min r1 r2) :r2 (max r1 r2) :c1 0 :c2 (dec max-cols)}))))))

(defn area->a1 [{:keys [r1 c1 r2 c2]}]
  (if (and (= r1 r2) (= c1 c2))
    (cell->a1 r1 c1)
    (str (cell->a1 r1 c1) ":" (cell->a1 r2 c2))))
