(ns sutra.calx.fn.util
  (:refer-clojure :exclude [num])
  (:require [clojure.string :as str]
            [sutra.calx.value :as v :refer [error?]]
            [sutra.calx.eval :as ev])
  (:import (sutra.calx.value Ref)))

(set! *warn-on-reflection* true)

(defmacro check
  [bindings & body]
  (if (empty? bindings)
    `(do ~@body)
    (let [[form expr & more] bindings
          g (gensym "v")]
      `(let [~g ~expr]
         (if (v/error? ~g) ~g (let [~form ~g] (check ~(vec more) ~@body)))))))

(defn present? [x] (not (v/omitted? x)))

(defn- scalar-of [x]
  (let [x (v/scalarize x)] (if (v/array? x) (ffirst x) x)))

(defn num
  ([x] (num x 0.0))
  ([x default]
   (if (v/omitted? x)
     (if (nil? default) v/ERR-VALUE (double default))
     (v/to-number (scalar-of x)))))

(defn trunc
  ^double [^double x]
  (if (neg? x) (Math/ceil x) (Math/floor x)))

(defn int*
  ([x] (int* x 0))
  ([x default]
   (let [n (num x default)]
     (if (error? n) n
         (let [d (double n)]
           (if (> (Math/abs d) 9.0E15) v/ERR-NUM
             (long (if (neg? d) (Math/ceil d) (Math/floor d)))))))))

(defn text
  ([x] (text x ""))
  ([x default]
   (if (v/omitted? x) default (v/to-text (scalar-of x)))))

(defn bool
  ([x] (bool x false))
  ([x default]
   (if (v/omitted? x) default (v/to-bool (scalar-of x)))))

(defn result
  [x]
  (if (number? x) (v/normalize-number x) x))

(defn ->2d
  [x]
  (cond (v/omitted? x) nil
        (v/array? x) x
        :else [[x]]))

(defn arg->2d
  [ctx x]
  (->2d (ev/deref-value ctx x)))

(defn flat [x] (if (v/array? x) (apply concat x) [x]))

(defn collect
  ([ctx args] (collect ctx args {}))
  ([ctx args {:keys [text-in-refs bool-in-refs] :or {text-in-refs :skip bool-in-refs :skip}}]
   (reduce
    (fn [acc a]
      (cond
        (v/omitted? a) acc
        (or (instance? Ref a) (v/array? a))
        (let [vals (if (instance? Ref a) (map peek (ev/ref-cells ctx a)) (flat a))]
          (reduce (fn [acc x]
                    (cond
                      (number? x) (conj acc (double x))
                      (error? x) (reduced x)
                      (boolean? x) (if (= bool-in-refs :number) (conj acc (if x 1.0 0.0)) acc)
                      (string? x) (if (= text-in-refs :zero) (conj acc 0.0) acc)
                      :else acc))
                  acc vals))
        :else
        (let [x (v/to-number a)]
          (if (error? x) (reduced x) (conj acc x)))))
    []
    args)))

(defmacro with-collected
  [[sym expr] & body]
  `(let [~sym ~expr] (if (v/error? ~sym) ~sym (do ~@body))))

(defn all-values
  [ctx args]
  (mapcat (fn [a]
            (cond (v/omitted? a) []
                  (instance? Ref a) (flat (ev/ref-values ctx a))
                  (v/array? a) (flat a)
                  :else [a]))
          args))

(defn wildcard->regex
  [pattern]
  (let [sb (StringBuilder. "(?is)")]
    (loop [cs (seq pattern)]
      (when-let [c (first cs)]
        (cond
          (and (= c \~)
               (second cs)) (do (.append sb (java.util.regex.Pattern/quote (str (second cs))))
                                         (recur (nnext cs)))
          (= c \*) (do (.append sb ".*") (recur (next cs)))
          (= c \?) (do (.append sb ".") (recur (next cs)))
          :else (do (.append sb (java.util.regex.Pattern/quote (str c))) (recur (next cs))))))
    (re-pattern (str sb))))

(defn has-wildcards? [s] (boolean (re-find #"(?<!~)[*?]" s)))

(defn criteria-pred
  ([crit] (criteria-pred nil crit))
  ([loc crit]
   (let [crit (v/scalarize crit)
         crit (if (v/array? crit) (ffirst crit) crit)]
     (cond
       (number? crit) (fn [x] (and (number? x) (== (double x) (double crit))))
       (boolean? crit) (fn [x] (= x crit))
       (nil? crit) (fn [x] (or (nil? x) (= "" x)))
       (error? crit) (fn [x] (= x crit))
       (string? crit)
       (let [[_ op rhs] (re-matches #"(?s)(<=|>=|<>|<|>|=)?(.*)" crit)
             n (v/parse-number loc rhs)
             n (or n (v/parse-date-time loc rhs))
             b (v/text->boolean loc rhs)
             e (v/parse-error loc rhs)
             cmp (fn [x pred]
                   (cond
                     n (and (number? x) (pred (compare (double x) (double n))))
                     (some? b) (and (boolean? x) (pred (compare x b)))
                     :else (and (string? x)
                                (pred (compare (str/lower-case x) (str/lower-case rhs))))))]
         (case op
           (nil "=") (cond
                       (and (= ""
                               rhs)) (if (= op "=") #(or (nil? %) (= "" %)) #(or (nil? %) (= "" %)))
                       n #(or (and (number? %) (== (double %) (double n)))
                              (and (string? %) (= (v/parse-number loc %) n)))
                       (some? b) #(= % b)
                       e #(= % e)
                       (has-wildcards? rhs) (let [re (wildcard->regex rhs)]
                                              #(and (string? %) (boolean (re-matches re %))))
                       :else #(and (string? %) (= (str/lower-case %) (str/lower-case rhs))))
           "<>" (cond
                  (= "" rhs) #(not (or (nil? %) (= "" %)))
                  n #(not (and (number? %) (== (double %) (double n))))
                  (some? b) #(not= % b)
                  e #(not= % e)
                  (has-wildcards? rhs) (let [re (wildcard->regex rhs)]
                                         #(not (and (string? %) (re-matches re %))))
                  :else #(not (and (string? %) (= (str/lower-case %) (str/lower-case rhs)))))
           "<" #(cmp % neg?)
           ">" #(cmp % pos?)
           "<=" #(cmp % (fn [c] (<= c 0)))
           ">=" #(cmp % (fn [c] (>= c 0)))))))))

(defn pad-args
  [hi args]
  (if (and hi (< (count args) hi))
    (concat args (repeat (- hi (count args)) v/omitted))
    args))

(defn num-fn
  [[lo hi] f & {:keys [defaults]}]
  {:arity [lo hi]
   :lift :all
   :impl (fn [& args]
           (let [nums (map-indexed (fn [i a] (num a (get defaults i 0.0))) (pad-args hi args))]
             (or (first (filter error? nums))
                 (result (apply f nums)))))})

(defn text-fn
  [[lo hi] f]
  {:arity [lo hi]
   :lift :all
   :impl (fn [& args]
           (let [ts (map text (pad-args hi args))]
             (or (first (filter error? ts)) (apply f ts))))})
