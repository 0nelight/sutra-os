(ns sutra.calx.eval
  (:require [clojure.string :as str]
            [sutra.calx.value :as v :refer [error?]]
            [sutra.calx.address :as addr])
  (:import (sutra.calx.value Ref Lambda)))

(set! *warn-on-reflection* true)

(declare evaluate deref-value)

(defn clip-ref
  [ctx ^Ref r]
  (let [[nr nc] ((:extent ctx) (:sheet r))
        r2 (min (:r2 r) (max (:r1 r) (dec (max nr 1))))
        c2 (min (:c2 r) (max (:c1 r) (dec (max nc 1))))]
    (if (and (= r2 (:r2 r)) (= c2 (:c2 r))) r (assoc r :r2 r2 :c2 c2))))

(defn ref-values
  [ctx ^Ref r]
  (let [{:keys [sheet r1 c1 r2 c2]} (clip-ref ctx r)
        value-at (:value-at ctx)]
    (mapv (fn [row] (mapv (fn [col] (value-at sheet row col)) (range c1 (inc c2))))
          (range r1 (inc r2)))))

(defn ref-cells
  [ctx ^Ref r]
  (let [{:keys [sheet r1 c1 r2 c2]} (clip-ref ctx r)
        value-at (:value-at ctx)]
    (for [row (range r1 (inc r2)) col (range c1 (inc c2))
          :let [x (value-at sheet row col)]
          :when (some? x)]
      [row col x])))

(defn deref-value
  [ctx x]
  (if (instance? Ref x)
    (if (v/single-cell? x)
      ((:value-at ctx) (:sheet x) (:r1 x) (:c1 x))
      (ref-values ctx x))
    x))

(defn- node->ref [ctx n]
  (if (:sheet-name n)
    v/ERR-REF
    (let [sheet (or (:sheet n) (:sheet ctx))]
      (case (:shape n)
        (:cell :area) (v/->Ref sheet (:r1 n) (:c1 n) (:r2 n) (:c2 n))
        :cols (v/->Ref sheet 0 (:c1 n) (dec addr/max-rows) (:c2 n))
        :rows (v/->Ref sheet (:r1 n) 0 (:r2 n) (dec addr/max-cols))))))

(defn broadcast
  [f args]
  (let [arrays (filter v/array? args)]
    (if (empty? arrays)
      (apply f args)
      (let [nr (apply max (map v/rows arrays))
            nc (apply max (map v/cols arrays))
            pick (fn [a r c]
                   (if (v/array? a)
                     (let [ar (v/rows a) ac (v/cols a)
                           rr (if (= ar 1) 0 r) cc (if (= ac 1) 0 c)]
                       (if (and (< rr ar) (< cc ac)) (get-in a [rr cc]) v/ERR-NA))
                     a))]
        (v/make-array2 nr nc (fn [r c] (apply f (map #(pick % r c) args))))))))

(defn- arith [loc op a b]
  (let [x (v/to-number loc a)]
    (if (error? x)
      x
      (let [y (v/to-number loc b)]
        (if (error? y)
          y
          (let [x (double x) y (double y)]
            (case op
              :add (v/normalize-number (+ x y))
              :sub (v/normalize-number (- x y))
              :mul (v/normalize-number (* x y))
              :div (if (zero? y) v/ERR-DIV0 (v/normalize-number (/ x y)))
              :pow (if (and (zero? x) (zero? y)) v/ERR-NUM
                       (v/normalize-number (Math/pow x y))))))))))

(defn- concat-op [loc a b]
  (let [x (v/to-text loc a)]
    (if (error? x) x (let [y (v/to-text loc b)] (if (error? y) y (str x y))))))

(defn- compare-op [op a b]
  (cond
    (error? a) a
    (error? b) b
    :else (let [c (v/compare-values a b)]
            (case op :eq (zero? c) :ne (not (zero? c)) :lt (neg? c) :gt (pos? c)
                     :le (<= c 0) :ge (>= c 0)))))

(defn- scalar-binop [loc op]
  (case op
    (:add :sub :mul :div :pow) #(arith loc op %1 %2)
    :concat #(concat-op loc %1 %2)
    (:eq :ne :lt :gt :le :ge) #(compare-op op %1 %2)))

(defn- range-op [ctx a b]
  (if (and (instance? Ref a) (instance? Ref b) (= (:sheet a) (:sheet b)))
    (v/->Ref (:sheet a) (min (:r1 a) (:r1 b)) (min (:c1 a) (:c1 b))
             (max (:r2 a) (:r2 b)) (max (:c2 a) (:c2 b)))
    v/ERR-VALUE))

(defn- eval-op [ctx {:keys [op args]}]
  (case op
    :range (range-op ctx (evaluate ctx (first args)) (evaluate ctx (second args)))
    :neg (broadcast #(arith (:locale ctx) :sub 0.0 %)
                    [(deref-value ctx (evaluate ctx (first args)))])
    :pos (deref-value ctx (evaluate ctx (first args)))
    :pct (broadcast #(arith (:locale ctx) :div % 100.0)
                    [(deref-value ctx (evaluate ctx (first args)))])
    (let [a (deref-value ctx (evaluate ctx (first args)))
          b (deref-value ctx (evaluate ctx (second args)))]
      (if (or (v/lambda? a) (v/lambda? b))
        v/ERR-VALUE
        (broadcast (scalar-binop (:locale ctx) op) [a b])))))

(defn call-lambda
  [ctx ^Lambda lam args]
  (let [params (:params lam)]
    (if (> (count args) (count params))
      v/ERR-VALUE
      (let [env (merge (:env lam)
                       (zipmap params (concat args (repeat v/omitted))))]
        (evaluate (assoc ctx :env env) (:body lam))))))

(defn- arity-ok? [{[lo hi] :arity} n]
  (and (>= n (or lo 0)) (or (nil? hi) (<= n hi))))

(defn- first-error [args]
  (some #(when (error? %) %) args))

(defn- as-text [loc x]
  (cond (v/array? x) (v/amap2 #(as-text loc %) x)
        (or (number? x) (boolean? x)) (v/to-text loc x)
        :else x))

(defn- text-arg? [which i]
  (cond (= which :all) true
        (set? which) (contains? which i)
        (= :except (first which)) (not (contains? (second which) i))))

(defn- text-args [loc which args]
  (if (and loc which)
    (into [] (map-indexed (fn [i a] (if (text-arg? which i) (as-text loc a) a))) args)
    args))

(defn- apply-spec [ctx spec fname args]
  (let [{:keys [impl lift ctx? errors]} spec
        args (text-args (:locale ctx) (:text-args spec) args)
        call (fn [& xs] (if ctx? (apply impl ctx xs) (apply impl xs)))]
    (if lift
      (let [lifted? (if (= lift :all) (constantly true) #(contains? lift %))
            arrays? (some (fn [[i a]] (and (lifted? i) (v/array? a))) (map-indexed vector args))]
        (if arrays?
          (let [idx (vec (keep-indexed (fn [i a] (when (and (lifted? i) (v/array? a)) i)) args))
                arrs (mapv #(nth args %) idx)
                f (fn [& elems]
                    (let [args' (reduce (fn [acc [i e]] (assoc acc i e)) (vec args)
                                        (map vector idx elems))]
                      (if-let [e (and (not= errors :pass) (first-error args'))]
                        e
                        (apply call args'))))]
            (broadcast f arrs))
          (if-let [e (and (not= errors :pass) (first-error args))] e (apply call args))))
      (if-let [e (and (not= errors :pass) (first-error args))] e (apply call args)))))

(defn eval-arg
  [ctx node]
  (if (= :missing (:kind node))
    v/omitted
    (deref-value ctx (evaluate ctx node))))

(defn eval-arg-ref
  [ctx node]
  (if (= :missing (:kind node))
    v/omitted
    (evaluate ctx node)))

(defn call-function
  [ctx fname arg-nodes]
  (if-let [lam (let [x (get (:env ctx) fname)] (when (v/lambda? x) x))]
    (call-lambda ctx lam (mapv #(eval-arg-ref ctx %) arg-nodes))
    (if-let [spec (get (:fns ctx) fname)]
      (if-not (arity-ok? spec (count arg-nodes))
        v/ERR-NA
        (try
          (case (:args spec :values)
            :lazy (apply (:impl spec) ctx arg-nodes)
            :refs (let [args (mapv #(eval-arg-ref ctx %) arg-nodes)]
                    (apply (:impl spec) ctx args))
            :values (apply-spec ctx spec fname (mapv #(eval-arg ctx %) arg-nodes)))
          (catch ArithmeticException _ v/ERR-NUM)
          (catch IndexOutOfBoundsException _ v/ERR-REF)
          (catch NumberFormatException _ v/ERR-VALUE)))
      (let [ast (when-let [names (:names ctx)] (names fname (:sheet ctx)))
            lam (when ast (deref-value ctx (evaluate (dissoc ctx :env) ast)))]
        (if (v/lambda? lam)
          (call-lambda ctx lam (mapv #(eval-arg-ref ctx %) arg-nodes))
          v/ERR-NAME)))))

(defn- eval-name [ctx {:keys [name sheet sheet-name]}]
  (let [up (str/upper-case name)]
    (if (and (nil? sheet) (nil? sheet-name) (contains? (:env ctx) up))
      (get (:env ctx) up)
      (if sheet-name
        v/ERR-NAME
        (if-let [ast (when-let [names (:names ctx)] (names up (or sheet (:sheet ctx))))]
          (evaluate (dissoc ctx :env) ast)
          v/ERR-NAME)))))

(defn- eval-spill [ctx {:keys [ref]}]
  (let [r (evaluate ctx ref)]
    (if (and (instance? Ref r) (v/single-cell? r))
      (if-let [[nr nc] ((:spill-at ctx) (:sheet r) (:r1 r) (:c1 r))]
        (v/->Ref (:sheet r) (:r1 r) (:c1 r) (+ (:r1 r) (dec nr)) (+ (:c1 r) (dec nc)))
        (if ((:formula-at ctx (constantly nil)) (:sheet r) (:r1 r) (:c1 r)) r v/ERR-REF))
      v/ERR-REF)))

(defn evaluate
  [ctx node]
  (case (:kind node)
    :lit (:value node)
    :ref (node->ref ctx node)
    :name (eval-name ctx node)
    :call (call-function ctx (:fn node) (:args node))
    :invoke (let [f (deref-value ctx (evaluate ctx (:fn node)))]
              (cond
                (v/lambda? f) (call-lambda ctx f (mapv #(eval-arg-ref ctx %) (:args node)))
                (error? f) f
                :else v/ERR-VALUE))
    :op (eval-op ctx node)
    :paren (evaluate ctx (:expr node))
    :missing v/omitted
    :spill (eval-spill ctx node)
    :array (mapv (fn [row]
                   (mapv (fn [n]
                           (let [x (v/scalarize (deref-value ctx (evaluate ctx n)))]
                             (if (v/array? x) v/ERR-VALUE x)))
                         row))
                 (:rows node))))

(defn finalize
  [ctx x]
  (let [x (deref-value ctx x)
        fix (fn [e] (cond (nil? e) 0.0
                          (v/omitted? e) 0.0
                          (v/lambda? e) v/ERR-CALC
                          (v/array? e) v/ERR-VALUE
                          :else e))]
    (cond
      (v/array? x) (let [x (v/amap2 fix x)]
                     (if (and (= 1 (v/rows x)) (= 1 (v/cols x))) (ffirst x) x))
      :else (fix x))))

(defn evaluate-value [ctx ast]
  (finalize ctx (evaluate ctx ast)))
