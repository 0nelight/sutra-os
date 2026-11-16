(ns sutra.calx.fn.logical
  (:require [clojure.string :as str]
            [sutra.calx.value :as v :refer [error?]]
            [sutra.calx.eval :as ev]
            [sutra.calx.fn.util :as u]))

(set! *warn-on-reflection* true)

(defn- truthy
  [ctx node]
  (let [x (ev/eval-arg ctx node)]
    (cond
      (v/omitted? x) false
      (v/array? x) (v/amap2 v/to-bool x)
      :else (v/to-bool x))))

(defn- lazy-if [ctx test then else]
  (let [t (truthy ctx test)
        branch (fn [b node default]
                 (cond (nil? node) default
                       (= :missing (:kind node)) 0.0
                       :else (ev/evaluate ctx node)))]
    (cond
      (error? t) t
      (v/array? t)
      (let [a (ev/deref-value ctx (branch true then true))
            b (ev/deref-value ctx (branch false else false))]
        (ev/broadcast (fn [c x y] (cond (error? c) c c x :else y)) [t a b]))
      t (branch true then true)
      :else (branch false else false))))

(defn- logical-values
  [ctx args]
  (reduce (fn [acc a]
            (cond
              (v/omitted? a) acc
              (or (v/ref? a) (v/array? a))
              (reduce (fn [acc x]
                        (cond (error? x) (reduced x)
                              (boolean? x) (conj acc x)
                              (number? x) (conj acc (not (zero? (double x))))
                              :else acc))
                      acc
                      (if (v/ref? a) (map peek (ev/ref-cells ctx a)) (u/flat a)))
              :else (let [b (v/to-bool a)] (if (error? b) (reduced b) (conj acc b)))))
          [] args))

(defn- bool-agg [f]
  {:arity [1 255] :args :refs
   :impl (fn [ctx & args]
           (let [bs (logical-values ctx args)]
             (cond (error? bs) bs
                   (empty? bs) v/ERR-VALUE
                   :else (f bs))))})

(defn- make-lambda [ctx & nodes]
  (let [params (butlast nodes) body (last nodes)]
    (if (and body (every? #(= :name (:kind %)) params))
      (v/->Lambda (mapv #(str/upper-case (:name %)) params) body (:env ctx))
      v/ERR-VALUE)))

(defn- let-impl [ctx & nodes]
  (if (even? (count nodes))
    v/ERR-VALUE
    (loop [ctx ctx [n x & more] nodes]
      (if (nil? x)
        (ev/evaluate ctx n)
        (if (= :name (:kind n))
          (let [val (ev/evaluate ctx x)]
            (recur (assoc-in ctx [:env (str/upper-case (:name n))] val) more))
          v/ERR-VALUE)))))

(defn- lambda-arg [ctx x]
  (let [x (ev/deref-value ctx x)] (if (v/lambda? x) x nil)))

(defn- call [ctx lam & args]
  (v/scalarize (ev/deref-value ctx (ev/call-lambda ctx lam args))))

(defn- scalar-cell [x]
  (if (v/array? x) v/ERR-CALC x))

(def functions
  {"TRUE"  {:arity [0 0] :impl (constantly true)}
   "FALSE" {:arity [0 0] :impl (constantly false)}
   "NOT"   {:arity [1 1] :lift :all :impl (fn [x] (v/to-bool x))}
   "AND"   (bool-agg #(every? true? %))
   "OR"    (bool-agg #(boolean (some true? %)))
   "XOR"   (bool-agg #(odd? (count (filter true? %))))

   "IF" {:arity [1 3] :args :lazy
         :impl (fn [ctx test & [then else]] (lazy-if ctx test then else))}

   "IFS" {:arity [2 254] :args :lazy
          :impl (fn [ctx & nodes]
                  (if (odd? (count nodes))
                    v/ERR-NA
                    (loop [[c x & more] nodes]
                      (if (nil? c)
                        v/ERR-NA
                        (let [t (truthy ctx c)]
                          (cond (error? t) t
                                (v/array? t) v/ERR-VALUE
                                t (ev/evaluate ctx x)
                                :else (recur more)))))))}

   "IFERROR" {:arity [2 2] :args :lazy
              :impl (fn [ctx x alt]
                      (let [r (ev/eval-arg ctx x)]
                        (cond (error? r) (ev/evaluate ctx alt)
                              (v/array? r) (let [a (ev/eval-arg ctx alt)]
                                             (ev/broadcast (fn [x y] (if (error? x) y x)) [r a]))
                              :else r)))}

   "IFNA" {:arity [2 2] :args :lazy
           :impl (fn [ctx x alt]
                   (let [r (ev/eval-arg ctx x)
                         na? #(= v/ERR-NA %)]
                     (cond (na? r) (ev/evaluate ctx alt)
                           (v/array? r) (let [a (ev/eval-arg ctx alt)]
                                          (ev/broadcast (fn [x y] (if (na? x) y x)) [r a]))
                           :else r)))}

   "SWITCH" {:arity [3 254] :args :lazy
             :impl (fn [ctx expr & cases]
                     (let [x (ev/eval-arg ctx expr)]
                       (if (error? x)
                         x
                         (loop [[k val & more :as cs] cases]
                           (cond
                             (empty? cs) v/ERR-NA
                             (nil? val) (ev/evaluate ctx k)
                             :else (let [kv (ev/eval-arg ctx k)]
                                     (if (and (not (error? kv)) (v/values-equal? x kv)
                                              (= (string? x) (string? kv)))
                                       (ev/evaluate ctx val)
                                       (recur more))))))))}

   "CHOOSE" {:arity [2 255] :args :lazy
             :impl (fn [ctx idx & options]
                     (let [i (u/int* (ev/eval-arg ctx idx))]
                       (cond (error? i) i
                             (<= 1 i (count options)) (ev/evaluate ctx (nth options (dec i)))
                             :else v/ERR-VALUE)))}

   "LET" {:arity [3 253] :args :lazy :impl let-impl}
   "LAMBDA" {:arity [1 254] :args :lazy :impl make-lambda}

   "ISOMITTED" {:arity [1 1] :args :lazy
                :impl (fn [ctx n]
                        (if (= :name (:kind n))
                          (v/omitted? (get (:env ctx) (str/upper-case (:name n))))
                          false))}

   "MAP" {:arity [2 254] :args :refs
          :impl (fn [ctx & args]
                  (let [lam (lambda-arg ctx (last args))
                        arrays (map #(u/arg->2d ctx %) (butlast args))]
                    (if-not lam
                      v/ERR-VALUE
                      (ev/broadcast (fn [& xs] (scalar-cell (apply call ctx lam xs))) arrays))))}

   "REDUCE" {:arity [3 3] :args :refs
             :impl (fn [ctx init arr f]
                     (if-let [lam (lambda-arg ctx f)]
                       (reduce (fn [acc x]
                                 (let [r (call ctx lam acc x)] (if (error? r) (reduced r) r)))
                               (v/scalarize (ev/deref-value ctx init))
                               (u/flat (u/arg->2d ctx arr)))
                       v/ERR-VALUE))}

   "SCAN" {:arity [3 3] :args :refs
           :impl (fn [ctx init arr f]
                   (if-let [lam (lambda-arg ctx f)]
                     (let [a (u/arg->2d ctx arr)
                           nc (v/cols a)
                           vals (reductions (fn [acc x] (call ctx lam acc x))
                                            (v/scalarize (ev/deref-value ctx init))
                                            (u/flat a))]
                       (mapv vec (partition nc (map scalar-cell (rest vals)))))
                     v/ERR-VALUE))}

   "BYROW" {:arity [2 2] :args :refs
            :impl (fn [ctx arr f]
                    (if-let [lam (lambda-arg ctx f)]
                      (mapv (fn [row] [(scalar-cell (call ctx lam [row]))]) (u/arg->2d ctx arr))
                      v/ERR-VALUE))}

   "BYCOL" {:arity [2 2] :args :refs
            :impl (fn [ctx arr f]
                    (if-let [lam (lambda-arg ctx f)]
                      [(mapv (fn [col] (scalar-cell (call ctx lam (mapv vector col))))
                             (v/transpose (u/arg->2d ctx arr)))]
                      v/ERR-VALUE))}

   "MAKEARRAY" {:arity [3 3] :args :refs
                :impl (fn [ctx nr nc f]
                        (let [nr (u/int* (ev/deref-value ctx nr))
                              nc (u/int* (ev/deref-value ctx nc))
                              lam (lambda-arg ctx f)]
                          (cond (error? nr) nr (error? nc) nc
                                (not lam) v/ERR-VALUE
                                (or (< nr 1) (< nc 1)) v/ERR-VALUE
                                :else
                                  (v/make-array2 nr nc
                                                 (fn [r c]
                                                   (scalar-cell
                                                    (call ctx lam (double (inc r))
                                                          (double (inc c)))))))))}})
