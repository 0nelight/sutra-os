(ns sutra.ref.expr
  (:refer-clojure :exclude [eval])
  (:require [clojure.string :as str]
            [sutra.ref.err :refer [fail!]]
            [sutra.ref.geo :as geo]
            [sutra.ref.value :as v]
            [sutra.ref.tokens :as tokens]))

(def max-depth 200)

(defn same-value?
  [a b]
  (cond (and (number? a) (number? b)) (== a b)
        (and (inst? a) (inst? b))     (= (inst-ms a) (inst-ms b))
        :else                         (= a b)))

(def comparisons
  {:=           [:equality :eq]   :not=     [:equality :ne]
   :<           [:ordering :lt]   :>        [:ordering :gt]
   :<=          [:ordering :lte]  :>=       [:ordering :gte]
   :nil?        [:nullity  :nil]
   :starts-with [:text :prefix]   :contains [:text :substring]
   :contains-ci [:text :substring-ci]
   :has         [:membership :has]})

(defn- code-point-compare [^String x ^String b]
  #?(:clj  (java.util.Arrays/compare (.toArray (.codePoints x)) (.toArray (.codePoints b)))
     :cljs (compare (vec (.from js/Array x #(.codePointAt % 0)))
                    (vec (.from js/Array b #(.codePointAt % 0))))))

(defn comparison-match? [op x b]
  (let [[cls test] (comparisons op)]
    (case cls
      :nullity  (if b (nil? x) (some? x))
      :equality (when-not (or (nil? x) (nil? b))
                  (let [s (same-value? x b)] (if (= test :eq) s (not s))))
      :ordering (when-not (or (nil? x) (nil? b))
                  (let [c (cond (and (string? x) (string? b)) (code-point-compare x b)
                                (and (inst? x) (inst? b))     (compare (inst-ms x) (inst-ms b))
                                :else                         (compare x b))]
                    (case test :lt (neg? c) :gt (pos? c) :lte (not (pos? c)) :gte (not (neg? c)))))
      :membership (when (and (sequential? x) (some? b))
                    (boolean (some #(same-value? % b) x)))
      :text     (when-not (nil? x)
                  (let [f (if (= test :substring-ci) #(str/lower-case (str %)) str)]
                    (if (= test :prefix)
                      (str/starts-with? (f x) (f b))
                      (str/includes? (f x) (f b))))))))

(defn match?
  ([o node] (match? o node nil))
  ([o node other]
   (if (nil? node)
     true
     (let [[op a b] node]
       (cond
         (= op :and)   (every? #(match? o % other) (rest node))
         (= op :or)    (boolean (some #(match? o % other) (rest node)))
         (= op :id-in) (contains? (set a) (:id o))
         (comparisons op) (boolean (comparison-match? op (v/value o a) b))
         (geo/ops op)  (geo/match? op (geo/object-point #(v/value o %) a) b)
         (#{:matches-tokens :matches-tokens-in} op) (tokens/match? (map #(v/value o %) a) b)
         other (boolean (other o node))
         :else (fail! :unsupported-predicate
                      (str "cannot evaluate predicate-IR node: " (pr-str node))
                      {:node node}))))))

(defn- as-list [op x]
  (cond (nil? x) []
        (sequential? x) x
        (set? x) (vec x)
        :else (fail! :not-a-list (str op " compares a LIST of values, and got " (pr-str x)
                                      " — take the list off an object-list parameter with"
                                      " [:pluck <param> <property>] or [:pluck-id <param>].")
                     {:op op :value x})))

(defn- as-double [r] #?(:clj (if (ratio? r) (double r) r) :cljs r))

(def ^:private add' #?(:clj +' :cljs +))
(def ^:private sub' #?(:clj -' :cljs -))
(def ^:private mul' #?(:clj *' :cljs *))

(defn- div* [a b]
  #?(:clj  (if (or (decimal? a) (decimal? b))
             (.divide (bigdec a) (bigdec b) java.math.MathContext/DECIMAL64)
             (as-double (/ a b)))
     :cljs (/ a b)))

(defn- text-of [x]
  (cond (keyword? x) (name x)
        #?@(:clj [(decimal? x) (.toPlainString (.stripTrailingZeros ^BigDecimal x))])
        (and (float? x) (== x (Math/floor x)) (< (abs x) 1e15)) (str (long x))
        :else (str x)))

(defn- concat-strings
  [opt & xs]
  (let [{:keys [separator null-if-any?]} (if (map? opt) opt {:separator opt})
        parts (remove nil? xs)]
    (when-not (or (empty? parts) (and null-if-any? (some nil? xs)))
      (str/join (or separator "") (map text-of parts)))))

(def value-ops
  (let [eqv same-value?
        nev (complement same-value?)
        rej (fn [f & {:as more}] (merge {:fn f :nil :reject} more))
        val (fn [f] {:fn f :nil :value})]
    {:+ (rej add')  :- (rej sub')  :* (rej mul')  :> (rej >)  :< (rej <)  :>= (rej >=)  :<= (rej <=)
     :div   (rej div* :zero-guard true)
     :mod   (rej mod :zero-guard true)
     :abs   (rej (fn [x] (if (neg? x) (sub' x) x)))
     :round (rej (fn ([x] (Math/round (double x)))
                   ([x d] (let [f (Math/pow 10 (long d))]
                            (/ (Math/round (* (double x) f)) f)))))
     := (val eqv)  :!= (val nev)  :not= (val nev)
     :approx= (val (fn [a b] (if (and (number? a) (number? b))
                               (<= (abs (- (double a) (double b))) 0.005)
                               (= a b))))
     :matches (val (fn [a b]
                     (boolean (and (some? a) (some? b) (re-find (re-pattern (str b)) (str a))))))
     :includes (val (fn [xs y] (boolean (some #(eqv % y) (as-list :includes xs)))))
     :includes-any (val (fn [xs & ys]
                          (let [rs (mapcat #(if (or (sequential? %) (set? %)) % [%]) ys)]
                            (boolean (some (fn [x] (some #(eqv x %) rs))
                                           (as-list :includes-any xs))))))
     :concat-strings (val concat-strings)
     :each=  (val (fn [xs y] (every? #(eqv % y) (as-list :each= xs))))
     :each!= (val (fn [xs y] (every? #(nev % y) (as-list :each!= xs))))}))

(defn- field [V x]
  (if (keyword? x)
    (let [r (get V x ::absent)]
      (if (= r ::absent)
        (fail! :unknown-field (str "Unknown field: " x) {:field x :available (vec (keys V))})
        r))
    x))

(defn- link-ctx! [ctx op]
  (or (:traverse ctx)
      (fail! :no-link-context (str op " requires a link context") {:op op})))

(defn- neighbours [ctx op over where]
  (cond->> ((link-ctx! ctx op) (:self ctx) over)
    (some? where) (filterv #(match? % where))))

(defn- best
  [beats? key xs]
  (reduce (fn [p q] (if (beats? (compare (key p) (key q))) q p)) xs))

(declare pred)

(defn eval
  ([V e] (eval V e nil 0))
  ([V e ctx] (eval V e ctx 0))
  ([V e ctx depth]
   (when (> depth max-depth)
     (fail! :too-deep (str "Expression depth > " max-depth) {:depth depth}))
   (if-not (vector? e)
     (field V e)
     (let [[op & args] e
           d   (inc depth)
           ev  #(eval V % ctx d)]
       (case op
         :get      (cond-> (field V (first args)) (next args) (v/attr-of (second args)))
         :prop     (v/value (field V (first args)) (second args))
         :coalesce (some (fn [a] (let [x (ev a)] (when (some? x) x))) args)
         :and      (every? #(boolean (ev %)) args)
         :or       (boolean (some ev args))
         :not      (not (ev (first args)))
         :in       (let [x  (ev (first args))
                         cs (mapv ev (rest args))
                         cs (if (and (= 1 (count cs)) (sequential? (first cs))) (first cs) cs)]
                     (boolean (some #(= x %) cs)))
         :contains? (let [nrm #(-> (str %) str/lower-case str/trim (str/replace #"\s+" " "))
                          h   (nrm (ev (first args)))]
                      (boolean (some (fn [n]
                                       (let [n (nrm (ev n))] (and (seq n) (str/includes? h n))))
                                     (rest args))))

         :connected?
         (let [[over p] args]
           (boolean (some #(pred (:values %) p (assoc ctx :self %))
                          ((link-ctx! ctx op) (:self ctx) over))))

         :connected-all?
         (let [[over a needed] args
               found  (into #{} (map #(v/value % a)) ((link-ctx! ctx op) (:self ctx) over))
               needed (if (vector? needed) (mapv ev needed) needed)]
           (every? found needed))

         :connected-count
         (let [[over where] args] (count (neighbours ctx op over where)))

         (:connected-sum :connected-max :connected-min :connected-avg)
         (let [[over a where] args
               xs (keep #(v/value % a) (neighbours ctx op over where))]
           (case op
             :connected-sum (reduce add' 0 xs)
             :connected-max (when (seq xs) (best neg? identity xs))
             :connected-min (when (seq xs) (best pos? identity xs))
             :connected-avg (when (seq xs) (as-double (/ (reduce add' 0 xs) (count xs))))))

         :connected-one
         (let [[over pick] args
               cands ((link-ctx! ctx op) (:self ctx) over)]
           (when (> (count cands) 1)
             (fail! :not-one (str ":connected-one over " over " found " (count cands)
                                  " neighbours — the link is declared at-most-one; fix the data"
                                  " or the cardinality, do not pick one")
                    {:over over :found (count cands)}))
           (v/value (first cands) pick))

         :connected-arg-max
         (let [[over by pick where] args
               cands (filter #(some? (v/value % by)) (neighbours ctx op over where))]
           (when (seq cands) (v/value (best neg? #(v/value % by) cands) pick)))

         (let [spec (or (value-ops op) (fail! :unknown-op (str "Unknown op: " op) {:op op :expr e}))
               xs   (mapv ev args)]
           (when (and (:zero-guard spec) (some #(and (number? %) (zero? %)) (rest xs)))
             (fail! :division-by-zero (str "Division by zero: " (pr-str e)) {:expr e}))
           (when (and (= :reject (:nil spec)) (some nil? xs))
             (fail! :nil-in-computation (str "nil in computation: " (pr-str e)) {:expr e}))
           (apply (:fn spec) xs)))))))

(defn pred
  ([V p] (pred V p nil))
  ([V p ctx] (boolean (if (vector? p) (eval V p ctx) p))))

(def arg-shapes
  {:get               {:head [:field :name]}
   :prop              {:head [:field :name]}
   :not               {:head [:expr]}
   :connected?        {:head [:name :foreign]}
   :connected-all?    {:head [:name :name :exprs]}
   :connected-count   {:head [:name :foreign]}
   :connected-sum     {:head [:name :name :foreign]}
   :connected-max     {:head [:name :name :foreign]}
   :connected-min     {:head [:name :name :foreign]}
   :connected-avg     {:head [:name :name :foreign]}
   :connected-one     {:head [:name :name]}
   :connected-arg-max {:head [:name :name :name :foreign]}})

(def known-ops
  (into (into #{:coalesce :and :or :in :contains?} (keys arg-shapes))
        (keys value-ops)))

(defn unknown-ops
  [p]
  (into (sorted-set)
        (comp (filter vector?) (keep first) (filter keyword?) (remove known-ops))
        (tree-seq coll? seq p)))

(defn- arg-shape [op i]
  (let [{:keys [head rest]} (get arg-shapes op)]
    (or (get head i) rest :expr)))

(defn field-refs
  [p]
  (letfn [(walk [e acc]
            (cond
              (keyword? e)      (conj acc e)
              (not (vector? e)) acc
              :else
              (let [[op & args] e]
                (reduce-kv (fn [a i x]
                             (case (arg-shape op i)
                               :field (cond-> a (keyword? x) (conj x))
                               :expr  (walk x a)
                               :exprs (if (vector? x) (reduce #(walk %2 %1) a x) (walk x a))
                               a))
                           acc (vec args)))))]
    (if (vector? p) (walk p #{}) #{})))
