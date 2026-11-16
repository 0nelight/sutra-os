(ns sutra.types.expr
  (:refer-clojure :exclude [eval])
  (:require [sutra.types.refs :as refs]
            [sutra.ref.expr :as rexpr]
            [sutra.types.object :as obj]
            [sutra.types.query :as q]))

(set! *warn-on-reflection* true)

(def value-ops rexpr/value-ops)

(def link-ops
  q/link-op-names)

(defn- link-op? [op] (and (keyword? op) (link-ops op)))

(defn link-refs
  [expr]
  (if-not (vector? expr)
    #{}
    (let [[op & args] expr]
      (into (if (and (link-op? op) (seq args)) #{(first args)} #{})
            (mapcat link-refs)
            args))))

(defn link-expr?
  [expr]
  (boolean (seq (link-refs expr))))

(defn field-refs
  [expr]
  (cond
    (keyword? expr)      #{expr}
    (not (vector? expr)) #{}
    :else (let [[op & args] expr]
            (cond
              (link-op? op) #{}
              (#{:get :prop} op) (into #{} (comp (take 1) (filter keyword?)) args)
              :else         (into #{} (mapcat field-refs) args)))))

(def structural-ops
  #{:get :prop :coalesce :and :or :not :in :contains?})

(def known-ops
  (into structural-ops (concat (keys value-ops) link-ops)))

(def ^:private link-op-expr-args
  q/expr-arg-positions)

(defn unknown-ops
  [expr]
  (if-not (vector? expr)
    #{}
    (let [[op & args] expr]
      (into (if (or (not (keyword? op)) (known-ops op)) #{} #{op})
            (mapcat unknown-ops)
            (if-let [ps (link-op-expr-args op)]
              (keep-indexed (fn [i a] (when (ps i) a)) args)
              (when-not (link-op? op) args))))))

(def binding-ref-ops
  (conj obj/binding-ref-ops :get))

(defn criterion-problems
  [criterion]
  (concat
   (when-let [bad (seq (unknown-ops (:pred criterion)))]
     [{:kind :pred :ops (vec (sort bad))}])
   (for [[bk v] (:test criterion)
         :when  (and (vector? v) (keyword? (first v))
                     (not (binding-ref-ops (first v))))]
     {:kind :test :binding bk :ops [(first v)]})))

(def computing-ops
  (into known-ops #{:coalesce :prop}))

(defn rule-value-problems
  [action]
  (for [[i step] (map-indexed vector (refs/steps (:rules action)))
        [prop v] (:values step)
        :when    (and (vector? v) (keyword? (first v))
                      (not (binding-ref-ops (first v)))
                      (computing-ops (first v)))]
    {:step i :op (:op step) :property prop :expr-op (first v)
     :message (str "rule value " (name prop) " is the expression " (pr-str v)
                   ", and a rule does not compute — it would write the vector itself"
                   " and every submission would fail on the value's type. Compute it"
                   " on a hidden parameter, {:" (name prop) " {:type … :exposure :hidden"
                   " :compute " (pr-str v) "}}, and write [:get :" (name prop) "]; or"
                   " back the action with an edit function.")}))

(defn eval-order
  ([pairs] (eval-order pairs nil))
  ([pairs want]
   (let [pairs   (vec pairs)
         by-name (into {} pairs)
         deps-of (fn [ex] (into #{} (filter by-name) (field-refs ex)))
         wanted  (when want
                   (loop [w (into #{} (filter by-name) want)]
                     (let [w' (into w (mapcat #(deps-of (by-name %))) w)]
                       (if (= w w') w (recur w')))))
         chosen  (cond->> pairs wanted (filterv (comp wanted first)))]
     (loop [out [] done #{} left chosen]
       (if (empty? left)
         out
         (let [[ready blocked] ((juxt filter remove)
                                (fn [[a ex]] (every? #(or (done %) (= a %)) (deps-of ex)))
                                left)]
           (if (empty? ready)
             (into out left)
             (recur (into out ready)
                    (into done (map first) ready)
                    (vec blocked)))))))))

(defn- ref-ctx [vals {:keys [state object-id object-type traverse-fn]}]
  (when traverse-fn
    {:self     {:id object-id :object-type-ref object-type :values vals}
     :traverse (fn [self over] (traverse-fn state (:id self) over (:object-type-ref self)))}))

(defn eval
  ([vals expr]     (eval vals expr nil))
  ([vals expr ctx] (rexpr/eval vals expr (ref-ctx vals ctx))))

(defn pred
  ([vals p]     (pred vals p nil))
  ([vals p ctx] (rexpr/pred vals p (ref-ctx vals ctx))))
