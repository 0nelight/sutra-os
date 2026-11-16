(ns sutra.capps.object-filter
  (:require [clojure.string :as str]
            [sutra.kit.refusal :as refusal]
            [clojure.walk :as walk]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.capps.format :refer [v compare-operands]]
            [sutra.ref.tokens :as tokens]))

(def var-type :object-set-filter)

(defn- term-of
  [t]
  (let [t (walk/keywordize-keys t)]
    (cond-> (update t :op ->kw)
      (:prop t) (update :prop ->kw)
      (:link t) (update :link ->kw))))

(defn normalize
  [x]
  (let [m (walk/keywordize-keys x)]
    (cond
      (sequential? m) {:operator :and :terms (mapv term-of m)}
      (map? m)        {:operator (or (->kw (:operator m)) :and)
                       :terms    (mapv term-of (:terms m))}
      :else           {:operator :and :terms []})))

(defn- ref-name
  [x]
  (when (and (map? x) (= 1 (count x)))
    (some-> (or (:val x) (:var x)) ->kw)))

(defn- resolve-value
  [rv x]
  (cond
    (ref-name x)    (rv (ref-name x))
    (sequential? x) (into [] (mapcat #(let [r (resolve-value rv %)]
                                        (cond (sequential? r) r (nil? r) nil :else [r])))
                          x)
    :else           x))

(defn resolved
  [x rv]
  (update (normalize x) :terms
          (fn [ts]
            (mapv (fn [t] (reduce (fn [t k] (if (contains? t k)
                                              (update t k #(resolve-value rv %))
                                              t))
                                  t [:values :value :min :max]))
                  ts))))

(defn- blank?*
  [x]
  (or (nil? x) (and (string? x) (str/blank? x)) (and (coll? x) (empty? x))))

(declare ops)

(defn- open-term?
  [t]
  (if-let [open? (get-in ops [(:op t) :open?])] (boolean (open? t)) false))

(defn active
  [x]
  (vec (remove open-term? (:terms (normalize x)))))

(defn parse-syntax
  [s]
  (walk/postwalk #(if (and (vector? %) (= :phrase (first %))) [:word (second %)] %)
                 (tokens/parse s :and)))

(defn- syntax-tree
  [{:keys [value advanced?]}]
  (if advanced? (parse-syntax value) [:word (str value)]))

(defn- has-text?
  [hay needle]
  (str/includes? (str/lower-case (str hay)) (str/lower-case (str needle))))

(defn- fold-tree
  [alg tree]
  (let [go #(fold-tree alg %)]
    (case (first tree)
      :and  ((:and alg) (map go (rest tree)))
      :or   ((:or alg) (map go (rest tree)))
      :not  ((:not alg) (go (second tree)))
      :word ((:word alg) (second tree))
      (:else alg))))

(defn- holds
  [word?]
  {:and #(every? true? %) :or #(boolean (some true? %)) :not not :word word? :else true})

(defn- value-of
  [row prop]
  (let [vs (v row)]
    (if (contains? vs prop) (get vs prop) (get vs (name prop)))))

(defn- in-range?
  [x {:keys [min max]}]
  (and (some? x)
       (or (blank?* min) (when-let [[a b] (compare-operands x min)] (>= a b)))
       (or (blank?* max) (when-let [[a b] (compare-operands x max)] (<= a b)))))

(defn- row-type
  [row]
  (some-> (or (:object-type-ref row) (:object-type row)) str))

(defn- applies?
  [{:keys [type]} row]
  (or (nil? type) (nil? (row-type row)) (= (str type) (row-type row))))

(defn- keyword-hit?
  [{:keys [fields] :as t} row]
  (let [hay  (if (seq fields) (map #(value-of row (->kw %)) fields) (vals (v row)))
        tree (syntax-tree t)]
    (or (nil? tree)
        (fold-tree (holds (fn [w] (boolean (some #(has-text? % w) hay)))) tree))))

(defn- term-matches?
  [{:keys [op prop negate? link] :as t} row]
  (let [x   (when prop (value-of row prop))
        hit (if-let [hit (get-in ops [op :hit])] (hit t x row) true)]
    (or (some? link)
        (not (applies? t row))
        (if negate? (not hit) (boolean hit)))))

(defn matches?
  [x row]
  (let [{:keys [operator]} (normalize x)
        ts (active x)]
    (cond
      (empty? ts)      true
      (= :or operator) (boolean (some #(term-matches? % row) ts))
      :else            (every? #(term-matches? % row) ts))))

(defn- refuse!
  [code]
  (refusal/refuse! code {}))

(defn- all-of [ws] (if (next ws) {:and (vec ws)} (first ws)))
(defn- any-of [ws] (if (next ws) {:or (vec ws)} (first ws)))

(defn- where-of
  [props]
  {:and  #(all-of (vec %))
   :or   #(any-of (vec %))
   :not  (fn [_] (refuse! :refusal.object-filter/no-not-over-text))
   :word (fn [w] (any-of (mapv (fn [p] {p {:contains-ci w}}) props)))
   :else nil})

(defn- range-where
  [p {:keys [min max]}]
  (cond-> []
    (not (blank?* min)) (conj {p {:>= min}})
    (not (blank?* max)) (conj {p {:<= max}})))

(def ^:private ops
  {:is       {:open? #(blank?* (:values %))
              :hit   (fn [{:keys [values]} x _] (some? (some #(= (str %) (str x)) values)))
              :own   (fn [{:keys [prop values negate?]} _]
                       (if negate?
                         (all-of (mapv (fn [x] {:or [{prop {:not= x}} {prop {:nil? true}}]})
                                       values))
                         (any-of (mapv (fn [x] (if (nil? x) {prop {:nil? true}} {prop {:= x}}))
                                       values))))
              :hop   (fn [{:keys [prop values]}]
                       (if (next values)
                         (refuse! :refusal.object-filter/linked-one-value)
                         {prop {:= (first values)}}))}
   :null     {:hit (fn [_ x _] (nil? x))
              :own (fn [{:keys [prop negate?]} _] {prop {:nil? (not negate?)}})
              :hop (fn [{:keys [prop]}] {prop {:nil? true}})}
   :contains {:open? #(blank?* (:value %))
              :hit   (fn [{:keys [value]} x _] (and (some? x) (has-text? x value)))
              :own   (fn [{:keys [prop value negate?]} _]
                       (if negate?
                         (refuse! :refusal.object-filter/no-not-over-text)
                         {prop {:contains-ci (str value)}}))
              :hop   (fn [{:keys [prop value]}] {prop {:contains-ci (str value)}})}
   :range    {:open? #(and (blank?* (:min %)) (blank?* (:max %)))
              :hit   (fn [t x _] (in-range? x t))
              :own   (fn [{:keys [prop negate?] :as t} _]
                       (if negate?
                         (refuse! :refusal.object-filter/negated-range)
                         (all-of (range-where prop t))))
              :hop   (fn [{:keys [prop min max]}]
                       (cond-> {}
                         (not (blank?* min)) (assoc-in [prop :>=] min)
                         (not (blank?* max)) (assoc-in [prop :<=] max)))}
   :keyword  {:open?     #(blank?* (:value %))
              :hit       (fn [t _ row] (keyword-hit? t row))
              :any-prop? true
              :own       (fn [{:keys [fields] :as t} {:keys [strings]}]
                           (let [props (if (seq fields) (mapv ->kw fields) (vec strings))
                                 tree  (syntax-tree t)]
                             (when (and (seq props) tree) (fold-tree (where-of props) tree))))}
   :has-link {:hop (fn [_] {})}})

(defn- own-where
  [{:keys [op prop] :as t} {:keys [attr?] :as schema-of}]
  (let [{:keys [own any-prop?]} (ops op)]
    (when (and own (or any-prop? (attr? prop)))
      (own t schema-of))))

(defn- hop-where
  [{:keys [op] :as t}]
  (if-let [hop (get-in ops [op :hop])] (hop t) {}))

(defn ->where
  [x schema-of]
  (let [{:keys [operator]} (normalize x)
        ts   (active x)
        hops (filter :link ts)]
    (when (and (seq hops) (= :or operator) (next ts))
      (refuse! :refusal.object-filter/linked-or))
    {:own  (let [ws (into [] (keep #(own-where % schema-of)) (remove :link ts))]
             (when (seq ws) (if (= :or operator) (any-of ws) (all-of ws))))
     :hops (reduce (fn [acc t] (update acc (:link t) merge (hop-where t))) nil hops)}))

(defn- same-slot?
  [a b]
  (and (= (:prop a) (:prop b)) (= (:link a) (:link b)) (= (:op a) (:op b))))

(defn with-term
  [x t]
  (let [{:keys [terms] :as f} (normalize x)
        t (term-of t)]
    (assoc f :terms (if (some #(same-slot? % t) terms)
                      (mapv #(if (same-slot? % t) t %) terms)
                      (conj terms t)))))

(defn without-term
  [x t]
  (let [t (term-of t)]
    (update (normalize x) :terms (fn [ts] (filterv #(not (same-slot? % t)) ts)))))

(defn term-for
  [x slot]
  (let [slot (term-of slot)]
    (some #(when (same-slot? % slot) %) (:terms (normalize x)))))

(defn toggle-value
  [x slot value multi?]
  (let [slot (assoc slot :op :is)
        have (vec (:values (term-for x slot)))
        on?  (some #(= (str %) (str value)) have)
        vs   (cond on?    (filterv #(not= (str %) (str value)) have)
                   multi? (conj have value)
                   :else  [value])
        t    (assoc slot :values vs)]
    (if (seq vs) (with-term x t) (without-term x t))))

(defn with-operator
  [x op]
  (assoc (normalize x) :operator (->kw op)))

(defn- extract-key
  [dflt-term new-term k]
  (let [x (get dflt-term k)
        n (get new-term k)]
    (cond
      (ref-name x)    {(ref-name x) n}
      (sequential? x) (let [refs (keep ref-name x)]
                        (zipmap refs (concat (when (sequential? n) n) (repeat nil))))
      :else           nil)))

(defn extraction
  [default x]
  (reduce (fn [acc t]
            (let [n (term-for x t)]
              (into acc (mapcat #(extract-key t n %)) [:values :value :min :max])))
          {} (:terms (normalize default))))

(defn names-variables?
  [x]
  (boolean (some (fn [t] (some (fn [k]
                                 (let [y (get t k)]
                                   (or (ref-name y) (and (sequential? y) (some ref-name y)))))
                               [:values :value :min :max]))
                 (:terms (normalize x)))))
