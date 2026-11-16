(ns sutra.store.derived
  (:require [sutra.ref.err :as rerr]
            [sutra.ref.expr :as rexpr]
            [sutra.ref.objectset :as ros]
            [sutra.ref.sides :as sides]
            [sutra.types.attribute :as attribute]
            [sutra.types.expr :as texpr]
            [sutra.types.query :as q]
            [sutra.types.value-types :as vt]))

(set! *warn-on-reflection* true)

(def lowering
  {:connected-count   {:plan :with :fn :count}
   :connected-sum     {:plan :with :fn :sum}
   :connected-avg     {:plan :with :fn :avg}
   :connected-min     {:plan :with :fn :min}
   :connected-max     {:plan :with :fn :max}
   :connected?        {:plan :with :fn :count :boolean? true}
   :+                 {:plan :compute}
   :-                 {:plan :compute}
   :*                 {:plan :compute}
   :div               {:plan :compute}
   :abs               {:plan :compute}
   :coalesce          {:plan :compute}
   :concat-strings    {:plan :compute}
   :get               {:plan :read}
   :round             {:plan :evaluated}
   :mod               {:plan :evaluated}
   :connected-one     {:plan :evaluated}
   :connected-arg-max {:plan :evaluated}
   :connected-all?    {:plan :evaluated}})

(defn- plan-of [x] (when (vector? x) (get-in lowering [(first x) :plan] :evaluated)))

(defn derived-of
  [W T]
  (for [a (get-in W [:object-types T :attribute-list])
        :let [e (attribute/derived-expression a)]
        :when e]
    [(:attribute-name a) e]))

(defn column?
  [k]
  (and (keyword? k) (= (namespace ::_) (namespace k))))

(defn- col-name [d & [i]] (keyword (namespace ::_) (cond-> (name d) i (str "." i))))

(defn cast-of
  [W T d]
  (when-let [t (some #(when (= d (:attribute-name %)) (:attribute-type %))
                     (get-in W [:object-types T :attribute-list]))]
    (let [{:keys [kind precision
                  scale]} (try (vt/col-type-of t) (catch clojure.lang.ExceptionInfo _ nil))]
      (case kind
        :decimal                          (when precision [:decimal (or scale 0)])
        (:tinyint :smallint :int :bigint) :integer
        (:float :double)                  :double
        nil))))

(defn- ir-pred?
  [p]
  (or (nil? p)
      (and (vector? p)
           (let [[op & args] p]
             (cond (#{:and :or} op) (every? ir-pred? args)
                   (contains? rexpr/comparisons op) (keyword? (first args))
                   :else false)))))

(defn- far-type
  [W T over]
  (try (:far (sides/resolve-side W T over))
       (catch clojure.lang.ExceptionInfo ex
         (when-not (#{:unknown-side :ambiguous-side} (rerr/kind ex)) (throw ex)))))

(defn- boolean-test
  [col [op _ v]]
  (case [op (boolean v)]
    ([:= true] [:not= false] [:!= false]) [:> col 0]
    ([:= false] [:not= true] [:!= true])  [:= col 0]
    [op col v]))

(defn- predicate-on
  [cols p]
  (if-not (vector? p)
    p
    (let [[op a] p]
      (cond (#{:and :or} op) (into [op] (map #(predicate-on cols %)) (rest p))
            (contains? cols a) (let [c (cols a)]
                                 (if (:boolean? c) (boolean-test (:name c) p)
                                   (assoc p 1 (:name c))))
            :else p))))

(defn- chain-columns
  [n]
  (loop [n n acc #{}]
    (case (:op n)
      (:with :compute) (recur (:in n) (conj acc (:name n)))
      :filter          (recur (:in n) acc)
      acc)))

(defn- with-steps
  [member steps]
  (reduce (fn [in step] (if ((chain-columns in) (:name step)) in (assoc step :in in)))
          member steps))

(defn- far-derived?
  [W far a]
  (and (some? a) (contains? (into #{} (map first) (derived-of W far)) a)))

(defn- far-row-expr
  [W F a]
  (let [derived (into {} (derived-of W F))
        stored  (set (map :attribute-name (get-in W [:object-types F :attribute-list])))]
    (letfn [(ex [seen x]
              (cond
                (or (number? x) (string? x)) x
                (keyword? x) (cond
                               (seen x)    nil
                               (derived x) (when (nil? (cast-of W F x))
                                             (ex (conj seen x) (derived x)))
                               (stored x)  x)
                (and (vector? x) (= :read (plan-of x)) (= 2 (count x))) (ex seen (second x))
                (and (vector? x) (= :compute (plan-of x)))
                (let [args (mapv #(ex seen %) (rest x))]
                  (when (every? some? args) (into [(first x)] args)))))]
      (when (contains? derived a) (ex #{a} (derived a))))))

(declare lower)

(defn- far-relation
  [W far needed visiting]
  (when (seq needed)
    (let [cols (reduce (fn [acc a]
                         (if-let [c (and (not (visiting [far a]))
                                         (lower W far a (conj visiting [far a])))]
                           (assoc acc a c)
                           (reduced nil)))
                       {} needed)]
      (when cols
        {:far-in (with-steps {:op :scan :root far} (mapcat :steps (vals cols)))
         :cols   cols}))))

(defn- fold-step
  [W T e nm visiting]
  (let [[op & args] e
        {:keys [over of where pred]} (q/link-args op args)
        where   (if (= :connected? op) pred where)
        far     (far-type W T over)
        derived? #(far-derived? W far %)
        w-needs (filterv derived? (texpr/field-refs where))
        row     (when (and (derived? of) (empty? w-needs)) (far-row-expr W far of))
        needed  (when-not row (distinct (cond-> w-needs (derived? of) (conj of))))
        rel     (far-relation W far needed visiting)
        cols    (:cols rel)]
    (when (and far (ir-pred? where) (or (nil? of) (keyword? of))
               (or (empty? needed) rel)
               (not (:boolean? (get cols of))))
      (merge {:op :with :name nm :fn (get-in lowering [op :fn]) :link over}
             (cond
               row (cond-> {:expr row} (cast-of W far of) (assoc :cast (cast-of W far of)))
               of  {:attr (or (get-in cols [of :name]) of)})
             (when where {:where (predicate-on cols where)})
             (when-let [far-in (:far-in rel)] {:far-in far-in})))))

(declare lower*)

(defn- lower-expr
  [{:keys [W T derived stored d visiting] :as cx} steps x]
  (cond
    (or (number? x) (string? x)) [x steps]
    (keyword? x) (if (contains? derived x)
                   (when-let [c (lower* cx x)]
                     (when-not (:boolean? c) [(:name c) (into steps (:steps c))]))
                   (when (stored x) [x steps]))
    :else
    (case (plan-of x)
      :read    (when (= 2 (count x)) (lower-expr cx steps (second x)))
      :with    (when-not (get-in lowering [(first x) :boolean?])
                 (let [nm (col-name d (inc (count steps)))]
                   (when-let [step (fold-step W T x nm visiting)] [nm (conj steps step)])))
      :compute (when-let [[args steps] (reduce (fn [[args steps] t]
                                                 (if-let [[t' steps] (lower-expr cx steps t)]
                                                   [(conj args t') steps]
                                                   (reduced nil)))
                                               [[] steps] (rest x))]
                 [(into [(first x)] args) steps])
      nil)))

(defn- lower*
  [{:keys [W T derived seen visiting] :as cx} d]
  (let [e    (derived d)
        cast (cast-of W T d)]
    (when (and e (not (seen d)))
      (if (= :with (plan-of e))
        (let [boolean? (boolean (get-in lowering [(first e) :boolean?]))]
          (if (or boolean? (nil? cast))
            (when-let [step (fold-step W T e (col-name d) visiting)]
              {:name (col-name d) :steps [step] :boolean? boolean?})
            (when-let [step (fold-step W T e (col-name d "w") visiting)]
              {:name  (col-name d)
               :steps [step {:op :compute :name (col-name d) :expr (:name step) :cast cast}]})))
        (when-let [[expr steps] (lower-expr (assoc cx :d d :seen (conj seen d)) [] e)]
          (when (vector? expr)
            {:name  (col-name d)
             :steps (conj steps (cond-> {:op :compute :name (col-name d) :expr expr}
                                  cast (assoc :cast cast)))}))))))

(defn- lowering-cx [W T visiting]
  {:W W :T T :derived (into {} (derived-of W T)) :seen #{} :visiting visiting
   :stored (set (map :attribute-name (get-in W [:object-types T :attribute-list])))})

(defn lower
  ([W T d] (lower W T d #{}))
  ([W T d visiting] (lower* (lowering-cx W T visiting) d)))

(defn- residual
  [W T d steps x]
  (if-let [step (and (= :with (plan-of x))
                   (fold-step W T x (col-name d (str "f" (inc (count steps)))) #{}))]
    [(if (get-in lowering [(first x) :boolean?]) [:> (:name step) 0] (:name step))
     (conj steps step)]
    (if (and (vector? x) (seq x))
      (let [[args steps] (reduce (fn [[args steps] y]
                                   (let [[y' steps] (residual W T d steps y)]
                                     [(conj args y') steps]))
                                 [[] steps] (rest x))]
        [(into [(first x)] args) steps])
      [x steps])))

(defn- reading
  [W T derived]
  (let [declared (into {} (derived-of W T))]
    (reduce (fn [[how steps] [a e]]
              (if-let [c (and (= e (declared a)) (lower W T a))]
                [(conj how [a {:column c}]) (into steps (:steps c))]
                (let [[e' steps] (residual W T a steps e)]
                  [(conj how [a {:expr e'}]) steps])))
            [[] []]
            derived)))

(defn- lowered
  [W T attrs]
  (into {} (keep (fn [a] (when-let [c (lower W T a)] [a c]))) attrs))

(def ^:private shaping? #{:order :page :project :aggregate})

(defn- under-shaping
  [n f]
  (if (shaping? (:op n)) (update n :in under-shaping f) (f n)))

(defn- term-map [t]
  (cond (keyword? t) {:attr t}
        (map? t)     t
        (and (vector? t) (#{:bucket :width} (first t)))
        (let [[k a x] t] (if (= :bucket k) {:attr a :unit x} {:attr a :width x}))))

(defn- term-name [{:keys [attr unit width name]}]
  (or name (cond unit  (keyword (str (clojure.core/name attr) "-" (clojure.core/name unit)))
                 width (keyword (str (clojure.core/name attr) "-w" width))
                 :else attr)))

(defn- lower-filters
  [W n]
  (let [n (cond-> n
            (map? (:in n))        (update :in #(lower-filters W %))
            (sequential? (:in n)) (update :in #(mapv (partial lower-filters W) %)))]
    (if-let [cols (and (= :filter (:op n)) (:where n)
                       (not-empty (lowered W (ros/type-of {} W n)
                                           (map second
                                                (filter vector?
                                                        (tree-seq vector? rest (:where n)))))))]
      (-> n (update :in with-steps (mapcat :steps (vals cols)))
          (update :where #(predicate-on cols %)))
      n)))

(defn- lower-aggregate
  [W n]
  (let [T     (ros/type-of {} W n)
        aggs  (when (sequential? (:aggs n)) (:aggs n))
        cols  (lowered W T (concat (keep :attr aggs) (keep (comp :attr term-map) (:group-by n))))]
    (if (empty? cols)
      n
      (-> n
          (assoc :aggs (mapv (fn [{f :fn a :attr :as m}]
                               (if-let [c (cols a)]
                                 (assoc m :attr (:name c)
                                        :name (or (:name m) (keyword (str (name a) "-" (name f)))))
                                 m))
                             aggs))
          (update :group-by #(mapv (fn [t]
                                     (let [tm (term-map t)]
                                       (if-let [c (some-> tm :attr cols)]
                                         (assoc tm :attr (:name c) :name (term-name tm))
                                         t)))
                                   %))
          (update :in under-shaping #(with-steps % (mapcat :steps (vals cols))))))))

(defn- lower-shaping
  [W n]
  (if (shaping? (:op n))
    (let [n (update n :in #(lower-shaping W %))]
      (case (:op n)
        :aggregate (lower-aggregate W n)
        :order     (if-let [c (lower W (ros/type-of {} W n) (:order-by n))]
                     (-> n (assoc :order-by (:name c))
                         (update :in under-shaping #(with-steps % (:steps c))))
                     n)
        n))
    (lower-filters W n)))

(defn- shaping-chain
  [ir]
  (take-while some? (iterate #(when (shaping? (:op %)) (:in %)) ir)))

(defn- answers-objects?
  [W ir]
  (when-not (some #(= :aggregate (:op %)) (shaping-chain ir))
    (ros/type-of {} W ir)))

(defn- projected
  [ir]
  (some #(when (= :project (:op %)) (:select %)) (shaping-chain ir)))

(defn- read-list
  [W T ir derived]
  (if derived
    (texpr/eval-order derived)
    (texpr/eval-order (derived-of W T) (when-let [s (projected ir)] (set s)))))

(defn plan
  ([W ir] (plan W ir nil))
  ([W ir derived]
   (let [ir (lower-shaping W ir)]
     (if-let [T (answers-objects? W ir)]
       (let [[how steps] (reading W T (read-list W T ir derived))
             inputs (into #{} (comp (keep (comp :expr second)) (mapcat texpr/field-refs)) how)
             names  (into (set (map (comp :name :column second) (filter (comp :column second) how)))
                          (map :name) steps)]
         (letfn [(extend-select [n]
                                (if (shaping? (:op n))
                                  (cond-> (update n :in extend-select)
                                    (= :project
                                       (:op n))
                                      (update :select #(vec (distinct (concat % names inputs)))))
                                  (with-steps n steps)))]
           (if (seq steps) (extend-select ir) ir)))
       ir))))

(defn plan-attrs
  [W ir]
  (let [T (ros/type-of {} W ir)]
    (concat (when-let [w (when-not (= :with (:op ir)) (:where ir))]
              (for [[op a] (filter vector? (tree-seq vector? rest w))
                    :when (and (keyword? op) (not (#{:and :or :id-in :having-all} op))
                               (keyword? a))]
                [T a]))
            (when-let [o (:order-by ir)] (for [a (if (sequential? o) o [o])] [T a]))
            (when (sequential? (:aggs ir)) (for [a (keep :attr (:aggs ir))] [T a]))
            (for [t (:group-by ir)
                  :let [a (:attr (term-map t))]
                  :when a]
              [T a])
            (when-let [in (:in ir)]
              (if (map? in) (plan-attrs W in) (mapcat #(plan-attrs W %) in))))))

(defn stated-columns
  [W T derived]
  (let [[how _] (reading W T derived)]
    (into {}
          (keep (fn [[a {:keys [column expr]}]]
                  (cond column        [a (:name column)]
                        (keyword? expr) [a expr])))
          how)))

(defn unstated
  [W ir]
  (first (filter (fn [[T a]] (some #{a} (map first (derived-of W T)))) (plan-attrs W ir))))

(def ^:private read-as-nil
  #{:nil-in-computation :division-by-zero})

(defn read-value
  ([T id a f] (read-value T id a f nil))
  ([T id a f advisory]
   (try (f)
        (catch clojure.lang.ExceptionInfo ex
          (when-not (or (read-as-nil (rerr/kind ex))
                        (and advisory (advisory a)))
            (throw (ex-info (str "cannot read " a " of " T " " id ": " (ex-message ex))
                            (assoc (ex-data ex) :type T :id id :attribute a)
                            ex)))))))

(defn evaluate
  [evaluate* V e]
  (letfn [(ev [x]
            (if (= :compute (plan-of x))
              (let [vs (mapv ev (rest x))
                    ks (mapv #(keyword (str "_" %)) (range (count vs)))]
                (ros/compute-value (zipmap ks vs) (into [(first x)] ks) true))
              (evaluate* V (literal x))))
          (literal [x]
            (cond (= :compute (plan-of x)) (ev x)
                  (= :with (plan-of x))    x
                  (and (vector? x) (seq x)) (into [(first x)] (map literal) (rest x))
                  :else x))]
    (ev e)))

(defn- unscale
  [^java.math.BigDecimal v]
  (if (<= (.scale v) 0)
    (let [b (.toBigInteger v)] (if (<= (.bitLength b) 62) (.longValue b) v))
    v))

(defn fold-value
  [f v]
  (if (instance? java.math.BigDecimal v)
    (case f
      (:count :sum) (unscale v)
      :avg          (let [s (.stripTrailingZeros ^java.math.BigDecimal v)]
                      (if (<= (.scale s) 0) (unscale s) (double s)))
      v)
    v))

(defn leftovers
  ([W ir] (leftovers W ir nil))
  ([W ir derived]
   (if-let [T (answers-objects? W ir)]
     (into [] (keep (fn [[a {:keys [expr]}]] (when expr [a expr])))
           (first (reading W T (read-list W T ir derived))))
     [])))

(defn reader
  ([W ir] (reader W ir nil nil))
  ([W ir derived] (reader W ir derived nil))
  ([W ir derived advisory]
   (when-let [T (answers-objects? W ir)]
     (let [[how steps] (reading W T (read-list W T ir derived))
           fold  (into {} (keep (fn [step] (when (:fn step) [(:name step) (:fn step)]))) steps)
           unset (into {} (map (fn [{:keys [attribute-name]}] [attribute-name nil]))
                       (get-in W [:object-types T :attribute-list]))
           shown (some-> (projected ir) set)
           casts (into {} (map (fn [[a _]] [a (cast-of W T a)])) how)
           own   (fn [values] (into {} (remove (comp column? key)) values))]
       (fn [row columns evaluate*]
         (let [cols (into {} (keep (fn [[k v]] (when (column? k) [k (fold-value (fold k) v)])))
                          columns)]
           (cond-> (reduce (fn [r [a {:keys [column expr]}]]
                             (assoc-in r [:values a]
                                       (ros/cast-value
                                        (casts a)
                                        (if column
                                          (let [v (get cols (:name column))]
                                            (if (:boolean? column) (some-> v pos?) v))
                                          (read-value T (:id r) a
                                                      #(evaluate (fn [V e] (evaluate* r V e))
                                                                 (merge unset (:values r) cols)
                                                                 expr)
                                                      advisory)))))
                           (update row :values own)
                           how)
             shown (update :values select-keys shown))))))))
