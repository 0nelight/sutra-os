(ns sutra.ref.objectset
  (:refer-clojure :exclude [read])
  (:require [sutra.kit.coll :as coll]
            [sutra.ref.err :refer [fail!]]
            [sutra.ref.expr :as expr]
            [sutra.ref.geo :as geo]
            [sutra.ref.sides :as sides]
            [sutra.ref.sigma :as s]))

(set! *warn-on-reflection* true)

(def node-ops
  {:scan          {:in :none :kind :leaf   :fields #{:root} :required #{:root}}
   :static        {:in :none :kind :leaf   :fields #{:ids :root} :required #{:ids}}
   :filter        {:in :one  :kind :member :fields #{:where :include}}
   :search-around {:in :one  :kind :member :fields #{:link :dir} :required #{:link}}
   :with          {:in :one  :kind :member
                   :fields #{:name :fn :attr :expr :cast :link :dir :where :far-in}
                   :required #{:name :fn :link}}
   :compute       {:in :one  :kind :member :fields #{:name :expr :cast} :required #{:name :expr}}
   :union         {:in :many :kind :member}
   :intersect     {:in :many :kind :member}
   :subtract      {:in :many :kind :member}
   :order         {:in :one  :kind :shape  :fields #{:order-by :sort-dir}}
   :page          {:in :one  :kind :shape  :fields #{:limit :offset}}
   :project       {:in :one  :kind :shape  :fields #{:select}}
   :aggregate     {:in :one  :kind :shape  :fields #{:aggs :group-by :accuracy} :required #{:aggs}}
   :template      {:in :one  :kind :meta   :fields #{:param-slots} :required #{:param-slots}}})

(defn- inputs [{:keys [op in]}]
  (case (get-in node-ops [op :in]) :one [in] :many in []))

(defn- malformed! [msg data]
  (fail! :malformed-object-set msg data))

(defn- check-pred! [p]
  (when (some? p)
    (when-not (vector? p) (malformed! "a predicate node is a vector [op & args]" {:node p}))
    (let [[op & args] p
          n (count args)]
      (cond
        (#{:and :or} op)             (run! check-pred! args)
        (contains? expr/comparisons
                   op) (when-not (= 2 n) (malformed! "a comparison is [op field value]" {:node p}))
        (geo/ops op)
          (when-let [m (if (= 2 n) (apply geo/explain op args) (str op " is [op field value]"))]
            (malformed! m {:node p}))
        (#{:matches-tokens :matches-tokens-in} op)
                                     (when-not (and (= 2 n) (sequential? (first args)))
                                       (malformed! "[:matches-tokens [<attr> …] <text>]"
                                                   {:node p}))
        (= :id-in op)                (when-not (and (= 1 n) (coll? (first args)))
                                       (malformed! "[:id-in <coll-of-ids>]" {:node p}))
        (= :having-all op)           (when-not (and (= 2 n) (coll? (second args)))
                                       (malformed! "[:having-all <as> <coll>]" {:node p}))
        :else                        (malformed! (str "unknown predicate op " op) {:node p})))))

(def agg-fns {:count :optional :sum :required :avg :required :min :required :max :required
              :cardinality :required})

(def bucket-units #{:year :quarter :month :week :day :hour :minute :second})

(def max-buckets 10000)

(defn- term [t]
  (cond
    (keyword? t) {:kind :attr :attr t}
    (map? t)     (assoc t
                        :kind (cond (:unit t) :bucket (:width t) :width (:link t) :link
                                :else :attr))
    (vector? t)  (let [[k a x] t]
                   (case k
                     :bucket {:kind :bucket :attr a :unit x}
                     :width  {:kind :width :attr a :width x}
                     :origin {:kind :origin}
                     :link   {:kind :link :link a}
                     {:kind :unknown :term t}))
    :else        {:kind :unknown :term t}))

(defn- term-name [{:keys [kind attr unit width link name]}]
  (or name
      (case kind
        :attr   attr
        :bucket (keyword (str (clojure.core/name attr) "-" (clojure.core/name unit)))
        :width  (keyword (str (clojure.core/name attr) "-w" width))
        :origin :origin
        :link   (keyword (clojure.core/name link)))))

(defn- metric-name [{f :fn :keys [attr name]}]
  (or name
      (keyword (if attr (str (clojure.core/name attr) "-" (clojure.core/name f))
                 (clojure.core/name f)))))

(defn- aggregate-shape
  [{:keys [aggs group-by accuracy]}]
  (when-not (contains? #{nil :exact} accuracy)
    (malformed! (str ":accuracy " accuracy " — only :exact is evaluated") {:accuracy accuracy}))
  (let [metrics (cond (= :count aggs) [{:fn :count}]
                      (sequential? aggs) (vec aggs)
                      :else (malformed! ":aggs is :count or a vector of {:fn :attr :name}"
                                        {:aggs aggs}))
        terms   (mapv term (or group-by []))]
    (when (empty? metrics) (malformed! ":aggs must name at least one metric" {}))
    (doseq [m metrics]
      (let [need (agg-fns (:fn m))]
        (cond
          (not (map? m))
            (malformed! "a metric is a map {:fn :attr :name}" {:agg m})
          (nil? need)
            (malformed! (str "unknown aggregate fn " (:fn m)) {:agg m})
          (and (= :required need)
               (nil? (:attr m))) (malformed! (str (:fn m) " needs :attr") {:agg m})
          (and (some? (:attr m))
               (not (keyword? (:attr m)))) (malformed! ":attr is an attribute name" {:agg m}))))
    (doseq [{:keys [kind attr unit width link] :as t} terms]
      (cond
        (= :unknown kind)                    (malformed! "bad :group-by term" {:term (:term t)})
        (= :origin kind)                     nil
        (= :link
           kind)                       (when-not (keyword? link)
                                         (malformed! "[:link <side>]" {:term t}))
        (not (keyword? attr))                (malformed! ":group-by names an attribute" {:term t})
        (and (= :bucket kind)
             (not (bucket-units unit))) (malformed! (str "unknown bucket unit " unit) {:term t})
        (and (= :width kind)
             (not (and (number? width)
                       (pos? width)))) (malformed! "[:width a w] needs w > 0" {:term t})))
    (let [terms   (mapv #(assoc % :name (term-name %)) terms)
          metrics (mapv #(assoc % :name (metric-name %)) metrics)
          dupes   (keep (fn [[k n]] (when (> n 1) k))
                        (frequencies (map :name (concat terms metrics))))]
      (when (seq dupes) (malformed! "two result columns share a name" {:keys (vec dupes)}))
      {:terms terms :metrics metrics})))

(def with-fns #{:count :sum :avg :min :max})

(def compute-ops
  {:+ [2 nil] :- [1 nil] :* [2 nil] :div [2 2] :abs [1 1] :coalesce [1 nil]
   :concat-strings [2 nil]})

(defn compute-refs
  [e]
  (cond (keyword? e) [e]
        (vector? e)  (mapcat compute-refs (rest e))
        :else        []))

(defn- check-expr! [e]
  (cond
    (or (keyword? e) (number? e) (string? e) (nil? e)) nil
    (vector? e) (let [[op & args] e
                      [lo hi] (or (compute-ops op)
                                  (malformed!
                                   (str ":compute knows " (sort (keys compute-ops)) ", not " op)
                                   {:expr e}))]
                  (when (or (< (count args) lo) (and hi (> (count args) hi)))
                    (malformed! (str op " takes " lo (if hi (str "–" hi) " or more") " arguments")
                                {:expr e}))
                  (run! check-expr! args))
    :else (malformed! "a :compute operand is a number, a string, an attribute or a column"
                      {:expr e})))

(defn- cast-shape [c]
  (when-not (or (nil? c) (#{:integer :double} c)
                (and (vector? c) (= :decimal (first c)) (nat-int? (second c)) (= 2 (count c))))
    (malformed! ":compute :cast is :integer, :double or [:decimal scale]" {:cast c})))

(defn- far-in-shape
  [os]
  (loop [n os]
    (case (:op n)
      (:with :compute) (recur (:in n))
      (:scan :static)  nil
      (malformed! ":with :far-in is column nodes (:with, :compute) above a leaf" {:node n}))))

(defn- compute-shape [{:keys [name expr cast]}]
  (when-not (keyword? name)
    (malformed! ":compute names its column with a keyword :name" {:name name}))
  (check-expr! expr)
  (cast-shape cast))

(defn- with-shape
  [{:keys [name attr expr cast far-in] f :fn}]
  (when-not (keyword? name) (malformed! ":with names its column with a keyword :name" {:name name}))
  (when-not (with-fns f) (malformed! (str ":with :fn is one of " (sort with-fns)) {:fn f}))
  (when (and attr expr)
    (malformed! ":with folds an :attr or an :expr, not both" {:attr attr :expr expr}))
  (when (and (not= :count f) (nil? attr) (nil? expr))
    (malformed! (str ":with " f " needs :attr or :expr") {:fn f}))
  (when (and (some? attr) (not (keyword? attr)))
    (malformed! ":attr is an attribute name" {:attr attr}))
  (when (and (some? cast) (nil? expr)) (malformed! ":with :cast belongs to its :expr" {:cast cast}))
  (when (some? expr) (check-expr! expr) (cast-shape cast))
  (when (some? far-in) (far-in-shape far-in)))

(defn compute-value
  ([values e] (compute-value values e false))
  ([values e exact?]
   (cond
     (keyword? e) (get values e)
     (vector? e)  (let [[op & args] e
                        xs (map #(compute-value values % exact?) args)]
                    (case op
                      :coalesce       (first (remove nil? xs))
                      :concat-strings (apply (get-in expr/value-ops [op :fn]) xs)
                      (when (every? some? xs)
                        (cond
                          (and (= :div op) (zero? (second xs))) nil
                          exact? (apply (get-in expr/value-ops [op :fn]) xs)
                          :else
                          (case op
                            :+   (reduce +' xs)
                            :*   (reduce *' xs)
                            :-   (if (next xs) (reduce -' xs) (-' (first xs)))
                            :abs (let [x (first xs)] (if (neg? x) (-' x) x))
                            :div (let [[a b] xs] (/ (double a) (double b))))))))
     :else e)))

(defn cast-value
  [c v]
  (cond
    (or (nil? c) (not (number? v))) v
    (= :double c)  (double v)
    (= :integer c) (.longValueExact (.setScale (bigdec v) 0 java.math.RoundingMode/HALF_UP))
    :else          (.setScale (bigdec v) (int (second c)) java.math.RoundingMode/HALF_UP)))

(defn- check-node!
  [os]
  (when-not (map? os) (malformed! "an object-set node is a map {:op … :in …}" {:node os}))
  (let [{:keys [op in]} os
        {arity :in
         :keys [fields required]} (or (node-ops op) (malformed! (str "unknown op " op) {:node os}))
        extra   (remove (into #{:op :in} fields) (keys os))
        missing (remove #(contains? os %) required)]
    (case arity
      :none (when (some? in) (malformed! (str op " is a leaf and takes no :in") {:node os}))
      :one  (when-not (map? in) (malformed! (str op " takes one object set as :in") {:node os}))
      :many (when-not (and (sequential? in) (seq in) (every? map? in))
              (malformed! (str op " takes a non-empty vector of object sets") {:node os})))
    (when (seq missing) (malformed! (str op " requires " (vec missing)) {:node os}))
    (when (seq extra) (malformed! (str op " has no field(s) " (vec extra)) {:node os}))
    (when (= :template op)
      (malformed! "a template is not a read — substitute its :param-slots" {:node os}))
    (check-pred! (:where os))
    (when (= :aggregate op) (aggregate-shape os))
    (when (= :with op) (with-shape os))
    (when (= :compute op) (compute-shape os))
    (run! check-node! (inputs os))
    (some-> (:far-in os) check-node!)))

(defn- shaping? [os] (= :shape (get-in node-ops [(:op os) :kind])))

(defn check-read!
  [os]
  (check-node! os)
  (loop [n os seen #{}]
    (if (shaping? n)
      (let [op (:op n)]
        (when (seen op) (malformed! (str "a second " op " in one read") {:node n}))
        (when (and (seen :aggregate) (not= :aggregate op))
          (malformed! (str op " below an :aggregate — an aggregate has no window of members")
                      {:node n}))
        (recur (:in n) (conj seen op)))
      (do (when (every? seen [:aggregate :project])
            (malformed! ":project and :aggregate in one read" {}))
          (doseq [m (tree-seq map? inputs n) :when (shaping? m)]
            (malformed! (str (:op m) " in a member position") {:node m}))))))

(defn resolve-side
  [_cx W T handle]
  (sides/resolve-side W T handle))

(defn resolve-hop [_cx W T hop]
  (sides/resolve-hop W T hop))

(defn type-of
  [cx W os]
  (case (:op os)
    (:scan :static)                (:root os)
    :search-around                 (:far (resolve-hop cx W (type-of cx W (:in os)) os))
    (:union :intersect :subtract)  (let [ts (distinct (keep #(type-of cx W %) (:in os)))]
                                     (when (next ts)
                                       (fail! :type-mismatch
                                              (str (:op os) " over different types " (vec ts)
                                                   "")
                                              {:op (:op os) :types (vec ts)}))
                                     (first ts))
    (type-of cx W (:in os))))

(defn- ext [cx W T]
  (cond
    (contains? (:object-types W) T) (s/call cx :extension W T)
    (contains? (:interfaces W)
               T)   (mapcat #(s/call cx :extension W %) (s/call cx :implementers W T))
    :else (fail! :unknown-type (str "Unknown type: " T) {:type T})))

(defn- declared [cx W T]
  (if (contains? (:interfaces W) T)
    (vec (s/call cx :interface-properties W T))
    (mapv :attribute-name (get-in W [:object-types T :attribute-list]))))

(defn- check-attrs! [cx W T attrs & [extra]]
  (let [known (into (set (declared cx W T)) extra)]
    (doseq [a attrs :when (not (known a))]
      (fail! :unknown-attribute (str T " has no attribute " a) {:type T :attribute a}))))

(defn- pred-attrs [p]
  (when (vector? p)
    (let [[op a] p]
      (cond (#{:and :or} op) (mapcat pred-attrs (rest p))
            (contains? expr/comparisons op) [a]
            (#{:matches-tokens :matches-tokens-in} op) a
            (geo/ops op) (geo/field-attrs a)))))

(defn- adjacency
  ([cx W σ] (adjacency cx W σ #{}))
  ([cx W {:keys [link dir]} into]
   (reduce (fn [m [f t]] (let [[n x] (if (= :to dir) [f t] [t f])] (update m n (fnil conj into) x)))
           {} (s/call cx :edges W link))))

(declare fold-values computing)

(defn- withs
  [os]
  (loop [n os acc []]
    (case (:op n)
      (:with :compute) (recur (:in n) (conj acc n))
      :filter          (recur (:in n) acc)
      acc)))

(defn- with-names [os] (map :name (withs os)))

(defn- far-value
  [{:keys [attr expr cast]}]
  (cond
    attr #(get (:values %) attr)
    expr #(cast-value cast (compute-value (:values %) expr (some? cast)))))

(defn- with-value
  [cx W T {:keys [attr expr where far-in] f :fn :as node}]
  (let [σ    (resolve-hop cx W T node)
        far  (:far σ)
        cols (when far-in (with-names far-in))
        _    (check-attrs! cx W far (cond-> (vec (pred-attrs where))
                                      attr (conj attr)
                                      expr (into (compute-refs expr)))
                           cols)
        adj  (adjacency cx W σ)
        val  (far-value node)
        with-cols (if far-in (computing cx W far far-in) identity)
        objs (coll/index-by :id (filter #(expr/match? % where) (map with-cols (ext cx W far))))]
    (fn [o]
      (let [xs (keep objs (get adj (:id o)))]
        (fold-values f (when val (keep val xs)) (count xs))))))

(defn- compute-fn
  [cx W T {:keys [expr in cast]}]
  (check-attrs! cx W T (compute-refs expr) (with-names in))
  (fn [o] (cast-value cast (compute-value (:values o) expr (some? cast)))))

(defn- computing
  [cx W T os]
  (let [fs (mapv (fn [n]
                   [(:name n)
                    (if (= :compute (:op n)) (compute-fn cx W T n) (with-value cx W T n))])
                 (reverse (withs os)))]
    (if (empty? fs)
      identity
      (fn [o] (reduce (fn [o [nm f]] (assoc-in o [:values nm] (f o))) o fs)))))

(defn- check-with-names!
  [cx W T os]
  (let [names (with-names os)
        known (set (declared cx W T))]
    (doseq [nm names :when (known nm)]
      (malformed! (str "column " nm " shadows an attribute of " T) {:name nm :type T}))
    (doseq [[nm k] (frequencies names) :when (> k 1)]
      (malformed! (str "two columns are named " nm) {:name nm}))))

(declare ids-of)

(defn- typed
  [os near]
  (cond-> os (and (= :static (:op os)) (nil? (:root os))) (assoc :root near)))

(defn- having-all
  [sides o [op as C]]
  (when-not (= :having-all op)
    (fail! :unsupported-predicate (str "cannot evaluate predicate node " op) {:op op}))
  (let [adj (or (get sides as)
                (fail! :unknown-include (str "having-all names no :include " as) {:as as}))]
    (every? (get adj (:id o) #{}) C)))

(defn- where-test [cx W {:keys [where include in] :as node}]
  (let [T     (type-of cx W node)
        _     (check-attrs! cx W T (pred-attrs where) (with-names in))
        sides (into {} (map (fn [i] [(:as i) (adjacency cx W (resolve-hop cx W T i))])) include)]
    #(expr/match? % where (partial having-all sides))))

(defn- membership
  [cx W os]
  (let [all (fn [] (mapv #(membership cx W %) (:in os)))]
    (case (:op os)
      :scan              (constantly true)
      :static            (let [I (set (:ids os))] #(contains? I (:id %)))
      :filter            (let [in? (membership cx W (:in os))
                               ok? (where-test cx W os)
                               ext (computing cx W (type-of cx W os) (:in os))]
                           #(and (in? %) (ok? (ext %))))
      (:with :compute)   (let [T (type-of cx W os)]
                           (check-with-names! cx W T os)
                           (membership cx W (:in os)))
      :search-around     (let [σ   (resolve-hop cx W (type-of cx W (:in os)) os)
                               adj (adjacency cx W σ)
                               far (into #{} (mapcat adj) (ids-of cx W (typed (:in os) (:near σ))))]
                           #(contains? far (:id %)))
      :union             (let [fs (all)] (fn [o] (boolean (some #(% o) fs))))
      :intersect         (let [fs (all)] (fn [o] (every? #(% o) fs)))
      :subtract          (let [[f & r] (all)] (fn [o] (and (f o) (not-any? #(% o) r)))))))

(defn members
  [cx W os]
  (let [T (or (type-of cx W os)
              (fail! :untyped-read "a static set without :root cannot be read" {:node os}))]
    (filterv (membership cx W os) (ext cx W T))))

(defn- ids-of [cx W os]
  (into #{} (map :id) (members cx W os)))

(defn- code-points ^ints [^String s] (.toArray (.codePoints s)))

(defn- cmp
  [a b]
  (cond (and (nil? a) (nil? b)) 0 (nil? a) 1 (nil? b) -1
        (and (string? a) (string? b)) (java.util.Arrays/compare (code-points a) (code-points b))
        (and (inst? a) (inst? b))     (compare (inst-ms a) (inst-ms b))
        :else (compare a b)))

(defn- key-order [dir]
  (let [c (fn [xs ys] (or (first (remove zero? (map cmp xs ys))) 0))]
    (if (= :desc dir) #(c %2 %1) c)))

(defn- check-paging! [{:keys [limit offset]}]
  (when (and (some? offset) (nil? limit)) (malformed! ":offset without :limit" {:offset offset}))
  (when (and (some? offset) (neg? offset))
    (malformed! ":offset must not be negative" {:offset offset}))
  (when (and (some? limit) (neg? limit)) (malformed! ":limit must not be negative" {:limit limit})))

(defn- window [{:keys [limit offset]} xs]
  (cond->> xs offset (drop offset) limit (take limit)))

(defn- rows
  [cx W T X {:keys [order-by sort-dir select] :as shape} computed]
  (check-attrs! cx W T (cond-> (vec select) order-by (conj order-by)) computed)
  (let [attrs (cond->> (concat (declared cx W T) computed) (seq select) (filter (set select)))]
    (->> (if order-by
           (sort-by (fn [o] [(get-in o [:values order-by]) (:id o)]) (key-order (or sort-dir :asc))
                    X)
           (sort-by :id cmp X))
         (window shape)
         (mapv (fn [o] {:id (:id o) :object-type-ref (:object-type-ref o)
                        :values (into {} (map (fn [a] [a (get-in o [:values a])])) attrs)})))))

(defn- ->utc ^java.time.LocalDateTime [v]
  (let [utc java.time.ZoneOffset/UTC]
    (condp instance? v
      java.time.LocalDateTime  v
      java.time.LocalDate      (.atStartOfDay ^java.time.LocalDate v)
      java.time.OffsetDateTime
        (.toLocalDateTime (.withOffsetSameInstant ^java.time.OffsetDateTime v utc))
      java.time.ZonedDateTime
        (.toLocalDateTime (.withZoneSameInstant ^java.time.ZonedDateTime v utc))
      java.time.Instant        (java.time.LocalDateTime/ofInstant v utc)
      java.sql.Timestamp       (.toLocalDateTime ^java.sql.Timestamp v)
      java.sql.Date            (.atStartOfDay (.toLocalDate ^java.sql.Date v))
      java.util.Date
        (java.time.LocalDateTime/ofInstant (.toInstant ^java.util.Date v) utc)
      String                   (some (fn [parse] (try (->utc (parse v))
                                                      (catch java.time.DateTimeException _ nil)))
                                     [#(java.time.Instant/parse %)
                                      #(java.time.OffsetDateTime/parse %)
                                      #(java.time.LocalDateTime/parse %)
                                      #(java.time.LocalDate/parse %)])
      nil)))

(defn bucket-label
  [unit v]
  (when-let [t (->utc v)]
    (let [f #(.format t (java.time.format.DateTimeFormatter/ofPattern %))]
      (case unit
        :year    (f "yyyy")
        :quarter (str (f "yyyy") "-Q" (inc (quot (dec (.getMonthValue t)) 3)))
        :month   (f "yyyy-MM")
        :week    (format "%04d-W%02d" (.get t java.time.temporal.IsoFields/WEEK_BASED_YEAR)
                         (.get t java.time.temporal.IsoFields/WEEK_OF_WEEK_BASED_YEAR))
        :day     (f "yyyy-MM-dd")
        :hour    (f "yyyy-MM-dd HH")
        :minute  (f "yyyy-MM-dd HH:mm")
        :second  (f "yyyy-MM-dd HH:mm:ss")))))

(defn- width-value
  [v w]
  (when (number? v)
    (if (and (integer? v) (integer? w))
      (* (Math/floorDiv (long v) (long w)) w)
      (* (Math/floor (/ (double v) (double w))) w))))

(defn- hop-below
  [os]
  (case (:op os)
    :search-around            os
    (:filter :with :compute :order :project) (recur (:in os))
    (fail! :no-origin "[:origin] needs a :search-around to group by" {:op (:op os)})))

(defn- instances
  [cx W T member X terms]
  (let [base (if-let [t (first (filter #(= :origin (:kind %)) terms))]
               (let [hop  (hop-below member)
                     σ    (resolve-hop cx W (type-of cx W (:in hop)) hop)
                     back (adjacency cx W (update σ :dir {:to :from :from :to}))
                     from (ids-of cx W (typed (:in hop) (:near σ)))]
                 (for [o X, origin (sort-by identity cmp (filter from (get back (:id o))))]
                   {:o o (:name t) origin}))
               (map (fn [o] {:o o}) X))]
    (reduce (fn [insts {:keys [link name]}]
              (let [σ   (resolve-side cx W T link)
                    adj (adjacency cx W σ)
                    tk  (get-in W [:object-types (:far σ) :title-key])
                    far (coll/index-by :id (ext cx W (:far σ)))
                    ls  (fn [o] (sort-by identity cmp (filter far (get adj (:id o)))))]
                (mapcat (fn [inst]
                          (if-let [l (seq (ls (:o inst)))]
                            (for [l l]
                              (assoc inst
                                     name (cond-> {:id l}
                                            tk (assoc :title (get-in (far l) [:values tk])))))
                            [(assoc inst name nil)]))
                        insts)))
            base
            (filter #(= :link (:kind %)) terms))))

(defn- term-value [inst {:keys [kind attr unit width name]}]
  (let [v (get-in inst [:o :values attr])]
    (case kind
      :attr   v
      :bucket (bucket-label unit v)
      :width  (width-value v width)
      (:origin :link) (get inst name))))

(defn fold-values
  [f xs n]
  (let [best (fn [better?] (reduce #(if (better? (cmp %2 %1)) %2 %1) xs))]
    (case f
      :count       (if xs (count xs) n)
      :cardinality (count (distinct xs))
      :sum         (reduce +' 0 xs)
      :avg         (when (seq xs) (.divide (bigdec (reduce +' xs)) (bigdec (count xs))
                                           java.math.MathContext/DECIMAL128))
      :min         (when (seq xs) (best neg?))
      :max         (when (seq xs) (best pos?)))))

(defn- metric-value [{f :fn a :attr} insts]
  (fold-values f (when a (keep #(get-in % [:o :values a]) insts)) (count insts)))

(defn- order-val [v] (if (map? v) (:id v) v))

(defn- aggregate
  [cx W T member X shape computed]
  (let [{:keys [terms metrics]} (aggregate-shape shape)
        {:keys [order-by sort-dir]} shape]
    (check-attrs! cx W T (keep :attr (concat terms metrics)) computed)
    (let [insts   (instances cx W T member X terms)
          groups  (if (empty? terms)
                    [[[] insts]]
                    (group-by (fn [i] (mapv #(term-value i %) terms)) insts))
          buckets (map (fn [[k is]]
                         {:group   (zipmap (map :name terms) k)
                          :metrics (into {} (map (fn [m] [(:name m) (metric-value m is)]))
                                         metrics)})
                       groups)
          value   (fn [b n] (order-val (get (:group b) n (get (:metrics b) n))))
          names   (set (map :name (concat terms metrics)))]
      (when (> (count buckets) max-buckets)
        (fail! :bucket-limit-exceeded (str "more than " max-buckets " buckets")
               {:buckets (count buckets) :limit max-buckets}))
      (if (empty? terms)
        (vec buckets)
        (let [keys (if order-by
                     (if (names order-by)
                       (cons order-by (remove #{order-by} (map :name terms)))
                       (fail! :bad-aggregate-order
                              (str "order an aggregate by one of its result names " (sort names))
                              {:order-by order-by}))
                     (map :name terms))
              dir  (if order-by (or sort-dir :asc) :asc)]
          (->> buckets
               (sort-by (fn [b] (mapv #(value b %) keys)) (key-order dir))
               (window shape)
               vec))))))

(defn- peel
  [os]
  (loop [n os shape {}]
    (case (:op n)
      :order     (recur (:in n) (merge shape (select-keys n [:order-by :sort-dir])))
      :page      (recur (:in n) (merge shape (select-keys n [:limit :offset])))
      :project   (recur (:in n) (merge shape (select-keys n [:select])))
      :aggregate (recur (:in n) (merge shape (select-keys n [:aggs :group-by :accuracy])))
      [n shape])))

(defn read
  [cx W os]
  (check-read! os)
  (let [[member shape] (peel os)
        _ (check-paging! shape)
        T (or (type-of cx W member)
              (fail! :untyped-read "a static set without :root cannot be read" {:node os}))
        X (mapv (computing cx W T member) (members cx W member))
        computed (reverse (with-names member))]
    (if (contains? shape :aggs)
      (aggregate cx W T member X shape computed)
      (rows cx W T X shape computed))))

(def resource-keys #{:id :object-set-name :project :ignore-inherited :description :lifecycle})

(def ^:private flat-layers
  [[:filter [:where :include]] [:aggregate [:aggregate :group-by :accuracy]] [:project [:select]]
   [:order [:order-by :sort-dir]] [:page [:limit :offset]]])

(def flat-keys
  (into #{:root :param-slots} (mapcat second) flat-layers))

(defn flat->ir
  [{:keys [root] :as flat}]
  (when-let [ks (seq (remove flat-keys (keys flat)))]
    (fail! :unknown-key (str "unknown key(s) " (vec ks) " in a saved set")
           {:unknown (vec ks) :allowed (vec (sort flat-keys))}))
  (reduce (fn [ir [op ks]]
            (let [fields (into {} (keep (fn [k] (when (some? (get flat k))
                                                  [(if (= :aggregate k) :aggs k) (get flat k)])))
                               ks)]
              (if (empty? fields) ir (into {:op op :in ir} fields))))
          (cond-> {:op :scan} (contains? flat :root) (assoc :root root))
          flat-layers))

(defn- temporal?
  [x]
  (or (instance? java.time.temporal.Temporal x) (instance? java.util.Date x)
      (and (string? x)
           (boolean (some (fn [parse]
                            (try (parse x) true (catch java.time.DateTimeException _ false)))
                          [#(java.time.Instant/parse %) #(java.time.OffsetDateTime/parse %)
                           #(java.time.LocalDateTime/parse %) #(java.time.LocalDate/parse %)])))))

(def ^:private slot-types
  {:string string? :integer integer? :double number? :decimal number? :boolean boolean?
   :id string?
   :date      #(temporal? %)
   :timestamp #(temporal? %)})

(defn- slot [s]
  (let [s (if (keyword? s) {:name s} s)]
    (when-not (and (map? s) (keyword? (:name s)) (or (nil? (:type s)) (slot-types (:type s))))
      (malformed! "a param slot is :name or {:name :type :optional? :default}" {:slot s}))
    s))

(defn- param? [x] (and (vector? x) (= :param (first x))))

(defn- bind-args
  [slots args]
  (let [by-name (coll/index-by :name slots)]
    (when-let [ks (seq (remove by-name (keys args)))]
      (fail! :unknown-argument (str "no slot named " (vec ks))
             {:unknown (vec ks) :slots (vec (keys by-name))}))
    (into {} (for [{:keys [name type optional? default]} slots]
               (let [v (if (contains? args name)
                         (get args name)
                         (if optional? default
                             (fail! :missing-argument (str "missing argument " name) {:slot name})))
                     ok? (slot-types type (constantly true))]
                 (when-not (or (nil? v) (if (sequential? v) (every? ok? v) (ok? v)))
                   (fail! :type-mismatch (str "argument " name " is not " type)
                          {:slot name :expected type}))
                 [name v])))))

(defn- subst-pred [vals p]
  (let [arg (fn [x] (if (param? x)
                      (let [k (second x)]
                        (if (contains? vals k) (get vals k)
                            (fail! :unknown-argument (str "[:param " k "] names no slot")
                                   {:param k})))
                      x))
        as-list (fn [v] (cond (nil? v) [] (sequential? v) (vec v) :else [v]))]
    (when (some? p)
      (let [[op a b] p]
        (cond
          (#{:and :or} op)    (into [op] (map #(subst-pred vals %)) (rest p))
          (= :id-in op)       [:id-in (as-list (arg a))]
          (= :having-all op)  [:having-all a (as-list (arg b))]
          (geo/ops op)        [op a (if (map? b) (update-vals b arg) (arg b))]
          (and (contains? expr/comparisons op) (param? b))
          (let [v (arg b)]
            (cond (not (sequential? v)) [op a v]
                  (not= :=
                        op)           (fail! :type-mismatch
                                             (str "a list argument needs := , not " op) {:op op})
                  (seq v)                (into [:or] (map #(vector := a %)) v)
                  :else                  [:id-in []]))
          :else p)))))

(defn- subst
  [vals node]
  (cond
    (map? node)    (cond-> node
                     (contains? node :where) (update :where #(subst-pred vals %))
                     (param? (:ids node))    (update :ids (fn [[_ k]]
                                                            (let [v (get vals k)]
                                                              (cond (nil? v) []
                                                                (sequential? v) (vec v)
                                                                :else [v]))))
                     (contains? node :in)    (update :in #(subst vals %)))
    (vector? node) (mapv #(subst vals %) node)
    :else node))

(defn instantiate
  [definition args]
  (let [template? (= :template (:op definition))
        slots     (mapv slot (:param-slots definition))
        body      (if template? (:in definition) (dissoc definition :param-slots))
        names     (set (map :name slots))
        _         (when-let [k (first (filter names
                                              (tree-seq coll?
                                                        #(cond (map? %) (vals %) (param? %) [] :else
                                                           (seq %)) body)))]
                    (malformed! (str "slot " k " used bare — write [:param " k "]")
                                {:slot k}))
        body      (if (:op body) body (flat->ir body))
        ir        (subst (bind-args slots (or args {})) body)]
    (when-let [p (first (filter param? (tree-seq coll? seq ir)))]
      (malformed! (str p " outside a value position") {:param p}))
    ir))

(defn lookup [W n]
  (or (get-in W [:object-sets n])
      (some (fn [[_ spec]] (when (= n (:object-set-name spec)) spec)) (:object-sets W))))

(defn query
  [cx W n args]
  (let [spec (or (lookup W n)
                 (fail! :unknown-object-set (str "Unknown object-set: " n) {:object-set n}))
        _    (s/call cx :authorize :read {:type :object-sets :id (:id spec)})
        r    (read cx W (instantiate (apply dissoc spec resource-keys) args))]
    r))
