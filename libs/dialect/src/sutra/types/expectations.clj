(ns sutra.types.expectations
  (:require [sutra.types.kinds :as kinds]
            [sutra.types.table :as tbl]
            [sutra.kit.anomaly :as anomaly]
            [clojure.string :as str]
            [sutra.types.column :as column]
            [sutra.types.transform-io :as tio])
  (:import (java.time Instant LocalDate LocalDateTime OffsetDateTime ZoneOffset ZonedDateTime)))

(set! *warn-on-reflection* true)

(defn col-key
  [c]
  (column/attr->col (keyword (name c))))

(defn canonical-rows
  [rows]
  (mapv (fn [r] (persistent! (reduce-kv (fn [m k v] (assoc! m (col-key k) v)) (transient {}) r)))
        rows))

(defn- blank? [v] (or (nil? v) (= "" v)))

(defn temporal? [v]
  (or (instance? Instant v) (instance? OffsetDateTime v) (instance? ZonedDateTime v)
      (instance? LocalDateTime v) (instance? LocalDate v) (instance? java.util.Date v)))

(defn- parse-instant
  [s]
  (let [s (str/replace (str/trim s) #"T(.*[+-]\d{2})(\d{2})$" "T$1:$2")]
    (some (fn [f] (try (f s) (catch java.time.DateTimeException _ nil)))
          [#(Instant/parse %)
           #(.toInstant (OffsetDateTime/parse %))
           #(.toInstant (LocalDateTime/parse %) ZoneOffset/UTC)
           #(.toInstant (.atStartOfDay (LocalDate/parse %)) ZoneOffset/UTC)])))

(defn ->instant
  [v]
  (cond
    (instance? Instant v)        v
    (instance? OffsetDateTime v) (.toInstant ^OffsetDateTime v)
    (instance? ZonedDateTime v)  (.toInstant ^ZonedDateTime v)
    (instance? LocalDateTime v)  (.toInstant ^LocalDateTime v ZoneOffset/UTC)
    (instance? LocalDate v)      (.toInstant (.atStartOfDay ^LocalDate v) ZoneOffset/UTC)
    (instance? java.sql.Date v)  (->instant (.toLocalDate ^java.sql.Date v))
    (instance? java.util.Date v) (.toInstant ^java.util.Date v)
    (string? v)                  (parse-instant v)
    :else                        nil))

(defn- ->num [v]
  (cond (number? v) v
        (string? v) (parse-double (str/trim v))
        :else       nil))

(defn- comparable-pair
  [a b]
  (cond
    (and (number? a) (number? b))     [a b]
    (or (temporal? a) (temporal? b))  (let [x (->instant a) y (->instant b)] (when (and x y) [x y]))
    (number? b)                       (some-> (->num a) (vector b))
    (number? a)                       (some->> (->num b) (vector a))
    :else                             [(str a) (str b)]))

(defn- cmp
  [a b]
  (when-let [[x y] (comparable-pair a b)] (compare x y)))

(defn- same? [a b] (= 0 (cmp a b)))

(defn- plus-ms [^Instant i ms] (.plusMillis i (long ms)))

(defn- seconds-between [^Instant from ^Instant to]
  (/ (double (- (.toEpochMilli to) (.toEpochMilli from))) 1000.0))

(def ^:private type-preds
  {:string    string?
   :boolean   boolean?
   :integer   integer?
   :long      integer?
   :short     integer?
   :byte      integer?
   :double    number?
   :float     number?
   :decimal   number?
   :date      #(or (instance? LocalDate %) (instance? java.sql.Date %))
   :timestamp temporal?})

(defn- type-pred
  [t]
  (let [t (if (string? t) (keyword t) t)]
    (cond
      (and (vector? t) (= :array (first t)))
      (let [p (type-pred (second t))]
        (fn [v] (and (sequential? v) (every? #(or (blank? %) (p %)) v))))

      (and (vector? t) (= :decimal (first t))) number?

      :else (or (get type-preds t)
                (anomaly/coded! :refusal.dialect/expectations-unknown-type :incorrect
                  "unknown type {type:pr}"
                  {:problem (str "unknown type " (pr-str t) " — one of "
                                 (str/join ", " (map name (sort (keys type-preds))))
                                 " or [:array t]")}
                  {:type t})))))

(defn- row-check [pred] {:level :row :pred pred})
(defn- dataset-check [run] {:level :dataset :run run})
(defn- row? [c] (= :row (:level c)))

(defn- elide [n s] (if (> (count s) n) (str (subs s 0 n) "…") s))

(defn- show [x] (elide 160 (pr-str x)))

(defn- verdict
  ([passed? detail] (verdict passed? detail 0))
  ([passed? detail failing] {:passed? (boolean passed?) :detail detail :failing failing}))

(defn run
  [c rows]
  (if (row? c)
    (let [bad (into [] (remove (:pred c)) rows)]
      (if (empty? bad)
        (verdict true (str (count rows) " row(s) pass"))
        (verdict false (str (count bad) " of " (count rows) " row(s) fail, e.g. "
                            (str/join " " (map show (take 3 bad))))
                 (count bad))))
    ((:run c) rows)))

(defn- problem! [form msg]
  (anomaly/coded! :refusal.dialect/expectation :incorrect
    "expectation {shown}: {problem}"
    {:expectation form :problem msg} {:shown (show form)}))

(defn- step
  [form s]
  (cond (keyword? s) [s]
        (and (vector? s) (keyword? (first s))) s
        :else (problem! form (str "a step is a keyword or [op & args], not " (show s)))))

(defn- dataset-ref?
  [x]
  (and (vector? x) (= :dataset-ref (first x))))

(defn- ref-rows
  [form {:keys [ref aliases]} alias]
  (when (and aliases (not (contains? aliases (keyword (name alias)))))
    (problem! form (str "dataset-ref " (pr-str alias) " names no input of this transform — "
                        "one of " (str/join ", " (map name (sort aliases))))))
  (delay (canonical-rows (if ref (ref (keyword (name alias))) []))))

(defn- arg
  [form env v]
  (if (dataset-ref? v)
    (let [[_ alias what] v]
      (when-not (= :count what)
        (problem! form "a dataset-ref compared as a value is [:dataset-ref alias :count]"))
      (let [rows (ref-rows form env alias)] #(count @rows)))
    (constantly v)))

(defn- null-passes [p] (fn [x row] (or (blank? x) (p x row))))

(defn- ordering [ok?]
  (fn [form env [v]]
    (let [v (arg form env v)] (null-passes (fn [x _] (some-> (cmp x (v)) ok?))))))

(defn- col-ordering [ok?]
  (fn [_ _ [c]]
    (let [k (col-key c)]
      (fn [x row] (let [y (get row k)] (or (blank? x) (blank? y) (some-> (cmp x y) ok?)))))))

(defn- ts-ordering [ok?]
  (fn [form _ [ts]]
    (let [at (or (->instant ts) (problem! form (str "not a timestamp: " (pr-str ts))))]
      (null-passes (fn [x _] (some-> (->instant x) (compare at) ok?))))))

(defn- ts-col-ordering [ok?]
  (fn [_ _ [c offset]]
    (let [k (col-key c) ms (long (* 1000 (double (or offset 0))))]
      (fn [x row]
        (let [a (->instant x) b (some-> (get row k) ->instant (plus-ms ms))]
          (or (blank? x) (blank? (get row k)) (and a b (ok? (compare a b)))))))))

(def ^:private not-neg (complement neg?))
(def ^:private not-pos (complement pos?))

(def tests
  {:gt (ordering pos?) :gte (ordering not-neg) :lt (ordering neg?) :lte (ordering not-pos)
   :equals     (fn [form env [v]] (let [v (arg form env v)] (null-passes (fn [x _] (same? x (v))))))
   :not-equals (fn [form env [v]]
                 (let [v (arg form env v)] (null-passes (fn [x _] (not (same? x (v)))))))
   :non-null   (fn [_ _ _] (fn [x _] (not (blank? x))))
   :is-null    (fn [_ _ _] (fn [x _] (blank? x)))
   :is-in      (fn [_ _ vs]
                 (let [in? (fn [x] (some #(if (nil? %)
                                            (blank? x)
                                            (and (not (blank? x)) (same? x %)))
                                         vs))]
                   (fn [x _] (boolean (if (sequential? x) (every? in? x) (in? x))))))
   :rlike      (fn [form _ [re]]
                 (let [p (try (re-pattern re)
                               (catch Exception e
                                 (problem! form (ex-message e))))]
                   (null-passes (fn [x _] (re-find p (str x))))))
   :has-type   (fn [_ _ [t]] (let [p (type-pred t)] (null-passes (fn [x _] (p x)))))
   :array-contains (fn [_ _ [v]] (fn [x _] (and (sequential? x) (boolean (some #(same? % v) x)))))
   :equals-col     (col-ordering zero?)
   :not-equals-col (fn [_ _ [c]]
                     (let [k (col-key c)]
                       (fn [x row] (let [y (get row k)]
                                     (or (blank? x) (blank? y) (not (same? x y)))))))
   :gt-col (col-ordering pos?) :gte-col (col-ordering not-neg)
   :lt-col (col-ordering neg?) :lte-col (col-ordering not-pos)
   :is-after (ts-ordering pos?) :is-on-or-after (ts-ordering not-neg)
   :is-before (ts-ordering neg?) :is-on-or-before (ts-ordering not-pos)
   :is-after-col (ts-col-ordering pos?) :is-on-or-after-col (ts-col-ordering not-neg)
   :is-before-col (ts-col-ordering neg?) :is-on-or-before-col (ts-col-ordering not-pos)
   :is-in-foreign-col
   (fn [form env [r]]
     (let [[_ alias sel] (when (dataset-ref? r) r)
           [op c]        (when (vector? sel) sel)]
       (when-not (and alias (= :col op) c)
         (problem! form "is-in-foreign-col takes [:dataset-ref alias [:col column]]"))
       (let [rows (ref-rows form env alias) k (col-key c)
             vals (delay (into #{} (comp (map #(get % k)) (remove blank?) (map str)) @rows))]
         (null-passes (fn [x _] (contains? @vals (str x)))))))})

(def ^:private row-tests
  #{:equals-col :not-equals-col :gt-col :gte-col :lt-col :lte-col
    :is-after-col :is-on-or-after-col :is-before-col :is-on-or-before-col})

(defn- variance [xs sample?]
  (let [xs (mapv double xs) n (count xs)]
    (when (> n (if sample? 1 0))
      (let [m (/ (reduce + xs) n)]
        (Math/sqrt (/ (reduce + (map #(let [d (- % m)] (* d d)) xs))
                      (if sample? (dec n) n)))))))

(defn- extreme [better?]
  (fn [xs] (reduce (fn [a b] (if (or (nil? a) (some-> (cmp b a) better?)) b a))
                   nil (remove blank? xs))))

(def aggregates
  {:null-count            (fn [xs] (count (filter blank? xs)))
   :null-percentage       (fn [xs] (if (seq xs)
                                     (double (/ (count (filter blank? xs)) (count xs)))
                                     0.0))
   :distinct-count        (fn [xs] (count (into #{} (remove blank?) xs)))
   :approx-distinct-count (fn [xs] (count (into #{} (remove blank?) xs)))
   :sum                   (fn [xs] (reduce + 0 (keep ->num xs)))
   :standard-deviation-sample     (fn [xs] (variance (keep ->num xs) true))
   :standard-deviation-population (fn [xs] (variance (keep ->num xs) false))
   :max-value             (extreme pos?)
   :min-value             (extreme neg?)})

(defn- now-of
  [now]
  (or now (anomaly/raise! :fault "a check against the current time needs the run's :now" {})))

(defn- value-steps
  [{:keys [now]}]
  {:size (fn [x] (when (sequential? x) (count x)))
   :timestamp-offset-to-current-time
   (fn [x] (some-> (->instant x) (seconds-between (now-of now))))
   :timestamp-offset-from-current-time
   (fn [x] (some-> (now-of now) (seconds-between (->instant x))))})

(defn- seconds-arg
  [v]
  (if (map? v)
    (reduce-kv (fn [s u n] (+ s (* n (case u :seconds 1 :minutes 60 :hours 3600 :days 86400 0))))
               0 v)
    v))

(defn- chain
  [form env steps]
  (when (empty? steps) (problem! form "a column chain ends in a test, e.g. [:gt 0]"))
  (let [steps (mapv #(step form %) steps)
        [t-op & t-args] (peek steps)
        mods  (pop steps)
        vsteps (value-steps env)
        offset? (some #(#{:timestamp-offset-to-current-time :timestamp-offset-from-current-time}
                        (first %)) mods)]
    (when-not (contains? tests t-op)
      (problem! form (str "the last step " (pr-str t-op) " is no test — one of "
                          (str/join ", " (map name (sort (keys tests)))))))
    (doseq [[op] mods]
      (when-not (or (contains? aggregates op) (contains? vsteps op))
        (problem! form (str (pr-str op) " is no column step — one of "
                            (str/join ", " (map name (sort (concat (keys aggregates)
                                                                   (keys vsteps)))))))))
    (let [aggs (filterv (comp aggregates first) mods)
          [pre post] (split-with (comp not aggregates first) mods)
          vfn  (fn [ms] (apply comp identity (reverse (map (comp vsteps first) ms))))]
      (when (> (count aggs) 1) (problem! form "a column chain takes one aggregate"))
      (when (and (seq aggs) (row-tests t-op))
        (problem! form (str (pr-str t-op) " compares two columns of a row — not an aggregate")))
      (when (and (seq aggs) (seq pre))
        (problem! form "a value step before an aggregate — aggregate first"))
      {:aggregate (some-> (ffirst aggs) aggregates)
       :value     (vfn (if (seq aggs) (rest post) mods))
       :test      ((tests t-op) form env (cond->> t-args offset? (mapv seconds-arg)))
       :test-op   t-op})))

(defmulti compile*
  (fn [form _] (when (vector? form) (first form))))

(defn compile-expectation
  [form env]
  (let [form (if (keyword? form) [form] form)]
    (when-not (and (vector? form) (keyword? (first form)))
      (problem! form "an expectation is [op & args], e.g. [:col :a [:gt 0]]"))
    (when-not (contains? (methods compile*) (first form))
      (problem! form (str "unknown expectation " (pr-str (first form)) " — one of "
                          (str/join ", " (->> (keys (methods compile*)) (remove nil?)
                                              sort (map name))))))
    (compile* form env)))

(defmethod compile* :true  [_ _] (row-check (constantly true)))
(defmethod compile* :false [_ _] (row-check (constantly false)))

(defn- children [form env es]
  (when (empty? es) (problem! form "takes at least one expectation"))
  (mapv #(compile-expectation % env) es))

(defmethod compile* :all
  [[_ & es :as form] env]
  (let [cs (children form env es)]
    (if (every? row? cs)
      (row-check (apply every-pred (map :pred cs)))
      (dataset-check
       (fn [rows]
         (let [rs (map #(run % rows) cs) bad (remove :passed? rs)]
           (verdict (empty? bad) (str/join "; " (map :detail (if (seq bad) bad rs)))
                    (reduce + 0 (map :failing bad)))))))))

(defmethod compile* :any
  [[_ & es :as form] env]
  (let [cs (children form env es)]
    (if (every? row? cs)
      (row-check (fn [r] (boolean (some #((:pred %) r) cs))))
      (dataset-check
       (fn [rows]
         (let [rs (mapv #(run % rows) cs)]
           (if-let [ok (first (filter :passed? rs))]
             (verdict true (:detail ok))
             (verdict false (str "none pass: " (str/join "; " (map :detail rs)))
                      (apply min (map :failing rs))))))))))

(defmethod compile* :negate
  [[_ e & more :as form] env]
  (when (or (nil? e) (seq more)) (problem! form "negate takes one expectation"))
  (let [c (compile-expectation e env)]
    (if (row? c)
      (row-check (complement (:pred c)))
      (dataset-check (fn [rows] (let [r (run c rows)]
                                  (verdict (not (:passed? r)) (str "negated: " (:detail r)))))))))

(defmethod compile* :when
  [[_ w t o & more :as form] env]
  (when (or (nil? t) (seq more)) (problem! form "when takes [:when when then otherwise?]"))
  (let [[w t o] (map #(compile-expectation % env) [w t (or o [:true])])]
    (when-not (row? w) (problem! form "the when-expectation of :when is tested row by row"))
    (if (and (row? t) (row? o))
      (row-check (fn [r] (if ((:pred w) r) ((:pred t) r) ((:pred o) r))))
      (dataset-check
       (fn [rows]
         (let [{yes true no false} (group-by (comp boolean (:pred w)) rows)
               a (run t (vec yes)) b (run o (vec no))]
           (verdict (and (:passed? a) (:passed? b))
                    (str "then: " (:detail a) "; otherwise: " (:detail b))
                    (+ (:failing a) (:failing b)))))))))

(defn- aggregate-check
  [{:keys [aggregate value test]} k]
  (fn [rows]
    (let [v (value (aggregate (map #(get % k) rows)))]
      [(boolean (test v nil)) v])))

(defmethod compile* :col
  [[_ c & steps :as form] env]
  (when-not (or (keyword? c) (string? c) (symbol? c)) (problem! form "[:col column & steps]"))
  (let [k (col-key c)]
    (if (= [[:exists]] (mapv #(step form %) steps))
      (dataset-check (fn [rows] (let [ok (boolean (some #(contains? % k) rows))]
                                  (verdict ok (str (name c) (if ok " exists" " is no column"))))))
      (let [{:keys [aggregate value test] :as ch} (chain form env steps)]
        (if aggregate
          (let [agg (aggregate-check ch k)]
            (dataset-check (fn [rows] (let [[ok v] (agg rows)]
                                        (verdict ok (str (name c) " → " (pr-str v)))))))
          (row-check (fn [r] (boolean (test (value (get r k)) r)))))))))

(defn- groups [ks rows]
  (if (empty? ks) {[] rows} (group-by (fn [r] (mapv #(get r %) ks)) rows)))

(defn- group-verdict
  [cols failed n what]
  (if (empty? failed)
    (verdict true (str n " group(s) by " (pr-str (mapv name cols)) " pass"))
    (verdict false (str (count failed) " of " n " group(s) by " (pr-str (mapv name cols)) " "
                        what ", e.g. " (str/join " " (map show (take 3 failed))))
             (count failed))))

(defmethod compile* :group-by
  [[_ cols & steps :as form] env]
  (when-not (and (vector? cols) (every? #(or (keyword? %) (string? %)) cols))
    (problem! form "[:group-by [columns…] & steps]"))
  (let [ks    (mapv col-key cols)
        steps (mapv #(step form %) steps)
        [[op & args] & more] steps]
    (case op
      :is-unique
      (do (when (or (seq args) (seq more)) (problem! form ":is-unique ends the chain"))
          (when (empty? ks) (problem! form ":is-unique needs the columns it groups by"))
          (dataset-check
           (fn [rows]
             (let [gs (groups ks rows)
                   dup (keep (fn [[k rs]] (when (> (count rs) 1) [k (count rs)])) gs)]
               (group-verdict cols (map (fn [[k n]] (str (show k) " ×" n)) dup) (count gs)
                              "are not unique")))))

      :count
      (let [[[t-op & t-args]] more
            test (when (contains? tests t-op) ((tests t-op) form env t-args))]
        (when-not (and test (= 1 (count more)) (not (row-tests t-op)))
          (problem! form ":count ends in one value test, e.g. [:count [:gt 0]]"))
        (dataset-check
         (fn [rows]
           (let [gs  (groups ks rows)
                 bad (keep (fn [[k rs]] (when-not (test (count rs) nil) [k (count rs)])) gs)]
             (if (empty? ks)
               (verdict (empty? bad) (str "count " (count rows)))
               (group-verdict cols (map (fn [[k n]] (str (show k) " count " n)) bad) (count gs)
                              "fail"))))))

      :col
      (let [[c] args
            k   (col-key (or c (problem! form "[:col column] inside :group-by")))
            ch  (chain form env more)]
        (when-not (:aggregate ch)
          (problem! form "a column inside :group-by is aggregated, e.g. [:col :v] :sum [:gt 0]"))
        (let [agg (aggregate-check ch k)]
          (dataset-check
           (fn [rows]
             (let [gs  (groups ks rows)
                   bad (keep (fn [[g rs]] (let [[ok v] (agg rs)] (when-not ok [g v]))) gs)]
               (group-verdict cols (map (fn [[g v]] (str (show g) " " (name c) "=" (pr-str v)))
                                        bad)
                              (count gs) "fail"))))))

      (problem! form "a :group-by chain is :is-unique, :count [test] or [:col c] aggregate [test]"))))

(defmethod compile* :count
  [[_ & steps] env]
  (compile-expectation (into [:group-by [] :count] steps) env))

(defmethod compile* :primary-key
  [[_ & cols :as form] _]
  (when (empty? cols) (problem! form "[:primary-key column & columns]"))
  (let [ks (mapv col-key cols)]
    (dataset-check
     (fn [rows]
       (let [nulls (filterv (fn [r] (some #(blank? (get r %)) ks)) rows)
             gs    (groups ks (remove (set nulls) rows))
             dup   (keep (fn [[k rs]] (when (> (count rs) 1) (str (show k) " ×" (count rs)))) gs)
             ok?   (and (empty? nulls) (empty? dup))]
         (verdict ok?
                  (if ok?
                    (str (count rows) " row(s), key " (pr-str (mapv name cols)) " unique, not null")
                    (str/join "; " (cond-> []
                                     (seq nulls) (conj (str (count nulls)
                                                            " row(s) with a null key"))
                                     (seq dup)   (conj (str (count dup) " duplicate key(s), e.g. "
                                                            (str/join " " (take 3 dup)))))))
                  (+ (count nulls) (count dup))))))))

(defmethod compile* :schema
  [[_ [op types] :as form] _]
  (when-not (and (#{:contains :equals :is-subset-of} op) (map? types))
    (problem! form "[:schema [:contains|:equals|:is-subset-of {column type}]]"))
  (let [want (into {} (map (fn [[c t]] [(col-key c) (type-pred t)])) types)]
    (dataset-check
     (fn [rows]
       (let [have    (into #{} (mapcat keys) rows)
             missing (remove have (keys want))
             extra   (remove want have)
             typed   (remove (fn [[k p]] (every? #(let [v (get % k)] (or (blank? v) (p v))) rows))
                             (filter (comp have key) want))
             bad     (cond-> []
                       (and (#{:contains :equals} op) (seq missing))
                       (conj (str "missing " (pr-str (mapv name missing))))
                       (and (#{:equals :is-subset-of} op) (seq extra))
                       (conj (str "not declared " (pr-str (mapv name (sort extra)))))
                       (seq typed)
                       (conj (str "wrong type " (pr-str (mapv (comp name key) typed)))))]
         (verdict (empty? bad)
                  (if (seq bad) (str/join "; " bad) (str (count have) " column(s) fit"))
                  (count bad)))))))

(def on-errors #{:fail :warn})

(defn on-error
  [c]
  (keyword (str/lower-case (name (or (:on-error c) :fail)))))

(defn evaluate
  [{:keys [name expectation] :as c} rows env]
  (let [r (try (run (compile-expectation expectation env) (canonical-rows rows))
               (catch clojure.lang.ExceptionInfo e
                 (if (:problem (ex-data e)) (verdict false (ex-message e) 0) (throw e))))]
    {:name name :on-error (on-error c) :status (if (:passed? r) :passed :failed)
     :detail (elide 2000 (:detail r)) :failing (:failing r)}))

(defn- op-kw [s] (keyword (str/replace (name s) "_" "-")))

(defn from-wire
  [x]
  (cond
    (and (map? x) (contains? x :E))
    (into [(op-kw (:E x))] (concat (map from-wire (:args x)) (map from-wire (:steps x))))

    (and (map? x) (contains? x :step))
    (if (seq (:args x)) (into [(op-kw (:step x))] (map from-wire (:args x))) (op-kw (:step x)))

    (sequential? x) (mapv from-wire x)
    (map? x)        (update-vals x from-wire)
    :else           x))

(defn checks-from-wire
  [checks]
  (mapv (fn [{:keys [expectation name on_error]}]
          {:name name :expectation (from-wire expectation)
           :on-error (keyword (str/lower-case (or on_error "FAIL")))})
        checks))

(def checkable-input-kinds
  (tbl/where kinds/kinds :whole-rows?))

(defn- inputs-map [t] (if (map? (:inputs t)) (:inputs t) {}))

(defn checks-of
  [t]
  (concat
   (for [[alias in] (inputs-map t), c (:checks in)] {:scope [:input alias] :check c})
   (for [o (tio/outputs-of t), c (:checks o)] {:scope [:output (:name o)] :check c})))

(defn input-checks
  [t]
  (into {} (for [[alias in] (inputs-map t) :when (seq (:checks in))] [alias (:checks in)])))

(defn output-checks
  [t out-name]
  (let [outs (tio/outputs-of t)
        o    (if (and (nil? out-name) (= 1 (count outs)))
               (first outs)
               (some #(when (= (some-> out-name name) (some-> (:name %) name)) %) outs))]
    (:checks o)))

(defn- check-problems
  [aliases {:keys [name expectation] :as c}]
  (cond
    (not (map? c)) [(str "a check is {:name :expectation :on-error}, not " (show c))]
    (str/blank? (str name)) ["a check needs a :name — it identifies the check in Data Health"]
    (not (on-errors (on-error c))) [(str name ": :on-error is :fail or :warn")]
    (nil? expectation) [(str name ": no :expectation")]
    :else (try (compile-expectation expectation {:aliases aliases}) []
               (catch clojure.lang.ExceptionInfo e
                 (if (:problem (ex-data e)) [(str name ": " (ex-message e))] (throw e))))))

(defn transform-problems
  [t]
  (let [cs      (checks-of t)
        ins     (inputs-map t)
        aliases (into #{} (comp (filter (fn [[_ in]] (checkable-input-kinds
                                                      (keyword (:kind in)))))
                                (map (comp keyword name key)))
                      ins)
        dup     (keep (fn [[n k]] (when (> k 1) n)) (frequencies (keep (comp :name :check) cs)))
        ps      (concat
                 (map #(str "check name " (pr-str %) " is used twice — a name is one check") dup)
                 (mapcat #(check-problems aliases (:check %)) cs)
                 (for [[alias in] ins
                       :when (and (seq (:checks in))
                                  (not (checkable-input-kinds (keyword (:kind in)))))]
                   (str "input " (name alias) " is a " (name (:kind in)) " — a check reads a "
                        "dataset, transform or object-type input whole"))
                 (when (and (:incremental t) (not= :replace (:write-mode (:incremental t))))
                   (for [o (tio/outputs-of t)
                         :when (and (seq (:checks o)) (or (:object-type o) (:link-type o)))]
                     (str "output " (some-> (:name o) name)
                          " is an incremental object/link output — "
                          "its checks would see only what one run emits, never the whole; "
                          "land a :dataset and check that"))))]
    (not-empty (vec ps))))
