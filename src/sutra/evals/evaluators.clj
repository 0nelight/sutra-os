(ns sutra.evals.evaluators
  (:require [sutra.kit.refusal :as refusal]
            [clojure.string :as str])
  (:import (java.time LocalDate)
           (java.time.temporal ChronoUnit)))

(set! *warn-on-reflection* true)

(defn fval [m field] (when (and (map? m) (some? field)) (get m (keyword field))))

(defn as-decimal ^java.math.BigDecimal [x]
  (cond (number? x) (bigdec x)
        (string? x) (try (bigdec (str/trim x)) (catch NumberFormatException _ nil))))

(defn as-date
  ^LocalDate [x]
  (cond (instance? LocalDate x)          x
        (instance? java.sql.Date x)      (.toLocalDate ^java.sql.Date x)
        (instance? java.time.Instant x)  (LocalDate/parse (subs (str x) 0 10))
        (instance? java.util.Date x)     (LocalDate/parse
                                          (subs (str (.toInstant ^java.util.Date x)) 0 10))
        :else (let [s (str x)]
                (when (re-matches #"\d{4}-\d{2}-\d{2}([T ].*)?" s)
                  (try (LocalDate/parse (subs s 0 10))
                       (catch java.time.format.DateTimeParseException _ nil))))))

(defn same-value? [a e]
  (cond
    (= a e) true
    (and (map? a) (map? e))
    (and (= (set (map name (keys a))) (set (map name (keys e))))
         (every? (fn [[k v]] (same-value? (get a (keyword (name k))) v)) e))
    (and (sequential? a) (sequential? e))
    (and (= (count a) (count e)) (every? true? (map same-value? a e)))
    (or (keyword? a) (keyword? e))
    (and (or (keyword? a) (string? a)) (or (keyword? e) (string? e)) (= (name a) (name e)))
    (or (number? a) (number? e))
    (let [x (as-decimal a) y (as-decimal e)] (boolean (and x y (zero? (.compareTo x y)))))
    :else
    (let [x (as-date a) y (as-date e)] (boolean (and x y (= x y))))))

(defn- squash [s] (-> (str s) str/lower-case (str/replace #"\s+" " ") str/trim))

(defn- as-coll [x] (cond (nil? x) [] (sequential? x) x (set? x) (vec x) :else [x]))

(defn- text-match? [{:keys [ignore-case ignore-whitespace]} a e]
  (if (and (string? a) (string? e) (or ignore-case ignore-whitespace))
    (let [f (comp (if ignore-case str/lower-case identity)
                  (if ignore-whitespace #(str/replace % #"\s+" "") identity))]
      (= (f a) (f e)))
    (same-value? a e)))

(defn levenshtein [a b]
  (let [a (str a) b (str b) n (count b)]
    (peek (reduce (fn [prev ca]
                    (reduce (fn [row j]
                              (conj row (min (inc (peek row))
                                             (inc (nth prev (inc j)))
                                             (+ (nth prev j) (if (= ca (nth b j)) 0 1)))))
                            [(inc (first prev))] (range n)))
                  (vec (range (inc n))) a))))

(defn- tokens [s] (remove str/blank? (str/split (str/lower-case (str s)) #"[^\p{L}\p{N}]+")))

(defn rouge-1
  [actual expected]
  (let [fa (frequencies (tokens actual)) fe (frequencies (tokens expected))
        hit (reduce + 0 (for [[t n] fe] (min n (get fa t 0))))
        na (reduce + 0 (vals fa)) ne (reduce + 0 (vals fe))
        p (if (pos? na) (double (/ hit na)) 0.0)
        r (if (pos? ne) (double (/ hit ne)) 0.0)]
    {:precision p :recall r :f-measure (if (pos? (+ p r)) (/ (* 2 p r) (+ p r)) 0.0)}))

(defn- subset? [want got]
  (cond (map? want) (and (map? got)
                         (every? (fn [[k v]] (subset? v (get got (keyword (name k))))) want))
        (sequential? want) (and (sequential? got) (every? (fn [w] (some #(subset? w %) got)) want))
        :else (same-value? got want)))

(def catalog
  {:exact-match      {:field? true :metric :boolean
                      :about "actual = expected; numbers, dates and timestamps coerced
                              (:ignore-case, :ignore-whitespace for strings)"}
   :array-match      {:field? true :metric :boolean
                      :about "two lists hold the same elements (:ordered? default true)"}
   :regex            {:field? true :metric :boolean
                      :about "the actual matches the regular expression :pattern (or the
                              expected value); :partial? true finds it anywhere"}
   :levenshtein      {:field? true :metric :numeric
                      :about "edits between actual and expected — set :objective {:max n}"}
   :string-length    {:field? true :metric :boolean :expected? false
                      :about "the actual's length lies in :min..:max"}
   :keywords         {:field? true :metric :boolean
                      :about "every one of :keywords (or the expected list) is in the actual"}
   :contains         {:field? true :metric :boolean
                      :about "the actual text contains every expected phrase (case and
                              whitespace folded)"}
   :numeric-range    {:field? true :metric :boolean
                      :about "the actual number lies in :min..:max, or within :tolerance
                              (absolute) / :tolerance-pct of the expected"}
   :temporal-range   {:field? true :metric :boolean
                      :about "the actual date lies in :min..:max, or within :tolerance-days
                              of the expected"}
   :set-contains     {:field? true :metric :boolean
                      :about "the actual is one of the expected values"}
   :set-size         {:field? true :metric :boolean :expected? false
                      :about "the actual collection's size lies in :min..:max"}
   :rouge            {:field? true :metric :numeric
                      :about "ROUGE-1 f-measure of actual against expected text (precision and
                              recall in debug)"}
   :llm-judge        {:field? true :metric :numeric :io? true
                      :about "a model scores the actual 0..1 against :rubric and the expected
                              (default objective {:min 0.5})"}
   :tool-use         {:field? true :metric :boolean
                      :about "an agent used every expected tool"}
   :proposed-actions {:field? true :metric :boolean
                      :about "an agent proposed every expected action with those args"}
   :edits            {:field? true :metric :boolean
                      :about "an action's planned edits contain every expected edit (a
                              partial map each, compared as a subset)"}
   :function         {:needs [:function] :metric :any :io? true
                      :about "a query Function judges: called with {:actual :expected :input},
                              its first row's :value-field (else its first boolean or number)
                              is the metric, its strings the debug output"}
   :precision        {:field? true :needs [:class] :metric :boolean :aggregate :confusion
                      :about "of the cases predicted :class, the share truly :class"}
   :recall           {:field? true :needs [:class] :metric :boolean :aggregate :confusion
                      :about "of the cases truly :class, the share predicted :class"}
   :f1               {:field? true :needs [:class] :metric :boolean :aggregate :confusion
                      :about "harmonic mean of precision and recall on :class"}
   :abstain          {:metric :boolean
                      :about "on the cases whose expected says {:abstain true}, the target
                              returned nothing"}})

(defn kind-of [ev] (some-> (:kind ev) name keyword))

(defn expected-field [ev] (or (:expected ev) (:field ev)))

(def ^:dynamic *judge*
  nil)

(def ^:dynamic *call-function*
  nil)

(def ^:dynamic *map-io-cases*
  mapv)

(defmulti judge
  (fn [ev _case] (kind-of ev)))

(defn- want [ev e] (fval e (expected-field ev)))

(defn- got [ev a] (if (:field ev) (fval a (:field ev)) a))

(defmethod judge :exact-match [ev {:keys [actual expected]}]
  (let [w (want ev expected)]
    (when (some? w) {:value (text-match? ev (got ev actual) w)})))

(defmethod judge :array-match [ev {:keys [actual expected]}]
  (let [w (want ev expected)]
    (when (some? w)
      (let [a (as-coll (got ev actual)) w (as-coll w)]
        {:value (if (false? (:ordered? ev))
                  (and (= (count a) (count w))
                       (= (frequencies (map squash a)) (frequencies (map squash w))))
                  (same-value? (vec a) (vec w)))}))))

(defmethod judge :regex [ev {:keys [actual expected]}]
  (when-let [p (or (:pattern ev) (want ev expected))]
    (let [a (str (got ev actual)) re (re-pattern (str p))]
      {:value (boolean (if (:partial? ev) (re-find re a) (re-matches re a)))})))

(defmethod judge :levenshtein [ev {:keys [actual expected]}]
  (let [w (want ev expected)]
    (when (some? w)
      (let [a (got ev actual)]
        {:value (levenshtein (if (nil? a) "" (str a)) (str w))}))))

(defn- within? [x lo hi] (and (some? x) (or (nil? lo) (>= x lo)) (or (nil? hi) (<= x hi))))

(defmethod judge :string-length [ev {:keys [actual]}]
  {:value (within? (count (str (got ev actual))) (:min ev) (:max ev))})

(defmethod judge :keywords [ev {:keys [actual expected]}]
  (let [ks (as-coll (or (:keywords ev) (want ev expected)))]
    (when (seq ks)
      (let [a (squash (got ev actual))]
        {:value (every? #(str/includes? a (squash %)) ks)}))))

(defmethod judge :contains [ev {:keys [actual expected]}]
  (let [w (want ev expected)]
    (when (some? w)
      (let [a (squash (got ev actual))]
        {:value (every? #(str/includes? a (squash %)) (as-coll w))}))))

(defn- num-bounds [ev w]
  (let [e (some-> (as-decimal w) double)]
    (cond (or (some? (:min ev)) (some? (:max ev))) [(:min ev) (:max ev)]
          (and e (:tolerance ev))     [(- e (:tolerance ev)) (+ e (:tolerance ev))]
          (and e (:tolerance-pct ev)) (let [d (Math/abs (* e (/ (:tolerance-pct ev) 100.0)))]
                                        [(- e d) (+ e d)])
          e [e e])))

(defmethod judge :numeric-range [ev {:keys [actual expected]}]
  (when-let [[lo hi] (num-bounds ev (want ev expected))]
    (let [a (some-> (as-decimal (got ev actual)) double)]
      {:value (within? a lo hi)
       :debug (when-not (within? a lo hi) (str "expected " lo ".." hi ", got " a))})))

(defn- date-bounds [ev w]
  (let [e (as-date w) d (:tolerance-days ev)]
    (cond (or (:min ev) (:max ev)) [(some-> (:min ev) as-date) (some-> (:max ev) as-date)]
          (and e d) [(.minusDays e (long d)) (.plusDays e (long d))]
          e [e e])))

(defmethod judge :temporal-range [ev {:keys [actual expected]}]
  (when-let [[lo hi] (date-bounds ev (want ev expected))]
    (let [a (as-date (got ev actual))]
      {:value (boolean (and a (or (nil? lo) (not (.isBefore a lo)))
                            (or (nil? hi) (not (.isAfter a hi)))))
       :debug (when (and a lo)
                (str (.between ChronoUnit/DAYS lo a) " day(s) from the lower bound"))})))

(defmethod judge :set-contains [ev {:keys [actual expected]}]
  (let [w (want ev expected)]
    (when (some? w)
      (let [a (got ev actual)] {:value (boolean (some #(same-value? a %) (as-coll w)))}))))

(defmethod judge :set-size [ev {:keys [actual]}]
  {:value (within? (count (as-coll (got ev actual))) (:min ev) (:max ev))})

(defmethod judge :rouge [ev {:keys [actual expected]}]
  (let [w (want ev expected)]
    (when (some? w)
      (let [r (rouge-1 (got ev actual) w)]
        {:value (:f-measure r) :debug (select-keys r [:precision :recall])}))))

(defmethod judge :llm-judge [ev {:keys [actual expected index]}]
  (when-not *judge*
    (refusal/refuse! :refusal.evals/llm-judge-no-model {:evaluator (:name ev)}))
  (let [w (want ev expected)]
    (when (or (some? w) (:rubric ev))
      (let [{:keys [score reason]} (*judge* ev index (got ev actual) w)]
        {:value score :debug reason}))))

(defmethod judge :tool-use [ev {:keys [actual expected]}]
  (let [w (want ev expected)]
    (when (some? w)
      (let [used (set (map str (got ev actual)))]
        {:value (every? #(contains? used (str %)) (as-coll w))}))))

(defn- proposal-matches? [want got]
  (and (= (str (:action want)) (str (:action got)))
       (every? (fn [[k v]] (= (str v) (str (get (:args got) (keyword (name k)))))) (:args want))))

(defmethod judge :proposed-actions [ev {:keys [actual expected]}]
  (let [w (want ev expected)]
    (when (some? w)
      {:value (every? (fn [x] (some #(proposal-matches? x %) (got ev actual))) (as-coll w))})))

(defmethod judge :edits [ev {:keys [actual expected]}]
  (let [w (want ev expected)]
    (when (some? w)
      (let [plan (as-coll (got ev actual))
            miss (remove (fn [x] (some #(subset? x %) plan)) (as-coll w))]
        {:value (empty? miss)
         :debug (when (seq miss) (str "not planned: " (pr-str (vec miss))))}))))

(defmethod judge :function [ev {:keys [actual expected input]}]
  (when-not *call-function*
    (refusal/refuse! :refusal.evals/function-evaluator-local {:evaluator (:name ev)}))
  (let [row (first (*call-function* (keyword (name (:function ev)))
                                    {:actual actual :expected expected :input input}))
        vf  (:value-field ev)
        v   (if vf
              (fval row vf)
              (some (fn [[_ x]] (when (or (boolean? x) (number? x)) x)) (sort-by key row)))]
    (when (some? v)
      {:value v
       :debug (not-empty (into {} (filter (fn [[_ x]] (string? x))) row))})))

(defmethod judge :precision [ev c] (judge (assoc ev :kind :recall) c))
(defmethod judge :f1 [ev c] (judge (assoc ev :kind :recall) c))
(defmethod judge :recall [ev {:keys [actual expected]}]
  (let [cls (:class ev)]
    {:value (= (= (got ev actual) cls) (= (want ev expected) cls))}))

(defmethod judge :abstain [_ev {:keys [actual expected]}]
  (when (:abstain expected) {:value (nil? actual)}))

(defmethod judge :default [ev _]
  (refusal/refuse! :refusal.evals/unknown-evaluator-kind
                   {:kind (:kind ev) :evaluator (:name ev)}
                   {:kinds (str/join ", " (map name (sort (keys catalog))))}))

(defn default-objective [ev v]
  (cond (boolean? v)                     {:expect true}
        (= :llm-judge (kind-of ev))      {:min 0.5}))

(defn passes?
  [ev v]
  (let [{:keys [expect min max] :as o} (or (:objective ev) (default-objective ev v))]
    (cond (nil? v)                  false
          (nil? o)                  nil
          (contains? o :expect)     (= (boolean v) (boolean expect))
          (number? v)               (within? v min max)
          :else                     (boolean v))))

(defn- rate [num den] (when (pos? den) (double (/ num den))))

(defn- numeric [v] (cond (boolean? v) (if v 1.0 0.0) (number? v) (double v)))

(defn- confusion [ev as es]
  (let [cls (:class ev)]
    (reduce (fn [acc [a e]]
              (let [p? (= (got ev a) cls) t? (= (want ev e) cls)]
                (cond-> acc
                  (and p? t?)       (update :tp inc)
                  (and p? (not t?)) (update :fp inc)
                  (and (not p?) t?) (update :fn inc))))
            {:tp 0 :fp 0 :fn 0} (map vector as es))))

(defn- confusion-value [kind {:keys [tp fp fn]}]
  (let [p (or (rate tp (+ tp fp)) 0.0) r (or (rate tp (+ tp fn)) 0.0)]
    (case kind
      :precision p
      :recall    r
      :f1        (if (zero? (+ p r)) 0.0 (/ (* 2 p r) (+ p r))))))

(defn score
  ([ev as es] (score ev as es nil))
  ([ev as es ins]
   (let [kind   (kind-of ev)
         each   (if (get-in catalog [kind :io?]) *map-io-cases* mapv)
         per    (vec (each (fn [[i a e]]
                             (when-let [{:keys [value debug]}
                                        (judge ev {:actual a :expected e :index i
                                                   :input (nth ins i nil)})]
                               (cond-> {:value value :pass? (passes? ev value)}
                                 (some? debug) (assoc :debug debug))))
                           (map vector (range) as es)))
         graded (keep-indexed (fn [i p] (when p [i p])) per)
         misses (vec (keep (fn [[i p]] (when (false? (:pass? p)) i)) graded))
         n      (count graded)]
     (case (get-in catalog [kind :aggregate])
       :confusion
       (let [c (confusion ev as es)]
         {:value (confusion-value kind c) :detail (assoc c :class (:class ev)) :per-case per})
       (if (= :abstain kind)
         {:value  (if (zero? n) 1.0 (rate (- n (count misses)) n))
          :detail {:abstained (- n (count misses)) :n n :leaked misses}
          :per-case per}
         (let [vals (keep (comp numeric :value second) graded)
               bool (every? (comp boolean? :value second) graded)]
           {:value    (when (seq vals) (/ (reduce + vals) (count vals)))
            :detail   (cond
                        bool {:hits (- n (count misses)) :n n :misses misses}
                        (= :llm-judge kind)
                        {:n n :misses misses
                         :scores (vec (for [[i p] graded]
                                        {:case i :score (:value p) :reason (:debug p)}))}
                        :else {:n n :misses misses})
            :per-case per}))))))
