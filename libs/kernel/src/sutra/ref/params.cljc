(ns sutra.ref.params
  (:require [clojure.string :as str]
            [sutra.ref.term :as term]))

(defn supplied? [x]
  (cond (nil? x) false
        (string? x) (not (str/blank? x))
        (or (sequential? x) (set? x) (map? x)) (boolean (seq x))
        :else true))

(defn- blank-text? [x] (and (string? x) (str/blank? x)))

(defn absent-blanks
  [specs params]
  (if-not (map? params)
    params
    (reduce-kv (fn [acc p spec]
                 (let [x (get acc p)]
                   (cond
                     (contains? #{nil :string} (:type spec)) acc
                     (blank-text? x) (assoc acc p nil)
                     (and (sequential? x)
                          (some blank-text? x)) (assoc acc p (vec (remove blank-text? x)))
                     :else acc)))
               params specs)))

(defn- given? [spec x]
  (if (true? (:multiple spec)) (some? x) (supplied? x)))

(defn missing-required
  [specs params]
  (->> specs
       (filter (fn [[p spec]] (and (true? (:required spec)) (not (given? spec (get params p))))))
       (map key) (sort-by name) vec))

(def implicit-context-params #{:regel-jetzt :regel-heute :regel-jetzt-text :regel-benutzer})

(defn implicit-context-param? [p]
  (contains? implicit-context-params (keyword p)))

(defn day-text
  [zone x]
  (cond
    (nil? x) nil
    (string? x) (when-let [[_ y m d] (re-find #"^(\d{4})-(\d{2})-(\d{2})" (str/trim x))]
                  (str y "-" m "-" d))
    (integer? x) (day-text zone #?(:clj  (java.time.Instant/ofEpochMilli (long x))
                                   :cljs (js/Date. x)))
    :else #?(:clj  (condp instance? x
                     java.time.LocalDate (str x)
                     java.sql.Date (str (.toLocalDate ^java.sql.Date x))
                     java.time.LocalDateTime (str (.toLocalDate ^java.time.LocalDateTime x))
                     java.time.OffsetDateTime
                     (str (.toLocalDate (.atZone (.toInstant ^java.time.OffsetDateTime x) zone)))
                     java.time.ZonedDateTime
                     (str (.toLocalDate (.atZone (.toInstant ^java.time.ZonedDateTime x) zone)))
                     java.util.Date
                     (str (.toLocalDate (.atZone (.toInstant ^java.util.Date x) zone)))
                     java.time.Instant (str (.toLocalDate (.atZone ^java.time.Instant x zone)))
                     nil)
             :cljs (when (instance? js/Date x)
                     (let [p #(if (< % 10) (str "0" %) (str %))]
                       (str (.getFullYear x) "-" (p (inc (.getMonth x))) "-" (p (.getDate x))))))))

(defn normalise-dates
  [zone specs params]
  (reduce-kv (fn [acc p spec]
               (if (supplied? (get acc p))
                 (update acc p #(term/update-at (term/of spec) #{:date} (partial day-text zone) %))
                 acc))
             params specs))

(def integral-types (term/types-of :integral))
(def numeric-types (term/types-of :integral :fractional))

(defn- integral-value? [x]
  (and (number? x)
       (or (integer? x)
           (let [d (double x)] (and #?(:clj (not (Double/isInfinite d)) :cljs (js/isFinite d))
                                    (== d (Math/floor d)))))))

(defn- fits-term?
  [zone t x]
  (case (first t)
    :scalar        (case (second t)
                     :integral   (integral-value? x)
                     :fractional (number? x)
                     :boolean    (boolean? x)
                     :date       (some? (day-text zone x))
                     (:text :uuid :instant :opaque) true)
    :decimal       (number? x)
    :vector        (and (sequential? x) (every? number? x))
    (:list :array) (if (sequential? x)
                     (every? #(fits-term? zone (second t) %) x)
                     (fits-term? zone (second t) x))
    :struct        (and (map? x)
                        (every? (fn [[k y]]
                                  (or (nil? y)
                                      (fits-term? zone (get (second t) k [:named nil]) y)))
                                x))
    (:ref :object-set :named) true))

(defn fits? [zone spec x]
  (fits-term? zone (term/of spec) x))

(defn type-problems [zone specs params]
  (->> specs
       (keep (fn [[p spec]]
               (let [x (get params p)]
                 (when (and (supplied? x) (not (fits? zone spec x)))
                   {:parameter p :expected (:type spec) :value x}))))
       (sort-by (comp name :parameter)) vec))

(defn- base-type [t]
  (let [b (if (sequential? t) (first t) t)]
    (when (or (keyword? b) (string? b)) (keyword (name b)))))

(defn number-kind
  [spec write-types]
  (let [own (base-type (:type spec))
        of  (fn [t] (cond (integral-types t) :integral
                          (= :decimal t) :decimal
                          (#{:double :float :decimal} t) :double))]
    (cond
      (not (numeric-types own)) nil
      (not= :number own) (of own)
      :else (let [ks (into #{} (keep (comp of base-type)) write-types)]
              (cond (ks :integral) :integral (ks :decimal) :decimal (ks :double) :double)))))

(def unreadable ::unreadable)

(defn decode-text
  [t s]
  (if-not (string? s)
    s
    (let [{:keys [value problems]} (term/parse (term/base (if (sequential? t) (first t) t)) s)]
      (if (seq problems) unreadable value))))

(defn coerce-number [kind x]
  (cond
    (not (number? x)) x
    (= :integral kind) (if (integral-value? x) (long x) ::unreadable)
    (= :double kind) (double x)
    (= :decimal kind) #?(:clj (bigdec x) :cljs x)
    :else x))

(defn- blank-val? [x] (or (nil? x) (and (string? x) (str/blank? x))))

(defn- int-of [c] #?(:clj (int c) :cljs (.charCodeAt (str c) 0)))

(defn- ean-ok? [s]
  (and (re-matches #"\d+" s)
       (contains? #{8 12 13 14} (count s))
       (let [ds (mapv #(- (int-of %) 48) s)
             n  (count ds)]
         (zero? (mod (reduce + (map-indexed (fn [i d] (* d (if (even? (- n 1 i)) 1 3))) ds)) 10)))))

(defn- iban-ok? [s]
  (let [s (str/upper-case (str/replace s #"\s" ""))]
    (and (re-matches #"[A-Z]{2}\d{2}[0-9A-Z]{11,30}" s)
         (let [r (str (subs s 4) (subs s 0 4))
               n (apply str
                        (map (fn [c]
                               (if (re-matches #"[A-Z]" (str c)) (str (- (int-of c) 55)) (str c)))
                             r))]
           (= 1 (reduce (fn [m d] (mod (+ (* m 10) (- (int-of d) 48)) 97)) 0 n))))))

(def ^:private formats
  {:email #(re-matches #"(?i)[^@\s]+@[^@\s]+\.[^@\s]+" %)
   :uuid  #(re-matches #"(?i)[0-9a-f]{8}-([0-9a-f]{4}-){3}[0-9a-f]{12}" %)
   :ean   ean-ok?
   :iban  iban-ok?})

(def ^:private pattern-of (memoize re-pattern))

(def ^:private one-of-set
  (memoize (fn [one-of] (into #{} (map #(if (map? %) (:value %) %)) one-of))))

(defn- format-ok? [fmt x]
  (if-let [p (formats fmt)] (or (not (string? x)) (boolean (p x))) true))

(defn violations
  [attrs values complete?]
  (reduce
   (fn [acc {:keys [attribute-name opts]}]
     (let [{:keys [required constraints]} opts
           {:keys [one-of min-length max-length regex format min max]} constraints
           present? (contains? values attribute-name)
           x        (get values attribute-name)
           check?   (and present? (some? x))
           viol     (fn [c & kvs]
                      (into {:attribute attribute-name :constraint c} (apply hash-map kvs)))]
       (cond-> acc
         (and required (if present? (blank-val? x) complete?)) (conj (viol :required))
         (and (seq one-of) check?
              (not (contains? (one-of-set one-of) x)))
         (conj (viol :one-of :value x :allowed (vec one-of)))
         (and min-length check? (string? x) (< (count x) min-length)) (conj (viol :min-length))
         (and max-length check? (string? x) (> (count x) max-length)) (conj (viol :max-length))
         (and regex check? (string? x) (not (re-matches (pattern-of regex) x))) (conj (viol :regex))
         (and format check? (not (format-ok? format x))) (conj (viol :format))
         (and (some? min) check? (number? x) (< x min)) (conj (viol :min))
         (and (some? max) check? (number? x) (> x max)) (conj (viol :max)))))
   [] attrs))

(defn param-constraint-problems [specs params]
  (->> specs
       (mapcat (fn [[p spec]]
                 (when-let [cs (not-empty (:constraints spec))]
                   (let [x  (get params p)
                         xs (cond (sequential? x) x (nil? x) [] :else [x])]
                     (for [e xs
                           :when (and (supplied? e) (not (map? e)))
                           viol (violations [{:attribute-name p :opts {:constraints cs}}] {p e}
                                            false)]
                       (assoc viol :parameter p :value e))))))
       (sort-by (comp name :parameter)) vec))

(defn with-section-overrides
  [action]
  (let [lowered (for [sec (get-in action [:form :sections])
                      o   (:overrides sec)
                      :when (= :hidden (some-> (:exposure o) name keyword))
                      p   (map keyword (:params sec))]
                  [p (cond-> {:when (:when o) :exposure :hidden}
                       (true? (get-in action [:params p :required])) (assoc :required false))])]
    (reduce (fn [a [p o]]
              (if (and (contains? (:params a) p)
                       (not (some #{o} (get-in a [:params p :overrides]))))
                (update-in a [:params p :overrides] #(into [o] %))
                a))
            action
            (reverse lowered))))
