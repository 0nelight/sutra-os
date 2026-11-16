(ns sutra.types.llm-cost
  (:require [sutra.kit.coerce :refer [->num ->num0]]
            [clojure.string :as str]
            [sutra.types.rid :as rid]))

(def default-currency
  "USD")

(def ^:const per-tokens
  1000000.0)

(defn- effective-from
  [row]
  (str (or (:effective-from row) (:effective_from row))))

(defn rate-at
  [rate-rows model ts]
  (let [ts (str ts)]
    (when-let [row (->> rate-rows
                        (filter #(= (str (:model %)) (str model)))
                        (filter #(<= (compare (effective-from %) ts) 0))
                        (sort-by effective-from)
                        last)]
      {:in       (->num (:rate-in row))
       :out      (->num (:rate-out row))
       :currency (or (some-> (:currency row) not-empty) default-currency)})))

(defn call-cost
  [rate prompt-tokens completion-tokens]
  (let [{:keys [in out]} rate]
    (when (and in out)
      (/ (+ (* (->num0 prompt-tokens) in)
            (* (->num0 completion-tokens) out))
         per-tokens))))

(defn price-call
  [rate-rows {:keys [model ts] :as call}]
  (let [rate (rate-at rate-rows model ts)]
    (assoc call
           :cost     (call-cost rate (:prompt-tokens call) (:completion-tokens call))
           :rate-in  (:in rate)
           :rate-out (:out rate)
           :currency (:currency rate))))

(defn price-calls
  [rate-rows call-rows]
  (mapv #(price-call rate-rows %) call-rows))

(defn row-id
  [model effective-from in out currency]
  (let [s (str model "|" effective-from "|" in "|" out "|" (or currency default-currency))]
    #?(:clj  (str (java.util.UUID/nameUUIDFromBytes (.getBytes ^String s "UTF-8")))
       :cljs (str (hash s)))))

(defn iso-day
  [ts]
  (let [s (str ts)] (if (>= (count s) 10) (subs s 0 10) s)))

(defn within-day-window?
  [since until ts]
  (let [d (iso-day ts)
        s (not-empty (str (or since "")))
        u (not-empty (str (or until "")))]
    (and (or (nil? s) (>= (compare d s) 0))
         (or (nil? u) (<= (compare d u) 0)))))

(defn caller-key
  [caller]
  (let [c (str caller)]
    (cond
      (str/blank? (str/trim c)) nil
      (rid/rid-locator c)       (rid/rid-locator c)
      :else                     (-> c str/trim str/lower-case (str/replace "_" "-")))))

(defn caller-keys
  [rid t-name]
  (into #{} (keep caller-key) [rid t-name]))

(defn resolve-resource
  [index caller]
  (let [x (or (some->> (caller-key caller) (get index)) caller)]
    (if (or (keyword? x) (and (string? x) (seq x))) (name x) (str x))))

(def ^:private rate-limit-markers
  ["budget reached" "http 429" "rate limit" "rate_limit" "too many requests"])

(defn rate-limited?
  [row]
  (let [e (str/lower-case (str (:error row)))]
    (boolean (and (seq e) (some #(str/includes? e %) rate-limit-markers)))))

(defn usage-rollup
  [key-fn rows]
  (into {}
        (for [[g rs] (group-by key-fn rows)
              :when (some? g)
              :let [refused (filterv rate-limited? rs)
                    errored (filterv #(and (seq (str (:error %)))
                                           (not (rate-limited? %))) rs)
                    ok      (filterv #(empty? (str (:error %))) rs)
                    pt      (long (reduce + 0.0 (map #(->num0 (:prompt-tokens %)) ok)))
                    ct      (long (reduce + 0.0 (map #(->num0 (:completion-tokens %)) ok)))
                    priced  (keep :cost ok)]]
          [g {:attempted         (count rs)
              :successful        (count ok)
              :rate-limited      (count refused)
              :failed            (count errored)
              :prompt-tokens     pt
              :completion-tokens ct
              :tokens            (+ pt ct)
              :cost              (when (seq priced) (reduce + 0.0 priced))
              :currency          (or (some #(some-> (:currency %) not-empty) ok)
                                     default-currency)
              :models            (vec (sort (distinct (keep #(some-> (:model %) str not-empty)
                                                            rs))))}])))

(defn rollup
  [out-key key-fn priced]
  (->> priced
       (group-by #(or (key-fn %) "?"))
       (map (fn [[g rs]]
              (let [priced-cs (keep :cost rs)]
                {out-key g
                 :calls    (count rs)
                 :cost     (when (seq priced-cs) (reduce + 0.0 priced-cs))
                 :unpriced (count (remove :cost rs))
                 :tokens   (long (reduce + 0.0 (map #(+ (->num0 (:prompt-tokens %))
                                                        (->num0 (:completion-tokens %))) rs)))})))
       (sort-by (juxt #(or (:cost %) -1.0) :tokens) #(compare %2 %1))))
