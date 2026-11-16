(ns sutra.types.unit-tests
  (:require [clojure.string :as str]
            [sutra.types.expectations :as xp]
            [sutra.types.languages :as langs]
            [sutra.types.transform-io :as tio]))

(set! *warn-on-reflection* true)

(def runnable-languages (set langs/compute-language-names))

(defn- norm
  [v]
  (cond
    (or (nil? v) (= "" v))                 nil
    (number? v)                            (.stripTrailingZeros (bigdec v))
    (keyword? v)                           (name v)
    (xp/temporal? v)                       (str (xp/->instant v))
    (sequential? v)                        (mapv norm v)
    :else                                  v))

(defn- project [cols row] (into {} (map (fn [c] [c (norm (get row c))])) cols))

(defn- show [rows] (str/join " " (map pr-str (take 5 rows))))

(defn compare-rows
  [expect got]
  (let [expect (xp/canonical-rows expect)
        got    (xp/canonical-rows got)
        cols   (vec (distinct (mapcat keys expect)))
        want   (frequencies (map #(project cols %) expect))
        have   (frequencies (map #(project cols %) got))
        diff   (fn [a b] (vec (mapcat (fn [[r n]] (repeat (- n (get b r 0)) r))
                                      (filter (fn [[r n]] (> n (get b r 0))) a))))
        miss   (diff want have)
        extra  (diff have want)
        ok?    (if (empty? expect) (empty? got) (and (empty? miss) (empty? extra)))]
    {:passed?    ok?
     :missing    (vec (take 20 miss))
     :unexpected (vec (take 20 (if (empty? expect) (map #(project (keys %) %) got) extra)))
     :detail     (cond
                   ok? (str (count got) " row(s) as expected")
                   (empty? expect) (str "expected no rows, got " (count got) ": " (show got))
                   :else (str/join "; " (cond-> []
                                          (seq miss)  (conj (str (count miss) " expected row(s)"
                                                                 " missing: " (show miss)))
                                          (seq extra) (conj (str (count extra) " row(s) not"
                                                                 " expected: " (show extra))))))}))

(defn- test-problems
  [t aliases outs {:keys [name inputs expect output] :as ut}]
  (let [nm (or name "a test")]
    (cond
      (not (map? ut)) [(str "a unit test is {:name :inputs :expect}, not " (pr-str ut))]
      (str/blank? (str name)) ["a unit test needs a :name"]
      :else
      (concat
       (when-not (runnable-languages (keyword (:language t)))
         [(str nm ": a unit test runs a SQL, :python or :clojure transform")])
       (when-not (or (nil? inputs) (and (map? inputs) (every? sequential? (vals inputs))))
         [(str nm ": :inputs is {alias [rows]}")])
       (for [a (keys inputs) :when (not (contains? aliases (xp/col-key a)))]
         (str nm ": " (pr-str a) " is no input of this transform — one of "
              (str/join ", " (map clojure.core/name (sort aliases)))))
       (when-not (and (sequential? expect) (every? map? expect))
         [(str nm ": :expect is the vector of rows the output must be")])
       (when (and output (not (contains? outs (keyword (clojure.core/name output)))))
         [(str nm ": " (pr-str output) " is no output of this transform")])))))

(defn transform-problems
  [t]
  (when-let [uts (seq (:tests t))]
    (let [aliases (into #{} (map xp/col-key) (keys (when (map? (:inputs t)) (:inputs t))))
          outs    (into #{} (keep #(some-> (:name %) name keyword))
                        (tio/outputs-of t))
          dup     (keep (fn [[n k]] (when (> k 1) n)) (frequencies (keep :name uts)))]
      (not-empty
       (vec (concat (map #(str "unit test name " (pr-str %) " is used twice") dup)
                    (mapcat #(test-problems t aliases outs %) uts)))))))
