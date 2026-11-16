(ns ^:unit ontology.timeseries-vocabulary-test
  (:require [sutra.kit.anomaly :as anomaly]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [sutra.store.access.timeseries :as ats]
            [sutra.types.aggregate :as agg]
            [sutra.types.sql-ident :as sqli]
            [sutra.types.timeseries :as ts]))

(def ^:private tsp
  {:attribute-name :produktivstunden
   :attribute-type :timeseries
   :opts {:series {:sync :zeit_serien :rollup :sum :unit "h"}}})

(def ^:private tsp-no-rollup
  {:attribute-name :saldo
   :attribute-type :timeseries
   :opts {:series {:sync :bank_serien}}})

(def ^:private plain
  {:attribute-name :bezeichnung :attribute-type :string})

(def ^:private sync-cols
  [{:attribute-name :series_id :attribute-type :string}
   {:attribute-name :ts        :attribute-type :date}
   {:attribute-name :value     :attribute-type :decimal}
   {:attribute-name :extra     :attribute-type :string}])

(deftest declaration
  (testing "a well-formed TSP explains clean"
    (is (nil? (ts/explain-property tsp)))
    (is (ts/series-property? tsp))
    (is (not (ts/series-property? plain))))

  (testing "a :timeseries with no :series rider is refused — it holds a series ID nothing resolves"
    (let [p (ts/explain-property {:attribute-name :x :attribute-type :timeseries})]
      (is (= :missing-series-spec (:problem p)))
      (is (str/includes? (:message p) ":series"))))

  (testing "an unknown rollup names the known ones"
    (let [p (ts/explain-property (assoc-in tsp [:opts :series :rollup] :median))]
      (is (= :unknown-rollup (:problem p)))
      (is (str/includes? (:message p) "sum"))))

  (testing "an ordinary property is not a TSP declaration at all"
    (is (nil? (ts/explain-property plain)))))

(deftest sync-contract
  (testing ""
    (is (nil? (ts/explain-sync :zeit_serien sync-cols)))
    (is (= #{:series_id :ts :value} ts/sync-column-names)))

  (testing "a missing column says WHICH, and that extras are fine"
    (let [p (ts/explain-sync :zeit_serien (remove #(= :value (:attribute-name %)) sync-cols))]
      (is (= :sync-missing-columns (:problem p)))
      (is (= #{:value} (:missing p)))
      (is (str/includes? (:message p) "extra columns are fine"))))

  (testing "a timestamp column may be DATE or TIMESTAMP — a daily series must not need a cast"
    (doseq [t [:date :timestamp :instant]]
      (is (nil? (ts/explain-sync :s (assoc-in (vec sync-cols) [1 :attribute-type] t)))
          (str t " is a legal time column"))))

  (testing "a PARAMETRIC decimal value column is accepted — [:decimal 12 2] is how money is spelled"
    (is (nil? (ts/explain-sync :s (assoc-in (vec sync-cols) [2 :attribute-type] [:decimal 12 2]))))
    (is (= :decimal (ts/base-type [:decimal 12 2]))))

  (testing "…but a parametric NON-value is still refused"
    (let [p (ts/explain-sync :s (assoc-in (vec sync-cols) [2 :attribute-type] [:array :number]))]
      (is (= :sync-column-type (:problem p)))
      (is (= :value (:column p)))))

  (testing "a series id that is not a string is refused"
    (let [p (ts/explain-sync :s (assoc-in (vec sync-cols) [0 :attribute-type] :integer))]
      (is (= :sync-column-type (:problem p)))
      (is (= :series_id (:column p))))))

(deftest fetch-request
  (testing "a bucketed read with NO rollup anywhere is REFUSED, never defaulted"
    (let [p (ts/explain-fetch tsp-no-rollup {:bucket :week})]
      (is (= :rollup-required (:problem p)))
      (is (str/includes? (:message p) "no default"))))

  (testing "the property's declared rollup satisfies it; the request overrides it"
    (is (nil? (ts/explain-fetch tsp {:bucket :week})))
    (is (= :sum (:rollup (ts/normalize-fetch tsp {:bucket :week}))))
    (is (= :max (:rollup (ts/normalize-fetch tsp {:bucket :week :rollup :max})))))

  (testing "a rollup with nothing to fold is refused rather than ignored"
    (is (= :rollup-without-bucket (:problem (ts/explain-fetch tsp {:rollup :sum})))))

  (testing "a raw read carries no rollup"
    (is (nil? (:rollup (ts/normalize-fetch tsp {})))))

  (testing "the unit rides along for the chart axis"
    (is (= "h" (:unit (ts/normalize-fetch tsp {:bucket :week})))))

  (testing "an unknown bucket unit names the known ones"
    (let [p (ts/explain-fetch tsp {:bucket :fortnight})]
      (is (= :unknown-bucket-unit (:problem p)))
      (is (str/includes? (:message p) "week"))))

  (testing "an inverted window is refused — it can only ever answer empty"
    (is (= :empty-window (:problem (ts/explain-fetch tsp {:from "2026-08-01"
                                                          :until "2026-01-01"})))))

  (testing "a NON-TSP property is refused before anything reads a :series rider"
    (let [p (ts/explain-fetch plain {:bucket :week})]
      (is (= :not-a-series-property (:problem p)))
      (is (str/includes? (:message p) ":timeseries"))))

  (testing "normalize THROWS a client-error on a malformed read — one refusal, whichever store"
    (let [e (try (ts/normalize-fetch tsp-no-rollup {:bucket :week}) nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (some? e))
      (is (true? (anomaly/client-error? (ex-data e)))))))

(defn- emit
  ([attr req] (ats/compile-fetch (ts/normalize-fetch attr req) "d_zeit"))
  ([attr req dialect] (ats/compile-fetch (ts/normalize-fetch attr req) "d_zeit" dialect)))

(deftest emitter
  (testing "a raw read projects the stored columns, ordered, with bound ids"
    (let [[sql params] (emit tsp {:series-ids ["a" "b"]})]
      (is (str/includes? sql "FROM `d_zeit` s"))
      (is (str/includes? sql "s.`series_id` IN (?, ?)"))
      (is (str/includes? sql "ORDER BY `sid` ASC, `tsx` ASC"))
      (is (not (str/includes? sql "GROUP BY")) "a raw read groups nothing")
      (is (= ["a" "b"] params))))

  (testing "a bucketed read groups by (series, bucket) and folds by the rollup"
    (let [[sql _] (emit tsp {:series-ids ["a"] :bucket :week})]
      (is (str/includes? sql "SUM(s.`value`)"))
      (is (str/includes? sql (str "GROUP BY s.`series_id`, "
                                  (agg/group-sql sqli/default-dialect
                                                 {:kind :bucket :unit :week} "s.`ts`"))))))

  (testing "the window is HALF-OPEN: >= from, < until"
    (let [[sql params] (emit tsp {:series-ids ["a"] :from "2025-01-01" :until "2026-01-01"})]
      (is (str/includes? sql "s.`ts` >= ?"))
      (is (str/includes? sql "s.`ts` < ?") "an inclusive upper bound would drop the last day")
      (is (not (str/includes? sql "<=")))
      (is (= ["a" "2025-01-01" "2026-01-01"] params))))

  (testing "EVERY caller value is a bind — no request value is ever statement text"
    (let [[sql params] (emit tsp {:series-ids ["'; DROP TABLE x --"]
                                  :from "2025-01-01"})]
      (is (not (str/includes? sql "DROP")))
      (is (= 2 (count params)))))

  (testing "the aliases are letters only — a digit is renamed by the kebab builder and nils every value"
    (doseq [[_ {:keys [sql key]}] ts/aliases]
      (is (re-matches #"[a-z]+" sql) (str sql " must be lowercase letters only"))
      (is (= (keyword sql) key) "the alias and the key it answers under are one fact")))

  (testing "the bucket expression is the AGGREGATE vocabulary's, not a second spelling"
    (let [[sql _] (emit tsp {:series-ids ["a"] :bucket :month})]
      (is (str/includes? sql
                         (agg/group-sql sqli/default-dialect {:kind :bucket :unit :month}
                                        "s.`ts`")))))

  (testing "the emitter spells the bucket in postgres"
    (let [[pg _] (emit tsp {:series-ids ["a"] :bucket :week} :postgres)]
      (is (str/includes? pg "to_char"))
      (is (not (str/includes? pg "DATE_FORMAT"))))))

(defn- row [sid t v] {:sid sid :tsx t :val v})

(deftest reshape
  (testing "flat rows become one entry per series, points in read order"
    (is (= [{:series-id "a" :points [["2026-W01" 1] ["2026-W02" 2]]}
            {:series-id "b" :points [["2026-W01" 3]]}]
           (ts/->series [(row "a" "2026-W01" 1) (row "a" "2026-W02" 2)
                         (row "b" "2026-W01" 3)]))))

  (testing "series order survives MORE THAN EIGHT series — the array-map cliff"
    (let [ids  (mapv #(str "s" %) (range 12))
          rows (mapv #(row % "2026-W01" 1) ids)]
      (is (= ids (mapv :series-id (ts/->series rows)))
          "an array-map becomes a hash-map on the 9th assoc and shuffles the legend")))

  (testing "the point count is over EVERY series, and the cap is enforced on it"
    (let [series (ts/->series [(row "a" "x" 1) (row "a" "y" 2) (row "b" "z" 3)])]
      (is (= 3 (ts/point-count series)))
      (is (nil? (ts/check-point-count! 3)))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"exceeded"
                            (ts/check-point-count! (inc ts/max-points))))))

  (testing "the series-width cap points at the object set, which is what the caller meant"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"narrow the object set"
                          (ts/check-series-count! (inc ts/max-series))))))

(deftest bucket-label-parity
  (testing "every unit a series may bucket by is one the aggregate vocabulary labels"
    (doseq [[unit {:keys [example]}] agg/bucket-units]
      (is (some? example) (str unit " has a pinned example label"))
      (is (some? (ts/time-expr :postgres {:bucket unit} "s.`ts`"))))))
