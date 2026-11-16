(ns ^:unit apps.capp-scenario-compare-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.capps.ctx :as cctx]
            [sutra.capps.table :as tbl]))

(def ^:private de (cctx/of {:locale :de}))

(defn- mech [nr name std]
  {:id (str "id-" nr) :values {:lizenz nr :name name :schichtstunden std}})

(def ^:private base [(mech "1" "Ansgar" 40) (mech "2" "Britt" 40) (mech "3" "Corvin" 20)])

(def ^:private szen-a
  [(mech "1" "Ansgar" 40) (mech "2" "Britt" 20) (mech "3" "Corvin" 20) (mech "9" "Neuzugang" 40)])
(def ^:private szen-b [(mech "1" "Ansgar" 40) (mech "2" "Britt" 40)])

(def ^:private key-fn (tbl/field-fn :lizenz))
(def ^:private segs ["a" "b"])
(def ^:private joined
  (tbl/compare-rows base {"a" szen-a "b" szen-b} segs key-fn))

(deftest rows-join-on-the-business-key-not-the-object-id
  (testing "every base row appears once, keyed, with its twin from each scenario"
    (is (= ["1" "2" "3" "9"] (mapv :key joined))
        "base order first, then rows that exist ONLY in a scenario — an appended creation")
    (is (= "Britt" (get-in (:base (second joined)) [:values :name])))
    (is (= 20 (get-in (get (:by (second joined)) "a") [:values :schichtstunden]))
        "the scenario's own value, not the base's"))
  (testing "a row missing on one side is nil there, not absent from the table"
    (let [cosmo (nth joined 2)
          neu (nth joined 3)]
      (is (some? (:base cosmo)))
      (is (nil? (get (:by cosmo) "b")) "Corvin fehlt im Schichtplan B")
      (is (nil? (:base neu)) "der Neuzugang hat keine Basis-Zeile")
      (is (some? (get (:by neu) "a"))))))

(deftest only-columns-with-modifications-are-flagged
  (let [name-col {:label "Name" :prop :name}
        std-col  {:label "Schichtstd." :prop :schichtstunden}]
    (testing "an unchanged column reports no modification for that scenario"
      (let [stable (tbl/compare-rows [(mech "1" "Ansgar" 40)] {"a" [(mech "1" "Ansgar" 40)]} ["a"]
                                     key-fn)]
        (is (false? (tbl/compare-changed? name-col stable "a" de)))
        (is (false? (tbl/compare-changed? std-col stable "a" de)))))
    (testing "a changed value flags the column"
      (let [rows (tbl/compare-rows [(mech "2" "Britt" 40)] {"a" [(mech "2" "Britt" 20)]} ["a"]
                                   key-fn)]
        (is (true? (tbl/compare-changed? std-col rows "a" de)))
        (is (false? (tbl/compare-changed? name-col rows "a" de))
            "the name did not move — its column must not widen")))
    (testing "a created or deleted row IS a modification"
      (let [created (tbl/compare-rows [] {"a" [(mech "9" "Neuzugang" 40)]} ["a"] key-fn)
            deleted (tbl/compare-rows [(mech "3" "Corvin" 20)] {"a" []} ["a"] key-fn)]
        (is (true? (tbl/compare-changed? name-col created "a" de)))
        (is (true? (tbl/compare-changed? name-col deleted "a" de)))))
    (testing "a difference the FORMAT hides is not a modification"
      (let [eur (fn [nr v] {:id nr :values {:k nr :betrag v}})
            rows (tbl/compare-rows [(eur "1" 100.004)] {"a" [(eur "1" 100.006)]} ["a"]
                                   (tbl/field-fn :k))]
        (is (false? (tbl/compare-changed? {:label "€" :prop :betrag :fmt :eur0} rows "a" de)))
        (is (true?  (tbl/compare-changed? {:label "€" :prop :betrag :fmt :eur} rows "a" de))
            "at two decimals the same pair IS visibly different")))))

(deftest sorting-orders-join-rows-by-their-base
  (testing ":sort ranks on the base row; scenario-only rows fall to the end"
    (let [sorted (tbl/compare-sort joined [[:name :desc]])]
      (is (= ["3" "2" "1" "9"] (mapv :key sorted))
          "Corvin, Britt, Ansgar — dann der Neuzugang ohne Basis")))
  (testing "no :sort leaves the join order untouched"
    (is (= (mapv :key joined) (mapv :key (tbl/compare-sort joined nil))))))

(deftest chart-tagging-turns-scenarios-into-one-series-each
  (testing "every branch's rows land in ONE set, tagged with their series label"
    (let [tagged (tbl/compare-tag-rows base {"a" szen-a} {"a" "Szenario A"} :szenario "Basis")]
      (is (= (+ (count base) (count szen-a)) (count tagged)))
      (is (= #{"Basis" "Szenario A"} (set (map #(get-in % [:values :szenario]) tagged)))
          "the tag is where field-fn looks — inside :values for a payload object")
      (is (= 3 (count (filter #(= "Basis" (get-in % [:values :szenario])) tagged))))))
  (testing "an aggregate row (no :values) is tagged on the row itself"
    (let [agg [{:monat 1 :betrag 10}]
          tagged (tbl/compare-tag-rows agg {"a" [{:monat 1 :betrag 12}]} {"a" "A"} :szenario
                                       "Basis")]
      (is (= ["Basis" "A"] (mapv :szenario tagged))))))
