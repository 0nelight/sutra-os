(ns ^:unit ontology.decimal-precision-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.parser.ent :as ent]
            [sutra.store.derived :as sd]
            [sutra.authoring.install.diff :as diff]
            [sutra.authoring.install.live :as live]
            [sutra.authoring.install.findings :as fnd]
            [sutra.authoring.install.findings.backing :as backing-findings]))

(defn- ent-of [t]
  {:schema {:objects {:rechnung {:attributes [[:nr :string] [:betrag t]] :plural "Rechnungen"}}}})

(deftest a-property-declares-precision-and-scale
  (is (nil? (ent/explain (ent-of [:decimal 12 2]))))
  (is (nil? (ent/explain (ent-of :decimal))) "bare :decimal stays, stored as DECIMAL(38,10)")
  (testing "a scale above the precision, or a precision past 38, is refused"
    (is (some? (ent/explain (ent-of [:decimal 4 6]))))
    (is (some? (ent/explain (ent-of [:decimal 39 2]))))))

(defn- decimal-precision-problems [property-type column-type]
  (fnd/scan backing-findings/decimal-precision
            (diff/diff {:datasets {:re-ds {:dataset-name :re-ds
                                           :columns [[:nr :string] [:BETRAG column-type]]}}}
                       {}
                       [{:kind :object-type :entity :rechnung :outcome :created
                         :args {:attributes [[:nr :string] [:betrag property-type]]
                                :backing    {:dataset :re-ds :primary-key :nr
                                             :column-map {:betrag :BETRAG}}}}]
                       {}
                       (live/world-index {}))))

(deftest the-backing-column-must-agree
  (is (= [:decimal-precision-mismatch]
         (map :rule (decimal-precision-problems [:decimal 12 2] [:decimal 14 2])))
      "precision and scale must match the backing column")
  (is (empty? (decimal-precision-problems [:decimal 14 2] [:decimal 14 2])))
  (is (empty? (decimal-precision-problems :decimal [:decimal 14 2]))
      "a bare :decimal declares no width to disagree with"))

(deftest a-derived-value-takes-the-declared-scale
  (let [W {:object-types {:ot/rechnung {:object-type-name :rechnung
                                        :attribute-list
                                          [{:attribute-name :anteil :attribute-type [:decimal 12 2]}
                                           {:attribute-name :rest :attribute-type :decimal}
                                           {:attribute-name :stueck :attribute-type :integer}]
                                        :plural "Rechnungen"}}}]
    (is (= [:decimal 2] (sd/cast-of W :ot/rechnung :anteil)))
    (is (= [:decimal 10] (sd/cast-of W :ot/rechnung :rest)))
    (is (= :integer (sd/cast-of W :ot/rechnung :stueck)))))

(defn- typed [t] {:schema {:objects {:auftrag {:attributes [[:nr :string] [:x t]]
                                               :plural "Aufträge"}}}})

(deftest arrays-and-structs-take-their-shapes
  (testing "an array of anything but an array"
    (is (nil? (ent/explain (typed [:array :string]))))
    (is (nil? (ent/explain (typed [:array [:decimal 12 2]]))))
    (is (nil? (ent/explain (typed [:array [:struct {:filename :string}]]))))
    (is (re-find #"nested arrays" (pr-str (ent/explain (typed [:array [:array :string]]))))))
  (testing "a struct of 1 to 10 scalar fields"
    (is (nil? (ent/explain (typed [:struct
                                   {:strasse :string :plz :integer :betrag [:decimal 12 2]}]))))
    (is (some? (ent/explain (typed [:struct {}]))))
    (is (some? (ent/explain (typed [:struct
                                    (zipmap (map #(keyword (str "f" %)) (range 11))
                                            (repeat :string))]))))
    (is (some? (ent/explain (typed [:struct {:inner [:struct {:a :string}]}])))
        "structs do not nest")
    (is (some? (ent/explain (typed [:struct {:tags [:array :string]}])))
        "a struct holds no array")))
