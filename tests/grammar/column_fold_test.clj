(ns ^:unit grammar.column-fold-test
  (:require [clojure.test :refer [deftest testing is]]
            [sutra.types.column :as column]
            [sutra.catalog.kernel :as kernel]
            [sutra.ref.install :as ri]
            [sutra.store.pg.tables :as pg-tables]))

(def ^:private folded-twins #'pg-tables/folded-twins)

(def ^:private attrs
  [:stück-zahl :fälligkeit :anhänge :prüfung-nötig? :prüfer-kürzel :begründung
   :überholungsintervall :vollständig-geprüft? :doc-number :menge
   :GRÖSSE :außenhaut])

(deftest one-fold-for-every-namespace-that-spells-it
  (doseq [a attrs]
    (is (= (name (column/attr->col a))
           (column/sql-name a))
        (str "fold disagrees for " a))))

(deftest sql-name-folds-umlauts
  (is (= "stueck_zahl" (column/sql-name :stück-zahl)))
  (is (= "faelligkeit" (column/sql-name :fälligkeit)))
  (is (= "aussenhaut" (column/sql-name :außenhaut)))
  (testing "idempotent on an already-folded physical name"
    (is (= "stueck_zahl" (column/sql-name "stueck_zahl")))))

(deftest folded-twins-finds-only-the-pre-fold-spelling
  (is (= {"stück_zahl" "stueck_zahl"}
         (folded-twins ["internal_uuid" "stück_zahl" "menge"]
                       #{"internal_uuid" "stueck_zahl" "menge"})))
  (testing "a column already spelled folded is not a twin"
    (is (= {} (folded-twins ["stueck_zahl"] #{"stueck_zahl"}))))
  (testing "an undeclared umlaut column is left alone — nothing asks for it"
    (is (= {} (folded-twins ["altwert_übrig"] #{"menge"}))))
  (testing "both spellings present is still a twin — the caller decides"
    (is (= {"stück_zahl" "stueck_zahl"}
           (folded-twins ["stück_zahl" "stueck_zahl"] #{"stueck_zahl"})))))

(def ^:private legal-column #"[A-Za-z0-9_]+")

(def ^:private praedikat-namen
  [:kosten-stimmt? :behoben? :egt-marge-stimmt? :buchung-stimmt? :lufttüchtig?
   :aog? :offen? :prüfung-nötig? :storniert? :vollständig-geprüft? :ø-verbrauch])

(deftest predicate-attributes-that-had-no-column-now-have-one
  (doseq [a praedikat-namen]
    (is (re-matches legal-column (column/sql-name a))
        (str "still illegal: " a " → " (column/sql-name a)))))

(deftest the-clojure-suffixes-and-the-slashed-o-have-a-spelling
  (testing "a predicate's ? says how the NAME reads, not what the column holds"
    (is (= "offen" (column/sql-name :offen?)))
    (is (= "pruefung_noetig" (column/sql-name :prüfung-nötig?)))
    (is (= "vollstaendig_geprueft" (column/sql-name :vollständig-geprüft?))))

  (testing "ø stands for a letter, like the umlauts, so it gets the letters"
    (is (= "oe_verbrauch" (column/sql-name :ø-verbrauch))))

  (testing "a bang is a suffix too"
    (is (= "loeschen" (column/sql-name :löschen!)))))

(deftest the-fold-does-NOT-rewrite-what-it-has-no-rule-for
  (is (= "ri.test.object_type.triebwerk" (column/sql-name :ri.test.object-type.triebwerk)))
  (is (not (re-matches legal-column (column/sql-name :größe%)))
      "…which is exactly what the :columns-valid? gate reports"))

(deftest the-migration-pairs-the-old-spelling-with-the-new-one
  (is (= {"prüfung_nötig?" "pruefung_noetig"}
         (folded-twins ["internal_uuid" "prüfung_nötig?"]
                       #{"internal_uuid" "pruefung_noetig"})))
  (is (= {"ø_verbrauch" "oe_verbrauch"}
         (folded-twins ["ø_verbrauch"] #{"oe_verbrauch"}))))

(def ^:private column-problems (ri/stated-gates :columns-valid?))

(defn- ot [nm attrs]
  {:object-type-name nm
   :attribute-list (mapv (fn [a] {:attribute-name a :attribute-type :string}) attrs)})

(deftest a-type-declaring-both-spellings-is-refused
  (let [schema {:object-types {"ri.o.1" (ot :mangel [:offen :offen?])}}]
    (is (= [{:problem :fold-collision :entity-class :object-types :entity :mangel
             :column :offen :attributes [:offen :offen?]}]
           (column-problems schema))
        "the finding names the type, the column and BOTH attributes — the fold
         itself never shows the author either name")
    (is (some? (:column-errors (kernel/reasons schema)))
        "and it is itemized, not hidden behind a bare gate keyword")
    (is (not (kernel/accept? schema))
        "it is a refusal: the second attribute would silently read the first
         one's values")))

(deftest a-name-that-cannot-BE-a-column-is-refused-at-push-time
  (let [schema {:object-types {"ri.o.1" (ot :mangel [:größe%])}}
        [f]    (column-problems schema)]
    (is (= :illegal-column (:problem f)))
    (is (= :mangel (:entity f)))
    (is (= [:größe%] (:attributes f)))
    (is (not (kernel/accept? schema)))))

(deftest distinct-spellings-are-left-alone
  (is (empty? (column-problems
               {:object-types {"ri.o.1" (ot :mangel [:offen :behoben? :ø-verbrauch])}}))
      "one attribute per column is the ordinary case, umlauts and ? included"))

(deftest a-structurally-broken-schema-does-not-throw-the-explainer
  (is (empty? (column-problems
               {:object-types {"ri.o.1" {:object-type-name :mangel
                                         :attribute-list [{:attribute-type :string}]
                                         :plural "Mängel"}}}))))
