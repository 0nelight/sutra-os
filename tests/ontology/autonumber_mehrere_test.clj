(ns ^:unit ontology.autonumber-mehrere-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.parser.spec :as spec]))

(def ^:private einer
  {:table "SYS_AUTONR" :where {"TABELLENNAME" "ADR_STAMM" "FELDNAME" "ADR_NUMMER"}
   :counter-column "STARTNUMMER" :width-column "STELLENANZAHL" :prefix-column "VORWAHL"
   :into "ADR_NUMMER"})

(def ^:private zweiter
  (assoc einer :where {"TABELLENNAME" "ADR_STAMM" "FELDNAME" "ADRNR_FIBU"}
         :into "ADRNR_FIBU"))

(defn- allow [autonumber]
  {"ADR_STAMM"
   (cond-> {:pk "ADR_ID"
            :identity "ADR_ID"
            :object-type :adresse
            :match-key "ADR_NUMMER"
            :timestamp-attr :geaendert
            :source-of-record :apply-most-recent
            :log {:table "SYS_NOTICE_CHANGES"
                  :columns {:table "TABLE_NAME" :pk "PK_FIELDVALUE"
                            :column "COLUMN_NAME" :previous "PREVIOUS_VALUE"
                            :next "SET_VALUE" :date "EROEFFNUNGSDATUM"
                            :time "EROEFFNUNGSTIME" :correlation "POSITION"
                            :actor ["CURRENTUSER" "OPEN_USER"]}
                  :constants {"PROGRAM_NAME" "SUTRA"}
                  :insert-constants {"PROGRAM_NAME" "SUTRA"}}
            :column-map {"kurzbezeichnung" "KURZBEZEICHNUNG"}
            :writable-columns ["KURZBEZEICHNUNG"]
            :insertable-columns ["KURZBEZEICHNUNG" "ADR_NUMMER" "ADRNR_FIBU"]}
     autonumber (assoc :autonumber autonumber))})

(deftest one-counter-still-works
  (is (spec/writeback-valid? (allow einer))
      "die bisherige Form bleibt gueltig — eine Map, kein Vektor"))

(deftest two-counters-are-accepted
  (is (spec/writeback-valid? (allow [einer zweiter]))
      "zwei Zaehler in einem Vektor"))

(deftest every-counter-must-hold-up
  (testing "auch der zweite braucht eine Breite"
    (is (not (spec/writeback-valid? (allow [einer (dissoc zweiter :width-column)])))))
  (testing "auch der zweite muss in eine EINFUEGBARE Spalte schreiben"
    (is (not (spec/writeback-valid? (allow [einer (assoc zweiter :into "GIBT_ES_NICHT")])))))
  (testing "und ein einzelner ebenso — die Regel wurde nicht weicher"
    (is (not (spec/writeback-valid? (allow (dissoc einer :width-column)))))))

(deftest no-counter-is-fine
  (is (spec/writeback-valid? (allow nil))))
