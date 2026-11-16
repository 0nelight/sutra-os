(ns ^:unit ontology.delete-constants-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.sources.policy :as policy]))

(def ^:private t0 (java.time.LocalDateTime/of 2026 10 7 9 30 5))

(def ^:private log-basis
  {:table "SYS_NOTICE_CHANGES"
   :columns {:table "TABLE_NAME" :pk "PK_FIELDVALUE" :column "COLUMN_NAME"
             :previous "PREVIOUS_VALUE" :next "SET_VALUE"
             :actor ["CURRENTUSER"]}
   :constants {"NOTICE_TYPE_ID" 1 "PROGRAM_NAME" "SUTRA"}})

(defn- plan [log]
  (#'policy/delete-plan t0 "ADR_STAMM"
                    {:pk "ADR_ID" :deletable? true
                     :delete-guard ["ADR_NUMMER" "KURZBEZEICHNUNG"]
                     :log log :column-map {}}
                    {:pk-value 4134
                     :expect {"ADR_NUMMER" "1137" "KURZBEZEICHNUNG" "PROBE"}
                     :actor "gwen" :correlation "abc"}))

(deftest without-delete-constants-a-deletion-looks-like-a-change
  (let [rows (get-in (plan log-basis) [:log :rows])]
    (is (= 2 (count rows)) "eine Zeile je Waechterspalte")
    (is (every? #(= 1 (get % "NOTICE_TYPE_ID")) rows)
        "das ist der Code fuer eine AENDERUNG — genau der Fehler")))

(deftest delete-constants-win-over-constants
  (let [rows (get-in (plan (assoc log-basis :delete-constants {"NOTICE_TYPE_ID" 2}))
                     [:log :rows])]
    (is (every? #(= 2 (get % "NOTICE_TYPE_ID")) rows)
        "die Loeschung traegt jetzt den Loeschcode")
    (testing "was :delete-constants nicht nennt, bleibt aus :constants stehen"
      (is (every? #(= "SUTRA" (get % "PROGRAM_NAME")) rows)))))

(deftest the-before-image-is-intact
  (let [rows (get-in (plan (assoc log-basis :delete-constants {"NOTICE_TYPE_ID" 2}))
                     [:log :rows])
        nach (into {} (map (juxt #(get % "COLUMN_NAME") #(get % "PREVIOUS_VALUE"))) rows)]
    (is (= {"ADR_NUMMER" "1137" "KURZBEZEICHNUNG" "PROBE"} nach)
        "der alte Wert steht drin — sonst waere die Zeile spurlos weg")
    (is (every? #(nil? (get % "SET_VALUE")) rows)
        "und kein neuer, denn es gibt keinen mehr")))
