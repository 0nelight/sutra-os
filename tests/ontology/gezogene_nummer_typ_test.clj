(ns ^:unit ontology.gezogene-nummer-typ-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.actions.effects :as action-effects]))

(def ^:private welt
  {:object-types {17 {:attribute-list [{:attribute-name :adr-nummer :attribute-type :integer}
                                       {:attribute-name :fibu-nummer :attribute-type :string}]
                      :plural "Objekte"}}})

(def ^:private zwang #'action-effects/coerce-to-declared)

(deftest gezogene-nummer-wird-zur-zahl
  (is (= 1138 (zwang welt 17 :adr-nummer "001138"))))

(deftest zeichenkette-bleibt-gepolstert
  (is (= "001138" (zwang welt 17 :fibu-nummer "001138"))
      "bei :string bleiben die fuehrenden Nullen — sie sind dort die Nummer"))

(deftest unlesbares-bleibt-stehen
  (testing "eine Nummer mit Vorwahl ist keine Zahl — dann lieber laut scheitern als still nil"
    (is (= "K-0042" (zwang welt 17 :adr-nummer "K-0042")))))

(deftest nil-bleibt-nil
  (is (nil? (zwang welt 17 :adr-nummer nil))))
