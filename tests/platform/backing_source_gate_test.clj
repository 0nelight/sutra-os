(ns ^:unit platform.backing-source-gate-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.authoring.install :as install]
            [sutra.authoring.install.plan :as install-plan]
            [sutra.builds.plan :as plan]))

(defn- gate
  [desired]
  (->> (install-plan/plan-problems desired {} (install/plan-schema desired {} {}) {})
       (filter #(= :root-transform-sourceless (:rule %)))))

(def ^:private dataset-schema
  {:projects {:werft {}}
   :datasets {:zeitbuchung_raw
              {:project :werft
               :external-source {:datasource :mro}}}})

(deftest eine-wurzel-ohne-tabelle-wird-beim-speichern-abgewiesen
  (testing "eine :external-source, die nur die Datasource nennt, faellt"
    (let [[f] (gate dataset-schema)]
      (is (some? f))
      (is (= :zeitbuchung_raw-sync (:entity f)))
      (is (= :mro (:datasource f)))
      (is (re-find #":source\.table" (:message f)))))
  (testing "mit :table geht es durch"
    (is (empty? (gate (assoc-in dataset-schema
                                [:datasets :zeitbuchung_raw :external-source :table]
                                "ZV_ZEITEN")))))
  (testing "eine :query statt einer :table genuegt genauso"
    (is (empty? (gate (assoc-in dataset-schema
                                [:datasets :zeitbuchung_raw :external-source :query]
                                "select 1"))))))

(def ^:private link-schema
  {:projects {:werft {}}
   :objects {:rechnung {:project :werft
                        :attributes [[:id :string]]
                        :external-source {:datasource :mro :table "R"
                                          :primary-key :id}
                        :plural "Rechnungen"}}
   :links [[:anrechnung :rechnung :rechnung :many-to-many
            {:project :werft
             :external-source {:datasource :mro
                               :from-key-column :from_id
                               :to-key-column :to_id}}]]})

(deftest ein-m-n-link-ohne-join-tabelle-faellt-genauso
  (let [[f] (gate link-schema)]
    (is (some? f) "eine m:n-Verknuepfung braucht eine benannte Join-Tabelle")
    (is (= :anrechnung-conn-rows (:entity f))))
  (is (empty? (gate (assoc-in link-schema [:links 0 4 :external-source :table] "c_anrechnung")))))

(def ^:private live-schema
  {:object-types {:rechnung {:object-type-name :rechnung
                             :attribute-list [{:attribute-name :id :attribute-type :string}]
                             :plural "Rechnungen"}}
   :link-types {:anrechnung {:link-type-name :anrechnung
                             :from-object-type :rechnung
                             :to-object-type :rechnung
                             :cardinality :many-to-many}}
   :transforms {:rechnung-rows {:transform-name :rechnung-rows
                                :input {:kind :datasource :datasource :mro}
                                :source {:table "R"}
                                :output {:kind :object-type :object-type :rechnung
                                         :name "rechnung-rows" :primary-key :id}}
                :anrechnung-conn-rows {:transform-name :anrechnung-conn-rows
                                       :input {:kind :datasource :datasource :mro}
                                       :source {}
                                       :output {:kind :link-type :link-type :anrechnung
                                                :name "anrechnung-conn-rows"
                                                :from-key-column :from_id
                                                :to-key-column :to_id}}}})

(deftest eine-durchgerutschte-wurzel-kippt-den-lesezugriff-nicht
  (testing "der BUILD-Pfad bleibt streng"
    (is (thrown-with-msg? Exception #"Root transform must declare"
                          (plan/derive-plan-from-schema live-schema))))
  (testing "der LESE-Pfad laesst den Rest der Ontologie stehen und benennt den Ausfall"
    (let [{:keys [plan unresolved]} (plan/derive-plan-leniently live-schema)]
      (is (contains? (:objects plan) :rechnung)
          "ein gesunder Typ laedt weiter — genau das war der Ausfall")
      (is (empty? (:links plan)))
      (is (= [:link-type] (map :kind unresolved)))
      (is (= [:anrechnung] (map :name unresolved)))
      (is (re-find #"Root transform must declare" (:error (first unresolved)))))))
