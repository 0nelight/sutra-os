(ns ^:unit platform.source-adopt-test
  (:require [sutra.parser.spec :as spec]
            [clojure.test :refer [deftest is testing]]
            [sutra.parser.adopt :as adopt]
            [sutra.parser.ent :as ent]
            [sutra.parser.lint :as lint]
            [sutra.authoring.install :as install]
            [sutra.authoring.install.plan :as iplan]))

(def ^:private cols
  [{:name "ID" :ontology-type :integer} {:name "Name" :ontology-type :string}
   {:name "Modified" :ontology-type :string}])

(def ^:private base
  {:source :erp :table "Kunden_Stamm" :columns cols :existing {:objects #{} :datasets #{}}})

(deftest a-table-becomes-a-virtual-dataset-and-a-backed-type
  (let [{:keys [schema dataset key type]} (adopt/plan (assoc base :as :kunde :project :p))]
    (is (= :kunden_stamm dataset))
    (is (= [:id] key) "the one column named id is the key")
    (is (= :kunde type))
    (is (= {:source :erp :table "Kunden_Stamm" :primary-key [:id]}
           (get-in schema [:datasets :kunden_stamm :virtual])))
    (is (= {:dataset :kunden_stamm :primary-key :id} (get-in schema [:objects :kunde :backing])))
    (testing "the fragment is a well-formed schema that plans and lints clean"
      (let [full (merge {:projects {:p {}}
                         :sources  {:erp {:project :p :type :postgres
                                          :jdbc-url "jdbc:postgresql://db:5432/erp"
                                          :egress ["db:5432"]}}}
                        schema)
            plan (install/plan-schema full {} {})]
        (is (nil? (ent/explain {:schema full})))
        (is (some #(= [:object-type :kunde :created] ((juxt :kind :entity :outcome) %)) plan))
        (is (empty? (iplan/plan-problems full {} plan)))
        (is (not-any? #(= :backing-without-producer (:id %))
                      (lint/run-lint full (spec/vocabulary))))))))

(deftest it-refuses-to-guess-or-to-shadow
  (let [problem (comp :problem adopt/plan)]
    (is (= :no-key (problem (assoc base :columns [{:name "Name"}]))) "no key column, no guess")
    (is (= :unknown-key (problem (assoc base :key [:nr]))))
    (is (= :type-exists (problem (assoc base :as :kunde :existing {:objects #{:kunde}})))
        "a shadow type is refused")
    (is (= :dataset-exists (problem (assoc base :existing {:datasets #{:kunden_stamm}}))))
    (is (= :unknown-dataset (problem (assoc base :into :gibts-nicht))))
    (is (= :composite-key (problem (assoc base :as :kunde :key [:id :name]))))))

(deftest into-binds-an-existing-dataset-to-the-source
  (let [{:keys [schema
                dataset]} (adopt/plan (assoc base :into :postfach_mails :key [:id] :project :mail
                                                    :existing {:datasets #{:postfach_mails}}))]
    (is (= :postfach_mails dataset))
    (is (= :virtual (get-in schema [:datasets :postfach_mails :kind])))
    (is (nil? (get-in schema [:datasets :postfach_mails :project])) "it stays where it stands")))
