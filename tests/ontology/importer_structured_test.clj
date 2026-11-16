(ns ^:unit ontology.importer-structured-test
  (:require [sutra.builds.import.coerce :as coerce]
            [sutra.builds.plan :as plan]
            [clojure.test :refer [deftest testing is]]))

(def ^:private derive-casts #'plan/derive-casts)

(defn- cast-row [row to]
  (coerce/apply-transforms row [{:op :cast :column :v :to to}]))

(deftest a-structured-attribute-is-cast
  (testing "an array or struct attribute gets a cast, as a scalar auto-cast type does"
    (is (= [{:op :cast :column :gruppen :to [:array :string]}
            {:op :cast :column :adresse :to [:struct {:plz :integer}]}]
           (derive-casts [{:attribute-name :gruppen :attribute-type [:array :string]}
                          {:attribute-name :adresse :attribute-type [:struct {:plz :integer}]}
                          {:attribute-name :name :attribute-type :string}])))))

(deftest json-text-is-read-back-into-its-structure
  (testing "a list a Python transform returned lands as JSON text and comes back a list"
    (is (= {:v ["Familie" "Mitarbeiter"]} (cast-row {:v "[\"Familie\",\"Mitarbeiter\"]"}
                                                    [:array :string])))
    (is (= {:v []} (cast-row {:v "[]"} [:array :string]))))
  (testing "a struct comes back with keyword fields"
    (is (= {:v {:plz 9020}} (cast-row {:v "{\"plz\":9020}"} [:struct {:plz :integer}]))))
  (testing "anything else stays as it is, for the type check to judge"
    (is (= {:v ["a"]} (cast-row {:v ["a"]} [:array :string])))
    (is (= {:v "Familie, Mitarbeiter"} (cast-row {:v "Familie, Mitarbeiter"} [:array :string])))
    (is (= {:v "[kaputt"} (cast-row {:v "[kaputt"} [:array :string])))
    (is (= {:v nil} (cast-row {:v nil} [:array :string])))))
