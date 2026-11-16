(ns ^:unit platform.template-values-test
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing]]
            [sutra.types.submission :as csub]))

(def ^:private params
  {:posten   {:type :object-set :object-type :rechnung}
   :auftrag  {:type :object :object-type :arbeitsauftrag}
   :monat    {:type :string}})

(defn- decode [values]
  (csub/decode-template-values params values #(json/read-str % :key-fn keyword)))

(deftest object-set-text-becomes-an-id-list-or-a-read-spec
  (testing "a comma list is an id list"
    (is (= ["4711" "4712"] (:posten (decode {:posten "4711, 4712"})))))

  (testing "one id is a list of one"
    (is (= ["4711"] (:posten (decode {:posten "4711"})))))

  (testing "an EDN vector and a JSON array are both id lists"
    (is (= ["a" "b"] (:posten (decode {:posten "[\"a\" \"b\"]"}))))
    (is (= ["a" "b"] (:posten (decode {:posten "[\"a\",\"b\"]"}))))
    (is (= ["4711" "4712"] (:posten (decode {:posten "[4711 4712]"})))))

  (testing "an EDN map is a read spec"
    (is (= {:where {:status {:= "offen"}}}
           (:posten (decode {:posten "{:where {:status {:= \"offen\"}}}"})))))

  (testing "a JSON object is a read spec with keyword keys"
    (is (= {:where {:status {:= "offen"}}}
           (:posten (decode {:posten "{\"where\": {\"status\": {\"=\": \"offen\"}}}"})))))

  (testing "an empty value is an explicitly empty set, not the whole type"
    (is (= [] (:posten (decode {:posten ""}))))))

(deftest only-object-sets-are-touched
  (is (= {:auftrag "WO25.0034" :monat "2026-07,2026-08"}
         (decode {:auftrag "WO25.0034" :monat "2026-07,2026-08"})))
  (testing "an undeclared name passes through for the door to refuse"
    (is (= {:nix "a,b"} (decode {:nix "a,b"})))))

(deftest unreadable-text-reaches-the-door-unchanged
  (is (= "{:where" (:posten (decode {:posten "{:where"}))))
  (is (= "[a b" (:posten (csub/decode-template-values params {:posten "[a b"})))))
