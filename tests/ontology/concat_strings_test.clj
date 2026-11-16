(ns ^:unit ontology.concat-strings-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.ref.expr :as rexpr]
            [sutra.ref.objectset :as ros]
            [sutra.store.derived :as sd]
            [sutra.types.expr :as expr]))

(def ^:private titel [:concat-strings " – " :lieferant :nr])

(deftest concat-strings-is-counterparts-concatenate-strings
  (testing "the parts joined by the separator"
    (is (= "Muster GmbH – 4711" (expr/eval {:lieferant "Muster GmbH" :nr "4711"} titel))))
  (testing "a nil part is skipped, never a dangling separator"
    (is (= "4711" (expr/eval {:lieferant nil :nr "4711"} titel))))
  (testing ":null-if-any? makes one nil part the whole nil"
    (is (nil? (expr/eval {:lieferant nil :nr "4711"}
                         [:concat-strings {:separator "-" :null-if-any? true} :lieferant :nr]))))
  (testing "no part at all is nil, so a title built from it falls back like an unset one"
    (is (nil? (expr/eval {:lieferant nil :nr nil} titel))))
  (testing "a whole number reads as one, a keyword as its name"
    (is (= "R 4711 offen"
           (expr/eval {:nr 4711.0 :s :offen} [:concat-strings " " "R" :nr :s])))
    (is (= "4711" (expr/eval {:nr 4711.00M} [:concat-strings "" :nr])))))

(deftest every-door-knows-it
  (is (empty? (rexpr/unknown-ops titel)))
  (is (empty? (expr/unknown-ops titel)))
  (is (= #{:lieferant :nr} (expr/field-refs titel)))
  (is (= #{:lieferant :nr} (rexpr/field-refs titel))))

(def ^:private W
  {:object-types
   {:rechnung
    {:object-type-name :rechnung
     :title-key :titel
     :attribute-list [{:attribute-name :lieferant :attribute-type :string}
                      {:attribute-name :nr :attribute-type :string}
                      {:attribute-name :titel :attribute-type :string
                       :opts {:derived {:expression titel}}}]
     :plural "Rechnungen"}}})

(deftest a-derived-title-is-computed-by-the-store
  (let [ir {:op :static :root :rechnung :ids []}
        {:keys [steps]} (sd/lower W :rechnung :titel)]
    (is (empty? (sd/leftovers W ir))
        "the plan states it, so a list can order by the title")
    (is (= [{:op :compute :name (:name (last steps)) :expr titel}] steps))
    (is (= "Muster GmbH – 4711"
           (ros/compute-value {:lieferant "Muster GmbH" :nr "4711"} titel true)))
    (is (= "4711" (ros/compute-value {:nr "4711"} titel true)))
    (is (nil? (ros/compute-value {} titel true)))))
