(ns ^:unit ontology.landing-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [sutra.builds.import.materialize :as mat]))

(def ^:private T "ri.sutra.main.object-type.00000000-0000-0000-0000-00000000abcd")

(def ^:private schema
  {:object-types {T {:object-type-name :thing
                     :attribute-list [{:attribute-name :pk :attribute-type :string :description "x"}
                                      {:attribute-name :amount :attribute-type :decimal}]
                     :plural "Things"}}})

(deftest a-landing-is-a-value
  (let [{:keys [name columns]} (mat/landing schema T 1700000000 "0a1b2c3d")]
    (is (str/starts-with? name mat/landing-prefix))
    (is (str/ends-with? name (str "_" (Long/toString 1700000000 36) "_0a1b2c3d")))
    (is (<= (count name) 63) "a pg identifier")
    (is (= [{:attribute-name :pk :attribute-type :string}
            {:attribute-name :amount :attribute-type :decimal}]
           columns)
        "the declared columns, nothing else")
    (is (not= name (:name (mat/landing schema T 1700000000 "ffffffff")))
        "the suffix sets two reads apart"))
  (testing "planning sees the same columns a run lands"
    (is (= {:columns (:columns (mat/landing schema T 1 "x"))} (mat/shape-of schema T)))))

(deftest a-sweep-takes-only-stale-landings
  (let [now   1700000000
        at    (fn [sec] (:name (mat/landing schema T sec "0a1b2c3d")))
        old   (at (- now mat/landing-max-age-sec 1))
        fresh (at (- now 60))]
    (is (= [old]
           (mat/stale-landings [old fresh "s_glpi_hosts" "d_thing" "l_not_a_landing"] now
                               mat/landing-max-age-sec))
        "only what landing gave out, and only past the age no read reaches")))

(deftest a-staged-type-is-read-under-its-edits
  (testing " T3-T13, as the store's index commit"
    (let [source  [{:id "a" :values {:n 1 :m 1}}
                   {:id "b" :values {:n 2 :m 2}}
                   {:id "c" :values {:n 3}}
                   {:id "d" :values {:n 4}}]
          edits   {"b" {:attrs #{:m} :values {:m 20}}
                   "c" {:deleted? true}
                   "d" {:created? true :values {:n 40}}
                   "e" {:created? true :values {:n 50}}
                   "f" {:created? true :deleted? true :values {:n 60}}}
          current {"d" {:id "d" :values {:n 40}}
                   "e" {:id "e" :values {:n 50 :x true}}}
          by-id   (into {} (map (juxt :id :values)) (mat/under-edits source edits current))]
      (is (= {:n 1 :m 1} (by-id "a")) "an unedited row follows the source")
      (is (= {:n 2 :m 20} (by-id "b")) "an edited attribute keeps the edit's value")
      (is (not (contains? by-id "c")) "an object an edit deleted stays gone")
      (is (= {:n 40} (by-id "d")) "an object an edit created no longer follows the source")
      (is (= {:n 50 :x true} (by-id "e")) "an edit-created object the source lacks is read")
      (is (not (contains? by-id "f")) "an object an edit created and then deleted is gone"))))
