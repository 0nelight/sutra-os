(ns ^:unit ontology.param-term-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [sutra.ref.params :as rp]
            [sutra.ref.term :as term]
            [sutra.types.value-types :as vt]))

(def ^:private ot "ri.sutra.main.object-type.d52a9bfc-1e41-4251-a9b3-e1f8705a6b14")
(def ^:private ref-types #(when (= ot %) [ot]))

(def ^:private gen-scalar-type (gen/elements (sort (keys term/scalar-classes))))

(def ^:private gen-declared
  (gen/one-of
   [gen-scalar-type
    (gen/return [:decimal 38 10])
    (gen/return [:vector 3])
    (gen/fmap (fn [t] [:array t]) gen-scalar-type)
    (gen/fmap (fn [m] [:struct m])
              (gen/map (gen/elements [:a :b :c]) gen-scalar-type {:min-elements 1}))
    (gen/return ot)
    (gen/return :object-set)]))

(def ^:private gen-spec
  (gen/let [t gen-declared multiple? gen/boolean]
    (cond-> {:type t} multiple? (assoc :multiple true))))

(def ^:private gen-handle
  (gen/fmap (fn [id] {:id (str id) :object-type-ref ot :values {:name (str id)}}) gen/uuid))

(defn- gen-scalar [class]
  (case class
    :text       gen/string-alphanumeric
    :integral   gen/large-integer
    :fractional (gen/double* {:NaN? false :infinite? false})
    :boolean    gen/boolean
    :uuid       gen/uuid
    :date       (gen/fmap #(java.time.LocalDate/ofEpochDay %) (gen/choose 0 40000))
    :instant    (gen/fmap #(java.time.Instant/ofEpochMilli %) (gen/choose 0 4102444800000))
    :opaque     gen/string-alphanumeric))

(defn- gen-value [t]
  (case (first t)
    :scalar        (gen-scalar (second t))
    :decimal       (gen-scalar :fractional)
    :vector        (gen/vector (gen-scalar :fractional) 3)
    (:list :array) (gen/vector (gen-value (second t)) 0 4)
    :struct        (apply gen/hash-map (mapcat (fn [[k ft]] [k (gen-value ft)]) (second t)))
    :ref           gen-handle
    :object-set    (gen/vector gen-handle 0 3)))

(defn- json? [x]
  (cond
    (or (nil? x) (string? x) (number? x) (boolean? x)) true
    (vector? x) (every? json? x)
    (map? x)    (and (every? keyword? (keys x)) (every? json? (vals x)))
    :else       false))

(def ^:private crosses-and-reads-back
  (prop/for-all [[t x] (gen/bind gen-spec
                                 (fn [spec]
                                   (let [t (term/of spec ref-types)]
                                     (gen/tuple (gen/return t) (gen-value t)))))]
                (let [r (term/render t x)
                      w (:value r)
                      p (term/parse t w)]
                  (and (empty? (:problems r))
                       (json? w)
                       (empty? (:problems p))
                       (= w (:value (term/render t (:value p))))))))

(deftest every-term-crosses-the-form-and-reads-back
  (let [r (tc/quick-check 500 crosses-and-reads-back)]
    (is (:pass? r) (pr-str (:shrunk r)))))

(deftest typed-text-reads-as-the-value-it-shows
  (doseq [[t x] [[:integer 3] [:long -7] [:number 12.5] [:decimal 0.75] [:boolean true]
                 [:boolean false]]]
    (testing t
      (let [tm (term/of {:type t})
            w  (:value (term/render tm x))]
        (is (= x (:value (term/parse tm (str w)))))))))

(deftest text-that-does-not-read-stays-in-place-and-is-named
  (let [r (term/parse (term/of {:type :integer :multiple true}) ["1" "x" "3"])]
    (is (= [1 "x" 3] (:value r)))
    (is (= 1 (count (:problems r))))))

(deftest the-term-table-and-the-value-type-registry-agree
  (is (= (disj (set (keys vt/value-types)) :vector) (set (keys term/scalar-classes))))
  (doseq [t (keys vt/value-types)]
    (is (not= :named (first (term/base t))) (str t " has no term"))))

(deftest a-multiple-date-param-keeps-its-dates
  (let [z (java.time.ZoneId/of "Europe/Vienna")
        d (fn [s] (java.time.LocalDate/parse s))]
    (is (= {:d ["2026-06-29" "2026-07-01"]}
           (rp/normalise-dates z {:d {:type :date :multiple true}}
                               {:d [(d "2026-06-29") (d "2026-07-01")]})))))

(deftest references-are-the-handles-where-the-term-says-object
  (let [h {:id "a1" :object-type-ref ot :values {}}]
    (is (= [h] (term/references (term/of {:type ot :multiple true} ref-types) [h])))
    (is (= [] (term/references (term/of {:type :string}) h)))))

(deftest fits-reads-the-term
  (let [z (java.time.ZoneId/of "Europe/Vienna")]
    (testing "a decimal, a JSON-named type and an array's elements are checked like :integer"
      (is (not (rp/fits? z {:type [:decimal 38 10]} "1.5")))
      (is (rp/fits? z {:type [:decimal 38 10]} 1.5M))
      (is (not (rp/fits? z {:type "integer"} 1.5)))
      (is (not (rp/fits? z {:type [:array :integer]} [1 "x"]))))
    (testing "a list is judged element by element, one value as one element"
      (is (rp/fits? z {:type :integer :multiple true} [1 2]))
      (is (rp/fits? z {:type :integer :multiple true} 3))
      (is (not (rp/fits? z {:type :date :multiple true} ["2026-06-29" "morgen"]))))
    (testing "text and references take what they are given"
      (is (rp/fits? z {:type :string} 17))
      (is (rp/fits? z {:type ot} "a1")))))

(deftest a-row-where-the-term-says-object-is-its-object
  (let [row {:id "a1" :values {:name "A"}}]
    (is (= [row] (term/references (term/of {:type ot} vector) row)))
    (is (= "a1" (:value (term/render (term/of {:type ot} vector) row))))
    (is (= "a1" (:value (term/render (term/of {:type :string}) row)))
        "a text param given an object holds its id")))
