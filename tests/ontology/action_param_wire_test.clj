(ns ^:unit ontology.action-param-wire-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.ref.term :as term]
            [sutra.types.submission :as csub]
            [sutra.actions.operations :as ops]))

(def ^:private partner-rid "ri.sutra.main.object-type.d52a9bfc-1e41-4251-a9b3-e1f8705a6b14")

(defn- obj [id] {:id id :object-type-ref partner-rid :values {:name (str "P" id)}})

(def ^:private ref-types #(when (= partner-rid %) [partner-rid]))

(defn- values [specs bindings]
  (ops/form-values (update-vals specs #(term/of % ref-types)) bindings))

(def ^:private cases
  [[:string    "1406133674"                "1406148827"                "1406133674"]
   [:integer   3                           4                           3]
   [:number    12.5                        7                           12.5]
   [:boolean   true                        false                       true]
   [:date      (java.time.LocalDate/of 2026 6 29)
    (java.time.LocalDate/of 2026 7 1)                       "2026-06-29"]
   [:timestamp (java.time.Instant/parse "2026-06-29T08:00:00Z")
    (java.time.Instant/parse "2026-07-01T08:00:00Z")        "2026-06-29T08:00:00Z"]
   [partner-rid (obj "a1")                 (obj "b2")                  "a1"]])

(deftest every-type-reaches-the-form
  (doseq [[t one two expected] cases
          multiple? [false true]]
    (testing (str t (when multiple? " :multiple"))
      (let [spec (cond-> {:type t} multiple? (assoc :multiple true))
            x    (if multiple? [one two] one)
            {vs :values bad :unencodable} (values {:p spec} {:p x})]
        (is (empty? bad))
        (is (contains? vs :p) "the resolved default is dropped before the form sees it")
        (if multiple?
          (do (is (vector? (:p vs)))
              (is (= 2 (count (:p vs))))
              (is (= expected (first (:p vs)))))
          (is (= expected (:p vs))))
        (when (#{:string :integer :number :boolean} t)
          (is (= x (csub/coerce-param-value spec (:p vs)))
              "the wire value reads back as the binding"))))))

(deftest an-object-param-carries-its-rows-for-the-caption
  (is (= {:p [(obj "a1") (obj "b2")]}
         (:objects (values {:p {:type partner-rid :multiple true}} {:p [(obj "a1") (obj "b2")]})))))

(deftest a-single-value-for-a-multiple-param-becomes-a-list
  (is (= {:p ["x"]} (:values (values {:p {:type :string :multiple true}} {:p "x"})))))

(deftest nothing-and-empty-stay-absent
  (let [r (values {:p {:type :string} :q {:type :string :multiple true}} {:p nil :q []})]
    (is (= {} (:values r)))
    (is (empty? (:unencodable r)))))

(deftest a-shape-json-cannot-carry-is-reported-not-dropped-unseen
  (let [r (values {:p {:type :string} :ok {:type :string}} {:p (fn []) :ok "v"})]
    (is (= {:ok "v"} (:values r)))
    (is (= [:p] (mapv :parameter (:unencodable r))))))
