(ns ^:unit ontology.edit-function-transpile-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.catalog.program :as prog]))

(def ^:private schema
  {:object-types
   {"ri.b" {:object-type-name :arbeitsauftrag
            :attribute-list [{:attribute-name :nr :attribute-type :string}]
            :plural "Arbeitsaufträge"}
    "ri.v" {:object-type-name :vorhaben
            :attribute-list [{:attribute-name :vorhaben-nr :attribute-type :string}
                             {:attribute-name :name :attribute-type :string}]
            :plural "Vorhaben"}}
   :link-types
   {"ri.bv" {:link-type-name :arbeitsauftrag-vorhaben
             :from-object-type "ri.b" :to-object-type "ri.v"
             :cardinality :one-to-many}}})

(defn- compiled-steps [src]
  (let [{:keys [params steps]} (prog/parse-source
                                {:language :clojure :source src
                                 :types {:wo :arbeitsauftrag}})]
    (:steps (prog/compile-function schema steps params))))

(deftest link-relation-compiles-to-a-create-link-edge
  (let [src (str "(defn wire [wo]"
                 "  (let [v (-> OntologyClient/action"
                 "              (.create-vorhaben {:vorhaben-nr \"V-1\" :name \"Neubau\"}))]"
                 "    (-> OntologyClient/action (.link-arbeitsauftrag-vorhaben wo v))))")
        [obj link] (compiled-steps src)]
    (testing "the object create still compiles (unchanged)"
      (is (= :create (:op obj)))
      (is (= :vorhaben (:object-type obj)))
      (is (= {:vorhaben-nr "V-1" :name "Neubau"} (:values obj))))
    (testing "link-<relation> is a :create-link edge, NOT a dead :effect"
      (is (= :create-link (:op link)))
      (is (= :arbeitsauftrag-vorhaben (:link-type link))
          "the relation apiName rides through (executor resolves the RID)")
      (is (= [:get :wo :id] (:from-id link))
          "from-id is the param object's :id (resolve-binding reads a handle or a raw id)")
      (is (= [:get (:as obj) :id] (:to-id link))
          "to-id is the NEWLY created object's :id, via its :as binding"))))

(deftest an-unknown-action-method-still-falls-through-to-effect
  (let [src "(defn f [x] (-> OntologyClient/action (.recompute-totals x)))"
        [step] (compiled-steps src)]
    (is (= :effect (:op step)))
    (is (= :recompute-totals (:fn step)))))

(deftest a-tagged-parameter-of-a-step-function-is-typed
  (let [params #(:params (prog/parse-source (merge {:language :clojure :schema schema} %)))
        src    "(defn lies [^Arbeitsauftrag wo n] (OntologyClient/apply-action :x {:wo wo :n n}))"]
    (testing "^Tag names the object type, so a run can dereference the id it is handed"
      (is (= :arbeitsauftrag (get-in (params {:source src}) [:wo :type])))
      (is (nil? (get-in (params {:source src}) [:n :type]))))
    (testing "explicit :types still win"
      (is (= :vorhaben (get-in (params {:source src :types {:wo :vorhaben}}) [:wo :type]))))))
