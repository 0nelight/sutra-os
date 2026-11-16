(ns ^:unit ontology.importer-scoping
  (:require [sutra.builds.import.nodes :as import-nodes]
            [clojure.test :refer [deftest is testing]]
            [sutra.builds.plan :as plan]))

(defn- resolve-target-rids [schema names]
  (:object-types (plan/resolve-build-targets schema names)))

(def ^:private schema
  {:object-types
   {"rid-email" {:object-type-name :email :plural "Emails"}
    "rid-be"    {:object-type-name :auftrags-ereignis :plural "Bestellereignisse"}}
   :transforms
   {    "rid-classify" {:transform-name :classify-events
                    :output {:kind :object-type :object-type :auftrags-ereignis}}
    "rid-pull"     {:transform-name :pull-mailbox
                    :output {:kind :object-type :object-type :email}}}})

(deftest resolve-target-rids-test
  (testing "a TRANSFORM name resolves to its output object-type's RID (the B1 bug)"
    (is (= #{"rid-be"} (resolve-target-rids schema [:classify-events])))
    (is (= #{"rid-email"} (resolve-target-rids schema [:pull-mailbox]))))

  (testing "an OBJECT-TYPE name still resolves directly (back-compat)"
    (is (= #{"rid-be"} (resolve-target-rids schema [:auftrags-ereignis])))
    (is (= #{"rid-email"} (resolve-target-rids schema [:email]))))

  (testing "a mix of transform + object-type names unions their RIDs"
    (is (= #{"rid-email" "rid-be"}
           (resolve-target-rids schema [:email :classify-events]))))

  (testing "string names resolve like keywords"
    (is (= #{"rid-be"} (resolve-target-rids schema ["classify-events"]))))

  (testing "unknown names are dropped (never scope to nothing-by-accident vs everything)"
    (is (= #{} (resolve-target-rids schema [:does-not-exist])))
    (is (= #{"rid-be"} (resolve-target-rids schema [:classify-events :does-not-exist])))))

(def ^:private fk-spec
  {:from-type :auftrags-ereignis :to-type :partner-email :fk-side :from
   :fk-attribute :von-adresse :cardinality :many-to-one})

(def ^:private seed-writeback-endpoints #'import-nodes/seed-writeback-endpoints)

(deftest empty-endpoint-resolves-to-zero-links-not-a-throw
  (let [links {:ereignis-le fk-spec}]
    (testing "an endpoint whose live table is EMPTY is still recorded — as empty"
      (let [idx (seed-writeback-endpoints (constantly []) links {})]
        (is (contains? idx :partner-email)
            "the PK-side endpoint is known-and-empty, not absent")
        (is (= {} (get idx :partner-email)))))

    (testing "an endpoint with live rows is indexed by its scalar values (unchanged)"
      (let [rows-of (fn [t] (when (= t :partner-email)
                              [{:id "uuid-le1" :values {:adresse "a@x.example"}}]))
            idx     (seed-writeback-endpoints rows-of links {})]
        (is (= {"a@x.example" "uuid-le1"} (get idx :partner-email)))))

    (testing "an endpoint already in pk-index is not re-read"
      (is (= {} (seed-writeback-endpoints (fn [_] (throw (ex-info "should not be called" {})))
                                          links
                                          {:partner-email {"a@x.example" "uuid-le1"}
                                           :auftrags-ereignis {}}))))))
