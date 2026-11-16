(ns ^:unit platform.spaces-ontologies-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.catalog.branch-policy :as policy]
            [sutra.catalog.grants :as grants]
            [sutra.types.authority :as authority]
            [sutra.types.ontologies :as ont]
            [sutra.types.review :as review]
            [sutra.types.schema :as sch]))

(def ^:private schema
  {:organizations {"ri.o.acme" {:organization-name :acme :members ["ann"] :guests ["gus"]
                                :apply ["acme-admin"] :home-space :acme-privat
                                :discovery {:users :members}}
                   "ri.o.lief" {:organization-name :lief :members ["ben"]
                                :discovery {:name :collaborators}}
                   "ri.o.dritt" {:organization-name :dritt :members ["dora"]
                                 :discovery {:name :members}}}
   :spaces        {"ri.s.acme"   {:space-name :acme-privat :organization :acme}
                   "ri.s.shared" {:space-name :gemeinsam :organizations [:acme :lief]}}
   :projects      {"ri.p.kunden" {:project-name :kunden :space "ri.s.acme"}
                   "ri.p.share"  {:project-name :share :space "ri.s.shared"}
                   "ri.p.alt"    {:project-name :alt}}
   :object-types  {"ri.ot.kunde" {:object-type-name :kunde :project "ri.p.kunden"}
                   "ri.ot.flug"  {:object-type-name :flug :project "ri.p.share"}
                   "ri.ot.ort"   {:object-type-name :ort :project "ri.p.alt"}}
   :link-types    {"ri.lt.ok"  {:link-type-name :am :project "ri.p.kunden"
                                :from-object-type :kunde :to-object-type :ort}
                   "ri.lt.bad" {:link-type-name :fliegt :project "ri.p.kunden"
                                :from-object-type :kunde :to-object-type :flug}}})

(deftest one-ontology-per-space
  (let [by (into {} (map (juxt :name identity)) (ont/ontologies schema))]
    (is (= :default (:kind (by "default"))))
    (is (= :private (:kind (by "acme-privat"))))
    (is (= :shared (:kind (by "gemeinsam"))))
    (is (= ["ri.o.acme" "ri.o.lief"] (:organizations (by "gemeinsam"))))
    (is (= {:object-types 1 :link-types 2} (:counts (by "acme-privat"))))
    (is (= {:object-types 1} (:counts (by "default"))) "no space: the default ontology")
    (is (= #{"ri.ot.flug"}
           (set (keys (:object-types (ont/in-ontology schema "ri.s.shared"))))))))

(deftest a-reference-stays-in-its-ontology
  (is (= [{:section :link-types :resource "ri.lt.bad" :ontology "ri.s.acme"
           :references "ri.ot.flug" :in "ri.s.shared"}]
         (vec (ont/cross-ontology-references schema)))
      "a link into the default ontology is fine; one into another space's is not"))

(deftest one-name-per-ontology-not-per-instance
  (let [s (assoc-in schema [:object-types "ri.ot.kunde2"]
                    {:object-type-name :kunde :project "ri.p.share"})]
    (is (= "ri.ot.kunde2" (sch/rid-of (sch/index (sch/in-scope s "ri.s.shared"))
                                      :object-types :kunde)))
    (is (= "ri.ot.kunde" (sch/rid-of (sch/index (sch/in-scope s "ri.s.acme"))
                                     :object-types :kunde)))))

(deftest a-projects-organizations-lie-within-its-spaces
  (is (empty? (ont/organization-violations
               (assoc-in schema [:projects "ri.p.share" :shared-with] [:lief]))))
  (is (= [{:project "ri.p.kunden" :space "ri.s.acme" :organizations ["ri.o.lief"]}]
         (vec (ont/organization-violations
               (assoc-in schema [:projects "ri.p.kunden" :shared-with] [:lief]))))))

(deftest one-primary-organization-guests-elsewhere
  (is (empty? (ont/primary-conflicts schema)))
  (is (= [{:principal "ann" :organizations ["ri.o.acme" "ri.o.lief"]}]
         (vec (ont/primary-conflicts
               (update-in schema [:organizations "ri.o.lief" :members] conj "ann")))))
  (is (= "ri.o.acme" (ont/primary-organization schema ["x" "ann"]))))

(defn- db [schema subject]
  (authority/database (authority/schema-facts schema)
                      (authority/role-facts grants/standard-roles)
                      (authority/subject-facts subject)))

(deftest walls-of-shared-spaces-and-guests
  (testing "a guest is inside the organization"
    (is (not (contains? (authority/blocked (db schema {:who "gus"}) "gus") "ri.ot.kunde"))))
  (testing "a shared space admits each of its organizations, a private one only its own"
    (let [blocked (authority/blocked (db schema {:who "ben"}) "ben")]
      (is (not (contains? blocked "ri.ot.flug")))
      (is (contains? blocked "ri.ot.kunde"))))
  (is (= #{"ri.o.acme" "ri.o.lief"} (authority/space-organizations schema :gemeinsam))))

(deftest applying-an-organization-needs-apply-organization
  (let [base    #(authority/schema-database (authority/schema-facts %))
        after   (assoc-in schema [:spaces "ri.s.neu"] {:space-name :neu :organization :acme})
        applied (authority/applied (base schema) (base after))]
    (is (= {"ri.s.neu" #{"ri.o.acme"}} applied))
    (let [task {:apply-gates [{:gate "apply-organization/ri.o.acme" :holders ["acme-admin"]}]}]
      (is (seq (review/requires review/default-rules task {:author "ann"}))
          "an author without Apply organization needs an approval")
      (is (empty? (filter #(some #{:apply} (:by %))
                          (review/requires review/default-rules task {:author "acme-admin"})))
          "the holder applies it themself")
      (let [[rule] (filter #(some #{:apply} (:by %))
                           (review/requires review/default-rules task {:author "ann"}))]
        (is (review/qualifies? rule task {:who "acme-admin"} {:author "ann"}))
        (is (not (review/qualifies? rule task {:who "ben" :owner? true} {:author "ann"})))))))

(deftest discoverability
  (let [acme #{"ri.o.acme"} lief #{"ri.o.lief"}]
    (is (ont/discoverable? schema acme "ri.o.lief" :name) "collaborators share a space")
    (is (not (ont/discoverable? schema acme "ri.o.dritt" :name)))
    (is (ont/discoverable? schema #{"ri.o.dritt"} "ri.o.dritt" :name) "its own members may")
    (is (= #{"ri.o.dritt"} (ont/hidden-organizations schema acme :name)))
    (is (ont/principal-hidden? schema lief ["ann"]) "acme shows its users to members only")
    (is (not (ont/principal-hidden? schema acme ["ann"])))
    (is (not (ont/principal-hidden? schema lief ["nobody"])))))

(deftest a-new-project-lands-in-its-creators-home-space
  (is (= {:name :neu :space :acme-privat}
         (ont/with-creator-space schema ["ann"] {:name :neu})))
  (is (= {:name :neu :space :x} (ont/with-creator-space schema ["ann"] {:name :neu :space :x})))
  (is (= {:name :neu} (ont/with-creator-space schema ["ben"] {:name :neu}))
      "lief has no home space and none of its own"))

(deftest a-branch-stays-in-one-ontology
  (let [tasks [{:task "object-types/a" :ontology "ri.s.acme" :ontology-name "acme-privat"}
               {:task "projects/p"}]]
    (is (nil? (policy/ontology-block nil tasks)))
    (is (nil? (policy/ontology-block "acme-privat" tasks)))
    (is (= ["object-types/a"]
           (:outside-ontology (policy/ontology-block "gemeinsam" tasks))))))
