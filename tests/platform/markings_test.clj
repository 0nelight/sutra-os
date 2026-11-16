(ns ^:unit platform.markings-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.catalog.branch-policy :as policy]
            [sutra.catalog.grants :as grants]
            [sutra.types.authority :as authority]
            [sutra.types.review :as review]))

(def ^:private schema
  {:projects     {"ri.p.hr"      {:project-name :hr :markings [:pii]
                                  :grants [{:principal "anna" :role :ontology-owner}
                                           {:principal "ben" :role :ontology-viewer}]}
                  "ri.p.finanz"  {:project-name :finanz
                                  :grants [{:principal "ben" :role :ontology-viewer}]}
                  "ri.p.offen"   {:project-name :offen :forbidden-markings [:pii]}
                  "ri.p.geheim"  {:project-name :geheim :allowed-markings [:secret :confidential]}}
   :object-types {"ri.ot.person" {:project "ri.p.hr"}
                  "ri.ot.gehalt" {:project "ri.p.finanz" :markings [:confidential]}
                  "ri.ot.kosten" {:project "ri.p.finanz"}
                  "ri.ot.lage"   {:project :offen}
                  "ri.ot.plan"   {:project :geheim :markings [:secret]}}
   :datasets     {"ri.ds.person" {:project "ri.p.hr" :object-type "ri.ot.person"}}
   :transforms   {"ri.t.kosten"  {:project "ri.p.finanz"
                                  :inputs {:p {:kind :object-type :object-type "ri.ot.person"}}
                                  :output {:kind :object-type :object-type "ri.ot.kosten"}}
                  "ri.t.lage"    {:project :offen
                                  :input {:kind :dataset :dataset "ri.ds.person"}
                                  :output {:kind :object-type :object-type "ri.ot.lage"}}}})

(def ^:private classification
  {:secret {:category "cls" :rank 3} :confidential {:category "cls" :rank 2}})

(defn- db [subject]
  (authority/database (authority/schema-facts schema)
                      (authority/role-facts grants/standard-roles)
                      (authority/subject-facts (assoc subject :classification classification))))

(defn- blocked [subject] (authority/blocked (db subject) (:who subject)))

(deftest a-marking-travels-to-everything-made-from-it
  (let [b (blocked {:who "ben"})]
    (testing "a project's marking holds for the resources it holds"
      (is (contains? b "ri.p.hr"))
      (is (contains? b "ri.ot.person"))
      (is (contains? b "ri.ds.person")))
    (testing "and along the lineage, across projects"
      (is (contains? b "ri.t.kosten"))
      (is (contains? b "ri.ot.kosten")
          "computed from persons, so it carries :pii in another project")
      (is (contains? b "ri.ot.lage")))
    (testing "what nothing marks stays open"
      (is (not (contains? b "ri.p.finanz"))))))

(deftest a-principal-needs-every-marking
  (is (empty? (filter #{"ri.ot.kosten" "ri.ot.person"} (blocked {:who "ben" :clears #{:pii}}))))
  (is (contains? (blocked {:who "ben" :clears #{:pii}}) "ri.ot.gehalt")
      ":confidential is still missing")
  (is (empty? (blocked {:who "root" :unrestricted? true}))
      "an unrestricted principal is blocked by nothing"))

(deftest a-classification-covers-the-lower-ranks-of-its-category
  (let [b (blocked {:who "ben" :clears #{:pii :secret}})]
    (is (not (contains? b "ri.ot.gehalt")) ":secret covers :confidential")
    (is (not (contains? b "ri.ot.plan"))))
  (is (contains? (blocked {:who "ben" :clears #{:pii :confidential}}) "ri.ot.plan")
      ":confidential does not cover :secret"))

(deftest a-mandatory-no-overrides-every-role-yes
  (let [d (db {:who "ben"})]
    (testing "a granted project the subject is not cleared for grants nothing"
      (is (= {"ri.p.finanz" #{:read :discover}}
             (authority/granted-operations d "ben" #{"ri.p.hr" "ri.p.finanz"}))))
    (testing "the refusal explains the marking"
      (let [{:keys [allow? because]} (authority/decide d {:who "ben" :do :read :on "ri.p.hr"})]
        (is (not allow?))
        (is (= :blocked/marking (:rule because)))
        (is (some #(= [:marked ["ri.p.hr" :pii]] (:fact %)) (:from because))))))
  (testing "a global role does not pass a marking either"
    (let [d (db {:who "cleo" :global-role :ontology-viewer})]
      (is (:allow? (authority/decide d {:who "cleo" :do :read :on "ri.p.finanz"})))
      (is (not (:allow? (authority/decide d {:who "cleo" :do :read :on "ri.p.hr"}))))))
  (testing "cleared, the grant holds again"
    (is (contains? (get (authority/granted-operations (db {:who "ben" :clears #{:pii}}) "ben")
                        "ri.p.hr") :read))))

(deftest the-schema-stage-is-the-whole-policy
  (let [base (authority/schema-database (authority/schema-facts schema)
                                        (authority/role-facts grants/standard-roles))]
    (doseq [s [{:who "ben"} {:who "ben" :clears #{:pii}} {:who "anna"}
               {:who "root" :unrestricted? true}]]
      (let [s (assoc s :classification classification)]
        (is (= (authority/blocked (db s) (:who s))
               (authority/blocked (authority/with-subject base (authority/subject-facts s))
                                  (:who s))))))))

(deftest a-project-constraint-is-an-invariant-of-the-schema
  (let [vs (set (map (juxt :resource :marking :reason) (authority/constraint-violations schema)))]
    (is (contains? vs ["ri.ot.lage" :pii :forbidden])
        "inherited along the lineage, and forbidden where it lands")
    (is (contains? vs ["ri.t.lage" :pii :forbidden]))
    (is (not (some #(= "ri.ot.plan" (first %)) vs)) ":secret is allowed in :geheim")
    (is
     (empty?
      (authority/constraint-violations (update schema :projects dissoc "ri.p.offen"
                                               "ri.p.geheim"))))))

(deftest removing-a-marking-expands-access
  (testing "a marking added or removed is never growth"
    (is (= :breaking (review/change-kind "modified" {:markings [:pii]} {:markings [:pii :secret]})))
    (is (= :breaking (review/change-kind "modified" {:markings [:pii]} {:markings []}))))
  (testing "only a removed marking expands access"
    (is (review/expands-access? "modified" {:markings [:pii :secret]} {:markings [:pii]}))
    (is (not (review/expands-access? "modified" {:markings [:pii]} {:markings [:pii :secret]})))
    (is (not (review/expands-access? "removed" {:markings [:pii]} nil))))
  (testing "it waits for whoever holds the lifted control, even on the author's own project"
    (let [t {:kind :breaking :project "ri.p.hr" :expands-access true}]
      (is (= [{:when [:expands-access] :by [:access] :count 1}]
             (review/requires review/default-rules t
                              {:author "anna" :author-owns? (constantly true)})))))
  (testing "and never lands as a direct write"
    (is (policy/protected-row? {:resources #{} :projects #{}}
                               {:rid "ri.ot.person" :expands-access true}))
    (is (not (policy/protected-row? {:resources #{} :projects #{}} {:rid "ri.ot.person"})))))

(deftest a-source-marking-travels-to-what-is-read-from-it
  (let [schema {:projects     {"ri.p.ein" {:project-name :ein}}
                :sources      {"ri.src.erp" {:source-name :erp :project "ri.p.ein" :markings [:pii]}
                               "ri.src.web" {:source-name :web :project "ri.p.ein"}}
                :datasets     {"ri.ds.raw" {:project "ri.p.ein"
                                            :external-source {:datasource :erp :table "T"}}}
                :object-types {"ri.ot.kunde" {:project "ri.p.ein"}
                               "ri.ot.mail"  {:project "ri.p.ein"}
                               "ri.ot.offen" {:project "ri.p.ein"}}
                :transforms   {"ri.t.sync" {:project "ri.p.ein"
                                            :input {:kind :datasource :datasource :erp
                                                    :table "KUNDE"}
                                            :output {:kind :object-type :object-type "ri.ot.kunde"}}
                               "ri.t.ext"  {:project "ri.p.ein" :external-systems [:erp]
                                            :output {:kind :object-type :object-type "ri.ot.mail"}}
                               "ri.t.web"  {:project "ri.p.ein" :external-systems [:web]
                                            :output {:kind :object-type
                                                     :object-type "ri.ot.offen"}}}}
        db     (fn [s] (authority/database (authority/schema-facts schema)
                                           (authority/role-facts grants/standard-roles)
                                           (authority/subject-facts s)))
        b      (authority/blocked (db {:who "ben"}) "ben")]
    (testing "the source carries its marking, and what reads it does too"
      (is (contains? b "ri.src.erp"))
      (is (contains? b "ri.ot.kunde") "a :datasource input")
      (is (contains? b "ri.ot.mail") "an external system")
      (is (contains? b "ri.ds.raw") "an :external-source"))
    (testing "an unmarked source marks nothing"
      (is (not (contains? b "ri.src.web")))
      (is (not (contains? b "ri.ot.offen"))))
    (testing "a clearance opens all of it"
      (is (empty? (authority/blocked (db {:who "ben" :clears #{:pii}}) "ben"))))
    (testing "a project constraint judges a source like any resource"
      (is (contains? (set (map (juxt :resource :marking :reason)
                               (authority/constraint-violations
                                (assoc-in schema [:projects "ri.p.ein" :forbidden-markings]
                                          [:pii]))))
                     ["ri.ot.kunde" :pii :forbidden])))))
