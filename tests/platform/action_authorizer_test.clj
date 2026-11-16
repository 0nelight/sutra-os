(ns ^:unit platform.action-authorizer-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.actions.action :as action]))

(def ^:private schema
  {:projects     {"ri.p.a" {:project-name :a}}
   :object-types {"ri.ot.m" {:object-type-name :m :project "ri.p.a" :plural "Ms"}
                  "ri.ot.h" {:object-type-name :h :project "ri.p.a" :plural "Hs"}}
   :link-types   {"ri.lt.l" {:link-type-name :l :project "ri.p.a"}}
   :actions      {:melden {:rules []}}})

(def ^:private viewer
  {:user "u" :operations #{} :project-operations {"ri.p.a" #{:read}}
   :hidden-object-types #{"ri.ot.h"}})

(defn- authorize [auth declared?]
  (#'action/make-authorizer (constantly false) schema auth declared?))

(defn- denied? [f op target]
  (try (f op target nil) false
       (catch clojure.lang.ExceptionInfo e (boolean (:sutra/acl-denied (ex-data e))))))

(deftest a-declared-action-needs-only-to-see-what-it-edits
  (let [f (authorize viewer true)]
    (doseq [op [:create :update :delete-object]]
      (is (not (denied? f op {:type "ri.ot.m" :id "1"})) op))
    (is (not (denied? f :create-link {:type "ri.lt.l" :id "1"})))
    (testing "a type the writer cannot see is not edited, whatever the action says"
      (is (denied? f :update {:type "ri.ot.h" :id "1"})))
    (testing "a marking and project management keep their operation"
      (is (denied? f :apply-marking {:type "ri.ot.m" :id "1"}))
      (is (denied? f :create {:type :projects :id "ri.p.a"})))))

(deftest outside-an-action-an-edit-needs-the-operation
  (let [f (authorize viewer false)]
    (is (denied? f :create {:type "ri.ot.m" :id "1"}))
    (is (denied? f :update {:type "ri.ot.m" :id "1"}))))

(deftest no-one-sees-nothing-and-edits-nothing
  (is (denied? (authorize nil true) :update {:type "ri.ot.m" :id "1"})))

(deftest which-actions-are-declared
  (is (action/declared-action? schema :melden))
  (is (not (action/declared-action? schema :apply-marking)) "a platform action is not")
  (is (not (action/declared-action? schema :create-object-type)))
  (is (not (action/declared-action? schema nil)) "ad-hoc rules name no action"))
