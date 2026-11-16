(ns ^:unit ontology.schedule-scope-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.catalog.transforms :as transforms]
            [sutra.builds.plan :as plan]))

(def ^:private schema
  {:object-types {"ri.email" {:object-type-name :email :plural "Emails"}
                  "ri.pos-a" {:object-type-name :pos-a :plural "Pos as"}
                  "ri.pos-b" {:object-type-name :pos-b :plural "Pos bs"}}
   :transforms
   {:pull    {:transform-name :pull
              :input  {:kind :datasource :datasource :imap}
              :output {:kind :object-type :object-type "ri.email"}}
    :parse-a {:transform-name :parse-a :language :python
              :inputs {:mail {:kind :object-type :object-type "ri.email"}}
              :output {:kind :object-type :object-type "ri.pos-a"}}
    :parse-b {:transform-name :parse-b :language :python
              :inputs {:mail {:kind :object-type :object-type "ri.email"}}
              :output {:kind :object-type :object-type "ri.pos-b"}}}})

(deftest downstream-closure-test
  (testing "closure includes the target + its transitive consumers, excludes upstream"
    (is (= #{:pull :parse-a :parse-b} (transforms/downstream-closure schema [:pull])))
    (is (= #{:parse-a} (transforms/downstream-closure schema [:parse-a]))
        "a leaf consumer is just itself"))
  (testing "unknown targets are dropped"
    (is (= #{} (transforms/downstream-closure schema [:nope])))))

(deftest scope->only-types-test
  (testing ":self (default / nil scope) = just the target"
    (is (= #{:email} (plan/scope->only-types schema "email" :self)))
    (is (= #{:email} (plan/scope->only-types schema "email" nil))))

  (testing ":with-dependencies pulls the target + upstream producers' output types"
    (is (= #{:pos-a :email} (plan/scope->only-types schema "pos-a" :with-dependencies)))
    (is (= #{:email} (plan/scope->only-types schema "email" :with-dependencies))))

  (testing ":downstream pulls the target + every consumer's output type"
    (is (= #{:email :pos-a :pos-b} (plan/scope->only-types schema "email" :downstream)))
    (is (= #{:pos-a} (plan/scope->only-types schema "pos-a" :downstream))))

  (testing "a blank/absent target ⇒ nil"
    (is (nil? (plan/scope->only-types schema "" :downstream)))
    (is (nil? (plan/scope->only-types schema nil :self)))))

(deftest several-targets-comma-separated
  (testing ":self over two targets is both of them, not one bogus keyword"
    (is (= #{:pos-a :pos-b} (plan/scope->only-types schema "pos-a,pos-b" :self))))

  (testing "whitespace around a comma is trimmed, not turned into a keyword"
    (is (= #{:pos-a :pos-b} (plan/scope->only-types schema "pos-a, pos-b" :self))))

  (testing ":with-dependencies unions the upstream of EVERY target"
    (is (= #{:pos-a :pos-b :email}
           (plan/scope->only-types schema "pos-a,pos-b" :with-dependencies))))

  (testing ":downstream unions the consumers of every target"
    (is (= #{:email :pos-a :pos-b}
           (plan/scope->only-types schema "email,pos-a" :downstream))))

  (testing "a target that resolves to no build node is still kept in the set"
    (is (contains? (plan/scope->only-types schema "pos-a,nope" :downstream) :nope)))

  (testing "empty segments from a trailing or doubled comma are ignored"
    (is (= #{:pos-a} (plan/scope->only-types schema "pos-a,," :self)))
    (is (= #{:pos-a} (plan/scope->only-types schema "pos-a," :self)))))
