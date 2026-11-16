(ns ^:unit grammar.standing-findings-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.authoring.install :as install]
            [sutra.authoring.install.plan :as install-plan]
            [support.in-project :refer [in-project]]
            [sutra.parser.project-export :as pe]))

(def ^:private action
  {:display-name "Ändern"
   :params       {:auftrag {:type :auftrag :required true}}
   :rules        [{:op :writeback-update :object-type :auftrag :target [:get :auftrag]
                   :set [:nr]}]})

(def ^:private live
  (in-project :t
   {:object-types {"ri.o.1" {:object-type-name :auftrag
                            :attribute-list [{:attribute-name :nr :attribute-type :string}]
                            :primary-key [:nr]
                             :plural "Aufträge"}}
   :actions      {:aendern action}}))

(defn- tree [a]
  (in-project :t {:objects {:auftrag {:attributes [[:nr :string]] :match-keys [:nr]
                                      :plural "Aufträge"}}
                  :actions {:aendern a}}))

(def ^:private opts {:strict? true :strict-sections (pe/carried-sections true)})

(defn- rules [t]
  (set (map :rule (install-plan/plan-problems t live (install/plan-schema t live opts)))))

(deftest a-problem-on-a-resource-the-install-leaves-as-it-is-does-not-refuse-it
  (testing "the action stood so before: re-stating it adds nothing"
    (is (not (contains? (rules (tree action)) :writeback-verb-removed))))
  (testing "changing it is judged as anything new is"
    (is (contains? (rules (tree (assoc action :display-name "Anders"))) :writeback-verb-removed))))
