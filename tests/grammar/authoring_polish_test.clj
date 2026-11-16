(ns ^:unit grammar.authoring-polish-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [sutra.authoring.install.plan :as plan]
            [sutra.cli.projection :as projection]
            [sutra.parser.canonical-print :as cp]
            [sutra.types.review :as review]))

(def ^:private with-revision (var-get #'plan/with-revision))

(def ^:private action
  {:name :x :display-name "X" :rules [{:op :update :object-type :t}]})

(def ^:private live
  (-> action (dissoc :name) (assoc :action-name :x :revision 13)))

(deftest a-revision-in-a-text-is-neither-a-change-nor-a-value
  (testing "unchanged stays at the store's count, whatever the text states"
    (is (= 13 (:revision (with-revision action :x live))))
    (is (= 13 (:revision (with-revision (assoc action :revision 13) :x live))))
    (is (= 13 (:revision (with-revision (assoc action :revision 2) :x live)))))
  (testing "a real change adds one to the store's count"
    (is (= 14 (:revision (with-revision (assoc action :display-name "Y" :revision 2)
                           :x live)))))
  (testing "a new action starts at one"
    (is (= 1 (:revision (with-revision action :x nil))))))

(deftest a-revision-alone-breaks-nothing
  (is (= :accretion (review/change-kind "modified" {:a 1 :revision 12} {:a 1 :revision 13})))
  (is (= :breaking (review/change-kind "modified" {:a 1 :revision 12} {:a 2 :revision 13}))))

(deftest a-node-prints-what-it-is-first
  (let [out (cp/canonical-ent-str {:aa 1 :text "t" :tone :info :when {:item :x}
                                   :widget :badge})]
    (is (< (str/index-of out ":widget") (str/index-of out ":when")
           (str/index-of out ":aa") (str/index-of out ":text")))))

(deftest a-preview-names-what-it-would-change
  (is (= ["    update action: a, b" "    update application: v"]
         (projection/change-lines
          [{:verb "update" :type "application" :type-name "v"}
           {:verb "update" :type "action" :type-name "b"}
           {:verb "update" :type "action" :type-name "a"}])))
  (testing "a row of a real plan names its node in its args"
    (is (= ["    update application: eingangsdokument-view-full"]
           (projection/change-lines
            [{:verb "update" :type "application"
              :args {:application-name "eingangsdokument-view-full"}}])))
    (is (= ["    update action: x"]
           (projection/change-lines [{:verb "update" :type "action" :args {:name :x}}])))))
