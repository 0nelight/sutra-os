(ns ^:unit apps.tab-visibility-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.apps.http.view :refer [prune-layout-visibility]]
            [sutra.parser.lint :as lint]))

(def ^:private rechnung "ri.sutra.main.object-type.00000000-0000-0000-0000-000000000001")
(def ^:private partner "ri.sutra.main.object-type.00000000-0000-0000-0000-000000000002")
(def ^:private link "ri.sutra.main.link-type.00000000-0000-0000-0000-000000000003")

(def ^:private schema
  {:object-types {rechnung {:object-type-name :rechnung :project "buchhaltung"}
                  partner  {:object-type-name :partner :project "stamm"}}
   :link-types   {link {:link-type-name :rechnung-partner
                        :from-object-type rechnung :to-object-type partner}}})

(defn- layout [gate]
  {:kind :tabs
   :tabs [{:id :stamm :body {:widget :markdown}}
          {:id :rechnungen :visibility {:link gate} :body {:widget :markdown}}]})

(defn- tabs [gate auth] (mapv :id (:tabs (prune-layout-visibility (layout gate) schema auth))))

(def ^:private buchhaltung
  {:operations #{} :project-operations {"buchhaltung" #{:read} "stamm" #{:read}}})

(def ^:private andere {:operations #{} :project-operations {"stamm" #{:read}}})

(deftest a-tab-follows-who-may-see-the-linked-type
  (testing "readable project, no marking: the tab stays"
    (is (= [:stamm :rechnungen] (tabs :rechnung buchhaltung))))
  (testing "the project refuses the read: the tab is gone"
    (is (= [:stamm] (tabs :rechnung andere))))
  (testing "a marking hides the type although the project allows it"
    (is (= [:stamm] (tabs :rechnung (assoc buchhaltung :hidden-object-types #{rechnung})))))
  (testing "a link type counts for both its ends"
    (is (= [:stamm :rechnungen] (tabs :rechnung-partner buchhaltung)))
    (is (= [:stamm] (tabs :rechnung-partner andere)))))

(deftest sections-and-pages-follow-the-same-rule
  (let [node {:kind :rows
              :items [{:kind :section :title "Rechnungen" :visibility {:link :rechnung}}
                      {:kind :section :title "Stamm"}]}]
    (is (= ["Stamm"] (mapv :title (:items (prune-layout-visibility node schema andere)))))))

(deftest a-gate-naming-nothing-is-reported
  (let [found (fn [gate]
                (let [rule (some #(when (= :visibility-link-unknown (:id %)) %) @#'lint/rules)]
                  ((:scan rule)
                   (assoc schema :applications
                          {"a" {:application-name :a :layout (layout gate)}}))))]
    (is (seq (found :rechnugn)))
    (is (empty? (found :rechnung)))))
