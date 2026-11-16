(ns ^:unit grammar.retire-candidate-referrer-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.authoring.install.live :as live]
            [sutra.authoring.install.findings.retire :as retire]))

(defn- ix [W] (live/world-index W))

(defn- ot [n] (str "ri.sutra.main.object-type." (name n)))

(def ^:private ids [{:attribute-name :id :attribute-type :string}])

(def ^:private live
  {:object-types {(ot :mangel)           {:object-type-name :mangel :attribute-list ids
                                          :plural "Mängel"}
                  (ot :quittieren-log) {:object-type-name :quittieren-log :attribute-list ids}}
   :actions {:quittieren {:rules [{:op :update :object-type (ot :mangel) :target [:get :r]}
                                  {:op :create :object-type (ot :quittieren-log)
                                   :values {:log-id [:get :log-id]}}]}}})

(def ^:private log-goes
  {:kind :object-type :entity :quittieren-log :outcome :deleted})

(defn- referenced [plan]
  (filter #(= :retire-referenced (:rule %)) (retire/retire-problems live plan (ix live))))

(deftest a-referrer-the-push-rewrites-is-judged-by-what-it-becomes
  (testing "the action drops :action-log in the same push — nothing points at the log any more"
    (is (empty? (referenced
                 [log-goes
                  {:kind :action :entity :quittieren :outcome :updated
                   :args {:name :quittieren
                          :rules [{:op :update :object-type (ot :mangel) :target [:get :r]}]}}]))))
  (testing "a rewrite that STILL creates the log keeps the block"
    (is (= 1 (count (referenced
                     [log-goes
                      {:kind :action :entity :quittieren :outcome :updated
                       :args {:name :quittieren :description "neu"
                              :rules [{:op :create :object-type (ot :quittieren-log)}]}}])))))
  (testing "a referrer the push leaves alone is judged live, as before"
    (let [[p] (referenced [log-goes])]
      (is (= ["action quittieren"] (:referrers p)))
      (is (re-find #"still point" (:message p))))))
