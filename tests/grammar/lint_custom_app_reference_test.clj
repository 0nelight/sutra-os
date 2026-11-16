(ns ^:unit grammar.lint-custom-app-reference-test
  (:require [clojure.test :refer [deftest is]]
            [sutra.parser.lint :as lint]))

(def ^:private ot-rid "ri.sutra.main.object-type.11111111-1111-4111-8111-111111111111")
(def ^:private act-rid "ri.sutra.main.action.22222222-2222-4222-8222-222222222222")
(def ^:private app-rid "ri.sutra.main.custom-application.33333333-3333-4333-8333-333333333333")

(defn- schema [scope extra]
  {:object-types {ot-rid {:id ot-rid :object-type-name :station}}
   :actions      {act-rid {:id act-rid :action-name :station-melden}}
   :custom-applications
   {app-rid (merge {:custom-application-name :stations-client :scope scope} extra)}})

(defn- hits [s]
  (->> (lint/run-lint s nil)
       (filter #(= :custom-app-unknown-reference (:rule %)))
       (map :entity)
       sort))

(deftest references-by-rid-or-name-resolve
  (is (empty? (hits (schema {:object-types [ot-rid :station] :actions [act-rid :station-melden]}
                            nil)))))

(deftest a-name-nothing-declares-is-an-error
  (is (= ["stations-client.gibts-nicht" "stations-client.ohne-projekt"
          "stations-client.weg"]
         (hits (schema {:object-types [:gibts-nicht] :actions [:weg]}
                       {:project :ohne-projekt})))))
