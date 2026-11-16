(ns ^:api ontology.flaw-unseen-staging-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api]))

(use-fixtures :once pod/with-pod)

(def ^:private csv-file "/tmp/sutra-reach/flaw_unseen_staging.csv")

(defn- col [n t] {:attribute-name n :attribute-type t})

(def ^:private schema
  {:schema
   {:projects {:flawstg {}}
    :sources  {:flawsrc {:project :flawstg :type :csv :path csv-file}}
    :datasets {:stg_out {:project :flawstg
                         :columns [(col :sku :string) (col :label :string)]}}
    :transforms
    {:stg-read {:project :flawstg
                :language :pg-sql
                :inputs {:src {:kind :datasource :datasource :flawsrc :table "csv"}}
                :query "SELECT sku AS sku, label AS label FROM src"
                :output {:kind :dataset :dataset :stg_out}}}}})

(defn- output-rows []
  (let [sc  (:body (api/get! :schema))
        rid (some (fn [[k d]] (when (= :stg_out (:dataset-name d)) k)) (:datasets sc))]
    (:rows (:body (api/get! :dataset-preview {:id (str rid)})))))

(deftest a-sql-transform-reads-its-own-staging-table
  (io/make-parents csv-file)
  (spit csv-file "sku,label\ns1,alpha\ns2,beta\n")
  (testing "a CSV source feeding an SQL transform installs"
    (let [r (api/post! :install schema)]
      (is (ok? r) (pr-str (:body r))))
    (is (ok? (api/post! :staging)) "commit"))
  (testing "the build stages the source and the transform reads the staged table"
    (let [r (api/post! :import-run)]
      (is (ok? r) (pr-str (:body r))))
    (is (= #{"s1" "s2"} (set (map #(or (get-in % [:values :sku]) (:sku %)) (output-rows))))
        "the output holds the source's rows — the staging table was visible to the read")))
