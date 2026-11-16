(ns ^:api ontology.producer-replacement-test
  (:require [sutra.parser.spec :as spec]
            [support.wire :refer [ok?]]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.parser.lint :as lint]
            [sutra.wire.api :as api]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-pod)

(defn- live-schema [] (:body (api/get! :schema)))

(defn- thing-sync-tx
  []
  (->> (:transforms (live-schema))
       vals
       (some #(when (= :thing_raw-sync (:transform-name %)) %))))

(def ^:private seed-schema
  {:schema {:projects {:prodrepl {}}
           :datasets {:seed {:project :prodrepl
                              :kind :standalone
                              :columns [{:attribute-name :pk :attribute-type :string}
                                        {:attribute-name :label :attribute-type :string}]}}}})

(def ^:private datasource-schema
  {:schema
   {:projects {:prodrepl {}}
    :sources {:ds {:project :prodrepl :type :sqlite
                   :jdbc-url "jdbc:sqlite:/tmp/sutra_prodrepl.sqlite"}}
    :datasets {:thing_raw {:project :prodrepl
                           :kind :standalone
                           :columns [{:attribute-name :pk :attribute-type :string}
                                     {:attribute-name :label :attribute-type :string}]
                           :external-source {:datasource :ds :table "thing"
                                             :primary-key :pk}}}}})

(def ^:private keyless-schema
  (assoc-in datasource-schema
            [:schema :datasets :thing_raw :external-source]
            {:datasource :ds :table "thing"}))

(def ^:private compute-schema
  {:schema
   {:projects {:prodrepl {}}
    :datasets {:thing_raw {:project :prodrepl
                           :kind :standalone
                           :columns [{:attribute-name :pk :attribute-type :string}
                                     {:attribute-name :label :attribute-type :string}]
                           :external-source
                           {:language :sqlite-sql :primary-key :pk
                            :inputs {:seed {:kind :dataset :dataset :seed}}
                            :query "SELECT pk AS pk, label AS label FROM seed"}}}}})

(defn- install! [schema] (and (ok? (api/post! :install schema)) (ok? (api/post! :staging))))

(deftest backing-mode-flip-clears-ghost-fields
  (testing "seed dataset + datasource-synced :thing_raw"
    (is (install! seed-schema))
    (is (install! datasource-schema))
    (let [t (thing-sync-tx)]
      (is (some? t) "the dataset's :external-source lifted to a <name>-sync producer")
      (is (= :dataset (get-in t [:output :kind])) "which outputs the DATASET, not an object type")
      (is (= :datasource (get-in t [:input :kind])) "starts as a datasource sync")))

  (testing "FLIP datasource → compute (the arbeitszeit→zeitbuchung scenario)"
    (is (install! compute-schema) "the flip reconciles cleanly")
    (let [t (thing-sync-tx)]
      (is (= :sqlite-sql (:language t)) "now compute")
      (is (= "SELECT pk AS pk, label AS label FROM seed" (:query t)) "with its program")
      (is (some? (:inputs t))    "and its :inputs")
      (is (nil? (:input t))      "no datasource :input ghost")
      (is (nil? (:source t))     "no datasource :source ghost")
      (is (= :dataset (get-in t [:output :kind])) "still producing the same dataset")))

  (testing "FLIP compute → datasource (reverse direction stays clean too)"
    (is (install! datasource-schema) "the reverse flip reconciles cleanly")
    (let [t (thing-sync-tx)]
      (is (= :datasource (get-in t [:input :kind])) "datasource sync again")
      (is (nil? (:language t))   "no compute :language ghost")
      (is (nil? (:query t))      "no compute :query ghost")
      (is (nil? (:inputs t))     "no compute :inputs ghost")))

  (testing "RETIRE the sync key by re-declaring without one (wartungs_satz_raw)"
    (is (install! keyless-schema) "a keyless :external-source installs")
    (let [t (thing-sync-tx)]
      (is (nil? (:primary-key t)) "the key is RETIRED, not omit-preserved")
      (is (= :datasource (get-in t [:input :kind])) "still the same sync")
      (is (= "thing" (get-in t [:source :table])) "with its binding intact")))

  (testing "and re-declaring WITH a key sets it again"
    (is (install! datasource-schema))
    (is (= :pk (:primary-key (thing-sync-tx)))
        "a named key wins — the whole-grain case, not a one-way door"))

  (testing "and nothing this test authored crosses the layer line"
    (is (empty? (->> (lint/run-lint (live-schema) (spec/vocabulary))
                     (filter #(= :source-syncs-into-ontology (:rule %)))))
        "no datasource is synced straight into an object type")))
