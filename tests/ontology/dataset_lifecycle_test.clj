(ns ^:api ontology.dataset-lifecycle-test
  (:require [support.wire :refer [ok?]]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.java.io :as io]
            [sutra.wire.api :as api]
            [sutra.compute.sandbox :as sandbox]
            [support.in-project :refer [in-project]]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-pod)

(defn- py [f] (slurp (io/resource (str "support/fixtures/python/" f))))
(defn- objs [t] (:rows (:body (api/list-objects t))))
(defn- val-of [o k] (get-in o [:values k]))

(defn- col [n t] {:attribute-name n :attribute-type t})

(defn- dataset-rid [nm]
  (->> (:datasets (:schema (:body (api/get! :snapshot))))
       (some (fn [[rid d]] (when (= nm (:dataset-name d)) rid)))))

(defn- schema []
  {:schema
   {:objects
    {:message    {:symbol "✉️" :attributes [[:pk :string] [:betreff :string]] :plural "Messages"}
     :attachment {:symbol "📎" :attributes [[:pk :string] [:filename :string]
                                            [:nbytes :integer]]
                  :plural "Attachments"}}
    :transforms
    {     :producer
     {:language :python :code (py "dataset_producer.py")
      :inputs  {:seed {:kind :dataset :dataset :seed}}
      :outputs [{:kind :dataset :name :raw_messages    :dataset :raw_messages
                 :primary-key :message_id}
                {:kind :dataset :name :raw_attachments :dataset :raw_attachments
                 :primary-key :filename}]}
     :msg-obj
     {:language :python :code (py "message_from_dataset.py")
      :inputs {:msgs {:kind :dataset :dataset :raw_messages}}
      :primary-key :pk
      :output {:kind :object-type :object-type :message}}
     :att-obj
     {:language :python :code (py "attachment_from_media.py")
      :inputs {:atts {:kind :media-set :dataset :raw_attachments}}
      :primary-key :pk
      :output {:kind :object-type :object-type :attachment}}}}})

(deftest dataset-source-to-object
  (if-not (sandbox/available?)
    (println "  (dataset-lifecycle SKIPPED — python sandbox unavailable)")
    (do
      (testing "create the three standalone datasets (seed + two raw landing sets)"
        (is (ok? (api/mutate :create-project {:name :lifecycle} {:stage? true}))
            "the project it sits in")
        (is (ok? (api/mutate :create-dataset
                             {:name :seed :project :lifecycle :columns [(col :message_id :string)
                                                    (col :betreff :string)
                                                    (col :n_att :string)]}
                             {:stage? true})))
        (is (ok? (api/mutate :create-dataset
                             {:name :raw_messages :project :lifecycle
                              :columns [(col :message_id :string)
                    (col :betreff :string)]}
                             {:stage? true})))
        (is (ok? (api/mutate :create-dataset
                             {:name :raw_attachments :project :lifecycle
                              :columns [(col :message_id :string) (col :filename :string)
                                        (col :media_ref :media-reference)]}
                             {:stage? true})))
        (is (ok? (api/post! :staging)) "commit datasets")
        (is (some? (dataset-rid :raw_messages)) "raw_messages is in the schema"))

      (testing "install the producer + two consumers"
        (is (ok? (api/post! :install (update (schema) :schema #(in-project :lifecycle %))))
            "install stages")
        (is (ok? (api/post! :staging)) "commit schema")
        (is (= 3 (count (:transforms (:body (api/get! :schema))))) "three transforms"))

      (testing "seed two messages (m1 → 2 attachments, m2 → 1)"
        (is (ok? (api/post! :dataset-write {:name "seed"}
                            {:rows [{:values {:message_id "m1" :betreff "Hallo" :n_att "2"}}
              {:values {:message_id "m2" :betreff "Welt"  :n_att "1"}}]
       :message "seed"}))))

      (testing "run-import: producer lands datasets, consumers read them"
        (is (ok? (api/post! :import-run)) "import ran"))

      (testing "the producer landed both datasets with independent cardinality"
        (is (= 2
               (count (:rows (:body (api/get! :dataset-preview
                                              {:id (dataset-rid :raw_messages)})))))
            "raw_messages: 1 row per message")
        (is (= 3
               (count (:rows (:body (api/get! :dataset-preview
                                              {:id (dataset-rid :raw_attachments)})))))
            "raw_attachments: N rows per message (2 + 1)"))

      (testing ":dataset INPUT — message objects mapped from the landed dataset"
        (is (= 2 (count (objs :message))) "two :message objects")
        (is (some #(= "Hallo" (val-of % :betreff)) (objs :message)) "row values carried through"))

      (testing ":media-set INPUT — bytes streamed from the CAS inside the sandbox"
        (let [as (objs :attachment)]
          (is (= 3 (count as)) "one :attachment per media item")
          (is (every? #(pos? (val-of % :nbytes)) as)
              "every attachment read a positive byte-count → the blob bytes reached the transform"))))))
