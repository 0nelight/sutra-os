(ns ^:api platform.build-profile-test
  (:require [support.wire :refer [ok?]]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.java.io :as io]
            [sutra.wire.api :as api]
            [sutra.compute.sandbox :as sandbox]
            [support.in-project :refer [in-project]]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-pod)

(defn- py [f] (slurp (io/resource (str "support/fixtures/python/" f))))

(defn- col [n t] {:attribute-name n :attribute-type t})

(defn- schema []
  {:schema
   {:transforms
    {:producer
     {:language :python :code (py "dataset_producer.py")
      :inputs  {:seed {:kind :dataset :dataset :seed}}
      :outputs [{:kind :dataset :name :raw_messages :dataset :raw_messages
                 :primary-key :message_id}
                {:kind :dataset :name :raw_attachments :dataset :raw_attachments
                 :primary-key :filename}]}}}})

(defn- dataset! [nm cols]
  (is (ok? (api/mutate :create-dataset {:name nm :project :profiled :columns cols}
                       {:stage? true}))))

(defn- run-of [body]
  (:body (api/get! :run {:id (:run-id body)})))

(deftest a-profiled-build-says-where-its-time-went
  (if-not (sandbox/available?)
    (println "  (build-profile SKIPPED — python sandbox unavailable)")
    (do
      (is (ok? (api/mutate :create-project {:name :profiled} {:stage? true})))
      (dataset! :seed [(col :message_id :string) (col :betreff :string) (col :n_att :string)])
      (dataset! :raw_messages [(col :message_id :string) (col :betreff :string)])
      (dataset! :raw_attachments [(col :message_id :string) (col :filename :string)
                                  (col :media_ref :media-reference)])
      (is (ok? (api/post! :staging)))
      (is (ok? (api/post! :install (update (schema) :schema #(in-project :profiled %)))))
      (is (ok? (api/post! :staging)))
      (is (ok? (api/post! :dataset-write {:name "seed"}
                          {:rows [{:values {:message_id "m1" :betreff "x" :n_att "1"}}]
                           :message "seed"})))
      (let [resp (api/post! :import-run {:only [:producer] :profile true})
            run  (run-of (:body resp))
            {:keys [phases profile]} (:progress run)]
        (is (ok? resp))
        (testing "the time in the queue is a phase of the run"
          (is (some #(= "queued" (name (:phase %))) phases)
              (pr-str phases)))
        (testing "the profile outlives the run, on its row"
          (is (pos? (:samples profile 0)) (pr-str (keys profile)))
          (is (pos? (:thread-ms profile 0)))
          (is (some #(re-find #"^sutra\.builds\.import/" (:frame %)) (:inclusive profile))
              "the build's own frames, not the pool's"))
        (testing "a job says how long reading its input signatures took"
          (let [jobs (:jobs (:body (api/get! :run-jobs {:id (:run-id (:body resp))})))]
            (is (some (fn [j] (some #(= "input-sigs" (:name %)) (:job_spans j))) jobs)
                (pr-str (map :job_spans jobs))))))
      (testing "without the flag a run carries no profile"
        (let [resp (api/post! :import-run {:only [:producer]})]
          (is (nil? (get-in (run-of (:body resp)) [:progress :profile]))))))))
