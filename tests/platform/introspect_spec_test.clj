(ns ^:unit platform.introspect-spec-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [sutra.compute.external :as ext]
            [sutra.compute.sandbox :as sandbox]
            [sutra.parser.spec :as spec]))

(def ^:private mailbox-payload
  {:inputs           {}
   :output           nil
   :outputs          {:raw_messages    {:kind "dataset" :dataset "raw_messages"
                                        :primary_key "message_id"}
                      :raw_attachments {:kind "dataset" :dataset "raw_attachments"
                                        :primary_key "att_id"}}
   :output_order     ["raw_messages" "raw_attachments"]
   :external_systems ["mailbox"]
   :incremental      true
   :semantic_version nil})

(deftest a-job-has-a-time-limit-only-if-its-transform-states-one
  (let [argv #(sandbox/wrap-argv {:inner-cmd ["python3"] :lib-dir "/l" :module-path "/m"
                                 :limits (sandbox/limits-of % nil)})]
    (testing "a build job without a stated duration: neither wall nor cpu limit"
      (let [l (sandbox/limits-of {:build-job? true} nil)]
        (is (nil? (:wall-seconds l)))
        (is (nil? (:cpu-seconds l)))
        (is (pos? (:address-space-bytes l)) "the profile's resources stay"))
      (let [a (argv {:build-job? true})]
        (is (not-any? #{"timeout"} a))
        (is (not-any? #(str/starts-with? % "--cpu=") a))))
    (testing "stated: the wall limit it names, still no cpu limit"
      (let [l (sandbox/limits-of {:build-job? true :allowed-run-seconds 7200} nil)]
        (is (= 7200 (:wall-seconds l)))
        (is (nil? (:cpu-seconds l))))
      (is (= "7200s" (second (drop-while (complement #{"timeout"})
                                         (argv {:build-job? true :allowed-run-seconds 7200}))))))
    (testing "outside a build the limits stay: preview, test, Function"
      (is (= 60 (:wall-seconds (sandbox/limits-of {} nil))))
      (is (= 120 (:wall-seconds (sandbox/limits-of {:timeout-ms 120000} nil))))
      (is (some #{"timeout"} (argv {}))))))

(deftest a-functions-timeout-reaches-the-transform-worker
  (let [job  (atom nil)
        call #'ext/run-transform-remote!]
    (with-redefs-fn {#'ext/post-edn (fn [_ payload _ & _] (reset! job payload) {:rows []})}
      #(call "http://w" {:transform-name :f :code "" :timeout-ms 120000} {}))
    (is (= 120000 (:timeout-ms @job)))))

(deftest the-stated-duration-reaches-the-spec
  (is (= 7200 (:allowed-run-seconds (ext/introspect->spec {:inputs {} :allowed_run_seconds 7200}))))
  (is (not (contains? (ext/introspect->spec {:inputs {} :allowed_run_seconds nil})
                      :allowed-run-seconds))
      "no statement, no field: a JobSpec id does not move"))

(deftest multi-output-declaration-survives-the-mapping
  (let [{:keys [outputs] :as spec} (ext/introspect->spec mailbox-payload)]
    (testing "both outputs arrive as EDN entries, keyed and keywordized"
      (is (= [{:name :raw_messages    :kind :dataset :dataset :raw_messages
               :primary-key :message_id}
              {:name :raw_attachments :kind :dataset :dataset :raw_attachments
               :primary-key :att_id}]
             (mapv #(select-keys % [:name :kind :dataset :primary-key]) outputs))))
    (testing "the spec the save persists passes the parser's own validation —
              without this the transform installs and the planner then finds the
              datasets producerless"
      (is (spec/transform-valid?
           (merge {:transform-name :pull-mailbox :language :python :code "# py"
                   :project :beschaffung :repository :beschaffung}
                  spec))))
    (testing ":output and :outputs stay mutually exclusive"
      (is (nil? (:output spec))))))

(deftest declaration-order-is-preserved
  (testing "entry 0 is the incremental cursor — the importer exports
            `(first (:outputs compute))` as $SUTRA_PREVIOUS_OUTPUT, so a reordering
            here silently moves pull_mailbox's high-water mark to the attachments"
    (is (= [:raw_messages :raw_attachments]
           (mapv :name (:outputs (ext/introspect->spec mailbox-payload)))))
    (is (= [:raw_attachments :raw_messages]
           (mapv :name (:outputs (ext/introspect->spec
                                  (assoc mailbox-payload :output_order
                                         ["raw_attachments" "raw_messages"])))))))
  (testing "a worker too old to send `output_order` falls back to the map's own
            order rather than dropping the outputs"
    (is (= #{:raw_messages :raw_attachments}
           (set (mapv :name (:outputs (ext/introspect->spec
                                       (dissoc mailbox-payload :output_order)))))))))

(deftest single-output-path-is-unchanged
  (testing "a lone `output=` still lifts :primary-key/:column-map to the TOP level"
    (is (= {:sutra.code/entry :transform
            :inputs {:mail {:kind :dataset :dataset :raw_messages}}
            :output {:kind :object-type :object-type :email}
            :primary-key :message-id
            :semantic-version 3}
           (ext/introspect->spec
            {:inputs           {:mail {:kind "dataset" :dataset "raw_messages"}}
             :output           {:kind "object-type" :object-type "email"
                                :primary_key "message-id"}
             :outputs          nil
             :output_order     nil
             :external_systems []
             :incremental      nil
             :semantic_version 3}))))
  (testing "an undeclared multi-output emits no :outputs key at all"
    (is (not (contains? (ext/introspect->spec
                         {:inputs {:a {:kind "dataset" :dataset "d"}}
                          :output {:kind "object-type" :object-type "o"
                                   :primary_key "k"}})
                        :outputs)))))

(deftest per-output-column-map-rides-the-entry
  (testing "multi-output has no single top level to lift a column-map to, so it
            folds into the entry beside :name"
    (is (= {:name :rows :kind :object-type :object-type :thing
            :primary-key :id :column-map {:a :b}}
           (first (:outputs (ext/introspect->spec
                             {:inputs  {}
                              :outputs {:rows {:kind "object-type" :object-type "thing"
                                               :primary_key "id" :column_map {:a "b"}}}
                              :output_order ["rows"]})))))))

(deftest the-prompt-key-becomes-structure-not-a-string
  (testing "columns keep DECLARATION ORDER — the order is part of the hash"
    (is (= {:input :mail :columns [:subject :body-text] :column :prompt-hash}
           (:prompt-key (ext/introspect->spec
                         {:inputs     {:mail {:kind "dataset" :dataset "raw_messages"}}
                          :prompt_key ["mail.subject" "mail.body_text"]})))))
  (testing "an explicit stamp column overrides the default, underscores folded"
    (is (= :frage-hash
           (:column (:prompt-key (ext/introspect->spec
                                  {:inputs {:mail {:kind "dataset" :dataset "raw_messages"}}
                                   :prompt_key ["mail.subject"]
                                   :prompt_hash_column "frage_hash"}))))))
  (testing "two inputs are REFUSED — hashing a row needs a row, and two inputs have none"
    (is (thrown? Exception
                 (ext/introspect->spec {:inputs {}
                                        :prompt_key ["mail.subject" "att.filename"]}))))
  (testing "a bare column name is refused rather than searched for across the inputs"
    (is (thrown? Exception
                 (ext/introspect->spec {:inputs {} :prompt_key ["subject"]}))))
  (testing "undeclared ⇒ the key is ABSENT, not an empty map the estimate would trust"
    (is (not (contains? (ext/introspect->spec {:inputs {}}) :prompt-key)))))
