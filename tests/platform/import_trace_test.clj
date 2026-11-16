(ns ^:api platform.import-trace-test
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
   {:objects {:widget {:symbol "🧩" :primary-key :pk :attributes [[:pk :string] [:label :string]]
                       :plural "Widgets"}
              :thing  {:symbol "🔧" :attributes [[:pk :string] [:label :string]] :plural "Things"}}
    :transforms
    {:mk-widget
     {:language :python :code (py "incr_added.py")
      :incremental true
      :inputs {:seed {:kind :dataset :dataset :seed_ai}}
      :primary-key :pk
      :output {:kind :object-type :object-type :widget}}
     :skip-thing
     {:language :python :code (py "incr_skip.py")
      :incremental true
      :inputs {:seed {:kind :dataset :dataset :seed_ai}}
      :primary-key :pk
      :output {:kind :object-type :object-type :thing}}}}})

(deftest trace-switch-round-trips-the-read-path-diagnostic
  (if-not (sandbox/available?)
    (println "  (trace-switch SKIPPED — python sandbox unavailable)")
    (do
      (is (ok? (api/mutate :create-project {:name :trace} {:stage? true}))
          "the project it sits in")
      (is (ok? (api/mutate :create-dataset
                           {:name :seed_ai :project :trace
                            :columns [(col :tid :string) (col :label :string)]}
                           {:stage? true})))
      (is (ok? (api/post! :staging)))
      (is (ok? (api/post! :install (update (schema) :schema #(in-project :trace %)))))
      (is (ok? (api/post! :staging)))
      (is (ok? (api/post! :dataset-write {:name "seed_ai"}
                          {:rows [{:values {:tid "a" :label "alpha"}}
         {:values {:tid "b" :label "beta"}}]
  :mode :append :message "seed"})))

      (testing "untraced build ⇒ NO :trace in the response (opt-in)"
        (let [b (:body (api/post! :import-run))]
          (is (nil? (:trace b)) "trace absent unless requested")))

      (is (ok? (api/post! :dataset-write {:name "seed_ai"}
                          {:rows [{:values {:tid "c" :label "gamma"}}]
  :mode :append :message "seed c"})))

      (testing "traced build ⇒ :trace carries the mk-widget read-path entry"
        (let [b     (:body (api/post! :import-run {:trace true}))
              trace (:trace b)
              read  (first (filter #(and (= :dataset-read (:kind %))
                                         (= :mk-widget (:consumer %))) trace))
              prev  (first (filter #(and (= :prev-output (:kind %))
                                         (= :mk-widget (:consumer %))) trace))
              py    (first (filter #(and (= :code (:kind %))
                                         (= :skip-thing (:consumer %))) trace))]
          (is (seq trace) "trace present when requested")
          (is (some? read) "a dataset-read entry for the mk-widget consumer")
          (is (= :seed_ai (:input read)) "input resolved to the dataset NAME")
          (is (true? (:incremental? read)) "flagged incremental")
          (is (contains? read :rows) "carries the input row count")
          (is (some? prev) "a previous_output entry for the incremental consumer")
          (is (contains? prev :done-rows) "carries the done-set size")
          (testing "code-execution entry: the runner's transform-internal breadcrumbs + data fingerprint"
            (is (some? py) "a :code entry for the previous_output()-driven consumer")
            (let [in-ev   (first (filter #(= "input" (:event %)) (:events py)))
                  prev-ev (first (filter #(= "previous_output" (:event %)) (:events py)))]
              (is (some? prev-ev) "carries the previous_output() breadcrumb the runner recorded")
              (is (= 2 (:rows prev-ev)) "previous_output fingerprint counts the 2 prior rows")
              (is (seq (:key_sample prev-ev))
                  "previous_output fingerprint samples the KEY column (the done-set the input is filtered against)")
              (is (some? in-ev)
                  "carries an INPUT fingerprint (the compute-side data the read-path count can't show)")
              (is (some #{"tid"} (:cols in-ev))
                  "input fingerprint lists the ACTUAL columns — so 'is the key column present?' is answerable in the trace"))))))))
