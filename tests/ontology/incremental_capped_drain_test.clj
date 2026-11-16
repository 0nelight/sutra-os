(ns ^:api ontology.incremental-capped-drain-test
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
(defn- widget-count [] (count (:rows (:body (api/list-objects :widget)))))

(defn- observed
  [import-response]
  (->> (:jobs (:body (api/get! :run-jobs {:id (get-in import-response [:body :run-id])})))
       (keep :pending_observed)
       first))

(defn- append! [rows]
  (is (ok? (api/post! :dataset-write {:name "seed_ai"}
                      {:rows (mapv (fn [[t l]] {:values {:tid t :label l}}) rows)
:mode :append :message "seed_ai"}))))

(defn- schema []
  {:schema
   {:objects {:widget {:symbol "🧩" :primary-key :pk :attributes [[:pk :string] [:label :string]]
                       :plural "Widgets"}}
    :transforms
    {:mk-widget
     {:language :python :code (py "incr_capped.py")
      :incremental true
      :inputs {:seed {:kind :dataset :dataset :seed_ai}}
      :primary-key :pk
      :output {:kind :object-type :object-type :widget}}}}})

(deftest capped-incremental-drains-across-builds
  (if-not (sandbox/available?)
    (println "  (incremental-capped-drain SKIPPED — python sandbox unavailable)")
    (do
      (testing "install the capped incremental transform + seed 5 rows"
        (is (ok? (api/mutate :create-project {:name :capped} {:stage? true}))
            "the project it sits in")
        (is (ok? (api/mutate :create-dataset
                             {:name :seed_ai :project :capped
                              :columns [(col :tid :string) (col :label :string)]}
                             {:stage? true})))
        (is (ok? (api/post! :staging)) "commit dataset")
        (is (ok? (api/post! :install (update (schema) :schema #(in-project :capped %))))
            "install stages")
        (is (ok? (api/post! :staging)) "commit schema")
        (append! [["a" "A"] ["b" "B"] ["c" "C"] ["d" "D"] ["e" "E"]]))

      (testing "build 1 — CAP=2: only 2 of the 5 land, and the pointer is WITHHELD"
        (let [r (api/post! :import-run)]
          (is (ok? r) "import 1")
          (is (= 5 (observed r))
              "…and the body REPORTED the backlog it was handed: 5 pending, 2 taken")))

      (testing "build 2 — the STRANDING FIX: re-reads the same delta, done-filter drains the NEXT 2"
        (let [r (api/post! :import-run)]
          (is (ok? r) "import 2")
          (is (= 4 (widget-count))
              "climbed 2→4 — the un-processed rows were NOT stranded (a stranded pointer would freeze at 2)")
          (is (= 3 (observed r)) "the measurement follows the drain: 3 left")))

      (testing "build 3 — drains the last 1"
        (let [r (api/post! :import-run)]
          (is (ok? r) "import 3")
          (is (= 5 (widget-count)) "all 5 drained")
          (is (= 1 (observed r)))))

      (testing "build 4 — fully drained ⇒ no more work, pointer now advances (steady state)"
        (let [r (api/post! :import-run)]
          (is (ok? r) "import 4")
          (is (= 5 (widget-count)) "still 5 — nothing left to process")
          (is (= 0 (observed r))
              "measured ZERO — the answer 'nothing is left', which is not the same as no answer"))))))
