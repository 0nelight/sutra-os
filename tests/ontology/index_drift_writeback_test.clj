(ns ^:api ontology.index-drift-writeback-test
  (:require [support.wire :refer [ok?]]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.store.access.datasets :as dsets]
            [sutra.store.access.scope :as ctx]
            [sutra.wire.api :as api]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-pod)

(defn- col [n t] {:attribute-name n :attribute-type t})

(defn- drift-result [type-name]
  (->> (:results (:body (api/post! :health-run-all)))
       (some #(when (= (str "index-drift:" (name type-name)) (:check-id %)) %))))

(def ^:private schema
  {:schema
   {:objects {:gadget {:project :wb :symbol "⚙️" :primary-key :pk
                       :attributes [[:pk :string] [:label :string]]
                       :plural "Gadgets"}}
    :transforms
    {:mk-gadget {:project :wb :language :clojure
                 :code "(fn [{:keys [src]}] (for [r src] {:pk (:tid r) :label (:label r)}))"
                 :inputs {:src {:kind :dataset :dataset :src}}
                 :primary-key :pk
                 :output {:kind :object-type :object-type :gadget}}}}})

(deftest index-drift-ignores-legitimate-writeback
  (testing "a source-backed object type, built"
    (is (ok? (api/mutate :create-project {:name :wb} {:stage? true})))
    (is (ok? (api/mutate :create-dataset
                         {:name :src :project :wb
                          :columns [(col :tid :string) (col :label :string)]}
                         {:stage? true})))
    (is (ok? (api/post! :staging)) "commit dataset")
    (is (ok? (api/post! :install schema)) "install stages")
    (is (ok? (api/post! :staging)) "commit schema")
    (is (ok? (api/post! :dataset-write {:name "src"} {:rows [{:values {:tid "t1" :label "alpha"}}]
                                       :mode :snapshot :message "seed"})))
    (is (ok? (api/post! :import-run)) "build"))

  (testing "index matches its producer ⇒ passed"
    (let [r (drift-result :gadget)]
      (is (some? r) "the built-in drift check reports on this type")
      (is (= "passed" (:status r)))))

  (testing "a WRITEBACK object makes the row counts differ — and that is not drift"
    (is (ok? (api/mutate :create-action
                         {:name :gadget-anlegen :project :wb
                          :params {:pk    {:name "Pk" :type :string :required true}
                                   :label {:name "Label" :type :string :required true}}
                          :rules [{:op :create :object-type :gadget
                                   :values {:pk    [:get :arg0 :pk]
                                            :label [:get :arg0 :label]}}]
                          :edits [{:object-type :gadget}]}
                         {:stage? true}))
        "author the Action Type")
    (is (ok? (api/post! :staging)) "commit the action")
    (is (ok? (api/mutate :gadget-anlegen {:pk "w1" :label "user-made"}))
        "an Action creates an object no producer backs")
    (let [r (drift-result :gadget)]
      (is (= "passed" (:status r))
          "still passed — the check asks the changelog, not the row counts")
      (is (= "0" (:value r))
          "and reports zero instances a reindex would drop")))

  (testing "a REAL orphan is still caught — the check did not become a no-op"
    (let [sc      (:body (api/get! :schema))
          out-rid (->> (:transforms sc) vals
                       (some #(when (= :mk-gadget (:transform-name %))
                                (get-in % [:output :dataset]))))
          ds-name (name (:dataset-name (get (:datasets sc) out-rid)))]
      (is (some? ds-name) "found the backing dataset behind :gadget")
      (is (ok? (api/post! :dataset-write {:name ds-name}
                          {:rows [] :mode :snapshot :message "wipe backing"}))
          "empty the backing table out of band")
      (let [r (drift-result :gadget)]
        (is (= "failed" (:status r)) "drift is reported")
        (is (= "1" (:value r))
            "one instance would be dropped — the source-backed row, not the edited one")
        (is (re-find #"reindex gadget" (str (:detail r)))
            "and the detail names the remedy")))))

(def ^:private starved-schema
  {:schema
   {:objects
    {:widget {:project :starved :symbol "🧩" :primary-key :pk
              :attributes [[:pk :string] [:n :decimal]]
              :checks [{:name "nicht-negativ" :expr [:>= :n 0] :inputs [:n]}]
              :plural "Widgets"}}
    :transforms
    {:mk-widget {:project :starved :language :clojure
                 :code "(fn [{:keys [wsrc]}] (for [r wsrc] {:pk (:tid r) :n (:amount r)}))"
                 :inputs {:wsrc {:kind :dataset :dataset :wsrc}}
                 :primary-key :pk
                 :output {:kind :object-type :object-type :widget}}}}})

(defn- backing-dataset
  [t]
  (->> (:transforms (:body (api/get! :schema))) vals
       (some #(when (= t (:transform-name %)) (get-in % [:output :dataset])))))

(deftest index-drift-catches-a-starved-index
  (testing "a source-backed object type, built"
    (is (ok? (api/mutate :create-project {:name :starved} {:stage? true})))
    (is (ok? (api/mutate :create-dataset
                         {:name :wsrc :project :starved
                          :columns [(col :tid :string) (col :amount [:decimal 5 2])]}
                         {:stage? true})))
    (is (ok? (api/post! :staging)) "commit dataset")
    (is (ok? (api/post! :install starved-schema)) "install stages")
    (is (ok? (api/post! :staging)) "commit schema")
    (is (ok? (api/post! :dataset-write {:name "wsrc"} {:rows [{:values {:tid "t1" :amount "1.00"}}]
                                        :mode :snapshot :message "seed"})))
    (is (ok? (api/post! :import-run)) "build"))

  (testing "a build whose publish group is isolated fails — and gives its backing dataset
            back as it was at build-low, so index and backing still agree"
    (is (ok? (api/post! :dataset-write {:name "wsrc"} {:rows [{:values {:tid "t1" :amount "1.00"}}
                                                              {:values {:tid "t2" :amount "-1.00"}}]
                                        :mode :snapshot :message "poison"})))
    (let [imp (api/post! :import-run)]
      (is (= 500 (:status imp))
          "the build FAILS on an isolated publish failure")
      (is (seq (get-in imp [:body :data :publish-failures]))
          "and it says so in the response"))
    (let [r (drift-result :widget)]
      (is (= "passed" (:status r)) "no drift: the failed publish retracted its backing write")
      (is (re-find #"1 widget object\(s\) vs 1 row\(s\)" (str (:detail r))))))

  (testing "the index is EMPTY of a row its producer backs — that is drift"
    (let [ds (backing-dataset :mk-widget)]
      (is (some? ds) "found the backing dataset behind :widget")
      (dsets/write-rows! (ctx/system-store "main") ds :append
                         [{:id (str (random-uuid)) :values {:pk "t2" :n "-1.00"}}])
      (let [r (drift-result :widget)]
        (is (= "failed" (:status r))
            "reported, where the orphan-only check answered passed on 0-vs-1224")
        (is (= "1" (:value r)) "one row the producer backs is not indexed")
        (is (re-find #"NOT indexed" (str (:detail r))))
        (is (re-find #"run-import --only widget" (str (:detail r)))
            "and the detail names the remedy — republish, not reindex"))))

  (is (ok? (api/post! :dataset-write {:name "wsrc"}
                      {:rows [] :mode :snapshot :message "un-poison"}))
      "clear the unpublishable row so later builds in this pod are green again"))
