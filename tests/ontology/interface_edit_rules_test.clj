(ns ^:unit ontology.interface-edit-rules-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.catalog.kernel :as kernel]))

(def ^:private world
  {:interfaces
   {:ticket {:interface-name :ticket
             :property-list [{:attribute-name :ticket-id :attribute-type :string}
                             {:attribute-name :title     :attribute-type :string}]}}
   :object-types
   {:bug     {:object-type-name :bug
              :implements [:ticket]
              :attribute-list [{:attribute-name :ticket-id :attribute-type :string}
                               {:attribute-name :title     :attribute-type :string}
                               {:attribute-name :severity  :attribute-type :string}]
              :plural "Bugs"}
    :feature {:object-type-name :feature
              :implements [:ticket]
              :attribute-list [{:attribute-name :ticket-id :attribute-type :string}
                               {:attribute-name :title     :attribute-type :string}]
              :plural "Features"}
    :invoice {:object-type-name :invoice
              :attribute-list [{:attribute-name :nr :attribute-type :string}]
              :plural "Invoices"}}
   :transforms
   {:bug-src {:transform-name :bug-src
              :output {:kind :object-type :object-type :bug}
              :primary-key :ticket-id}}
   :link-types {}})

(defn- logged [res] (mapv (juxt :op :type :payload) (:ops res)))

(defn- run
  ([rule] (run rule {}))
  ([rule bindings]
   (let [res (kernel/eval-step rule world bindings {})]
     [(logged res) res])))

(defn- fails [rule bindings]
  (try (run rule bindings) nil
       (catch Exception e (or (ex-message e) (str e)))))

(def ^:private a-bug {:id "b1" :object-type-ref :bug :values {}})

(deftest update-of-interface-writes-through-to-the-concrete-type
  (let [[log] (run {:op :update-of-interface :interface :ticket
                    :target [:get :b :id] :values {:title "renamed"}}
                   {:b a-bug})
        [verb t payload] (first log)]
    (is (= :update-object verb) "the ordinary update handler did the write")
    (is (= :bug t) "…on the implementer the reference resolved to, not the interface")
    (is (= {:title "renamed"} (:values payload)))))

(deftest a-type-specific-property-is-refused
  (let [msg (fails {:op :update-of-interface :interface :ticket
                    :target [:get :b :id] :values {:severity "high"}}
                   {:b a-bug})]
    (is (some? msg) "severity is :bug's own, not :ticket's")
    (is (re-find #"severity" msg) "the offending property is named")
    (is (re-find #"ticket" msg) "and so is the contract it is not part of")))

(deftest the-primary-key-is-refused-per-implementer
  (testing ""
    (let [msg (fails {:op :update-of-interface :interface :ticket
                      :target [:get :b :id] :values {:ticket-id "T-2"}}
                     {:b a-bug})]
      (is (some? msg))
      nil))
  (testing ":feature has no producer, so it has no key to protect — the rule
            proceeds rather than inventing a failure"
    (let [[log] (run {:op :update-of-interface :interface :ticket
                      :target [:get :f :id] :values {:ticket-id "T-2"}}
                     {:f {:id "f1" :object-type-ref :feature :values {}}})]
      (is (= :feature (second (first log)))))))

(deftest a-non-implementer-is-refused-and-the-alternatives-are-named
  (let [msg (fails {:op :update-of-interface :interface :ticket
                    :target [:get :i :id] :values {:title "x"}}
                   {:i {:id "i1" :object-type-ref :invoice :values {}}})]
    (is (some? msg))
    (is (re-find #"invoice" msg) "the offending type is named")
    (is (and (re-find #"bug" msg) (re-find #"feature" msg))
        "and the types that DO implement it are listed")))

(deftest an-interface-that-is-not-one-is-refused
  (is (re-find #"not an interface"
               (fails {:op :update-of-interface :interface :bug
                       :target [:get :b :id] :values {:title "x"}}
                      {:b a-bug}))))

(deftest create-of-interface-needs-the-implementers-primary-key
  (testing "'objects cannot be created without a primary key'"
    (let [msg (fails {:op :create-of-interface :interface :ticket
                      :object-type :bug :values {:title "new"}} {})]
      (is (some? msg))
      (is (re-find #"ticket-id" msg) "the key it is missing is named")))
  (testing "with the key present it creates through the ordinary handler"
    (let [[log] (run {:op :create-of-interface :interface :ticket
                      :object-type :bug
                      :values {:ticket-id "T-1" :title "new"}} {})
          [verb t payload] (first log)]
      (is (= [:create-object :bug] [verb t]))
      (is (= {:ticket-id "T-1" :title "new"} (:values payload)))))
  (testing "the object type may arrive as a PARAMETER"
    (let [[log] (run {:op :create-of-interface :interface :ticket
                      :object-type [:get :arg0 :ot]
                      :values {:ticket-id "T-9" :title "p"}}
                     {:arg0 {:ot :bug}})]
      (is (= :bug (second (first log)))))))

(deftest delete-of-interface-deletes-the-concrete-object
  (let [[log] (run {:op :delete-of-interface :interface :ticket
                    :target [:get :b :id]} {:b a-bug})]
    (is (= [:delete-object :bug] (take 2 (first log)))))
  (is (some? (fails {:op :delete-of-interface :interface :ticket
                     :target [:get :i :id]}
                    {:i {:id "i1" :object-type-ref :invoice :values {}}}))
      "membership is still guarded when nothing is written"))
