(ns ^:unit platform.tx-vector-schema-test
  (:require [sutra.store.api :as api]
            [sutra.store.tx-vector :as tv]
            [clojure.test :refer [deftest is testing]]
            [sutra.catalog.kernel :as kernel]))

(def ^:private oid #uuid "00000000-0000-0000-0000-0000000000a1")
(def ^:private lid #uuid "00000000-0000-0000-0000-0000000000b2")

(def ^:private sample-ops
  {:create-object   {:op :create-object   :type :aircraft
                     :payload {:id oid :values {:tail "OE-ABC"}}}
   :update-object   {:op :update-object   :type :aircraft
                     :payload {:id oid :values {:tail "OE-XYZ"}}}
   :update-markings {:op :update-markings :type :aircraft
                     :payload {:id oid :add [:secret] :remove [:public]}}
   :delete-object   {:op :delete-object   :type :aircraft :payload {:id oid}}
   :create-link     {:op :create-link     :type :flown-by
                     :payload {:id lid :from-id oid :to-id oid}}
   :delete-link     {:op :delete-link     :type :flown-by :payload {:id lid}}})

(deftest every-store-op-has-a-descriptor
  (is (= (set (keys sample-ops)) (set (keys api/op-descriptors)))
      "an op without a descriptor would be misrouted to the effects half"))

(deftest malformed-crud-ops-are-rejected
  (testing "a CRUD op missing its :payload key"
    (is (some? (tv/explain-tx-vector [{:op :create-object :type :aircraft}]))))
  (testing "a create-object whose payload has no :id"
    (is (some? (tv/explain-tx-vector
                [{:op :create-object :type :aircraft :payload {:values {}}}]))))
  (testing "a create-link missing an endpoint"
    (is (some? (tv/explain-tx-vector
                [{:op :create-link :type :flown-by :payload {:id lid :from-id oid}}]))))
  (testing "an unknown op that is not even an event envelope"
    (is (some? (tv/explain-tx-vector [{:not-an-op true}]))))
  (testing "the vector must be sequential, not a set"
    (is (some? (tv/explain-tx-vector #{(:delete-link sample-ops)})))))

(deftest store-op?-and-the-schema-agree-on-which-half-an-op-is-in
  (doseq [op (vals sample-ops)]
    (is (tv/store-op? op) (str (:op op) " should classify as an instance write")))
  (doseq [fx [{:op :notify} {:op :invented}]]
    (is (not (tv/store-op? fx)) (str (:op fx) " should classify as a side effect")))
  (testing "and the closed half really is closed"
    (is (not (tv/store-op? {:op :create-widget :type :x :payload {:id oid}})))))

(deftest a-tx-vector-the-kernel-actually-produced-validates
  (let [schema {:object-types {:person {:object-type-name :person
                                        :attribute-list [{:attribute-name :name
                                                          :attribute-type :string}]
                                        :plural "Personen"}}
                :link-types {} :actions {}}
        ctx    {:read-fn nil :actor "tester"}
        result (kernel/eval-steps [{:op :create :id "p1" :object-type :person
                                    :values {:name "Afra"}}]
                                  schema {} ctx)
        txv    (into (vec (:ops result)) (:side-effects result))]
    (testing "the kernel produced a write at all (guards against a vacuous pass)"
      (is (seq txv))
      (is (some #(= :create-object (:op %)) txv)))
    (testing "and it satisfies the schema"
      (is (nil? (tv/explain-tx-vector txv))
          (str "a tx-vector the kernel really produced does not match the schema "
               "that claims to describe it: " (pr-str txv))))
    (testing "the recorder's CRUD op carries a nil :markings — the schema must "
      (is (contains? (:payload (first (filter tv/store-op? txv))) :markings)))))
