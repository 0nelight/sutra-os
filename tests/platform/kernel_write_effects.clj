(ns ^:integration platform.kernel-write-effects
  (:require [clojure.test :refer [deftest use-fixtures]]
            [sutra.store.access.scope :as ctx]
            [sutra.catalog.kernel :as kernel]
            [sutra.actions.operations :as ops]
            [sutra.ref.clock :as clock]
            [sutra.ref.install :as ri]
            [support.helpers :refer [check section]]
            [support.pod :as pod]
            [support.store :as store]))

(use-fixtures :once pod/with-mem-stores-only)

(defn- uuid [] (str (java.util.UUID/randomUUID)))

(def ^:private schema
  {:object-types
   {:person {:object-type-name :person
             :attribute-list [{:attribute-name :name :attribute-type :string}
                              {:attribute-name :alter :attribute-type :number}]
             :plural "Personen"}}
   :link-types {}
   :actions {:make-person
             {:rules [{:op :create :id [:get :arg0 :id]
                       :object-type :person :values [:get :arg0 :values]}]}}})

(defn- kernel-ctx []
  {:read-fn nil :actor "tester"})

(defn- submit! [branch txv] (store/submit-tx-vector! (ctx/store branch) txv))

(deftest write-path
  (let [branch (str "kwrite-" (subs (uuid) 0 8))]
    (try
      (store/fork! branch)
      (store/install! branch (ri/derived {:sigma clock/system} schema))

      (section "eval-steps creates a row the store reads back")
      (let [r (kernel/eval-steps [{:op :create :id "p1" :object-type :person
                                 :values {:name "Afra" :alter 36}}]
                               schema {} (kernel-ctx))]
        (submit! branch (:ops r)))
      (check "person row landed via the kernel write path"
             (= 1 (count (store/objects branch :person))))

      (section "execute-action (the live dispatch-action! path) writes too")
      (let [r (ops/execute-action schema {} (kernel-ctx) nil :make-person
                                  {:arg0 {:id "p2" :values {:name "Bent" :alter 7}}})]
        (submit! branch (:ops r)))
      (check "action-created person row landed"
             (= 2 (count (store/objects branch :person))))
      (finally
        (try (store/retire! branch) (catch Exception _ nil))))))
