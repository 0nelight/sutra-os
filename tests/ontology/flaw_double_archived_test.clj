(ns ^:api ontology.flaw-double-archived-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.store.access.streams :as streams]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api]))

(use-fixtures :once pod/with-mem-pod)

(defn- col [n t] {:attribute-name n :attribute-type t})

(def ^:private schema
  {:schema
   {:projects {:flawda {}}
    :datasets {:da_events {:project :flawda :kind :standalone
                           :columns [(col :what :string)]
                           :stream  {:partitions 1}}}}})

(defn- dataset-rid [nm]
  (some (fn [[rid d]] (when (= nm (:dataset-name d)) rid))
        (:datasets (:schema (:body (api/get! :snapshot))))))

(defn- archived []
  (count (:rows (:body (api/get! :dataset-preview {:id (dataset-rid :da_events)})))))

(deftest double-archived
  (is (ok? (api/post! :install schema)))
  (is (ok? (api/post! :staging)))
  (is (ok? (api/post! :stream-records {:name "da_events"} {:records [{:what "a"} {:what "b"}]})))
  (testing "the first archive writes its rows, then fails before it moves the cursor"
    (with-redefs [streams/set-cursors! (let [orig streams/set-cursors!]
                                         (fn [store kind & more]
                                           (if (= :archive kind)
                                             (throw
                                              (ex-info "eingeschleust: Absturz vor dem Cursor" {}))
                                             (apply orig store kind more))))]
      (is (not (ok? (api/post! :stream-archive {:name "da_events"} {})))))
    (is (= 2 (archived))))
  (testing "the repeat archives the same buffer again: same rows, no copies"
    (is (ok? (api/post! :stream-archive {:name "da_events"} {})))
    (is (= 2 (archived)))))
