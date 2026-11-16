(ns ^:api platform.lineage-facts-api-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [sutra.parser.authoring.standard :as standard]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api]))

(use-fixtures :once pod/with-mem-pod-auth)

(use-fixtures :each pod/as-admin)

(def ^:private schema
  {:schema {:projections standard/projections
            :projects    {:buero {}}
            :objects     {:auftrag {:primary-key :nr :attributes [[:nr :string]]
                                    :match-keys [:nr] :project :buero :plural "Aufträge"}
                          :kunde   {:primary-key :nr :attributes [[:nr :string]]
                                    :match-keys [:nr] :project :buero :plural "Kunden"}}}})

(deftest to-reaches-the-answer-over-the-wire
  (is (ok? (api/post! :install schema)))
  (is (ok? (api/post! :staging)))
  (let [r (api/get! :lineage-facts {:node "object-type/auftrag" :to "object-type/kunde"})]
    (is (ok? r) (pr-str (:body r)))
    (is (= false (get-in r [:body :path :found?])) (pr-str (:body r)))
    (is (= [] (get-in r [:body :edges])) "no way, so no edges of one"))
  (let [r (api/get! :lineage-facts {:column "nr"})]
    (is (ok? r) (pr-str (:body r)))
    (is (some? (get-in r [:body :column-note])) (pr-str (:body r)))))
