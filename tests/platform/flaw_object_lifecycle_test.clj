(ns ^:integration platform.flaw-object-lifecycle-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.ref.install :as ri]
            [sutra.ref.clock :as clock]
            [sutra.catalog.lookup :as lookup]
            [support.pod :as pod]
            [support.store :as store]))

(use-fixtures :once pod/with-mem-stores-only)

(def ^:private W
  (ri/derived {:sigma clock/system}
              {:object-types
               {:auftrag {:object-type-name :auftrag
                          :attribute-list [{:attribute-name :nr :attribute-type :string}
                                           {:attribute-name :stand :attribute-type :string}]
                          :lifecycle {:property :stand :states [:neu :frei :fertig]
                                      :transitions [{:action :freigeben :from [:neu] :to :frei}
                                                    {:action :abschliessen :from [:frei]
                                                     :to :fertig}]}
                          :plural "Aufträge"}}}))

(defn- refusal [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:store/refusal (ex-data e)))))

(deftest lifecycle-bypass
  (store/with-fresh-branch [b "flaw-lc"]
    (store/install! b W)
    (let [T (lookup/rid-of W :object-types :auftrag)]
      (testing "a create starts in the initial state"
        (is (= :lifecycle-edge
               (refusal #(store/insert-object! b T {:id "a-0" :values {:nr "0" :stand "fertig"}}))))
        (is (nil? (refusal #(store/insert-object! b T
                                                  {:id "a-1" :values {:nr "1" :stand "neu"}})))))
      (testing "a write that skips an edge is refused, whichever action makes it"
        (is (= :lifecycle-edge (refusal #(store/update-object! b T "a-1" {:stand "fertig"}))))
        (is (= "neu" (get-in (store/object b T "a-1") [:values :stand]))))
      (testing "declared edges move; other attributes write freely"
        (is (nil? (refusal #(store/update-object! b T "a-1" {:stand "frei"}))))
        (is (nil? (refusal #(store/update-object! b T "a-1" {:nr "1b"}))))
        (is (nil? (refusal #(store/update-object! b T "a-1" {:stand "fertig"}))))))))
