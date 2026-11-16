(ns ^:unit apps.client-ir-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.serve.server :as server]
            [sutra.types.paths :as paths]))

(deftest address-takes-path-params-and-declared-query-keys-from-one-map
  (is (= "/api/runs" (paths/address :runs)))
  (is (= "/api/objects/luftfahrzeug?limit=5"
         (paths/address :objects-of {:type :luftfahrzeug :limit 5 :offset nil})))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"undeclared query param"
                        (paths/address :runs {:kind :build :foo 1})))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"missing path param"
                        (paths/address :run {})))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"missing path param"
                        (paths/address :run))))

(deftest ir-joins-both-halves-of-every-endpoint
  (let [endpoints (:endpoints (server/ir))]
    (is (= (set (keys paths/paths)) (set (keys endpoints))))
    (doseq [[id e] endpoints
            :when (contains? (server/endpoint-facts) id)]
      (testing (str id)
        (is (= (paths/paths id) (:path e)))
        (is (= (paths/params-of (:path e)) (:params e [])))
        (is (seq (:methods e)))
        (is (keyword? (:auth e)))
        (is (keyword? (:service e)))))))

(deftest declared-query-and-encoding-reach-the-ir
  (let [endpoints (:endpoints (server/ir))]
    (doseq [[id q] paths/query]
      (is (= (vec (sort (keys q))) (:query (endpoints id))) (str id)))
    (doseq [[id enc] paths/encoding]
      (is (= enc (:encoding (endpoints id))) (str id)))))
