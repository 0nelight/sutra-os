(ns sutra.coord.conformance
  (:require [clojure.test :refer [is testing]]
            [sutra.coord.api :as coord]))

(set! *warn-on-reflection* true)

(defn check-leases [make]
  (testing "one live holder per name; epochs rise on takeover, not on renewal"
    (let [co (make)
          a  (coord/acquire! co {:name "x" :owner "a" :ttl-ms 60000})]
      (is (= "a" (:owner a)))
      (is (nil? (coord/acquire! co {:name "x" :owner "b" :ttl-ms 60000}))
          "a live lease is not taken")
      (is (= (:epoch a) (:epoch (coord/acquire! co {:name "x" :owner "a" :ttl-ms 60000})))
          "re-acquiring keeps the epoch")
      (is (= (:epoch a) (:epoch (coord/renew! co a 60000))))
      (is (= "a" (:owner (coord/holder co "x"))))
      (is (true? (coord/release! co a)))
      (is (nil? (coord/holder co "x")))
      (let [b (coord/acquire! co {:name "x" :owner "b" :ttl-ms 60000})]
        (is (> (:epoch b) (:epoch a)) "a new holder gets a higher epoch")
        (is (nil? (coord/renew! co a 60000)) "a former holder cannot renew")
        (is (false? (coord/release! co a)) "nor release"))))
  (testing "a lapsed lease is taken over"
    (let [co (make)
          a  (coord/acquire! co {:name "y" :owner "a" :ttl-ms 1})]
      (Thread/sleep 20)
      (is (nil? (coord/holder co "y")))
      (let [b (coord/acquire! co {:name "y" :owner "b" :ttl-ms 60000})]
        (is (= "b" (:owner b)))
        (is (> (:epoch b) (:epoch a))))))
  (testing "acquire waits up to :wait-ms"
    (let [co (make)]
      (coord/acquire! co {:name "z" :owner "a" :ttl-ms 50})
      (is (= "b"
             (:owner (coord/acquire! co {:name "z" :owner "b" :ttl-ms 60000 :wait-ms 2000})))))))

(defn check-revoke [make]
  (testing "revoke! takes a live lease away and raises the epoch"
    (let [co (make)
          a  (coord/acquire! co {:name "r" :owner "a" :ttl-ms 60000})
          e  (coord/revoke! co "r")]
      (is (> e (:epoch a)))
      (is (nil? (coord/holder co "r")))
      (is (nil? (coord/renew! co a 60000)))
      (is (> (:epoch (coord/acquire! co {:name "r" :owner "b" :ttl-ms 60000})) e)
          "the next holder's epoch is above the revoked one"))))

(defn check-all [make]
  (check-leases make)
  (check-revoke make))
