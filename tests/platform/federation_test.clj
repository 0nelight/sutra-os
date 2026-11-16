(ns ^:unit platform.federation-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.access.federation :as fed]
            [support.mem :as mem]))

(defn- trust [nm other & {:as m}]
  (merge {:name nm :url (str "https://" nm ".example")
          :public-key (:public-key (fed/identity-of other))}
         m))

(deftest an-assertion-crosses-one-declared-trust
  (let [home (mem/store) away (mem/store) third (mem/store) now 1000]
    (fed/declare-trust! home "admin" (trust "away" away :outbound? true))
    (fed/declare-trust! away "admin" (trust "home" home :inbound? true
                                            :subjects {"alice" "alice-local"}))
    (let [{a :assertion url :url} (fed/assertion home "away" "alice" now)
          v (fed/verify away a now)]
      (is (= "https://away.example" url))
      (is (= {:trust "home" :subject "alice" :local "alice-local"}
             (select-keys v [:trust :subject :local])))
      (testing "single use"
        (is (true? (fed/spend! away (:claims v) now)))
        (is (false? (fed/spend! away (:claims v) now))))
      (testing "expired"
        (is (= "expired" (:refused (fed/verify away a (+ now fed/assertion-ttl-ms))))))
      (testing "addressed to another instance"
        (fed/declare-trust! third "admin" (trust "home" home :inbound? true))
        (is (= "addressed to another instance" (:refused (fed/verify third a now)))))
      (testing "a tampered assertion"
        (is (= "malformed assertion" (:refused (fed/verify away (str "x" a) now)))))
      (testing "revoked"
        (is (true? (fed/revoke-trust! away "home")))
        (is (= "no trust for this issuer" (:refused (fed/verify away a now))))))))

(deftest only-outbound-trusts-issue-and-only-inbound-accept
  (let [home (mem/store) away (mem/store)]
    (fed/declare-trust! home "admin" (trust "away" away))
    (is (nil? (fed/assertion home "away" "alice" 0)))
    (is (= [] (fed/instances home)))
    (fed/declare-trust! home "admin" (trust "away" away :outbound? true))
    (fed/declare-trust! away "admin" (trust "home" home))
    (is (= "the trust does not accept logins"
           (:refused (fed/verify away (:assertion (fed/assertion home "away" "alice" 0)) 0))))
    (is (= [{:name "away" :url "https://away.example"}] (fed/instances home)))))

(deftest a-trust-is-checked-before-it-is-kept
  (let [home (mem/store) away (mem/store)]
    (is (thrown? clojure.lang.ExceptionInfo
                 (fed/declare-trust! home "admin" (trust "Away!" away))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (fed/declare-trust! home "admin" (trust "away" away :url "ftp://x"))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (fed/declare-trust! home "admin" (trust "self" home))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (fed/declare-trust! home "admin" (assoc (trust "away" away)
                                                         :public-key "AAAA"))))
    (is (empty? (fed/trusts home)))))

(deftest a-trust-reads-what-cli-and-json-send
  (let [home (mem/store) away (mem/store)
        row  (fed/declare-trust! home "admin"
                                 (trust "away" away :inbound? "true"
                                        :subjects "alice=alice-local, bob=b"))]
    (is (true? (:inbound? row)))
    (is (false? (:outbound? row)))
    (is (= {"alice" "alice-local" "bob" "b"} (:subjects row)))
    (is (= {"carol" "c"}
           (:subjects (fed/declare-trust! home "admin"
                                          (trust "away" away :subjects {:carol "c"})))))))
