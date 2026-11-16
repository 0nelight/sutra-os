(ns ^:unit platform.pg-pool-sizing-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.store.storage.pg]))

(def ^:private parse-pool-size #'sutra.store.storage.pg/parse-pool-size)

(deftest a-sane-setting-wins
  (testing "the point of the knob: sizing belongs to the deployment"
    (is (= 1 (parse-pool-size "1" 24)))
    (is (= 12 (parse-pool-size "12" 24)))
    (is (= 40 (parse-pool-size "40" 24)))
    (is (= 16 (parse-pool-size "  16  " 24)) "whitespace an env var picks up is not a typo")))

(deftest a-garbled-setting-degrades-to-the-default
  (testing "every way this can be wrong ends at the default — never a throw, and
            never zero. The instance boots and the operator reads the log, rather
            than the instance not booting at all."
    (doseq [bad ["" "   " "acht" "8x" "x8" "-4" "0" "3.5" "1e3"]]
      (is (= 24 (parse-pool-size bad 24))
          (str (pr-str bad) " must not become a pool size")))))

(deftest settings-are-read-from-the-configuration-alone
  (let [s (sutra.store.storage.pg/settings {"SUTRA_PG_URL" "jdbc:postgresql://h/db"
                                            "SUTRA_PG_POOL_BUILD" "3"
                                            "SUTRA_PG_CONNECT_TIMEOUT_MS" "9000"})]
    (is (= "jdbc:postgresql://h/db" (:url s)))
    (is (= "sutra" (:user s)) "what the configuration does not name falls to the default")
    (is (= 3 (get-in s [:lanes :build :max-pool-size])))
    (is (= 9000 (get-in s [:lanes :control :connect-timeout-ms])))
    (is (= 5000 (get-in s [:lanes :auth :connect-timeout-ms]))
        "the auth lane never waits longer than 5 s, whatever the configuration says"))
  (is (= 5000 (get-in (sutra.store.storage.pg/settings {}) [:lanes :auth :connect-timeout-ms])))
  (is (nil? (get-in (sutra.store.storage.pg/settings {}) [:lanes :control :connect-timeout-ms]))))
