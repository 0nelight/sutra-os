(ns ^:unit platform.spnego-config-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.sso.spnego :as spnego]
            [sutra.access.http.identity :as idp]
            [support.mem :as mem]))

(def ^:private fake-keytab-b64 "BQIAAABTAAEAC0VYQU1QTEUuQ09N")

(def ^:private provider (get-in spnego/module [:contributes :identity/providers :spnego]))

(deftest spnego-config-roundtrips-and-gates
  (let [store     (mem/store)
        cfg    #(idp/config store provider)
        status #(idp/status store provider)]
    (testing "config is unset ⇒ not ready, handshake unavailable"
      (is (false? (spnego/ready? (cfg))))
      (is (nil? (spnego/accept-negotiate (cfg) "AAAA")) "accept no-ops while not ready"))
    (testing "a partial config (enabled but no keytab) is NOT ready"
      (idp/set-config! store provider {:realm "EXAMPLE.COM" :kdc "dc1.example.com"
                                    :spn "HTTP/sutra.example.com@EXAMPLE.COM" :enabled true})
      (is (false? (spnego/ready? (cfg))) "missing keytab ⇒ not ready ⇒ password path still")
      (is (= "EXAMPLE.COM" (:realm (status))))
      (is (false? (:keytab? (status)))))
    (testing "a corrupt keytab is rejected at set time (fail loud, not at login)"
      (is (thrown? clojure.lang.ExceptionInfo
                   (idp/set-config! store provider {:keytab-b64 "not valid base64 !!!"}))))
    (testing "a complete config ⇒ ready; the keytab is reported only as a boolean"
      (idp/set-config! store provider {:keytab-b64 fake-keytab-b64 :enabled true})
      (is (true? (spnego/ready? (cfg))))
      (is (true? (:keytab? (status))))
      (is (not-any? #{:keytab :keytab-b64} (keys (status))) "raw keytab never echoed"))
    (testing "disabling flips readiness off without dropping the stored fields"
      (idp/set-config! store provider {:enabled false})
      (is (false? (spnego/ready? (cfg))))
      (is (= "EXAMPLE.COM" (:realm (status))) "fields retained"))))
