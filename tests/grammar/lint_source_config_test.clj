(ns ^:unit grammar.lint-source-config-test
  (:require [sutra.parser.spec :as spec]
            [clojure.test :refer [deftest testing is]]
            [sutra.parser.lint :as lint]
            [sutra.parser.lint.source :as lint-source]))

(deftest code-in-data-trap-is-an-error
  (testing "a symbol-headed list config value (the `(str …)` trap) is a hard error"
    (let [fs (lint-source/lint-source-config
              spec/default-source-types
              [{:name "mailbox" :type :imap
                :config {:host "imap.x.com"
                         :include (list 'str "shared/a" "shared/b")}}])
          inc (first (filter #(= "mailbox.include" (:entity %)) fs))]
      (is (some? inc) "the :include code-in-data value is flagged")
      (is (= :error (:severity inc)) "and it is an ERROR (it fails at run time)")
      (is (= :sources (:entity-class inc)))))

  (testing "a plain string / vector value is NOT flagged as code-in-data"
    (let [fs (lint-source/lint-source-config
              spec/default-source-types
              [{:name "mailbox" :type :imap
                :config {:host "imap.x.com" :password "sealed"
                         :include "shared/a,shared/b"
                         :tags ["a" "b"]}}])]
      (is (empty? (filter #(= :error (:severity %)) fs))
          "no false positive on a correctly-authored config"))))

(deftest descriptor-field-checks
  (testing "a blank required non-secret descriptor field warns"
    (let [fs (lint-source/lint-source-config spec/default-source-types
                                      [{:name "api" :type :rest :config {:api-key "k"}}])
          bu (first (filter #(= "api.base-url" (:entity %)) fs))]
      (is (some? bu) ":base-url (required) is flagged when missing")
      (is (= :warn (:severity bu)))))

  (testing "a numeric descriptor field carrying a non-numeric value warns"
    (let [fs (lint-source/lint-source-config
              spec/default-source-types
              [{:name "mailbox" :type :imap
                :config {:host "imap.x.com" :password "sealed" :port "not-a-number"}}])
          p (first (filter #(= "mailbox.port" (:entity %)) fs))]
      (is (some? p) ":port declared numeric but non-numeric is flagged")
      (is (= :warn (:severity p)))))

  (testing "a numeric string in a numeric field is fine"
    (let [fs (lint-source/lint-source-config
              spec/default-source-types
              [{:name "mailbox" :type :imap
                :config {:host "imap.x.com" :password "sealed" :port "993"}}])]
      (is (empty? (filter #(= "mailbox.port" (:entity %)) fs))
          "\"993\" is an acceptable numeric value")))

  (testing "an unknown source type gets no descriptor findings (only code-in-data)"
    (let [fs (lint-source/lint-source-config spec/default-source-types
                                      [{:name "x" :type :some-custom :config {:whatever "v"}}])]
      (is (empty? fs) "no descriptor → no required/type findings, and no code-in-data here"))))

(deftest run-lint-preserves-per-finding-severity
  (testing "mixed severities survive the run-lint stamp"
    (let [schema   {:sources {:s1 {:source-name "mailbox" :type :imap
                                   :config {:host "imap.x.com"
                                            :include (list 'str "a" "b")
                                            :port "abc"}}}}
          findings (lint/run-lint schema (spec/vocabulary))
          sc       (filter #(= :source-config (:rule %)) findings)]
      (is (seq sc) "the source-config rule fired")
      (is (contains? (set (map :severity sc)) :error) "the code-in-data error survived")
      (is (contains? (set (map :severity sc)) :warn)  "the numeric warn survived")
      nil)))

(deftest a-required-field-may-sit-beside-config
  (is (empty? (filter #(= "mailbox.host" (:entity %))
                      (lint-source/lint-source-config
                       spec/default-source-types
                       [{:name "mailbox" :type :imap :host "imap.x.com"
                         :config {:folder "INBOX"}}])))
      "a connector's field given at the top of the source is not reported missing"))
