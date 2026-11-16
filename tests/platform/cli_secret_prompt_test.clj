(ns ^:unit platform.cli-secret-prompt-test
  (:require [clojure.test :refer [deftest is]]
            [sutra.cli.auth :as auth]
            [sutra.cli.instance]))

(deftest fill-secrets-prompts-only-sentinels
  (let [asked (atom [])
        ask   (fn [label] (swap! asked conj label) (str label "-val"))
        out   (auth/fill-secrets
               {:mailbox {:type :imap :secrets {:username "PLAINTEXT-AT-AUTHOR-TIME-ONLY"
                                                :password "already-set"}}}
               ask)]
    (is (= ["mailbox.username"] @asked))
    (is (= "mailbox.username-val" (get-in out [:mailbox :secrets :username])))
    (is (= "already-set"          (get-in out [:mailbox :secrets :password])))))

(deftest seal-skips-sentinel-and-preserves-existing-credential
  (let [out (auth/seal-schema-sources
             {}
             {:sources {:mailbox {:type    :imap
                                  :config  {:host "192.0.2.79" :folder "INBOX"
                                            :tls true :since "2026-06-21"}
                                  :secrets {:username "PLAINTEXT-AT-AUTHOR-TIME-ONLY"
                                            :password "PLAINTEXT-AT-AUTHOR-TIME-ONLY"}}}})
        src (get-in out [:sources :mailbox])]
    (is (nil? (:secrets src))       "skipped sentinel secrets are dropped, not sealed")
    (is (nil? (:secret-cipher src)) "no cipher shipped ⇒ merge-existing preserves the live one")
    (is (= "2026-06-21" (get-in src [:config :since]))
        "the non-secret :config (incl :since) survives")
    (is (true? (:external-transform src)) "the descriptor still opts into worker secret-release")))

(deftest a-connector-source-is-not-marked-external-transform
  (with-redefs [sutra.cli.instance/facts
                (constantly {:source-types {:imap {:label "IMAP (connector)"
                                                   :fields [{:key :host :required true}]
                                                   :egress-from {:host-port [:host :port]}}}})]
    (let [src (get-in (auth/seal-schema-sources
                       {}
                       {:sources {:mailbox {:type :imap :host "imap.example.com" :port 993}}})
                      [:sources :mailbox])]
      (is (nil? (:external-transform src))
          "a plugin's form names no :capability :external-transform")
      (is (= ["imap.example.com:993"] (:egress src))
          "its egress comes from its top-level fields"))))
