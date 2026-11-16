(ns ^:unit platform.source-cipher-preserve-test
  (:require [clojure.test :refer [deftest is]]
            [sutra.types.rid :as rid]
            [sutra.catalog.kernel :as kernel]))

(deftest config-only-source-update-preserves-sealed-cipher
  (let [src-rid (rid/mint-for :sources (random-uuid))
        world {:sources {src-rid {:source-name :mailbox :id src-rid :type :imap
                                  :secret-cipher "SEALED-BLOB-abc123"
                                  :external-transform true
                                  :config {:host "192.0.2.79" :folder "INBOX" :tls true}}}}
        step     (kernel/step nil :create :source {:name :mailbox :type :imap
                                           :config {:host "192.0.2.79" :folder "INBOX"
                                                    :tls true :since "2026-06-21"}})
        prepared (kernel/prepare world step)
        v        (:value prepared)]
    (is (= "SEALED-BLOB-abc123" (:secret-cipher v))
        "the prior sealed credential is preserved (not wiped by the cipher-less update)")
    (is (= "2026-06-21" (get-in v [:config :since]))
        "the new :since config lands")
    (is (= "INBOX" (get-in v [:config :folder]))
        "and the rest of the config stays intact")))

(deftest supplied-cipher-still-overrides
  (let [src-rid  (rid/mint-for :sources (random-uuid))
        world    {:sources {src-rid {:source-name :mailbox :id src-rid :type :imap
                                     :secret-cipher "OLD" :config {:host "h"}}}}
        step     (kernel/step nil :create :source {:name :mailbox :type :imap
                                           :secret-cipher "NEW" :config {:host "h"}})
        v        (:value (kernel/prepare world step))]
    (is (= "NEW" (:secret-cipher v)) "a supplied cipher wins over the existing one")))
