(ns ^:integration platform.connectors
  (:require [sutra.sources.datasources :as ds]
            [sutra.sources.policy :as policy]
            [sutra.parser.spec :as spec]
            [support.helpers :refer [check section expect-error]]))

(def ^:private sftp-spec
  {:type :sftp :access :read-only
   :host "fw" :port 22 :user "root"
   :identity-file "/keys/id_ed25519" :known-hosts "/keys/known_hosts"
   :remote-path "/etc/sutra/export.csv" :path "/tmp/sutra-reach/fw.csv"
   :egress #{"fw:22"}})

(def ^:private command-spec
  {:type :command :access :read-only
   :host "fw" :port 22 :user "root"
   :identity-file "/keys/id_ed25519" :known-hosts "/keys/known_hosts"
   :command "cat /etc/sutra/export.txt" :allow-commands #{"cat /etc/sutra/export.txt"}
   :path "/tmp/sutra-reach/fw.txt"
   :egress #{"fw:22"}})

(defn run-tests []
  (section "valid-source? — :sftp / :command")
  (check "sftp valid" (true? (boolean (spec/source-valid? sftp-spec))))
  (check "sftp without :egress invalid"
         (false? (boolean (spec/source-valid? (dissoc sftp-spec :egress)))))
  (check "sftp without :remote-path invalid"
         (false? (boolean (spec/source-valid? (dissoc sftp-spec :remote-path)))))
  (check "sftp without :path invalid"
         (false? (boolean (spec/source-valid? (dissoc sftp-spec :path)))))
  (check "command valid" (true? (boolean (spec/source-valid? command-spec))))
  (check "command without :egress invalid"
         (false? (boolean (spec/source-valid? (dissoc command-spec :egress)))))
  (check "command without :allow-commands invalid"
         (false? (boolean (spec/source-valid? (dissoc command-spec :allow-commands)))))
  (check "command not on its own whitelist invalid"
         (false? (boolean (spec/source-valid? (assoc command-spec :command "rm -rf /")))))

  (section "valid-source? — :access capability role")
  (check ":read-write WITHOUT a :writeback allowlist is invalid"
         (false? (boolean (spec/source-valid? (assoc sftp-spec :access :read-write)))))
  (check ":read-write WITH a well-formed :writeback allowlist is valid"
         (true? (boolean (spec/source-valid?
                          (assoc sftp-spec :access :read-write
                                 :writeback {"fw_hosts" {:pk "id" :writable-columns #{"name"}
                                                         :log :none}})))))
  (check ":read-write REST source whose :webhooks are its allowlist is valid"
         (true? (boolean (spec/source-valid?
                          {:type :rest :egress ["api.x.com:443"] :access :read-write
                           :webhooks {:post {:method :post :path-template "/2/tweets"
                                             :params [:text]}}}))))
  (check ":read-write with an EMPTY :webhooks map allowlists nothing — invalid"
         (false? (boolean (spec/source-valid?
                           {:type :rest :egress ["api.x.com:443"] :access :read-write
                            :webhooks {}}))))
  (check "bogus :access invalid"
         (false? (boolean (spec/source-valid? (assoc sftp-spec :access :write-only)))))

  (section "egress — SSH endpoints (host:22)")
  (check "spec-endpoint reads :host/:port for ssh types"
         (= ["fw" 22] (policy/spec-endpoint sftp-spec)))
  (check "spec-endpoint defaults ssh port to 22"
         (= ["fw" 22] (policy/spec-endpoint (dissoc sftp-spec :port))))
  (check "sftp to allowlisted host:port allowed"
         (some? (policy/assert-egress-allowed! sftp-spec)))
  (check "sftp to non-allowlisted host rejected"
         (some?
          (expect-error #(policy/assert-egress-allowed! (assoc sftp-spec :egress #{"other:22"})))))
  (check "command without :egress rejected"
         (some? (expect-error #(policy/assert-egress-allowed! (dissoc command-spec :egress)))))

  (section "command whitelist — exact full match")
  (check "exact command allowed" (some? (policy/assert-command-allowed! command-spec)))
  (check "non-listed command rejected"
         (some?
          (expect-error #(policy/assert-command-allowed! (assoc command-spec :command "cat")))))
  (check "empty whitelist rejected"
         (some?
          (expect-error
           #(policy/assert-command-allowed! (assoc command-spec :allow-commands #{})))))
  (check "non-:command type is a no-op"
         (some? (policy/assert-command-allowed! sftp-spec)))

  (section "reach! — fail-closed, gates before any link")
  (check "reach! aborts on egress violation (no connect)"
         (some? (expect-error #(ds/connector-reach! (assoc sftp-spec :egress #{"other:22"})))))
  (check "reach! aborts on non-whitelisted command (no connect)"
         (some? (expect-error #(ds/connector-reach! (assoc command-spec :command "cat")))))
  (check "reach! requires strict known_hosts (no promiscuous fallback)"
         (some? (expect-error #(ds/connector-reach! (dissoc command-spec :known-hosts)))))
  (check "reach! requires a key (no ambient-secret auth)"
         (some? (expect-error #(ds/connector-reach! (dissoc command-spec :identity-file)))))
  (check "reach! rejects an unknown connector type"
         (some? (expect-error #(ds/connector-reach! {:type :postgres}))))

  (println "\n  connectors: all checks passed"))
