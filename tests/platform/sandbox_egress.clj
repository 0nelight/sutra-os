(ns ^:integration platform.sandbox-egress
  (:require [sutra.kit.digest :as digest]
            [sutra.parser.spec :as spec]
            [sutra.kit.read-only-sql :as read-only]
            [sutra.sources.policy :as policy]
            [sutra.sources.egress :as egress]
            [sutra.compute.external :as ext]
            [sutra.compute.sandbox :as sandbox]
            [support.helpers :refer [check section expect-error]]
            [sutra.compute.runner :as runner]))

(defn- temp-csv! []
  (let [f (java.io.File/createTempFile "sutra_test_" ".csv")]
    (spit f "name,age\nalice,30\nbob,41\n")
    (.deleteOnExit f)
    (.getAbsolutePath f)))

(defn- module
  [csv-path body]
  (str "import os, socket\n"
       "from sutra_transforms import transform, Input, Output\n"
       "@transform(inputs={'people': Input(kind='csv', path=" (pr-str csv-path) ")},\n"
       "           output=Output(kind='object-type', object_type='p', primary_key='n'))\n"
       "def go(people):\n"
       body))

(defn run-tests []
  (section "read-only SQL guard")
  (check "SELECT allowed"
         (= "SELECT * FROM t" (read-only/assert-read-only-sql! "SELECT * FROM t")))
  (check "WITH…SELECT allowed"
         (some? (read-only/assert-read-only-sql! "WITH x AS (SELECT 1) SELECT * FROM x")))
  (check "trailing ; allowed"    (some? (read-only/assert-read-only-sql! "SELECT 1;")))
  (check "DROP rejected"
         (some? (expect-error #(read-only/assert-read-only-sql! "DROP TABLE t"))))
  (check "INSERT rejected"
         (some? (expect-error #(read-only/assert-read-only-sql! "INSERT INTO t VALUES (1)"))))
  (check "DELETE rejected"
         (some? (expect-error #(read-only/assert-read-only-sql! "DELETE FROM t"))))
  (check "stacked stmt rejected"
         (some? (expect-error #(read-only/assert-read-only-sql! "SELECT 1; DROP TABLE t"))))
  (check "data-modifying CTE rejected"
         (some?
          (expect-error
           #(read-only/assert-read-only-sql!
             "WITH d AS (DELETE FROM t RETURNING *) SELECT * FROM d"))))
  (check "'drop' inside a string literal is fine"
         (some? (read-only/assert-read-only-sql! "SELECT 'please do not drop me' AS msg")))

  (section "egress allowlist")
  (check "sqlite (file) exempt"
         (some? (policy/assert-egress-allowed! {:type :sqlite :jdbc-url "jdbc:sqlite:/tmp/x.db"})))
  (check "csv exempt"
         (some? (policy/assert-egress-allowed! {:type :csv :path "/tmp/x.csv"})))
  (check "postgres without :egress rejected"
         (some? (expect-error #(policy/assert-egress-allowed!
                                {:type :postgres :jdbc-url "jdbc:postgresql://10.0.0.5:5432/db"}))))
  (check "postgres to non-allowlisted host rejected"
         (some? (expect-error #(policy/assert-egress-allowed!
                                {:type :postgres :jdbc-url "jdbc:postgresql://10.0.0.5:5432/db"
                                 :egress #{"192.168.1.1:5432"}}))))
  (check "postgres to allowlisted host:port allowed"
         (some? (policy/assert-egress-allowed!
                 {:type :postgres :jdbc-url "jdbc:postgresql://10.0.0.5:5432/db"
                  :egress #{"10.0.0.5:5432"}})))

  (section "valid-source? registration")
  (check "sqlite valid"
         (true? (boolean (spec/source-valid? {:type :sqlite :jdbc-url "jdbc:sqlite:/tmp/x.db"}))))
  (check "csv valid"      (true? (boolean (spec/source-valid? {:type :csv :path "/tmp/x.csv"}))))
  (check "postgres w/o egress invalid"
         (false? (boolean (spec/source-valid? {:type :postgres
                                               :jdbc-url "jdbc:postgresql://h:5432/db"}))))
  (check "postgres w/ egress valid"
         (true? (boolean (spec/source-valid? {:type :postgres
                                              :jdbc-url "jdbc:postgresql://h:5432/db"
                                              :egress #{"h:5432"} :access :read-only}))))
  (check "bad :access invalid"
         (false? (boolean (spec/source-valid? {:type :sqlite :jdbc-url "x" :access :bogus}))))
  (check "sftp w/ egress valid"
         (true? (boolean (spec/source-valid? {:type :sftp :host "fw" :remote-path "/x.csv"
                                              :path "/tmp/x.csv" :egress #{"fw:22"}
                                              :access :read-only}))))
  (check "sftp w/o egress invalid"
         (false? (boolean (spec/source-valid? {:type :sftp :host "fw" :remote-path "/x.csv"
                                               :path "/tmp/x.csv"}))))
  (check "command needs its command on the whitelist"
         (false? (boolean (spec/source-valid? {:type :command :host "fw" :command "cat"
                                               :path "/tmp/x.txt" :egress #{"fw:22"}
                                               :allow-commands #{"cat /etc/sutra/export.txt"}}))))

  (section "sandbox primitive")
  (check "bwrap available on this host" (true? (boolean (sandbox/available?))))
  (let [argv (sandbox/wrap-argv {:inner-cmd ["python3"] :lib-dir "/lib/dir"
                                 :module-path "/m.py" :input-paths ["/a.csv"]})]
    (check "argv cuts network"  (some #{"--unshare-all"} argv))
    (check "argv clears env"    (some #{"--clearenv"} argv))
    (check "argv binds input"   (boolean (some #{"/a.csv"} argv))))

  (when (sandbox/available?)
    (section "sandboxed transform execution")
    (let [csv (temp-csv!)]
      (let [{:keys [rows]} (runner/run-code! :python
                                             {:transform-name :t-ok
                     :code (module csv
                                   "    return [{'n': r['name'], 'a': int(r['age'])} for r in people]\n")} {:input-paths [csv]})]
        (check "CSV transform returns 2 rows" (= 2 (count rows)))
        (check "rows carry expected values"   (= #{"alice" "bob"} (set (map :n rows)))))

      (let [body (module csv "    return [{'n': r['name']} for r in people]\n")
            seen (atom nil)]
        (runner/run-code! :python {:transform-name :t-sha :code body}
                          {:input-paths [csv]
:hook {:before (fn [_])
:after  (fn [_tname _ms _rows code-sha] (reset! seen code-sha))}})
        (check "after-hook reports a code sha"      (string? @seen))
        (check "sha matches the body that ran"      (= (digest/sha256-hex body) @seen))
        (check "sha differs for a different body"
               (not= @seen (digest/sha256-hex (str body "\n# changed\n")))))

      (let [{:keys [rows]} (runner/run-code! :python
                                             {:transform-name :t-secret
                     :code (module csv
                                   "    return [{'n': r['name'], 'secret': os.environ.get('SUTRA_PG_PASSWORD','ABSENT')} for r in people]\n")} {:input-paths [csv]})]
        (check "no secret leaks into sandbox" (every? #(= "ABSENT" (:secret %)) rows)))

      (check "socket egress fails"
             (some?
              (expect-error
               #(runner/run-code! :python
                 {:transform-name :t-net
                  :code (module csv
                                "    socket.create_link(('1.1.1.1',53), timeout=2)\n    return []\n")}
                 {:input-paths [csv]}))))

      (check "undeclared file read fails"
             (some?
              (expect-error
               #(runner/run-code! :python
                 {:transform-name :t-fence
                  :code (str "from sutra_transforms import transform, Input, Output\n"
                             "@transform(inputs={'x': Input(kind='csv', path='/etc/hostname')},\n"
                             "           output=Output(kind='object-type', object_type='p', "
                             "primary_key='n'))\n"
                             "def go(x): return []\n")}
                 {:input-paths []}))))))
  (section "external transform — network sandbox profile")
  (let [net (sandbox/wrap-argv {:inner-cmd ["python3"] :lib-dir "/lib/dir"
                                :module-path "/m.py" :network true
                                :env {"SUTRA_SECRETS" "/s.json"}})]
    (check "network profile does NOT cut net" (nil? (some #{"--unshare-all"} net)))
    (check "network profile keeps pid isolation" (boolean (some #{"--unshare-pid"} net)))
    (check "network profile binds resolv.conf" (boolean (some #{"/etc/resolv.conf"} net)))
    (check "network profile injects secret-file env"
           (boolean (some (fn [[a b]] (and (= a "--setenv") (= b "SUTRA_SECRETS")))
                          (partition 2 1 net)))))

  (section "external transform — egress /secret gate")
  (check "flagged source releases its secret"
         (= {:user "u"}
            (:fields (egress/handle-secret {:spec {:external-transform true :user "u"}}))))
  (check "unflagged source refused (fail-closed)"
         (some? (expect-error #(egress/handle-secret {:spec {:user "u"}}))))

  (when (sandbox/available?)
    (section "external transform — decorator grant discovery")
    (let [ext   (str "from sutra_transforms import transform, Output\n"
                     "@transform(inputs={}, output=Output(kind='object-type', object_type='e', "
                     "primary_key='k'),\n"
                     "           external_systems=['gls-inbox'])\n"
                     "def go(): return []\n")
          plain (str "from sutra_transforms import transform, Output\n"
                     "@transform(inputs={}, output=Output(kind='object-type', object_type='e', "
                     "primary_key='k'))\n"
                     "def go(): return []\n")]
      (check "decorator grant discovered"
             (= [:gls-inbox]
                (mapv keyword
                      (:external_systems (#'ext/introspect! {:transform-name :g :code ext})))))
      (check "plain transform → no grant"
             (= []
                (mapv keyword
                      (:external_systems (#'ext/introspect! {:transform-name :p :code plain}))))))

    (section "external transform — secret injected into network jail")
    (let [sf (java.io.File/createTempFile "sutra_test_secrets_" ".json")]
      (spit sf "{\"gls-inbox\":{\"user\":\"gls@example.com\"}}")
      (.deleteOnExit sf)
      (let [{rows :rows} (ext/run-transform-local!
                          {:transform-name :t-ext
                           :code (str "from sutra_transforms import transform, Output, get_secret\n"
                                      "@transform(inputs={}, output=Output(kind='object-type', "
                                      "object_type='e', primary_key='k'))\n"
                                      "def go(): return [{'k': get_secret('gls-inbox','user')}]\n")}
                          {:secrets-path (.getAbsolutePath sf)})]
        (check "get_secret reads the injected secret in the network jail"
               (= "gls@example.com" (:k (first rows))))))

    (section "incremental transform — previous-output injection")
    (let [net (sandbox/wrap-argv {:inner-cmd ["python3"] :lib-dir "/lib/dir"
                                  :module-path "/m.py"
                                  :env {"SUTRA_PREVIOUS_OUTPUT" "/p.json"}})]
      (check "argv injects previous-output env"
             (boolean (some (fn [[a b]] (and (= a "--setenv") (= b "SUTRA_PREVIOUS_OUTPUT")))
                            (partition 2 1 net)))))
    (let [pf (java.io.File/createTempFile "sutra_test_prev_" ".json")]
      (spit pf "[{\"uid\": \"7\"}, {\"uid\": \"42\"}, {\"uid\": \"13\"}]")
      (.deleteOnExit pf)
      (let [{rows :rows} (ext/run-transform-local!
                          {:transform-name :t-incr
                           :code
                             (str
                              "from sutra_transforms import transform, Output, previous_output\n"
                              "@transform(inputs={}, output=Output(kind='object-type', "
                              "object_type='e', primary_key='k'))\n"
                              "def go():\n"
                              "    prev = previous_output()\n"
                              "    cursor = max(int(r['uid']) for r in prev) if prev else 0\n"
                              "    return [{'k': str(cursor)}]\n")}
                          {:previous-path (.getAbsolutePath pf)})]
        (check "previous_output() reads the injected prior output (cursor = max uid)"
               (= "42" (:k (first rows))))))
    (let [{rows :rows} (ext/run-transform-local!
                        {:transform-name :t-incr0
                         :code
                           (str "from sutra_transforms import transform, Output, previous_output\n"
                                "@transform(inputs={}, output=Output(kind='object-type', "
                                "object_type='e', primary_key='k'))\n"
                                "def go(): return [{'k': str(len(previous_output()))}]\n")}
                        {})]
      (check "previous_output() is [] on the first build (no SUTRA_PREVIOUS_OUTPUT)"
             (= "0" (:k (first rows))))))

  (println "\n  sandbox-egress: all checks passed"))
