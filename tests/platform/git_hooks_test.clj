(ns ^:unit platform.git-hooks-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [sutra.tooling.git-hooks :as hooks]))

(defn- tmp-dir []
  (let [d (io/file (System/getProperty "java.io.tmpdir")
                   (str "hooks-" (System/nanoTime)))]
    (.mkdirs d)
    (.getPath d)))

(deftest install-says-what-it-actually-did
  (let [d (tmp-dir)]
    (is (= :written (hooks/install! d "/a/bin/change")) "not there before")
    (is (= :unverändert (hooks/install! d "/a/bin/change")) "byte-identical")
    (testing "a DIFFERENT client path rewrites the file, and says so"
      (is (= :aktualisiert (hooks/install! d "/b/bin/change")))
      (is (str/includes? (slurp (io/file d "pre-push")) "/b/bin/change")
          "the file really did change — the old answer claimed it had not"))))

(deftest a-foreign-hook-is-never-clobbered
  (let [d (tmp-dir)]
    (spit (io/file d "pre-push") "#!/bin/sh\n# somebody else's\n")
    (is (= :fremd (hooks/install! d "/a/bin/change")))
    (is (str/includes? (slurp (io/file d "pre-push")) "somebody else's")))
  (is (= :kein-ort (hooks/install! nil "/a/bin/change"))))

(deftest the-client-is-resolved-at-run-time
  (testing "the baked path is a HINT — an override wins, PATH is the fallback"
    (let [b (hooks/body "/baked/bin/change")]
      (is (str/includes? b "${SUTRA_CHANGE_BIN:-/baked/bin/change}"))
      (is (str/includes? b "command -v sutra-change"))))
  (testing "and finding nothing exits 0, never 1"
    (is (str/includes? (hooks/body "/x") "[ -x \"$CHANGE\" ] || exit 0"))))
