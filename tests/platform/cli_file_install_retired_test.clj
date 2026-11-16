(ns ^:unit platform.cli-file-install-retired-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [sutra.cli.main :as main]))

(def ^:private dead-base "http://127.0.0.1:1")

(defn- write! [name content]
  (let [f (io/file (System/getProperty "java.io.tmpdir") (str "sutra-retired-" name))]
    (spit f content)
    (.deleteOnExit f)
    (.getPath f)))

(defn- run
  [file]
  (let [sw (java.io.StringWriter.)
        code (binding [*err* sw *out* sw]
               (main/run-file! file dead-base {:yes? true :creds-path ".run/none.edn"}))]
    [code (str sw)]))

(deftest a-runbook-wrapping-an-install-is-refused
  (testing "the back door: a runbook whose steps install is the same act"
    (let [preset (write! "wrapped.ent" (pr-str {:schema {:datasets {:x {}}}}))
          [code out] (run (write! "wrap.edn"
                                  (pr-str {:title "install"
                                           :steps [[:install {:preset preset}]
                                                   [:commit]]})))]
      (is (= 1 code))
      (is (str/includes? out "retired"))
      (is (str/includes? out preset)))))
