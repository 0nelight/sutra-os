(ns ^:unit platform.cli-noninteractive-gates-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [sutra.i18n.core :as i18n]
            [sutra.cli.out :as out]
            [sutra.cli.repl :as repl]))

(defn- declined-line [] (i18n/t :cli/declined-no-terminal))
(defn- no-terminal-phrase
  []
  (second (str/split (i18n/t :cli/repl-no-terminal {:zweig "main" :rat ""}) #" — ")))

(defn- with-stdin
  [s f]
  (let [in (java.io.BufferedReader. (java.io.StringReader. s))]
    (binding [*in* in]
      (let [r (f)]
        [r (str/join "\n" (line-seq in))]))))

(defn- captured
  [f]
  (let [w (java.io.StringWriter.)]
    (binding [*out* w *err* w] (let [r (f)] [r (str w)]))))

(deftest a-gate-with-no-terminal-declines-instead-of-reading-commands
  (let [[[answer out] left]
        (with-stdin "transform delete something\nbranch checkout main\n"
          #(captured (fn [] (out/prompt-yes? {} "delete the world?"))))]
    (testing "it says no"
      (is (false? answer)))
    (testing "it says WHY, and how to mean it"
      (is (str/includes? out "delete the world?"))
      (is (str/includes? out (declined-line)))
      (is (str/includes? out "--yes")))
    (testing "and the batch's next commands are still there to run"
      (is (str/includes? left "transform delete something"))
      (is (str/includes? left "branch checkout main")))))

(deftest an-empty-stdin-does-not-hang
  (let [[[answer _] _] (with-stdin "" #(captured (fn [] (out/prompt-yes? {} "go?"))))]
    (is (false? answer))))

(deftest --yes-still-passes-and-still-reads-nothing
  (let [[[answer out] left]
        (with-stdin "next command\n"
          #(captured (fn [] (out/prompt-yes? {:yes? true} "go?"))))]
    (is (true? answer))
    (is (str/includes? out (i18n/t :cli/auto-yes)))
    (is (str/includes? left "next command")
        "an auto-yes must not eat a line either")))

(deftest the-typed-confirm-for-main-holds-the-same-line
  (let [[[answer out] left]
        (with-stdin "main\n" #(captured (fn [] (#'repl/typed-confirm? {} "main"))))]
    (is (false? answer) "a line in the batch is not a person typing")
    (is (str/includes? out (no-terminal-phrase)))
    (is (str/includes? left "main") "…and it was not consumed"))
  (testing "--yes still passes it"
    (is (true? (#'repl/typed-confirm? {:yes? true} "main")))))
