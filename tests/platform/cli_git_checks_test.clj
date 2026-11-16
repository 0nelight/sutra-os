(ns ^:unit platform.cli-git-checks-test
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.test :refer [deftest is]]
            [sutra.cli.git :as git]))

(defn- clone-with-origin [url]
  (let [d (.toFile (java.nio.file.Files/createTempDirectory
                    "checks-origin" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (sh/sh "git" "init" "-q" (.getPath d))
    (sh/sh "git" "-C" (.getPath d) "remote" "add" "origin" url)
    (spit (io/file d "a.txt") "a")
    (sh/sh "git" "-C" (.getPath d) "add" "a.txt")
    (sh/sh "git" "-C" (.getPath d) "-c" "user.name=t" "-c" "user.email=t@t"
           "commit" "-q" "-m" "a")
    (.getPath d)))

(deftest checks-resolves-the-named-remote-form
  (let [dir  (clone-with-origin "http://o.example/git/controlling/controlling.git")
        seen (atom nil)]
    (with-redefs [git/with-auth      (fn [_ f] (f))
                  git/named-repo-rid (fn [repo] (when (= "controlling/controlling" repo)
                                                  "ri.sutra.main.repository.ctl"))
                  git/report-checks! (fn [_ rid sha] (reset! seen [rid sha]) {:status :passed})]
      (is (= 0 (git/checks! {} dir nil))
          "a clone of /git/<project>/<repo>.git answered `names no Sutra-OS repository`")
      (is (= "ri.sutra.main.repository.ctl" (first @seen))))))
