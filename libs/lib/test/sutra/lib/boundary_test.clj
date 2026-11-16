(ns sutra.lib.boundary-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(def platform
  #"^sutra\.(capps\.(?!derived$)|viz\.|wire\.|i18n\.|tooling\.|kit\.(pubsub|effects|cache)$|parser\.ent$|types\.llm-)")

(defn- loaded-in-fresh-jvm []
  (let [java (str (io/file (System/getProperty "java.home") "bin" "java"))
        p    (-> (ProcessBuilder.
                  [java "-cp" (System/getProperty "java.class.path") "clojure.main"
                   "-e"
                   "(require 'sutra.lib.store 'sutra.lib.dsl) (run! println (sort (map str (loaded-libs))))"])
                 (.redirectErrorStream true)
                 (.start))
        out  (slurp (.getInputStream p))]
    (assert (zero? (.waitFor p)) out)
    (filter #(str/starts-with? % "sutra.") (str/split-lines out))))

(deftest loads-no-platform-code
  (let [loaded (loaded-in-fresh-jvm)]
    (is (some #{"sutra.lib.store"} loaded))
    (is (empty? (filter #(re-find platform %) loaded)))))
