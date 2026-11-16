(ns sutra.store.storage.git-store
  (:require [sutra.foundation.env :as env]))

(set! *warn-on-reflection* true)

(defn- dir []
  (or (env/getenv "SUTRA_GIT_DIR")
      (str (System/getProperty "user.home") java.io.File/separator ".sutra" java.io.File/separator
           "git")))

(def module
  {:name :git-store :provides {:git/dir dir}})
