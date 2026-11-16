(ns ^:unit grammar.release-conflict-test
  (:require [clojure.test :refer [deftest is]]
            [sutra.catalog.functions :as functions]
            [sutra.types.versions :as vers]))

(deftest a-release-stored-with-its-status-still-matches
  (let [live {:releases {"1.0.0" {:body "x" :status :published}}}]
    (is (nil? (functions/release-conflict live "1.0.0"
                                     (vers/snapshot
                                      {:body "x" :status :published :version "1.0.0"})))
        "a template published before :status joined the metadata keys is not a changed body")
    (is (some? (functions/release-conflict live "1.0.0" (vers/snapshot {:body "y" :version "1.0.0"}))))))
