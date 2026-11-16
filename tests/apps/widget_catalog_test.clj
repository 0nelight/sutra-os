(ns ^:unit apps.widget-catalog-test
  (:require [clojure.test :refer [deftest is]]
            [sutra.capps.catalog :as cat]
            [sutra.parser.ent-view :as ent-view]))

(def ^:private config-schemas @#'ent-view/widget-config-schemas)

(deftest every-typed-config-key-is-a-catalogued-param
  (let [roster (into #{} (for [{:keys [widget params]} cat/widget-specs
                               {:keys [key]} params]
                           [widget key]))]
    (is (empty? (remove roster (keys config-schemas)))
        "a schema for a param the roster lacks types nothing")))
