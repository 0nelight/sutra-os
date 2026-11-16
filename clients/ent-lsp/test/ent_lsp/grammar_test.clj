(ns ent-lsp.grammar-test
  (:require [clojure.test :refer [deftest is testing]]
            [ent-lsp.grammar :as grammar]))

(def ^:private schema
  {:objects {:kunde {:attributes [[:kunde-id :string] [:name :string]] :match-keys [:kunde-id]}}})

(deftest a-plain-schema-lowers
  (testing "so its references can be resolved"
    (is (map? (grammar/lower schema)))))
