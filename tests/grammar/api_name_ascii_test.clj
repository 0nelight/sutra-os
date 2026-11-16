(ns ^:unit grammar.api-name-ascii-test
  (:require [clojure.test :refer [deftest is]]
            [sutra.types.column :as column]
            [clojure.string :as str]
            [sutra.parser.ent :as ent]
            [sutra.types.api-name :as api-name]))

(deftest the-refusal-names-the-transliteration
  (is (re-find #"Write :groesse-test\." (api-name/problem :grösse-test)))
  (is (re-find #"Replace é\." (api-name/problem :café)))
  (is (nil? (api-name/problem :groesse-test)))
  (is (nil? (api-name/problem :offen?))))

(deftest display-names-stay-free
  (is (nil? (ent/explain
             {:schema {:objects {:arbeitsauftrag {:name "Größe"
                                            :attributes [[:faellig-am :date {:name "Fällig am"}]]
                                                  :plural "Größen"}}
                       :links [[:arbeitsauftrag-folgeauftrag :arbeitsauftrag :arbeitsauftrag
                                :many-to-one
                                {:forward-name "Folgeaufträge"}]]}}))))

(deftest the-suggestion-is-the-column-the-fold-mints
  (doseq [a [:grösse :straße :prüfung-nötig :ø-preis]]
    (is (= (name (column/attr->col a))
           (str/replace (api-name/transliterate (name a)) "-" "_")))))
