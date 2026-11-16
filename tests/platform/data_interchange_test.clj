(ns ^:unit platform.data-interchange-test
  (:require [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [sutra.builds.interchange :as ix]))

(def ^:private schema-file "boundaries/data-interchange/dataset-schema.schema.json")

(defn- render []
  (str (json/write-str (ix/json-schema) :indent true :escape-unicode false) "\n"))

(deftest the-schema-file-is-current
  (is (= (render) (slurp schema-file))
      (str schema-file " is stale — run (platform.data-interchange-test/-main)")))

(deftest the-reference-grammar-datasets-are-descriptors
  (let [datasets (get-in (edn/read-string (slurp "resources/presets/grammar.ent"))
                         [:schema :datasets])
        declared (filter (comp :columns val) datasets)]
    (is (seq declared))
    (doseq [[n ds] declared]
      (testing (str n)
        (is (nil? (ix/validate (select-keys ds [:kind :columns :primary-key]))))))))

(deftest a-parameterised-type-is-a-descriptor
  (is (nil? (ix/validate {:columns [{:attribute-name :betrag :attribute-type [:decimal 12 2]}
                                    {:attribute-name :tags :attribute-type [:array :string]}]})))
  (is (some? (ix/validate {:columns [{:attribute-name :x :attribute-type :no-such-type}]}))))

(defn -main [& _]
  (spit schema-file (render))
  (println "wrote" schema-file))
