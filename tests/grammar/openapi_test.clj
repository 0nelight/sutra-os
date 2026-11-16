(ns ^:unit grammar.openapi-test
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk]
            [sutra.http.openapi :as openapi]
            [sutra.types.paths :as paths]))

(defn- refs [doc]
  (let [acc (atom #{})]
    (walk/postwalk (fn [v] (when (and (map? v) (string? (get v "$ref")))
                             (swap! acc conj (get v "$ref")))
                     v)
                   doc)
    @acc))

(defn- read-json [s] (json/read-str s))

(deftest the-openapi-document-is-current
  (is (= (openapi/render (openapi/document)) (slurp openapi/openapi-file))
      (str openapi/openapi-file " is stale — run (sutra.http.openapi/-main)")))

(deftest the-operations-schema-is-current
  (is (= (openapi/render (openapi/operations)) (slurp openapi/operations-file))
      (str openapi/operations-file " is stale — run (sutra.http.openapi/-main)")))

(deftest the-openapi-document-holds-together
  (let [doc     (read-json (openapi/render (openapi/document)))
        ops     (for [[_ item] (get doc "paths") [_ op] item] op)
        schemas (set (keys (get-in doc ["components" "schemas"])))]
    (is (= "3.1.0" (get doc "openapi")))
    (testing "every path has an operation"
      (is (= [] (vec (for [[p item] (get doc "paths") :when (empty? item)] p)))))
    (testing "every endpoint method is an operation"
      (is (= (count (for [[_ r] paths/endpoints m (keys (:methods r))] m)) (count ops))))
    (testing "operation ids are unique"
      (is (= [] (vec (for [[id n] (frequencies (map #(get % "operationId") ops))
                           :when (< 1 n)]
                       id)))))
    (testing "every reference resolves"
      (is (= #{} (set (remove #(schemas (subs % (count "#/components/schemas/")))
                              (refs doc))))))))

(deftest the-operations-schema-holds-together
  (let [doc  (read-json (openapi/render (openapi/operations)))
        defs (set (keys (get doc "$defs")))]
    (is (seq (get doc "x-sutra-operations")))
    (is (= #{} (set (remove #(defs (subs % (count "#/$defs/"))) (refs doc)))))))
