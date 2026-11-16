(ns ^:api ontology.semantic-search-test
  (:require [support.wire :refer [ok?]]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.wire.api :as api]
            [support.pod :as pod]
            [support.seed :as seed]))

(use-fixtures :once pod/with-mem-pod)

(def ^:private schema
  {:projects {:semsearch {}}
   :objects {:doc {:project    :semsearch
                   :name       "Doc"
                   :primary-key :pk :attributes [[:pk :string] [:embedding [:vector 3]]]
                   :semantic-search {:property :embedding :k 5 :metric :cosine}
                   :plural "Docs"}}})

(defn- new-doc!
  [pk vec3]
  (seed/object! :doc (str "doc-" pk) {:pk pk :embedding vec3}))

(deftest semantic-search-knn
  (testing "install a type with a [:vector n] property + :semantic-search"
    (is (ok? (api/post! :install {:schema schema})) "install stages")
    (is (ok? (api/post! :staging)) "commit — reconcile accepts the vector column + declaration"))

  (testing "vectors store + round-trip through the JSON column"
    (new-doc! "east"  [1.0 0.0 0.0])
    (new-doc! "near"  [0.9 0.1 0.0])
    (new-doc! "north" [0.0 1.0 0.0])
    (new-doc! "up"    [0.0 0.0 1.0])
    (let [d (some #(when (= "east" (get-in % [:values :pk])) %)
                  (:rows (:body (api/list-objects :doc))))]
      (is (= [1.0 0.0 0.0] (mapv double (get-in d [:values :embedding])))
          "the [:vector 3] value decoded back into a numeric vector")))

  (testing "KNN ranks the type's objects nearest-first over the query vector"
    (let [r (:body (api/get! :objects-knn (merge {:type :doc} {:vector "1,0,0" :k 2})))]
      (is (= :cosine (:metric r)) "metric defaulted from the declaration")
      (is (= ["east" "near"] (map #(get-in % [:values :pk]) (:results r)))
          "exact match, then the near neighbour; orthogonal ones ranked out of the top-2")
      (is (= 2 (count (:results r))) "?k=2 honoured")
      (is (every? #(contains? % :score) (:results r)) "each result carries a similarity score")))

  (testing "a type without :semantic-search / a bad query is a clean 400"
    (is (not (ok? (api/get! :objects-knn (merge {:type :doc} {}))))
        "no query vector ⇒ 400 with guidance")))
