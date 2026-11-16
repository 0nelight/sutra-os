(ns ^:api ontology.incremental-decorator-test
  (:require [support.wire :refer [ok?]]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.java.io :as io]
            [sutra.wire.api :as api]
            [sutra.compute.sandbox :as sandbox]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-pod)

(defn- py [f] (slurp (io/resource (str "support/fixtures/python/" f))))
(defn- col [n t] {:attribute-name n :attribute-type t})

(defn- mk-thing-spec
  []
  (->> (:transforms (:body (api/get! :schema)))
       vals
       (some #(when (= :mk-thing (:transform-name %)) %))))

(defn- schema []
  {:schema
   {:objects {:thing {:project :dec :symbol "🔩" :attributes [[:pk :string] [:label :string]]
                      :plural "Things"}}
    :transforms
    {:mk-thing
     {:project :dec :language :python :code (py "incr_decorated.py")
      :inputs {:seed {:kind :dataset :dataset :seed}}
      :primary-key :pk
      :output {:kind :object-type :object-type :thing}}}}})

(deftest decorator-declares-incremental
  (if-not (sandbox/available?)
    (println "  (incremental-decorator SKIPPED — python sandbox unavailable)")
    (do
      (testing "install a transform whose EDN carries NO :incremental"
        (is (ok? (api/mutate :create-project {:name :dec} {:stage? true})))
        (is (ok? (api/mutate :create-dataset
                             {:name :seed :project :dec
                              :columns [(col :tid :string) (col :label :string)]}
                             {:stage? true})))
        (is (ok? (api/post! :staging)))
        (is (ok? (api/post! :install (schema))))
        (is (ok? (api/post! :staging))))

      (testing "save-time extraction lifted :incremental out of the decorator"
        (let [spec (mk-thing-spec)]
          (is (some? spec) "transform installed")
          (is (true? (:incremental spec))
              ":incremental derived from @transform(incremental=True)")))

      (testing "…and :semantic-version with it"
        (is (= 3 (:semantic-version (mk-thing-spec)))
            ":semantic-version derived from @transform(semantic_version=3)"))

      (testing "re-installing the SAME flag-less EDN does not drop it (the drift guard)"
        (is (ok? (api/post! :install (schema))))
        (is (ok? (api/post! :staging)))
        (is (true? (:incremental (mk-thing-spec)))
            ":incremental survives a re-install — it is re-derived from the code")
        (is (= 3 (:semantic-version (mk-thing-spec)))
            ":semantic-version survives it too — same re-derivation, same guard")))))
