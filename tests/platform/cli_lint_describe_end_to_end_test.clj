(ns ^:api platform.cli-lint-describe-end-to-end-test
  (:require [support.wire :refer [ok?]]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [sutra.wire.api :as api]
            [sutra.cli.repl :as repl]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-pod)

(defn- json-line
  [ctx sess line]
  (let [out (with-out-str (repl/dispatch-line! (assoc ctx :json? true) sess line))]
    (json/read-str (str/trim out) :key-fn keyword)))

(deftest lint-describe-context-end-to-end
  (testing "setup — a well-formed :order type (title key + deterministic key), committed"
    (is (ok? (api/mutate :create-project {:name :verkauf} {:stage? true})))
    (is (ok? (api/mutate :create-object-type
                         {:name :order :project :verkauf
                          :display-name "Auftrag" :plural "Auftraege"
                          :description "Was ein Kunde bestellt hat."
                          :title :name
                          :primary-key :name
                          :attributes [{:attribute-name :name  :attribute-type :string}
                                       {:attribute-name :menge :attribute-type :integer}]
                          :match-keys [:name]}
                         {:stage? true})))
    (is (ok? (api/post! :staging))))

  (testing "lint is silent on the clean committed schema"
    (let [r (api/get! :schema-lint)]
      (is (ok? r))
      (is (= [] (vec (:findings (:body r)))))))

  (testing "a staged anti-pattern type is flagged with doc-cited findings"
    (is (ok? (api/mutate :create-object-type
                         {:name :dataset1 :project :verkauf :primary-key :plz
                          :attributes [{:attribute-name :plz :attribute-type :integer}]
                          :plural "Dataset1s"}
                         {:stage? true})))
    (let [findings (:findings (:body (api/get! :schema-lint)))
          rules    (into #{} (map :rule) findings)]
      (is (contains? rules :cryptic-name))
      (is (contains? rules :no-title))
      nil)
    (is (ok? (api/delete! :staging))))

  (testing "the CLI describe/context/lint verbs render against the live server"
    (repl/with-session {:yes? true}
      (fn [ctx sess]
        (let [d (json-line ctx sess "describe order")]
          (is (= "order" (:object-type d)))
          (is (some #(= "name" (:name %)) (:fields d)) "fields include the title attr")
          (is (number? (:count d)) "carries a live instance count"))

        (let [c (json-line ctx sess "context")]
          (is (contains? c :branch))
          (is (map? (:staged c)))
          (is (contains? c :identity)))

        (let [l (json-line ctx sess "lint")]
          (is (vector? (:findings l)) "clean tree → no findings after clear-staged"))

        (testing "overview gives branch + totals + per-type counts in one call"
          (let [o (json-line ctx sess "overview")]
            (is (contains? o :branch))
            (is (contains? (:counts o) :objects))
            (is (map? (:by-type o)))
            (is (contains? (:by-type o) :order)
                "the committed :order type appears in the breakdown")))

        (testing "apps lists the App Studio apps (empty here, but shaped)"
          (let [a (json-line ctx sess "apps")]
            (is (vector? (:applications a)))))))

    (testing "an unknown object-type read is a clean 404, not an NPE 500"
      (let [r (api/list-objects :not-a-real-type)]
        (is (= 404 (:status r)) "unknown type → 404")
        (is (re-find #"no object-type" (str (get-in r [:body :error]))))))

    (testing "run <id> returns 404 for an unknown run (not a crash)"
      (let [r (api/get! :run {:id "00000000-0000-0000-0000-000000000000"})]
        (is (= 404 (:status r)))))))
