(ns ^:unit ontology.index-changelog-test
  (:require [sutra.builds.import.changelog :as changelog]
            [sutra.catalog.changes :as changes]
            [sutra.store.control.import-keys :as import-keys]
            [clojure.test :refer [deftest is testing]]))

(def ^:private decls
  [(changes/create-object "a1" :konto {:n 1})
   (changes/create-object "a2" :konto {:n 2})
   (changes/update-object "a3" :konto {:n 3})
   (changes/delete-object "a4" :konto)
   (changes/create-object "b1" :buchung {:n 1})])

(defn- ids [ds] (mapv (comp :id :row) ds))

(deftest a-type-is-indexed-from-what-its-backing-write-changed
  (testing "a known changelog keeps only the creates whose id changed"
    (is (= ["a2" "a3" "a4" "b1"] (ids (changelog/publish-changes decls {:konto #{"a2"}})))))
  (testing "an empty changelog publishes no create of that type"
    (is (= ["a3" "a4" "b1"] (ids (changelog/publish-changes decls {:konto #{}})))))
  (testing "a type without a changelog, or none at all (a full refresh), is indexed whole"
    (is (= (ids decls) (ids (changelog/publish-changes decls {:buchung #{"b1"}}))))
    (is (= (ids decls) (ids (changelog/publish-changes decls nil))))))

(deftest a-held-key-is-not-written-again
  (is (= [["konto" "b" "2"] ["konto" "c" "3"]]
         (import-keys/new-keys {"a" "1" "b" "other"}
                               [["konto" "a" "1"] ["konto" "b" "2"] ["konto" "c" "3"]])))
  (testing "a uuid is compared as the string the table holds"
    (let [u (random-uuid)]
      (is (empty? (import-keys/new-keys {"a" (str u)} [["konto" "a" u]]))))))

(deftest import-keys-follow-the-changelog
  (is (= {:konto {"k2" "a2"} :buchung {"x" "b1"}}
         (changelog/changed-keys {:konto {"k1" "a1" "k2" "a2"} :buchung {"x" "b1"}}
                                 {:konto #{"a2"}}))))
