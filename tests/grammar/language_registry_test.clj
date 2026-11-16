(ns ^:unit grammar.language-registry-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.ref.install :as ref-install]
            [sutra.types.languages :as langs]))

(deftest the-kernel-knows-the-languages-of-the-registry
  (is (= (set (keys langs/compute-languages)) ref-install/compute-languages)))

(deftest every-language-declares-what-its-readers-derive
  (doseq [[lang entry] langs/compute-languages]
    (testing lang
      (is (string? (:label entry)))
      (is (string? (:ext entry)))
      (is (#{:always :when-present} (:export-inputs entry))))))

(deftest an-alias-names-a-language-of-the-registry
  (doseq [[lang {:keys [alias-of]}] langs/compute-languages
          :when alias-of]
    (testing lang
      (is (contains? langs/compute-languages alias-of))
      (is (nil? (:alias-of (langs/compute-languages alias-of)))))))

(deftest a-selection-offers-every-language-but-an-alias
  (is (= (set (remove (comp :alias-of langs/compute-languages) (keys langs/compute-languages)))
         (set langs/selectable-languages)))
  (is (apply distinct? langs/selectable-languages)))

(deftest the-default-query-language-is-a-selectable-query-language
  (is (langs/query-language? langs/default-query-language))
  (is (some #{langs/default-query-language} langs/selectable-languages)))
