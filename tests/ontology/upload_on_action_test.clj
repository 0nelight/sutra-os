(ns ^:unit ontology.upload-on-action-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.cli.verbs.imports]
            [sutra.types.upload :as upload]))

(def ^:private facts
  {:hash "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
   :filename "FIBU_2024.csv"
   :dataset "konten_uploads"})

(defn- resolve* [args] (upload/resolve-args args facts))

(deftest the-file-identifier-binds-into-action-parameters
  (testing "each placeholder resolves to its own fact"
    (is (= {:beleg (:hash facts)} (resolve* {:beleg "{{media-reference}}"})))
    (is (= {:name (:filename facts)} (resolve* {:name "{{filename}}"})))
    (is (= {:ziel (:dataset facts)} (resolve* {:ziel "{{dataset}}"}))))

  (testing "several parameters may bind, and literals pass through untouched"
    (is (= {:beleg (:hash facts) :name (:filename facts) :art "erfolgskonten"}
           (resolve* {:beleg "{{media-reference}}" :name "{{filename}}"
                      :art "erfolgskonten"}))))

  (testing "an empty arg map is not a special case"
    (is (= {} (resolve* {})))))

(deftest substitution-is-WHOLE-VALUE-only
  (testing "a placeholder inside a longer string is NOT substituted"
    (is (= {:msg "Beleg {{media-reference}} ist da"}
           (resolve* {:msg "Beleg {{media-reference}} ist da"}))))
  (testing "leading/trailing text disables it in both directions"
    (is (= {:a " {{filename}}"} (resolve* {:a " {{filename}}"})))
    (is (= {:a "{{filename}} "} (resolve* {:a "{{filename}} "})))))

(deftest an-unknown-placeholder-is-left-VERBATIM
  (is (= {:a "{{filenam}}"} (resolve* {:a "{{filenam}}"})))
  (is (= {:a "{{}}"} (resolve* {:a "{{}}"}))))

(deftest non-string-args-survive
  (is (= {:jahr 2024 :art :erfolgskonten :flags {:x true} :n nil}
         (resolve* {:jahr 2024 :art :erfolgskonten :flags {:x true} :n nil}))))

(deftest the-placeholder-vocabulary-is-closed-and-named
  (is (= #{"{{media-reference}}" "{{filename}}" "{{dataset}}"}
         (set (keys upload/placeholders))))
  (is (= #{:hash :filename :dataset}
         (set (vals upload/placeholders)))))

(deftest the-landing-line-renders-what-the-server-said
  (let [line #(str (#'sutra.cli.verbs.imports/landed-build-line "uploads" %))]
    (is (re-find #"belege-folgen" (line ["belege-folgen"])))
    (is (not (re-find #"belege-folgen" (line []))))
    (is (re-find #"--build" (line [])) "no watcher: the way to build is named")
    (is (= (line nil) (line [])))))
