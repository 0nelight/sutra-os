(ns ^:unit platform.stage-decisions-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.authoring.stage]))

(def ^:private checked #'sutra.authoring.stage/checked)
(def ^:private preview-answer #'sutra.authoring.stage/preview-answer)

(defn- untimed [_ f] (f))

(deftest a-schema-that-cannot-be-staged-is-refused-as-data
  (testing "not a map"
    (is (= :refusal.authoring/install-needs-schema
           (first (:refusal (checked {} {} {:schema "x" :preview? true} untimed))))))
  (testing "a repository the schema does not know"
    (is (= [:refusal.authoring/install-no-repository {:repository "nope"}]
           (:refusal (checked {} {} {:schema {} :repository "nope"} untimed)))))
  (testing "a schema that passes every check is handed on lowered"
    (let [{:keys [refusal] :as r} (checked {} {} {:schema {}} untimed)]
      (is (nil? refusal))
      (is (contains? r :lowered)))))

(deftest what-a-preview-answers-is-decided-from-what-it-read
  (let [reads {:tally {:creates 1} :staged [:row]}]
    (is (= {:creates 1 :preview? true :staged [:row]} (preview-answer reads nil)))
    (testing "a strict install names its retirements, all repositories without one"
      (is (= :all (:strict-repository
                   (preview-answer (assoc reads :retire {:retiring [:t]}) nil)))))
    (testing "plan problems refuse it, each named"
      (let [a (preview-answer (assoc reads :refused [{:entity :t :message "m"}] :planned [:p])
                              nil)]
        (is (:refused? a))
        (is (= {:plan [{:name "t" :error "m"}]} (:problems a)))))
    (testing "kernel problems count only when the before is known"
      (is (= {:k [1]}
             (:problems (preview-answer (assoc reads :reasons-before {} :reasons-after {:k [1]})
                                        nil))))
      (is (nil? (:problems
                 (preview-answer (assoc reads
                                        :reasons-before :sutra.authoring.stage/unknown
                                        :reasons-after {:k [1]})
                                 nil)))))
    (testing "only new lint warnings are advisories"
      (let [w {:rule :r :entity :e :message "m"}]
        (is (nil? (:advisories
                   (preview-answer (assoc reads :warnings-before [w] :warnings-after [w]) nil))))
        (is (= [w] (:advisories (preview-answer (assoc reads :warnings-after [w]) nil))))))))
