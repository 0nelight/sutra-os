(ns ^:unit grammar.flaw-releases-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.authoring.install :as install]))

(defn- publish
  [desired-entry live-entry]
  (->> (install/plan-schema {:functions
                               {:escalate
                                  (merge {:target-type :ticket :language :clojure} desired-entry)}}
                            {:functions
                               {"ri.w.1" (merge {:function-name :escalate :target-type :ticket
                                                 :language :clojure} live-entry)}}
                            {})
       (filter #(= :edit-functions (:phase %)))
       first))

(defn- refusal [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (.getMessage e))))

(def ^:private released
  {:source "V12" :language :clojure :target-type :ticket
   :param-types {:ticket :ticket}})

(deftest major-break
  (let [live (merge released {:version "1.2.0" :releases {"1.2.0" released}})
        lower {:source "V11" :version "1.1.0" :param-types {:ticket :ticket :note :string}}]
    (testing "sanity: a 1.3.0 that drops an input of 1.2.0 is refused"
      (is (some? (refusal #(publish {:source "V13" :version "1.3.0" :param-types {}} live)))))
    (testing "1.1.0 after 1.2.0, with an input :note that 1.2.0 does not take: 1.1.0 → 1.2.0 breaks"
      (is (some? (refusal #(publish lower live)))
          "an :auto-upgrade consumer pinned to 1.1.0 would be moved onto 1.2.0, which drops :note"))))
