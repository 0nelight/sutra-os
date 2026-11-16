(ns ^:unit platform.cli-diff-refused-test
  (:require [clojure.test :refer [deftest is]]
            [sutra.cli.live]
            [sutra.cli.verbs.checks]
            [sutra.authoring.install.plan :as install-plan]
            [sutra.wire.api :as api]))

(defn- diff [body]
  (with-redefs [api/post!
                  (fn [id & _] (case id :install {:status 200 :body body}))
                sutra.cli.verbs.checks/candidate-schema    (fn [_ _] {})
                sutra.cli.verbs.checks/push-repository     (fn [_] nil)
                sutra.cli.live/live-schema         (fn [] {})]
    (let [r (atom nil)
          out (with-out-str (reset! r (#'sutra.cli.verbs.checks/diff-preset! {} ["ontology.ent"])))]
      [out @r])))

(def ^:private planned
  (install-plan/planned-rows [{:kind :transform :entity :parse-a :outcome :updated
                          :action :update-transform :args {:name :parse-a}}
                         {:kind :transform :entity :parse-b :outcome :updated
                          :action :update-transform :args {:name :parse-b}}
                         {:kind :object-type :entity :konto :outcome :skipped}]))

(deftest a-refused-plan-never-reads-as-no-changes
  (let [[out r] (diff {:created 0 :updated 2 :skipped 1 :staged [] :preview? true
                       :refused? true :planned planned
                       :problems {:plan [{:name "parse-a"
                                          :error "would remove its last producer"}]}})]
    (is (not (re-find #"keine Änderungen|no changes" out)))
    (is (re-find #"parse-a" out) "the planned rows are listed")
    (is (re-find #"2 " out))
    (is (= :abort r) "and the refusal still fails the line")))
