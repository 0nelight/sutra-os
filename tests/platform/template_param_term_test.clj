(ns ^:unit platform.template-param-term-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.compute.render :as render]
            [sutra.types.submission :as csub]))

(def ^:private params
  {:flag  {:type :boolean}
   :menge {:type :number}
   :name  {:type :string}
   :set   {:type :object-set :object-type :partner}
   :obj   {:type :object :object-type :partner}})

(deftest the-cli-reads-each-parameter-by-its-type
  (is (= {:flag false :menge 1.5 :name "007" :set ["a" "b"] :obj "a1"}
         (csub/decode-template-values params {:flag "nein" :menge "1.5" :name "007"
                                              :set "a, b" :obj "a1"})))
  (testing "what does not read stays as given"
    (is (= {:menge "viel"} (csub/decode-template-values params {:menge "viel"})))))

(defn- bound-flag [raw]
  (:flag (render/bind nil nil {:parameters {:flag {:type :boolean}}} {:flag raw} false)))

(deftest the-render-reads-a-truth-value-it-is-told
  (is (false? (bound-flag "false")))
  (is (true? (bound-flag true)))
  (is (thrown? Exception (bound-flag "vielleicht"))))
