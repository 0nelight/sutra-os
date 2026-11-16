(ns ^:unit platform.decimal-precision-test
  (:require [clojure.test :refer [deftest is]]
            [sutra.authoring.install.diff :as diff]
            [sutra.authoring.install.live :as live]
            [sutra.authoring.install.findings.backing :as backing-findings]))

(defn- typ [k ds]
  {:object-type-name k :backing {:dataset ds}
   :attribute-list [{:attribute-name :y :attribute-type [:decimal 10 2]}]})

(def ^:private W {:object-types {:a (typ :a :ds) :b (typ :b :ds2)}})

(def ^:private text
  {:datasets {:ds {:columns [[:y [:decimal 12 2]]]} :ds2 {:columns [[:y [:decimal 12 2]]]}}})

(defn- found [plan]
  (map :entity ((:find backing-findings/decimal-precision)
                (diff/diff text W plan {} (live/world-index W)))))

(deftest a-push-reports-the-mismatches-of-what-it-touched
  (is (empty? (found [])) "a push that touches neither reports neither")
  (is (= [:a] (found [{:kind :object-type :entity :a :outcome :updated :args {}}]))
      "a staged type")
  (is (= [:b] (found [{:kind :dataset :entity :ds2 :outcome :updated :args {}}]))
      "a type its changed dataset backs"))
