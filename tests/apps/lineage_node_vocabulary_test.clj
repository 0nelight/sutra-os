(ns ^:unit apps.lineage-node-vocabulary-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.viz.lineage-projection :as lp]))

(deftest an-object-type-is-bare-on-the-canvas-and-prefixed-in-the-status-map
  (is (= "arbeitsauftrag" (lp/node-id :object-type :arbeitsauftrag)))
  (is (= "object-type/arbeitsauftrag" (lp/status-key :object-type :arbeitsauftrag)))
  (testing "every other kind spells the same both ways"
    (doseq [{:keys [kind bare?]} lp/node-kinds :when (not bare?)]
      (is (= (lp/node-id kind :x) (lp/status-key kind :x)) kind))))

(deftest a-transform-alias-resolves-to-the-transform
  (is (= "transform/befund-parsen" (lp/node-id :sql :befund-parsen)))
  (is (= "transform/befund-parsen" (lp/status-key :python :befund-parsen)))
  (testing "the bug this table exists to stop: a dataset must not take the transform token"
    (is (not= (lp/status-key :dataset :x) (lp/status-key :transform :x)))))

(deftest parse-is-the-inverse-of-node-id
  (doseq [{:keys [kind]} lp/node-kinds]
    (is (= [kind :ein-name] (lp/parse-node-id (lp/node-id kind :ein-name))) kind))
  (is (nil? (lp/parse-node-id "hangar/x")) "an unknown prefix is not a node"))

(deftest hiding-and-rendering-read-the-same-ids
  (let [inv {:datasources #{:acars} :datasets #{:acars_roh} :object-types #{:befund}}]
    (is (= #{"datasource/acars" "dataset/acars_roh" "befund"} (lp/rendered-node-ids inv)))
    (is (= {:datasources #{:acars} :datasets #{} :object-types #{:befund}}
           (lp/visible-inventory inv #{"dataset/acars_roh"})))
    (testing "an object-type hides under its BARE id, not a prefixed one"
      (is (= #{} (:object-types (lp/visible-inventory inv #{"befund"}))))
      (is (= #{:befund} (:object-types (lp/visible-inventory inv #{"object-type/befund"})))))))
