(ns ^:unit grammar.canvas-view-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.kit.remote :as rd]
            [sutra.viz.lineage-branch :as lb]))

(def ^:private graphs
  [{:id "g1" :name "b" :mode :lineage}
   {:id "g2" :name "c" :mode :lineage}
   {:id "g3" :name "z" :mode :ontology}])

(deftest the-open-view-of-the-mode-stays
  (is (= {:use :current}
         (lb/canvas-view {:mode :lineage :current {:mode :lineage} :catalog rd/loading
                          :graphs []}))))

(deftest the-last-saved-graph-of-the-mode-by-name
  (is (= {:open "g2"}
         (lb/canvas-view {:mode :lineage :current {:mode :ontology} :catalog :loaded
                          :graphs graphs}))))

(deftest a-new-graph-only-when-the-catalog-says-there-is-none
  (testing "loaded and none of the mode"
    (is (= {:create true}
           (lb/canvas-view {:mode :explorer :catalog :loaded :graphs graphs}))))
  (testing "still loading: wait, do not create"
    (is (= {:wait :loading} (lb/canvas-view {:mode :lineage :catalog rd/loading :graphs []}))))
  (testing "failed: say why"
    (is (= {:wait :failed :reason "502"}
           (lb/canvas-view {:mode :lineage :catalog (rd/failed "502") :graphs []})))))
