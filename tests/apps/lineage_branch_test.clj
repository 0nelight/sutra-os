(ns ^:unit apps.lineage-branch-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.viz.lineage-branch :as lb]))

(deftest ein-lineage-graph-gehoert-der-instanz
  (is (lb/keeps-view? :lineage))
  (is (lb/keeps-view? "lineage"))
  (is (not (lb/keeps-view? :ontology)))
  (is (not (lb/keeps-view? nil))))

(deftest gespeicherter-zweig-wird-nur-angeboten-wo-er-etwas-aendert
  (let [offer #(lb/switch-to (merge {:current "x" :known ["x" "y"]} %))]
    (is (= "y" (offer {:saved "y"})))
    (is (= "main" (offer {:saved "main"})))
    (is (nil? (offer {:saved "x"})) "schon dort")
    (is (nil? (offer {:saved nil})) "alter Graph ohne Wahl")
    (is (nil? (offer {:saved "weg"})) "Zweig gibt es nicht mehr")
    (is (nil? (lb/switch-to {:saved "main" :current nil :known []})) "nil ist main")))

(def ^:private tasks
  [{:section :object-types :name "mangel" :status "modified"}
   {:section "datasets" :name "roh" :status "added"}
   {:section :transforms :name :weg :status "removed"}
   {:section :roles :name "admin" :status "added"}
   {:section :object-types :name "mangel" :status "modified"}])

(deftest was-der-zweig-aenderte-kommt-als-knoten
  (let [refs (lb/modified-refs tasks)]
    (is (= [[:object-type :mangel] [:dataset :roh]] refs))
    (is (= ["object-type/mangel" "dataset/roh"] (lb/modified-node-ids tasks)))
    (testing "fehlt, wenn nicht im Scope oder verborgen"
      (is (= refs (lb/missing-from-graph refs nil nil)))
      (is (= [[:dataset :roh]]
             (lb/missing-from-graph refs {:object-types #{:mangel}} nil)))
      (is (= [[:object-type :mangel]]
             (lb/missing-from-graph refs {:object-types #{:mangel} :datasets #{:roh}}
                                    #{"mangel"}))))))

(deftest ein-knoten-zeigt-seinen-zweig
  (let [refs [[:transform :t]]]
    (is (= "z" (lb/viewed-branch refs "z" :transform :t)))
    (is (= "z" (lb/viewed-branch refs "z" :python :t)) "Alias eines Transforms")
    (is (= "main" (lb/viewed-branch refs "z" :dataset :t)))
    (is (= "main" (lb/viewed-branch [] nil :dataset :t)))))
