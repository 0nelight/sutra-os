(ns sutra-ui.graph-editor.events-smoke-test
  (:require [cljs.test :refer [deftest is]]
            [sutra-ui.graph-editor.events]
            [sutra-ui.state.graph :as graph]
            [sutra-ui.state.machine-testing :refer [step]]))

(defn- fx-of [kind [_ fx]] (keep (fn [[k a]] (when (= kind k) a)) fx))

(defn- layout [] (js/Promise.resolve {}))

(deftest an-asked-for-layout-runs-one-at-a-time-and-can-be-undone
  (let [job {:layout [layout] :place identity :mode :ontology :busy? true}
        [s :as r] (step {} [:shared.graph/lay-out job])]
    (is (get-in s [:shared.graph :busy]))
    (is (= [[graph/record-graph-state!]] (fx-of :invoke r)))
    (is (= [s []] (step s [:shared.graph/lay-out job])))
    (is (false? (get-in (first (step s (conj (:err (first (fx-of :call r))) "x")))
                        [:shared.graph :busy])))))

(deftest a-layout-placing-nothing-applies-nothing
  (let [job {:layout [layout] :place (constantly nil) :mode :ontology}]
    (is (empty? (second (step {} [:shared.graph/laid-out job {"a" {:x 1 :y 2}}]))))
    (is (= [:shared.graph/fit]
           (:event (first (fx-of :after (step {} [:shared.graph/laid-out
                                                  (assoc job :place identity)
                                                  {"a" {:x 1 :y 2}}]))))))))

(deftest a-confirmed-deletion-runs-what-was-named
  (let [f #(do)
        [{:keys [ok]}] (fx-of :confirm (step {} [:shared.graph/confirm {:title "t"} f 1 2]))]
    (is (= [[f 1 2]] (fx-of :invoke (step {} ok))))))

(deftest a-new-colour-group-takes-the-next-colour-and-needs-a-name
  (let [[{:keys [ok]}] (fx-of :shared.graph/prompt
                              (step {} [:shared.graph/new-color-group "t" 7 ["a"]]))]
    (is (= [:shared.graph/color-group-named "#238551" ["a"]] ok))
    (is (= [{} []] (step {} (conj ok ""))))
    (is (= [[graph/create-color-group-for-nodes! "g" "#238551" ["a"]]]
           (fx-of :invoke (step {} (conj ok "g")))))))
