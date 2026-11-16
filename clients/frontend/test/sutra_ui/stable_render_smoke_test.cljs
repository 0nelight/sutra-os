(ns sutra-ui.stable-render-smoke-test
  (:require [sutra-ui.apphost.widgets.action :as action-widgets]
            [cljs.test :refer [deftest is]]
            [sutra-ui.ui.core :as ui]
            [sutra.capps.table :as tbl]))

(def ^:private a1 {:id "a" :values {:n 1}})
(def ^:private a2 {:id "a" :values {:n 2}})
(def ^:private b  {:id "b" :values {:n 1}})

(deftest the-table-chooses-only-where-nothing-is-chosen
  (is (= [a1] (tbl/active-sync nil [a1 b] true)))
  (is (nil? (tbl/active-sync nil [a1 b] false)))
  (is (nil? (tbl/active-sync nil [] true)))
  (is (nil? (tbl/active-sync [a1] [a2 b] true)) "a chosen object is never rewritten")
  (is (nil? (tbl/active-sync [b] [a1] true)) "nor one the rows no longer hold"))

(defn- shape [hiccup]
  [(first hiccup) (first (nth hiccup 2))])

(deftest an-action-button-keeps-its-shape-when-a-note-appears
  (let [plain  (action-widgets/action-button {} {:label "Freigeben"})
        noted  (action-widgets/action-button {} {:label "Freigeben"
                                                  :reason "schon freigegeben"})]
    (is (= (shape plain) (shape noted)) "same wrapper, the button in the same place")
    (is (= ui/button (first (nth (nth plain 2) 2))) "the button sits in its tooltip")
    (is (nil? (nth plain 3)))
    (is (some? (nth noted 3)))))
