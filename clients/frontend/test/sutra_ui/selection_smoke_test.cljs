(ns sutra-ui.selection-smoke-test
  (:require [cljs.test :refer [deftest is testing]]
            [reagent.core :as r]
            [sutra-ui.apphost.state :as state]))

(defn- row [t id n] {:id id :object-type-ref t :values {:n n}})

(defn- page [& rows] {:rechnung (vec rows)})

(defn- handle [payload* v]
  (doto (state/selection (r/atom nil) (state/payload-index payload*)) (reset! v)))

(deftest a-chosen-object-reads-as-the-page-holds-it-now
  (let [payload* (r/atom (page (row "rechnung" "r1" 1) (row "rechnung" "r2" 1)))
        sel      (handle payload* [(row "rechnung" "r1" 1)])]
    (reset! payload* (page (row "rechnung" "r1" 2) (row "rechnung" "r2" 1)))
    (is (= [(row "rechnung" "r1" 2)] @sel) "the reload's row, not the copy taken on the click")
    (testing "an object the page no longer holds reads as last seen, never older or empty"
      (reset! payload* (page (row "rechnung" "r2" 1)))
      (is (= [(row "rechnung" "r1" 2)] @sel))
      (reset! payload* (page))
      (is (= [(row "rechnung" "r1" 2)] @sel)))
    (testing "a new choice starts from what was written, not from what was remembered"
      (reset! sel [(row "rechnung" "r1" 5)])
      (is (= [(row "rechnung" "r1" 5)] @sel)))))

(deftest the-shape-written-is-the-shape-read
  (let [payload* (r/atom (page (row "rechnung" "r1" 2)))
        old      (row "rechnung" "r1" 1)]
    (is (= (row "rechnung" "r1" 2) @(handle payload* old)) "a bare row")
    (is (= #{(row "rechnung" "r1" 2)} @(handle payload* #{old})) "a set (multi-select)")
    (is (= "abschnitt-7" @(handle payload* "abschnitt-7")) "a scalar key stays a key")
    (is (nil? @(handle payload* nil)))
    (is (= [{:id "x"}] @(handle payload* [{:id "x"}])) "a row without its type is left alone")))

(deftest writing-through-the-handle
  (let [payload* (r/atom (page (row "rechnung" "r1" 2) (row "rechnung" "r2" 2)))
        sel      (handle payload* #{(row "rechnung" "r1" 1) (row "rechnung" "r2" 1)})]
    (swap! sel disj (row "rechnung" "r1" 2))
    (is (= #{(row "rechnung" "r2" 2)} @sel) "a checkbox unticks the row as it is drawn now")
    (let [seen (atom nil)]
      (add-watch sel ::t (fn [_ _ _ n] (reset! seen n)))
      (reset! sel nil)
      (is (= ::cleared (if (nil? @seen) ::cleared @seen)) "watches see the write"))))

(deftest the-same-object-in-two-shapes
  (let [plain  (row "rechnung" "r1" 2)
        linked (assoc (row "rechnung" "r1" 3) :link/lieferant "l9")
        idx    (state/row-index {:rechnung [plain] :link/von [linked]})]
    (is (= [plain] (state/freshen [(row "rechnung" "r1" 1)] idx)))
    (is (= [linked] (state/freshen [(assoc (row "rechnung" "r1" 1) :link/lieferant "l1")] idx))
        "the shape it was taken from")))
