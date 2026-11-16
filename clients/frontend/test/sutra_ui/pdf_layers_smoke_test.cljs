(ns sutra-ui.pdf-layers-smoke-test
  (:require [cljs.test :refer-macros [deftest testing is]]
            [sutra.capps.pdf-layers :as pdf-layers]
            [clojure.string]))

(def ^:private row
  {:id "o1" :position-id "p1" :pdf-seite 1 :beschreibung "Powerbond"
   :pdf-boxen ["{\"x1\": 32.6, \"y1\": 365.9, \"x2\": 568.4, \"y2\": 377.5}"
               "{\"x1\": 530, \"y1\": 380, \"x2\": 560, \"y2\": 390}"]})

(deftest a-box-string-becomes-a-mark
  (is (= {:x1 32.6 :y1 365.9 :x2 568.4 :y2 377.5}
         (pdf-layers/parse-box "{\"x1\": 32.6, \"y1\": 365.9, \"x2\": 568.4, \"y2\": 377.5}")))
  (is (= {:x1 1.0 :y1 2.0 :x2 3.0 :y2 4.0} (pdf-layers/parse-box {"x1" 1 "y1" 2 "x2" 3 "y2" 4})))
  (is (nil? (pdf-layers/parse-box "{\"x1\": 1}")) "a box without all four corners draws nothing"))

(deftest a-band-has-no-sides
  (is (= {:y1 10.0 :y2 20.0 :page 2} (pdf-layers/parse-box "{\"y1\": 10, \"y2\": 20, \"page\": 2}")))
  (is (nil? (pdf-layers/parse-box "{\"x1\": 1, \"y1\": 10, \"y2\": 20}"))
      "one side alone is a broken box, not a band")
  (is (= "{\"y1\": 10, \"y2\": 20, \"page\": 2}"
         (pdf-layers/band-text 2 {:x1 0 :y1 10 :x2 595 :y2 20})))
  (let [seen (atom nil)
        [m]  (pdf-layers/marks [{:rows [{:id "a" :s 2 :b ["{\"y1\": 10, \"y2\": 20}"]}]
                                 :page :s :boxes :b
                                 :on-boxes (fn [_ boxes] (reset! seen boxes))}])]
    (is (= :band (:kind m)) "a box without sides draws as a band, whatever the layer says")
    ((:on-change m) {:x1 0 :y1 11 :x2 595 :y2 21})
    (is (= ["{\"y1\": 11, \"y2\": 21, \"page\": 2}"] @seen) "and stays one when it is moved")))

(deftest a-layer-is-rows-plus-page-and-boxes-properties
  (let [ms (pdf-layers/marks [{:rows [row] :page :pdf-seite :boxes :pdf-boxen
                       :color "#fbc02d" :label :beschreibung :key :position-id}])]
    (is (= 2 (count ms)) "one mark per box — two lines, two rectangles")
    (is (= {:x 32.6 :y 365.9 :w (- 568.4 32.6) :h (- 377.5 365.9)} (:geom (first ms))))
    (is (every? #(= 1 (:page %)) ms))
    (is (every? #(= row (:row %)) ms) "the mark carries its ROW for the selection event")
    (is (= "Powerbond" (:label (first ms))))
    (is (= "#fbc02d" (:color (first ms)))))
  (testing "the active row is drawn in the selection colour"
    (is (= "#1976d2"
           (:color (first (pdf-layers/marks [{:rows [row] :page :pdf-seite :boxes :pdf-boxen
                                        :key :position-id :active "p1"}]))))))
  (testing "a row without a page, or without boxes, is simply not drawn"
    (is (empty? (pdf-layers/marks [{:rows [(dissoc row :pdf-seite)] :page :pdf-seite
                                    :boxes :pdf-boxen}])))
    (is (empty? (pdf-layers/marks [{:rows [(assoc row :pdf-boxen nil)] :page :pdf-seite
                                    :boxes :pdf-boxen}]))))
  (testing "the array may arrive as one JSON string"
    (is (= 2
           (count (pdf-layers/marks [{:rows
                                        [(assoc row
                                                :pdf-boxen
                                                  (str "["
                                                       (clojure.string/join "," (:pdf-boxen row))
                                                       "]"))]
                              :page :pdf-seite :boxes :pdf-boxen}]))))))

(deftest a-stamp-sits-at-a-fixed-place-and-only-when-its-property-is-set
  (let [layer {:rows [{:rechnungs-id "r1" :gezahlt-am "2026-07-12" :zahlungsart "überweisung"}]
               :kind :stamp :page 1 :boxes {:x1 360 :y1 36 :x2 570 :y2 116}
               :title "BEZAHLT" :body [:gezahlt-am :zahlungsart] :when :gezahlt-am
               :color "#c62828" :key :rechnungs-id}
        [m & more] (pdf-layers/marks [layer])]
    (is (nil? more))
    (is (= :stamp (:kind m)))
    (is (= 1 (:page m)) "a page NUMBER, not a property")
    (is (= {:x 360.0 :y 36.0 :w 210.0 :h 80.0} (:geom m)) "a literal box, not a property")
    (is (= "BEZAHLT" (:title m)))
    (is (= "12.07.2026 · überweisung" (:text m)) "dates print the way a stamp prints them")
    (is (empty? (pdf-layers/marks [(update-in layer [:rows 0] dissoc :gezahlt-am)]))
        "no payment date, no stamp")))
