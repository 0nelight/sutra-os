(ns ^:unit apps.column-inherits-property-display-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.capps.ctx :as cctx]
            [sutra.capps.format :as fmt]
            [sutra.capps.table :as tbl]))

(def ^:private de (cctx/of {:locale :de}))

(def ^:private props
  {:kosten  {:type :decimal :name "Kosten"
             :format {:kind :currency :currency "EUR" :decimals 2 :grouping true}}
   :betriebsstunden {:type :double :name "Betriebsstunden"
                     :format {:decimals 0 :grouping true :kind :unit :unit "h"}}
   :auslastung  {:type :decimal :format {:kind :percentage :decimals 2 :suffix "%"}}
   :ata-kapitel {:type :string :name "ATA-Kapitel"}
   :bemerkung {:type :string}})

(deftest label-comes-from-the-property
  (let [[a b c] (tbl/with-prop-display [{:prop :kosten} {:prop :ata-kapitel} {:prop :bemerkung}]
                  props)]
    (is (= "Kosten" (:label a)))
    (is (= "ATA-Kapitel" (:label b)) "the ontology's display name, hyphen and all")
    (is (= "Bemerkung" (:label c)) "no declared name ⇒ humanized, never blank")))

(deftest the-column-keeps-the-last-word
  (let [[c] (tbl/with-prop-display [{:prop :kosten :label "Kosten ohne USt" :align :left}] props)]
    (is (= "Kosten ohne USt" (:label c)))
    (is (= :left (:align c)) "an author's alignment is not overwritten by the base type")
    (is (some? (:fmt c)) "the key they did NOT set still inherits")))

(deftest numeric-columns-align-right
  (let [by-prop (into {} (map (juxt :prop identity))
                      (tbl/with-prop-display [{:prop :kosten} {:prop :ata-kapitel}] props))]
    (is (= :right (get-in by-prop [:kosten :align])))
    (is (nil? (get-in by-prop [:ata-kapitel :align])) "text does not align on its last place")))

(deftest format-renders-from-the-declaration
  (testing "the ontology's :format spec IS the renderer — no widget-side name in between"
    (let [f #(:fmt (first (tbl/with-prop-display [{:prop %}] props)))]
      (is (= "1.234,50 €" ((f :kosten) 1234.5 de)))
      (is (= "1.234 h" ((f :betriebsstunden) 1234.4 de)))
      (is (= "12,50 %" ((f :auslastung) 12.5 de))))))

(deftest an-unformatted-property-keeps-the-widget-default
  (is (nil? (:fmt (first (tbl/with-prop-display [{:prop :ata-kapitel}] props))))
      ":id/:rid/no format ⇒ no formatter, so `:auto` still decides")
  (is (nil? (fmt/by-spec {:kind :id})))
  (is (nil? (fmt/by-spec {}))))

(deftest a-column-without-a-prop-survives
  (testing "the empty column the app builder appends binds nothing and must not throw"
    (is (= [{} {:label "Frei"}]
           (tbl/with-prop-display [{} {:label "Frei"}] props)))))

(deftest a-prop-the-type-does-not-declare-passes-through
  (testing "a module-scoped derived property has no ontology entry — and no default"
    (is (= [{:prop :egt-reserve}] (tbl/with-prop-display [{:prop :egt-reserve}] props)))))

(deftest no-table-changes-nothing
  (testing "an app opened before the server shipped a display table renders as it did"
    (let [cols [{:prop :kosten} {:prop :ata-kapitel :label "Kapitel"}]]
      (is (= cols (tbl/with-prop-display cols nil)))
      (is (= cols (tbl/with-prop-display cols {}))))))

(deftest a-formatter-passes-through-fmt-by
  (testing "an inherited :fmt is a FN, and cell-text must not send it back through the name table"
    (let [[c] (tbl/with-prop-display [{:prop :kosten}] props)]
      (is (= "10,00 €" (tbl/cell-text c {:values {:kosten 10}} de))))))

(deftest grouping-false-drops-the-separator
  (is (= "1234,50" ((fmt/by-spec {:decimals 2 :grouping false}) 1234.5 de))))

(deftest the-spec-arrives-off-the-json-wire
  (testing "`:decimals` rides as a STRING. Read as-is it reaches fmt's rounding and throws at paint time."
    (is (= "1.234 h"
           ((fmt/by-spec {:kind "unit" :unit "h" :decimals "0" :grouping true}) 1234.4 de)))
    (is (= "1.234,50 €" ((fmt/by-spec {:kind "currency" :currency "EUR" :decimals "2"}) 1234.5 de)))
    (is (= "12,5 %"     ((fmt/by-spec {:kind "percentage" :decimals "1"}) 12.5 de))))
  (testing "and so does the whole property entry, `:type` included"
    (let [[c] (tbl/with-prop-display
                [{:prop :betriebsstunden}]
                {:betriebsstunden {:type "double" :name "Ist-Stunden"
                                   :format {:kind "unit" :unit "h" :decimals "0" :grouping true}}})]
      (is (= "Ist-Stunden" (:label c)))
      (is (= :right (:align c)) "a base type that arrived as a string still aligns")
      (is (= "40 h" (tbl/cell-text c {:values {:betriebsstunden 40}} de))))))
