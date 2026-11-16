(ns ^:unit apps.app-read-want-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.capps.catalog :as cat]
            [sutra.objects.views :as views]
            [sutra.ref.objectset :as ros]))

(def ^:private layout
  {:object-types ["arbeitsauftrag"]
   :vars {:shown {:from "alle" :where [["freigabe" "=" nil]]}}
   :pages [{:body {:kind :loop
                   :sort [["fällig-am" "asc"]]
                   :body {:items [{:widget "markdown" :text {:item "kennzeichen"}}
                                  {:widget "badge" :fmt "eur" :text {:item :kosten}}
                                  {:widget "badge" :when {:item "nachpruefung-fällig?"}
                                   :text "abgezeichnet — Nachprüfung fällig"}
                                  {:widget "action-button" :action "auftrag-abzeichnen"
                                   :params {:auftrag {:item "id"}}}]}}}]})

(deftest every-shape-a-renderer-binds-is-collected
  (let [w (cat/referenced-properties layout)]
    (testing "an :item ref, whether authored as a string or a keyword"
      (is (contains? w :kennzeichen))
      (is (contains? w :kosten)))
    (testing "a :where term and a :sort term"
      (is (contains? w :freigabe))
      (is (contains? w :fällig-am)))
    (testing ":when — the slot that decides whether a row renders at all"
      (is (contains? w :nachpruefung-fällig?)))
    (testing "an action param's :item"
      (is (contains? w :id)))))

(deftest a-slot-the-grammar-does-not-have-yet-is-still-collected
  (is (contains? (cat/referenced-properties
                  {:pages [{:widget "sparkline" :some-slot-invented-tomorrow "teilekosten"}]})
                 :teilekosten)))

(deftest a-label-that-spells-an-attribute-only-over-collects
  (testing "over-collecting costs one derivation; under-collecting costs a wrong number"
    (is (contains? (cat/referenced-properties {:widget "badge" :text "kosten"}) :kosten))))

(deftest blank-and-non-name-leaves-are-not-properties
  (let [w (cat/referenced-properties {:a "" :b nil :c 42 :d true})]
    (is (not (contains? w (keyword ""))))
    (is (= #{:a :b :c :d} w) "map keys are names too; values that are not are dropped")))

(def ^:private rid :ri.ot/arbeitsauftrag)

(def ^:private schema
  {:object-types
   {rid {:attribute-list
         [{:attribute-name :kosten}
          {:attribute-name :lohnkosten
           :opts {:derived {:expression [:connected-sum :stunde-auftrag :betrag]}}}
          {:attribute-name :teilekosten
           :opts {:derived {:expression [:connected-sum :teil-auftrag :preis]}}}]
         :plural "Objekte"}}})

(def ^:private row
  {:id "r1" :values {:kosten 27.09M :lohnkosten nil :teilekosten 27.09M}})

(deftest a-derived-property-outside-want-is-absent-not-stale
  (let [out (first (views/attach-derived nil schema rid [row] #{:kosten}))]
    (is (not (contains? (:values out) :teilekosten))
        "the stale column must not read back as the property")
    (is (not (contains? (:values out) :lohnkosten)))
    (is (= 27.09M (get-in out [:values :kosten])) "a stored property is untouched")))

(deftest an-empty-plan-still-drops-the-columns
  (testing "narrowing everything away is the case that returns early — it must still drop"
    (let [out (first (views/attach-derived nil schema rid [row] #{}))]
      (is (= {:kosten 27.09M} (:values out))))))

(def ^:private local-schema
  {:object-types
   {rid {:attribute-list
         [{:attribute-name :kosten}
          {:attribute-name :aufschlag}
          {:attribute-name :summe
           :opts {:derived {:expression [:+ [:get :kosten] [:get :aufschlag]]}}}]
         :plural "Objekte"}}})

(defn- answered-by-the-reference
  [schema rows]
  (fn [[_ ir]]
    (ros/read {:sigma {:extension (fn [_ _] (map #(assoc % :object-type-ref rid) rows))}}
              schema ir)))

(deftest want-nil-keeps-every-derived-key
  (testing "the unnarrowed read is unchanged: nothing is dropped, every key is written"
    (let [in  {:id "r1" :values {:kosten 10M :aufschlag 2M :summe :stale}}
          out (first (views/attach-derived (answered-by-the-reference local-schema [in])
                                           local-schema rid [in] nil))]
      (is (= 12M (get-in out [:values :summe])) "computed, not the stale column"))))

(deftest a-narrowed-read-drops-what-it-does-not-compute
  (let [in  {:id "r1" :values {:kosten 10M :aufschlag 2M :summe :stale}}
        out (first (views/attach-derived nil local-schema rid [in] #{:kosten}))]
    (is (not (contains? (:values out) :summe)))
    (is (= 10M (get-in out [:values :kosten])))))
