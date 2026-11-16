(ns ^:unit platform.names-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.parser.names :as names]
            [sutra.types.forms :as forms]))

(def ^:private authored
  {:projects     {:einkauf {:synonyms {:lieferer :lieferant}} :lager {} :it {}}
   :objects      {:lieferant {:project :einkauf :attributes [[:nr :string]] :plural "Lieferanten"}
                  :artikel   {:project :lager :attributes [[:nr :string]] :plural "Artikel"}
                  :it        {:project :it :attributes [[:nr :string]] :plural "Its"}}
   :repositories {:it {:project :it}}
   :actions      {:artikel-anlegen {:rules [{:op :create :object-type :artikel}]}}})

(deftest every-name-of-every-kind-with-its-project
  (let [idx (names/index (forms/author authored))]
    (is (contains? (set (map (juxt :kind :name :project) (:entries idx)))
                   [:action :artikel-anlegen :lager])
        "an action's project is the one the membership table derives")
    (is (= [{:alias :lieferer :name :lieferant :kind :object-type :project :einkauf}]
           (:synonyms idx)))
    (testing "one name in several kinds is a homonym, not a duplicate"
      (is (= [[:it #{:object-type :project :repository}]]
             (mapv (juxt :name :kinds) (names/homonyms idx))))
      (is (empty? (names/duplicates idx))))))

(deftest a-name-twice-in-one-kind-is-a-duplicate
  (let [idx (names/index (forms/installed {:object-types {"r1" {:object-type-name :artikel
                                                                :plural "Artikel"}
                                                          "r2" {:object-type-name :Artikel
                                                                :plural "Artikel"}}}))]
    (is (= [[:object-type #{"r1" "r2"}]]
           (mapv (juxt :kind #(set (map :rid (:entries %)))) (names/duplicates idx)))
        "names compare folded, and the RIDs say which two")))

(deftest a-synonym-that-hides-a-real-name-shadows-it
  (let [idx
          (names/index (forms/author (assoc-in authored [:projects :lager :synonyms]
                                               {:artikel :lieferant})))]
    (is (= [[:artikel :lager]] (mapv (juxt :alias :project) (names/shadows idx))))))

(deftest what-to-do-with-a-name-about-to-be-written
  (let [idx (names/index (forms/author authored))]
    (is (= :exists (:verdict (names/suggest idx :artikel :object-type :lager))))
    (is (= :use (:verdict (names/suggest idx :artikel :object-type :einkauf)))
        "another project's: use it, no import")
    (is (= :use (:verdict (names/suggest idx :lieferer :object-type :lager)))
        "an alias some project adopted leads to the real type")
    (is (= :close (:verdict (names/suggest idx :artikeln :object-type :einkauf))))
    (is (= :create (:verdict (names/suggest idx :pruefbericht :object-type :einkauf))))))
