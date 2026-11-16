(ns ^:unit ontology.link-in-list-chunking-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.store.access.scope :as ctx]
            [sutra.store.access.substrate :as sub]
            [sutra.objects.reads :as objects]
            [sutra.store.core.effects]
            [sutra.store.pg.layout :as layout]
            [sutra.store.pg.rows :as rows]
            [sutra.store.pg.sql :as sql]))

(def ^:private treiber-decke 65535)

(defn- laeufe
  [filters]
  (let [params (atom [])]
    (with-redefs [ctx/store               (constantly {})
                  layout/link-relation (fn [& _] (sql/frag "(edges)"))
                  sql/rows             (fn [_c _sql ps]
                                         (swap! params conj (count ps))
                                         (mapv (fn [id] {:id id :from_id id :to_id "z"})
                                               (mapcat :xs
                                                       (filter
                                                        #(instance? sutra.store.pg.sql.TextArray %)
                                                        ps))))
                  sub/read-links       (fn [_st L where]
                                         (mapv #(#'sutra.store.core.effects/->link L %)
                                               (rows/fk-links
                                                nil {:ns "main" :sig {}} L (or where {}))))]
      (let [rows (#'objects/link-rows (ctx/store) :kante filters)]
        {:params @params :rows rows}))))

(deftest kleine-mengen-bleiben-eine-anweisung
  (testing "der haeufige Fall aendert sich nicht"
    (let [{:keys [params rows]} (laeufe {:from-ids (mapv str (range 10))})]
      (is (= 1 (count params)) "nichts wird gestueckelt, was passt")
      (is (= 10 (count rows))))))

(deftest die-decke-wird-nicht-mehr-gerissen
  (let [ids (mapv str (range 77715))
        {:keys [params rows]} (laeufe {:from-ids ids})]
    (testing "keine Anweisung bindet mehr als der Treiber traegt: die ids gehen als ein Array"
      (is (every? #(<= % treiber-decke) params)
          (str "groesste Anweisung: " (apply max params) " Parameter"))
      (is (= [1] params) "eine Anweisung, ein Parameter"))
    (testing "das Ergebnis ist vollstaendig"
      (is (= 77715 (count rows)) "keine Zeile verloren, keine doppelt")
      (is (= (set ids) (set (map :id rows))))
      (is (= (set ids) (set (map :from-id rows))) "each link keeps the end it was asked by")
      (is (= #{"z"} (set (map :to-id rows)))))))

(deftest ein-limit-waehlt-global-und-nicht-je-stueck
  (let [ids (mapv #(format "%06d" %) (range 50000))
        {:keys [rows]} (laeufe {:from-ids ids :limit 5})]
    (is (= 5 (count rows)))
    (is (= ["000000" "000001" "000002" "000003" "000004"] (mapv :id rows)))))

(deftest leere-mengen-fragen-gar-nicht-erst
  (let [{:keys [params rows]} (laeufe {:from-ids []})]
    (is (empty? params))
    (is (empty? rows))))
