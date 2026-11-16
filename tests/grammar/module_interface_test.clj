(ns ^:unit grammar.module-interface-test
  (:require [sutra.parser.spec :as spec]
            [clojure.test :refer [deftest testing is]]
            [sutra.capps.variables :as vars]
            [sutra.parser.ent :as ent]
            [sutra.parser.lint :as lint]
            [sutra.types.versions :as vers]))

(defn- tuer [vs]
  (try (ent/parse-enta {:name :probe :layout {:kind :rows :vars vs :items []}} "p.enta")
       :angenommen
       (catch Exception e (.getMessage e))))

(deftest nur-eine-external-id-macht-eine-variable-adressierbar
  (testing "eine Variable ohne :external-id gehoert der App allein"
    (is (false? (vars/interface? {:default 1})))
    (is (empty? (vars/url-slots {:q {:default ""}}))))
  (testing "der Schluessel nach aussen ist die external id, nie der Variablenname"
    (is (= {:jahr :geschaeftsjahr}
           (vars/url-slots {:geschaeftsjahr {:type :number :default 2026 :external-id :jahr}})))
    (is (= :jahr (vars/url-key {:external-id :jahr}))))
  (testing "in die URL schreiben darf nur, wer eine nennt"
    (is (re-find #"keine :external-id"
                 (tuer {:seite {:type :keyword :default :a :in-url :always}})))
    (is (= :angenommen
           (tuer {:seite {:type :keyword :default :a :external-id :seite :in-url :always}})))))

(defn- vars-of [vs]
  (-> (ent/parse-enta {:name :probe :layout {:kind :rows :vars vs :items []}} "p.enta")
      (get-in [:schema :applications :probe :layout :vars])))

(deftest das-alte-url-flag-wird-beim-lesen-gehoben
  (testing "`:url? true` sagte beides auf einmal, und beides bleibt erhalten"
    (is (= {:type :keyword :default :a :external-id :seite :in-url :always}
           (:seite (vars-of {:seite {:type :keyword :default :a :url? true}})))
        "der Variablenname WAR der Schluessel in der URL — als external id ueberlebt
         jeder Link, den jemand aufbewahrt hat"))
  (testing "eine schon erklaerte external id wird nicht ueberschrieben"
    (is (= :rechnung
           (:external-id (:sel (vars-of {:sel {:type :string :default nil
                                               :external-id :rechnung :url? true}}))))
        "sonst aenderte die Migration genau den Schluessel, den sie erhalten soll"))
  (testing "gelesen, nicht geschrieben — was gehoben wurde, traegt die alte Form nicht mehr"
    (is (not (contains? (:seite (vars-of {:seite {:type :keyword :default :a :url? true}}))
                        :url?)))))

(deftest in-die-url-gehoert-nur-was-eine-geschriebene-form-hat
  (testing "ohne :type weiss niemand, ob \"2026\" Zahl oder Text ist"
    (is (re-find #"braucht dafuer :type"
                 (tuer {:j {:default 2026 :external-id :j :in-url :always}}))))
  (testing "eine gefilterte Objektmenge hat keine"
    (is (re-find #"gefilterte Objektmenge"
                 (tuer {:r {:source :rechnung}
                        :offen {:type :object-set :from :r :where [[:status := "offen"]]
                                :external-id :offen :in-url :always}}))))
  (testing "aber als Startwert in die App reichen darf man sie sehr wohl"
    (is (= :angenommen
           (tuer {:r {:source :rechnung}
                  :offen {:type :object-set :from :r :where [[:status := "offen"]]
                          :external-id :offen}})))))

(deftest jede-schnittstellenvariable-ist-aus-der-url-initialisierbar
  (let [vs {:a {:type :string :default "" :external-id :a :in-url :never}
            :b {:type :string :default "" :external-id :b :in-url :when-visible}
            :c {:type :string :default "" :external-id :c :in-url :always}
            :d {:type :string :default ""}}]
    (testing "gelesen werden darf jede erklaerte — auch :never"
      (is (= #{:a :b :c} (set (keys (vars/url-slots vs))))))
    (testing "zurueckgeschrieben wird nur, was es sagt"
      (is (= {:b :when-visible :c :always} (vars/url-writers vs))))
    (testing "und :in-url ist ohne Angabe :never — die Adresszeile fuellt sich nicht von selbst"
      (is (= :never (vars/in-url {:external-id :x}))))))

(deftest die-drei-verhalten-sind-ein-katalog-kein-schluesselwort
  (testing "jedes Verhalten traegt selbst, was die Adresszeile von ihm wissen muss"
    (is (= [:never :when-visible :always] (mapv :id vars/url-behaviors)))
    (is (every? :label vars/url-behaviors))
    (is (every? :hint vars/url-behaviors)))
  (testing "nichts verzweigt auf den Namen"
    (is (false? (:writes? (vars/url-behavior :never))))
    (is (true?  (:writes? (vars/url-behavior :always))))
    (is (false? (:only-while-live? (vars/url-behavior :always))))
    (is (true?  (:only-while-live? (vars/url-behavior :when-visible)))))
  (testing "ein unbekanntes Verhalten faellt auf den sichersten zurueck"
    (is (= :never (:id (vars/url-behavior :quatsch))))))

(def ^:private seiten-baum
  {:kind   :pages
   :select {:var :seite}
   :pages  [{:id :liste  :body {:kind :rows :items [{:widget :text :text {:val :q}}]}}
            {:id :detail :body {:kind :rows :items [{:widget :text :text {:val :id-filter}}]}}]})

(deftest live-vars-folgt-der-offenen-seite
  (let [specs {:seite     {:type :keyword :default :liste}
               :q         {:type :string :default ""}
               :id-filter {:type :string :default ""}}]
    (testing "auf der Liste zaehlt q, nicht der Detailfilter"
      (let [live (vars/live-vars seiten-baum specs {:seite :liste})]
        (is (contains? live :q))
        (is (not (contains? live :id-filter)))))
    (testing "auf dem Detail genau umgekehrt"
      (let [live (vars/live-vars seiten-baum specs {:seite :detail})]
        (is (contains? live :id-filter))
        (is (not (contains? live :q)))))
    (testing "die Seitenvariable selbst ist immer gelesen — sie waehlt ja"
      (is (contains? (vars/live-vars seiten-baum specs {:seite :liste}) :seite)))
    (testing "eine Variable, die eine sichtbare liest, zaehlt mit"
      (let [specs (assoc specs :q {:type :string :default "" :expr [:str {:val :roh}]}
                         :roh {:type :string :default ""})]
        (is (contains? (vars/live-vars seiten-baum specs {:seite :liste}) :roh)
            "sonst verschwaende ein Zwischenschritt den Wert aus der URL")))
    (testing "eine unlesbare Auswahl laesst JEDE Seite stehen"
      (let [live (vars/live-vars seiten-baum specs {})]
        (is (contains? live :q))
        (is (contains? live :id-filter)
            "eine unentscheidbare Wahl darf die Antwort nicht verkleinern")))))

(def ^:private zwei-apps
  {:applications
   {:r1 {:application-name :leitwarte
         :layout {:kind :pages :pages []
                  :vars {:jahr   {:type :number :default 2026 :external-id :year :in-url :always}
                         :privat {:default nil}}}}
    :r2 {:application-name :verkaufen
         :layout {:kind :rows
                  :items [{:widget :button
                           :on-click {:do :open-app :app :leitwarte :with {:year {:val :j}}}}]}}}})

(defn- open-app-findings [schema]
  (filterv #(= :open-app-misses-the-interface (:rule %)) (lint/run-lint schema (spec/vocabulary))))

(deftest open-app-wird-gegen-die-schnittstelle-des-ziels-geprueft
  (testing "eine Abbildung auf eine erklaerte external id ist in Ordnung"
    (is (empty? (open-app-findings zwei-apps))))
  (testing "eine private Variable des Ziels ist keine Schnittstelle"
    (let [s (assoc-in zwei-apps [:applications :r2 :layout :items 0 :on-click :with]
                      {:privat 1})]
      (is (re-find #"keine :external-id" (:message (first (open-app-findings s)))))
      (is (re-find #"sie bietet :year" (:message (first (open-app-findings s))))
          "die Meldung nennt, was das Ziel stattdessen annimmt")))
  (testing "eine App, die es nicht gibt, ist ein Sprung ins Leere"
    (let [s (assoc-in zwei-apps [:applications :r2 :layout :items 0 :on-click :app] :gibtsnicht)]
      (is (re-find #"ins Leere" (:message (first (open-app-findings s))))))))

(def ^:private alt-app
  {:application-name :leitwarte
   :published "1"
   :releases {"1" {:layout {:kind :rows
                            :vars {:seite {:type :keyword :default :a :url? true}}
                            :items []}}}
   :layout {:kind :rows
            :vars {:seite {:type :keyword :default :a :url? true}}
            :items []}})

(defn- seite-of [schema]
  (get-in schema [:applications :r1 :layout :vars :seite]))

(deftest ein-installierter-stand-wird-beim-lesen-gehoben
  (is (= {:type :keyword :default :a :external-id :seite :in-url :always}
         (seite-of (vars/lift-schema-applications {:applications {:r1 alt-app}})))))

(deftest eine-veroeffentlichte-fassung-wird-mitgehoben
  (testing "eine Fassung ist eine eigene Ablage des GANZEN Layouts — sie traegt die
            alte Form also selbst, und der Kopf zu heben koennte sie verfehlen"
    (is (contains? (get-in alt-app [:releases "1" :layout :vars :seite]) :url?)
        "so liegt sie im Bestand"))
  (testing "das Heben ist ein TIEFER Lauf, also erfasst es die Fassungen mit"
    (let [gehoben (vars/lift-schema-applications {:applications {:r1 alt-app}})]
      (is (= {:type :keyword :default :a :external-id :seite :in-url :always}
             (get-in gehoben [:applications :r1 :releases "1" :layout :vars :seite])))))
  (testing "darum bekommt auch der Betrachter die neue Form — published-view tauscht
            den Kopf gegen eine Fassung, und beide sind gehoben"
    (is (= {:type :keyword :default :a :external-id :seite :in-url :always}
           (seite-of (vers/published-view
                      (vars/lift-schema-applications {:applications {:r1 alt-app}}))))))
  (testing "und eine benannte Fassung, wie ?v= sie holt, ebenso"
    (is (= {:type :keyword :default :a :external-id :seite :in-url :always}
           (get-in (vers/at-version
                    (vars/lift-url-flags alt-app) "1")
                   [:layout :vars :seite])))))
