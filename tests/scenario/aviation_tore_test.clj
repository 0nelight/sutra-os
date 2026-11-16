(ns ^:api scenario.aviation-tore-test
  (:require [sutra.catalog.grants :as grants]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [support.in-project :refer [install-in-project]]
            [sutra.access.principals :as pr]
            [support.aviation :refer [ontologie]]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(defn- als-admin [f]
  (binding [tx/*auth-token* (-> (api/post! :login {:username grants/admin-username
                                                   :password pr/default-admin-password})
                                :body :token)]
    (f)))

(defn- mit-welt [f]
  (assert (ok? (install-in-project {:schema (update ontologie :projects assoc :linie {})})))
  (assert (ok? (api/post! :staging)))
  (f))

(use-fixtures :once pod/with-mem-pod-auth als-admin mit-welt)

(defn- versuch*
  [install! schema]
  (let [r (install! {:schema schema})
        c (when (ok? r) (api/post! :staging))
        ok (and (ok? r) (ok? c))
        text (pr-str (:body r) (:body c))]
    (when-not ok (api/delete! :staging))
    {:ok? (boolean ok)
     :text text
     :regeln (set (map keyword (re-seq #"(?<=:rule :)[a-z0-9?-]+" text)))
     :warnungen (vec (get-in r [:body :warnings]))}))

(defn- versuch
  [schema]
  (versuch* install-in-project schema))

(defn- versuch-roh
  [schema]
  (versuch* #(api/post! :install %) schema))

(defn- app [n layout & [extra]]
  {:applications {n (merge {:project :linie :object-type :mangel :layout layout}
                           extra)}})

(deftest apps-an-der-tuer
  (testing "eine App, die einen Namen bindet, den es nicht gibt, wird abgelehnt; ein Interface ist erlaubt"
    (let [r (versuch (app :maengelliste {:object-types [:mangel :gibtsnicht]}))]
      (is (not (:ok? r)))
      (is (re-find #"gibtsnicht" (:text r))))
    (let [r (versuch (app :teileliste
                          {:object-types [:wartbares-teil] :aggregate-types [:pruefbar]}))]
      (is (:ok? r) (:text r)))
    (is (not (:ok? (versuch (app :auswertung
                                 {:object-types [:mangel] :aggregate-types [:auch-nicht]}))))
        "auch die Liste der nur ausgewerteten Typen"))

  (testing "eine App ohne Projekt wird abgelehnt; eine Objektansicht gehört dem Projekt ihres Typs"
    (let [r (versuch-roh {:applications {:ohne-projekt {:layout {}}}})]
      (is (not (:ok? r)))
      (is (re-find #"resource-without-project" (:text r))))
    (is (:ok? (versuch-roh {:applications {:mangel-ansicht {:form-factor :panel
                                                            :object-type :mangel :layout {}}}}))))

  (testing "eine Auswahl, die heißt wie ihr Typ, wird abgelehnt und ein Name vorgeschlagen"
    (let [r (versuch (app :verwechslung {:object-types [:mangel] :vars {:mangel {:default nil}}}))]
      (is (not (:ok? r)))
      (is (re-find #":mangel-gewaehlt" (:text r))))
    (is (:ok? (versuch (app :klar {:object-types [:mangel] :vars {:gewaehlt {:default nil}}}))))
    (is (not (:ok? (versuch (app :innen
                                 {:object-types [:mangel]
                                  :items [{:kind :section :vars {:mangel {:default nil}}}]}))))
        "auch in einem inneren Abschnitt"))

  (testing "getrennte Zielgruppen gehören nicht in eine App; dieselben Leute auf zwei Seiten schon"
    (let [seiten (fn [& ps] {:kind :pages :select :s :pages (vec ps)})]
      (is (not (:ok? (versuch (app :gemischt
                                   (seiten {:id :linie :visibility {:roles [:mechaniker]}}
                                           {:id :kosten :visibility {:roles [:controlling]}}))))))
      (is (:ok? (versuch (app :gemeinsam
                              (seiten {:id :a :visibility {:roles [:mechaniker]}} {:id :b})))))
      (is (:ok? (versuch (app :je-profil
                              {:kind :profiles :select :p
                               :variants [{:id :a :visibility {:profiles [:mechaniker]}}
                                          {:id :b :visibility {:profiles [:controlling]}}]})))
          ":profiles ist der erlaubte Weg, ein Modul je Profil anders zu zeigen")
      (testing "viele Seiten werden nur gemeldet, nicht gesperrt"
        (let [r (versuch (app :viele
                              (apply seiten (for [i (range 8)] {:id (keyword (str "s" i))}))))]
          (is (:ok? r) (:text r))
          (let [befunde (filter #(= :app-page-sprawl (keyword (:rule %)))
                                (:findings (:body (api/get! :schema-lint))))]
            (is (seq befunde) (pr-str (keys (:body (api/get! :schema-lint)))))
            (is (some #(re-find #"8" (str (:message %))) befunde))))))))

(deftest actions-an-der-tuer
  (testing "ein Zahl-Parameter, vorbelegt aus einer Text-Eigenschaft, wird abgelehnt; ein Text-Parameter nimmt jede"
    (let [aktion (fn [typ]
                   {:actions {:kosten-uebernehmen
                              {:params
                                 {:mangel {:name "Mangel" :type :mangel :required true
                                           :prefill :current-object}
                                  :betrag {:name "Betrag" :type typ
                                           :default {:from :mangel :property :beschreibung}}}
                               :rules [{:op :update :object-type :mangel :target [:get :mangel]
                                        :values {:pruefer [:get :betrag]}}]
                               :edits [{:object-type :mangel}]}}})]
      (let [r (versuch (aktion :number))]
        (is (not (:ok? r)))
        (is (re-find #"prefill-type-mismatch" (:text r)))
        (is (re-find #"kosten-uebernehmen" (:text r))))
      (is (:ok? (versuch (aktion :string)))))
    (testing "… eine Zahl-Eigenschaft passt, und ein Objektparameter nimmt den Schlüssel"
      (is (:ok? (versuch {:actions {:stunden-uebernehmen
                                    {:params
                                       {:teil {:name "Teil" :type :triebwerk :required true
                                               :prefill :current-object}
                                        :stunden
                                          {:name "Stunden" :type :number
                                           :default {:from :teil :property :betriebsstunden}}
                                        :lfz {:name "Lfz" :type :luftfahrzeug
                                              :default {:from :teil :property :kennzeichen}}}
                                     :rules [{:op :update :object-type :triebwerk
                                              :target [:get :teil]
                                              :values {:betriebsstunden [:get :stunden]}}]
                                     :edits [{:object-type :triebwerk}]}}}))))))

(defn- lint-befunde [regel]
  (filter #(= regel (keyword (:rule %))) (:findings (:body (api/get! :schema-lint)))))

(deftest ein-handgebautes-formular-wird-beraten
  (let [layout (fn [& extra]
                 {:object-types [:mangel]
                  :vars {:nr {:default nil} :text {:default nil} :gewaehlt {:default nil}
                         :zeilen {:source :mangel}}
                  :items (into [{:widget :text-input :sel {:var :nr}}
                                {:widget :text-input :sel {:var :text}}
                                {:widget :action-button :action :mangel-melden
                                 :params {:mangel-nr {:val :nr} :beschreibung {:val :text}
                                          :luftfahrzeug {:val :gewaehlt}}}]
                               extra)})]
    (let [r (versuch (app :meldeblatt (layout)))] (is (:ok? r) (:text r)))
    (let [[f] (filter #(= "meldeblatt.mangel-melden" (:entity %))
                      (lint-befunde :writeback-form-hand-built))]
      (is (some? f) "zwei eigene Felder für eine Action sind ein Formular")
      (is (= "warn" (name (:severity f))))
      (is (re-find #":action-form" (:message f)) "die Meldung nennt den Ausweg")
      (is (re-find #"\[(:mangel-nr :beschreibung|:beschreibung :mangel-nr)\]" (:message f)))
      (is (not (re-find #"luftfahrzeug" (:message f)))
          "ein Parameter ohne eigenes Feld ist kein Formularfeld"))
    (let [r (versuch (app :meldeblatt
                          (layout {:widget :object-table :rows {:val :zeilen}
                                    :where [[:mangel-nr := {:val :nr}]
                                            [:beschreibung := {:val :text}]]})))]
      (is (:ok? r) (:text r)))
    (is (empty? (filter #(= "meldeblatt.mangel-melden" (:entity %))
                        (lint-befunde :writeback-form-hand-built)))
        "ein Wert, den das Modul auch anderswo liest, ist eine Modulvariable")))

(deftest zum-schluss-jede-beworbene-app-steht-auf-einem-arbeitsbereich
  (let [kachel (fn [n & [extra]]
                 {n (merge {:promoted true :project :linie :layout {}} extra)})]
    (is (:ok? (versuch {:applications (merge (kachel :leitwarte) (kachel :faktura) (kachel :schicht)
                                             (kachel :still {:promoted false})
                                             (kachel :mangel-view-panel
                                                     {:form-factor :panel
                                                      :object-type :mangel}))})))
    (testing "ohne Arbeitsbereich gibt es die Pflicht nicht; ab dem ersten muss jede beworbene App stehen"
      (let [r (api/post! :workspace-file {:name "linie"}
                         {:file {:project :linie :apps [:leitwarte] :home []}})]
        (is (not (ok? r)))
        (is (re-find #"faktura" (pr-str (:body r))))
        (is (not (re-find #"still|mangel-view-panel" (pr-str (:body r))))
            "nicht Beworbenes und Objektansichten stehen auf keinem Portal")
        (api/delete! :staging)))
    (testing ":anchored und :landing wählen nur aus :apps — der Arbeitsbereich ist seine :apps"
      (let [r (api/post! :workspace-file {:name "linie"}
                         {:file {:project :linie :apps [:leitwarte] :anchored [:faktura]
                                 :landing :schicht :home []}})]
        (is (not (ok? r)))
        (is (re-find #"must also be listed in :apps" (pr-str (:body r))))
        (api/delete! :staging))
      (let [r (api/post! :workspace-file {:name "linie"}
                         {:file {:project :linie :apps [:leitwarte :faktura :schicht]
                                 :anchored [:faktura]
                                 :landing :schicht :home []}})]
        (is (ok? r) (pr-str (:body r)))
        (api/post! :staging)))))
