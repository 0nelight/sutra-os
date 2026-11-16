(ns ^:api ontology.standard-package-test
  (:require [sutra.catalog.grants :as grants]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.access.principals :as pr]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(use-fixtures :once pod/with-mem-pod-auth)

(def ^:private paket
  "einzelunternehmen")

(defn- publish [a args] (api/mutate a args {:publish? true}))
(defn- dry     [a args] (api/mutate a args {:dry-run? true}))

(defn- rows [ot] (:rows (:body (api/list-objects ot))))

(defn- row-where [ot k v]
  (first (filter #(= v (get-in % [:values k])) (rows ot))))

(defn- id-where [ot k v]
  (or (:id (row-where ot k v))
      (throw (ex-info (str "no " (name ot) " with " k " = " v) {:ot ot}))))

(defn- refused? [{:keys [status body]} re]
  (and (contains? #{400 422} status) (boolean (re-find re (str (:error body))))))

(defn- passes? [r] (= 200 (:status r)))

(deftest das-standardpaket-installiert-und-seine-actions-halten
  (tx/with-token (-> (api/post! :login
                                {:username grants/admin-username
                                 :password pr/default-admin-password})
                     :body :token)
    (testing "install through the door the solo seed uses"
      (let [r (api/install-package paket)]
        (is (ok? r) (pr-str (:body r))))
      (let [r (api/post! :staging)]
        (is (ok? r) (pr-str (:body r)))))

    (testing "Stammdaten through the package's own actions"
      (is (ok? (publish :organisation-anlegen {:bezeichnung "Stadtwerke" :organisation-id "org-1"
                                               :email "buchhaltung@stadtwerke.example"})))
      (is (ok? (publish :organisation-anlegen
                        {:bezeichnung "Ohne Adresse" :organisation-id "org-2"})))
      (is (ok? (publish :bankkonto-anlegen {:bezeichnung "Giro" :bankkonto-id "k-1"})))
      (let [konto (id-where :bankkonto :bankkonto-id "k-1")
            org   (id-where :organisation :organisation-id "org-1")]
        (is (ok? (publish :buchung-erfassen {:konto konto :betrag -100 :verwendungszweck "Strom"
                                             :buchung-id "b-1"})))
        (is (ok? (publish :buchung-erfassen
                          {:konto konto :betrag 250 :verwendungszweck "Kunde zahlt"
                           :buchung-id "b-2"})))
        (doseq [[nr rid] [["R-1" "r-1"] ["R-2" "r-2"]]]
          (let [r (publish :rechnung-erfassen {:aussteller org :belegnummer nr :betrag 100
                                               :rechnung-id rid})]
            (is (ok? r) (pr-str (:body r)))))))

    (testing "Rechnung abgleichen: nie mehr als offen"
      (let [rechnung (id-where :eingangsrechnung :rechnung-id "r-1")
            buchung  (id-where :bankbuchung :buchung-id "b-1")
            args     {:rechnung rechnung :buchung buchung}]
        (is (refused? (dry :rechnung-ausgleichen (assoc args :betrag 150))
                      #"hoeher als der offene Rest")
            "150 > offen 100")
        (is (passes? (dry :rechnung-ausgleichen (assoc args :betrag 100)))
            "exactly what is open")
        (is (ok? (publish :rechnung-ausgleichen (assoc args :betrag 100 :vorgang-id "a-1"))))
        (is (== 0 (get-in (row-where :eingangsrechnung :rechnung-id "r-1") [:values :offen]))
            "offen = 0, not negative")
        (is (refused? (dry :rechnung-ausgleichen (assoc args :betrag 1))
                      #"bereits beglichen")
            "a second reconciliation of a settled invoice")))

    (testing "Ueberweisung: entwurf -> freigeben -> einreichen"
      (let [rechnung (id-where :eingangsrechnung :rechnung-id "r-2")
            konto    (id-where :bankkonto :bankkonto-id "k-1")]
        (is (refused? (dry :ueberweisung-vorbereiten
                           {:rechnung (id-where :eingangsrechnung :rechnung-id "r-1") :konto konto
                            :iban "AT611904300234573201"})
                      #"bereits beglichen")
            "a settled invoice is not paid twice")
        (is (ok? (publish :ueberweisung-vorbereiten {:rechnung rechnung :konto konto
                                                     :iban "AT611904300234573201"
                                                     :vorgang-id "u-1"})))
        (is (= "Stadtwerke"
               (get-in (row-where :ueberweisung :auftrag-id "u-1") [:values :empfaenger]))
            "the recipient is prefilled from the invoice's issuer")
        (is (ok? (publish :ueberweisung-vorbereiten {:rechnung rechnung :konto konto
                                                     :empfaenger "Stadtwerke GmbH & Co KG"
                                                     :iban "AT611904300234573201"
                                                     :vorgang-id "u-2"})))
        (is (= "Stadtwerke GmbH & Co KG"
               (get-in (row-where :ueberweisung :auftrag-id "u-2") [:values :empfaenger]))
            "a prefill, not a copy: what the payer typed wins")
        (let [u (id-where :ueberweisung :auftrag-id "u-1")]
          (is (refused? (dry :ueberweisung-senden {:ueberweisung u}) #"Nur eine freigegebene")
              "a draft is not sent")
          (is (ok? (publish :ueberweisung-freigeben {:ueberweisung u})))
          (let [v (:values (row-where :ueberweisung :auftrag-id "u-1"))]
            (is (= "freigegeben" (:auftragsstand v)))
            (is (some? (:freigegeben-am v)) "the approval is stamped"))
          (is (refused? (dry :ueberweisung-freigeben {:ueberweisung u}) #"Nur ein Entwurf")
              "approving twice")
          (let [r (dry :ueberweisung-senden {:ueberweisung u})]
            (is (passes? r) (pr-str (:body r))))
          (let [vorher (:values (row-where :ueberweisung :auftrag-id "u-1"))
                r      (publish :ueberweisung-senden {:ueberweisung u})]
            (is (not (ok? r)) "the placeholder payment service is unreachable")
            (is (= vorher (:values (row-where :ueberweisung :auftrag-id "u-1")))
                "a failed submission changes nothing")))))

    (testing "Vertraege und Versicherungen: anlegen, nicht nur kuendigen"
      (let [org (id-where :organisation :organisation-id "org-1")]
        (is (refused? (dry :dauervertrag-anlegen {:bezeichnung "Strom"}) #"Vertragspartner")
            ":vertrag requires its partner")
        (is (ok? (publish :dauervertrag-anlegen {:bezeichnung "Strom" :partner org :art "energie"
                                                 :betrag 80 :dauervertrag-id "dv-1"})))
        (let [v (:values (row-where :dauervertrag :dauervertrag-id "dv-1"))]
          (is (= "org-1" (:partner-id v)))
          (is (= "monatlich" (:takt v)) "the declared default"))
        (is (refused? (dry :dauervertrag-anlegen {:bezeichnung "Strom" :partner org :art "gas"})
                      #"nicht erlaubt")
            "art is one of the property's values")
        (is (ok? (publish :vertrag-kuendigen
                          {:vertrag (id-where :dauervertrag :dauervertrag-id "dv-1")
                           :bis "2026-12-31"})))
        (let [v (:values (row-where :dauervertrag :dauervertrag-id "dv-1"))]
          (is (some? (:gekuendigt-am v)) "stamped today by its prefill")
          (is (some? (:gueltig-bis v)) "the end the cancellation sets"))
        (is (refused? (dry :versicherung-anlegen {:bezeichnung "Haushalt"}) #"Versicherer"))
        (is (ok? (publish :versicherung-anlegen
                          {:bezeichnung "Haushalt" :partner org :sparte "haushalt"
                           :betrag 240 :versicherung-id "vs-1"})))
        (let [v (:values (row-where :versicherung :versicherung-id "vs-1"))]
          (is (= "org-1" (:partner-id v)))
          (is (= "jaehrlich" (:takt v))))))

    (testing "Beitrag: Kanal nur, was gesendet werden kann"
      (is (refused? (dry :beitrag-entwerfen {:kanal "mastodon" :text "Hallo"})
                    #"nicht erlaubt \(erlaubt: x\)")
          "mastodon is a valid beitrag.kanal, but nothing can send it")
      (is (passes? (dry :beitrag-entwerfen {:kanal "x" :text "Hallo"}))))

    (testing "Beitrag: entwurf -> freigeben -> veroeffentlichen"
      (is (ok? (publish :beitrag-entwerfen {:kanal "x" :text "Hallo" :vorgang-id "p-1"})))
      (let [b (id-where :beitrag :auftrag-id "p-1")]
        (is (refused? (dry :beitrag-veroeffentlichen {:beitrag b}) #"Nur ein freigegebener")
            "a draft is not posted")
        (is (ok? (publish :beitrag-freigeben {:beitrag b})))
        (is (= "freigegeben"
               (get-in (row-where :beitrag :auftrag-id "p-1") [:values :auftragsstand])))
        (is (refused? (dry :beitrag-freigeben {:beitrag b}) #"Nur ein Entwurf")
            "approving twice")
        (let [r (dry :beitrag-veroeffentlichen {:beitrag b})]
          (is (passes? r) (pr-str (:body r))))))

    (testing "Betrieb: Kunde und Ausgangsrechnung ueber die eigenen Actions"
      (let [org-1 (id-where :organisation :organisation-id "org-1")
            org-2 (id-where :organisation :organisation-id "org-2")]
        (is (refused? (dry :kunde-anlegen {:kundennummer "K1"}) #"Organisation"))
        (is (ok? (publish :kunde-anlegen {:organisation org-1 :kundennummer "K1"
                                          :kundenbeziehung-id "kb-1"})))
        (is (ok? (publish :kunde-anlegen {:organisation org-2 :kundennummer "K2"
                                          :kundenbeziehung-id "kb-2"})))
        (is (= 14 (get-in (row-where :kundenbeziehung :kundenbeziehung-id "kb-1")
                          [:values :zahlungsziel-tage])))
        (is (= "Stadtwerke" (get-in (row-where :kundenbeziehung :kundenbeziehung-id "kb-1")
                                    [:values :kunde-name]))
            "the role reads its party's name, it does not copy it"))
      (is (refused? (dry :ausgangsrechnung-erfassen {:belegnummer "A-1" :betrag 200}) #"Kunde"))
      (doseq [[nr rid kb betrag] [["A-1" "ar-1" "kb-1" 200] ["A-2" "ar-2" "kb-2" 50]]]
        (is (ok? (publish :ausgangsrechnung-erfassen
                          {:kunde (id-where :kundenbeziehung :kundenbeziehung-id kb)
                           :belegnummer nr :betrag betrag :rechnung-id rid}))))
      (is (== 200 (get-in (row-where :ausgangsrechnung :rechnung-id "ar-1") [:values :offen]))))

    (testing "Betrieb: Zahlungseingang nie mehr als offen, Mahnung mit Freigabe"
      (let [eingang (id-where :bankbuchung :buchung-id "b-2")
            args    {:rechnung (id-where :ausgangsrechnung :rechnung-id "ar-1") :buchung eingang}]
        (is (refused? (dry :zahlungseingang-ausgleichen (assoc args :betrag 250))
                      #"hoeher als der offene Rest"))
        (is (passes? (dry :zahlungseingang-ausgleichen (assoc args :betrag 200)))))
      (is (refused? (dry :mahnung-entwerfen
                         {:rechnung (id-where :ausgangsrechnung :rechnung-id "ar-2")})
                    #"E-Mail")
          "no address, no draft -- it could never go out")
      (is (ok? (publish :mahnung-entwerfen
                        {:rechnung (id-where :ausgangsrechnung :rechnung-id "ar-1")
                         :vorgang-id "m-1"})))
      (let [v (:values (row-where :mahnung :auftrag-id "m-1"))]
        (is (= "Stadtwerke" (:empfaenger v)) "prefilled from the customer")
        (is (= "buchhaltung@stadtwerke.example" (:empfaenger-email v)))
        (is (= "A-1" (:belegnummer v))))
      (let [m (id-where :mahnung :auftrag-id "m-1")]
        (is (refused? (dry :mahnung-senden {:mahnung m}) #"Nur eine freigegebene")
            "a draft is not sent")
        (is (ok? (publish :mahnung-freigeben {:mahnung m})))
        (is (= "freigegeben"
               (get-in (row-where :mahnung :auftrag-id "m-1") [:values :auftragsstand])))
        (is (refused? (dry :mahnung-freigeben {:mahnung m}) #"Nur ein Entwurf"))
        (let [r (dry :mahnung-senden {:mahnung m})]
          (is (passes? r) (pr-str (:body r))))
        (let [vorher (:values (row-where :mahnung :auftrag-id "m-1"))
              r      (publish :mahnung-senden {:mahnung m})]
          (is (not (ok? r)) "the placeholder delivery service is unreachable")
          (is (= vorher (:values (row-where :mahnung :auftrag-id "m-1")))
              "a failed send leaves the dunning notice as it was"))))))
