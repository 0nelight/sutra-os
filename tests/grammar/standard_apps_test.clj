(ns ^:unit grammar.standard-apps-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.authoring.install :as install]
            [sutra.parser.presets :as presets]
            [sutra.types.versions :as vers]))

(def ^:private pakete
  {"1_basis.ent" #{}
   "3_privat.ent" #{:rechnungen :zahlungen :vertraege :konten}
   "3_betrieb.ent" #{:rechnungen :zahlungen :vertraege :konten :mahnungen}
   "4_einzelunternehmen.ent" #{:rechnungen :zahlungen :vertraege :konten :mahnungen}})

(deftest die-apps-sind-im-portal-und-bestehen-die-install-tuer
  (doseq [datei (keys pakete)]
    (testing datei
      (let [s    (presets/paket-schema datei)
            plan (install/plan-schema s {} {})]
        (is (not-any? #(= :failed (:outcome %)) plan)
            (pr-str (filter #(= :failed (:outcome %)) plan)))
        (doseq [[k a] (:applications s)
                :let [args (:args (some #(when (= k (:entity %)) %) plan))]]
          (testing k
            (is (vers/published? args)
                "der erste Install schneidet Version 1 — ein Entwurf stuende in keinem Portal")
            (is (:promoted a))
            (is (seq (:display-name a))
                "die Kachel traegt einen Titel, nicht den Griff")
            (is (= (:display-name a) (:display-name args))
                "und die Installation traegt ihn weiter")
            (is (contains? (:collections s) (:collection a))
                "die Ueberschrift, unter der die App im Portal steht, reist mit")))))))

(deftest eine-app-ohne-ihren-typ-reist-nicht-mit
  (is (not (presets/passt? {:objects {:bankkonto {}} :actions {}}
                           {:object-types [:bankkonto :eingangsrechnung]})))
  (is (not (presets/passt? {:objects {:bankkonto {}} :actions {}}
                           {:object-types [:bankkonto] :actions [{:action :buchung-erfassen}]})))
  (is (presets/passt? {:objects {:bankkonto {}} :actions {:buchung-erfassen {}}}
                      {:object-types [:bankkonto] :actions [{:action :buchung-erfassen}]})))

(deftest keine-variable-verdeckt-einen-objekttyp
  (testing "`:from :x` liest eine gleichnamige Variable statt des Objekttyps
            (sutra.capps.variables/spec-value) -- eine Auswahl `:mahnung` machte so die
            Liste aller Mahnungen leer"
    (let [s (presets/paket-schema "4_einzelunternehmen.ent")]
      (doseq [[k a] (:applications s)
              :let [vars  (get-in a [:layout :vars])
                    typen (set (keep :from (vals vars)))]]
        (testing k
          (is (empty? (filter (set (keys vars)) typen))))))))
