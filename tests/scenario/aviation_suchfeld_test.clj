(ns ^:api scenario.aviation-suchfeld-test
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [support.aviation :refer [ontologie]]
            [support.in-project :refer [install-in-project]]
            [support.pod :as pod]
            [support.wire :refer [ok? publish!]]
            [sutra.access.principals :as pr]
            [sutra.catalog.grants :as grants]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(def ^:private namen ["Jannik" "Maren" "Oskar Hafenmeier"])

(def ^:private admin (atom nil))

(defn- tun! [action args] (assert (ok? (api/mutate action args {:publish? true}))))

(defn- zeilen [typ] (:rows (:body (api/list-objects typ))))

(defn- zeilen-schleife [vorbelegt]
  {:kind :loop
     :items {:val :maengel}
     :key :mangel-nr
     :body
     {:kind :toolbar
      :items
      [{:widget :button
        :label "wählen"
        :on-click [{:do :set :to {:item :mangel-nr} :var {:var :gewaehlt}}]}
       {:widget :combo-input
        :class "suchfeld"
        :action :mangel-beheben
        :value vorbelegt
        :value-param :mechaniker
        :params {:mangel {:item :id}}
        :items {:val :kandidaten}
        :item-value :id
        :item-label :name
        :item-search [:name]
        :strict? true}
       {:widget :badge
        :text "gewählt"
        :when [:= {:item :mangel-nr} {:var :gewaehlt}]}]}})

(defn- zuweisung [vorbelegt]
  {:kind :rows
   :object-types [:mangel :mechaniker]
   :action {:action :mangel-beheben :parameters [:mangel :mechaniker]}
   :vars {:maengel    {:source :mangel}
          :kandidaten {:source :mechaniker :limit 2}
          :gewaehlt   {:default nil}}
   :items
   [{:kind :columns
     :items
     [{:kind :rows
       :items
       [(zeilen-schleife vorbelegt)
        {:widget :markdown :text "Was unter den Zeilen steht"}]}]}]})

(defn- mit-welt [f]
  (reset! admin (-> (api/post! :login {:username grants/admin-username
                                       :password pr/default-admin-password})
                    :body :token))
  (binding [tx/*auth-token* @admin]
    (assert (ok? (install-in-project {:schema ontologie})))
    (assert (ok? (api/post! :staging)))
    (tun! :luftfahrzeug-registrieren {:kennzeichen "D-AINA" :muster "A320"})
    (tun! :mechaniker-anlegen {:lizenz "L-1" :name "Jannik" :muster "A320"})
    (tun! :mechaniker-anlegen {:lizenz "L-2" :name "Maren" :muster "A320"})
    (tun! :mechaniker-anlegen {:lizenz "L-3" :name "Oskar Hafenmeier" :muster "A320"})
    (tun! :mangel-melden {:mangel-nr "P-1" :luftfahrzeug "D-AINA" :beschreibung "Leck"
                          :kategorie "C"})
    (assert (ok? (api/mutate :create-application
                             {:name :werft-zuweisung :project :flotte
                              :layout (zuweisung "L-1")}
                             {:stage? true})))
    (assert (ok? (api/post! :staging)))
    (assert (ok? (publish! :werft-zuweisung)))
    (f)))

(use-fixtures :once pod/with-mem-pod-auth mit-welt)

(defn- browser-lauf []
  (let [harness (io/file (System/getProperty "user.dir") "tests/frontend")
        {:keys [out err]}
        (sh/sh "timeout" "90" "node" "suchfeld-zeile.mjs"
               (str (pod/base-url) "/apps/werft-zuweisung")
               (pod/base-url) @admin (json/write-str namen)
               :dir harness
               :env (assoc (into {} (System/getenv))
                           "NODE_PATH" (str (io/file harness "node_modules"))))
        zeile (some #(when (str/starts-with? % "ERGEBNIS ") %) (str/split-lines out))]
    (assert zeile (str "keine Ausgabe des Browsers: " out err))
    (json/read-str (subs zeile (count "ERGEBNIS ")) :key-fn keyword)))

(deftest ein-suchfeld-in-der-zeile-bietet-alles-an-und-schreibt-seine-zeile
  (let [lauf (browser-lauf)]
    (is (nil? (:fehler lauf)) (str lauf))
    (testing "die Vorbelegung steht im Feld"
      (is (= "L-1" (:vorbelegt lauf))))
    (testing "ein Neuzeichnen der Zeile baut das Feld nicht neu auf, eine offene Liste bliebe sonst
              nicht offen"
      (is (true? (:gleichesFeld lauf)) (str lauf)))
    (testing "die offene Liste liegt obenauf, die Schleife schneidet sie nicht ab"
      (is (true? (:listeObenauf lauf)) (str lauf)))
    (testing "der Fokus zeigt die ganze Seite, nicht nur den vorbelegten Wert"
      (is (= 2 (count (:beimFokus lauf))) (str lauf)))
    (testing "wer außerhalb der Seite liegt, findet die Suche auf dem Server"
      (is (some? (:ausserhalbDerSeite lauf)) "die Menge muss gekürzt sein, sonst prüft das nichts")
      (is (some #{(:ausserhalbDerSeite lauf)} (:beimTippen lauf)) (str lauf)))
    (testing "die Wahl schreibt genau diese Zeile"
      (let [mangel (tx/with-token @admin
                     (:values (first (filter #(= "P-1" (get-in % [:values :mangel-nr]))
                                             (zeilen :mangel)))))]
        (is (= "behoben" (:zustand mangel)) (str lauf))
        (is (= (:ausserhalbDerSeite lauf) (:pruefer mangel)))))))
