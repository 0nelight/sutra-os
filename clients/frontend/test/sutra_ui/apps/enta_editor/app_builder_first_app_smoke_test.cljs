(ns sutra-ui.apps.enta-editor.app-builder-first-app-smoke-test
  (:require [cljs.test :refer [deftest is testing run-tests]]
            [clojure.string :as str]
            [reagent.dom.server :as rds]
            [sutra.capps.catalog :as cat]
            [sutra.capps.table :as tbl]
            [sutra-ui.render-ctx :as rctx]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.apps.enta-editor.state :as es]
            [sutra-ui.apps.enta-editor.form :as form]
            [sutra-ui.apps.enta-editor.fields :as f]
            [sutra-ui.apps.enta-editor.core :as ec]))

(def ^:private schema-fixture
  {:objects      {:eingangsrechnung [{:name :rechnung-id :type :string}]
                  :bankkonto        [{:name :bankkonto-id :type :string}]}
   :object-types {:eingangsrechnung
                  {:symbol "🧾"
                   :fields [{:name :rechnung-id :type :string}
                            {:name :belegnummer :type :string}
                            {:name :faellig-am :type :date}
                            {:name :intern :type :string :opts {:visibility :hidden}}]}
                  :bankkonto {:symbol "🏦" :fields [{:name :bankkonto-id :type :string}]}}})

(def ^:private actions-fixture
  {:ueberweisung-vorbereiten
   {:display-name "Rechnung bezahlen"
    :params {:rechnung   {:type :eingangsrechnung :name "Rechnung" :required true
                          :prefill :current-object}
             :konto      {:type :bankkonto :name "Von Konto" :required true}
             :vorgang-id {:type :string :exposure :hidden :prefill :uuid}}}})

(def ^:private tree-fixture
  {:kind :rows :title "Neue App"
   :vars {:suche {:default ""} :auswahl {:default nil}}
   :items [{:widget :object-table :rows nil :cols [{:prop nil}]}
           {:widget :action-form}]})

(defn- fresh! []
  (reset! catalog/schema schema-fixture)
  (reset! catalog/canonical-schema {:actions actions-fixture})
  (swap! es/ui assoc :draft {:app nil :file {:name :neue-app :status :draft :layout tree-fixture}
                    :tree tree-fixture :saved nil :data {}
                    :cursor [] :version 1 :loading? false :error nil :staged nil
                    :past [] :future []}))

(deftest picking-an-object-set-binds-its-type
  (fresh!)
  (testing "a table pointed at a type is served that type — without the Overview tab"
    (es/put-param! [:items 0] :rows {:from :eingangsrechnung})
    (is (= [:eingangsrechnung] (:object-types (es/file)))))
  (testing "a VARIABLE named in {:from …} is not a type"
    (es/put-var! [] :offene {:from :suche :where []})
    (is (= [:eingangsrechnung] (:object-types (es/file)))))
  (testing "picking an Action whitelists it"
    (es/put-param! [:items 1] :action :ueberweisung-vorbereiten)
    (is (= [{:action :ueberweisung-vorbereiten}] (:actions (es/file)))))
  (testing "nothing is ever unbound, and one undo takes the binding back with the edit"
    (es/undo!)
    (is (nil? (:actions (es/file))))
    (is (= [:eingangsrechnung] (:object-types (es/file)))))
  (testing "an entry the author wrote keeps its shape"
    (let [f {:name :x :actions [{:action :ueberweisung-vorbereiten :parameters [:konto]}]}]
      (is (= f (cat/bind-dependencies f {:widget :action-form :action :ueberweisung-vorbereiten}
                                      [:eingangsrechnung]))))))

(deftest opening-and-saving-unchanged-binds-nothing
  (fresh!)
  (swap! es/ui update :draft #(assoc % :saved [(:file %) (:tree %)]))
  (let [before (es/enta-file)]
    (is (not (es/dirty?)) "a loaded app that nobody touched is not dirty")
    (is (= before (es/enta-file)))))

(deftest type-and-action-pickers-speak-display-names
  (fresh!)
  (is (= [["bankkonto" "🏦 Bankkonto"] ["eingangsrechnung" "🧾 Eingangsrechnung"]]
         (f/object-type-options)))
  (is (= [["ueberweisung-vorbereiten" "Rechnung bezahlen"]] (form/action-options)))
  (testing "the select shows the label and keeps the API name as its value"
    (let [html (rds/render-to-static-markup
                [form/select-field "eingangsrechnung" (f/object-type-options) identity "…"])]
      (is (str/includes? html "🧾 Eingangsrechnung"))
      (is (str/includes? html "value=\"eingangsrechnung\""))))
  (testing "a stored display name wins over the humanized API name"
    (swap! catalog/schema assoc-in [:object-types :bankkonto :display-name] "Bankverbindung")
    (is (= "🏦 Bankverbindung" (f/type-label :bankkonto)))))

(deftest add-all-properties-adds-what-is-missing
  (fresh!)
  (is (= [:rechnung-id :belegnummer :faellig-am]
         (form/missing-columns :eingangsrechnung [{:prop nil}]))
      "every visible property, in the type's order; a hidden one stays hidden")
  (is (= [:rechnung-id :faellig-am]
         (form/missing-columns :eingangsrechnung [{:prop :belegnummer}])))
  (is (empty? (form/missing-columns nil []))))

(deftest parameter-defaults-list-the-actions-own-parameters
  (fresh!)
  (testing "picked by the name the form shows; server-filled ones are not offered"
    (is (= [[:rechnung "Rechnung"] [:konto "Von Konto"]]
           (mapv (juxt first second) (form/action-params :ueberweisung-vorbereiten)))))
  (testing "a current-object parameter without a default is named — the form hides it"
    (is (= ["Rechnung"] (form/hidden-without-default :ueberweisung-vorbereiten {})))
    (is (empty? (form/hidden-without-default :ueberweisung-vorbereiten
                                             {:rechnung {:val :rechnung-id}})))))

(deftest the-inspector-warns-about-the-hidden-parameter
  (fresh!)
  (es/put-param! [:items 1] :action :ueberweisung-vorbereiten)
  (es/select! [:items 1])
  (es/put-param! [:items 1] :defaults {nil nil})
  (let [html (rds/render-to-static-markup [form/inspector])]
    (is (str/includes? html "steht nicht im Formular"))
    (is (str/includes? html "Von Konto") "the defaults picker lists the declared params")))

(deftest an-empty-builder-has-no-problems
  (swap! es/ui assoc :draft {:app nil :file nil :tree nil :cursor [] :version 0
                    :past [] :future [] :data {}})
  (let [html (rds/render-to-static-markup [ec/app-builder])]
    (is (not (str/includes? html "brauchen noch etwas"))
        "nothing is open, so nothing can be missing")))

(deftest a-date-column-reads-as-a-date
  (rctx/install! "de")
  (let [f   (:fmt (tbl/prop-display {:type :date} :faellig-am))
        ctx (rctx/current)]
    (is (some? f))
    (is (= "15.10.2026" (f (js/Date. 2026 9 15) ctx)))
    (is (not (str/includes? (f (js/Date. 2026 9 15) ctx) "GMT")))))

(defn -main [& _] (run-tests))
