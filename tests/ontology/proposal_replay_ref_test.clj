(ns ^:api ontology.proposal-replay-ref-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.parser.sugar :as sugar]
            [support.in-project :refer [in-project]]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api]))

(use-fixtures :once pod/with-mem-pod-auth)
(use-fixtures :each pod/as-admin)

(def ^:private schema
  {:schema
   (sugar/lower-schema
    (in-project :einkauf
    {:objects {:geschaeftspartner {:primary-key :partner-id
                                   :attributes [[:partner-id :string] [:name :string]]
                                   :match-keys [:partner-id] :title :name
                                   :plural "Geschaeftspartner"}
               :eingangsdokument  {:primary-key :dok-id
                                   :attributes [[:dok-id :string] [:lesung-partner-id :string]
                                                [:lesung-nr :string] [:modell :string]]
                                   :match-keys [:dok-id] :title :dok-id
                                   :plural "Eingangsdokumente"}
               :rechnung
                 {:primary-key :nr
                  :attributes [[:nr :string] [:lieferant-id :string] [:dokument-id :string]]
                  :match-keys [:nr] :title :nr
                  :plural "Rechnungen"}}
     :actions {:partner-anlegen
               {:name :partner-anlegen
                :params {:partner-id {:name "Partner id" :type :string :required true}
                         :name {:name "Name" :type :string :required true}}
                :rules [{:op :create :object-type :geschaeftspartner :id :partner-id
                         :values {:partner-id :partner-id :name :name}}]
                :edits [{:object-type :geschaeftspartner}]}
               :dokument-anlegen
               {:name :dokument-anlegen
                :params {:dok-id {:name "Dok id" :type :string :required true}
                         :lesung-partner-id {:name "Lesung partner id" :type :string}
                         :lesung-nr {:name "Lesung nr" :type :string}}
                :rules [{:op :create :object-type :eingangsdokument :id :dok-id
                         :values {:dok-id :dok-id :lesung-partner-id :lesung-partner-id
                                  :lesung-nr :lesung-nr :modell "grok-4"}}]
                :edits [{:object-type :eingangsdokument}]}
               :rechnung-erfassen
               {:name :rechnung-erfassen
                :params {:nr        {:name "Nr" :type :string :required true}
                         :dokument  {:name "Dokument" :type :eingangsdokument :required true}
                         :lieferant {:name "Lieferant" :type :geschaeftspartner :required true}}
                :rules [{:op :create :object-type :rechnung :id :nr
                         :values {:nr :nr :lieferant-id [:get :lieferant :id]
                                  :dokument-id [:get :dokument :id]}}]
                :edits [{:object-type :rechnung}]}}}))})

(defn- ids [t k]
  (into {} (map (juxt #(get-in % [:values k]) (comp str :id)))
        (:rows (:body (api/list-objects t)))))

(defn- auto-id []
  (some #(when (= "rechnungen-vorschlagen" (:name %)) (:id %))
        (:automations (:body (api/get! :automations)))))

(defn- by-nr [proposals] (into {} (map (juxt #(get-in % [:args :nr]) identity)) proposals))

(deftest a-reading-names-its-supplier-by-key
  (testing "setup: two suppliers, three read documents"
    (is (ok? (api/post! :install schema)))
    (is (ok? (api/post! :staging)))
    (is (ok? (api/mutate :partner-anlegen {:partner-id "4711" :name "Teile Maier"}
                         {:publish? true})))
    (is (ok? (api/mutate :partner-anlegen {:partner-id "0815" :name "Beschlag AG"}
                         {:publish? true})))
    (doseq [[d p n] [["d1" "4711" "R-1"] ["d2" "0815" "R-2"] ["d3" "9999" "R-3"]]]
      (is (ok? (api/mutate :dokument-anlegen {:dok-id d :lesung-partner-id p :lesung-nr n}
                           {:publish? true})))))

  (testing "the replay automation resolves the key to the object when it stages"
    (is (ok? (api/post! :automations {:name "rechnungen-vorschlagen" :on :eingangsdokument
               :effect {:action :rechnung-erfassen :mode :propose
                        :args {:nr {:self :lesung-nr} :dokument {:self :id}
                               :lieferant {:ref :geschaeftspartner :pk {:self :lesung-partner-id}}}
                        :key {:template "dok:{{dok-id}}"}
                        :reason {:template "{{modell}} las {{lesung-nr}}"}
                        :provenance {:model {:self :modell} :source-pk {:self :dok-id}}}})))
    (is (ok? (api/post! :automation-run {:id (auto-id)} nil))))

  (let [gp (ids :geschaeftspartner :partner-id)
        {:keys [proposals counts]} (:body (api/automation-proposals (auto-id) :open))
        ps (by-nr proposals)]
    (testing "one proposal per reading"
      (is (= 3 (get counts "open" (get counts :open)))))
    (testing "the key became the supplier's object id"
      (is (= (gp "4711") (str (get-in ps ["R-1" :args :lieferant]))))
      (is (= (gp "0815") (str (get-in ps ["R-2" :args :lieferant])))))
    (testing "a key no supplier has is left for the reviewer, not an error"
      (is (not (contains? (get-in ps ["R-3" :args]) :lieferant))))
    (testing "the list reads the supplier's title"
      (is (= "Teile Maier" (get-in ps ["R-1" :refs :lieferant :title])))
      (is (= "d1" (get-in ps ["R-1" :refs :dokument :title]))))

    (testing "accepting creates the invoice with both objects linked"
      (is (= "applied"
             (:status (:body (api/post! :automation-proposal-accept
                                        {:id (auto-id) :proposal (get-in ps ["R-1" :id])}
                                        (when (seq nil) {:args nil})))))))
    (testing "the open one is completed by the reviewer — by the supplier's key, as the API allows"
      (let [r (:body (api/accept-proposal (auto-id) (get-in ps ["R-3" :id]) {:lieferant "0815"}))]
        (is (= "applied" (:status r)) (pr-str r))))
    (let [rs (into {} (map (juxt #(get-in % [:values :nr]) :values))
                   (:rows (:body (api/list-objects :rechnung))))]
      (is (= (gp "4711") (get-in rs ["R-1" :lieferant-id])))
      (is (= (gp "0815") (get-in rs ["R-3" :lieferant-id]))))))
