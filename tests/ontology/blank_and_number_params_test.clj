(ns ^:api ontology.blank-and-number-params-test
  (:require [sutra.catalog.grants :as grants]
            [support.system :as tsys]
            [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.access.principals :as pr]
            [support.pod :as pod]
            [support.in-project :refer [in-project]]
            [support.seed :as seed]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(use-fixtures :once pod/with-mem-pod-auth)

(def ^:private lesen-source
  "(defn beleg-lesen [dokument]
     (let [l (OntologyClient/use-llm
               {:model \"grok-test\" :source :xai
                :prompt \"Lies den Beleg {{dokument.betreff}}.\"
                :output {:struct {:datum :string :faellig :string :netto :number}}
                :max-iterations 2})]
       (OntologyClient/apply-action :lesung-anlegen
                                    {:dokument dokument :datum (:datum l)
                                     :faellig (:faellig l) :netto (:netto l)})))")

(def ^:private schema
  {:objects
   {:eingangsdokument {:name "Eingangsdokument"
                       :primary-key :dok-id :attributes [[:dok-id :string] [:betreff :string]]
                       :plural "Eingangsdokumente"}
    :lesung           {:name "Lesung"
                       :primary-key :lesung-id
                       :attributes [[:lesung-id :string] [:dokument-ref :string] [:datum :date]
                                    [:faellig :date] [:netto :double] [:anzahl :integer]]
                       :plural "Lesungen"}}
   :functions
   {:beleg-lesen {:name :beleg-lesen :language :clojure :target-type :lesung
                  :param-types {:dokument :eingangsdokument}
                  :source lesen-source}}
   :actions
   {:lesung-anlegen
    {:name   :lesung-anlegen
     :params {:dokument {:name "Dokument" :type :eingangsdokument :required true}
              :id       {:type :string :prefill :uuid :exposure :hidden}
              :datum    {:name "Datum" :type :date}
              :faellig  {:name "Faellig" :type :date}
              :netto    {:name "Netto" :type :number}
              :anzahl   {:name "Anzahl" :type :number}}
     :rules  [{:op :create :object-type :lesung :id [:get :id]
               :values {:lesung-id [:get :id] :dokument-ref [:get :dokument :id]
                        :datum [:get :datum] :faellig [:get :faellig]
                        :netto [:get :netto] :anzahl [:get :arg0 :anzahl]}}]
     :edits  [{:object-type :lesung}]}
    :lesung-datiert
    {:name   :lesung-datiert
     :params {:dokument {:name "Dokument" :type :eingangsdokument :required true}
              :id       {:type :string :prefill :uuid :exposure :hidden}
              :datum    {:type :date :required true :name "Belegdatum"}}
     :rules  [{:op :create :object-type :lesung :id [:get :id]
               :values {:lesung-id [:get :id] :dokument-ref [:get :dokument :id]
                        :datum [:get :datum]}}]
     :edits  [{:object-type :lesung}]}
    :vom-modell-lesen
    {:name   :vom-modell-lesen
     :params {:dokument {:name "Dokument" :type :eingangsdokument :required true}}
     :rules  [{:op :apply-edit-function :function :beleg-lesen}]
     :edits  [{:object-type :lesung}]}}})

(defn- answer [v]
  {:tool_calls [{:id "c1" :type "function"
                 :function {:name "final_answer" :arguments (json/write-str {"value" v})}}]})

(defn- with-model [reply f]
  (tsys/with-provided {:llm/complete (fn [_] {:request-id "req-1" :message (answer reply)})}
    (f)))

(defn- lesung [id]
  (some #(when (= id (get-in % [:values :lesung-id])) (:values %))
        (:rows (:body (api/list-objects :lesung)))))

(defn- admin-token []
  (-> (api/post! :login {:username grants/admin-username :password pr/default-admin-password}) :body
      :token))

(defonce ^:private installed (atom false))

(defn- setup! []
  (when-not @installed
    (is (ok? (seed/identity-model!)))
    (is (ok? (api/post! :install {:schema (in-project :belege schema)})))
    (is (ok? (api/post! :staging)))
    (is (ok? (api/mutate :create-llm-model {:name "grok-test" :in 0.1 :out 0.1} {:stage? true})))
    (is (ok? (api/post! :staging)))
    (seed/object! :eingangsdokument "d1" {:dok-id "D-1" :betreff "Mail von Teile Maier"})
    (reset! installed true)))

(deftest a-blank-date-is-absent-and-a-number-takes-its-columns-type
  (tx/with-token (admin-token)
    (setup!)
    (testing "submit: \"\" for an optional date writes null, 0 for a double writes 0.0"
      (let [r (api/mutate :lesung-anlegen {:dokument "d1" :id "l-1" :datum "" :faellig "  "
                                           :netto 0 :anzahl 3.0}
                            {:publish? true})]
        (is (ok? r) (pr-str (:body r))))
      (let [l (lesung "l-1")]
        (is (some? l) "the row was written")
        (is (nil? (:datum l)))
        (is (nil? (:faellig l)))
        (is (== 0 (:netto l)))
        (is (== 3 (:anzahl l)) "3.0 for an integer column is the integer 3, through :arg0 too")))

    (testing "the plan already carries the column's representation"
      (let [{:keys [status body]} (api/mutate :lesung-anlegen {:dokument "d1" :id "l-x" :netto 0}
                                              {:dry-run? true})
            a (:after (first (:plan body)))]
        (is (= 200 status) (pr-str body))
        (is (float? (:netto a)) (pr-str (:netto a)))))

    (testing "a non-integral value for an integer column is refused at the parameter"
      (let [{:keys [status body]} (api/mutate :lesung-anlegen {:dokument "d1" :id "l-2" :anzahl 2.5}
                                              {:publish? true})]
        (is (= 400 status) (pr-str body))
        (is (re-find #"(?i)anzahl" (str (:error body))) (pr-str body))
        (is (nil? (lesung "l-2")))))

    (testing "\"\" for a REQUIRED date is a missing parameter, not a 500"
      (let [{:keys [status body]} (api/mutate :lesung-datiert {:dokument "d1" :id "l-3" :datum ""}
                                              {:publish? true})]
        (is (= 422 status) (pr-str body))
        (is (re-find #"Belegdatum" (str (:error body))) (pr-str body))
        (is (nil? (lesung "l-3")))))

    (testing "through OntologyClient/apply-action from a Logic function"
      (let [before (count (:rows (:body (api/list-objects :lesung))))
            r      (with-model {"datum" "" "faellig" "" "netto" 0}
                     #(api/mutate :vom-modell-lesen {:dokument "d1"} {:publish? true}))]
        (is (ok? r) (pr-str (:body r)))
        (let [rows (:rows (:body (api/list-objects :lesung)))
              neu  (last (sort-by #(str (get-in % [:values :lesung-id]))
                                  (remove #(#{"l-1"} (get-in % [:values :lesung-id])) rows)))]
          (is (= (inc before) (count rows)) "one Lesung written")
          (is (nil? (get-in neu [:values :datum])))
          (is (== 0 (get-in neu [:values :netto]))))))))
