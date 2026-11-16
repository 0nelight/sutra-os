(ns ^:api ontology.api-name-rename-test
  (:require [support.system :as system]
            [support.wire :refer [ok?]]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.parser.sugar :as sugar]
            [sutra.parser.project-emit :as pemit]
            [sutra.store.seat]
            [sutra.catalog.actions :as actions]
            [sutra.catalog.lookup :as lookup]
            [sutra.types.rid :as rid]
            [sutra.wire.api :as api]
            [sutra.authoring.mutation :as mutation]
            [sutra.authoring.install.world :as onto-install]
            [support.in-project :refer [in-project]]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-pod)

(defn- install! [schema]
  (let [r (api/post! :install {:schema (sugar/lower-schema (in-project :akten schema))})]
    (is (ok? r) (pr-str (:body r)))
    (is (ok? (api/post! :staging)) "commit")))

(defn- object-of [t id]
  (some #(when (= (str id) (str (:id %))) %) (:rows (:body (api/list-objects t)))))

(def ^:private vorgang
  {:objects {:vorgang {:primary-key :nr :attributes [[:nr :string] [:titel :string]]
                       :plural "Vorgänge"}}
   :actions
   {:vorgang-anlegen
    {:name   :vorgang-anlegen
     :params {:nr {:name "Nr" :type :string :required true}
              :titel {:name "Titel" :type :string :required true}}
     :rules  [{:op :create :object-type :vorgang :id :nr :values {:nr :nr :titel :titel}}]
     :edits  [{:object-type :vorgang}]}
    :vorgang-umbenennen
    {:name   :vorgang-umbenennen
     :params {:vorgang {:name "Vorgang" :type :vorgang :required true}
              :titel {:name "Titel" :type :string :required true}}
     :rules  [{:op :update :object-type :vorgang :target :vorgang :values {:titel :titel}}]
     :edits  [{:object-type :vorgang}]}}})

(deftest ein-umbenannter-typ-nimmt-seine-action-parameter-mit
  (install! vorgang)
  (let [oid (-> (api/mutate :vorgang-anlegen {:nr "V-1" :titel "alt"}
                            {:publish? true :return-edits? true})
                :body :edits :edits first :object-id)
        t-rid (lookup/rid-of (system/seated-schema) :object-types :vorgang)]
    (is (some? oid) "setup: ein Objekt")
    (testing "der Parameter haelt die RID, nicht den Namen"
      (let [sch    (system/seated-schema)
            params (get-in sch [:actions (lookup/rid-of sch :actions :vorgang-umbenennen)
                                :params])]
        (is (= t-rid (get-in params [:vorgang :type])))
        (is (= :string (get-in params [:titel :type])) "ein primitiver Typ bleibt, was er ist")))

    (is (ok? (api/rename :object-type :vorgang :akte)) "umbenennen ueber die Tuer")

    (let [sch (system/seated-schema)]
      (testing "die RID bleibt, der Name ist neu"
        (is (= t-rid (lookup/rid-of sch :object-types :akte)))
        (is (nil? (lookup/rid-of sch :object-types :vorgang))))
      (testing "der Parameter liest sich mit dem neuen Namen"
        (is (= :akte
               (get-in (actions/resolve-action sch :vorgang-umbenennen) [:params :vorgang :type])))
        (is (= :akte
               (get-in (rid/name-keyed sch) [:actions :vorgang-umbenennen :params :vorgang :type]))
            "auch in der Sicht, die das Frontend bekommt"))
      (testing "das eigene Backing-Dataset heisst wie der Typ"
        (let [ds (get-in sch [:object-types t-rid :dataset])]
          (is (= :akte (get-in sch [:datasets ds :dataset-name])))))
      (testing "Export und Projektion nennen den neuen Namen"
        (let [ent (:ent (pemit/export sch {:all-projects? true :inline? true}))]
          (is (= :akte (get-in ent [:schema :actions :vorgang-umbenennen :params :vorgang :type])))
          (is (= :akte (get-in ent [:schema :actions :vorgang-umbenennen :rules 0 :object-type])))))
      (testing "die Formularauswertung kennt den neuen Typ"
        (let [b (:body (api/post! :action-params nil
                                  (cond-> {:action :vorgang-umbenennen :args {:vorgang oid}}
                                    (seq nil) (assoc :search nil))))]
          (is (not (:error b)) (pr-str b)))))

    (testing "die Action laeuft mit einem Objekt des umbenannten Typs"
      (let [r (api/mutate :vorgang-umbenennen {:vorgang oid :titel "neu"} {:publish? true})]
        (is (ok? r) (pr-str (:body r))))
      (is (= "neu" (get-in (object-of :akte oid) [:values :titel]))))))

(defn- konto [waehrung]
  {:objects {:konto {:primary-key :nr :attributes [[:nr :string] [waehrung :string]]
                     :plural "Konten"}}
   :actions
   {:konto-anlegen
    {:name   :konto-anlegen
     :params {:nr {:name "Nr" :type :string :required true}
              :w {:name "W" :type :string :required true}}
     :rules  [{:op :create :object-type :konto :id :nr :values {:nr :nr waehrung :w}}]
     :edits  [{:object-type :konto}]}
    :waehrung-setzen
    {:name   :waehrung-setzen
     :params {:konto {:name "Konto" :type :konto :required true}
              :w {:name "W" :type :string :required true}}
     :rules  [{:op :update :object-type :konto :target :konto :values {waehrung :w}}]
     :edits  [{:object-type :konto}]}}})

(deftest eine-umbenannte-eigenschaft-behaelt-ihre-edits
  (install! (konto :waehrung_code))
  (let [oid (-> (api/mutate :konto-anlegen {:nr "K-1" :w "ATS"}
                            {:publish? true :return-edits? true})
                :body :edits :edits first :object-id)]
    (is (ok? (api/mutate :waehrung-setzen {:konto oid :w "EUR"} {:publish? true})) "Nutzer-Edit")
    (is (= "EUR" (get-in (object-of :konto oid) [:values :waehrung_code])) "setup")

    (testing "derselbe Spaltenname, neuer API-Name: kein Tor lehnt ab"
      (install! (konto :waehrung-code)))

    (testing "das Objekt zeigt den Edit unter dem neuen Namen"
      (let [o (object-of :konto oid)]
        (is (= "EUR" (get-in o [:values :waehrung-code])))
        (is (not (contains? (:values o) :waehrung_code)))))

    (testing "die Edit-Herkunft und die Historie tragen den neuen Namen"
      (let [{:keys [history provenance] :as h} (:body (api/edit-history :konto))
            s (pr-str h)]
        (is (= ["EUR" "ATS"] (keep #(get-in % [:edited-values :waehrung-code]) history))
            "beide Eintraege, die Anlage und der Edit, unter dem neuen Namen")
        (is (= "EUR" (get-in (first provenance) [:values :waehrung-code :v]))
            "der stehende Edit, den ein Abgleich wieder anwendet")
        (is (not (re-find #":waehrung_code" s)) s)))

    (testing "die Historie eines Objekts nennt die Action und den Wert davor"
      (let [{:keys [history timeline total transactions provenance]}
            (:body (api/edit-history :konto {:pk oid :timeline true :limit 1}))]
        (is (= 2 total))
        (is (= "waehrung-setzen" (:action (first history))))
        (is (= {:waehrung-code "ATS"} (:previous-values (first history))))
        (is (= 2 (count (:edits timeline))))
        (is (nil? transactions) "ein Objekt liest die Kette des Typs nicht mit")
        (is (nil? provenance))))

    (testing "die naechste Aenderung setzt auf dem umbenannten Edit auf"
      (is (ok? (api/mutate :waehrung-setzen {:konto oid :w "CHF"} {:publish? true})))
      (is (= "CHF" (get-in (object-of :konto oid) [:values :waehrung-code]))))))

(deftest ein-parameter-der-noch-den-namen-traegt-wird-vor-dem-umbenennen-aufgeloest
  (install! vorgang)
  (let [sch   (system/seated-schema)
        t-rid (lookup/rid-of sch :object-types :vorgang)
        alt   (assoc-in sch [:actions :vorgang-umbenennen :params :vorgang :type] "vorgang")
        neu   (mutation/renamed-schema alt :object-types :vorgang :vorgang-neu)]
    (is (= t-rid (get-in neu [:actions :vorgang-umbenennen :params :vorgang :type]))
        "ein vor dem Deploy gespeicherter Name wird mit dem ALTEN Namen aufgeloest, nicht verwaist")
    (is (= :vorgang-neu
           (get-in (rid/name-keyed neu) [:actions :vorgang-umbenennen :params :vorgang :type])))))

(deftest ein-umbenannter-typ-nimmt-seine-objektansichten-mit
  (install! vorgang)
  (let [sch   (system/seated-schema)
        t-rid (lookup/rid-of sch :object-types :vorgang)
        app   {:application-name :vorgang-view-panel :form-factor :panel :object-type t-rid
               :layout {}}
        neu   (mutation/renamed-schema (assoc-in sch [:applications :ri.test.view] app)
                                       :object-types :vorgang :mappe)]
    (is (= :mappe-view-panel (get-in neu [:applications :ri.test.view :application-name]))
        "der Name einer Objektansicht ist abgeleitet, also folgt er dem Typ")))

(deftest ein-umbenannter-typ-nimmt-namensbezuege-in-apps-und-python-mit
  (install! vorgang)
  (let [sch    (system/seated-schema)
        layout {:object-types [:vorgang :anderes]
                :vars  {:offen {:source :vorgang :where [[:= :titel "x"]]}
                        :alle  {:from "vorgang"}}
                :items [{:kind :object-type :object-type :vorgang}
                        {:widget :object-table :rows {:from :offen}}]}
        shadow {:vars {:vorgang {:default 1}} :items [{:widget :kpi :value {:from :vorgang}}]}
        code   (str "@transform(inputs={\"v\": Input(kind=\"object-type\","
                    " object_type=\"vorgang\")},\n"
                    "  output=Output(kind=\"object-type\", object_type='vorgang'))\n"
                    "def f(v): return v  # vorgang_alt, object_type=\"vorgangsart\"\n")
        neu    (-> sch
                   (assoc-in [:applications :ri.test.app]
                             {:application-name :liste :layout layout
                              :sidebar [{:object-type :vorgang}]})
                   (assoc-in [:applications :ri.test.shadow]
                             {:application-name :schatten :layout shadow})
                   (assoc-in [:transforms :ri.test.t] {:function-name :f :language :python
                                                       :code code})
                   (mutation/renamed-schema :object-types :vorgang :mappe))
        app    (get-in neu [:applications :ri.test.app])]
    (testing "die Typlisten, :source, :object-type und :from einer App"
      (is (= [:mappe :anderes] (get-in app [:layout :object-types])))
      (is (= :mappe (get-in app [:layout :vars :offen :source])))
      (is (= "mappe" (get-in app [:layout :vars :alle :from])) "ein String bleibt ein String")
      (is (= :mappe (get-in app [:layout :items 0 :object-type])))
      (is (= :offen (get-in app [:layout :items 1 :rows :from])) "eine Variable bleibt")
      (is (= [{:object-type :mappe}] (:sidebar app))))
    (testing "ein :from, das eine gleichnamige Variable meint, bleibt"
      (is (= shadow (get-in neu [:applications :ri.test.shadow :layout]))))
    (testing "Python: object_type=\"vorgang\" folgt, ein anderer Name nicht"
      (let [c (get-in neu [:transforms :ri.test.t :code])]
        (is (re-find #"object_type=\"mappe\"\)" c))
        (is (re-find #"object_type='mappe'" c))
        (is (re-find #"object_type=\"vorgangsart\"" c))
        (is (re-find #"vorgang_alt" c))))))

(deftest ein-backing-dataset-ohne-project-folgt-dem-projekt-seines-typs
  (install! vorgang)
  (let [sch   (system/seated-schema)
        t-rid (lookup/rid-of sch :object-types :vorgang)
        ds    (get-in sch [:object-types t-rid :dataset])
        alt   (update-in sch [:datasets ds] dissoc :project)
        neu   (mutation/renamed-schema alt :object-types :vorgang :mappe)
        {:keys [ok invalid]} (onto-install/prepare-schema neu {:old-schema alt})]
    (is (= :object-backing (get-in alt [:datasets ds :kind])) "setup")
    (is (nil? (get-in alt [:datasets ds :project])) "setup: ein Altbestand ohne :project")
    (is (some? ok) (pr-str invalid))
    (is (= :mappe (get-in ok [:datasets ds :dataset-name])))))
