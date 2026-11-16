(ns ^:api scenario.aviation-notizen-test
  (:require [sutra.catalog.grants :as grants]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [support.in-project :refer [install-in-project]]
            [sutra.access.principals :as pr]
            [support.aviation :refer [ontologie]]
            [support.pod :as pod]
            [support.wire :refer [commit! ok? stage! uid]]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(def ^:private welt
  (-> ontologie
      (assoc-in [:objects :user] {:attributes [[:user-id :string] [:rolle :string]]
                                  :primary-key :user-id :plural "Users"})
      (assoc-in [:actions :benutzer-anlegen]
                {:params {:id {:name "Id" :type :string :required true}
                          :rolle {:name "Rolle" :type :string :required true}}
                 :rules [{:op :create :object-type :user :id :id
                 :values {:user-id [:get :id] :rolle [:get :rolle]}}]
                 :edits [{:object-type :user}]})))

(def ^:private admin (atom nil))

(def ^:private personen
  {"planerin"       {:linienwartung :ontology-viewer}
   "schichtleiter"  {:linienwartung :ontology-editor}
   "werftingenieur" {:linienwartung :ontology-viewer :werftplanung :ontology-editor}
   "qualitaet"      {:linienwartung :ontology-editor :werftplanung :ontology-editor}
   "gast"           {}})

(defn- anmelden [u p] (-> (api/post! :login {:username u :password p}) :body :token))

(defn- mit-welt [f]
  (reset! admin (anmelden grants/admin-username pr/default-admin-password))
  (tx/with-token @admin
    (assert (ok? (install-in-project {:schema welt})))
    (assert (ok? (commit!)))
    (let [ids (into {} (for [u (keys personen)] [u (uid)]))]
      (doseq [[u id] ids]
        (assert (ok? (api/mutate :benutzer-anlegen {:id id :rolle "technik"} {:publish? true})))
        (assert (ok? (api/post! :register {:username u :password u :user-id id}))))
      (doseq [projekt [:linienwartung :werftplanung]]
        (stage! :create-project
                {:name projekt
                 :grants (vec (for [[u rollen] personen :let [r (rollen projekt)] :when r]
                                {:principal (ids u) :role r}))}))
      (assert (ok? (commit!)))))
  (f))

(use-fixtures :once pod/with-mem-pod-auth mit-welt)

(defn- als [u] (if (= u :admin) @admin (anmelden u u)))

(defn- notiz! [body] (-> (api/post! :documents body) :body :document :id str))

(defn- geladen [id]
  (let [b (:body (api/get! :document-of {:id id}))] (:version (or (:document b) b))))

(defn- ids [resp] (set (map (comp str :id) (get-in resp [:body :documents]))))

(defn- verschieben! [ref projekt] (api/post! :kompass-place {:ref ref :project projekt}))

(defn- baum [] (set (map :rid (get-in (api/get! :kompass-nodes) [:body :nodes]))))

(deftest das-projekt-entscheidet-wer-liest-und-bearbeitet
  (let [[linie werft alt] (tx/with-token @admin
                            [(notiz! {:name "Schichtübergabe D-AINA"
                                      :body "Leck Hydraulik grün, Nacharbeit offen"
                                      :project "linienwartung"})
                             (notiz! {:name "C-Check Slot KW 42" :body "Halle 3, zwei Schichten"
                                      :project "werftplanung"})
                             (notiz! {:name "Alte Notizen" :body "vor den Projekten"})])
        [ordner-linie ordner-werft ordner-alt]
        (tx/with-token @admin
          (mapv #(-> (api/post! :kompass-folders %) :body :rid)
                [{:name "Übergaben" :project "linienwartung"}
                 {:name "Slots" :project "werftplanung"}
                 {:name "Ablage"}]))]

    (testing "wer die Linie liest, liest die Werft nicht"
      (tx/with-token (als "planerin")
        (let [r (api/get! :document-of {:id werft})]
          (is (= 403 (:status r)))
          (is (re-find #"Projekt, das du nicht lesen darfst" (str (get-in r [:body :error])))
              "die Ablehnung ist ein deutscher Satz"))
        (is (ok? (api/get! :document-of {:id linie})))
        (is (= #{linie alt} (ids (api/documents))) "die Liste lässt die Werft-Notiz aus")
        (is (= 403 (:status (api/put! :document-of {:id linie} {:body "x"})))
            "lesen heißt nicht bearbeiten")
        (is (= 403 (:status (api/post! :documents {:name "neu" :project "werftplanung"})))
            "und nicht anlegen, wo man keine Rolle hat")
        (let [b (baum)]
          (is (contains? b ordner-linie))
          (is (contains? b ordner-alt))
          (is (not (contains? b ordner-werft)) "der Werft-Ordner fehlt im Baum"))))

    (testing "der Schichtleiter bearbeitet die Übergabe und legt in der Linie an"
      (tx/with-token (als "schichtleiter")
        (let [v (geladen linie)]
          (is (ok? (api/put! :document-of {:id linie}
                             {:name "Schichtübergabe D-AINA" :body "Leck behoben"
                              :expect-version v})))
          (testing "doc-lost-update: wer Version v geladen hat, überschreibt v+1 nicht"
            (let [r (api/put! :document-of {:id linie} {:body "veraltet" :expect-version v})]
              (is (= 409 (:status r)))
              (is (= "Leck behoben" (:body (let [b (:body (api/get! :document-of {:id linie}))]
                                             (or (:document b) b)))))))
          (is (= 400 (:status (api/put! :document-of {:id linie} {:body "ohne Version"})))
              "ein Speichern ohne geladene Version wird nicht geraten"))
        (is (ok? (api/post! :documents {:name "Übergabe Nacht" :project "linienwartung"})))))

    (testing "verschieben verlangt Bearbeiten in beiden Projekten"
      (tx/with-token (als "schichtleiter")
        (is (= 403 (:status (verschieben! (str "document/" linie) "werftplanung")))
            "nur die Quelle"))
      (tx/with-token (als "werftingenieur")
        (is (= 403 (:status (verschieben! (str "document/" linie) "werftplanung")))
            "das Ziel bearbeiten, die Quelle nur lesen"))
      (tx/with-token (als "qualitaet")
        (is (= 400 (:status (verschieben! (str "document/" linie) "")))
            "kein Projekt ist kein Ziel")
        (is (ok? (verschieben! (str "document/" linie) "werftplanung")) "beide bearbeiten"))
      (tx/with-token (als "schichtleiter")
        (is (= 403 (:status (verschieben! ordner-werft "linienwartung")))
            "ein Ordner fragt dasselbe"))
      (tx/with-token (als "qualitaet")
        (is (ok? (verschieben! ordner-linie "werftplanung"))))
      (tx/with-token (als "planerin")
        (is (= 403 (:status (api/get! :document-of {:id linie})))
            "in der Werft sieht die Planerin sie nicht mehr")))

    (testing "was vor den Projekten entstand, bleibt so offen wie es war"
      (tx/with-token (als "gast")
        (is (= #{alt} (ids (api/documents))))
        (is (ok? (api/get! :document-of {:id alt})))
        (is (ok? (api/put! :document-of {:id alt} {:name "Alte Notizen" :body "weiter"
                                                   :expect-version (geladen alt)})))
        (is (ok? (api/post! :documents {:name "Ohne Projekt"}))
            "und ohne Projekt anlegen geht weiter")
        (is (= #{ordner-alt} (set (filter #{ordner-linie ordner-werft ordner-alt} (baum)))))))

    (testing "der Admin liest und verschiebt alles; aus einem Projekt führt kein Weg zurück"
      (tx/with-token @admin
        (is (every? #(ok? (api/get! :document-of {:id %})) [linie werft alt]))
        (is (ok? (verschieben! (str "document/" alt) "linienwartung")))
        (is (= 400 (:status (verschieben! (str "document/" alt) nil)))))
      (tx/with-token (als "gast")
        (is (= 403 (:status (api/get! :document-of {:id alt}))) "jetzt entscheidet das Projekt")))))

(deftest aus-einer-notiz-wird-eine-arbeitskarte
  (tx/with-token @admin
    (let [id (notiz! {:name "Arbeitskarte Radwechsel" :format "markdown"
                      :body "# Radwechsel Hauptfahrwerk\n\nKennzeichen: {{ kennzeichen }}"
                      :project "werftplanung"})]
      (testing "die Vorlage ist eine geplante Änderung, noch nicht in der Ontologie"
        (is (ok? (api/post! :document-templatize {:id id}
                            {:name "radwechsel" :project "linienwartung"})))
        (is (= 404 (:status (api/get! :template-of {:name "radwechsel"})))))
      (testing "übernommen trägt sie den Text der Notiz"
        (is (ok? (commit!)))
        (let [t (:body (api/get! :template-of {:name "radwechsel"}))]
          (is (= "Arbeitskarte Radwechsel" (:title t)))
          (is (re-find #"\{\{ kennzeichen \}\}" (str (:source t))))))
      (testing "ein vergebener Name, ein fehlendes Projekt und ein ungültiger Name werden abgelehnt"
        (is (= 409
               (:status (api/post! :document-templatize {:id id}
                                   {:name "radwechsel" :project "linienwartung"}))))
        (is (= 400
               (:status (api/post! :document-templatize {:id id} {:name "bremsen" :project nil}))))
        (is (= 400
               (:status (api/post! :document-templatize {:id id}
                                   {:name "Rad Wechsel" :project "linienwartung"}))))))))
