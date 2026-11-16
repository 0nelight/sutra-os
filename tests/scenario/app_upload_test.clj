(ns ^:api scenario.app-upload-test
  (:require [sutra.catalog.grants :as grants]
            [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.access.principals :as pr]
            [sutra.types.paths :as paths]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]
            [support.pod :as pod]
            [support.wire :refer [ok? publish!]]))

(use-fixtures :once pod/with-pod-auth pod/with-scratch-blobs)

(defn- col [n t] {:attribute-name n :attribute-type t})
(defn- b64 [^String s] (.encodeToString (java.util.Base64/getEncoder) (.getBytes s "UTF-8")))
(defn- sha256 [^String s]
  (let [md (java.security.MessageDigest/getInstance "SHA-256")]
    (apply str (map #(format "%02x" (bit-and % 0xff)) (.digest md (.getBytes s "UTF-8"))))))

(def ^:private welt
  {:schema
   {:projects {:ablage {}}
    :datasets {:belege {:project :ablage
                        :columns [(col :media_ref :media-reference) (col :dateiname :string)]}}
    :objects  {:beleg   {:project :ablage :primary-key :pk
                         :attributes [[:pk :string] [:dateiname :string]]
                         :plural "Belege"}
               :eingang {:project :ablage :primary-key :datei
                         :attributes [[:datei :string] [:ziel :string]]
                         :plural "Eingänge"}}
    :actions  {:eingang-melden
               {:project :ablage
                :params {:beleg {:name "Beleg" :type :string :required true}
                         :name  {:name "Name" :type :string :required true}
                         :ziel  {:name "Ziel" :type :string :required true}}
                :rules  [{:op :create :object-type :eingang :id :beleg
                          :values {:datei [:get :name] :ziel [:get :ziel]}}]
                :edits  [{:object-type :eingang}]}
               :eingang-gesperrt
               {:project :ablage
                :params {:ziel {:name "Ziel" :type :string :required true}}
                :submission-criteria [{:test {:z [:get :ziel]} :pred [:not= :z "belege"]
                                       :message "Belege nimmt hier niemand an."}]
                :rules  [{:op :create :object-type :eingang :id :ziel
                          :values {:datei [:get :ziel] :ziel [:get :ziel]}}]
                :edits  [{:object-type :eingang}]}}
    :transforms
    {:beleg-aus-ablage {:project :ablage :language :pg-sql
                        :inputs {:b {:kind :dataset :dataset :belege}}
                        :query "SELECT media_ref AS pk, dateiname AS dateiname FROM b"
                        :primary-key :pk
                        :output {:kind :object-type :object-type :beleg}}}}})

(def ^:private app
  {:name :belegeingang :project :ablage :object-type :beleg
   :layout {:object-types [:beleg]
            :upload {:dataset :belege :media-column :media_ref :name-field :dateiname
                     :build [:beleg]
                     :on-upload {:action :eingang-melden
                                 :params {:beleg "{{media-reference}}" :name "{{filename}}"
                                          :ziel "{{dataset}}"}}}}})

(defn- direkt [nm]
  {:name nm :project :ablage :object-type :beleg
   :layout {:object-types [:beleg]
            :upload {:dataset :belege :media-column :media_ref :name-field :dateiname}}})

(def ^:private gesperrt
  (assoc-in (direkt :gesperrt) [:layout :upload :on-upload]
            {:action :eingang-gesperrt :params {:ziel "{{dataset}}"}}))

(defn- app-rid
  ([] (app-rid "belegeingang"))
  ([nm]
   (some (fn [[rid a]] (when (= nm (name (:application-name a))) rid))
         (:applications (:body (api/get! :schema))))))

(defn- hochladen! [rid inhalt]
  (tx/post-json! (paths/url :my-application-upload {:rid rid})
                 (json/write-str {:bytes (b64 inhalt) :dateiname "beleg.pdf"})))

(defn- anmelden [u p] (-> (api/post! :login {:username u :password p}) :body :token))

(defn- beleg-zeilen []
  (let [rid (some (fn [[rid d]] (when (= "belege" (name (:dataset-name d))) rid))
                  (:datasets (:body (api/get! :schema))))]
    (count (:rows (:body (api/get! :dataset-preview {:id rid}))))))

(defn- eingaenge []
  (:rows (:body (api/list-objects :eingang))))

(deftest ein-upload-baut-und-ruft-seine-action
  (tx/with-token (-> (api/post! :login {:username grants/admin-username
                                        :password pr/default-admin-password})
                     :body :token)
    (testing "die Welt und die App installieren"
      (let [r (api/post! :install welt)] (is (ok? r) (pr-str (:body r))))
      (is (ok? (api/post! :staging)))
      (is (ok? (api/mutate :create-application app {:stage? true})))
      (is (ok? (api/post! :staging)))
      (is (ok? (publish! :belegeingang)))
      (is (some? (app-rid))))
    (let [inhalt "%PDF-1.4 ein Beleg"
          r      (tx/post-json! (paths/url :my-application-upload {:rid (app-rid)})
                                (json/write-str {:bytes (b64 inhalt)
                                                 :dateiname "rechnung.pdf"}))]
      (testing "die Datei landet unter ihrer Adresse, und der Build der App läuft"
        (is (= 200 (:status r)) (pr-str (:body r)))
        (is (= (sha256 inhalt) (get-in r [:body :hash])))
        (is (true? (get-in r [:body :build :ok])) (pr-str (:body r))))
      (testing "die :on-upload-Action schreibt mit Adresse, Dateiname und Datensatz"
        (is (true? (get-in r [:body :action :ok])) (pr-str (get-in r [:body :action])))
        (is (= [{:datei "rechnung.pdf" :ziel "belege"}]
               (map #(select-keys (:values %) [:datei :ziel]) (eingaenge))))
        (is (= [(sha256 inhalt)] (map (comp str :id) (eingaenge))))))))

(deftest wer-hochladen-darf
  (let [admin (anmelden grants/admin-username pr/default-admin-password)]
    (tx/with-token admin
      (doseq [[u id] [["viewerin" "u-viewerin"] ["editorin" "u-editorin"]]]
        (is (ok? (api/post! :register {:username u :password u :user-id id}))))
      (is (ok? (api/mutate :update-project
                           {:name :ablage
                            :grants [{:principal "u-viewerin" :role :ontology-viewer}
                                     {:principal "u-editorin" :role :ontology-editor}]}
                           {:stage? true})))
      (is (ok? (api/mutate :create-application (direkt :ablage-direkt) {:stage? true})))
      (is (ok? (api/mutate :create-application gesperrt {:stage? true})))
      (is (ok? (api/post! :staging)))
      (is (ok? (publish! :ablage-direkt)))
      (is (ok? (publish! :gesperrt))))
    (let [[direkt-rid gesperrt-rid] (tx/with-token admin
                                      [(app-rid "ablage-direkt") (app-rid "gesperrt")])]
    (testing "direkt in den Datensatz: die Viewerin öffnet die App, darf aber nicht schreiben"
      (let [vorher (tx/with-token admin (beleg-zeilen))
            r      (tx/with-token (anmelden "viewerin" "viewerin")
                     (hochladen! direkt-rid "nur gelesen"))]
        (is (= 403 (:status r)) (pr-str (:body r)))
        (is (re-find #"Editor" (str (get-in r [:body :error]))))
        (is (= vorher (tx/with-token admin (beleg-zeilen))))))
    (testing "die Editorin darf in den Datensatz schreiben"
      (let [r (tx/with-token (anmelden "editorin" "editorin")
                (hochladen! direkt-rid "von der Editorin"))]
        (is (= 200 (:status r)) (pr-str (:body r)))))
    (testing "über eine Action entscheiden ihre Kriterien — vor dem Landen, auch für den Admin"
      (let [vorher (tx/with-token admin (beleg-zeilen))
            inhalt "abgelehnt, bevor er landet"
            r      (tx/with-token admin (hochladen! gesperrt-rid inhalt))]
        (is (= 422 (:status r)) (pr-str (:body r)))
        (is (re-find #"niemand" (str (get-in r [:body :error]))))
        (is (= vorher (tx/with-token admin (beleg-zeilen))))
        (is (not= 200 (:status (tx/with-token admin
                                 (api/get! :blob {:hash (sha256 inhalt)}))))))))))
