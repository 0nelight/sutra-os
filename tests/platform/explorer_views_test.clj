(ns ^:api platform.explorer-views-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [support.in-project :refer [install-in-project]]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.foundation.settings :as settings]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(defn- mit-typ [f]
  (assert (ok? (install-in-project :ansicht
                                   {:schema
                                      {:objects
                                         {:geraet {:primary-key :nr :attributes [[:nr :string]]
                                                   :plural "Geraete"}}}})))
  (assert (ok? (api/post! :staging)))
  (f))

(use-fixtures :once pod/with-mem-pod mit-typ)

(defn- abfrage [] {:find :geraet :where {}})

(defn- namen []
  (set (map :name (get-in (api/get! :explorations {:object-type :geraet})
                          [:body :explorations]))))

(deftest a-file-is-written-not-staged
  (let [v  (api/post! :explorations
                      {:object-type :geraet :kind :exploration :name "Alle"
                       :query (abfrage) :state {:viz :table :columns [:nr]}})
        id (get-in v [:body :id])]
    (testing "saved at once, nothing staged, private without a project"
      (is (ok? v) (pr-str (:body v)))
      (is (true? (get-in v [:body :private])))
      (is (= #{"Alle"} (namen)))
      (is (empty? (get-in (api/get! :staging) [:body :staged]))))
    (testing "a branch sees the same files: they ride on no branch"
      (is (ok? (api/post! :branches {:parent "main" :name "ansicht-zweig"})))
      (is (= #{"Alle"} (tx/with-branch "ansicht-zweig" (namen)))))
    (testing "two files may share a name, as in two folders"
      (let [zweite (api/post! :explorations {:object-type :geraet :kind :list :name "Alle"
                                              :query (abfrage)})]
        (is (ok? zweite) (pr-str (:body zweite)))
        (is (not= id (get-in zweite [:body :id])))
        (is (ok? (api/delete! :exploration {:id (get-in zweite [:body :id])})))))
    (testing "it selects like an object set"
      (is (ok? (api/post! :ontology-query {:object-set id}))))
    (testing "renamed and deleted in place, its layout retained"
      (is (= "Alle Geräte"
             (get-in (api/put! :exploration {:id id} {:name "Alle Geräte"}) [:body :name])))
      (is (= {:viz :table :columns [:nr]}
             (get-in (api/get! :exploration {:id id}) [:body :state])))
      (is (ok? (api/delete! :exploration {:id id})))
      (is (= #{} (namen)))
      (is (= 404 (:status (api/get! :exploration {:id id})))))
    (testing "a file of a type that does not exist is refused"
      (is (= 400
             (:status (api/post! :explorations
                                 {:object-type :gibtsnicht :kind :exploration :name "x"
                                  :query {:find :gibtsnicht}})))))))

(deftest a-dashboard-layout-is-written-not-staged
  (is (= 404 (:status (api/get! :explorer-dashboard {:object-type "geraet"}))))
  (is (ok? (api/put! :explorer-dashboard {:object-type "geraet"} {:layout {:charts [:nr]}})))
  (is (= {:charts [:nr]}
         (get-in (api/get! :explorer-dashboard {:object-type "geraet"}) [:body :layout])))
  (is (empty? (get-in (api/get! :staging) [:body :staged]))))

(defn- ordner! [nm projekt]
  (get-in (api/post! :kompass-folders {:name nm :project projekt}) [:body :rid]))

(defn- datei! [nm extra]
  (api/post! :explorations (merge {:object-type :geraet :kind :exploration :name nm
                                   :query (abfrage) :state {}}
                                  extra)))

(defn- knoten-zu [ref]
  (some #(when (= ref (:ref %)) %) (get-in (api/get! :kompass-nodes) [:body :nodes])))

(deftest a-folder-is-a-location
  (let [ordner (ordner! "Ablage" "ansicht")
        id     (get-in (datei! "Im Ordner" {}) [:body :id])
        ref    (str "exploration/" id)]
    (testing "placing a private file in a folder makes it public in the folder's project"
      (is (ok? (api/post! :kompass-place {:ref ref :parent-rid ordner})))
      (is (= ordner (:parent-rid (knoten-zu ref))))
      (let [f (:body (api/get! :exploration {:id id}))]
        (is (false? (:private f)))
        (is (some? (:project f)))))
    (testing "a file does not lie at the top of nothing"
      (is (= 400 (:status (api/post! :kompass-place {:ref ref :parent-rid nil})))))
    (testing "made private it leaves the folder"
      (is (ok? (api/put! :exploration {:id id} {:private true})))
      (is (nil? (knoten-zu ref))))
    (testing "deleted it leaves the folder too"
      (is (ok? (api/post! :kompass-place {:ref ref :parent-rid ordner})))
      (is (ok? (api/delete! :exploration {:id id})))
      (is (nil? (knoten-zu ref))))))

(deftest favorites-belong-to-the-index
  (let [ref "exploration/fav"]
    (is (ok? (api/post! :kompass-favorites {:ref ref :on true})))
    (is (some #{ref} (get-in (api/get! :kompass-index) [:body :favorites])))
    (is (ok? (api/post! :kompass-favorites {:ref ref :on false})))
    (is (not-any? #{ref} (get-in (api/get! :kompass-index) [:body :favorites])))))

(deftest a-project-lies-in-one-portfolio
  (let [neu (fn [nm projekte]
              (get-in (api/post! :kompass-portfolios {:name nm :projects projekte})
                      [:body :rid]))
        a   (neu "A" ["ansicht"])
        b   (neu "B" ["ansicht"])
        von (fn [rid] (some #(when (= rid (:rid %)) %)
                            (get-in (api/get! :kompass-index) [:body :portfolios])))]
    (is (empty? (:projects (von a))))
    (is (= 1 (count (:projects (von b)))))
    (is (ok? (api/delete! :kompass-portfolio {:rid a})))
    (is (nil? (von a)))))

(deftest the-index-dates-what-a-commit-changed
  (let [{:keys [rights home-folders]} (:body (api/get! :kompass-index))]
    (is (true? home-folders))
    (is (:read? (get rights "ansicht")))))

(deftest home-folders-can-be-turned-off
  (with-redefs [settings/value (let [v settings/value]
                                 (fn [k] (if (= :home-folders k) false (v k))))]
    (testing "a file with no project is refused, one in a project is not"
      (is (= 400 (:status (datei! "Privat" {}))))
      (is (ok? (datei! "Im Projekt" {:project "ansicht"}))))
    (is (false? (get-in (api/get! :kompass-index) [:body :home-folders])))))

(deftest a-collection-holds-refs
  (let [rid (get-in (api/post! :kompass-collections {:name "Kern" :refs ["document/1"]})
                    [:body :rid])
        von #(some (fn [c] (when (= rid (:rid c)) c))
                   (get-in (api/get! :kompass-index) [:body :collections]))]
    (is (= ["document/1"] (:refs (von))))
    (is (ok? (api/put! :kompass-collection {:rid rid} {:refs ["document/2"]})))
    (is (= ["document/2"] (:refs (von))))
    (is (ok? (api/delete! :kompass-collection {:rid rid})))
    (is (nil? (von)))))

(defn- b64 [s]
  (.encodeToString (java.util.Base64/getEncoder) (.getBytes ^String s "UTF-8")))

(deftest a-table-file-reads-as-columns-and-rows
  (let [{:keys [columns rows]}
        (:body (api/post! :kompass-tabular
                          {:filename "a.csv" :bytes-b64 (b64 "Nr;Menge;Preis Netto\nA;3;1,5\n\"B;2\";;2,0\n")}))]
    (is (= [{:name "nr" :type "string"} {:name "menge" :type "long"}
            {:name "preis_netto" :type "double"}]
           (mapv #(update % :type name) columns)))
    (is (= 2 (count rows)))
    (is (= "B;2" (:nr (second rows))))
    (is (nil? (:menge (second rows))))
    (is (= 1.5 (:preis_netto (first rows))))))

(deftest a-raw-file-is-uploaded-moved-and-deleted
  (let [v  (api/post! :kompass-files {:name "bild.png" :project "ansicht" :mime "image/png"
                                      :bytes-b64 (b64 "PNGDATEN")})
        id (get-in v [:body :id])]
    (is (ok? v) (pr-str (:body v)))
    (is (some #(= id (:id %)) (get-in (api/get! :kompass-files) [:body :files])))
    (is (= "PNGDATEN" (String. ^bytes (:bytes (tx/get-bytes! (str "/api/kompass/files/" id))))))
    (testing "a project is required"
      (is (= 400 (:status (api/post! :kompass-files {:name "x" :bytes-b64 (b64 "x")})))))
    (is (ok? (api/delete! :kompass-file {:id id})))
    (is (= 404 (:status (tx/get-bytes! (str "/api/kompass/files/" id)))))))

(deftest the-log-dates-every-resource
  (let [{:keys [modified]} (:body (api/get! :kompass-index))]
    (is (map? (:times modified)))
    (is (some #(re-find #"^object-types/" (name %)) (keys (:times modified))))))

(deftest a-reference-is-declared-and-taken-back
  (let [erklaert #(get-in (api/get! :project-deps) [:body :declared])]
    (is (ok? (api/mutate :create-project {:name :anderes} {:publish? true})))
    (is (ok? (api/post! :kompass-project-imports {:name "ansicht"} {:imports ["anderes"]})))
    (is (= ["anderes"] (mapv name (or (get (erklaert) "ansicht") (get (erklaert) :ansicht)))))
    (is (ok? (api/post! :kompass-project-imports {:name "ansicht"} {:imports []})))
    (is (empty? (or (get (erklaert) "ansicht") (get (erklaert) :ansicht))))))
