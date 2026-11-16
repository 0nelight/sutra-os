(ns ^:api scenario.aviation-review-test
  (:require [sutra.catalog.grants :as grants]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [support.in-project :refer [install-in-project]]
            [sutra.access.principals :as pr]
            [support.aviation :refer [kanten-aus ontologie]]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
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
                 :edits [{:object-type :user}]})
      (assoc-in [:projects :werkstatt] {})
      (assoc-in [:objects :werkzeug]
                {:project :werkstatt :attributes [[:nr :string] [:name :string] [:ort :string]]
                 :primary-key :nr :plural "Werkzeuge"})
      (update :links (fnil conj [])
              (kanten-aus [:lfz-werkzeug :luftfahrzeug :werkzeug :many-to-many {:project :flotte}]
                          :instandhaltungssystem "WERKZEUGAUSGABE"))))

(defn- anmelden [u p] (-> (api/post! :login {:username u :password p}) :body :token))

(defn- tun! [action args] (api/mutate action args {:publish? true}))

(def ^:private leute (atom {}))

(defn- benutzer! [name]
  (let [id (str (java.util.UUID/randomUUID))]
    (assert (ok? (tun! :benutzer-anlegen {:id id :rolle "ontology-editor"})))
    (assert (ok? (api/post! :register {:username name :password name :user-id id})))
    (swap! leute assoc name {:id id :token (anmelden name name)})))

(defn- eigentuemer! [projekt & namen]
  (let [r (api/mutate :update-project
                      {:name projekt
                       :grants (vec (for [n namen]
                                      {:principal (get-in @leute [n :id]) :role :ontology-owner}))}
                      {:stage? true})]
    (assert (ok? r) (pr-str (:body r)))
    (assert (ok? (api/post! :staging)))))

(defn- mit-welt [f]
  (binding [tx/*auth-token* (anmelden grants/admin-username pr/default-admin-password)]
    (let [r (install-in-project {:schema welt})] (assert (ok? r) (pr-str (:body r))))
    (assert (ok? (api/post! :staging)))
    (doseq [n ["flotti" "werki" "gast"]] (benutzer! n))
    (eigentuemer! :flotte "flotti")
    (eigentuemer! :werkstatt "werki")
    (f)))

(use-fixtures :once pod/with-mem-pod-auth mit-welt)

(defmacro ^:private als [wer & body] `(tx/with-token (get-in @leute [~wer :token]) ~@body))

(defn- werkzeug! [attrs]
  (let [r (install-in-project {:schema
                                 {:objects {:werkzeug {:project :werkstatt :attributes attrs
                                                       :primary-key :nr :plural "Werkzeuge"}}}})
        c (when (ok? r) (api/post! :staging))]
    (when-not (and (ok? r) (ok? c)) (api/delete! :staging))
    (assert (and (ok? r) (ok? c)) (pr-str (:body r) (:body c)))))

(defn- vorschlag!
  [wer attrs]
  (let [z (str "review-" (subs (str (java.util.UUID/randomUUID)) 0 8))]
    (als wer
         (assert (ok? (api/post! :branches {:parent "main" :name z})))
         (tx/with-branch z (werkzeug! attrs))
         (let [r (api/post! :proposals {:source z :target "main" :title "werkzeug"})]
           (assert (ok? r) (pr-str (:body r)))
           {:id (get-in r [:body :id]) :branch z}))))

(defn- aufgabe [id]
  (some #(when (= "werkzeug" (:name %)) %)
        (get-in (api/get! :proposal-tasks {:id id}) [:body :tasks])))

(defn- landen [wer id] (als wer (api/post! :proposal-merge {:id id} nil)))

(defn- attribute []
  (->> (vals (:object-types (:body (api/get! :schema))))
       (some #(when (= :werkzeug (:object-type-name %)) %))
       :attribute-list (map :attribute-name) set))

(def ^:private basis [[:nr :string] [:name :string] [:ort :string]])

(deftest auch-ein-zuwachs-an-fremdem-wartet-auf-einen-bearbeiter
  (let [{:keys [id]} (vorschlag! "gast" (conj basis [:preis :decimal]))
        t            (als "gast" (aufgabe id))]
    (is (= "accretion" (name (:kind t))) (pr-str t))
    (is (= [[:editor]] (mapv :by (:requires t))) "die Vorgabe, auch ohne Bruch")
    (is (re-find #"still open" (str (get-in (landen "gast" id) [:body :error]))))
    (is (ok? (als "werki" (api/post! :proposal-approve {:id id} {}))))
    (is (= "merged" (get-in (landen "gast" id) [:body :status])))
    (is (contains? (attribute) :preis))))

(deftest ein-bruch-an-fremdem-eigentum-wartet-auf-dessen-eigentuemer
  (werkzeug! basis)
  (let [{:keys [id]} (vorschlag! "gast" [[:nr :string] [:name :string]])
        t            (als "gast" (aufgabe id))]
    (testing "die Aufgabe sagt, dass sie bricht, was sie verlangt und wen es trifft"
      (is (= "breaking" (name (:kind t))) (pr-str t))
      (is (= 1 (count (:requires t))))
      (is (= ["flotte"] (map name (:consumers t))) "der Link der Flotte referenziert :werkzeug"))
    (testing "ohne Freigabe landet es nicht"
      (is (re-find #"still open" (str (get-in (landen "gast" id) [:body :error])))))
    (testing "der Autor und ein Eigentümer eines ANDEREN Projekts dürfen nicht urteilen"
      (is (= 400 (:status (als "gast" (api/post! :proposal-approve {:id id} {}))))
          "der Autor besitzt :werkstatt nicht")
      (let [r (als "flotti" (api/post! :proposal-approve {:id id} {}))]
        (is (= 400 (:status r)))
        (is (re-find #"\[:editor\]" (str (get-in r [:body :error])))
            "die Abweisung nennt die Regel")))
    (testing "der Eigentümer gibt frei, dann landet es"
      (is (ok? (als "werki" (api/post! :proposal-approve {:id id} {}))))
      (is (= "merged" (get-in (landen "gast" id) [:body :status])))
      (is (not (contains? (attribute) :ort))))))

(deftest ein-bruch-am-eigenen-eigentum-landet-ohne-freigabe
  (werkzeug! basis)
  (let [{:keys [id]} (vorschlag! "werki" [[:nr :string]])
        t            (als "werki" (aufgabe id))]
    (is (= "breaking" (name (:kind t))))
    (is (empty? (:requires t)) "wem es gehört, der wartet auf niemanden")
    (is (= "merged" (get-in (landen "werki" id) [:body :status])))))

(deftest rechte-zaehlen-wie-sie-beim-landen-stehen
  (werkzeug! basis)
  (let [{:keys [id]} (vorschlag! "gast" [[:nr :string] [:name :string]])]
    (is (ok? (als "werki" (api/post! :proposal-approve {:id id} {}))))
    (testing "Wettlauf: der Eigentümer verliert die Rolle zwischen Freigabe und Landen"
      (eigentuemer! :werkstatt)
      (let [m (landen "gast" id)]
        (is (not= "merged" (get-in m [:body :status])))
        (is (re-find #"still open" (str (get-in m [:body :error]))) (pr-str (:body m)))))
    (testing "bekommt er sie zurück, zählt dieselbe Freigabe wieder — sie ist eine Tatsache, ihr Gewicht ist von jetzt"
      (eigentuemer! :werkstatt "werki")
      (is (= "merged" (get-in (landen "gast" id) [:body :status]))))))

(deftest eine-freigabe-traegt-nur-den-stand-den-sie-sah
  (werkzeug! basis)
  (let [{:keys [id branch]} (vorschlag! "gast" [[:nr :string] [:name :string]])]
    (is (ok? (als "werki" (api/post! :proposal-approve {:id id} {}))))
    (testing "Wettlauf: die Quelle ändert dieselbe Ressource nach der Freigabe"
      (als "gast" (tx/with-branch branch (werkzeug! [[:nr :string]])))
      (let [m (landen "gast" id)]
        (is (not= "merged" (get-in m [:body :status])))
        (is (re-find #"stale" (str (get-in m [:body :error]))) (pr-str (:body m)))))
    (testing "neu freigegeben landet genau der neue Stand"
      (is (ok? (als "werki" (api/post! :proposal-approve {:id id} {}))))
      (is (= "merged" (get-in (landen "gast" id) [:body :status])))
      (is (= #{:nr} (attribute))))))

(deftest vier-augen-ist-eine-erklaerte-regel
  (werkzeug! basis)
  (is (ok? (api/post! :branch-policy {:name "main"} {:review [{:when [:always] :by [:other]}]})))
  (try
    (let [{:keys [id]} (vorschlag! "werki" (conj basis [:farbe :string]))]
      (testing "auch der Eigentümer, auch ein Zuwachs: der Autor urteilt nicht über sich"
        (is (= 400 (:status (als "werki" (api/post! :proposal-approve {:id id} {})))))
        (is (re-find #"still open" (str (get-in (landen "werki" id) [:body :error])))))
      (testing "ein anderer — hier ohne jede Eigentümerrolle — genügt der Regel"
        (is (ok? (als "gast" (api/post! :proposal-approve {:id id} {}))))
        (is (= "merged" (get-in (landen "werki" id) [:body :status])))))
    (testing "eine Regel mit unbekanntem Begriff wird abgewiesen, nicht gespeichert"
      (let [r (api/post! :branch-policy {:name "main"}
                         {:review [{:when [:freitags] :by [:owner]}]})]
        (is (= 400 (:status r)))
        (is (re-find #"freitags" (str (get-in r [:body :error]))))))
    (finally
      (api/post! :branch-policy {:name "main"} {:review nil}))))
