(ns ^:api scenario.aviation-branches-test
  (:require [sutra.catalog.grants :as grants]
            [clojure.string :as str]
            [support.in-project :refer [install-in-project]]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.access.principals :as pr]
            [support.aviation :refer [ontologie]]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(def ^:private welt
  (-> ontologie
      (assoc-in [:objects :user] {:attributes [[:user-id :string] [:rolle :string]]
                                  :primary-key :user-id :plural "Users"})
      (update :actions merge
              {:beschreiben
               {:params {:mangel {:name "Mangel" :type :mangel :required true}
                         :text {:name "Text" :type :string :required true}}
                :rules [{:op :update :object-type :mangel :target [:get :mangel]
                         :values {:beschreibung [:get :text]}}]
                :edits [{:object-type :mangel}]}
               :benutzer-anlegen
               {:params {:id {:name "Id" :type :string :required true}
                         :rolle {:name "Rolle" :type :string :required true}}
                :rules [{:op :create :object-type :user :id :id
                :values {:user-id [:get :id] :rolle [:get :rolle]}}]
                :edits [{:object-type :user}]}
               :laut-melden
               {:params {:mangel-nr {:name "Mangel nr" :type :string :required true}
                         :luftfahrzeug {:name "Luftfahrzeug" :type :luftfahrzeug :required true}}
                :rules [{:op :create :object-type :mangel
                         :values {:mangel-nr [:get :mangel-nr]
                                  :kennzeichen [:get :luftfahrzeug :kennzeichen]
                                  :zustand "offen" :kategorie "A" :beschreibung "laut"}}
                        {:op :notify :channel "email" :payload [:get :mangel-nr]}]
                :edits [{:object-type :mangel}]}
               :laut-melden-ueberall
               {:params {:mangel-nr {:name "Mangel nr" :type :string :required true}
                         :luftfahrzeug {:name "Luftfahrzeug" :type :luftfahrzeug :required true}}
                :rules [{:op :create :object-type :mangel
                         :values {:mangel-nr [:get :mangel-nr]
                                  :kennzeichen [:get :luftfahrzeug :kennzeichen]
                                  :zustand "offen" :kategorie "A" :beschreibung "laut"}}
                        {:op :notify :channel "email" :payload [:get :mangel-nr]}]
                :edits [{:object-type :mangel}]
                :on-branch {:notifications true}}})))

(def ^:private admin-token (atom nil))

(defn- anmelden [u p] (-> (api/post! :login {:username u :password p}) :body :token))

(defn- als-admin [f]
  (reset! admin-token (anmelden grants/admin-username pr/default-admin-password))
  (binding [tx/*auth-token* @admin-token] (f)))

(defn- tun! [action args] (api/mutate action args {:publish? true}))

(defn- mit-welt [f]
  (let [r (install-in-project {:schema welt})] (assert (ok? r) (pr-str (:body r))))
  (assert (ok? (api/post! :staging)))
  (doseq [k ["D-AINA" "D-AINB"]]
    (assert (ok? (tun! :luftfahrzeug-registrieren {:kennzeichen k :muster "A320"}))))
  (f))

(use-fixtures :once pod/with-mem-pod-auth als-admin mit-welt)

(defn- zweig [prefix] (str prefix "-" (subs (str (java.util.UUID/randomUUID)) 0 8)))

(defn- gabeln! [b] (assert (ok? (api/post! :branches {:parent "main" :name b}))))

(defn- typen []
  (set (map :object-type-name (vals (:object-types (:body (api/get! :schema)))))))

(defn- mangel [nr]
  (some #(when (= nr (get-in % [:values :mangel-nr])) %)
        (:rows (:body (api/list-objects :mangel)))))

(defn- melden! [nr k]
  (tun! :mangel-melden {:mangel-nr nr :luftfahrzeug k :beschreibung "x" :kategorie "C"}))

(defn- install! [schema]
  (let [r (install-in-project {:schema schema})
        c (when (ok? r) (api/post! :staging))]
    (when-not (and (ok? r) (ok? c)) (api/delete! :staging))
    (and (ok? r) (ok? c))))

(defn- zweig-zeile [b & [opts]]
  (some #(when (= b (:name %)) %) (:branches (:body (api/list-branches opts)))))

(defn- vorschlagen [b titel]
  (api/post! :proposals {:source b :target "main" :title titel}))

(deftest ein-zweig-liest-main-wo-er-nichts-geschrieben-hat
  (let [z (zweig "lesen")]
    (is (ok? (melden! "B-1" "D-AINA")))
    (is (ok? (melden! "B-2" "D-AINA")))
    (gabeln! z)
    (tx/with-branch z (is (ok? (tun! :beschreiben {:mangel "B-1" :text "im Zweig"}))))
    (is (ok? (tun! :beschreiben {:mangel "B-1" :text "auf main"})))
    (is (ok? (tun! :beschreiben {:mangel "B-2" :text "auf main"})))
    (is (ok? (melden! "B-3" "D-AINB")))

    (tx/with-branch z
      (testing "die eigene Schreibung des Zweigs gewinnt, je Objekt"
        (is (= "im Zweig" (get-in (mangel "B-1") [:values :beschreibung]))))
      (testing "was der Zweig nicht berührt hat, liest er von main — auch was nach der Gabelung kam"
        (is (= "auf main" (get-in (mangel "B-2") [:values :beschreibung])))
        (is (some? (mangel "B-3")))))
    (testing "main sieht nichts vom Zweig"
      (is (= "auf main" (get-in (mangel "B-1") [:values :beschreibung]))))
    (testing "eine Anfrage nennt ihren Zweig, die Antwort sagt, wo sie gewirkt hat; ohne Angabe ist es main"
      (is (= z (:branch (tx/with-branch z (api/get! :object-counts)))))
      (is (= "main" (:branch (api/get! :object-counts))) "einen Zweig zu nennen setzt nichts fest"))
    (testing "Festgeschriebenes übersteht einen Neustart, auf main wie auf dem Zweig"
      (is (ok? (api/post! :reboot)))
      (is (= "im Zweig" (tx/with-branch z (get-in (mangel "B-1") [:values :beschreibung]))))
      (is (= "auf main" (get-in (mangel "B-1") [:values :beschreibung]))))
    (testing "ein gelöschter Zweig ist weg"
      (is (ok? (api/delete! :branch {:name z})))
      (is (nil? (zweig-zeile z)))
      (let [eintrag (zweig-zeile z {:closed? true})]
        (is (= "closed" (:status eintrag)) "der Name bleibt im Verzeichnis, als Protokoll")
        (is (some? (:purged-at eintrag)))))))

(deftest ein-proposal-traegt-das-schema
  (let [z (zweig "tragen")]
    (gabeln! z)
    (tx/with-branch z
      (is (install! {:objects {:hangar {:primary-key :nr :attributes [[:nr :string]]
                                        :plural "Hangars"}}}))
      (is (ok? (melden! "T-1" "D-AINB"))))
    (is (not (contains? (typen) :hangar)))
    (is (nil? (mangel "T-1")))
    (let [r (vorschlagen z "Hangar")]
      (is (ok? r) (pr-str (:body r)))
      (is (= "open" (get-in r [:body :status])))
      (is (seq (get-in r [:body :preview :diff])))
      (let [m (api/post! :proposal-merge {:id (get-in r [:body :id])} nil)]
        (is (ok? m) (pr-str (:body m)))
        (is (= "merged" (get-in m [:body :status])))
        (is (seq (get-in m [:body :merge-commit])))))
    (is (contains? (typen) :hangar))
    (testing "Action-Edits auf einem Zweig sind ein Probelauf und kommen nicht mit"
      (is (nil? (mangel "T-1"))))))

(deftest konflikte-und-invarianten
  (is (install! {:objects {:werkstatt {:primary-key :nr :attributes [[:nr :string]]
                                       :plural "Werkstätten"}
                           :halle {:primary-key :nr :attributes [[:nr :string]] :plural "Hallen"}
                           :lager {:primary-key :nr :attributes [[:nr :string]] :plural "Lager"}
                           :schrank {:primary-key :nr :attributes [[:nr :string]]
                                     :plural "Schränke"}}}))
  (let [a (zweig "konflikt-a") b (zweig "konflikt-b") c (zweig "anderes") d (zweig "gleich")
        e (zweig "weg-gegen-edit") f (zweig "weg") g (zweig "dazu")]
    (doseq [x [a b c d e f g]] (gabeln! x))
    (tx/with-branch e (is (ok? (api/delete-object-type :lager))))
    (tx/with-branch f (is (ok? (api/delete-object-type :schrank))))
    (tx/with-branch a
      (is (install! {:objects {:werkstatt {:primary-key :nr
                                           :attributes [[:nr :string] [:ort :string]]
                                           :plural "Werkstätten"}}})))
    (tx/with-branch b
      (is (install! {:objects {:werkstatt {:primary-key :nr
                                           :attributes [[:nr :string] [:ort :decimal]]
                                           :plural "Werkstätten"}}})))
    (tx/with-branch g
      (is (install! {:objects {:werkstatt {:primary-key :nr
                                           :attributes [[:nr :string] [:leiter :string]]
                                           :plural "Werkstätten"}}})))
    (tx/with-branch c
      (is (install! {:objects {:halle {:primary-key :nr
                                       :attributes [[:nr :string] [:flaeche :decimal]]
                                       :plural "Hallen"}}})))
    (tx/with-branch d
      (is (install! {:objects {:werkstatt {:primary-key :nr
                                           :attributes [[:nr :string] [:ort :string]]
                                           :plural "Werkstätten"}}})))
    (is (ok? (api/post! :proposal-merge {:id (get-in (vorschlagen a "Ort") [:body :id])} nil)))

    (testing "zwei Zweige legen dasselbe Attribut verschieden an: der zweite landet nicht"
      (let [r (vorschlagen b "Ort als Zahl")
            m (api/post! :proposal-merge {:id (get-in r [:body :id])} nil)]
        (is (not (ok? m)))
        (is (re-find #"(?i)same ontology entities|conflict" (str (get-in m [:body :error])))
            (pr-str (:body m)))
        (is (= "open" (get-in (api/get! :proposal {:id (get-in r [:body :id])}) [:body :status])))
        (api/delete! :proposal {:id (get-in r [:body :id])})))

    (testing "zwei Zweige fügen derselben Deklaration verschiedene Attribute hinzu: Zuwachs kollidiert nicht"
      (is (= "merged"
             (get-in (api/post! :proposal-merge {:id (get-in (vorschlagen g "Leiter") [:body :id])}
                                nil)
                     [:body :status])))
      (is (= #{:nr :ort :leiter}
             (->> (vals (:object-types (:body (api/get! :schema))))
                  (some #(when (= :werkstatt (:object-type-name %)) %))
                  :attribute-list (map :attribute-name) set))))

    (testing "verschiedene Deklarationen vertragen sich"
      (is (ok? (api/post! :proposal-merge {:id (get-in (vorschlagen c "Fläche") [:body :id])}
                          nil))))

    (testing "dieselbe Änderung auf beiden Seiten ist Einigkeit, kein Konflikt"
      (let [m (api/post! :proposal-merge {:id (get-in (vorschlagen d "Ort nochmal") [:body :id])}
                         nil)]
        (is (ok? m) (pr-str (:body m)))))

    (testing "eine Löschung auf dem Zweig gegen eine Änderung auf main ist ein Konflikt"
      (is (install! {:objects {:lager {:primary-key :nr :attributes [[:nr :string] [:regal :string]]
                                       :plural "Lager"}}}))
      (let [r (vorschlagen e "Lager weg")
            m (api/post! :proposal-merge {:id (get-in r [:body :id])} nil)]
        (is (not (ok? m)) (pr-str (:body m)))
        (api/delete! :proposal {:id (get-in r [:body :id])}))
      (is (contains? (typen) :lager)))

    (testing "eine Löschung, gegen die nichts steht, landet"
      (is (ok? (api/post! :proposal-merge {:id (get-in (vorschlagen f "Schrank weg") [:body :id])}
                          nil)))
      (is (not (contains? (typen) :schrank))))

    (let [attrs (set (map :attribute-name
                          (:attribute-list
                           (some #(when (= :werkstatt (:object-type-name %)) %)
                                 (vals (:object-types (:body (api/get! :schema))))))))]
      (is (contains? attrs :ort))
      (is (contains? attrs :leiter) "der Zuwachs des zweiten Zweigs ist gelandet")))

  (testing "der Merge prüft die Invarianten des zusammengeführten Modells"
    (let [z (zweig "invariante")]
      (gabeln! z)
      (tx/with-branch z
        (is (install! {:objects {:werkbank {:primary-key :nr
                                            :attributes [[:nr :string] [:hallen-nr :string]]
                                            :plural "Werkbänke"}}
                       :links [[:werkbank-halle :werkbank :halle :many-to-one {:fk :hallen-nr}]]})))
      (is (ok? (api/delete-object-type :halle)))
      (let [r (vorschlagen z "Werkbank")
            m (api/post! :proposal-merge {:id (get-in r [:body :id])} nil)]
        (is (not (ok? m)) (pr-str (:body m)))
        (is (re-find #"not valid" (str (get-in m [:body :error]))) (pr-str (:body m)))
        (is (re-find #"no-dangling-refs" (pr-str (:body m))) (pr-str (:body m)))
        (is (not (contains? (typen) :werkbank)))
        (api/delete! :proposal {:id (get-in r [:body :id])})))))

(deftest seiteneffekte-auf-zweigen
  (let [z (zweig "effekt")]
    (testing "auf main geht die Benachrichtigung hinaus"
      (let [r (tun! :laut-melden {:mangel-nr "E-1" :luftfahrzeug "D-AINA"})]
        (is (ok? r) (pr-str (:body r)))
        (is (nil? (get-in r [:body :result :suppressed-side-effects])))))
    (gabeln! z)
    (tx/with-branch z
    (testing "auf einem Zweig wird die Action angewandt, die Benachrichtigung zurückgehalten und genannt"
      (let [r (tun! :laut-melden {:mangel-nr "E-2" :luftfahrzeug "D-AINA"})
            [gehalten] (get-in r [:body :result :suppressed-side-effects])]
        (is (ok? r))
        (is (= :notifications (keyword (:kind gehalten))) (pr-str (:body r)))
        (is (= z (:branch gehalten)))
        (is (some? (mangel "E-2")))))
    (testing "wer es erlaubt (:on-branch), benachrichtigt auch auf dem Zweig"
      (let [r (tun! :laut-melden-ueberall {:mangel-nr "E-3" :luftfahrzeug "D-AINA"})]
        (is (ok? r))
        (is (nil? (get-in r [:body :result :suppressed-side-effects]))))))))

(defn- policy! [b m] (api/post! :branch-policy {:name b} m))

(def ^:private vier-augen
  [{:when [:always] :by [:other]}])

(defn- benutzer! [name rolle]
  (let [id (str (java.util.UUID/randomUUID))]
    (assert (ok? (tun! :benutzer-anlegen {:id id :rolle rolle})))
    (assert (ok? (api/post! :register {:username name :password name :user-id id})))
    {:id id :token (anmelden name name)}))

(defn- verwalter!
  [{:keys [id]}]
  (assert (ok? (api/mutate :update-project
                           {:name :flotte :grants [{:principal id :role :ontology-owner}]}
                           {:stage? true})))
  (assert (ok? (api/post! :staging))))

(deftest ein-geschuetzter-zweig-nimmt-nur-gepruefte-proposals
  (let [pruefer (doto (benutzer! "pruefer" "builder") verwalter!)
        autorin (benutzer! "autorin" "ontology-editor")
        z       (zweig "feature")
        pid     (atom nil)]
    (try
      (testing "Schutz ist Sache des Admins und für alle sichtbar"
        (tx/with-token (:token pruefer)
          (is (= 403 (:status (policy! "main" {:protected true})))))
        (is (ok? (policy! "main" {:protected true :review vier-augen})))
        (tx/with-token (:token pruefer)
          (is (true? (:protected (zweig-zeile "main"))))))

      (testing "eine direkte Änderung an main wird abgelehnt und nennt den Weg; der Entwurf bleibt"
        (is (ok? (install-in-project {:schema
                                        {:objects
                                           {:notiz {:primary-key :text :attributes [[:text :string]]
                                                    :plural "Notizen"}}}})))
        (let [r (api/post! :staging)]
          (is (= 403 (:status r)))
          (is (re-find #"proposal" (str (get-in r [:body :error])))))
        (is (seq (get-in (api/get! :staging) [:body :staged])))
        (api/delete! :staging))

      (testing "auf dem eigenen Zweig landet dieselbe Änderung; main bleibt unberührt"
        (tx/with-token (:token autorin)
          (gabeln! z)
          (tx/with-branch z
            (is (install! {:objects {:notiz {:primary-key :text :attributes [[:text :string]]
                                             :plural "Notizen"}}}))
            (is (contains? (typen) :notiz))))
        (is (not (contains? (typen) :notiz))))

      (testing "das Proposal trägt die Hürde des Ziels und beginnt in Vorbereitung"
        (tx/with-token (:token autorin)
          (let [r (vorschlagen z "Notiz")]
            (is (ok? r))
            (reset! pid (get-in r [:body :id]))
            (is (every? #(seq (:requires %))
                        (get-in (api/get! :proposal-tasks {:id @pid}) [:body :tasks]))
                "jede Aufgabe trägt, was sie verlangt")
            (is (= "preparing" (get-in (api/get! :proposal {:id @pid}) [:body :stage])))
            (let [a (api/ask-review @pid ["pruefer"])]
              (is (= "in-review" (get-in a [:body :stage])))
              (is (= ["pruefer"] (get-in a [:body :reviewers])))))))

      (testing "wer den Zweig nicht besitzt, landet ihn nicht; die Autorin prüft sich nicht selbst"
        (tx/with-token (:token pruefer)
          (is (re-find #"only an owner"
                       (str (get-in (api/post! :proposal-merge {:id @pid} nil) [:body :error])))))
        (tx/with-token (:token autorin)
          (is (= 400 (:status (api/post! :proposal-approve {:id @pid} {}))))
          (is (re-find #"still open"
                       (str (get-in (api/post! :proposal-merge {:id @pid} nil) [:body :error]))))))

      (testing "eine Ablehnung sperrt, ohne zu schließen; eine spätere Zustimmung ersetzt sie"
        (tx/with-token (:token pruefer)
          (let [r (api/post! :proposal-reject {:id @pid} {:note "Name falsch"})]
            (is (= "open" (get-in r [:body :status])))))
        (tx/with-token (:token autorin)
          (is (re-find #"rejected by"
                       (str (get-in (api/post! :proposal-merge {:id @pid} nil) [:body :error])))))
        (tx/with-token (:token pruefer)
          (let [r (api/post! :proposal-approve {:id @pid} {})]
            (is (= 1 (get-in r [:body :approval-count])))
            (is (empty? (get-in r [:body :rejections]))))))

      (testing "Aufgaben je Ressource, mit RID benannt und mit Namen angezeigt; dann steht es auf merge"
        (let [ts (get-in (api/get! :proposal-tasks {:id @pid}) [:body :tasks])]
          (is (= #{"object-types" "datasets"} (set (map #(first (str/split (:task %) #"/")) ts))))
          (is (every? #(re-find #"/ri\." (:task %)) ts))
          (is (some #(= "notiz" (:name %)) ts)))
        (is (= "merge" (get-in (api/get! :proposal {:id @pid}) [:body :stage]))))

      (testing "ein Kommentar hängt an einer Ressource und bewegt kein Tor"
        (tx/with-token (:token pruefer)
          (let [task (:task (first (get-in (api/get! :proposal-tasks {:id @pid}) [:body :tasks])))
                c    (api/add-comment @pid "warum dieser Schlüssel?" task)]
            (is (ok? c))
            (let [ts (get-in (api/get! :proposal-tasks {:id @pid}) [:body :tasks])]
              (is (= 1 (count (:comments (some #(when (= task (:task %)) %) ts)))))
              (is (empty? (mapcat :comments (remove #(= task (:task %)) ts)))))
            (is (= 404
                   (:status (api/add-comment @pid "…"
                                             "object-types/ri.sutra.main.object-type.nope"))))
            (is (= "merge" (get-in (api/get! :proposal {:id @pid}) [:body :stage])))
            (is (ok? (api/delete! :proposal-comment {:id @pid :comment (get-in c [:body :id])}))))))

      (testing "jetzt landet es; ein gelandetes Proposal ist verbraucht"
        (tx/with-token (:token autorin)
          (let [m (api/post! :proposal-merge {:id @pid} nil)]
            (is (= "merged" (get-in m [:body :status])) (pr-str (:body m)))))
        (is (contains? (typen) :notiz))
        (is (= 400 (:status (api/post! :proposal-merge {:id @pid} nil))))
        (is (= 400 (:status (api/add-comment @pid "noch was"))))
        (is (not-any? #(= @pid (:id %))
                      (get-in (api/list-proposals {:status "open"}) [:body :proposals])))
        (is (some #(= @pid (:id %))
                  (get-in (api/list-proposals {:status "all"}) [:body :proposals])))
        (is (= "merged" (:status (zweig-zeile z {:closed? true})))))

      (testing "unter Vier-Augen zählt auch der Admin nicht für sich selbst"
        (let [eigen (zweig "eigen")]
          (gabeln! eigen)
          (tx/with-branch eigen
            (is (install! {:objects {:eigen {:primary-key :text :attributes [[:text :string]]
                                             :plural "Eigene"}}})))
          (let [id (get-in (vorschlagen eigen "eigen") [:body :id])]
            (is (= 400 (:status (api/post! :proposal-approve {:id id} {}))))
            (testing "mit der Vorgabe-Regel landet, wer allein ist, ohne Freigabe: ihm gehört alles"
              (is (ok? (policy! "main" {:review nil})))
              (is (= "merged" (get-in (api/post! :proposal-merge {:id id} nil) [:body :status])))
              (is (contains? (typen) :eigen))
              (is (ok? (policy! "main" {:review vier-augen})))))))

      (testing "ein geschlossener Zweig verschwindet aus der Liste und kommt nicht zurück"
        (let [g (zweig "zu")]
          (gabeln! g)
          (is (ok? (api/post! :branch-status {:name g} {:closed true})))
          (is (nil? (zweig-zeile g)))
          (is (= "closed" (:status (zweig-zeile g {:closed? true}))))
          (is (= 400 (:status (api/post! :branch-status {:name g} {:inactive false}))))))

      (testing "eine Kompass-Datei (Lineage-Graph) ist kein Schema und landet auch auf geschütztem main"
        (is (ok? (api/mutate :create-graph
                             {:name :freigabekette :title "Freigabekette" :mode :lineage}
                             {:stage? true})))
        (is (ok? (api/post! :staging)))
        (is (some #(= "freigabekette" (some-> % :name name)) (:graphs (:body (api/list-graphs))))))

      (testing "Aufbewahrung ist Wahl, und sie läuft in zwei Stufen: erst das Etikett, dann das Schließen"
        (let [alt (zweig "alt")]
          (is (empty? (get-in (api/post! :branch-sweep {:dry-run? true}) [:body :would-close])))
          (gabeln! alt)
          (is (ok? (policy! alt {:inactive-days 0})))
          (let [r (api/post! :branch-sweep {:dry-run? true})]
            (is (some #{alt} (get-in r [:body :would-mark-inactive])))
            (is (not-any? #{alt} (get-in r [:body :would-close]))))
          (is (ok? (api/post! :branch-sweep {:dry-run? false})))
          (is (some? (:inactive-at (zweig-zeile alt))))
          (is (ok? (policy! alt {:inactive-days 3650})))
          (is (ok? (api/post! :branch-sweep {:dry-run? false})))
          (is (nil? (:inactive-at (zweig-zeile alt))))))

      (testing "Auto-Land: ein Proposal mit Arbeit, das keine Freigabe verlangt, landet beim Vorschlagen"
        (is (ok? (policy! "main" {:protected true :review nil :auto-land true})))
        (let [leer (zweig "solo") voll (zweig "solo")]
          (gabeln! leer)
          (let [r (vorschlagen leer "leer")]
            (is (= "open" (get-in r [:body :status])))
            (is (not (get-in r [:body :auto-landed])))
            (api/delete! :proposal {:id (get-in r [:body :id])}))
          (gabeln! voll)
          (tx/with-branch voll
            (is (install! {:objects {:zettel {:primary-key :text :attributes [[:text :string]]
                                              :plural "Zettel"}}})))
          (let [r (vorschlagen voll "Zettel")]
            (is (true? (get-in r [:body :auto-landed])))
            (is (= "merged" (get-in r [:body :status])))
            (is (zero? (get-in r [:body :approval-count])) "nichts verlangt, nichts vorgetäuscht"))
          (is (contains? (typen) :zettel)))
        (testing "verlangt eine Regel eine Freigabe, bleibt es offen — Auto-Land überspringt kein Urteil"
          (is (ok? (policy! "main" {:review vier-augen})))
          (let [z (zweig "solo")]
            (gabeln! z)
            (tx/with-branch z
              (is (install! {:objects {:geheim {:primary-key :text :attributes [[:text :string]]
                                                :plural "Geheime"}}})))
            (let [r (vorschlagen z "geheim")]
              (is (= "open" (get-in r [:body :status])))
              (is (not (contains? (typen) :geheim)))
              (api/delete! :proposal {:id (get-in r [:body :id])}))))
        (testing "abgeschaltet gelten wieder die drei Schritte"
          (is (ok? (policy! "main" {:auto-land false})))
          (let [drei (zweig "solo")]
            (gabeln! drei)
            (tx/with-branch drei
              (is (install! {:objects {:spaeter {:primary-key :text :attributes [[:text :string]]
                                                 :plural "Spätere"}}})))
            (let [r (vorschlagen drei "später")]
              (is (= "open" (get-in r [:body :status])))
              (is (not (contains? (typen) :spaeter)))
              (api/delete! :proposal {:id (get-in r [:body :id])})))))
      (finally
        (policy! "main" {:protected false :review nil :auto-land false})))))

(defn- entwurf [] (get-in (api/get! :staging) [:body :staged]))

(defn- entwurf-namen [] (set (keep #(some-> % :args :name keyword) (entwurf))))

(deftest entwuerfe
  (api/delete! :staging)
  (is (ok? (install-in-project {:schema
                                  {:objects
                                     {:pruefstand {:primary-key :nr :attributes [[:nr :string]]
                                                   :plural "Prüfstände"}}}})))

  (testing "eine Vorschau antwortet mit dem, was sie stagen würde, und lässt den offenen Entwurf, wie er ist"
    (let [r (install-in-project {:schema {:objects {:vorschau {:attributes [[:nr :string]]
                                                               :plural "Vorschauen"}}}
                                 :preview? true})]
      (is (ok? r))
      (is (= [:vorschau] (vec (keep #(some-> % :args :name keyword) (:staged (:body r)))))))
    (is (= #{:pruefstand} (entwurf-namen))))

  (testing "die Vorschau nennt Tore, die erst beim Festschreiben laufen — und nur die des Kandidaten"
    (let [r (install-in-project {:schema
                                   {:objects
                                      {:titellos
                                         {:attributes [[:nr :string]] :title :gibt-es-nicht
                                          :plural "Titellose"}}}
                                 :preview? true})]
      (is (ok? r))
      (is (contains? (set (map keyword (get-in r [:body :problems :relational-checks-failed])))
                     :title-keys-valid?)))
    (is
     (nil? (get-in (install-in-project {:schema {:objects {:harmlos {:attributes [[:nr :string]]
                                                                     :plural "Harmlose"}}}
                                        :preview? true})
                   [:body :problems]))))

  (testing "dasselbe Anlegen zweimal wird an der Tür abgewiesen und nennt den Griff des ersten"
    (let [seqs (set (map :seq (entwurf)))
          r
            (install-in-project {:schema
                                   {:objects
                                      {:pruefstand {:primary-key :nr :attributes [[:nr :string]]
                                                    :plural "Prüfstände"}}}})
          p    (some #(when (= :already-staged (keyword (:rule %))) %)
                     (get-in r [:body :data :problems]))]
      (is (= 400 (:status r)))
      (is (contains? seqs (:seq p)))
      (is (= seqs (set (map :seq (entwurf)))))))

  (testing "Verwerfen trifft genau eine Zeile; ein unbekannter Griff trifft nichts"
    (is (ok? (install-in-project {:schema
                                    {:objects
                                       {:pruefstand-2 {:primary-key :nr :attributes [[:nr :string]]
                                                       :plural "Prüfstände 2"}}}})))
    (let [vorher (set (map :seq (entwurf)))]
      (api/delete! :staged-by-seq {:seq 999999})
      (is (= vorher (set (map :seq (entwurf)))))
      (is (ok? (api/delete! :staged-by-seq {:seq (apply max vorher)})))
      (is (= 1 (count (entwurf))))))
  (api/delete! :staging)

  (testing "bewegt sich main unter einem offenen Entwurf, holt ein Rebase die Änderung nach; im Konflikt gewinnt main, außer man sagt :mine"
    (is (install! {:objects {:messplatz {:primary-key :nr :attributes [[:nr :string]]
                                         :plural "Messplätze"}
                             :kalibrierplatz {:primary-key :nr :attributes [[:nr :string]]
                                              :plural "Kalibrierplätze"}}}))
    (let [kollege (benutzer! "kollege" "ontology-editor")]
      (is (ok? (install-in-project {:schema
                                      {:objects
                                         {:messplatz
                                            {:primary-key :nr
                                             :attributes [[:nr :string] [:von-mir :string]]
                                             :plural "Messplätze"}
                                          :kalibrierplatz
                                            {:primary-key :nr
                                             :attributes [[:nr :string] [:von-mir :string]]
                                             :plural "Kalibrierplätze"}}}})))
      (tx/with-token (:token kollege)
        (is (install! {:objects {:messplatz {:primary-key :nr
                                             :attributes [[:nr :string] [:vom-kollegen :string]]
                                             :plural "Messplätze"}
                                 :kalibrierplatz
                                   {:primary-key :nr
                                    :attributes [[:nr :string] [:vom-kollegen :string]]
                                    :plural "Kalibrierplätze"}}})))
      (let [rid (:id (some #(when (= :kalibrierplatz (:object-type-name %)) %)
                           (vals (:object-types (:body (api/get! :schema))))))
            r   (api/post! :staging-rebase {:resolutions {[:object-types rid] :mine}})]
        (is (ok? r) (pr-str (:body r)))
        (is (ok? (api/post! :staging)) "nach dem Rebase landet der Entwurf"))
      (let [attrs (fn [t]
                    (set (map :attribute-name
                              (:attribute-list
                               (some #(when (= t (:object-type-name %)) %)
                                        (vals (:object-types (:body (api/get! :schema)))))))))]
        (is (contains? (attrs :messplatz) :vom-kollegen) "ungelöst gewinnt main")
        (is (contains? (attrs :kalibrierplatz) :von-mir) ":mine gewinnt, wo man es sagt")))))

(deftest ein-proposal-landet-genau-den-freigegebenen-stand-genau-einmal
  (testing "Flaw merged-unapproved, double-merge, closed-as-merged, dangling-proposal"
    (let [pruefer (doto (benutzer! "pruefer-d129" "builder") verwalter!)
          z       (zweig "freigabe")]
      (gabeln! z)
      (tx/with-branch z
        (is (install! {:objects {:geprueft {:primary-key :text :attributes [[:text :string]]
                                            :plural "Geprüfte"}}})))
      (let [id (get-in (vorschlagen z "geprüft") [:body :id])]
        (tx/with-token (:token pruefer)
          (is (= 1 (get-in (api/post! :proposal-approve {:id id} {}) [:body :approval-count]))))
        (testing "die Quelle bewegt sich nach der Freigabe: die Freigabe trägt den neuen Stand nicht"
          (tx/with-branch z
            (is (install! {:objects {:ungeprueft {:primary-key :text :attributes [[:text :string]]
                                                  :plural "Ungeprüfte"}}})))
          (is (ok? (policy! "main" {:review vier-augen})))
          (let [m (api/post! :proposal-merge {:id id} nil)]
            (is (not= "merged" (get-in m [:body :status])))
            (is (re-find #"still open: object-types/" (str (get-in m [:body :error])))
                (pr-str (:body m))))
          (is (not (contains? (typen) :ungeprueft))))
        (testing "neu freigegeben landet es — und genau einmal"
          (tx/with-token (:token pruefer) (api/post! :proposal-approve {:id id} {}))
          (is (= "merged" (get-in (api/post! :proposal-merge {:id id} nil) [:body :status])))
          (is (= 400 (:status (api/post! :proposal-merge {:id id} nil)))))
        (is (ok? (policy! "main" {:review nil})))))
    (testing "ein geschlossenes Proposal stempelt seine Quelle nicht als gemergt"
      (let [z (zweig "zurueck")]
        (gabeln! z)
        (tx/with-branch z
          (is (install! {:objects {:verworfen {:primary-key :text :attributes [[:text :string]]
                                               :plural "Verworfene"}}})))
        (api/delete! :proposal {:id (get-in (vorschlagen z "verworfen") [:body :id])})
        (is (= "active" (:status (zweig-zeile z))))
        (api/delete! :branch {:name z})))
    (testing "wird die Quelle gelöscht, bleibt kein offenes Proposal zurück"
      (let [z  (zweig "verwaist")
            _  (gabeln! z)
            _  (tx/with-branch z
                 (install! {:objects {:verwaist {:primary-key :text :attributes [[:text :string]]
                                                 :plural "Verwaiste"}}}))
            id (get-in (vorschlagen z "verwaist") [:body :id])]
        (is (ok? (api/delete! :branch {:name z})))
        (is (= "closed" (get-in (api/get! :proposal {:id id}) [:body :status])))))))
