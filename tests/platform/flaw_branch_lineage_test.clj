(ns ^:api platform.flaw-branch-lineage-test
  (:require [support.system :as tsys]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [next.jdbc :as jdbc]
            [sutra.store.access.scope :as ctx]
            [sutra.store.pg.layout :as layout]
            [sutra.store.world :as world]
            [sutra.actions.staged :as staged-run]
            [sutra.governance.merge :as bm]
            [support.in-project :refer [in-project]]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(def ^:private welt
  {:objects {:fl-teil {:primary-key :nr :attributes [[:nr :string] [:text :string]]
                       :match-keys [:nr]
                       :plural "Fl Teile"}}
   :actions {:fl-anlegen
             {:params {:nr {:name "Nr" :type :string :required true}
                       :text {:name "Text" :type :string :required true}}
              :rules [{:op :create :object-type :fl-teil
                       :values {:nr [:get :nr] :text [:get :text]}}]
              :edits [{:object-type :fl-teil}]}
             :fl-aendern
             {:params {:teil {:name "Teil" :type :fl-teil :required true}
                       :text {:name "Text" :type :string :required true}}
              :rules [{:op :update :object-type :fl-teil :target [:get :teil]
                       :values {:text [:get :text]}}]
              :edits [{:object-type :fl-teil}]}
             :fl-umbenennen
             {:params {:teil {:name "Teil" :type :fl-teil :required true}
                       :text {:name "Text" :type :string :required true}}
              :submission-criteria [{:test {:t [:get :teil :text]} :pred [:!= :t "gesperrt"]
                                     :message "ein gesperrtes Teil wird nicht umbenannt"}]
              :rules [{:op :update :object-type :fl-teil :target [:get :teil]
                       :values {:text [:get :text]}}]
              :edits [{:object-type :fl-teil}]}
             :fl-loeschen
             {:params {:teil {:name "Teil" :type :fl-teil :required true}}
              :rules [{:op :delete-object :object-type :fl-teil :target [:get :teil]}]
              :edits [{:object-type :fl-teil}]}}})

(defn- install! [schema]
  (let [r (api/post! :install {:schema (in-project :lineage schema)})
        c (when (ok? r) (api/post! :staging))]
    (when-not (and (ok? r) (ok? c)) (api/delete! :staging))
    (and (ok? r) (ok? c))))

(use-fixtures :once pod/with-pod-auth)
(use-fixtures :each pod/as-admin)

(def ^:private welt-da (delay (assert (install! welt)) true))

(defn- zweig [prefix] (str prefix "-" (subs (str (java.util.UUID/randomUUID)) 0 8)))

(defn- gabeln! [parent b]
  (let [r (api/post! :branches {:parent parent :name b})]
    (assert (ok? r) (pr-str (:body r)))))

(defn- tun! [action args] (api/mutate action args {:publish? true}))

(defn- teile []
  (into {} (map (fn [r] [(get-in r [:values :nr]) (get-in r [:values :text])]))
        (:rows (:body (api/list-objects :fl-teil)))))

(defn- typen []
  (set (map :object-type-name (vals (:object-types (:body (api/get! :schema)))))))

(defn- attribute [t]
  (set (map :attribute-name
            (:attribute-list (some #(when (= t (:object-type-name %)) %)
                                   (vals (:object-types (:body (api/get! :schema)))))))))

(defn- landen! [b]
  (let [p (api/post! :proposals {:source b :target "main" :title (str "land " b)})
        m (api/post! :proposal-merge {:id (get-in p [:body :id])} nil)]
    (assert (ok? m) (pr-str (:body m)))
    (:body m)))

(defn- ns-da? [b]
  (with-open [c (jdbc/get-connection (tsys/ds))]
    (layout/ns-exists? c b)))

(deftest overlay-drift-die-sicht-eines-zweigs-aendert-sich-nur-durch-ereignisse-auf-ihm
  @welt-da
  (let [c (zweig "drift-c") p (zweig "drift-p") pc (zweig "drift-pc")]
    (is (ok? (tun! :fl-anlegen {:nr "D-1" :text "alt"})))
    (gabeln! "main" c)
    (testing "Schema: was main nach der Gabelung an T ändert, sieht das Kind nicht (Schnappschuss)"
      (is (install! {:objects
                       {:fl-teil {:primary-key :nr
                                  :attributes [[:nr :string] [:text :string] [:farbe :string]]
                                  :match-keys [:nr]
                                  :plural "Fl Teile"}
                        :fl-spaeter {:primary-key :nr :attributes [[:nr :string]]
                                     :plural "Fl Spätere"}}}))
      (is (contains? (attribute :fl-teil) :farbe))
      (tx/with-branch c
        (is (not (contains? (attribute :fl-teil) :farbe)))
        (is (not (contains? (typen) :fl-spaeter)))))
    (testing "Objekte: das Kind liest seine Wurzel main live — die Änderung auf main ist da"
      (is (ok? (tun! :fl-aendern {:teil "D-1" :text "neu auf main"})))
      (tx/with-branch c (is (= "neu auf main" (get (teile) "D-1")))))
    (testing "eine Ebene tiefer: was p nach der Gabelung von pc schreibt, sieht pc nicht"
      (gabeln! "main" p)
      (tx/with-branch p (is (ok? (tun! :fl-anlegen {:nr "D-P" :text "p vorher"}))))
      (gabeln! p pc)
      (tx/with-branch p
        (is (ok? (tun! :fl-aendern {:teil "D-P" :text "p nachher"})))
        (is (install! {:objects {:fl-nur-p {:primary-key :nr :attributes [[:nr :string]]
                                            :plural "Fl-nur-Ps"}}})))
      (tx/with-branch pc
        (is (= "p vorher" (get (teile) "D-P")))
        (is (not (contains? (typen) :fl-nur-p)))))))

(deftest data-skips-parent-und-child-lost-base
  @welt-da
  (let [p (zweig "kette-p") c (zweig "kette-c")]
    (is (ok? (tun! :fl-anlegen {:nr "K-M" :text "main"})))
    (gabeln! "main" p)
    (tx/with-branch p
      (is (ok? (tun! :fl-anlegen {:nr "K-P" :text "nur in p"})))
      (is (ok? (tun! :fl-aendern {:teil "K-M" :text "von p"})))
      (is (install! {:objects {:fl-kette {:primary-key :nr :attributes [[:nr :string]]
                                          :plural "Fl ketten"}}})))
    (gabeln! p c)
    (let [vorher (tx/with-branch c {:teile (teile) :typen (typen)})]
      (testing "data-skips-parent: das Kind liest die Zeilen seines Schema-Parents p, nicht nur main"
        (is (= "nur in p" (get-in vorher [:teile "K-P"])))
        (is (= "von p" (get-in vorher [:teile "K-M"])))
        (is (contains? (:typen vorher) :fl-kette)))
      (testing "child-lost-base: p wird gelöscht; c liest danach dasselbe wie davor"
        (is (ok? (api/delete! :branch {:name p})))
        (is (not (world/exists? (ctx/store) p)))
        (let [nachher (tx/with-branch c {:teile (teile) :typen (typen)})]
          (is (= (:teile vorher) (:teile nachher)))
          (is (= (:typen vorher) (:typen nachher))))))
    (api/delete! :branch {:name c})))

(deftest nach-dem-drop-der-mitte-antwortet-das-branch-verzeichnis
  @welt-da
  (let [p (zweig "mitte-p") c (zweig "mitte-c")]
    (gabeln! "main" p)
    (gabeln! p c)
    (try
      (is (ok? (api/delete! :branch {:name p})))
      (testing "das Kind lebt weiter, also muss die Liste der Zweige es zeigen können"
        (let [r (api/list-branches)]
          (is (ok? r) (pr-str (:body r)))
          (is (some #(= c (:name %)) (get-in r [:body :branches])))))
      (testing "die Aufbewahrung läuft weiter"
        (let [r (api/post! :branch-sweep {:dry-run? true})]
          (is (ok? r) (pr-str (:body r)))))
      (finally (api/delete! :branch {:name c})))))

(deftest half-branch-kontrollzeilen-und-daten-schema-gibt-es-beide-oder-keins
  @welt-da
  (let [b    (zweig "halb")
        orig layout/fork!]
    (testing "ein Fehler nach dem Anlegen des Daten-Schemas, im selben Fork"
      (let [r (with-redefs [layout/fork! (fn [c from to sig rev]
                                           (orig c from to sig rev)
                                           (when (= to b)
                                             (throw
                                              (ex-info "eingeschleust: nach dem Daten-Schema"
                                                       {}))))]
                (api/post! :branches {:parent "main" :name b}))]
        (is (not (ok? r)))))
    (testing "danach gibt es weder die Branch-Zeile noch das Daten-Schema"
      (is (not (world/exists? (ctx/store) b)))
      (is (not (ns-da? b))))
    (testing "derselbe Name lässt sich danach sauber anlegen: beides ist da"
      (gabeln! "main" b)
      (is (world/exists? (ctx/store) b))
      (is (ns-da? b)))))

(defn- staging-name []
  (some #(when (and (str/starts-with? % "staging/") (str/ends-with? % "/main")) %)
        (map :name (world/branches (ctx/store)))))

(deftest rebase-half-schema-und-basis-gehen-zusammen-oder-gar-nicht
  @welt-da
  (api/delete! :staging)
  (is (ok? (api/post! :install
                      {:schema {:objects {:fl-entwurf {:primary-key :nr :attributes [[:nr :string]]
                                                       :plural "Fl-Entwürfe"}}}})))
  (let [s (staging-name)
        z (zweig "rebase-quelle")]
    (is (some? s))
    (gabeln! "main" z)
    (tx/with-branch z
      (is (install! {:objects {:fl-von-main {:primary-key :nr :attributes [[:nr :string]]
                                             :plural "Fl-von-Mains"}}})))
    (landen! z)
    (let [basis-vorher (:base (world/branch (ctx/store) s))
          welt-vorher  (world/authored (world/at (ctx/store) s))
          orig         layout/layout!]
      (testing "ein Fehler nach dem Schreiben des rebasten Schemas, im selben Commit"
        (let [r (with-redefs [layout/layout! (fn [c n rev change]
                                               (when (= n s)
                                                 (throw
                                                  (ex-info "eingeschleust: nach dem Schema-Write"
                                                           {})))
                                               (orig c n rev change))]
                  (api/post! :staging-rebase {}))]
          (is (not (ok? r)) (pr-str (:body r)))))
      (testing "danach sind Schema und Basis beide alt"
        (is (= basis-vorher (:base (world/branch (ctx/store) s))))
        (is (= welt-vorher (world/authored (world/at (ctx/store) s)))))
      (testing "ohne Fehler rücken beide zusammen vor"
        (is (ok? (api/post! :staging-rebase {})))
        (let [basis (:base (world/branch (ctx/store) s))]
          (is (= "main" (:branch basis)))
          (is (> (long (:rev basis)) (long (:rev basis-vorher)))))
        (is (contains? (typen) :fl-von-main))
        (is (ok? (api/post! :staging)))
        (is (contains? (typen) :fl-entwurf))))))

(defn- historie [b] (world/history (world/at (ctx/store) b) {:limit 5000}))

(deftest lost-provenance-die-historie-einer-gemergten-quelle-bleibt-ueber-den-zweiten-elter-lesbar
  @welt-da
  (let [q (zweig "herkunft")]
    (gabeln! "main" q)
    (tx/with-branch q
      (is (install! {:objects {:fl-herkunft {:primary-key :nr :attributes [[:nr :string]]
                                             :plural "Fl-Herkünfte"}}}))
      (is (install! {:objects {:fl-herkunft {:primary-key :nr
                                             :attributes [[:nr :string] [:zweites :string]]
                                             :plural "Fl-Herkünfte"}}})))
    (let [eigene (into #{} (comp (filter #(= q (:branch %))) (map :rev)) (historie q))
          kopf   (:rev (world/head (world/at (ctx/store) q)))]
      (is (<= 2 (count eigene)))
      (landen! q)
      (testing "der Merge-Commit nennt den Quell-Kopf als zweiten Elter (D2)"
        (let [merge (first (filter #(and (= "main" (:branch %)) (:schema? %)) (historie "main")))]
          (is (= 2 (count (:parents merge))) (pr-str merge))
          (is (= kopf (second (:parents merge))))))
      (testing "Quelle löschen und Aufräumen laufen lassen: ihre Commits stehen weiter in main's Historie"
        (is (ok? (api/delete! :branch {:name q})))
        (is (ok? (api/post! :branch-sweep {:dry-run? false})))
        (let [auf-main (into #{} (map :rev) (historie "main"))]
          (is (every? auf-main eigene))))
      (testing "auch nachdem ein neuer Zweig gleichen Namens entstanden ist"
        (gabeln! "main" q)
        (let [auf-main (into {} (map (juxt :rev identity)) (historie "main"))]
          (is (every? #(= q (:branch (get auf-main %))) eigene)))))))

(deftest reapplied-staged-ein-abbruch-nach-dem-anwenden-wendet-nicht-erneut-an
  @welt-da
  (is (ok? (api/mutate :fl-anlegen {:nr "RS-1" :text "einmal"} {:stage? true})))
  (let [n    (atom 0)
        orig @#'bm/reconcile-after-commit!]
    (testing "Abbruch nach dem Commit auf main, in den Nachläufern (der Staging-Branch endet im Commit)"
      (with-redefs [bm/reconcile-after-commit! (fn [& args]
                                     (if (= 1 (swap! n inc))
                                       (throw (ex-info "eingeschleust: nach dem Anwenden" {}))
                                       (apply orig args)))]
        (is (not (ok? (api/post! :staging))))))
    (testing "der Wiederholungsversuch findet nichts mehr anzuwenden"
      (let [r (api/post! :staging)]
        (is (or (ok? r) (= 200 (:status r))) (pr-str (:body r)))))
    (is (= 1 (count (filter #(= "RS-1" (get-in % [:values :nr]))
                            (:rows (:body (api/list-objects :fl-teil))))))
        "das gestagte Objekt steht genau einmal auf main")))

(defn- teil-id [nr]
  (some #(when (= nr (get-in % [:values :nr])) (:id %))
        (:rows (:body (api/list-objects :fl-teil)))))

(defn- gestagt []
  (get-in (api/get! :staging) [:body :staged]))

(deftest stale-replay-eine-gestagte-action-trifft-den-heutigen-stand
  @welt-da
  (is (ok? (tun! :fl-anlegen {:nr "SR-1" :text "da"})))
  (let [id (teil-id "SR-1")]
    (is (ok? (api/mutate :fl-aendern {:teil id :text "gestagt"} {:stage? true})))
    (is (ok? (tun! :fl-loeschen {:teil id})) "auf main inzwischen gelöscht")
    (let [r (api/post! :staging)]
      (is (= 409 (:status r)) (pr-str (:body r)))
      (is (re-find #"does not apply" (str (get-in r [:body :error])))))
    (is (nil? (teil-id "SR-1")) "die alte Wirkung wird nicht abgespielt — nichts ersteht wieder")
    (api/delete! :staging)))

(deftest unchecked-criterion-das-kriterium-gilt-auf-dem-heutigen-main
  @welt-da
  (is (ok? (tun! :fl-anlegen {:nr "UC-1" :text "offen"})))
  (let [id (teil-id "UC-1")]
    (is (ok? (api/mutate :fl-umbenennen {:teil id :text "neu"} {:stage? true}))
        "beim Stagen gilt das Kriterium")
    (is (ok? (tun! :fl-aendern {:teil id :text "gesperrt"})))
    (is (= 409 (:status (api/post! :staging))) "beim Commit gilt es nicht mehr")
    (is (= "gesperrt" (get (teile) "UC-1")))
    (api/delete! :staging)))

(deftest lost-staged-verwerfen-nimmt-genau-eine-zeile
  @welt-da
  (is (ok? (api/mutate :fl-anlegen {:nr "LS-1" :text "eins"} {:stage? true})))
  (is (ok? (api/mutate :fl-anlegen {:nr "LS-2" :text "zwei"} {:stage? true})))
  (let [[a b] (map :seq (gestagt))
        orig  staged-run/stage-requests!]
    (testing "Abbruch beim Neuaufbau der Vorschau: die übrige Zeile bleibt"
      (with-redefs [staged-run/stage-requests! (fn [req calls & [opts]]
                                          (if (false? (:rows? opts))
                                            (throw (ex-info "eingeschleust: Vorschau-Neuaufbau" {}))
                                            (orig req calls opts)))]
        (api/delete! :staged-by-seq {:seq a}))
      (is (= [b] (map :seq (gestagt)))))
    (testing "und landet beim Commit"
      (is (ok? (api/post! :staging)))
      (is (= "zwei" (get (teile) "LS-2")))
      (is (nil? (get (teile) "LS-1"))))))
