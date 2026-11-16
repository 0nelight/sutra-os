(ns ^:api scenario.aviation-pipeline-test
  (:require [sutra.catalog.grants :as grants]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [support.in-project :refer [install-in-project]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [sutra.access.principals :as pr]
            [support.aviation :refer [ontologie]]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(defn- spalte [n t] {:attribute-name n :attribute-type t})

(def ^:private welt
  (-> ontologie
      (update :objects merge
              {:flug    {:primary-key :flug-nr :plural "Flüge"
                         :attributes [[:flug-nr :string] [:kennzeichen :string] [:datum :date]
                                      [:block-min :long] [:landungen :long]]}
               :bauteil {:plural "Bauteile"
                         :attributes [[:seriennummer :string] [:teilenummer :string]
                                      [:kennzeichen :string]
                                      [:stunden :decimal {:constraints {:min 0}}]]}})
      (update :links conj
              [:flug-luftfahrzeug :flug :luftfahrzeug :many-to-one
               {:fk :kennzeichen :forward-api-name :geflogen-von :reverse-api-name :fluege}])
      (update :transforms merge
             {:flugbuch-lesen
              {:language :pg-sql
               :query "SELECT flug_nr AS flug_nr, kennzeichen, datum, block_min, landungen, geloescht AS is_deleted FROM f"
               :inputs {:f {:kind :dataset :dataset :flugbuch}}
               :incremental true
               :primary-key :flug-nr
               :output {:kind :object-type :object-type :flug}}
              :teilestamm-lesen
              {:language :pg-sql
               :query "SELECT seriennummer, teilenummer, kennzeichen, stunden FROM t"
               :inputs {:t {:kind :dataset :dataset :teilestamm}}
               :primary-key :seriennummer
               :output {:kind :object-type :object-type :bauteil}}})))

(defn- als-admin [f]
  (binding [tx/*auth-token* (-> (api/post! :login {:username grants/admin-username
                                                   :password pr/default-admin-password})
                                :body :token)]
    (f)))

(defn- mit-welt [f]
  (assert (ok? (api/mutate :create-project {:name :flotte} {:stage? true})))
  (assert (ok? (api/mutate :create-project {:name :betrieb} {:stage? true})))
  (assert (ok? (api/mutate :create-dataset
                           {:name :flugbuch :project :flotte
                            :columns [(spalte :flug_nr :string) (spalte :kennzeichen :string)
                     (spalte :datum :date) (spalte :block_min :long)
                     (spalte :landungen :long) (spalte :geloescht :boolean)]}
                           {:stage? true})))
  (assert (ok? (api/mutate :create-dataset
                           {:name :teilestamm :project :flotte
                            :columns [(spalte :seriennummer :string) (spalte :teilenummer :string)
                     (spalte :kennzeichen :string) (spalte :stunden :decimal)]}
                           {:stage? true})))
  (assert (ok? (api/post! :staging)))
  (let [r (install-in-project {:schema welt})] (assert (ok? r) (pr-str (:body r))))
  (let [c (api/post! :staging)] (assert (ok? c) (pr-str (:body c))))
  (doseq [k ["D-AINA" "D-AINB"]]
    (assert (ok? (api/mutate :luftfahrzeug-registrieren {:kennzeichen k :muster "A320"}
                             {:publish? true}))))
  (f))

(use-fixtures :once pod/with-pod-auth als-admin mit-welt)

(defn- schreiben! [ds modus zeilen]
  (let [r (api/post! :dataset-write {:name (name ds)} {:rows (mapv (fn [v] {:values v}) zeilen)
                                                       :mode modus :message "Lieferung"})]
    (assert (ok? r) (pr-str (:body r)))))

(defn- bauen! [& [opts]]
  (let [r (api/post! :import-run opts)]
    (is (ok? r) (pr-str (:body r)))
    r))

(defn- zeilen [T] (:rows (:body (api/list-objects T))))

(defn- nach [T attr] (into {} (map (juxt #(get-in % [:values attr]) identity)) (zeilen T)))

(defn- flug [nr k tag min l & [weg?]]
  {:flug_nr nr :kennzeichen k :datum tag :block_min min :landungen l :geloescht (boolean weg?)})

(deftest das-flugbuch-wird-zu-fluegen
  (testing "ein Build macht aus den Zeilen Objekte, mit Primärschlüssel und FK-Link"
    (schreiben! :flugbuch :snapshot [(flug "LH100" "D-AINA" "2026-10-01" 95 1)
                                     (flug "LH101" "D-AINA" "2026-10-01" 90 1)
                                     (flug "LH200" "D-AINB" "2026-10-02" 120 1)])
    (bauen!)
    (is (= #{"LH100" "LH101" "LH200"} (set (keys (nach :flug :flug-nr)))))
    (is (= 95 (get-in (nach :flug :flug-nr) ["LH100" :values :block-min])))
    (let [lfz (get-in (nach :luftfahrzeug :kennzeichen) ["D-AINA" :id])
          r   (api/post! :ontology-query
                         {:find :luftfahrzeug :where {:kennzeichen {:= "D-AINA"}}
                          :around [:fluege]})]
      (is (some? lfz))
      (is (= #{"LH100" "LH101"} (set (map #(get-in % [:values :flug-nr]) (:rows (:body r)))))
          (pr-str (:body r)))))

  (testing "inkrementell: neue Zeilen kommen dazu, ein Lösch-Marker nimmt einen Flug weg"
    (schreiben! :flugbuch :snapshot [(flug "LH100" "D-AINA" "2026-10-01" 95 1 true)
                                     (flug "LH101" "D-AINA" "2026-10-01" 90 1)
                                     (flug "LH200" "D-AINB" "2026-10-02" 120 1)
                                     (flug "LH300" "D-AINB" "2026-10-03" 60 2)])
    (bauen!)
    (is (= #{"LH101" "LH200" "LH300"} (set (keys (nach :flug :flug-nr))))))

  (testing "ein wieder gemeldeter Flug lebt wieder auf"
    (schreiben! :flugbuch :snapshot [(flug "LH100" "D-AINA" "2026-10-01" 97 1)
                                     (flug "LH101" "D-AINA" "2026-10-01" 90 1)
                                     (flug "LH200" "D-AINB" "2026-10-02" 120 1)
                                     (flug "LH300" "D-AINB" "2026-10-03" 60 2)])
    (bauen!)
    (is (= 97 (get-in (nach :flug :flug-nr) ["LH100" :values :block-min]))))

  (testing "inkrementell heißt: eine angehängte Lieferung kommt dazu, der Bestand bleibt"
    (schreiben! :flugbuch :append [(flug "LH400" "D-AINA" "2026-10-04" 70 1)])
    (bauen!)
    (is (= #{"LH100" "LH101" "LH200" "LH300" "LH400"} (set (keys (nach :flug :flug-nr)))))))

(deftest der-teilestamm-ist-ein-vollabgleich
  (testing "ohne :incremental ersetzt jede Lieferung den Bestand"
    (schreiben! :teilestamm :snapshot
                [{:seriennummer "S-1" :teilenummer "P-7" :kennzeichen "D-AINA" :stunden 10M}
                 {:seriennummer "S-2" :teilenummer "P-7" :kennzeichen "D-AINB" :stunden 20M}])
    (bauen!)
    (is (= #{"S-1" "S-2"} (set (keys (nach :bauteil :seriennummer)))))
    (schreiben! :teilestamm :snapshot
                [{:seriennummer "S-2" :teilenummer "P-7" :kennzeichen "D-AINB" :stunden 21M}])
    (bauen!)
    (is (= #{"S-2"} (set (keys (nach :bauteil :seriennummer)))))
    (is (== 21 (get-in (nach :bauteil :seriennummer) ["S-2" :values :stunden]))))

  (testing "append hängt an die Lieferung an, statt sie zu ersetzen"
    (schreiben! :teilestamm :append
                [{:seriennummer "S-3" :teilenummer "P-9" :kennzeichen "D-AINA" :stunden 0M}])
    (bauen!)
    (is (= #{"S-2" "S-3"} (set (keys (nach :bauteil :seriennummer))))))

  (testing "ein doppelter Schlüssel oder ein verletzter Wert: der Typ veröffentlicht nichts, der Build sagt es"
    (schreiben! :teilestamm :snapshot
                [{:seriennummer "S-4" :teilenummer "P-1" :kennzeichen "D-AINA" :stunden 1M}
                 {:seriennummer "S-4" :teilenummer "P-2" :kennzeichen "D-AINA" :stunden 2M}])
    (let [r (api/post! :import-run)]
      (is (not (ok? r)))
      (is (re-find #"duplicate-primary-key" (pr-str (:body r))) (pr-str (:body r))))
    (is (= #{"S-2" "S-3"} (set (keys (nach :bauteil :seriennummer))))
        (pr-str (keys (nach :bauteil :seriennummer))))
    (schreiben! :teilestamm :snapshot
                [{:seriennummer "S-5" :teilenummer "P-1" :kennzeichen "D-AINA" :stunden -1M}])
    (is (not (ok? (api/post! :import-run))))
    (is (= #{"S-2" "S-3"} (set (keys (nach :bauteil :seriennummer)))))
    (testing "die Flüge daneben sind davon unberührt"
      (is (contains? (nach :flug :flug-nr) "LH400")))))

(defn- install! [schema]
  (let [r (install-in-project {:schema schema})
        c (when (ok? r) (api/post! :staging))]
    (when-not (and (ok? r) (ok? c)) (api/delete! :staging))
    {:ok? (boolean (and (ok? r) (ok? c))) :body (:body r) :commit (:body c)
     :text (pr-str (:body r) (:body c))}))

(def ^:private auswertung
  {:transforms
   {:stunden-je-lfz
    {:language :pg-sql
     :query "SELECT kennzeichen, SUM(block_min) AS minuten, SUM(landungen) AS zyklen FROM f GROUP BY kennzeichen"
     :inputs {:f {:kind :object-type :object-type :flug}}
     :output-columns [[:kennzeichen :string] [:minuten :long] [:zyklen :long]]}
    :umlauf
    {:language :pg-sql
     :query "SELECT kennzeichen AS kennzeichen, minuten / 60 AS stunden FROM s"
     :inputs {:s {:kind :transform :transform :stunden-je-lfz}}
     :output-columns [[:kennzeichen :string] [:stunden :long]]}}})

(deftest transforms-bauen-aufeinander-auf
  (is (:ok? (install! auswertung)))
  (schreiben! :flugbuch :snapshot [(flug "LH100" "D-AINA" "2026-10-01" 95 1)
                                   (flug "LH200" "D-AINB" "2026-10-02" 120 1)])
  (bauen!)

  (testing "die Vorschau rechnet über den heutigen Stand und schreibt nichts"
    (let [p (:body (api/preview-transform :stunden-je-lfz 10))]
      (is (nil? (:error p)) (pr-str p))
      (is (= #{"D-AINA" "D-AINB"} (set (map :kennzeichen (:rows p)))))
      (is (= (:count p) (count (:rows p)))))
    (testing "eine Grenze kürzt die Ausgabe, nicht die Eingabe"
      (let [p (:body (api/preview-transform :stunden-je-lfz 1))]
        (is (= 2 (:count p)))
        (is (= 1 (count (:rows p))))))
    (is (= 404 (:status (api/preview-transform :gibt-es-nicht 5)))))

  (testing "ein Build rechnet die Kette durch, ein Transform liest den anderen"
    (bauen! {:force true})
    (let [p (:body (api/preview-transform :umlauf 10))]
      (is (nil? (:error p)) (pr-str p))
      (is (seq (:rows p)))))

  (testing "ein Transform, den ein anderer liest, lässt sich nicht löschen; das Blatt schon"
    (let [r (api/delete! :transform-by {:name "stunden-je-lfz"})]
      (is (= 409 (:status r)) (pr-str (:body r)))
      (is (re-find #"transforms umlauf" (str (get-in r [:body :error]))) (pr-str (:body r))))
    (is (ok? (api/delete! :transform-by {:name "umlauf"})))
    (is (ok? (api/post! :staging)))
    (is (ok? (api/delete! :transform-by {:name "stunden-je-lfz"})))
    (is (ok? (api/post! :staging)))
    (is (not-any? #(#{:umlauf :stunden-je-lfz} (keyword (:transform-name %)))
                  (vals (:transforms (:body (api/get! :schema))))))))

(deftest sql-wird-beim-install-uebersetzt
  (testing "SQL, das Postgres nicht versteht, wird beim Install abgelehnt und benannt"
    (let [{:keys [ok? body]} (install!
                              {:transforms {:jahr {:language :pg-sql
                                                  :query "SELECT CAST(YEAR(f.datum) AS VARCHAR) AS k
                                                          FROM f"
                                                  :inputs {:f {:kind :dataset :dataset :flugbuch}}
                                                  :output-columns [[:k :string]]}}})]
      (is (not ok?))
      (is (re-find #"sql-refused" (pr-str body)))
      (is (re-find #"year" (pr-str body)))))
  (testing "CAST AS CHAR übersetzt, ist aber char(1) — die stille Falle wird abgelehnt"
    (let [{:keys [ok? body]} (install!
                              {:transforms {:zeichen {:language :pg-sql
                                                     :query "SELECT CAST(f.block_min AS CHAR) AS k
                                                             FROM f"
                                                     :inputs
                                                       {:f {:kind :dataset :dataset :flugbuch}}
                                                     :output-columns [[:k :string]]}}})]
      (is (not ok?))
      (is (re-find #"sql-char-without-length" (pr-str body)))))
  (testing "Kommentare mit genau diesen Fehlern stören nicht"
    (is
     (:ok?
      (install!
       {:transforms {:sauber {:language :pg-sql
                               :query "SELECT CAST(f.block_min AS VARCHAR) AS k -- YEAR(f.datum)\nFROM f /* CAST(x AS CHAR) */"
                               :inputs {:f {:kind :dataset :dataset :flugbuch}}
                               :output-columns [[:k :string]]}}}))))
  (testing "ein Entwurf wird geprüft, ohne zu stagen"
    (let [b (:body (api/post! :schema-sql-check
                              {:transforms {:jahr {:language :pg-sql
                                                   :query "SELECT YEAR(f.datum) AS k FROM f"
                                                   :inputs
                                                     {:f {:kind :dataset :dataset :flugbuch}}}}}))]
      (is (= [:sql-refused] (map (comp keyword :rule) (:findings b))) (pr-str b)))
    (is (empty? (get-in (api/get! :staging) [:body :staged]))))
  (testing "dieselbe Prüfung an der Mutationstür"
    (is (not (ok? (api/mutate :create-transform
                              {:name :zeichen-2 :project :flotte :language :pg-sql
                               :query "SELECT CAST(f.block_min AS CHAR) AS k
                                       FROM f"
                               :inputs {:f {:kind :dataset :dataset :flugbuch}}
                               :output-columns [[:k :string]]}
                              {:stage? true}))))
    (api/delete! :staging))
  (testing "ein Eingang, den erst dieser Push anlegt, ist unbekannt: nicht grün, nicht rot, sondern genannt"
    (let [ohne (api/post! :install
                          {:schema
                             {:datasets
                                {:wetter {:kind :standalone :columns [(spalte :ort :string)]}}}})]
      (is (re-find #"resource-without-project" (pr-str (:body ohne)))
          "ein Dataset gehört einem Projekt")
      (api/delete! :staging))
    (let [r (install-in-project
             {:schema {:projects {:betrieb {}}
                       :datasets
                         {:wetter {:project :betrieb :kind :standalone
                                   :columns [(spalte :ort :string)]}}
                       :transforms {:auf-wetter {:language :pg-sql
                                                 :query "SELECT YEAR(w.ort) AS k FROM w"
                                                 :inputs {:w {:kind :dataset :dataset :wetter}}
                                                 :output-columns [[:k :string]]}}}})]
      (is (ok? r) (pr-str (:body r)))
      (is (some #(re-find #"auf-wetter.*NOT compiled" %) (get-in r [:body :warnings])))
      (api/delete! :staging)))
  (testing "die Prüfung aller installierten SQL-Transforms"
    (let [b (:body (api/post! :schema-sql-check {}))]
      (is (:compiled? b) (pr-str b))
      (is (some #{:sauber} (map keyword (:checked b))))
      (is (empty? (:findings b)))))
  (testing "ändert sich ein Eingang nachträglich, wird der Bestand rot und benannt"
    (is (ok? (api/mutate :create-dataset
                         {:name :wartungsfenster :project :flotte
                          :columns [(spalte :stunden :long)]} {:stage? true})))
    (is (ok? (api/post! :staging)))
    (is (:ok? (install!
               {:transforms {:fenster {:language :pg-sql
                                       :query "SELECT CAST(w.stunden + 1 AS VARCHAR) AS k FROM w"
                                       :inputs {:w {:kind :dataset :dataset :wartungsfenster}}
                                       :output-columns [[:k :string]]}}})))
    (is (ok? (api/mutate :update-dataset
                         {:name :wartungsfenster :columns [(spalte :stunden :string)]}
                         {:stage? true})))
    (is (ok? (api/post! :staging)))
    (schreiben! :wartungsfenster :snapshot [{:stunden "zwölf"}])
    (let [b (:body (api/post! :schema-sql-check {}))]
      (is (= [["fenster" :sql-refused]]
             (map (juxt (comp name :entity) (comp keyword :rule)) (:findings b))) (pr-str b)))
    (testing "und ein Build mit ihm scheitert an genau diesem Job"
      (let [r (api/post! :import-run {:force true})]
        (is (= [:fenster] (map keyword (get-in r [:body :data :failed-jobs]))) (pr-str (:body r)))))
    (is (ok? (api/delete! :transform-by {:name "fenster"})))
    (is (ok? (api/post! :staging)))))

(deftest ein-build-hinterlaesst-protokoll
  (schreiben! :teilestamm :snapshot
              [{:seriennummer "S-9" :teilenummer "P-1" :kennzeichen "D-AINA" :stunden 1M}])
  (bauen! {:force true})
  (testing "jede Ausgabe schreibt eine Transaktion; ein Vollabgleich ist SNAPSHOT, ein inkrementeller APPEND"
    (let [tx-von (fn [t]
                   (:transactions (:body (api/get! :catalog-transactions {:dataset (name t)}))))]
      (is (= "SNAPSHOT" (:type (first (tx-von :bauteil)))) (pr-str (first (tx-von :bauteil))))
      (is (= "APPEND" (:type (first (tx-von :flug)))) (pr-str (first (tx-von :flug))))))
  (testing "der Lauf steht in der Liste, seine Jobs mit Status"
    (let [lauf (first (:runs (:body (api/list-runs "import"))))
          jobs (:jobs (:body (api/get! :run-jobs {:id (:id lauf)})))]
      (is (= "succeeded" (str (:status lauf))) (pr-str lauf))
      (is (seq jobs))
      (is (not-any? #(= "FAILED" (str (:status %))) jobs) (pr-str jobs))
      (is (some #(= "COMPLETED" (str (:status %))) jobs) (pr-str jobs))))
  (testing "ein abgelehnter Lauf benennt den Job und den Grund"
    (schreiben! :teilestamm :snapshot
                [{:seriennummer "S-9" :teilenummer "P-1" :kennzeichen "D-AINA" :stunden 1M}
                 {:seriennummer "S-9" :teilenummer "P-2" :kennzeichen "D-AINA" :stunden 1M}])
    (is (not (ok? (api/post! :import-run))))
    (let [lauf (first (:runs (:body (api/list-runs "import"))))
          jobs (:jobs (:body (api/get! :run-jobs {:id (:id lauf)})))]
      (is (= "failed" (str (:status lauf))))
      (is (some #(and (= "FAILED" (str (:status %))) (re-find #"duplicate" (str (:error %)))) jobs)
          (pr-str jobs))))
  (schreiben! :teilestamm :snapshot
              [{:seriennummer "S-9" :teilenummer "P-1" :kennzeichen "D-AINA" :stunden 1M}])
  (bauen!))

(deftest ersetzen-statt-anhaengen
  (is
   (:ok? (install!
          {:objects
             {:tagesplan {:attributes [[:flug-nr :string] [:kennzeichen :string]]
                          :plural "Tagespläne"}}
           :transforms {:tagesplan-lesen
                        {:language :pg-sql
                         :query "SELECT flug_nr AS flug_nr, kennzeichen FROM f WHERE NOT geloescht"
                         :inputs {:f {:kind :dataset :dataset :flugbuch}}
                         :primary-key :flug-nr
                         :incremental {:read-mode :current :write-mode :replace}
                         :output {:kind :object-type :object-type :tagesplan}}}})))
  (is (= {:read-mode :current :write-mode :replace}
         (:incremental (some #(when (= :tagesplan-lesen (keyword (:transform-name %))) %)
                             (vals (:transforms (:body (api/get! :schema))))))))
  (schreiben! :flugbuch :snapshot
              [(flug "LH500" "D-AINA" "2026-10-05" 60 1) (flug "LH501" "D-AINB" "2026-10-05" 60 1)])
  (bauen!)
  (is (= #{"LH500" "LH501"} (set (keys (nach :tagesplan :flug-nr)))))
  (testing ":current + :replace liest den ganzen heutigen Stand und schrumpft mit ihm"
    (schreiben! :flugbuch :snapshot [(flug "LH501" "D-AINB" "2026-10-05" 60 1)])
    (bauen!)
    (is (= #{"LH501"} (set (keys (nach :tagesplan :flug-nr)))))
    (is (= "SNAPSHOT"
           (:type
            (first
             (:transactions (:body (api/get! :catalog-transactions {:dataset "tagesplan"})))))))))

(def ^:private befundung-py
  "from sutra_transforms import transform, Input, Output, previous_output

@transform(inputs={'teile': Input(kind='dataset', dataset='teilestamm')},
           output=Output(kind='object-type', object_type='befundung', primary_key='seriennummer'),
           incremental=True)
def compute(teile):
    schon = {r.get('seriennummer') for r in previous_output()}
    return [{'seriennummer': r['seriennummer'],
             'klasse': 'hoch' if float(r['stunden'] or 0) > 10 else 'niedrig'}
            for r in teile if r['seriennummer'] not in schon]")

(deftest eine-teure-auswertung-laeuft-nur-fuer-das-neue
  (is (:ok? (install! {:objects
                         {:befundung {:attributes [[:seriennummer :string] [:klasse :string]]
                                      :plural "Befundungen"}}
                       :transforms
                         {:befunden {:language :python :code befundung-py :incremental true
                                     :inputs {:teile {:kind :dataset :dataset :teilestamm}}
                                     :primary-key :seriennummer
                                     :output {:kind :object-type :object-type :befundung}}}})))
  (let [letzte (fn []
                 (first
                  (:transactions (:body (api/get! :catalog-transactions {:dataset "befundung"})))))
        delta  (fn [] (long (or (:row-delta (letzte)) (:row_delta (letzte)) 0)))]
    (schreiben! :teilestamm :snapshot
                [{:seriennummer "B-1" :teilenummer "P-1" :kennzeichen "D-AINA" :stunden 20M}
                 {:seriennummer "B-2" :teilenummer "P-1" :kennzeichen "D-AINA" :stunden 5M}])
    (bauen!)
    (is (= {"B-1" "hoch" "B-2" "niedrig"}
           (update-vals (nach :befundung :seriennummer) #(get-in % [:values :klasse]))))
    (testing "unveränderte Eingabe, erzwungener Lauf: nichts wird neu verarbeitet"
      (bauen! {:force true})
      (is (= "APPEND" (:type (letzte))) (pr-str (letzte)))
      (is (zero? (delta)) (pr-str (letzte))))
    (testing "ein neues Teil: nur dieses wird verarbeitet, das alte bleibt"
      (schreiben! :teilestamm :append
                  [{:seriennummer "B-3" :teilenummer "P-1" :kennzeichen "D-AINB" :stunden 30M}])
      (bauen!)
      (is (= #{"B-1" "B-2" "B-3"} (set (keys (nach :befundung :seriennummer)))))
      (is (= 1 (delta)) (pr-str (letzte))))
    (testing "die Vorschau eines Python-Transforms läuft in der Sandbox und schreibt nichts"
      (let [p (:body (api/preview-transform :befunden 5))]
        (is (= "python" (name (:language p))))
        (is (nil? (:error p)) (pr-str p))))))

(deftest ein-verschobener-schluessel-verwaist-die-edits
  (is (:ok? (install!
             {:actions {:teil-bemerken {:params {:teil {:name "Teil" :type :bauteil :required true}
                                                 :teilenummer
                                                   {:name "Teilenummer" :type :string
                                                    :required true}}
                                        :rules [{:op :update :object-type :bauteil
                                                 :target [:get :teil]
                                                 :values {:teilenummer [:get :teilenummer]}}]
                                        :edits [{:object-type :bauteil}]}}})))
  (schreiben! :teilestamm :snapshot
              [{:seriennummer "K-1" :teilenummer "P-1" :kennzeichen "D-AINA" :stunden 1M}])
  (bauen!)
  (let [r (api/mutate :teil-bemerken {:teil "K-1" :teilenummer "P-1A"} {:publish? true})]
    (is (ok? r) (pr-str (:body r))))
  (testing "§9.2 ein neuer Primärschlüssel trifft jedes Edit des Typs — die Installation verlangt eine Migration"
    (let [{:keys [ok? body]} (install!
                              {:transforms {:teilestamm-lesen
                                            (assoc (get-in welt [:transforms :teilestamm-lesen])
                                                   :primary-key :teilenummer)}})]
      (is (not ok?))
      (is (re-find #"primary-key-changed|migration" (pr-str body)) (pr-str body)))))

(defn- ergaenzung [& {:as o}]
  (merge {:language :pg-sql
          :query "SELECT seriennummer, einsatzort FROM e"
          :inputs {:e {:kind :dataset :dataset :einsatzorte}}
          :output {:kind :object-type :object-type :bauteil :role :supplement
                   :primary-key :seriennummer
                   :column-map {:seriennummer :seriennummer :einsatzort :einsatzort}}}
         o))

(deftest eine-ergaenzung-traegt-spalten-zum-selben-schluessel
  (is (ok? (api/mutate :create-dataset
                       {:name :einsatzorte :project :flotte
                        :columns [(spalte :seriennummer :string) (spalte :einsatzort :string)]}
                       {:stage? true})))
  (is (ok? (api/post! :staging)))
  (let [bauteil (update-in (:bauteil (:objects welt)) [:attributes] conj [:einsatzort :string])]
    (testing "H7 eine Ergänzung teilt den Primärschlüssel ihres Typs"
      (let [r (install! {:objects {:bauteil bauteil}
                                :transforms
                                  {:einbau
                                     (ergaenzung :output
                                                 {:kind :object-type :object-type :bauteil
                                                  :role :supplement
                                                  :primary-key :teilenummer
                                                  :column-map
                                                    {:teilenummer :seriennummer
                                                     :einsatzort :einsatzort}})}})]
        (is (not (:ok? r)))
        (is (re-find #"merge key has to be the same" (:text r)))))
    (testing "… schreibt nur deklarierte Spalten"
      (let [r (install! {:objects {:bauteil bauteil}
                                :transforms
                                  {:einbau
                                     (ergaenzung :output
                                                 {:kind :object-type :object-type :bauteil
                                                  :role :supplement
                                                  :primary-key :seriennummer
                                                  :column-map
                                                    {:seriennummer :seriennummer
                                                     :lage :einsatzort}})}})]
        (is (not (:ok? r)))
        (is (re-find #"may only fill columns that already exist" (:text r)))))
    (testing "… und ist keine zweite Hauptquelle"
      (let [r (install! {:objects {:bauteil bauteil}
                                :transforms
                                  {:einbau
                                     (ergaenzung :output
                                                 {:kind :object-type :object-type :bauteil
                                                  :role :primary
                                                  :primary-key :seriennummer
                                                  :column-map
                                                    {:seriennummer :seriennummer
                                                     :einsatzort :einsatzort}})}})]
        (is (not (:ok? r)))
        (is (re-find #":backing-replaced" (:text r)))))
    (is (:ok? (install! {:objects {:bauteil bauteil} :transforms {:einbau (ergaenzung)}}))))
  (schreiben! :teilestamm :snapshot
              [{:seriennummer "E-1" :teilenummer "P-1" :kennzeichen "D-AINA" :stunden 1M}
               {:seriennummer "E-2" :teilenummer "P-1" :kennzeichen "D-AINA" :stunden 1M}])
  (schreiben! :einsatzorte :snapshot [{:seriennummer "E-1" :einsatzort "Triebwerk links"}
                                     {:seriennummer "E-9" :einsatzort "gibt es nicht"}])
  (bauen!)
  (testing "die Ergänzung legt ihre Spalte auf das Objekt mit demselben Schlüssel"
    (is (= "Triebwerk links" (get-in (nach :bauteil :seriennummer) ["E-1" :values :einsatzort]))))
  (testing "ein Objekt ohne Ergänzungszeile bleibt, wie es ist; eine Ergänzungszeile ohne Objekt legt keins an"
    (is (nil? (get-in (nach :bauteil :seriennummer) ["E-2" :values :einsatzort])))
    (is (= #{"E-1" "E-2"} (set (keys (nach :bauteil :seriennummer))))))
  (schreiben! :einsatzorte :snapshot [{:seriennummer "E-2" :einsatzort "Fahrwerk"}])
  (bauen!)
  (testing "was die Ergänzung nicht mehr liefert, nimmt sie zurück — Schlüssel und Objekt bleiben"
    (let [b (nach :bauteil :seriennummer)]
      (is (nil? (get-in b ["E-1" :values :einsatzort])))
      (is (= "Fahrwerk" (get-in b ["E-2" :values :einsatzort])))
      (is (= #{"E-1" "E-2"} (set (keys b))))
      (is (= "P-1" (get-in b ["E-1" :values :teilenummer])))))
  (schreiben! :einsatzorte :snapshot [])
  (bauen!)
  (testing "eine leere Lieferung nimmt nichts zurück"
    (is (= "Fahrwerk" (get-in (nach :bauteil :seriennummer) ["E-2" :values :einsatzort]))))
  (let [bauteil (update-in (:bauteil (:objects welt)) [:attributes] conj [:einsatzort :string])]
    (is (:ok? (install! {:objects {:bauteil bauteil
                                   :einbau-gelesen {:primary-key :seriennummer
                                                    :plural "Gelesene Einsatzorte"
                                                    :attributes [[:seriennummer :string]
                                                                 [:einsatzort :string]]}}
                         :transforms
                         {:einbau (ergaenzung
                                   :query (str "SELECT seriennummer, einsatzort FROM e"
                                               " WHERE seriennummer = 'E-1'")
                                   :incremental {:read-mode :added :write-mode :modify})
                          :einbau-leser
                          {:language :pg-sql
                           :query "SELECT seriennummer, einsatzort FROM b"
                           :inputs {:b {:kind :object-type :object-type :bauteil}}
                           :output {:kind :object-type :object-type :einbau-gelesen
                                    :primary-key :seriennummer
                                    :column-map {:seriennummer :seriennummer
                                                 :einsatzort :einsatzort}}}}}))))
  (schreiben! :einsatzorte :snapshot [{:seriennummer "E-1" :einsatzort "Triebwerk rechts"}
                                     {:seriennummer "E-2" :einsatzort "anderswo"}])
  (schreiben! :teilestamm :snapshot
              [{:seriennummer "E-1" :teilenummer "P-1" :kennzeichen "D-AINA" :stunden 2M}
               {:seriennummer "E-2" :teilenummer "P-1" :kennzeichen "D-AINA" :stunden 2M}])
  (bauen!)
  (testing "… und wer das Objekt im selben Lauf liest, sieht die behaltene Spalte"
    (let [g (nach :einbau-gelesen :seriennummer)]
      (is (= "Triebwerk rechts" (get-in g ["E-1" :values :einsatzort])))
      (is (= "Fahrwerk" (get-in g ["E-2" :values :einsatzort])))))
  (testing "eine modify-Ergänzung schreibt nur, was dazukam; der Rest behält seine Spalte"
    (let [b (nach :bauteil :seriennummer)]
      (is (= "Triebwerk rechts" (get-in b ["E-1" :values :einsatzort])))
      (is (= "Fahrwerk" (get-in b ["E-2" :values :einsatzort]))))))

(deftest zwei-quellen-ein-objekt
  (doseq [ds [:lager :werkstatt]]
    (is (ok? (api/mutate :create-dataset
                         {:name ds :project :flotte
                          :columns [(spalte :seriennummer :string) (spalte :charge :string)
                    (spalte :ort :string)]}
                         {:stage? true}))))
  (is (ok? (api/post! :staging)))
  (let [quelle (fn [ds pk] {:language :pg-sql :query (str "SELECT seriennummer, charge, ort FROM q")
                            :inputs {:q {:kind :dataset :dataset ds}}
                            :output {:kind :object-type :object-type :komponente :primary-key pk}})
        typ    {:plural "Komponenten"
                :attributes [[:seriennummer :string] [:charge :string] [:ort :string]]
                :match-keys [:seriennummer :charge]}]
    (testing "H7 zwei Hauptquellen für einen Typ ohne Entitätsauflösung: abgelehnt"
      (let [r (install! {:objects {:komponente (dissoc typ :match-keys)}
                         :transforms {:aus-lager (quelle :lager :seriennummer)
                                      :aus-werkstatt (quelle :werkstatt :charge)}})]
        (is (not (:ok? r)))
        (is (re-find #"exactly one source of truth" (:text r)))))
    (let [r (install! {:objects {:komponente (assoc typ :resolve-entity true)}
                       :transforms {:aus-lager (quelle :lager :seriennummer)
                                    :aus-werkstatt (quelle :werkstatt :charge)}})]
      (is (:ok? r) (:text r))))
  (schreiben! :lager :snapshot [{:seriennummer "K-77" :charge "CH-1" :ort "Regal 4"}])
  (schreiben! :werkstatt :snapshot [{:seriennummer "K-77" :charge "CH-1" :ort nil}
                                    {:seriennummer nil :charge "CH-2" :ort "Bank 2"}])
  (bauen!)
  (let [ks (zeilen :komponente)]
    (testing "dieselbe Komponente aus zwei Quellen ist ein Objekt"
      (is (= 2 (count ks)) (pr-str ks)))
    (testing "eine leere Quelle überschreibt nie eine gefüllte"
      (is (= "Regal 4"
             (some #(when (= "K-77" (get-in % [:values :seriennummer])) (get-in % [:values :ort]))
                   ks))))))

(def ^:private lieferung
  (gen/vector
   (gen/hash-map :seriennummer (gen/frequency [[12 (gen/elements ["G-1" "G-2" "G-3" "G-4" "G-5"])]
                                               [1 (gen/return nil)] [1 (gen/return "")]])
                 :teilenummer (gen/elements ["P-1" "P-2"])
                 :kennzeichen (gen/elements ["D-AINA" "D-AINB"])
                 :stunden
                 (gen/frequency [[10 (gen/elements [0M 1.5M 99M])] [1 (gen/return -2M)]
                                 [1 (gen/return nil)]]))
   1 4))

(defn- zulaessig? [zeilen]
  (let [nrn (map :seriennummer zeilen)]
    (and (every? seq nrn)
         (apply distinct? nrn)
         (every? #(or (nil? (:stunden %)) (>= (:stunden %) 0)) zeilen))))

(deftest ein-typ-veroeffentlicht-ganz-oder-gar-nicht
  (let [stand    (atom (set (keys (nach :bauteil :seriennummer))))
        faelle   (atom {})
        r (tc/quick-check 20
                          (prop/for-all [zeilen (gen/no-shrink lieferung)]
                            (schreiben! :teilestamm :snapshot zeilen)
                            (let [ok   (zulaessig? zeilen)
                                  lauf (api/post! :import-run)
                                  ist  (set (keys (nach :bauteil :seriennummer)))
                                  soll (if ok (set (map :seriennummer zeilen)) @stand)]
                              (swap! faelle update ok (fnil inc 0))
                              (reset! stand ist)
                              (and (= ok (ok? lauf)) (= soll ist))))
                          :seed 20261009)]
    (is (:pass? r) (pr-str (:fail r) (:shrunk r)))
    (testing "beide Ausgänge kamen vor"
      (is (every? #(>= (get @faelle % 0) 3) [true false]) (pr-str @faelle)))))

(deftest ein-schluessel-muss-ueber-rebuilds-stabil-sein
  (is (:ok? (install! {:objects
                         {:messwert {:primary-key :seriennummer
                                     :attributes [[:stunden :decimal] [:seriennummer :string]]
                                     :plural "Messwerte"}}
                       :transforms
                         {:messwerte {:language :pg-sql :query "SELECT stunden, seriennummer FROM t"
                                      :inputs {:t {:kind :dataset :dataset :teilestamm}}
                                      :primary-key :stunden
                                      :output {:kind :object-type :object-type :messwert}}}})))
  (schreiben! :teilestamm :snapshot
              [{:seriennummer "M-1" :teilenummer "P-1" :kennzeichen "D-AINA" :stunden 1.5M}])
  (let [r (api/post! :import-run)]
    (is (not (ok? r)))
    (is (re-find #"nondeterministic-primary-key" (pr-str (:body r))) (pr-str (:body r))))
  (is (empty? (zeilen :messwert)))
  (is (ok? (api/delete! :transform-by {:name "messwerte"})))
  (is (ok? (api/post! :staging))))

(defn- transform [n]
  (some #(when (= n (keyword (:transform-name %))) %)
        (vals (:transforms (:body (api/get! :schema))))))

(defn- aktion [n]
  (some #(when (= n (keyword (:action-name %))) %) (vals (:actions (:body (api/get! :schema))))))

(deftest ein-parameter-gilt-zur-laufzeit
  (schreiben! :flugbuch :snapshot
              [(flug "P-1" "D-AINA" "2026-10-06" 30 1) (flug "P-2" "D-AINA" "2026-10-06" 300 1)
               (flug "P-3" "D-AINB" "2026-10-06" 600 1)])
  (is (ok? (api/mutate :create-transform
                       {:name :langstrecken :project :flotte :language :pg-sql
                        :query "SELECT flug_nr, block_min FROM f WHERE block_min >= ${mindestens}"
                        :inputs {:f {:kind :dataset :dataset :flugbuch}}
                        :parameters
                          {:mindestens {:type :number :default 240
                                        :description "Ab wann ein Flug lang ist, in Minuten."}}
                        :output-columns [[:flug_nr :string] [:block_min :long]]}
                       {:stage? true})))
  (is (ok? (api/post! :staging)))
  (testing "der Parameter steht im Schema, die Abfrage behält den Verweis"
    (is (= {:type :number :default 240 :description "Ab wann ein Flug lang ist, in Minuten."}
           (get-in (transform :langstrecken) [:parameters :mindestens])))
    (is (re-find #"\$\{mindestens\}" (str (:query (transform :langstrecken))))))
  (testing "die Vorschau setzt den Wert ein; eine Überschreibung probiert einen anderen, ohne etwas zu ändern"
    (is (= #{"P-2" "P-3"}
           (set (map :flug_nr (:rows (:body (api/preview-transform :langstrecken 50)))))))
    (is (= #{"P-3"}
           (set (map :flug_nr
                     (:rows
                      (:body (api/preview-transform :langstrecken 50 nil {:mindestens 500})))))))
    (is (= 240 (get-in (transform :langstrecken) [:parameters :mindestens :default]))))
  (testing "ein Wert, der nicht zum Typ passt, wird abgelehnt, nicht umgedeutet"
    (let [p (:body (api/preview-transform :langstrecken 50 nil {:mindestens "1 OR 1=1"}))]
      (is (some? (:error p)))
      (is (empty? (:rows p)))))
  (testing "ein Verweis auf einen nicht deklarierten Parameter wird nicht live"
    (let [r (api/mutate :create-transform
                        {:name :tippfehler :project :flotte :language :pg-sql
                         :query "SELECT flug_nr FROM f WHERE block_min >= ${mindestns}"
                         :inputs {:f {:kind :dataset :dataset :flugbuch}}
                         :parameters {:mindestens {:type :number :default 240}}
                         :output-columns [[:flug_nr :string]]}
                        {:stage? true})]
      (when (ok? r) (api/post! :staging))
      (api/delete! :staging)
      (is (nil? (transform :tippfehler)))))
  (is (ok? (api/delete! :transform-by {:name "langstrecken"})))
  (is (ok? (api/post! :staging))))

(def ^:private kurz-py
  "from sutra_transforms import transform, Input, Output

@transform(inputs={'f': Input(kind='dataset', dataset='flugbuch')},
           outputs={'kurz': Output(kind='dataset', dataset='kurzstrecken')})
def compute(f):
    return {'kurz': [{'flug_nr': r['flug_nr']} for r in f if r['block_min'] < 60]}")

(deftest eine-aenderung-ersetzt-was-sie-nennt
  (is (ok? (api/mutate :create-transform
                       {:name :kurzstrecken :project :flotte :language :pg-sql
                        :query "SELECT flug_nr FROM f WHERE block_min < 60"
                        :inputs {:f {:kind :dataset :dataset :flugbuch}}
                        :output-columns [[:flug_nr :string]]}
                       {:stage? true})))
  (is (ok? (api/post! :staging)))

  (testing "K-install: was eine Änderung nicht nennt, bleibt"
    (is (ok? (api/mutate :update-transform
                         {:name :kurzstrecken :description "Flüge unter einer Stunde"}
                         {:stage? true})))
    (is (ok? (api/post! :staging)))
    (is (= "Flüge unter einer Stunde" (:description (transform :kurzstrecken))))
    (is (= :pg-sql (keyword (:language (transform :kurzstrecken)))))
    (is (re-find #"block_min < 60" (str (:query (transform :kurzstrecken))))))

  (testing "… ein Wechsel der Sprache nimmt die alte Verdrahtung mit"
    (is (:ok? (install! {:transforms {:kurzstrecken {:language :python :code kurz-py}}})))
    (is (= :python (keyword (:language (transform :kurzstrecken)))))
    (is (nil? (:query (transform :kurzstrecken))))
    (is (nil? (:output-columns (transform :kurzstrecken)))))

  (testing "eine Action mit neuen Regeln verliert die :edits, die sie nicht mehr nennt"
    (is (ok? (api/mutate :create-action
                         {:name :notiz-setzen
                          :params {:mangel {:name "Mangel" :type :mangel :required true}}
                          :rules [{:op :update :object-type :mangel :target [:get :mangel]
                                   :values {:beschreibung "geprüft"}}]
                          :edits [{:object-type :mangel}]}
                         {:stage? true})))
    (is (ok? (api/post! :staging)))
    (is (ok? (api/mutate :update-action {:name :notiz-setzen :description "nur der Text"}
                         {:stage? true})))
    (is (ok? (api/post! :staging)))
    (is (seq (:edits (aktion :notiz-setzen))))
    (is (ok? (api/mutate :update-action
                         {:name :notiz-setzen
                          :rules [{:op :update :object-type :mangel :target [:get :mangel]
                                   :values {:beschreibung "erneut geprüft"}}]}
                         {:stage? true})))
    (is (ok? (api/post! :staging)))
    (is (empty? (:edits (aktion :notiz-setzen))) (pr-str (aktion :notiz-setzen))))

  (is (ok? (api/delete! :transform-by {:name "kurzstrecken"})))
  (is (ok? (api/post! :staging))))

(deftest ein-transform-wird-einmal-angelegt-und-von-seinem-repository-geloescht
  (is
   (:ok?
    (install!
     {:repositories {:wartung {:type :transforms}}
      :transforms {:stunden-summe {:language :pg-sql :repository :wartung
                                   :query "SELECT kennzeichen, SUM(block_min) AS minuten FROM f GROUP BY kennzeichen"
                                   :inputs {:f {:kind :dataset :dataset :flugbuch}}
                                   :output-columns [[:kennzeichen :string] [:minuten :long]]}}})))

  (testing "K-install: create ist ein Upsert — außer für Transforms; ein zweites Anlegen wird abgelehnt, Ändern nicht"
    (let [r (api/mutate :create-transform
                        {:name :stunden-summe :project :flotte :language :pg-sql
                         :query "SELECT 1 AS x FROM f"
                         :inputs {:f {:kind :dataset :dataset :flugbuch}}
                         :output-columns [[:x :long]]}
                        {:stage? true})]
      (api/delete! :staging)
      (is (not (ok? r)))
      (is (re-find #"update-transform" (pr-str (:body r))) (pr-str (:body r))))
    (is (ok? (api/mutate :update-transform {:name :stunden-summe :description "je Luftfahrzeug"}
                         {:stage? true})))
    (is (ok? (api/post! :staging))))

  (testing "was ein Repository deklariert, löscht nicht das Verb, sondern ein Push dieses Repositorys, der es weglässt"
    (let [r (api/delete! :transform-by {:name "stunden-summe"})]
      (is (= 409 (:status r)) (pr-str (:body r)))
      (is (= "wartung" (str (get-in r [:body :repository]))) (pr-str (:body r))))
    (is (some? (transform :stunden-summe)))
    (let [r (install-in-project {:schema {:repositories {:wartung {:type :transforms}}}
                                 :strict? true :repository "wartung"})]
      (is (ok? r) (pr-str (:body r)))
      (is (ok? (api/post! :staging))))
    (is (nil? (transform :stunden-summe)))))

(def ^:private lieferschein-py
  "from sutra_transforms import transform, Input, Output

@transform(inputs={'teile': Input(kind='dataset', dataset='teilestamm')},
           outputs={'eingang': Output(kind='dataset', dataset='eingang')})
def compute(teile):
    return {'eingang': [{'seriennummer': r['seriennummer'],
                         'lieferant': 'ACME-' + r['teilenummer']} for r in teile]}")

(deftest ein-erzeuger-darf-eine-spalte-mehr-liefern
  (is (ok? (api/mutate :create-dataset
                       {:name :eingang :project :betrieb :columns [(spalte :seriennummer :string)]}
                       {:stage? true})))
  (is (ok? (api/post! :staging)))
  (is (:ok? (install!
             {:transforms {:lieferscheine {:language :python :code lieferschein-py
                                            :inputs {:teile {:kind :dataset :dataset :teilestamm}}
                                            :outputs
                                              [{:kind :dataset :name :eingang :dataset :eingang
                                                :primary-key :seriennummer}]}}})))
  (schreiben! :teilestamm :snapshot
              [{:seriennummer "L-1" :teilenummer "P-1" :kennzeichen "D-AINA" :stunden 1M}])
  (bauen!)
  (let [rid  (some (fn [[r d]] (when (= :eingang (keyword (:dataset-name d))) r))
                   (:datasets (:body (api/get! :schema))))
        rows (:rows (:body (api/get! :dataset-preview {:id rid})))]
    (is (= "ACME-P-1" (some #(get-in % [:values :lieferant]) rows)) (pr-str rows)))
  (testing "ein zweiter Build mit der Spalte, die es nun gibt, läuft genauso"
    (bauen! {:force true}))
  (is (ok? (api/delete! :transform-by {:name "lieferscheine"})))
  (is (ok? (api/post! :staging))))

(deftest ein-build-auf-einem-zweig
  (schreiben! :teilestamm :snapshot
              [{:seriennummer "Z-1" :teilenummer "p-9" :kennzeichen "D-AINA" :stunden 3M}])
  (bauen! {:force true})
  (let [z (str "teile-" (subs (str (random-uuid)) 0 8))]
    (is (ok? (api/post! :branches {:parent "main" :name z})))
    (tx/with-branch z
      (is
       (:ok?
        (install!
         {:objects
            {:bauteil {:attributes [[:seriennummer :string] [:teilenummer :string]
                                    [:kennzeichen :string]
                                    [:stunden :decimal {:constraints {:min 0}}]
                                    [:hersteller :string]]
                       :plural "Bauteile"}}
          :transforms {:teilestamm-lesen
                       {:language :pg-sql
                        :query "SELECT seriennummer, UPPER(teilenummer) AS teilenummer, kennzeichen, stunden, 'ACME' AS hersteller FROM t"
                        :inputs {:t {:kind :dataset :dataset :teilestamm}}
                        :primary-key :seriennummer
                        :output {:kind :object-type :object-type :bauteil}}}})))
      (bauen! {:force true}))
    (testing "Branching: der Build auf dem Zweig rechnet mit dem Code des Zweigs, auch in eine Spalte, die es nur dort gibt"
      (let [z1 (tx/with-branch z (get (nach :bauteil :seriennummer) "Z-1"))]
        (is (= "P-9" (get-in z1 [:values :teilenummer])))
        (is (= "ACME" (get-in z1 [:values :hersteller])))))
    (testing "… und main bleibt, wie es war"
      (let [m1 (get (nach :bauteil :seriennummer) "Z-1")]
        (is (= "p-9" (get-in m1 [:values :teilenummer])))
        (is (not (contains? (:values m1) :hersteller)))))
    (is (ok? (api/delete! :branch {:name z})))))

(defn- berechtigt [lizenz]
  (let [r (api/post! :ontology-query
                     {:find :mechaniker :where {:lizenz {:= lizenz}} :around [:muster-fuer]})]
    (is (ok? r) (pr-str (:body r)))
    (set (map #(get-in % [:values :kennzeichen]) (:rows (:body r))))))

(deftest eine-verbindungstabelle-und-was-benutzer-daran-aendern
  (is (:ok? (install! {:links [[:musterberechtigung :mechaniker :luftfahrzeug :many-to-many
                                {:join {:on [:muster :muster] :from-pk :lizenz :to-pk :kennzeichen}
                                 :forward-api-name :muster-fuer
                                 :reverse-api-name :muster-mechaniker}]]
                       :actions {:muster-berechtigen
                                 {:params
                                    {:mechaniker
                                       {:name "Mechaniker" :type :mechaniker :required true}
                                     :luftfahrzeug
                                       {:name "Luftfahrzeug" :type :luftfahrzeug :required true}}
                                  :rules [{:op :create-link :link-type :musterberechtigung
                                           :from-id [:get :mechaniker :id]
                                           :to-id [:get :luftfahrzeug :id]}]}
                                 :muster-entziehen
                                 {:params
                                    {:mechaniker
                                       {:name "Mechaniker" :type :mechaniker :required true}
                                     :luftfahrzeug
                                       {:name "Luftfahrzeug" :type :luftfahrzeug :required true}}
                                  :rules [{:op :delete-link :link-type :musterberechtigung
                                           :from-id [:get :mechaniker :id]
                                           :to-id [:get :luftfahrzeug :id]}]}
                                 :umschulen
                                 {:params
                                    {:mechaniker
                                       {:name "Mechaniker" :type :mechaniker :required true}
                                     :muster {:name "Muster" :type :string :required true}}
                                  :rules [{:op :update :object-type :mechaniker
                                           :target [:get :mechaniker]
                                           :values {:muster [:get :muster]}}]
                                  :edits [{:object-type :mechaniker}]}}})))
  (doseq [[l n m] [["L-1" "Alma" "A320"] ["L-2" "Bodo" "B747"]]]
    (is (ok? (api/mutate :mechaniker-anlegen {:lizenz l :name n :muster m} {:publish? true}))))

  (testing "ein Build verbindet jeden Mechaniker mit jedem Luftfahrzeug seines Musters"
    (bauen! {:force true})
    (is (= #{"D-AINA" "D-AINB"} (berechtigt "L-1")))
    (is (= #{} (berechtigt "L-2"))))

  (testing "Benutzer-Edits gewinnen gegen den nächsten Build — eine Verbindung bleibt, eine Trennung auch"
    (is (ok? (api/mutate :muster-berechtigen {:mechaniker "L-2" :luftfahrzeug "D-AINA"}
                         {:publish? true})))
    (is (ok? (api/mutate :muster-entziehen {:mechaniker "L-1" :luftfahrzeug "D-AINB"}
                         {:publish? true})))
    (bauen! {:force true})
    (is (= #{"D-AINA"} (berechtigt "L-1")))
    (is (= #{"D-AINA"} (berechtigt "L-2"))))

  (testing "was der Erzeuger nicht mehr liefert, verschwindet; die Benutzer-Edits bleiben"
    (is (ok? (api/mutate :umschulen {:mechaniker "L-1" :muster "B747"} {:publish? true})))
    (bauen! {:force true})
    (is (= #{} (berechtigt "L-1")))
    (is (= #{"D-AINA"} (berechtigt "L-2"))))

  (testing "nach einem neuen Backing nimmt der Build, was der alte Erzeuger lieferte; was Benutzer verbanden, bleibt"
    (is (ok? (api/mutate :umschulen {:mechaniker "L-1" :muster "A320"} {:publish? true})))
    (bauen! {:force true})
    (is (= #{"D-AINA"} (berechtigt "L-1")))
    (is (:ok? (install! {:links [[:musterberechtigung :mechaniker :luftfahrzeug :many-to-many
                                  {:join {:on [:name :muster] :from-pk :lizenz :to-pk :kennzeichen}
                                   :forward-api-name :muster-fuer
                                   :reverse-api-name :muster-mechaniker}]]})))
    (bauen! {:force true})
    (is (= #{} (berechtigt "L-1")))
    (is (= #{"D-AINA"} (berechtigt "L-2")))))

(deftest ein-fk-resolve-rechnet-den-schluessel-aus
  (is (ok? (api/mutate :create-dataset
                       {:name :lieferung :project :flotte
                        :columns [(spalte :posten :string) (spalte :kennzeichen :string)]}
                       {:stage? true})))
  (is (ok? (api/post! :staging)))
  (let [r (install!
           {:objects
              {:lieferposten {:primary-key :posten
                              :attributes [[:posten :string] [:kennzeichen :string]]
                              :plural "Lieferposten"}}
            :transforms
              {:lieferung-lesen {:language :pg-sql :query "SELECT posten, kennzeichen FROM l"
                                 :inputs {:l {:kind :dataset :dataset :lieferung}}
                                 :primary-key :posten
                                 :output {:kind :object-type :object-type :lieferposten}}}
            :links [[:posten-lfz :lieferposten :luftfahrzeug :many-to-one
                     {:fk-resolve {:key :posten :as :lfz-kennzeichen
                                   :query "SELECT p.posten, l.kennzeichen AS lfz_kennzeichen FROM lieferposten p JOIN luftfahrzeug l ON UPPER(p.kennzeichen) = l.kennzeichen"}
                      :forward-api-name :geliefert-fuer :reverse-api-name :lieferposten}]]})]
    (is (:ok? r) (:text r)))
  (schreiben! :lieferung :snapshot [{:posten "R-1" :kennzeichen "d-ainb"}])
  (bauen! {:force true})
  (testing "der Lieferant schreibt das Kennzeichen klein; der Build findet den Schlüssel, schreibt ihn als Eigenschaft und verlinkt darüber"
    (is (= "D-AINB" (get-in (nach :lieferposten :posten) ["R-1" :values :lfz-kennzeichen])))
    (let [r (api/post! :ontology-query
                       {:find :lieferposten :where {:posten {:= "R-1"}} :around [:geliefert-fuer]})]
      (is (= #{"D-AINB"} (set (map #(get-in % [:values :kennzeichen]) (:rows (:body r)))))
          (pr-str (:body r)))))
  (testing "der Link nimmt beim Löschen seine Erzeuger mit, den Ergänzer wie den Link-Erzeuger"
    (is (ok? (api/delete-conn-type :posten-lfz)))
    (is (nil? (transform :resolve-posten-lfz)))
    (is (nil? (transform :link-posten-lfz)))
    (is (ok? (api/delete! :transform-by {:name "lieferung-lesen"})))
    (is (ok? (api/post! :staging)))
    (is (ok? (api/delete-object-type :lieferposten true)))))

(def ^:private teile-zaehlen
  {:language :pg-sql
   :query "SELECT teilenummer, COUNT(*) AS anzahl FROM stueck_liste GROUP BY teilenummer"
   :inputs {:stueck_liste {:kind :dataset :dataset :stueckliste
                :checks [{:name "stückliste: seriennummer eindeutig"
                          :expectation [:primary-key :seriennummer]}]}}
   :primary-key :teilenummer
   :output {:kind :object-type :object-type :teil
            :checks [{:name "teil: höchstens zwei je nummer"
                      :expectation [:col :anzahl [:lte 2]] :on-error :fail}
                     {:name "teil: mindestens hundert"
                      :expectation [:count [:gt 100]] :on-error :warn}]}})

(defn- erwartung [n]
  (some #(when (= n (:name %)) %) (:checks (:body (api/get! :health-checks)))))

(deftest eine-erwartung-haelt-den-build-an
  (is (ok? (api/mutate :create-dataset
                       {:name :stueckliste :project :flotte
                        :columns [(spalte :seriennummer :string) (spalte :teilenummer :string)]}
                       {:stage? true})))
  (is (ok? (api/post! :staging)))
  (let [teil {:plural "Teile" :attributes [[:teilenummer :string] [:anzahl :long]]}]
    (testing "eine Erwartung, die nicht kompiliert, weist der Install ab"
      (let [r (install! {:objects {:teil teil}
                         :transforms {:teile-zaehlen
                                      (assoc-in teile-zaehlen [:output :checks 0 :expectation]
                                                [:col :anzahl [:frob 2]])}})]
        (is (not (:ok? r)))
        (is (re-find #"data expectations" (:text r)) (:text r))))
    (let [r (install! {:objects {:teil teil} :transforms {:teile-zaehlen teile-zaehlen}})]
      (is (:ok? r) (:text r))))
  (schreiben! :stueckliste :snapshot [{:seriennummer "S-1" :teilenummer "P-1"}
                                      {:seriennummer "S-2" :teilenummer "P-1"}])
  (bauen!)
  (testing "erfüllt: der Build läuft, das Ergebnis steht in Data Health; WARN meldet nur"
    (is (= 2 (get-in (nach :teil :teilenummer) ["P-1" :values :anzahl])))
    (is (= "passed" (get-in (erwartung "teil: höchstens zwei je nummer") [:latest :status])))
    (is (= "passed" (get-in (erwartung "stückliste: seriennummer eindeutig") [:latest :status])))
    (is (= "warning" (get-in (erwartung "teil: mindestens hundert") [:latest :status]))))
  (schreiben! :stueckliste :snapshot [{:seriennummer "S-1" :teilenummer "P-1"}
                                      {:seriennummer "S-2" :teilenummer "P-1"}
                                      {:seriennummer "S-3" :teilenummer "P-1"}])
  (testing "verletzt (FAIL): der Job bricht ab, das Objekt bleibt, wie es war"
    (is (not (ok? (api/post! :import-run))))
    (is (= 2 (get-in (nach :teil :teilenummer) ["P-1" :values :anzahl])))
    (is (= "failed" (get-in (erwartung "teil: höchstens zwei je nummer") [:latest :status]))))
  (schreiben! :stueckliste :snapshot [{:seriennummer "S-1" :teilenummer "P-1"}
                                      {:seriennummer "S-1" :teilenummer "P-2"}])
  (testing "eine verletzte Vorbedingung bricht ab, bevor der Transform läuft"
    (is (not (ok? (api/post! :import-run))))
    (is (= "failed" (get-in (erwartung "stückliste: seriennummer eindeutig") [:latest :status])))
    (is (= 2 (get-in (nach :teil :teilenummer) ["P-1" :values :anzahl]))))
  (schreiben! :stueckliste :snapshot [{:seriennummer "S-1" :teilenummer "P-1"}])
  (bauen!)
  (is (= 1 (get-in (nach :teil :teilenummer) ["P-1" :values :anzahl])))
  (let [teil  {:attributes [[:teilenummer :string] [:anzahl :long] [:teile-gruppe :string]]}
        liste [{:seriennummer "S-1" :teilenummer "P-1"} {:seriennummer "S-2" :teilenummer "P-1"}
               {:seriennummer "S-3" :teilenummer "P-2"}]
        tests [{:name "zwei Seriennummern, eine Teilenummer" :inputs {:stueck_liste liste}
                :expect [{:teilenummer "P-1" :anzahl 2} {:teilenummer "P-2" :anzahl 1}]}
               {:name "keine Stückliste, kein Teil" :expect []}]
        mit   (fn [t uts]
                {:objects {:teil teil}
                 :transforms {:teile-zaehlen (assoc t :tests uts)
                              :teile-zaehlen-clj
                              {:language :clojure
                               :code (str "(fn [{:keys [stueck_liste]}] (for [[n rs] (group-by"
                                          " :teilenummer stueck_liste)]"
                                          " {:teilenummer n"
                                          " :anzahl (count rs)}))")
                               :inputs {:stueck_liste {:kind :dataset :dataset :stueckliste}}
                               :output-columns [[:teilenummer :string] [:anzahl :long]]
                               :tests uts}}})
        lauf  (fn [n] (:body (api/test-transform n)))
        aus-objekt {:language :pg-sql
                    :query (str "SELECT o.teilenummer AS teilenummer, o.anzahl * 10 AS zehnfach,"
                                " b.seriennummer AS serien_nr, o.teile_gruppe AS gruppe"
                                " FROM teil_obj o JOIN bau_teil b"
                                " ON b.teilenummer = o.teilenummer AND b.stunden >= 1.5")
                    :inputs {:teil_obj {:kind :object-type :object-type :teil}
                             :bau_teil {:kind :object-type :object-type :bauteil}}
                    :output-columns [[:teilenummer :string] [:zehnfach :long]
                                     [:serien_nr :string] [:gruppe :string]]
                    :tests [{:name "ein Objekt-Eingang wird gelandet, Dezimal aus ganzer Zahl"
                             :inputs {:teil_obj [{:teilenummer "P-9" :anzahl 4
                                                  :teile_gruppe "Triebwerk"}]
                                      :bau_teil [{:seriennummer "S-9" :teilenummer "P-9"
                                                  :stunden 2}
                                                 {:seriennummer "S-8" :teilenummer "P-9"
                                                  :stunden 1}]}
                             :expect [{:teilenummer "P-9" :zehnfach 40 :serien_nr "S-9"
                                       :gruppe "Triebwerk"}]}]}]
    (testing "ein Unit-Test, der keinen Eingang des Transforms nennt, weist der Install ab"
      (let [r (install! (mit teile-zaehlen [{:name "x" :inputs {:gibt-es-nicht []} :expect []}]))]
        (is (not (:ok? r)))
        (is (re-find #"unit tests" (:text r)) (:text r))))
    (is (:ok? (install! (mit teile-zaehlen tests))))
    (is (:ok? (install! (assoc-in (mit teile-zaehlen tests)
                                  [:transforms :teile-aus-objekt] aus-objekt))))
    (testing "ein Objekttyp als Eingang: Testzeilen in den Spalten des Typs"
      (let [b (lauf :teile-aus-objekt)] (is (= 1 (:passed b)) (pr-str b))))
    (testing "SQL und Clojure: von Hand eingegebene Eingaben, erwartete Ausgabe — nichts aus dem Bestand"
      (doseq [n [:teile-zaehlen :teile-zaehlen-clj]]
        (let [b (lauf n)]
          (is (= 2 (:passed b)) (pr-str b))
          (is (= 0 (:failed b)) (pr-str b)))))
    (is (:ok? (install! (mit teile-zaehlen [{:name "falsch erwartet" :inputs {:stueck_liste liste}
                                              :expect [{:teilenummer "P-1" :anzahl 3}]}]))))
    (testing "eine Abweichung nennt, was fehlt und was zu viel ist"
      (let [{:keys [failed tests]} (lauf :teile-zaehlen)]
        (is (= 1 failed))
        (is (re-find #"missing.*:anzahl 3M?" (:detail (first tests))) (pr-str tests))
        (is (re-find #"not expected" (:detail (first tests))))))
    (is (:ok? (install! (mit teile-zaehlen tests))))))

(deftest zum-schluss-edits-ueberleben-build-und-umzug
  (is (:ok? (install! {:actions
                         {:teil-erfassen
                            {:params
                               {:seriennummer {:name "Seriennummer" :type :string :required true}
                                :teilenummer {:name "Teilenummer" :type :string :required true}}
                             :rules [{:op :create :object-type :bauteil
                                      :values {:seriennummer [:get :seriennummer]
                                               :teilenummer [:get :teilenummer]
                                               :kennzeichen "D-AINA"}}]
                             :edits [{:object-type :bauteil}]}
                          :teil-umbenennen
                            {:params {:teil {:name "Teil" :type :bauteil :required true}
                                      :teilenummer
                                        {:name "Teilenummer" :type :string :required true}}
                             :rules [{:op :update :object-type :bauteil
                                      :target [:get :teil]
                                      :values {:teilenummer [:get :teilenummer]}}]
                             :edits [{:object-type :bauteil}]}}})))
  (schreiben! :teilestamm :snapshot
              [{:seriennummer "U-1" :teilenummer "P-1" :kennzeichen "D-AINA" :stunden 1M}
               {:seriennummer "U-2" :teilenummer "P-2" :kennzeichen "D-AINA" :stunden 2M}])
  (bauen!)
  (is (ok? (api/mutate :teil-umbenennen {:teil "U-1" :teilenummer "P-1-korr"} {:publish? true})))
  (is (ok? (api/mutate :teil-erfassen {:seriennummer "U-9" :teilenummer "P-9"} {:publish? true})))

  (testing "ein Edit auf eine gelieferte Zeile überlebt den nächsten Build, auch wenn die Lieferung sich ändert"
    (schreiben! :teilestamm :snapshot
                [{:seriennummer "U-1" :teilenummer "P-1" :kennzeichen "D-AINA" :stunden 5M}
                 {:seriennummer "U-2" :teilenummer "P-2" :kennzeichen "D-AINA" :stunden 2M}])
    (bauen!)
    (let [u1 (get (nach :bauteil :seriennummer) "U-1")]
      (is (= "P-1-korr" (get-in u1 [:values :teilenummer])) "die Nutzer-Änderung gewinnt")
      (is (== 5 (get-in u1 [:values :stunden]))
          "die nicht bearbeitete Spalte folgt der Lieferung")))

  (testing "ein per Action angelegtes Objekt überlebt den Vollabgleich"
    (is (contains? (nach :bauteil :seriennummer) "U-9")))

  (testing "der Typ zieht auf ein anderes Dataset um; was nur der alte Erzeuger trug, geht"
    (is (ok? (api/mutate :create-dataset
                         {:name :teile_erfasst :project :betrieb
                          :columns [(spalte :seriennummer :string) (spalte :teilenummer :string)
                                    (spalte :kennzeichen :string) (spalte :stunden :decimal)]}
                         {:stage? true})))
    (is (ok? (api/post! :staging)))
    (is (ok? (api/delete! :transform-by {:name "einbau"})))
    (is (ok? (api/post! :staging)))
    (let [r (install! {:objects {:bauteil (-> (get-in welt [:objects :bauteil])
                                              (update :attributes conj [:einsatzort :string])
                                              (assoc :backing
                                                     {:dataset :teile_erfasst
                                                      :primary-key :seriennummer
                                                      :replaces :teilestamm-lesen}))}})]
      (is (:ok? r) (:text r)))
    (bauen!)
    (let [b (nach :bauteil :seriennummer)]
      (is (not-any? b ["U-1" "U-2"]) "gelieferte Zeilen gehen mit ihrem Erzeuger, auch bearbeitete")
      (is (contains? b "U-9") "per Action Angelegtes bleibt"))
    (testing "der alte Erzeuger steht noch, speist aber nichts; danach kann er gehen"
      (is (some? (transform :teilestamm-lesen)))
      (is (ok? (api/delete! :transform-by {:name "teilestamm-lesen"})))
      (is (ok? (api/post! :staging)))
      (bauen!)
      (is (contains? (nach :bauteil :seriennummer) "U-9")))
    (testing "und nimmt weiter neue Objekte an"
      (is (ok? (api/mutate :teil-erfassen {:seriennummer "U-10" :teilenummer "P-10"}
                           {:publish? true})))
      (bauen!)
      (is (contains? (nach :bauteil :seriennummer) "U-10")))))
