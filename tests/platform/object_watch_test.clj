(ns ^:unit platform.object-watch-test
  (:require [sutra.catalog.lookup :as lookup]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.store.access.watch :as watch]
            [support.system :as tsys]
            [sutra.builds.run]
            [sutra.serve.object-set :as object-set]
            [sutra.serve.stream]
            [sutra.capps.variables :as vars]))

(use-fixtures :each (fn [f] (watch/drop-subscribers!) (f) (watch/drop-subscribers!)))

(def ^:private rid "ri.sutra.main.object-type.97d6ca44-ab66-46ff-9cc3-a1aa265f3e59")
(def ^:private schema-mit-angebot
  {:object-types {rid {:object-type-name :angebot :plural "Angebote"}}})

(deftest ein-abo-hoert-nur-seinen-typ-auf-seinem-zweig
  (let [gesehen (atom [])
        _       (watch/subscribe! "main" :angebot #(swap! gesehen conj %))]
    (testing "der eigene Typ kommt durch, mit seinem Namen im Nutzlast"
      (watch/notify! "main" :angebot {:run-id "r1"})
      (is (= [{:run-id "r1" :object-type "angebot"}] @gesehen)))
    (testing "ein fremder Typ nicht"
      (watch/notify! "main" :rechnung {:run-id "r2"})
      (is (= 1 (count @gesehen))))
    (testing "und derselbe Typ auf einem anderen Zweig auch nicht — ein Abo ist
              an den Zweig gebunden, den der Leser sieht"
      (watch/notify! "szenario-a" :angebot {:run-id "r3"})
      (is (= 1 (count @gesehen))))))

(deftest ein-toter-sink-fliegt-raus-statt-die-runde-zu-kippen
  (let [gesehen (atom 0)]
    (watch/subscribe! "main" :angebot (fn [_] (throw (ex-info "Kanal zu" {}))))
    (watch/subscribe! "main" :angebot (fn [_] (swap! gesehen inc)))
    (watch/notify! "main" :angebot {:run-id "r1"})
    (is (= 1 @gesehen) "der gesunde Empfaenger bekommt seine Meldung")
    (is (= 1 (watch/subscriber-count "main" :angebot))
        "und der tote ist abgemeldet, statt bei jeder Runde erneut zu werfen")))

(deftest abmelden-nimmt-genau-eines-weg
  (let [ab1 (watch/subscribe! "main" :angebot (fn [_]))
        _   (watch/subscribe! "main" :angebot (fn [_]))]
    (is (= 2 (watch/subscriber-count "main" :angebot)))
    (ab1)
    (is (= 1 (watch/subscriber-count "main" :angebot)))))

(def ^:private beruehrte #'sutra.builds.run/touched-object-types)

(deftest ein-lauf-nennt-die-typen-die-er-angefasst-hat-als-rids
  (do
    (testing "ein BUILD nennt sie ueber :touched-types — SEINE Zeilen traegt sein
              Ergebnis gar nicht, und wer nur dort nachsieht, meldet nie etwas"
      (is (= [rid] (vec (beruehrte schema-mit-angebot {:touched-types [:angebot]})))))
    (testing "eine AKTION ueber ihren tx-vector, bei ihrer RID — und nur die
              Objekt-Schreibungen, ein Link ist kein Objekttyp"
      (is (= [rid]
             (vec (beruehrte schema-mit-angebot {:result {:tx-vector [{:op :update-object :type rid
                                                    :payload {:id "1"}}
                                                   {:op :create-link :type :anrechnung
                                                    :payload {:id "2"}}]}})))))
    (testing "auch durch die APP-Tuer, die ihren tx-vector :action-result nennt —
              der Weg, den ein Nutzer tatsaechlich nimmt"
      (is (= [rid]
             (vec (beruehrte schema-mit-angebot
                             {:action-result {:tx-vector [{:op :update-object :type rid
                                        :payload {:id "1"}}]}})))))
    (testing "beide Wege im selben Lauf ergeben EINEN Eintrag, nicht zwei"
      (is (= [rid]
             (vec (beruehrte schema-mit-angebot {:touched-types [:angebot]
                              :result {:tx-vector [{:op :update-object :type rid
                                                    :payload {:id "1"}}]}})))))
    (testing "was das Schema nicht kennt, faellt raus statt unter einem Namen zu
              landen, den niemand abfragt"
      (is (= [] (vec (beruehrte schema-mit-angebot {:touched-types [:gibt-es-nicht]})))))))

(deftest das-wecken-haengt-nicht-am-melden
  (watch/forget-changes!)
  (let [geweckt (atom 0)]
    (tsys/provide! :watch/wake #(swap! geweckt inc))
    (testing "das Melden weckt NICHT — der Lauf haelt sich zu diesem Zeitpunkt
              noch selbst als aktiv, und ein Planer, der das sieht, entscheidet
              'nichts zu tun' und schlaeft wieder ein"
      (watch/notify! "main" :angebot {:run-id "r1"})
      (watch/notify! "main" :rechnung {:run-id "r1"})
      (is (= 0 @geweckt)))
    (testing "geweckt wird ausdruecklich, und einmal"
      (watch/wake!)
      (is (= 1 @geweckt)))
    (testing "ein Wecker, der wirft, kippt nichts — der Lauf hat laengst
              committet, und ein Planer ist kein Grund, ihn nachtraeglich
              scheitern zu lassen"
      (tsys/provide! :watch/wake #(throw (ex-info "Planer aus" {})))
      (is (nil? (watch/wake!))))
    (tsys/provide! :watch/wake nil)
    (watch/forget-changes!)))

(def ^:private neu-abonnieren #'sutra.serve.stream/resubscribe!)

(defn- bekannte-typen
  [& typen]
  (let [bekannt (set typen)]
    (fn [_schema _kind k]
      (let [kw (keyword (if (keyword? k) (name k) (str k)))]
        (when (bekannt kw) kw)))))

(defn- basis
  [t]
  {"objectSet" {"type" "base" "objectType" t}})

(deftest ein-abo-satz-wird-ERSETZT-nicht-ergaenzt
  (with-redefs [lookup/->type-rid (bekannte-typen :angebot :rechnung)
                sutra.serve.stream/ws-send! (fn [_ _] nil)]
    (let [subs (atom {})
          ids  #(into {} (map (fn [[k s]] [(get-in k ["objectSet" "objectType"]) (:id s)])) @subs)]
      (testing "der erste Satz macht auf, was er nennt"
        (let [antwort (neu-abonnieren nil nil nil "main" subs
                                      [(basis "angebot") (basis "rechnung")])]
          (is (= ["success" "success"] (mapv :type antwort)))
          (is (= 1 (watch/subscriber-count "main" :angebot)))
          (is (= 1 (watch/subscriber-count "main" :rechnung)))))
      (let [vorher (ids)]
        (testing "der zweite schliesst, was er nicht mehr nennt — und laesst
                  offen, was in beiden steht: eine App, die einen Typ dazu
                  bekommt, reisst die anderen nicht ab"
          (neu-abonnieren nil nil nil "main" subs [(basis "rechnung")])
          (is (= 0 (watch/subscriber-count "main" :angebot)))
          (is (= 1 (watch/subscriber-count "main" :rechnung)))
          (is (= (get vorher "rechnung") (get (ids) "rechnung"))
              "dieselbe Subscription, nicht eine zweite unter neuer ID")))
      (testing "ein unbekannter Typ ist eine Absage mit Namen, kein stilles Nichts"
        (let [antwort (neu-abonnieren nil nil nil "main" subs
                                      [(basis "rechnung") (basis "gibtsnicht")])]
          (is (= ["success" "error"] (mapv :type antwort)))
          (is (= "INVALID_OBJECT_TYPE" (-> antwort second :errors first :error)))))
      (testing "und die leere Liste meldet alles ab"
        (is (= [] (neu-abonnieren nil nil nil "main" subs [])))
        (is (= 0 (watch/subscriber-count "main" :rechnung)))
        (is (empty? @subs))))))

(def ^:private abo-fuer #'sutra.serve.stream/subscription-for)

(deftest ein-abo-nimmt-den-namen-UND-die-rid
  (testing "der Deskriptor fuehrt seine Typen doppelt verschluesselt, also muss
            beides gehen — sonst faellt genau der Typ aus, den eine App unter
            seiner RID gebunden hat"
    (is (some? (abo-fuer nil schema-mit-angebot nil "main" {:object-type "angebot"})))
    (is (some? (abo-fuer nil schema-mit-angebot nil "main" {:object-type rid}))))
  (testing "und was kein Typ ist, bleibt abgewiesen"
    (is (nil? (abo-fuer nil schema-mit-angebot nil "main" {:object-type "gibt-es-nicht"})))
    (is (nil? (abo-fuer nil schema-mit-angebot nil "main" {:object-type 42}))))
  (watch/drop-subscribers!))

(deftest ein-objektset-kommt-in-counterparts-schreibweise
  (let [menge (fn [where] {"objectSet" {"type" "filter" "where" where
                                        "objectSet" {"type" "base" "objectType" "anruf"}}})]
    (testing "ein Filter wird zur Abfrage des Stores"
      (is (= {:object-type "anruf" :where {:nummer {:= "1"}}}
             (object-set/parse (menge {"type" "eq" "field" "nummer" "value" "1"}))))
      (is (= {:or [{:a {:= 1}} {:a {:= 2}}]}
             (:where (object-set/parse (menge {"type" "in" "field" "a" "value" [1 2]})))))
      (is (= {:and [{:a {:not= 1}} {:b {:nil? true}}]}
             (:where (object-set/parse
                      (menge {"type" "and"
                              "value" [{"type" "not"
                                        "value" {"type" "eq" "field" "a" "value" 1}}
                                       {"type" "isNull" "field" "b" "value" true}]}))))))
    (testing "die gewuenschten Eigenschaften reisen mit"
      (is (= [:a] (:properties (object-set/parse (assoc (basis "anruf") "propertySet" ["a"]))))))
    (testing "was der Store nicht beantwortet, ist eine Absage, keine stille Weite"
      (let [absage #(try (object-set/parse %) nil
                         (catch clojure.lang.ExceptionInfo e (object-set/error-of e)))]
        (is (= "INVALID_FILTER" (absage (menge {"type" "wildcard" "field" "a"}))))
        (is (= "INVALID_FILTER" (absage (menge {"type" "in" "field" "a" "value" []}))))
        (is (= "OBJECT_SET_NOT_SUPPORTED"
               (absage {"objectSet" {"type" "searchAround" "link" "x"}})))))))

(deftest eine-aenderung-trifft-nur-wen-sie-angeht
  (let [menge {:where {:nummer {:= "1"}}}
        zeile (fn [id] {:id id :values {:nummer "1"}})
        zustand #(mapv (juxt :state (comp :__primaryKey :object)) (:updates %))]
    (testing "ein Treffer ist eine Aenderung und zaehlt fortan zur Menge"
      (let [a (object-set/updates menge "anruf" #{} ["a"] [(zeile "a")])]
        (is (= [["ADDED_OR_UPDATED" "a"]] (zustand a)))
        (is (= #{"a"} (:members a)))))
    (testing "ein fremdes Objekt sagt niemandem etwas — der Grund fuer gefilterte Abos"
      (let [a (object-set/updates menge "anruf" #{"a"} ["b"] [])]
        (is (empty? (:updates a)))
        (is (= #{"a"} (:members a)))))
    (testing "wer den Filter verlaesst oder geloescht wird, ist REMOVED"
      (let [a (object-set/updates menge "anruf" #{"a"} ["a"] [])]
        (is (= [["REMOVED" "a"]] (zustand a)))
        (is (= #{} (:members a)))))
    (testing "ohne gehaltene Mitglieder ist jeder Abgang ein REMOVED: lieber zu oft als stale"
      (is (= [["REMOVED" "b"]] (zustand (object-set/updates menge "anruf" nil ["b"] [])))))
    (testing "aus einer Basismenge faellt nur, was es nicht mehr gibt"
      (is (= [["REMOVED" "a"]] (zustand (object-set/updates {} "anruf" nil ["a"] [])))))
    (testing "propertySet begrenzt die Werte, nie die Identitaet"
      (is (= {:__apiName "anruf" :__primaryKey "a"}
             (-> (object-set/updates {:properties [:x]} "anruf" nil ["a"] [(zeile "a")])
                 :updates first :object))))))

(deftest eine-seite-beobachtet-die-menge-die-sie-liest
  (let [basis {:type "base" :objectType "anruf"}
        menge (fn [where target] (vars/watched-set :anruf {:where where} (constantly target)))]
    (testing "ohne Bedingung der ganze Typ"
      (is (= basis (vars/watched-set :anruf {} identity))))
    (testing "eine Bedingung wird zum Filter"
      (is (= {:type "eq" :field "nummer" :value "1"}
             (:where (menge [[:nummer := {:var :x}]] "1")))))
    (testing "die Nummern eines Partners: :in ueber eine Liste"
      (is (= {:type "in" :field "nummer" :value ["1" "2"]}
             (:where (menge [[:nummer :in {:val :n}]] ["1" "2" "1"])))))
    (testing "was nichts eingrenzt oder der Store nicht beantwortet, beobachtet den ganzen Typ:
              zu oft aufwachen, nie etwas verpassen"
      (is (= basis (menge [[:nummer :in {:val :n}]] [])))
      (is (= basis (menge [[:* :contains {:var :x}]] "a")))
      (is (= basis (menge [[:a :truthy nil]] nil))))))
