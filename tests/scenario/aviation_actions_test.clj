(ns ^:api scenario.aviation-actions-test
  (:require [sutra.catalog.grants :as grants]
            [clojure.string :as str]
            [support.in-project :refer [install-in-project]]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [sutra.access.principals :as pr]
            [support.aviation :refer [ontologie]]
            [sutra.compute.sandbox :as sandbox]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.foundation.wire :as wire]
            [sutra.types.paths :as paths]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(def ^:private freigabe
  {:objects
   {:freigabeschein
    {:name "Freigabeschein" :title :schein-nr :match-keys [:schein-nr]
     :primary-key :schein-nr
     :attributes [[:schein-nr :string] [:kennzeichen :string] [:muster :string]
                  [:ausgestellt-am :date] [:gueltig-bis :date] [:protokolliert-am :timestamp]
                  [:stunden :decimal] [:landungen :integer] [:pruefart :string]
                  [:aussteller :string] [:notiz :string]]
     :plural "Freigabescheine"}}
   :actions
   {:schein-ausstellen
    {:params {:schein-nr      {:type :string :prefill :uuid :exposure :hidden}
              :luftfahrzeug   {:type :luftfahrzeug :required true :name "Luftfahrzeug"}
              :ausgestellt-am {:type :date :required true :name "Ausgestellt am"}
              :gueltig-bis    {:type :date :name "Gültig bis"}
              :stunden        {:type :decimal :name "Stunden"}
              :pruefart       {:name "Pruefart" :type :string
                               :constraints {:one-of ["voll" "sicht" "verboten"]}}
              :landungen      {:type :integer :name "Landungen" :required true
                               :overrides [{:when [:= :pruefart "sicht"]
                                            :required false :default 0 :exposure :disabled}]}
              :muster         {:name "Muster" :default {:from :luftfahrzeug :property :muster}}
              :aussteller     {:type :string :prefill :current-user :exposure :hidden}
              :notiz          {:name "Notiz" :type :string}}
     :form {:sections [{:params [:luftfahrzeug :ausgestellt-am :gueltig-bis]}
                       {:params [:pruefart :landungen :stunden]}
                       {:params [:muster :notiz]}]}
     :submission-criteria [{:test {:p [:get :pruefart]} :pred [:not= [:get :p] "verboten"]
                            :message "Diese Prüfart ist gesperrt."}]
     :rules [{:op :create :object-type :freigabeschein
              :values {:schein-nr [:get :schein-nr]
                       :kennzeichen [:get :luftfahrzeug :kennzeichen]
                       :muster [:get :muster]
                       :ausgestellt-am [:get :ausgestellt-am]
                       :gueltig-bis [:get :gueltig-bis]
                       :protokolliert-am [:now]
                       :stunden [:get :stunden]
                       :landungen [:get :landungen]
                       :pruefart [:get :pruefart]
                       :aussteller [:get :aussteller]
                       :notiz [:get :notiz]}}]
     :edits [{:object-type :freigabeschein}]}}})

(def ^:private operatoren
  {:pruefe-includes     [:includes :l :w]
   :pruefe-includes-any [:includes-any :l :l2]
   :pruefe-each-gleich  [:each= :l :w]
   :pruefe-each-ungleich [:each!= :l :w]
   :pruefe-in           [:in :w "A320" "B748"]
   :pruefe-matches      [:matches :w "^D-"]
   :pruefe-zahlen       [:includes :zs :z]})

(def ^:private kriterien-schema
  {:actions
   (into {} (for [[a pred] operatoren]
              [a {:params {:liste  {:name "Liste" :type :string :multiple true}
                           :liste2 {:name "Liste2" :type :string :multiple true}
                           :wert   {:name "Wert" :type :string}
                           :zahlen {:name "Zahlen" :type :decimal :multiple true}
                           :zahl   {:name "Zahl" :type :decimal}}
                  :submission-criteria [{:test {:l [:get :liste] :l2 [:get :liste2] :w [:get :wert]
                                                :zs [:get :zahlen] :z [:get :zahl]}
                                         :pred pred
                                         :message (str "abgewiesen: " (name a))}]
                  :rules []}]))})

(defn- als-admin [f]
  (binding [tx/*auth-token* (-> (api/post! :login {:username grants/admin-username
                                                   :password pr/default-admin-password})
                                :body :token)]
    (f)))

(defn- mit-welt [f]
  (doseq [s [ontologie freigabe kriterien-schema]]
    (assert (ok? (install-in-project {:schema s})))
    (assert (ok? (api/post! :staging))))
  (doseq [[k m] [["D-AINA" "A320"] ["D-ABYC" "B748"]]]
    (assert (ok? (api/mutate :luftfahrzeug-registrieren {:kennzeichen k :muster m}
                             {:publish? true}))))
  (f))

(use-fixtures :once pod/with-mem-pod-auth als-admin mit-welt)

(defn- tun! [action args] (api/mutate action args {:publish? true}))

(defn- fehler [r] (str (:status r) " " (get-in r [:body :error])))

(defn- schein [nr]
  (some #(when (= nr (get-in % [:values :schein-nr])) (:values %))
        (:rows (:body (tx/send! :post (paths/url :ontology-query) {:find :freigabeschein})))))

(defn- ausstellen! [args]
  (let [nr (str "S-" (java.util.UUID/randomUUID))]
    [nr (tun! :schein-ausstellen (merge {:schein-nr nr :luftfahrzeug "D-AINA"
                                         :ausgestellt-am "2026-10-01" :landungen 3
                                         :pruefart "voll"}
                                        args))]))

(defn- pruefen [action args]
  (let [b (:body (api/post! :action-params nil {:action action :args args}))]
    (when-not (:error b)
      (is (nil? (wire/problem [:http :action-params] b))
          "the answer is what a browser of any generation reads (sutra.foundation.wire)"))
    b))

(deftest parameter
  (testing "§8.2 Text auf einem Dezimalparameter ist 400 und nennt Parameter und Typ"
    (let [[nr r] (ausstellen! {:stunden "viel"})]
      (is (= 400 (:status r)) (fehler r))
      (is (re-find #"(?i)stunden" (fehler r)))
      (is (re-find #"decimal" (fehler r)))
      (is (nil? (schein nr)) "die Ablehnung kommt vor jeder Regel")))

  (testing "§8.2 Text auf einem Integer-Parameter ebenso"
    (is (= 400 (:status (second (ausstellen! {:landungen "drei"}))))))

  (testing "§8.2 eine Zahl ohne Bruchteil ist ein Integer, eine mit Bruchteil nicht"
    (let [[nr r] (ausstellen! {:landungen 3.0})]
      (is (ok? r) (fehler r))
      (is (= 3 (:landungen (schein nr)))))
    (is (= 400 (:status (second (ausstellen! {:landungen 2.5}))))))

  (testing "§2.1 Zahlen sind exakt, auch weit hinter dem Komma"
    (let [[nr r] (ausstellen! {:stunden 12345678901234567890.123456789M})]
      (is (ok? r) (fehler r))
      (is (== 12345678901234567890.123456789M (bigdec (:stunden (schein nr)))))))

  (testing "ein leeres optionales Datum ist ⊥"
    (let [[nr r] (ausstellen! {:gueltig-bis ""})]
      (is (ok? r) (fehler r))
      (is (nil? (:gueltig-bis (schein nr))))))

  (testing "ein leeres Pflichtfeld fehlt (422) und die Ablehnung nennt seinen Anzeigenamen"
    (doseq [leer ["" "   "]]
      (let [r (second (ausstellen! {:ausgestellt-am leer}))]
        (is (= 422 (:status r)) (fehler r))
        (is (re-find #"Ausgestellt am" (fehler r))))))

  (testing "0 und false sind Werte"
    (let [[nr r] (ausstellen! {:landungen 0})]
      (is (ok? r) (fehler r))
      (is (= 0 (:landungen (schein nr))))))

  (testing "ein Text-Parameter nimmt Text, der nur wie eine Zahl aussieht"
    (let [[nr r] (ausstellen! {:notiz "0815"})]
      (is (ok? r))
      (is (= "0815" (:notiz (schein nr))))))

  (testing "§8.3 Quellen: :current-user bindet immer, :uuid ist Text, ein Default liest die Eigenschaft eines anderen Parameters"
    (let [[_nr r] (ausstellen! {:schein-nr nil :aussteller "jemand-anderes"})
          s (some #(when (= "D-AINA" (get-in % [:values :kennzeichen])) (:values %))
                  (:rows (:body (tx/send! :post (paths/url :ontology-query)
                                          {:find :freigabeschein :order-by :protokolliert-am
                                           :sort-dir :desc :limit 1}))))]
      (is (ok? r) (fehler r))
      (is (= grants/admin-username (:aussteller s)))
      (is (string? (:schein-nr s)))
      (is (re-matches #"[0-9a-f-]{36}" (:schein-nr s)))
      (is (= "A320" (:muster s)) "muster kommt aus luftfahrzeug.muster")
      (is (some? (:protokolliert-am s))
          "[:now] in einem Regelwert ist ein versteckter :now-Parameter")))

  (testing "§8.3 ein ausdrücklich übergebener Wert schlägt den Default"
    (let [[nr r] (ausstellen! {:muster "A321"})]
      (is (ok? r))
      (is (= "A321" (:muster (schein nr))))))

  (testing "§8.3 Overrides: die Bedingung hebt die Pflicht auf und setzt den Default"
    (let [b (pruefen :schein-ausstellen
                     {:luftfahrzeug "D-AINA" :ausgestellt-am "2026-10-01" :pruefart "voll"})]
      (is (true? (get-in b [:params :landungen :declared-required])))
      (is (true? (:blocked? b)))
      (is (re-find #"Landungen" (str (:reason b)))))
    (let [b (pruefen :schein-ausstellen
                     {:luftfahrzeug "D-AINA" :ausgestellt-am "2026-10-01" :pruefart "sicht"})]
      (is (not (get-in b [:params :landungen :declared-required])))
      (is (= "disabled" (name (get-in b [:params :landungen :exposure]))))
      (is (= 0 (get-in b [:values :landungen])))
      (is (false? (:blocked? b))))
    (let [[nr r] (ausstellen! {:pruefart "sicht" :landungen nil})]
      (is (ok? r) (fehler r))
      (is (= 0 (:landungen (schein nr))))))

  (testing "§10.4 validate: ein Kriterium sperrt den Knopf mit seiner eigenen Nachricht und schreibt nichts"
    (let [vorher
            (count (:rows (:body (tx/send! :post (paths/url :ontology-query)
                                           {:find :freigabeschein}))))
          b (pruefen :schein-ausstellen {:luftfahrzeug "D-AINA" :ausgestellt-am "2026-10-01"
                                         :landungen 1 :pruefart "verboten"})]
      (is (true? (:blocked? b)))
      (is (= "Diese Prüfart ist gesperrt." (:reason b)))
      (is (= vorher
             (count (:rows (:body (tx/send! :post (paths/url :ontology-query)
                                            {:find :freigabeschein}))))))))

  (testing "die Kriteriennachricht ist die Ablehnung, auch für den Admin über /api/actions"
    (let [r (second (ausstellen! {:pruefart "verboten"}))]
      (is (= 422 (:status r)))
      (is (= "Diese Prüfart ist gesperrt." (get-in r [:body :error])))))

  (testing "§8.1 ein Parameter ohne Typ, der keine Eigenschaft schreibt, installiert nicht"
    (let [r (install-in-project {:schema {:actions {:typlos {:params {:x {:name "X"}} :rules []}}}})
          c (when (ok? r) (api/post! :staging))]
      (is (not (and (ok? r) (ok? c))))
      (api/delete! :staging))))

(defn- gleich? [a b]
  (cond (and (nil? a) (nil? b)) true
        (or (nil? a) (nil? b)) false
        (and (number? a) (number? b)) (== a b)
        :else (= a b)))

(defn- modell
  [action {:keys [liste liste2 wert zahlen zahl]}]
  (let [l (or liste []) l2 (or liste2 []) zs (or zahlen [])]
    (case action
      :pruefe-includes      (boolean (some #(gleich? % wert) l))
      :pruefe-includes-any  (boolean (some (fn [y] (some #(gleich? % y) l)) l2))
      :pruefe-each-gleich   (every? #(gleich? % wert) l)
      :pruefe-each-ungleich (every? #(not (gleich? % wert)) l)
      :pruefe-in            (boolean (some #(gleich? wert %) ["A320" "B748"]))
      :pruefe-matches       (boolean (and wert (re-find #"^D-" wert)))
      :pruefe-zahlen        (boolean (some #(gleich? % zahl) zs)))))

(def ^:private aufruf
  (let [txt  (gen/elements ["A320" "a320" "B748" "D-AINA" "d-aina" "X"])
        zahl (gen/elements [1 1.0M 2 2.5M 3])]
    (gen/tuple (gen/elements (keys operatoren))
               (gen/let [l  (gen/one-of [(gen/return nil) (gen/vector txt 0 3)])
                         l2 (gen/one-of [(gen/return nil) (gen/vector txt 0 2)])
                         w  (gen/one-of [(gen/return nil) txt])
                         zs (gen/one-of [(gen/return nil) (gen/vector zahl 0 3)])
                         z  (gen/one-of [(gen/return nil) zahl])]
                 (cond-> {}
                   l (assoc :liste l) l2 (assoc :liste2 l2) w (assoc :wert w)
                   zs (assoc :zahlen zs) z (assoc :zahl z))))))

(deftest kriterien-operatoren-folgen-der-definition
  (let [abweichung (atom nil)
        gesehen (atom #{})
        r (tc/quick-check 300
                          (prop/for-all [[action args] aufruf]
                                        (let [b (pruefen action args)
                                              soll (modell action args)
                                              ist  (and (not (:blocked? b)) (nil? (:error b)))
                                  _    (swap! gesehen conj [action ist])]
                                          (or (= soll ist)
                                              (do (reset! abweichung
                                                          {:action action :args args :soll soll
                                                           :plattform b})
                                                  false))))
                          :seed 20261009)]
    (is (:pass? r) (pr-str (or (get-in r [:shrunk :smallest]) @abweichung)))
    (is (empty? (for [a (keys operatoren) ist [true false] :when (not (@gesehen [a ist]))] [a ist]))
        "jeder Operator wurde angenommen und abgelehnt gesehen")))

(def ^:private regel-schema
  {:actions
   {:mangel-umhaengen
    {:params {:mangel {:name "Mangel" :type :mangel :required true}
              :ziel {:name "Ziel" :type :luftfahrzeug :required true}}
     :rules  [{:op :update :object-type :mangel :target [:get :mangel]
               :values {:kennzeichen [:get :ziel :kennzeichen]}}]
     :edits  [{:object-type :mangel}]}
    :mangel-loeschen
    {:params {:mangel {:name "Mangel" :type :mangel :required true}}
     :rules  [{:op :delete-object :object-type :mangel :target [:get :mangel]}]
     :edits  [{:object-type :mangel}]}
    :triebwerk-erfassen
    {:params {:triebwerk {:name "Triebwerk" :type :triebwerk}
              :seriennummer {:name "Seriennummer" :type :string}
              :stunden {:name "Stunden" :type :decimal}
              :luftfahrzeug {:name "Luftfahrzeug" :type :luftfahrzeug :required true}}
     :rules  [{:op :create-or-update :object-type :triebwerk :target [:get :triebwerk]
               :values {:seriennummer [:get :seriennummer] :betriebsstunden [:get :stunden]
                        :kennzeichen [:get :luftfahrzeug :kennzeichen]}}]
     :edits  [{:object-type :triebwerk}]}
    :stunden-nachtragen
    {:params {:teil {:name "Teil" :type :wartbares-teil :required true}
              :stunden {:name "Stunden" :type :decimal :required true}}
     :rules  [{:op :update-of-interface :interface :wartbares-teil :target [:get :teil]
               :values {:betriebsstunden [:get :stunden]}}]
     :edits  [{:interface :wartbares-teil}]}
    :egt-ueber-interface
    {:params {:teil {:name "Teil" :type :wartbares-teil :required true}
              :marge {:name "Marge" :type :decimal :required true}}
     :rules  [{:op :update-of-interface :interface :wartbares-teil :target [:get :teil]
               :values {:egt-marge [:get :marge]}}]
     :edits  [{:interface :wartbares-teil}]}
    :teil-ausbauen
    {:params {:teil {:name "Teil" :type :wartbares-teil :required true}}
     :rules  [{:op :delete-of-interface :interface :wartbares-teil :target [:get :teil]}]
     :edits  [{:interface :wartbares-teil}]}
    :teil-anlegen
    {:params {:seriennummer {:name "Seriennummer" :type :string}}
     :rules  [{:op :create-of-interface :interface :wartbares-teil :object-type :fahrwerk
               :values {:seriennummer [:get :seriennummer]}}]
     :edits  [{:interface :wartbares-teil}]}
}})

(def ^:private korrigieren
  {:actions {:beschreibung-korrigieren
             {:params {:mangel       {:name "Mangel" :type :mangel :required true}
                       :beschreibung {:name "Beschreibung" :type :string}}
              :rules  [{:op :update :object-type :mangel :target [:get :mangel]
                        :set [:beschreibung]}]
              :edits  [{:object-type :mangel}]}}})

(def ^:private ziel-per-text
  {:actions {:ziel-per-text
             {:params {:wert {:name "Wert" :type :string :required true}}
              :rules  [{:op :update :object-type :mangel :target [:get :wert]
                        :values {:zustand "geprueft"}}]
              :edits  [{:object-type :mangel}]}}})

(defn- lesen [typ]
  (:rows (:body (tx/send! :post (paths/url :ontology-query) {:find typ}))))

(defn- wert [typ k v attr]
  (some #(when (= v (get-in % [:values k])) (get-in % [:values attr])) (lesen typ)))

(defn- kanten [link]
  (set (map (juxt #(or (:from-id %) (:from_id %)) #(or (:to-id %) (:to_id %)))
            (:body (api/list-links {:type link})))))

(defn- id-von [typ k v] (some #(when (= v (get-in % [:values k])) (:id %)) (lesen typ)))

(deftest regeln
  (is (ok? (install-in-project {:schema regel-schema})))
  (is (ok? (api/post! :staging)))
  (is (ok? (tun! :mangel-melden
                 {:mangel-nr "R-1" :luftfahrzeug "D-AINA" :beschreibung "Leck" :kategorie "B"})))

  (testing "§2.3 ein FK-Link ist eine Funktion des Schlüsselattributs: die Kante steht sofort und folgt ihm"
    (let [m (id-von :mangel :mangel-nr "R-1")]
      (is (= #{[m (id-von :luftfahrzeug :kennzeichen "D-AINA")]}
             (set (filter #(= m (first %)) (kanten :mangel-luftfahrzeug)))))
      (is (ok? (tun! :mangel-umhaengen {:mangel "R-1" :ziel "D-ABYC"})))
      (is (= #{[m (id-von :luftfahrzeug :kennzeichen "D-ABYC")]}
             (set (filter #(= m (first %)) (kanten :mangel-luftfahrzeug)))))))

  (testing "§7.3 delete: mit dem Objekt gehen seine Kanten"
    (is (ok? (tun! :mangel-melden
                   {:mangel-nr "R-2" :luftfahrzeug "D-AINA" :beschreibung "x" :kategorie "C"})))
    (let [m (id-von :mangel :mangel-nr "R-2")]
      (is (ok? (tun! :mangel-loeschen {:mangel "R-2"})))
      (is (nil? (id-von :mangel :mangel-nr "R-2")))
      (is (not-any? #(= m (first %)) (kanten :mangel-luftfahrzeug)))))

  (testing "create-or-update: ein leeres Ziel legt an, ein aufgelöstes ändert"
    (is (ok? (tun! :triebwerk-erfassen
                   {:seriennummer "ESN-U" :stunden 10M :luftfahrzeug "D-AINA"})))
    (is (== 10 (wert :triebwerk :seriennummer "ESN-U" :betriebsstunden)))
    (is (ok? (tun! :triebwerk-erfassen {:triebwerk "ESN-U" :seriennummer "ESN-U" :stunden 20M
                                         :luftfahrzeug "D-AINA"})))
    (is (== 20 (wert :triebwerk :seriennummer "ESN-U" :betriebsstunden)))
    (is (= 1 (count (filter #(= "ESN-U" (get-in % [:values :seriennummer])) (lesen :triebwerk))))))

  (testing "Interface-Writes gehen auf den konkreten Typ"
    (is (ok? (tun! :fahrwerk-einbauen {:seriennummer "LG-R" :luftfahrzeug "D-AINA"})))
    (is (ok? (tun! :stunden-nachtragen {:teil "ESN-U" :stunden 30M})))
    (is (ok? (tun! :stunden-nachtragen {:teil "LG-R" :stunden 5M})))
    (is (== 30 (wert :triebwerk :seriennummer "ESN-U" :betriebsstunden)))
    (is (== 5 (wert :fahrwerk :seriennummer "LG-R" :betriebsstunden))))

  (testing "… eine Eigenschaft, die nicht zum Interface gehört, wird abgelehnt und benannt"
    (let [r (tun! :egt-ueber-interface {:teil "ESN-U" :marge 3M})]
      (is (#{400 422} (:status r)) (fehler r))
      (is (re-find #"egt-marge" (fehler r)))))

  (testing "… anlegen über das Interface legt den konkreten Typ an"
    (is (ok? (tun! :teil-anlegen {:seriennummer "LG-I"})))
    (is (some? (id-von :fahrwerk :seriennummer "LG-I"))))

  (testing "… und löschen über das Interface löscht das konkrete Objekt"
    (is (ok? (tun! :teil-ausbauen {:teil "LG-I"})))
    (is (nil? (id-von :fahrwerk :seriennummer "LG-I"))))

  (testing ":set leitet den Parameter samt Typ aus der Eigenschaft ab — auch im Teil-Install gegen einen Live-Typ"
    (is (ok? (install-in-project {:schema korrigieren})))
    (is (ok? (api/post! :staging)))
    (is (ok? (tun! :beschreibung-korrigieren {:mangel "R-1" :beschreibung "Leck rechts"})))
    (is (= "Leck rechts" (wert :mangel :mangel-nr "R-1" :beschreibung))))

  (testing "ein Regelziel ist ein Objektparameter, nie ein Textwert — das entscheidet der Install"
    (let [r (install-in-project {:schema ziel-per-text})]
      (is (= 400 (:status r)))
      (is (re-find #"object-reference parameter" (fehler r)))))

  (testing "§7.3 ein Objektparameter, der auf nichts zeigt, ist 422 und schreibt nichts"
    (let [r (tun! :mangel-umhaengen {:mangel "R-gibt-es-nicht" :ziel "D-ABYC"})]
      (is (= 422 (:status r)) (fehler r))
      (is (re-find #"R-gibt-es-nicht" (fehler r)))))

  (testing "§8.7 die Antwort nennt, was die Action angefasst hat"
    (let [e (get-in (api/mutate :mangel-melden
                                {:mangel-nr "R-3" :luftfahrzeug "D-AINA" :beschreibung "x"
                                 :kategorie "C"}
                                {:publish? true :return-edits? true})
                    [:body :edits])]
      (is (= 1 (:added-objects-count e)))
      (is (= 0 (:modified-objects-count e)))
      (is (= :add-object (:type (first (:edits e)))))
      (is (= (id-von :mangel :mangel-nr "R-3") (:object-id (first (:edits e))))))
    (let [e (get-in (api/mutate :mangel-umhaengen {:mangel "R-3" :ziel "D-ABYC"}
                                {:publish? true :return-edits? true})
                    [:body :edits])]
      (is (= 1 (:modified-objects-count e))))
    (let [e (get-in (api/mutate :mangel-loeschen {:mangel "R-3"}
                                {:publish? true :return-edits? true}) [:body :edits])]
      (is (= 1 (:deleted-objects-count e)))))

  (testing "§8.7 ein Batch ist ein Aufruf: scheitert ein Eintrag, landet keiner"
    (let [r (api/mutate-batch :mangel-melden
                              [{:mangel-nr "B-1" :luftfahrzeug "D-AINA" :beschreibung "x"
                                :kategorie "C"}
                               {:mangel-nr "B-2" :luftfahrzeug "D-AINA" :beschreibung "x"
                                :kategorie "Z"}]
                              {:publish? true})]
      (is (not (ok? r)))
      (is (nil? (id-von :mangel :mangel-nr "B-1"))))
    (let [r (api/mutate-batch :mangel-melden
                              (for [i (range 3)]
                                {:mangel-nr (str "B-" i) :luftfahrzeug "D-AINA" :beschreibung "x"
                                 :kategorie "C"})
                              {:publish? true :return-edits? true})]
      (is (ok? r) (fehler r))
      (is (= 3 (get-in r [:body :edits :added-objects-count]))))))

(def ^:private arc
  {:objects {:arc {:name "Lufttüchtigkeitsprüfzeugnis" :title :arc-nr :match-keys [:arc-nr]
                   :requires-action-log true
                   :primary-key :arc-nr
                   :attributes [[:arc-nr :string] [:kennzeichen :string] [:gueltig-bis :date]]
                   :plural "Lufttüchtigkeitsprüfzeugnisse"}}
   :actions {:arc-ausstellen
             {:action-log true
              :params {:arc-nr {:name "Arc nr" :type :string :required true}
                       :luftfahrzeug {:name "Luftfahrzeug" :type :luftfahrzeug :required true}
                       :gueltig-bis {:name "Gueltig bis" :type :date :required true}}
              :rules  [{:op :create :object-type :arc
                        :values {:arc-nr [:get :arc-nr]
                                 :kennzeichen [:get :luftfahrzeug :kennzeichen]
                                 :gueltig-bis [:get :gueltig-bis]}}]
              :edits  [{:object-type :arc}]}
             :arcs-verlaengern
             {:action-log true
              :params {:arcs {:name "Arcs" :type :arc :multiple true :required true}
                       :bis {:name "Bis" :type :date :required true}}
              :rules  [{:op :update :object-type :arc :target [:get :arcs]
                        :values {:gueltig-bis [:get :bis]}}]
              :edits  [{:object-type :arc}]}
             :arc-und-mangel
             {:action-log true
              :params {:arc {:name "Arc" :type :arc :required true}
                       :nr {:name "Nr" :type :string :required true}}
              :rules  [{:op :update :object-type :arc :target [:get :arc]
                        :values {:gueltig-bis "2026-01-01"}}
                       {:op :create :object-type :mangel
                        :values {:mangel-nr [:get :nr] :kennzeichen [:get :arc :kennzeichen]
                                 :beschreibung "ARC abgelaufen" :kategorie "A" :zustand "offen"}}]
              :edits  [{:object-type :arc} {:object-type :mangel}]}}})

(def ^:private arc-ohne-protokoll
  {:actions {:arc-heimlich
             {:params {:arc {:name "Arc" :type :arc :required true}}
              :rules  [{:op :update :object-type :arc :target [:get :arc]
                        :values {:gueltig-bis "2030-01-01"}}]
              :edits  [{:object-type :arc}]}}})

(deftest protokoll
  (is (ok? (install-in-project {:schema arc})) )
  (is (ok? (api/post! :staging)))

  (testing "ein Typ, der ein Protokoll verlangt, lässt keine Action ohne eines installieren"
    (let [r (install-in-project {:schema arc-ohne-protokoll})
          c (when (ok? r) (api/post! :staging))]
      (is (not (and (ok? r) (ok? c))))
      (is (re-find #"requires-action-log" (pr-str (:body r) (:body c))))
      (api/delete! :staging)))

  (testing "jede Ausführung schreibt ein Protokollobjekt: wer, wann, welches Objekt — verlinkt"
    (doseq [n ["ARC-1" "ARC-2" "ARC-3"]]
      (is (ok? (tun! :arc-ausstellen
                     {:arc-nr n :luftfahrzeug "D-AINA" :gueltig-bis "2027-01-01"}))))
    (let [arcs (set (keep #(when (#{"ARC-1" "ARC-2" "ARC-3"} (get-in % [:values :arc-nr])) (:id %))
                          (lesen :arc)))
          logs (filter #(#{"ARC-1" "ARC-2" "ARC-3"} (get-in % [:values :edited-objects 0]))
                       (lesen :arc-ausstellen-log))]
      (is (= 3 (count logs)))
      (is (every? #(= grants/admin-username (get-in % [:values :user-id])) logs))
      (is (every? #(some? (get-in % [:values :at])) logs))
      (is (= arcs (set (filter arcs (map second (kanten :arc-ausstellen-log-edited))))))))

  (testing "ein Massenaufruf ist eine Ausführung: ein Protokoll, verlinkt mit jedem bearbeiteten Objekt"
    (is (ok? (tun! :arcs-verlaengern {:arcs ["ARC-1" "ARC-2" "ARC-3"] :bis "2028-01-01"})))
    (let [[log & mehr] (lesen :arcs-verlaengern-log)
          arcs (set (keep #(when (#{"ARC-1" "ARC-2" "ARC-3"} (get-in % [:values :arc-nr])) (:id %))
                          (lesen :arc)))]
      (is (nil? mehr))
      (is (= #{"ARC-1" "ARC-2" "ARC-3"} (set (get-in log [:values :edited-objects]))))
      (is (= arcs (set (map second (kanten :arcs-verlaengern-log-edited)))))))

  (testing "über mehrere Typen hinweg: je Typ eine Liste und ein Link"
    (is (ok? (tun! :arc-und-mangel {:arc "ARC-1" :nr "P-1"})))
    (let [log (first (lesen :arc-und-mangel-log))]
      (is (= ["ARC-1"] (get-in log [:values :edited-objects-arc])) (pr-str log))
      (is (= ["P-1"] (get-in log [:values :edited-objects-mangel])))
      (is (= 1 (count (kanten :arc-und-mangel-log-edited-arc))))
      (is (= 1 (count (kanten :arc-und-mangel-log-edited-mangel)))))))

(def ^:private funktions-schema
  {:functions
   {:maengel-uebertragen
    {:language :clojure :target-type :mangel
     :source "(defn ^{:sutra/function {:edits [:mangel]}} maengel-uebertragen
                [^Luftfahrzeug von ^Luftfahrzeug nach]
                (when (= (:kennzeichen von) (:kennzeichen nach))
                  (sutra/user-facing-error \"Quelle und Ziel sind dasselbe Luftfahrzeug.\"))
                (doseq [m (ontology/links von :maengel {:where [:= :zustand \"offen\"]})]
                  (edits/modify m {:kennzeichen (:kennzeichen nach)})))"}
    :team-zuweisen
    {:language :clojure :target-type :mechaniker
     :source "(defn ^{:sutra/function {:edits [:mechaniker]}} team-zuweisen
                [^\"list[Mechaniker]\" team ^Luftfahrzeug lfz]
                (doseq [m (ontology/links lfz :berechtigte)]
                  (edits/remove-link m :berechtigt-fuer lfz))
                (doseq [m team]
                  (edits/add-link m :berechtigt-fuer lfz)))"}
    :heimlich
    {:language :clojure :target-type :mangel
     :source "(defn ^{:sutra/function {:edits [:mangel]}} heimlich [^Luftfahrzeug lfz]
                (edits/modify lfz {:status \"in-betrieb\"}))"}
}
   :actions
   {:maengel-uebertragen
    {:action-log true
     :params {:von {:name "Von" :type :luftfahrzeug :required true}
              :nach {:name "Nach" :type :luftfahrzeug :required true}}
     :rules  [{:op :apply-edit-function :function :maengel-uebertragen}]
     :edits  [{:object-type :mangel}]}
    :team-zuweisen
    {:params {:team {:name "Team" :type :mechaniker :multiple true :required true}
              :lfz {:name "Lfz" :type :luftfahrzeug :required true}}
     :rules  [{:op :apply-edit-function :function :team-zuweisen}]
     :edits  [{:link-type :berechtigung}]}
    :heimlich
    {:params {:lfz {:name "Lfz" :type :luftfahrzeug :required true}}
     :rules  [{:op :apply-edit-function :function :heimlich}]
     :edits  [{:object-type :mangel}]}
}})

(def ^:private ausbruch
  {:functions {:ausbruch {:language :clojure :target-type :mangel
                          :source "(defn ^{:sutra/function {:edits [:mangel]}} ausbruch
                                     [^Luftfahrzeug lfz]
                                     (slurp \"/etc/passwd\"))"}}})

(defn- install-abgelehnt? [schema]
  (let [r (install-in-project {:schema schema})
        c (when (ok? r) (api/post! :staging))]
    (api/delete! :staging)
    (not (and (ok? r) (ok? c)))))

(deftest funktionen
  (is (ok? (install-in-project {:schema funktions-schema})))
  (is (ok? (api/post! :staging)))
  (doseq [n ["F-1" "F-2" "F-3"]]
    (is (ok? (tun! :mangel-melden
                   {:mangel-nr n :luftfahrzeug "D-AINA" :beschreibung "x" :kategorie "C"}))))
  (is (ok? (tun! :maengel-zurueckstellen {:maengel ["F-3"]})))

  (testing "§10.4 plan: ein Dry-Run zeigt die Edits der Funktion und schreibt keinen"
    (let [r (api/mutate :maengel-uebertragen {:von "D-AINA" :nach "D-ABYC"} {:dry-run? true})]
      (is (ok? r) (fehler r))
      (is (re-find #":update|:modify" (pr-str (:body r))))
      (is (= "D-AINA" (wert :mangel :mangel-nr "F-1" :kennzeichen)))))

  (testing "§7.6 die Funktion rechnet ihre Edits: ein gefilterter Link-Read, dann Änderungen"
    (is (ok? (tun! :maengel-uebertragen {:von "D-AINA" :nach "D-ABYC"})))
    (is (= "D-ABYC" (wert :mangel :mangel-nr "F-1" :kennzeichen)))
    (is (= "D-ABYC" (wert :mangel :mangel-nr "F-2" :kennzeichen)))
    (is (= "D-AINA" (wert :mangel :mangel-nr "F-3" :kennzeichen)) "nur offene Mängel"))

  (testing "das Protokoll neben der Funktionsregel nennt, was die Funktion bearbeitet hat"
    (let [log (first (lesen :maengel-uebertragen-log))]
      (is (= #{"F-1" "F-2"}
             (set (filter #(str/starts-with? % "F-") (get-in log [:values :edited-objects]))))
          (pr-str log))))

  (testing "ein Nutzerfehler der Funktion erreicht den Nutzer wörtlich, und nichts wird geschrieben"
    (let [r (tun! :maengel-uebertragen {:von "D-ABYC" :nach "D-ABYC"})]
      (is (#{400 422} (:status r)) (fehler r))
      (is (= "Quelle und Ziel sind dasselbe Luftfahrzeug." (get-in r [:body :error])))))

  (testing "n:m-Links setzen und lösen; eine Liste kommt als Liste an"
    (is (ok? (tun! :mechaniker-anlegen {:lizenz "T-1" :name "Tjark" :muster "A320"})))
    (is (ok? (tun! :mechaniker-anlegen {:lizenz "T-2" :name "Tilda" :muster "A320"})))
    (is (ok? (tun! :team-zuweisen {:team ["T-1" "T-2"] :lfz "D-AINA"})))
    (is (= 2
           (count (filter #(= (id-von :luftfahrzeug :kennzeichen "D-AINA") (second %))
                          (kanten :berechtigung)))))
    (is (ok? (tun! :team-zuweisen {:team ["T-2"] :lfz "D-AINA"})))
    (is (= #{(id-von :mechaniker :lizenz "T-2")}
           (set (map first
                     (filter #(= (id-von :luftfahrzeug :kennzeichen "D-AINA") (second %))
                             (kanten :berechtigung)))))))

  (testing "T2 ein Edit außerhalb der deklarierten Typen lehnt die Action ab und nennt den Typ"
    (let [r (tun! :heimlich {:lfz "D-AINA"})]
      (is (not (ok? r)))
      (is (re-find #"luftfahrzeug" (fehler r)))
      (is (= "in-betrieb" (wert :luftfahrzeug :kennzeichen "D-AINA" :status)))))

  (testing "A2 die Sandbox hält, und das entscheidet schon der Install: kein Zugriff auf das Dateisystem"
    (let [r (install-in-project {:schema ausbruch})]
      (is (#{400 422} (:status r)) (fehler r))
      (is (re-find #"slurp" (pr-str (:body r))))))

  (testing "Install-Tore: eine Funktionsregel steht allein, braucht :edits und eine bekannte Funktion"
    (is (install-abgelehnt?
         {:actions {:gemischt {:params {:lfz {:name "Lfz" :type :luftfahrzeug}}
                               :rules [{:op :apply-edit-function :function :heimlich}
                                       {:op :update :object-type :luftfahrzeug :target [:get :lfz]
                                        :values {}}]
                               :edits [{:object-type :mangel} {:object-type :luftfahrzeug}]}}}))
    (is (install-abgelehnt?
         {:actions {:ohne-edits {:params {:lfz {:name "Lfz" :type :luftfahrzeug}}
                                 :rules [{:op :apply-edit-function :function :heimlich}]}}}))
    (is (install-abgelehnt?
         {:actions {:unbekannt {:params {:lfz {:name "Lfz" :type :luftfahrzeug}}
                                :rules [{:op :apply-edit-function :function :gibt-es-nicht}]
                                :edits [{:object-type :mangel}]}}}))))

(def ^:private py-kopf
  (str "from functions.api import function, OntologyEdit, UserFacingError, String\n"
       "from ontology_sdk import PlatformClient\n"
       "from ontology_sdk.ontology.objects import Mangel, Luftfahrzeug, Mechaniker, Triebwerk\n\n"))

(defn- py-funktion [quelle] {:language :python :target-type :mangel :source (str py-kopf quelle)})

(def ^:private py-schema
  {:functions
   {:py-zurueckstellen
    (py-funktion (str "@function(edits=[Mangel])\n"
                      "def py_zurueckstellen(lfz: Luftfahrzeug) -> list[OntologyEdit]:\n"
                      "    edits = PlatformClient().ontology.edits()\n"
                      "    for m in lfz.maengel:\n"
                      "        if m.zustand == 'offen':\n"
                      "            edits.objects.Mangel.edit(m).zustand = 'zurueckgestellt'\n"
                      "    return edits.get_edits()\n"))
    :py-markieren
    (py-funktion
     (str "@function(edits=[Mangel])\n"
          "def py_markieren(nrs: list[String]) -> list[OntologyEdit]:\n"
          "    edits = PlatformClient().ontology.edits()\n"
          "    for m in "
          "PlatformClient().ontology.objects.Mangel.where(Mangel.object_type.mangel_nr.is_in(nrs)).iterate():\n"
          "        edits.objects.Mangel.edit(m).beschreibung = 'markiert'\n"
          "    return edits.get_edits()\n"))
    :py-anlegen
    (py-funktion
     (str "@function(edits=[Mangel])\n"
          "def py_anlegen(lfz: Luftfahrzeug, nr: String) -> list[OntologyEdit]:\n"
          "    edits = PlatformClient().ontology.edits()\n"
          "    edits.objects.Mangel.create(nr, kennzeichen=lfz.kennzeichen, beschreibung='aus "
          "python', kategorie='C', zustand='offen')\n"
          "    return edits.get_edits()\n"))
    :py-umberechtigen
    (py-funktion
     (str "@function(edits=[Mechaniker])\n"
          "def py_umberechtigen(mech: Mechaniker, von: Luftfahrzeug, nach: Luftfahrzeug) -> "
          "list[OntologyEdit]:\n"
          "    edits = PlatformClient().ontology.edits()\n"
          "    edits.objects.Mechaniker.edit(mech).berechtigt_fuer.remove(von)\n"
          "    edits.objects.Mechaniker.edit(mech).berechtigt_fuer.add(nach)\n"
          "    return edits.get_edits()\n"))
    :py-sperre
    (py-funktion (str "@function(edits=[Mangel])\n"
                      "def py_sperre(lfz: Luftfahrzeug) -> list[OntologyEdit]:\n"
                      "    raise UserFacingError(lfz.kennzeichen + ' ist gesperrt')\n"))
    :py-heimlich
    (py-funktion (str "@function(edits=[Mangel])\n"
                      "def py_heimlich(lfz: Luftfahrzeug) -> list[OntologyEdit]:\n"
                      "    edits = PlatformClient().ontology.edits()\n"
                      "    edits.objects.Luftfahrzeug.edit(lfz).status = 'in-betrieb'\n"
                      "    return edits.get_edits()\n"))
    :py-direkt
    (py-funktion (str "@function(edits=[Mangel])\n"
                      "def py_direkt(lfz: Luftfahrzeug) -> list[OntologyEdit]:\n"
                      "    lfz.status = 'x'\n"
                      "    return []\n"))}
   :actions
   (into {} (for [[a ps edits] [[:py-zurueckstellen {:lfz :luftfahrzeug} [{:object-type :mangel}]]
                                [:py-markieren {:nrs [:string]} [{:object-type :mangel}]]
                                [:py-anlegen {:lfz :luftfahrzeug :nr :string}
                                 [{:object-type :mangel}]]
                                [:py-umberechtigen
                                 {:mech :mechaniker :von :luftfahrzeug :nach :luftfahrzeug}
                                 [{:link-type :berechtigung}]]
                                [:py-sperre {:lfz :luftfahrzeug} [{:object-type :mangel}]]
                                [:py-heimlich {:lfz :luftfahrzeug} [{:object-type :mangel}]]
                                [:py-direkt {:lfz :luftfahrzeug} [{:object-type :mangel}]]]]
              [a {:params (into {} (for [[p t] ps]
                                     [p
                                      (if (vector? t)
                                        {:name (name p) :type (first t) :multiple true
                                         :required true}
                                        {:name (name p) :type t :required true})]))
                  :rules  [{:op :apply-edit-function :function a}]
                  :edits  edits}]))})

(deftest python-funktionen
  (if-not (sandbox/available?)
    (println "  (python-funktionen ÜBERSPRUNGEN — keine Sandbox)")
    (do
      (is (ok? (install-in-project {:schema py-schema})))
      (is (ok? (api/post! :staging)))
      (doseq [n ["Y-1" "Y-2"]]
        (is (ok? (tun! :mangel-melden
                       {:mangel-nr n :luftfahrzeug "D-ABYC" :beschreibung "x" :kategorie "C"}))))

      (testing "ein Link-Read und eine Bedingung, dann Änderungen"
        (is (ok? (tun! :py-zurueckstellen {:lfz "D-ABYC"})))
        (is (= "zurueckgestellt" (wert :mangel :mangel-nr "Y-1" :zustand))))

      (testing "eine Suche mit is_in über einen Listenparameter wählt genau diese Objekte"
        (is (ok? (tun! :py-markieren {:nrs ["Y-1"]})))
        (is (= "markiert" (wert :mangel :mangel-nr "Y-1" :beschreibung)))
        (is (= "x" (wert :mangel :mangel-nr "Y-2" :beschreibung))))

      (testing "anlegen mit Schlüssel und Werten"
        (is (ok? (tun! :py-anlegen {:lfz "D-AINA" :nr "Y-9"})))
        (is (= "aus python" (wert :mangel :mangel-nr "Y-9" :beschreibung)))
        (is (= "D-AINA" (wert :mangel :mangel-nr "Y-9" :kennzeichen))))

      (testing "n:m-Links entfernen und hinzufügen"
        (is (ok? (tun! :mechaniker-anlegen {:lizenz "P-1" :name "Pius" :muster "A320"})))
        (is (ok? (tun! :berechtigen {:mechaniker "P-1" :luftfahrzeug "D-AINA"})))
        (is (ok? (tun! :py-umberechtigen {:mech "P-1" :von "D-AINA" :nach "D-ABYC"})))
        (is (= #{(id-von :luftfahrzeug :kennzeichen "D-ABYC")}
               (set (map second
                         (filter #(= (id-von :mechaniker :lizenz "P-1") (first %))
                                 (kanten :berechtigung)))))))

      (testing "UserFacingError erreicht den Nutzer wörtlich"
        (let [r (tun! :py-sperre {:lfz "D-AINA"})]
          (is (#{400 422} (:status r)) (fehler r))
          (is (= "D-AINA ist gesperrt" (get-in r [:body :error])))))

      (testing "ein Edit außerhalb der deklarierten Typen lehnt ab und nennt den Typ"
        (let [r (tun! :py-heimlich {:lfz "D-AINA"})]
          (is (#{400 422} (:status r)) (fehler r))
          (is (re-find #"luftfahrzeug" (fehler r)))))

      (testing "gelesene Objekte sind schreibgeschützt"
        (let [r (tun! :py-direkt {:lfz "D-AINA"})]
          (is (#{400 422} (:status r)) (fehler r))
          (is (re-find #"read-only" (fehler r))))))))

(defn- abgelehnt-mit [schema muster]
  (let [r (install-in-project {:schema schema})
        c (when (ok? r) (api/post! :staging))
        text (pr-str (:body r) (:body c))]
    (api/delete! :staging)
    (and (not (and (ok? r) (ok? c))) (boolean (re-find muster text)))))

(deftest statische-tore
  (testing "ein abgeleiteter Ausdruck mit einem unbekannten Operator"
    (is (abgelehnt-mit
         {:objects {:pruefstand {:attributes [[:status :string]
                                               [:frei? :boolean
                                                {:derived
                                                   {:expression
                                                      [:and [:nope [:get :status] "x"]]}}]]
                                 :plural "Prüfstände"}}}
         #"nope")))
  (testing "ein Kriterium mit einem unbekannten Operator, und eines, dessen Test nicht das Bindungsvokabular spricht"
    (is (abgelehnt-mit {:actions {:k1 {:params {:m {:name "M" :type :mangel}}
                                       :submission-criteria
                                         [{:test {:a [:get :m :kategorie]} :pred [:isnt :a "A"]}]
                                       :rules []}}}
                       #"isnt"))
    (is (abgelehnt-mit {:actions {:k2 {:params {:m {:name "M" :type :mangel}}
                                       :submission-criteria
                                         [{:test {:a [:got :m :kategorie]} :pred [:!= :a "A"]}]
                                       :rules []}}}
                       #"got")))
  (testing "ein Regelwert rechnet nicht — die Ablehnung verweist auf :compute"
    (is (abgelehnt-mit
         {:actions {:k3 {:params {:t {:name "T" :type :triebwerk}}
                         :rules [{:op :update :object-type :triebwerk
                                  :target [:get :t]
                                  :values
                                    {:betriebsstunden [:+ [:get :t :betriebsstunden] 1]}}]
                         :edits [{:object-type :triebwerk}]}}}
         #":compute")))
  (testing "eine Action schreibt keine abgeleitete Eigenschaft"
    (is (abgelehnt-mit {:actions {:k4 {:params {:l {:name "L" :type :luftfahrzeug}}
                                       :rules [{:op :update :object-type :luftfahrzeug
                                                :target [:get :l]
                                                :values {:offene-maengel 0}}]
                                       :edits [{:object-type :luftfahrzeug}]}}}
                       #"offene-maengel")))
  (testing "connected-one über eine Seite, die viele erreichen kann"
    (is (abgelehnt-mit (update-in ontologie [:objects :luftfahrzeug :attributes] conj
                                  [:ein-mangel :string
                                   {:derived {:expression [:connected-one :maengel :mangel-nr]}}])
                       #"MANY")))
  (testing "ein Lebenszyklus mit einem Übergang in einen unbekannten Zustand wird benannt"
    (let [r (install-in-project {:schema
                                   (assoc-in ontologie
                                             [:objects :luftfahrzeug :lifecycle :transitions 0 :to]
                                             :kaputt)
                                 :preview? true})]
      (is (re-find #"unknown states" (pr-str (:body r)))))))

(def ^:private erp
  {:erp {:type :postgres :jdbc-url "jdbc:postgresql://erp.invalid/mro" :access :read-write
         :project :werft
         :egress ["erp.invalid:5432"]
         :writeback {"MANGEL_ERP" {:object-type :mangel :pk "MANGEL_NR" :identity "MANGEL_NR"
                                   :writable-columns ["BESCHREIBUNG"]
                                   :insertable-columns ["BESCHREIBUNG"]
                                   :log :none}}
         :webhooks {:melden {:kind :table :table "MANGEL_ERP" :op :insert}}}})

(defn- mit-erp [actions] {:projects {:werft {}} :sources erp :actions actions})

(deftest webhook-tore
  (testing "eine Action erreicht die Außenwelt nur über einen Webhook — die Writeback-Verben gibt es nicht mehr"
    (is (abgelehnt-mit (mit-erp {:w1 {:rules [{:op :writeback-create :object-type :mangel}]}})
                       #"writeback-verb-removed")))
  (testing "ein Webhook nennt seine Position und steht als eigene Regel"
    (is (abgelehnt-mit (mit-erp {:w2
                                   {:rules [{:op :invoke-webhook :source :erp :webhook :melden}]}})
                       #"webhook-phase-required"))
    (is (abgelehnt-mit (mit-erp {:w3 {:rules [{:op :if :pred true
                                               :then {:op :invoke-webhook :phase :side-effect
                                                      :source :erp :webhook :melden}}]}})
                       #"webhook-phase-required")))
  (testing "höchstens ein Writeback, und dort steht nur ein Webhook"
    (is (abgelehnt-mit (mit-erp {:w4
                                   {:rules [{:op :invoke-webhook :phase :writeback :source :erp
                                             :webhook :melden}
                                            {:op :invoke-webhook :phase :writeback :source :erp
                                             :webhook :melden}]}})
                       #"multiple-writeback-rules"))
    (is (abgelehnt-mit (mit-erp {:w5 {:params {:m {:name "M" :type :mangel :required true}}
                                      :rules [{:op :update :phase :writeback :object-type :mangel
                                               :target [:get :m]
                                               :values {:zustand "x"}}]
                                      :edits [{:object-type :mangel}]}})
                       #"writeback-phase-invalid")))
  (testing "eine Action ändert Objekte und Links, nicht das Schema"
    (is (abgelehnt-mit (mit-erp {:w6 {:rules [{:op :create-object-type :name :x :plural "Xs"}]}})
                       #"schema-edit-in-action")))
  (testing "ein Tabellen-Webhook schreibt nur eine freigegebene Tabelle, und nur mit insert, update oder delete"
    (is (abgelehnt-mit {:projects {:werft {}}
                        :sources (assoc-in erp [:erp :webhooks :melden :table] "POSITION")}
                       #"webhook-table-invalid"))
    (is (abgelehnt-mit {:projects {:werft {}}
                        :sources (assoc-in erp [:erp :webhooks :melden :op] :upsert)}
                       #"webhook-table-invalid")))
  (testing "§9.4 ein Seiteneffekt ändert nichts — auch nicht über eine Action, die er aufruft"
    (is (abgelehnt-mit {:actions {:w7 {:params {:m {:name "M" :type :mangel :required true}}
                                       :rules [{:op :update :phase :side-effect :object-type :mangel
                                                :target [:get :m]
                                                :values {:zustand "x"}}]
                                       :edits [{:object-type :mangel}]}}}
                       #"side-effect-edit"))
    (is (abgelehnt-mit {:actions {:stempeln {:params {:m {:name "M" :type :mangel :required true}}
                                             :rules [{:op :update :object-type :mangel
                                                      :target [:get :m]
                                                      :values {:zustand "x"}}]
                                             :edits [{:object-type :mangel}]}
                                  :w8 {:params {:m {:name "M" :type :mangel :required true}}
                                       :rules [{:op :notify :phase :side-effect
                                                :channel "werkstatt"}
                                               {:op :action :phase :side-effect
                                                :chain [{:op :call :fn :stempeln}]}]}}}
                       #"side-effect-edit")))
  (testing "ein Writeback, Änderungen und Seiteneffekte zusammen sind die Form, die installiert"
    (let [r (install-in-project
             {:schema (mit-erp {:w9 {:params {:nr {:name "Nr" :type :string :required true}}
                                     :rules [{:op :invoke-webhook :phase :writeback :source :erp
                                              :webhook :melden
                                              :params {:nr [:get :nr]} :as :wb}
                                             {:op :create :object-type :mangel
                                              :values {:mangel-nr [:get :wb :identity]
                                                       :zustand "offen"}}
                                             {:op :invoke-webhook :phase :side-effect :source :erp
                                              :webhook :melden
                                              :params {:nr [:get :nr]}}]
                                     :edits [{:object-type :mangel}]}})
              :preview? true})]
      (is (ok? r) (pr-str (:body r))))))

(deftest kriterien-ergaenzungen
  (is (ok? (install-in-project {:schema {:actions
                                         {:sammel-freigabe
                                          {:params
                                             {:maengel
                                                {:name "Maengel" :type :mangel :multiple true
                                                 :required true}}
                                           :submission-criteria
                                             [{:test {:n [:count :maengel]} :pred [:<= :n 2]
                                               :message "Höchstens zwei Mängel je Sammelfreigabe."}]
                                           :rules []}
                                          :falsche-liste
                                          {:params {:m {:name "M" :type :mangel :required true}}
                                           :submission-criteria
                                             [{:test {:l [:get :m]} :pred [:includes :l "x"]}]
                                           :rules []}}}})))
  (is (ok? (api/post! :staging)))
  (doseq [n ["K-1" "K-2" "K-3"]]
    (is (ok? (tun! :mangel-melden
                   {:mangel-nr n :luftfahrzeug "D-AINA" :beschreibung "x" :kategorie "C"}))))
  (testing "[:count p] im Test zählt eine Liste"
    (is (false? (:blocked? (pruefen :sammel-freigabe {:maengel ["K-1" "K-2"]}))))
    (is (= "Höchstens zwei Mängel je Sammelfreigabe."
           (:reason (pruefen :sammel-freigabe {:maengel ["K-1" "K-2" "K-3"]})))))
  (testing "ein Einzelwert, wo eine Liste hingehört, wird abgelehnt und nennt :pluck"
    (let [r (tun! :falsche-liste {:m "K-1"})]
      (is (#{400 422} (:status r)) (fehler r))
      (is (re-find #":pluck" (fehler r))))))

(deftest draht
  (let [[nr r] (ausstellen! {:ausgestellt-am "2026-10-01"})]
    (is (ok? r) (fehler r))
    (testing "ein Datum ist ein Tag, ein Zeitpunkt ist ISO-8601 in UTC"
      (let [s (schein nr)]
        (is (= "2026-10-01" (str (:ausgestellt-am s))))
        (is (re-matches #"\d{4}-\d\d-\d\dT\d\d:\d\d:\d\d(\.\d+)?Z" (str (:protokolliert-am s))))))))

(def ^:private ergaenzungen
  {:object-sets {:offene-maengel-auswahl {:root :mangel :where [:= :zustand "offen"]}}
   :functions {:py-stunden-kopieren
               (py-funktion
                (str "@function(edits=[Triebwerk])\n"
                     "def py_stunden_kopieren(t: Triebwerk) -> list[OntologyEdit]:\n"
                     "    edits = PlatformClient().ontology.edits()\n"
                     "    edits.objects.Triebwerk.edit(t).egt_marge = t.betriebsstunden\n"
                     "    return edits.get_edits()\n"))}
   :actions
   {:mangel-waehlen {:params {:mangel {:name "Mangel" :type :mangel
                                       :options-from
                                         {:object-set :offene-maengel-auswahl :label :mangel-nr}}
                              :irgendein {:name "Irgendein" :type :mangel}}
                     :rules []}
    :stunden-uebernehmen {:params {:t {:name "T" :type :triebwerk :required true}
                                   :h {:name "H" :type :decimal
                                       :default {:from :t :property :betriebsstunden}}}
                          :rules [{:op :update :object-type :triebwerk :target [:get :t]
                                   :values {:egt-marge [:get :h]}}]
                          :edits [{:object-type :triebwerk}]}
    :arc-pruefen {:action-log {:object-type :pruefprotokoll}
                  :params {:arc {:name "Arc" :type :arc :required true}}
                  :rules [{:op :update :object-type :arc :target [:get :arc]
                           :values {:gueltig-bis "2029-01-01"}}]
                  :edits [{:object-type :arc}]}
    :ohne-typ {:params {:t {:name "T" :type :triebwerk}}
               :rules [{:op :create-or-update :target [:get :t] :values {:seriennummer "X"}}]}
    :py-stunden-kopieren {:params {:t {:name "T" :type :triebwerk :required true}}
                          :rules [{:op :apply-edit-function :function :py-stunden-kopieren}]
                          :edits [{:object-type :triebwerk}]}}})

(deftest ergaenzungen-actions
  (is (ok? (install-in-project {:schema arc})))
  (is (ok? (api/post! :staging)))
  (let [r (install-in-project {:schema (cond-> ergaenzungen
                                         (not
                                          (sandbox/available?))
                                         (-> (update :functions dissoc :py-stunden-kopieren)
                                             (update :actions dissoc :py-stunden-kopieren)))})]
    (is (ok? r) (fehler r)))
  (is (ok? (api/post! :staging)))
  (doseq [n ["E-1" "E-2"]]
    (is (ok? (tun! :mangel-melden
                   {:mangel-nr n :luftfahrzeug "D-AINA" :beschreibung "x" :kategorie "C"}))))
  (is (ok? (tun! :maengel-zurueckstellen {:maengel ["E-2"]})))

  (testing ":options-from beantwortet die Auswahl aus der Objektmenge, ohne Angabe aus dem ganzen Typ"
    (let [b (pruefen :mangel-waehlen {})]
      (is (contains? (set (map #(get-in % [:values :mangel-nr])
                               (get-in b [:options :mangel :rows]))) "E-1"))
      (is (not (contains? (set (map #(get-in % [:values :mangel-nr])
                                    (get-in b [:options :mangel :rows]))) "E-2")))
      (is (contains? (set (map #(get-in % [:values :mangel-nr])
                               (get-in b [:options :irgendein :rows]))) "E-2"))))

  (testing "§8.3 ein Default aus einer ⊥-Eigenschaft lässt den Parameter leer"
    (is (ok? (tun! :triebwerk-einbauen {:seriennummer "ESN-D0" :luftfahrzeug "D-AINA"})))
    (is (ok? (tun! :triebwerk-einbauen
                   {:seriennummer "ESN-D1" :betriebsstunden 42.5M :luftfahrzeug "D-AINA"})))
    (is (ok? (tun! :stunden-uebernehmen {:t "ESN-D0"})))
    (is (ok? (tun! :stunden-uebernehmen {:t "ESN-D1"})))
    (is (nil? (wert :triebwerk :seriennummer "ESN-D0" :egt-marge)))
    (is (== 42.5 (wert :triebwerk :seriennummer "ESN-D1" :egt-marge))))

  (testing "ein eigener Protokolltyp wird gewürdigt"
    (is (ok? (tun! :arc-ausstellen
                   {:arc-nr "ARC-E" :luftfahrzeug "D-AINA" :gueltig-bis "2027-01-01"})))
    (is (ok? (tun! :arc-pruefen {:arc "ARC-E"})))
    (is (= 1 (count (lesen :pruefprotokoll)))))

  (testing "create-or-update ohne Objekttyp und ohne Ziel kann nichts anlegen — Ablehnung, die das sagt"
    (let [r (tun! :ohne-typ {})]
      (is (#{400 422} (:status r)) (fehler r))
      (is (re-find #"object-type|object type" (fehler r)))))

  (testing "§2.1 eine Dezimalzahl bleibt exakt, auch durch eine Python-Funktion"
    (when (sandbox/available?)
      (is (ok? (tun! :triebwerk-einbauen
                     {:seriennummer "ESN-P" :betriebsstunden 12345678901234.123456789M
                      :luftfahrzeug "D-AINA"})))
      (is (ok? (tun! :py-stunden-kopieren {:t "ESN-P"})))
      (is (== 12345678901234.123456789M
              (bigdec (wert :triebwerk :seriennummer "ESN-P" :egt-marge)))))))
