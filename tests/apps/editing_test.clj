(ns ^:unit apps.editing-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.capps.catalog :as cat]
            [sutra.capps.editing :as ed]))

(def ^:private fixtures
  (for [id [:leer :liste :dashboard :reiter]]
    [id (ed/draft {:name :probe :status :draft :layout (cat/template id)})]))

(defn- every-cursor
  [editor]
  (map (comp vec :path) (cat/flatten-tree (:tree editor))))

(deftest katalog-ist-vollstaendig-beschrieben
  (doseq [{:keys [id label group needs do] :as in} ed/intents]
    (testing (str id)
      (is (keyword? id))
      (is (string? label) "jede Absicht hat eine Beschriftung — sie wird der Undo-Name")
      (is (contains? ed/group-labels group) "jede Absicht hängt in einer benannten Gruppe")
      (is (contains? ed/needs needs) "jede Absicht nennt, welche Auswahl sie braucht")
      (is (ifn? do))
      (is (or (nil? (:tag in)) (ifn? (:tag in)))))))

(deftest jeder-tastenweg-gehoert-genau-einer-absicht
  (let [ks (mapcat :keys ed/intents)]
    (is (= (count ks) (count (set ks))) (str "doppelt belegt: " (pr-str ks)))
    (is (= (count ks) (count ed/by-key)))))

(deftest ein-tastendruck-findet-seine-absicht
  (is (= :redo (ed/by-key (ed/chord {:key "Z" :ctrl true :shift true})))
      "Shift meldet den Buchstaben groß — der Katalog schreibt ihn klein")
  (is (= :undo (ed/by-key (ed/chord {:key "z" :ctrl true}))))
  (is (= :remove (ed/by-key (ed/chord {:key "Delete"}))))
  (is (= :move-up (ed/by-key (ed/chord {:key "ArrowUp" :alt true}))))
  (is (= :cursor-prev (ed/by-key (ed/chord {:key "ArrowUp"})))
      "ohne alt ist dieselbe Taste Navigation, nicht Verschieben"))

(deftest escape-kommt-aus-jedem-modus-heraus
  (doseq [m (keys ed/modes)]
    (is (= :idle (:mode (ed/transition {:mode m :pending [:irgendwas]} :cancel)))
        (str "Modus " m " hat keinen Ausgang"))
    (is (nil? (:pending (ed/transition {:mode m :pending [:irgendwas]} :cancel))))))

(deftest jeder-modus-ist-von-idle-erreichbar
  (doseq [m (remove #{:idle} (keys ed/modes))]
    (is (some (fn [[ereignis ziel]] (and (= ziel m) ereignis))
              (get ed/modes :idle))
        (str "Modus " m " ist von :idle aus nicht erreichbar"))))

(deftest unmoegliche-absicht-laesst-den-entwurf-in-ruhe
  (let [leer (ed/draft {:layout nil})]
    (doseq [{:keys [id]} ed/intents]
      (is (= leer (ed/run leer id {})) (str id " wirkt ohne geladene App")))
    (is (= "eine geladene App" (ed/blocked-because leer :add)))))

(deftest jeder-schritt-ist-genau-umkehrbar
  (doseq [[fid store0] fixtures
          cursor (every-cursor store0)
          {:keys [id step]} ed/intents
          :when step
          :let [editor   (assoc store0 :cursor cursor)
                args (case id
                       :add          {:kind :badge}
                       :move-to      {:src cursor :tgt []}
                       :retype       {:widget :markdown}
                       :put-param    {:key :title :value "Probe"}
                       :replace-node {:node (cat/blank-node :badge)}
                       :put-var      {:name :probe :spec {:default ""}}
                       :rename-var   {:from :suche :to :gesucht}
                       :drop-var     {:name :probe}
                       :bind-new-var {:key :text :ref :val :name :probe :spec {:default ""}}
                       :put-file-key {:key :icon :value "📦"}
                       :paste        nil
                       {})
                vor  (cond-> editor
                       (= :paste id)
                       (assoc :clip {:node (cat/blank-node :badge) :label "Abzeichen"})
                       (= :drop-var id)
                       (assoc-in [:tree :vars :probe] {:default nil}))
                store1  (ed/run vor id args)]
          :when (not= (select-keys vor [:tree :file]) (select-keys store1 [:tree :file]))]
    (testing (str fid " " id " @ " cursor)
      (is (= 1 (- (count (:past store1)) (count (:past vor))))
          "eine Absicht legt genau einen Schritt ab")
      (is (= (:label (get ed/intent-index id)) (ed/undo-label store1))
          "der Schritt trägt den Namen der Absicht")
      (let [zurueck (ed/run store1 :undo)]
        (is (= (select-keys vor [:tree :file]) (select-keys zurueck [:tree :file]))
            "undo stellt Baum und App-Angaben genau her")
        (is (= (:cursor vor) (:cursor zurueck)) "undo stellt auch die Auswahl her")
        (is (= (:tree store1) (:tree (ed/run zurueck :redo))) "redo führt wieder hin")))))

(deftest der-cursor-zeigt-nach-jedem-schritt-auf-einen-knoten
  (doseq [[fid store0] fixtures
          cursor (every-cursor store0)
          id     [:remove :cut :add :duplicate :move-up :move-down :paste]
          :let [editor  (-> store0
                        (assoc :cursor cursor)
                        (assoc :clip {:node (cat/blank-node :badge) :label "Abzeichen"}))
                store1 (ed/run editor id (when (= :add id) {:kind :badge}))]
          :when (not= (:tree editor) (:tree store1))]
    (testing (str fid " " id " @ " cursor)
      (is (contains? #{:widget :container} (cat/node-type (ed/node store1)))
          (str "Cursor " (pr-str (:cursor store1)) " zeigt auf "
               (pr-str (ed/node store1)))))))

(deftest tippen-verschmilzt-zu-einem-schritt
  (let [editor (-> (second (first fixtures)) (assoc :cursor []))
        getippt (reduce (fn [s t] (ed/run s :put-param {:key :title :value t}))
                        editor ["N" "Ne" "Neu" "Neue"])]
    (is (= 1 (count (:past getippt))) "ein Titel frisst nicht den halben Verlauf")
    (is (= "Neue" (:title (:tree getippt))))
    (is (= (:tree editor) (:tree (ed/run getippt :undo)))
        "und ein Undo nimmt das ganze Wort zurück")
    (let [zwei (-> editor
                   (ed/run :put-param {:key :title :value "A"})
                   (ed/run :put-param {:key :fill? :value true}))]
      (is (= 2 (count (:past zwei))) "zwei Parameter sind zwei Schritte"))))

(deftest eine-menue-absicht-braucht-kein-argument
  (doseq [{:keys [id]} (filter :menu ed/intents)]
    (let [gewirkt (for [[_ store0] fixtures
                        cursor (every-cursor store0)
                        :let [editor  (-> store0 (assoc :cursor cursor)
                                      (assoc :clip {:node (cat/blank-node :badge)
                                                    :label "Abzeichen"}))
                              store1 (ed/run editor id)]]
                    (not= (select-keys editor [:tree :file :clip])
                          (select-keys store1 [:tree :file :clip])))]
      (is (some true? gewirkt)
          (str id " steht im Menü, wirkt aber ohne Argument nirgends")))))

(deftest art-wechseln-behaelt-was-die-neue-art-auch-fuehrt
  (let [n {:widget :badge :text {:item :status} :tone :info :vars {:x {:default 1}}}
        m (ed/retype n :markdown)]
    (is (= :markdown (:widget m)))
    (is (= {:item :status} (:text m)) "ein geteilter Parameter überlebt den Wechsel")
    (is (= {:x {:default 1}} (:vars m)) "Variablen hängen am Knoten, nicht an der Art")
    (is (nil? (:tone m)) "was die neue Art nicht führt, fällt weg")))

(deftest der-verlauf-waechst-nicht-ueber-die-grenze
  (let [editor (reduce (fn [s i] (ed/run s :put-param {:key (keyword (str "p" i)) :value i}))
                   (assoc (second (first fixtures)) :cursor [])
                   (range (+ 10 ed/history-limit)))]
    (is (= ed/history-limit (count (:past editor))))))

(deftest jede-absicht-wird-vom-durchlauf-wirklich-erreicht
  (let [erreicht (into #{}
                       (for [[_ store0] fixtures
                             cursor (every-cursor store0)
                             {:keys [id]} ed/intents
                             :let [editor (-> store0 (assoc :cursor cursor)
                                          (assoc :clip {:node (cat/blank-node :badge)
                                                        :label "Abzeichen"})
                                          (assoc :past [{:tree nil :file nil :cursor []}]
                                                 :future [{:tree nil :file nil :cursor []}]))]
                             :when (ed/possible? editor id)]
                         id))]
    (is (= (set (map :id ed/intents)) erreicht)
        (str "nie möglich, also unerreichbar: "
             (pr-str (remove erreicht (map :id ed/intents)))))))

(deftest ein-name-ueberschreibt-nie-eine-andere-deklaration
  (let [editor (-> (ed/draft {:layout {:kind :rows :vars {:eins {:default 1}
                                                      :zwei {:default 2}}
                                   :items []}})
               (assoc :cursor []))]
    (testing "Kollision hängt eine Nummer an, statt die andere zu verschlucken"
      (let [nach (ed/run editor :rename-var {:from :eins :to :zwei})
            vars (:vars (:tree nach))]
        (is (= 2 (count vars)) "zwei Deklarationen bleiben zwei")
        (is (= #{:zwei :zwei-2} (set (keys vars))))
        (is (= 2 (:default (:zwei vars))) "die bestehende behält ihren Wert")))

    (testing "ohne Gross- und Kleinschreibung verglichen, wie in App Studio"
      (is (= #{:zwei :zwei-2}
             (set (keys (:vars (:tree (ed/run editor :rename-var {:from :eins :to :ZWEI}))))))))

    (testing "ein geleerter Name ändert nichts"
      (is (= editor (ed/run editor :rename-var {:from :eins :to nil}))))

    (testing "ein freier Name wird einfach genommen"
      (is (= #{:drei :zwei}
             (set (keys (:vars (:tree (ed/run editor :rename-var {:from :eins :to :drei}))))))))))

(deftest eine-gelesene-variable-wird-nicht-geloescht
  (let [editor (-> (ed/draft {:layout (cat/template :liste)}) (assoc :cursor []))]
    (testing ":suche wird vom Eingabefeld gelesen"
      (is (string? (ed/refusal editor :drop-var {:name :suche})))
      (is (= (:tree editor) (:tree (ed/run editor :drop-var {:name :suche})))
          "sonst zeigt der Baustein danach ins Leere"))

    (testing "was niemand liest, geht weg"
      (let [mit (assoc-in editor [:tree :vars :ungelesen] {:default nil})]
        (is (nil? (ed/refusal mit :drop-var {:name :ungelesen})))
        (is (nil? (:ungelesen (:vars (:tree (ed/run mit :drop-var {:name :ungelesen}))))))))

    (testing "auch eine Variable, die nur eine andere Variable liest, zählt"
      (let [mit (-> editor
                    (assoc-in [:tree :vars :ungelesen] {:default nil})
                    (assoc-in [:tree :vars :leser] {:expr [:+ {:val :ungelesen}]}))]
        (is (string? (ed/refusal mit :drop-var {:name :ungelesen}))
            "downstream variables zählen mit")))))
