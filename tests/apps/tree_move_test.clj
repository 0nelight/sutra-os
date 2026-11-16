(ns ^:unit apps.tree-move-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.capps.catalog :as cat]
            [sutra.capps.tree-move :as tm]))

(def ^:private baeume
  (into {} (for [id [:leer :liste :dashboard :reiter]] [id (cat/template id)])))

(defn- pfade [tree] (map (comp vec :path) (cat/flatten-tree tree)))

(defn- knoten-zahl
  [tree]
  (count (cat/flatten-tree tree)))

(deftest ein-ziel-nennt-sich-vollstaendig
  (doseq [[id tree] baeume
          path (pfade tree)
          zone tm/zones
          :let [t (tm/target tree path zone)]
          :when t]
    (testing (str id " " zone " @ " path)
      (is (contains? #{:many :panes :single} (:holds t)))
      (is (vector? (:parent t)))
      (is (keyword? (:slot t)))
      (is (or (= :single (:holds t)) (integer? (:index t)))
          "eine Reihe nennt den Index, ein Einzelplatz hat keinen"))))

(deftest wo-der-indikator-zeigt-landet-es-auch
  (doseq [[id tree] baeume
          path (pfade tree)
          zone tm/zones
          :let [neu (cat/blank-node :badge)
                res (tm/put tree (tm/target tree path zone) neu)]
          :when res]
    (testing (str id " " zone " @ " path)
      (let [gelandet (cat/node-at (:tree res) (:path res))]
        (is (= neu (if (tm/pane? gelandet) (:body gelandet) gelandet))
            "der genannte Pfad trägt genau den abgeworfenen Knoten")))))

(deftest hineinfallen-erreicht-jetzt-auch-platz-und-reiter
  (testing "der Rumpf eines Abschnitts ist ein Ziel (vorher unerreichbar)"
    (let [tree {:kind :section :title "A"}
          t    (tm/target tree [] :into)]
      (is (= {:parent [] :slot :body :holds :single :index nil} t))
      (is (= :badge (:widget (:body (:tree (tm/put tree t (cat/blank-node :badge)))))))))

  (testing "ein belegter Einzelplatz nimmt nichts mehr"
    (is (nil? (tm/target {:kind :section :body {:widget :badge}} [] :into))))

  (testing "ein Widget nimmt nie etwas hinein"
    (is (nil? (tm/target {:widget :badge} [] :into))))

  (testing "Reiter nehmen hinein und umschlagen dabei"
    (let [tree (cat/template :reiter)
          tabs [:items 0]
          t    (tm/target tree tabs :into)
          res  (tm/put tree t (cat/blank-node :badge))
          umschlag (cat/node-at (:tree res) (vec (butlast (:path res))))]
      (is (= :panes (:holds t)))
      (is (tm/pane? umschlag) "der Knoten kommt als Reiter-Umschlag an")
      (is (= :badge (:widget (:body umschlag))))
      (is (= :badge (:widget (cat/node-at (:tree res) (:path res))))
          "genannt wird der Rumpf — das ist die Zeile, die die Gliederung zeigt"))))

(deftest ein-reiter-bekommt-nie-die-id-eines-anderen
  (let [vorhanden [{:id :pane-0 :label "A" :body nil} {:id :pane-2 :label "C" :body nil}]
        neu       (cat/pane vorhanden {:widget :badge})]
    (is (= :pane-1 (:id neu)) "die freie Nummer wird gesucht, nicht gezählt")
    (is (= 3 (count (into #{} (map :id) (conj vorhanden neu))))
        "drei Reiter, drei verschiedene ids — sonst trifft :select den falschen")))

(deftest ein-knoten-wandert-und-bleibt-genau-einmal-da
  (doseq [[id tree] baeume
          src  (remove empty? (pfade tree))
          tgt  (pfade tree)
          zone tm/zones
          :let [vorher (knoten-zahl tree)
                res    (tm/move tree src tgt zone)]
          :when res]
    (testing (str id " " src " → " zone " " tgt)
      (is (= vorher (knoten-zahl (:tree res)))
          "verschieben erzeugt und verliert nichts")
      (let [quelle    (cat/node-at tree src)
            gelandet  (cat/node-at (:tree res) (:path res))]
        (is (= (if (tm/pane? quelle) (:body quelle) quelle)
               (if (tm/pane? gelandet) (:body gelandet) gelandet))
            "am genannten Pfad liegt der verschobene Knoten")))))

(deftest eine-zeile-zieht-das-was-sie-auch-loescht
  (let [tree  (cat/template :reiter)
        zeile [:items 0 :tabs 1 :body]]
    (testing "die Gliederung zeigt den Rumpf, gemeint ist der Reiter"
      (is (= [:items 0 :tabs 1] (tm/handle tree zeile))
          "dieselbe Regel, nach der sich die Zeile löscht (:remove-path)")
      (is (tm/movable? tree zeile)))

    (testing "umgeordnet wird der ganze Reiter, nie ein nackter Rumpf"
      (let [{t :tree p :path} (tm/move tree zeile [:items 0 :tabs 0 :body] :before)]
        (is (= [:zwei :eins] (mapv :id (get-in t [:items 0 :tabs])))
            "zwei Reiter lassen sich umordnen — das ging vorher gar nicht")
        (is (every? #(contains? % :body) (get-in t [:items 0 :tabs]))
            "kein Reiter bleibt ohne Rumpf stehen")
        (is (= [:items 0 :tabs 0 :body] p)
            "die Auswahl landet auf der Zeile, die die Gliederung zeigt")))

    (testing "ein Reiter kann die Reiter verlassen und wird dabei ausgepackt"
      (let [{t :tree} (tm/move tree zeile [] :into)]
        (is (= 1 (count (get-in t [:items 0 :tabs]))))
        (is (= :rows (:kind (last (:items t))))
            "in einer Reihe liegt der Rumpf, nicht der Umschlag")))))

(deftest in-sich-selbst-faellt-nichts
  (let [tree (cat/template :dashboard)]
    (doseq [zone tm/zones]
      (is (nil? (tm/move tree [:items 1] [:items 1 :items 0] zone))
          "ein Behälter kann nicht in sein eigenes Kind wandern"))))

(deftest verschieben-in-derselben-reihe-trifft-die-gemeinte-stelle
  (let [tree {:kind :rows :items [{:widget :badge :text "a"}
                                  {:widget :badge :text "b"}
                                  {:widget :badge :text "c"}]}
        texte (fn [t] (mapv :text (:items t)))]
    (is (= ["b" "a" "c"] (texte (:tree (tm/move tree [:items 1] [:items 0] :before)))))
    (is (= ["a" "c" "b"] (texte (:tree (tm/move tree [:items 1] [:items 2] :after)))))
    (is (= ["b" "a" "c"] (texte (:tree (tm/move tree [:items 0] [:items 1] :after))))
        "eins nach hinten: der Entfernen-Versatz ist eingerechnet")))

(defn- w [t] {:widget :badge :tone :ok :text t})

(def ^:private reihe
  {:kind :rows
   :items [(w "a")
           {:kind :columns :items [(w "b") (w "c")]}
           (w "d")
           {:kind :columns :items []}]})

(defn- formen [t] (mapv #(or (:text %) (:kind %)) (:items t)))

(deftest umordnen-in-derselben-reihe
  (testing "dahinter"
    (let [plan (tm/plan reihe [:items 0] [:items 2] :after)
          {t :tree p :path} (tm/move reihe [:items 0] [:items 2] :after)]
      (is (= {:parent [] :slot :items :holds :many :index 2} plan))
      (is (= [:columns "d" "a" :columns] (formen t))
          "der gezogene Knoten steht hinter dem Ziel, der Rest rückt auf")
      (is (= [:items 2] p) "die Auswahl folgt dem verschobenen Knoten")
      (is (= (w "a") (get-in t p)))))

  (testing "davor"
    (let [{t :tree p :path} (tm/move reihe [:items 2] [:items 0] :before)]
      (is (= ["d" "a" :columns :columns] (formen t)))
      (is (= [:items 0] p))))

  (testing "auf den unmittelbaren Vorgänger ist ein Nullzug"
    (is (= reihe (:tree (tm/move reihe [:items 3] [:items 2] :after))))
    (is (nil? (tm/move reihe [:items 2] [:items 2] :after)))))

(deftest hinein-in-einen-behaelter
  (let [plan (tm/plan reihe [:items 0] [:items 1] :into)
        {t :tree p :path} (tm/move reihe [:items 0] [:items 1] :into)]
    (is (= :many (:holds plan)))
    (is (= [:items 0] (:parent plan))
        "das Ziel rutscht auf, weil die Quelle davor entfällt")
    (is (= ["b" "c" "a"] (mapv :text (get-in t [:items 0 :items]))))
    (is (= [:items 0 :items 2] p))
    (is (= 3 (count (:items t))) "der Knoten steht genau einmal im Baum")))

(deftest in-den-eigenen-teilbaum-faellt-nichts
  (doseq [zone tm/zones]
    (is (nil? (tm/plan reihe [:items 1] [:items 1 :items 0] zone))
        "ein Behälter in sein eigenes Kind wäre ein verlorener Teilbaum")
    (is (nil? (tm/plan reihe [:items 1] [:items 1] zone)) "auf sich selbst")
    (is (nil? (tm/plan reihe [] [:items 0] zone)) "die Wurzel bleibt, wo sie ist")))

(deftest einfuegen-aus-der-ablage
  (let [{t :tree p :path} (tm/insert reihe [:items 3] :into (w "neu"))]
    (is (= [:items 3 :items 0] p))
    (is (= ["neu"] (mapv :text (get-in t [:items 3 :items]))))
    (is (= 4 (count (:items t))) "die Ablage fügt hinzu, sie verschiebt nicht"))

  (testing "neben ein Blatt"
    (let [{t :tree p :path} (tm/insert reihe [:items 0] :after (w "neu"))]
      (is (= [:items 1] p))
      (is (= ["a" "neu" :columns "d" :columns] (formen t)))))

  (testing "ein belegter Einzelplatz nimmt nichts auf"
    (is (nil? (tm/insert {:kind :section :body (w "x")} [] :into (w "neu"))))
    (is (nil? (tm/insert {:kind :section :body (w "x")} [:body] :after (w "neu")))
        "und neben einem Einzelplatz gibt es keine Reihe")))

(deftest die-abwurf-regel-ist-die-paletten-regel
  (is (= {:parent [:items 1] :slot :items :holds :many :index 2}
         (tm/add-target reihe [:items 1]))
      "ein Behälter nimmt hinein, ans Ende")
  (is (= {:parent [] :slot :items :holds :many :index 1}
         (tm/add-target reihe [:items 0]))
      "ein Blatt nimmt nichts hinein, also dahinter")
  (is (= {:parent [] :slot :items :holds :many :index 4}
         (tm/add-target reihe []))))
