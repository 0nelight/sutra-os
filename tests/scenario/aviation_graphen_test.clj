(ns ^:api scenario.aviation-graphen-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.types.paths :as paths]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(use-fixtures :once pod/with-mem-pod)

(defn- graphen [] (:graphs (:body (api/list-graphs))))
(defn- graph [handle] (:body (api/get! :graph-open {:handle (str handle)})))
(defn- benannt [n] (first (filter #(= n (:name %)) (graphen))))

(defn- speichern! [body]
  (is (ok? (api/mutate :create-graph body {:stage? true})))
  (is (ok? (api/post! :staging))))

(defn- ordner! [n] (:rid (:body (api/post! :kompass-folders {:name n :parent-rid nil}))))

(def ^:private titel "Linienwartung · Befundfluss")

(deftest ein-graph-ist-eine-benannte-abgelegte-ressource
  (testing "ein freier Titel wird zur Kennung, der Graph bekommt seine RID"
    (speichern! {:name titel :title titel :mode :lineage
                 :description "wie ein Befund aus dem Bordbuch zum Mangel wird"
                 :branch "main" :fallback-branches ["main"]
                 :visible-types {:object-types #{:mangel} :transforms #{:befunde-verdichten}}})
    (let [g (benannt :linienwartung-befundfluss)]
      (is (= titel (:title g)) "der Titel bleibt wörtlich, er ist nicht die Kennung")
      (is (= :lineage (:mode g)))
      (is (re-find #"^ri\.sutra\.main\.graph\." (str (:id g))))))

  (testing "die Wahl des Zweigs wird mit dem Graphen gespeichert"
    (let [g (graph "linienwartung-befundfluss")]
      (is (= "main" (:branch g)))
      (is (= ["main"] (vec (:fallback-branches g))))))

  (testing "nicht abgelegt hat er keinen Pfad; abgelegt schon"
    (is (nil? (:path (graph "linienwartung-befundfluss"))))
    (let [rid (:id (benannt :linienwartung-befundfluss))]
      (is (ok? (api/post! :kompass-place
                          {:ref rid :parent-rid (ordner! "Wartung") :name "Befundfluss"})))
      (is (= "/Wartung/Befundfluss" (:path (graph rid))))))

  (testing "über Name, RID, Pfad und eingefügten Titel dieselbe Ressource"
    (let [rid (:id (benannt :linienwartung-befundfluss))]
      (is (= rid
             (:id (graph "linienwartung-befundfluss"))
             (:id (graph rid))
             (:id (graph "/Wartung/Befundfluss"))
             (:id (graph titel))))))

  (testing "ein unbekannter Griff ist 404, kein leerer Graph"
    (is (= 404 (:status (api/get! :graph-open {:handle "gibt-es-nicht"})))))

  (testing "der Knotenumfang kommt unverändert zurück"
    (let [vt (:visible-types (graph "linienwartung-befundfluss"))]
      (is (= #{:mangel} (set (:object-types vt))))
      (is (= #{:befunde-verdichten} (set (:transforms vt))))))

  (testing "Layout und Farbschema hängen an der RID; der Name tut es auch"
    (let [rid (:id (benannt :linienwartung-befundfluss))]
      (is (ok? (tx/send! :post (paths/url :graph-layout {:id rid})
                         {:node-kind :object-type :node-key "mangel" :x 120 :y 40})))
      (is (ok? (api/post! :graph-schemes {:id rid}
                          {:name "nach Quelle" :rules {:kind :by-type} :is-default true})))
      (is (= 1 (count (:layout (graph rid)))))
      (is (= 1 (count (:color-schemes (graph rid)))))
      (is (ok? (tx/send! :post (paths/url :graph-layout {:id "linienwartung-befundfluss"})
                         {:node-kind :transform :node-key "befunde-verdichten" :x 10 :y 10})))
      (is (= 2 (count (:layout (graph rid))))))))

(deftest umbenennen-behaelt-rid-und-ort
  (speichern! {:name "Entwurf C-Check" :title "Entwurf C-Check" :mode :lineage})
  (let [rid (:id (benannt :entwurf-c-check))]
    (is (ok? (api/post! :kompass-place {:ref rid :parent-rid (ordner! "Entwuerfe")
                                         :name "C-Check"})))
    (is (ok? (api/mutate :update-graph
                         {:name "entwurf-c-check" :title "AOG-Verfolgung" :mode :lineage}
                         {:stage? true})))
    (is (ok? (api/post! :staging)))
    (let [g (graph "entwurf-c-check")]
      (is (= rid (:id g)))
      (is (= "AOG-Verfolgung" (:title g)))
      (is (= "/Entwuerfe/C-Check" (:path g)) "die Ablage zeigt auf die RID"))))

(deftest loeschen-raeumt-ablage-und-layout-mit-ab
  (speichern! {:name "Wegwerf" :title "Wegwerf" :mode :lineage})
  (let [rid (:id (benannt :wegwerf))]
    (is (ok? (api/post! :kompass-place {:ref rid :parent-rid (ordner! "Ablage") :name "Wegwerf"})))
    (is (ok? (tx/send! :post (paths/url :graph-layout {:id rid})
                       {:node-kind :object-type :node-key "mangel" :x 1 :y 2})))
    (is (ok? (api/mutate :delete-graph {:name "wegwerf"} {:stage? true})))
    (is (ok? (api/post! :staging)))
    (is (nil? (benannt :wegwerf)))
    (is (= 404 (:status (api/get! :graph-open {:handle "wegwerf"}))))
    (is (empty? (filter #(= rid (:ref %)) (:nodes (:body (api/get! :kompass-nodes)))))
        "kein Geistereintrag im Kompass")
    (is (empty? (:layout (graph rid))))))

(deftest ein-canvas-haelt-sein-layout-auch-ohne-graph
  (testing "der Standard-Canvas eines Modus nimmt Layout an, bevor jemand einen Graphen speichert"
    (is (ok? (tx/send! :post (paths/url :graph-layout {:id "mode:pipeline"})
                       {:node-kind :transform :node-key "bordbuch-parsen" :x 40 :y 80})))
    (let [g (graph "mode:pipeline")]
      (is (true? (:default-canvas? g)))
      (is (= :pipeline (:mode g)))
      (is (= [{:graph-id "mode:pipeline" :node-kind :transform :node-key "bordbuch-parsen" :x 40.0
               :y 80.0}]
             (mapv #(-> % (update :x double) (update :y double)) (:layout g)))))
    (is (empty? (filter #(= :mode-pipeline (:name %)) (graphen))) "und er ist keine Ressource"))
  (testing "was weder Graph noch Modus ist, bleibt 404"
    (is (= 404 (:status (api/get! :graph-open {:handle "mode:Pipeline"}))))
    (is (= 404 (:status (api/get! :graph-open {:handle "mode:pipe/line"}))))))

(defn- layout! [handle eintraege]
  (:body (api/post! :graph-layout {:id handle} {:entries eintraege :commit? true})))

(deftest ein-layout-wird-in-einem-schreibvorgang-gespeichert
  (let [h "mode:lineage" xy (juxt :node-key #(double (:x %)) #(double (:y %)))]
    (let [r (layout! h [{:node-kind :object-type :node-key "luftfahrzeug" :x 1 :y 2}
                        {:node-kind :object-type :node-key "mangel" :x 3 :y 4}
                        {:node-kind :transform :node-key "befunde-verdichten" :x 5 :y 6}])]
      (is (= 3 (:saved r)))
      (is (true? (:committed r))))
    (is (= #{["luftfahrzeug" 1.0 2.0] ["mangel" 3.0 4.0] ["befunde-verdichten" 5.0 6.0]}
           (set (map xy (:layout (graph h))))))
    (testing "erneut gespeichert wird ersetzt, nicht angehäuft"
      (is (= 1
             (:saved (layout! h [{:node-kind :object-type :node-key "luftfahrzeug" :x 99 :y 98}]))))
      (is (= 3 (count (:layout (graph h)))))
      (is (some #{["luftfahrzeug" 99.0 98.0]} (map xy (:layout (graph h))))))
    (testing "ein einzelner Eintrag bleibt ein Roundtrip"
      (is (ok? (api/post! :graph-layout {:id h}
                          {:node-kind :object-type :node-key "triebwerk" :x 7 :y 8})))
      (is (= 4 (count (:layout (graph h))))))
    (testing "ein Stapel für einen Griff, der nichts nennt, wird ganz abgelehnt"
      (is (= 404 (:status (api/post! :graph-layout {:id "mode:pipe/line"}
                                     {:entries [{:node-kind :object-type :node-key "x" :x 1 :y 1}]
                                      :commit? true})))))))

(deftest die-alte-ansichten-route-antwortet-noch
  (speichern! {:name "Altsonde" :title "Altsonde" :mode :lineage})
  (is (some #(= :altsonde (:name %)) (:graphs (:body (tx/send! :get (paths/url :views)))))))

(defn- annotationen [] (:annotations (:body (api/get! :kompass-annotations))))

(deftest annotationen-sind-geteilter-zustand
  (is (ok? (api/post! :kompass-annotations
                      {:ref "object-types/mangel" :tags ["lufttuechtigkeit" "kritisch"]
                       :trashed false :curated true})))
  (let [a (get (annotationen) "object-types/mangel")]
    (is (= #{"lufttuechtigkeit" "kritisch"} (set (:tags a))))
    (is (true? (:curated a)))
    (is (false? (:trashed a))))
  (testing "der Papierkorb ist ein Merkmal der Annotation und gilt für alle"
    (is (ok? (api/post! :kompass-annotations {:ref "datasets/acars_roh" :trashed true :tags []})))
    (is (true? (:trashed (get (annotationen) "datasets/acars_roh")))))
  (testing "die Titelseite eines Projekts ist eine Annotation wie jede"
    (is (ok? (api/post! :kompass-annotations {:ref "project/linienwartung"
                                            :doc "# Linienwartung\nTransitchecks und Befunde"}))))
  (testing "eine Annotation, die nichts mehr sagt, verschwindet"
    (is (ok? (api/post! :kompass-annotations
                        {:ref "object-types/mangel" :tags [] :trashed false :curated false})))
    (is (nil? (get (annotationen) "object-types/mangel")))
    (is (contains? (annotationen) "datasets/acars_roh"))))

(defn- bytes-von [k id]
  (let [{:keys [status ^bytes bytes content-type]} (tx/get-bytes! (paths/url k {:id id}))]
    {:status status :typ (str content-type) :text (when bytes (String. bytes "UTF-8"))}))

(deftest der-server-zeichnet-einen-gespeicherten-graphen
  (speichern! {:name "Befundbild" :title "Befundbild" :mode :lineage
               :visible-types {:object-types #{:mangel} :transforms #{:befunde-verdichten}}})
  (let [rid (:id (benannt :befundbild))]
    (is (ok? (tx/send! :post (paths/url :graph-layout {:id rid})
                       {:node-kind :object-type :node-key "mangel" :x 120 :y 40})))
    (let [{:keys [status typ text]} (bytes-von :graph-svg rid)]
      (is (= 200 status))
      (is (re-find #"image/svg\+xml" typ))
      (is (str/starts-with? text "<svg "))
      (is (str/includes? text "mangel"))
      (is (str/includes? text "Befundbild"))
      (is (not (str/includes? text "foreignObject")) "echte Vektorgrafik, kein DOM-Abzug"))
    (testing "dasselbe als Graphviz-Quelle, mit festgehaltenen Positionen"
      (let [{:keys [status typ text]} (bytes-von :graph-dot rid)]
        (is (= 200 status))
        (is (re-find #"text/vnd\.graphviz" typ))
        (is (str/starts-with? text "digraph "))
        (is (str/includes? text "mangel"))
        (is (str/includes? text "pos="))))
    (is (= 200 (:status (bytes-von :graph-svg "befundbild"))))
    (is (= 200 (:status (bytes-von :graph-dot "befundbild"))))
    (is (= 404 (:status (bytes-von :graph-svg "gibt-es-nicht")))))
  (testing "nur ein Lineage-Graph wird gezeichnet"
    (speichern! {:name "Ontologiebild" :title "Ontologiebild" :mode :ontology})
    (is (= 400 (:status (bytes-von :graph-svg "ontologiebild"))))
    (is (= 400 (:status (bytes-von :graph-dot "ontologiebild"))))))

(deftest ein-lineage-graph-liegt-nicht-auf-dem-zweig
  (let [z "lineage-sicht"]
    (is (ok? (api/post! :branches {:parent "main" :name z})))
    (tx/with-branch z
      (testing "vom Zweig aus angelegt und geaendert, wie CLI und Browser es schicken"
        (is (ok? (api/mutate :create-graph {:name "Sicht" :title "Sicht" :mode :lineage :branch z}
                             {:stage? true})))
        (is (ok? (api/mutate :update-graph {:name "sicht" :title "Neu" :mode :lineage :branch z}
                             {:stage? true})))
        (is (= "Neu" (:title (graph "sicht"))) "der Zweig liest ihn")
        (is (ok? (api/post! :graph-layout {:id "sicht"}
                            {:node-kind :transform :node-key "t" :x 1 :y 2}))))
      (testing "ein Proposal hat davon nichts"
        (is (empty? (filter #(= :graphs (keyword (:section %)))
                            (:tasks (:body (api/get! :branch-changes {:name z}))))))
        (is (empty? (:staged (:body (api/get! :staging)))))))
    (testing "main hat ihn sofort, mit der Wahl des Zweigs und dem Layout"
      (let [g (graph "sicht")]
        (is (= "Neu" (:title g)))
        (is (= z (:branch g)))
        (is (= 1 (count (:layout g))))))
    (testing "ein Graph einer anderen Sicht bleibt auf dem Zweig"
      (tx/with-branch z
        (is (ok? (api/mutate :create-graph {:name "Karte" :title "Karte" :mode :ontology}
                             {:stage? true})))
        (is (ok? (api/post! :staging)))
        (is (some? (benannt :karte))))
      (is (nil? (benannt :karte))))
    (testing "lineage facts findet ihn auf dem Zweig"
      (tx/with-branch z
        (is (ok? (api/get! :lineage-facts {:graph "sicht"})))))
    (testing "lineage facts nennt, was der Zweig aenderte"
      (tx/with-branch z
        (is (vector? (:modified (:body (api/get! :lineage-facts {:modified true})))))))))
