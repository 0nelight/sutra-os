(ns sutra-ui.apps.enta-editor.enta-editor-smoke-test
  (:require [cljs.test :refer [deftest is testing run-tests]]
            [clojure.string :as str]
            [reagent.dom.server :as rds]
            [sutra.capps.catalog :as cat]
            [sutra.capps.editing :as ed]
            [sutra.capps.table :as tbl]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.apphost.sdk :as app]
            [sutra-ui.apphost.render :as render]
            [sutra-ui.apps.enta-editor.state :as es]
            [sutra-ui.apps.enta-editor.form :as form]
            [sutra-ui.apps.enta-editor.tree :as tree]
            [sutra-ui.apps.enta-editor.palette :as palette]
            [sutra-ui.apps.enta-editor.core]))

(def ^:private tree-fixture
  {:kind :rows :title "Kosten"
   :vars {:jahr  {:default 2026}
          :zeile {:source :rechnungsposition}}
   :items [{:widget :kpi :label "Summe" :value {:val :zeile} :fmt :eur2}
           {:kind :tabs :select {:var :jahr}
            :tabs [{:id :liste :label "Liste"
                    :body {:widget :object-table
                           :rows {:val :zeile}
                           :cols [{:prop :nr :label "Nr"}]}}]}
           {:widget :text-input :sel {:var :jahr} :placeholder "Jahr"}]})

(def ^:private data-fixture
  {:zeile [{:id "p1" :values {:nr "R-1" :betrag 100}}
           {:id "p2" :values {:nr "R-2" :betrag 250}}]})

(deftest catalog-covers-the-compiled-runtime
  (testing "every catalogued widget resolves in the SDK's widget registry"
    (let [registry (set (keys render/widget-registry))]
      (is (every? registry (map :widget cat/widget-specs)))))
  (testing "every catalogued container is a real layout kind"
    (is (every? #(contains? #{:rows :columns :flow :toolbar :section :tabs :pages
                              :loop :profiles :overlay} %)
                (map :kind cat/container-specs))))
  (testing "every :fmt option the editor offers is a real formatter"
    (is (every? #(contains? tbl/formats %) cat/format-options)))
  (testing "no catalogued param names a kind the form cannot render"
    (is (every? #(contains? cat/param-kinds (:kind %))
                (mapcat :params (concat cat/widget-specs cat/container-specs))))))

(deftest outline-addresses-every-node
  (let [entries (cat/flatten-tree tree-fixture)]
    (testing "the outline reaches nested pane bodies"
      (is (= 5 (count entries)))
      (is (some #(= [:items 1 :tabs 0 :body] (:path %)) entries)))
    (testing "each path round-trips through node-at"
      (is (every? (fn [{:keys [path node]}] (= node (cat/node-at tree-fixture path)))
                  entries)))))

(deftest edits-are-pure-tree-ops
  (testing "add appends into a :many slot"
    (is (= 4 (count (:items (cat/add-child tree-fixture [] :items
                                           (cat/blank-node :badge)))))))
  (testing "remove drops exactly one child"
    (is (= 2 (count (:items (cat/remove-at tree-fixture [:items 0]))))))
  (testing "move swaps neighbours and is its own inverse"
    (is (= tree-fixture (-> tree-fixture (cat/move-at [:items 0] 1)
                            (cat/move-at [:items 1] -1)))))
  (testing "a blank container carries its child slots"
    (is (= {:kind :columns :items []} (cat/blank-node :columns)))))

(deftest problems-name-their-node
  (testing "a clean tree has no findings"
    (is (empty? (cat/explain-tree tree-fixture))))
  (testing "an undeclared variable handle is a finding, with its path"
    (let [broken (assoc-in tree-fixture [:items 2 :sel] {:var :gibt-es-nicht})
          probs  (cat/explain-tree broken)]
      (is (= 1 (count probs)))
      (is (= [:items 2] (:path (first probs))))
      (is (= :sel (:param (first probs))))))
  (testing "an unknown :fmt is a finding, not a silent fallback"
    (let [broken (assoc-in tree-fixture [:items 0 :fmt] :quatsch)]
      (is (= 1 (count (cat/explain-tree broken))))))
  (testing "a missing required param is a finding"
    (let [broken (update-in tree-fixture [:items 0] dissoc :value)]
      (is (seq (cat/explain-tree broken))))))

(deftest a-layered-chart-carries-its-measure-in-the-layers
  (let [layered {:widget :chart :rows {:from :auftrag} :x :monat
                 :series [{:name "Umsatz" :y :umsatz :kind :bar}
                          {:name "Marge" :y :marge :kind :line :axis :right}]}]
    (is (nil? (cat/explain-node layered)))
    (testing "…while a chart with neither :y nor layers is still a finding"
      (is (= :y (:param (cat/explain-node (dissoc layered :series))))))
    (testing "…a chart that counts, or draws its own layers, needs no :y"
      (let [plain (dissoc layered :series)]
        (is (nil? (cat/explain-node (assoc plain :agg :count))))
        (is (= :y (:param (cat/explain-node (assoc plain :agg :sum)))))
        (is (nil? (cat/explain-node {:widget :chart :layers [{:rows {:from :auftrag}}]})))))))

(deftest declared-vars-and-types
  (is (= [:jahr :zeile] (cat/declared-vars tree-fixture)))
  (is (= [:rechnungsposition]
         (cat/referenced-types {:widget :object-table :rows {:source :rechnungsposition}})))
  (is (= [] (cat/referenced-types {:widget :object-table :rows {:from :zeile}}))
      "a :from names a variable, never a type"))

(deftest preview-runs-the-shipping-runtime
  (testing "the fixture is a data layout the compiled runtime renders"
    (is (app/data-layout? tree-fixture)))
  (testing "rendering the draft through render-data-app paints the app"
    (swap! es/ui assoc :draft {:app :kosten
                      :descriptor {:name "Kosten" :layout tree-fixture}
                      :data data-fixture :tree tree-fixture
                      :cursor [] :version 1 :loading? false :error nil})
    (let [html (rds/render-to-static-markup
                [app/render-data-app (es/preview-descriptor) data-fixture])]
      (is (str/includes? html "capp--data"))
      (is (str/includes? html "R-1")))))

(deftest editor-panels-render
  (swap! es/ui assoc :draft {:app :kosten
                    :descriptor {:name "Kosten" :layout tree-fixture}
                    :data data-fixture :tree tree-fixture
                    :cursor [:items 0] :version 1 :loading? false :error nil})
  (testing "the outline paints every node's label"
    (let [html (rds/render-to-static-markup [tree/outline-panel])]
      (is (str/includes? html "Kennzahl"))
      (is (str/includes? html "Objekt-Tabelle"))))
  (testing "the inspector labels params in German, not as raw keywords"
    (let [html (rds/render-to-static-markup [form/inspector])]
      (is (str/includes? html "Beschriftung"))
      (is (str/includes? html "Format"))))
  (testing "the picker leads with the common blocks and offers all the others"
    (let [html (rds/render-to-static-markup [palette/picker identity])]
      (is (str/includes? html "Objekt-Tabelle"))
      (is (str/includes? html "Bausteine zeigen"))))
  (testing "the .enta view serialises the draft back to the FILE form"
    (let [txt (es/enta-text)]
      (is (str/includes? txt ":name"))
      (is (str/includes? txt ":layout"))
      (is (not (str/includes? txt ":applications"))
          "an `.enta` is the lean authoring form; the `:applications` wrapper is
           the monolith's and `parse-enta` refuses it"))))

(deftest the-column-panel-shows-what-it-would-inherit
  (testing "an empty Titel box is not a hole — it names the label the property gives"
    (reset! catalog/schema
            {:object-types
             {:rechnungsposition
              {:fields [{:name :nr :type :string :opts {:name "Belegnummer"}}
                        {:name :betrag :type :decimal
                         :opts {:format {:kind :currency :currency "EUR" :decimals 2}}}]}}})
    (swap! es/ui assoc :draft {:app :kosten
                      :descriptor {:name "Kosten" :layout tree-fixture}
                      :data data-fixture :tree tree-fixture
                      :cursor [:items 1 :tabs 0 :body] :version 1 :loading? false :error nil})
    (let [html (rds/render-to-static-markup [form/inspector])]
      (is (str/includes? html "Belegnummer")
          "the placeholder offers the ontology's display name, so an author sees
           there is nothing to type")))
  (testing "the shared rule is what produced it — not a second lookup in the panel"
    (is (= "Belegnummer" (:label (get (es/prop-display-of :rechnungsposition) :nr))))
    (is (some? (:fmt (get (es/prop-display-of :rechnungsposition) :betrag))))
    (is (= :right (:align (get (es/prop-display-of :rechnungsposition) :betrag))))))

(deftest editor-mutations-bump-the-preview-key
  (swap! es/ui assoc :draft {:app :kosten :descriptor {:name "Kosten" :layout tree-fixture}
                    :data data-fixture :tree tree-fixture
                    :cursor [] :version 1 :loading? false :error nil})
  (let [v0 (:version @es/draft)]
    (es/put-param! [:items 0] :label "Neu")
    (is (= "Neu" (get-in (es/tree) [:items 0 :label])))
    (is (> (:version @es/draft) v0))))

(deftest palette-is-searchable-and-grouped
  (testing "no query lists every entry, grouped, structure first"
    (let [groups (cat/palette)]
      (is (= :struktur (:group (first groups))))
      (is (= (count cat/palette-entries) (reduce + (map (comp count :items) groups))))))
  (testing "a query narrows by label, id and group"
    (is (= [:header-text :divider :kpi-row :kpi :property-cards :badge :empty-state
            :markdown]
           (mapv :id (:items (first (cat/palette "kennzahl"))))))
    (is (= {:aktion [:action-table] :daten [:object-table :pivot-table] :medien [:spreadsheet]}
           (into {} (map (juxt :group #(mapv :id (:items %)))) (cat/palette "tabelle")))
        "die lesende Tabelle unter Daten, die schreibende unter Aktion, das Tabellenblatt unter Medien")
    (is (empty? (cat/palette "gibtesnicht")))))

(deftest instrumentation-round-trips-to-the-tree
  (let [inst (cat/instrument tree-fixture)]
    (testing "every node carries the path that addresses it in the RAW tree"
      (doseq [{:keys [path node]} (cat/flatten-tree inst)]
        (is (= path (get node cat/path-key)))
        (is (some? (cat/node-at tree-fixture path)))))
    (testing "the raw tree is untouched"
      (is (nil? (get-in tree-fixture [:items 0 cat/path-key]))))))

(deftest preview-is-clickable-only-under-the-inspector
  (render/inspect! {:select identity :selected [:items 0]})
  (testing "an instrumented tree renders click targets"
    (let [html (rds/render-to-static-markup
                [app/render-data-app
                 {:name "K" :layout (cat/instrument tree-fixture)} data-fixture])]
      (is (str/includes? html "capp-inspect"))
      (is (str/includes? html "capp-inspect--sel"))))
  (render/inspect! nil)
  (testing "without the inspector — every shipped app — nothing is clickable"
    (let [html (rds/render-to-static-markup
                [app/render-data-app {:name "K" :layout tree-fixture} data-fixture])]
      (is (not (str/includes? html "capp-inspect"))))))

(deftest undo-redo-walks-the-history
  (swap! es/ui assoc :draft (merge ed/blank
                          {:app :kosten :descriptor {:name "Kosten" :layout tree-fixture}
                           :data data-fixture :tree tree-fixture :version 1}))
  (is (not (es/can-undo?)))

  (testing "eine Tipp-Phrase ist EIN Schritt — sonst frisst ein Titel den Verlauf"
    (es/put-param! [:items 0] :label "Eins")
    (es/put-param! [:items 0] :label "Zwei")
    (is (es/can-undo?))
    (is (= 1 (count (:past @es/draft))))
    (es/undo!)
    (is (= "Summe" (get-in (es/tree) [:items 0 :label]))
        "ein Undo nimmt das ganze Wort zurueck, nicht den letzten Tastendruck")
    (is (not (es/can-undo?)))
    (es/redo!)
    (is (= "Zwei" (get-in (es/tree) [:items 0 :label]))))

  (testing "das Verlassen des Feldes beendet die Phrase"
    (es/mode! :commit)
    (es/put-param! [:items 0] :label "Drei")
    (is (= 2 (count (:past @es/draft))) "zwei Phrasen sind zwei Schritte")
    (es/undo!)
    (is (= "Zwei" (get-in (es/tree) [:items 0 :label]))))

  (testing "ein anderes Feld beginnt ebenfalls einen neuen Schritt"
    (let [n (count (:past @es/draft))]
      (es/put-param! [:items 0] :fmt :auto)
      (is (= (inc n) (count (:past @es/draft))))))

  (testing "eine wirkungslose Aenderung waechst den Verlauf nicht"
    (es/mode! :commit)
    (let [n (count (:past @es/draft))]
      (es/put-param! [:items 0] :fmt :auto)
      (is (= n (count (:past @es/draft)))))))

(deftest templates-start-from-something-valid
  (doseq [{:keys [id tree]} cat/templates]
    (testing (str "template " id)
      (is (app/data-layout? tree))
      (is (= :container (cat/node-type tree)))))
  (testing "a template is reachable by id"
    (is (some? (cat/template :dashboard)))))

(deftest palette-drops-a-block-where-the-cursor-is
  (swap! es/ui assoc :draft (merge ed/blank
                          {:app nil :descriptor {:name "N"} :data {}
                           :tree (cat/template :leer) :version 1}))
  (testing "into the selected container when it takes children"
    (is (= (es/cursor) (:parent (es/container-target))))
    (is (= :many (:holds (es/container-target))))
    (es/add-at-cursor! :kpi)
    (is (= :kpi (get-in (es/tree) [:items 0 :widget])))
    (is (= [:items 0] (es/cursor))))
  (testing "beside a selected LEAF, not swallowed by it"
    (is (not= (es/cursor) (:parent (es/container-target)))
        "ein Blatt nimmt nichts hinein, also daneben")
    (is (= [] (:parent (es/container-target))))
    (es/add-at-cursor! :badge)
    (is (= 2 (count (:items (es/tree)))))))

(defn- fresh-draft! []
  (swap! es/ui assoc :draft {:app :kosten :file {:name :kosten :layout tree-fixture}
                    :descriptor {:name "Kosten" :layout tree-fixture}
                    :data data-fixture :tree tree-fixture :saved nil
                    :cursor [] :version 1 :loading? false :error nil :staged nil
                    :past [] :future []}))

(deftest undo-covers-the-module-half-too
  (fresh-draft!)
  (testing "a module edit is a history step — undo must not walk past it into
            an older layout edit"
    (es/put-param! [:items 0] :label "Eins")
    (es/set-object-types! [:rechnung])
    (is (= [:rechnung] (es/object-types)))
    (es/undo!)
    (is (empty? (es/object-types)) "undo skipped the module edit")
    (is (= "Eins" (get-in (es/tree) [:items 0 :label]))
        "undo reached back into the layout instead of the change just made"))
  (testing "and redo walks the same steps forward"
    (es/redo!)
    (is (= [:rechnung] (es/object-types)))
    (es/undo!)
    (es/undo!)
    (is (= "Summe" (get-in (es/tree) [:items 0 :label])))
    (is (not (es/can-undo?))))
  (testing "a module edit that changes nothing does not grow the history"
    (es/set-object-types! [:rechnung])
    (let [n (count (:past @es/draft))]
      (es/set-object-types! [:rechnung])
      (is (= n (count (:past @es/draft)))))))

(deftest undo-is-idempotent-at-the-bottom
  (fresh-draft!)
  (es/put-param! [:items 0] :label "Eins")
  (es/undo!)
  (let [after @es/draft]
    (is (not (es/can-undo?)))
    (es/undo!)
    (es/undo!)
    (is (= (dissoc after :version) (dissoc @es/draft :version))
        "an undo past the bottom of the history still changed the draft")))

(deftest undo-restores-the-cursor-with-the-tree
  (fresh-draft!)
  (es/select! [:items 0])
  (es/add! [] :items :kpi)
  (is (not= [:items 0] (es/cursor)) "add! selects the node it added")
  (es/undo!)
  (is (= [:items 0] (es/cursor))
      "undo left the cursor on a path the restored tree no longer has")
  (is (some? (es/node-at-cursor))))

(deftest opening-an-app-starts-a-fresh-history
  (fresh-draft!)
  (es/put-param! [:items 0] :label "Eins")
  (is (es/can-undo?))
  (testing "the reload case — a new draft cannot undo into the previous one"
    (es/new-app! :leer)
    (is (not (es/can-undo?)))
    (is (not (es/can-redo?)))))

(deftest the-same-clicks-from-the-same-state-end-in-the-same-draft
  (let [run (fn []
              (fresh-draft!)
              (es/put-param! [:items 0] :label "Eins")
              (es/set-object-types! [:rechnung])
              (es/put-param! [:items 0] :label "Zwei")
              (es/undo!)
              (es/undo!)
              (es/redo!)
              (dissoc @es/draft :version))]
    (is (= (run) (run)))))

(defn -main [& _] (run-tests))
