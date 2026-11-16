(ns sutra-ui.apps.lineage.lineage-smoke-test
  (:require [cljs.test :refer [deftest is testing run-tests use-fixtures]]
            [clojure.string :as str]
            [reagent.core :as r]
            [reagent.dom.server :as rds]
            [sutra.viz.palette :as palette]
            [sutra-ui.state.canvas :as canvas]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.colors :as colors]
            [sutra-ui.state.data :as data]
            [sutra-ui.state.graph :as graph]
            [sutra-ui.state.instances :as instances]
            [sutra-ui.state.lineage :as lineage]
            [sutra-ui.state.lineage-status :as lineage-status]
            [sutra-ui.state.panel :as panel]
            [sutra-ui.state.selection :as selection]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.apps.lineage.graph :as lgraph]
            [sutra-ui.apps.lineage.model :as lineage-model]
            [sutra-ui.graph-editor :as ge]
            [sutra-ui.graph-editor.legend :as legend]
            [sutra-ui.builder.bottom-panel :as bp]
            [sutra-ui.builder.bottom-panel.history :as bp-history]
            [sutra.i18n.core :as i18n]
            [sutra.wire.transport :as tx]
            ["reactflow" :refer [ReactFlowProvider]]))

(def ^:private transforms
  {:pull-mailbox
   {:transform-name :pull-mailbox :language :python
    :external-systems [:mailbox]
    :outputs [{:kind :dataset :name :raw_messages    :dataset :raw_messages}
              {:kind :dataset :name :raw_attachments :dataset :raw_attachments}]}
   :classify-events
   {:transform-name :classify-events :language :python
    :external-systems [:xai]
    :inputs {:mail {:kind :dataset :dataset :raw_messages}
             :grok {:kind :language-model}}
    :outputs [{:kind :object-type :object-type :auftrags-ereignis}]}
   :link-ereignis-bestellung
   {:transform-name :link-ereignis-bestellung :language :pg-sql
    :inputs {:a {:kind :object-type :object-type :auftrags-ereignis}
             :b {:kind :object-type :object-type :bestellung}}
    :outputs [{:kind :link-type :link-type :ereignis-bestellung}]}})

(def ^:private datasets
  {:raw_messages
   {:id "ri.sutra.main.dataset.raw_messages" :dataset-name :raw_messages
    :description "One row per pulled email."
    :columns [{:attribute-name :message_id :attribute-type :string}
              {:attribute-name :amount :attribute-type [:decimal 12 2]}]}
   :raw_attachments
   {:id "ri.sutra.main.dataset.raw_attachments" :dataset-name :raw_attachments
    :columns [{:attribute-name :att_id :attribute-type :string}]}})

(def ^:private canonical
  {:transforms transforms
   :datasets   datasets
   :object-types {:auftrags-ereignis {} :bestellung {}}
   :link-types {:ereignis-bestellung {}}
   :applications {:beschaffung-app {:application-name :beschaffung-app
                                    :object-type :auftrags-ereignis
                                    :layout  {:object-types [:auftrags-ereignis]}
                                    :status  :published :icon "🛒"}}})

(def ^:private view-shape
  {:objects {:auftrags-ereignis [{:name :id :type :uuid}
                                {:name :betrag :type [:decimal 12 2]}]
             :bestellung []}
   :object-types {:auftrags-ereignis {:fields []} :bestellung {:fields []}}
   :links [{:name :ereignis-bestellung :from :auftrags-ereignis :to :bestellung}]
   :interfaces {}})

(def ^:private served-graph
  {:pull-mailbox
   {:inputs [] :external-systems [:mailbox]
    :outputs [{:kind :dataset :name :raw_messages}
              {:kind :dataset :name :raw_attachments}]}
   :classify-events
   {:inputs [{:kind :dataset :name :raw_messages}
             {:kind :language-model :name :grok}]
    :external-systems [:xai]
    :outputs [{:kind :object-type :name :auftrags-ereignis}]}
   :link-ereignis-bestellung
   {:inputs [{:kind :object-type :name :auftrags-ereignis}
             {:kind :object-type :name :bestellung}]
    :external-systems []
    :outputs [{:kind :link-type :name :ereignis-bestellung}]}})

(defn- install-fixture! []
  (reset! catalog/canonical-schema canonical)
  (reset! lineage/lineage-graph served-graph)
  (reset! catalog/schema view-shape)
  (reset! data/datasources {:mailbox {:type :imap} :xai {:type :rest}})
  (reset! lineage-status/lineage-status {:nodes {}})
  (reset! data/dataset-preview-data nil)
  (reset! canvas/current-view
          {:id "ri.sutra.main.view.smoke" :name :smoke :mode :lineage
           :visible-types {:transforms #{:pull-mailbox :classify-events
                                         :link-ereignis-bestellung}
                           :object-types #{:auftrags-ereignis :bestellung}}})
  (swap! shell/ui-state assoc
         :nav :lineage-canvas :mode :lineage
         :open-relations #{}
         :color-mode :resource-kind :detail-modal nil :bottom-panel nil))

(use-fixtures :each {:before install-fixture!})

(defn- nodes [] (lgraph/lineage->rf-nodes @catalog/schema {} false false))
(defn- edges [] (lgraph/lineage->rf-edges @catalog/schema))
(defn- node-ids [] (set (map :id (nodes))))

(deftest only-the-data-node-types
  (testing ""
    (let [types (set (map :type (nodes)))]
      (is (every? #{"datasource" "dataset" "objectType"} types)
          (str "unexpected node type(s): " (pr-str types)))
      (is (not (contains? types "transform")))
      (doseq [n (nodes)]
        (is (= (:id n) (get-in n [:data :expandId]))
            (str "node " (:id n) " has no expand arrows")))
      (is (not (contains? types "sqlTransform")))
      (is (not (contains? types "pythonTransform")))
      (is (not (contains? types "application")))
      (is (not (contains? types "actionType")))
      (is (not (contains? types "function"))))))

(deftest every-rendered-kind-is-in-the-palette-and-the-legend
  (testing "a node kind the canvas draws has a palette entry — accent and
            label. Nodes no longer caption their kind, so a kind missing here
            would render as an unlabelled, uncoloured card."
    (doseq [k legend/lineage-kinds]
      (let [{:keys [label color]} (get palette/node-kinds k)]
        (is (string? label) (str k " has no legend label"))
        (is (string? color) (str k " has no accent colour")))))
  (testing "every kind drawn by the shared card chrome has a glyph. Object
            type is the sole exception — so it must NOT declare one."
    (doseq [k [:datasource :dataset :transform :application
               :action :function]]
      (is (string? (colors/node-kind-icon k)) (str k " has no glyph")))
    (is (nil? (colors/node-kind-icon :object-type))
        "object-type must not claim a kind glyph — each type brings its own"))
  (testing "accents are distinct: kind is read by colour first, so two kinds
            sharing one accent would be indistinguishable at a glance"
    (let [colors (map :color (vals palette/node-kinds))]
      (is (= (count colors) (count (set colors))))))
  (testing "every rf node's kind resolves to a palette accent — this is the
            drift the transform node had: it painted itself and went missing
            from the palette entirely"
    (let [kind-of {"datasource"  :datasource "dataset" :dataset
                   "objectType"  :object-type "application" :application
                   "transform"   :transform}]
      (doseq [n (nodes)
              :let [k (kind-of (:type n))]
              :when k]
        (is (string? (colors/node-kind-color k))
            (str "node " (:id n) " (" (:type n) ") has no palette accent"))))))

(defn- render-node
  [rf-type data]
  (let [component (get ge/node-types rf-type)]
    (rds/render-to-static-markup
     [(fn []
        (r/create-element
         ReactFlowProvider #js {}
         (r/create-element component #js {:data (clj->js data)})))])))

(deftest node-carries-its-kind-as-colour-and-glyph-not-a-caption
  (testing "the kind is no longer spelled out in a caption bar — the legend
            says it once instead"
    (doseq [[rf-type kind data] [["datasource" :datasource {:label "mailbox"}]
                                 ["dataset"    :dataset    {:label "raw_messages" :cols 3}]]]
      (let [html (render-node rf-type data)
            {:keys [label color icon]} (get palette/node-kinds kind)]
        (is (not (re-find (re-pattern (str ">" label "<")) html))
            (str rf-type " still captions its kind: " label))
        (is (re-find (re-pattern (str "(?i)" color)) html)
            (str rf-type " does not paint its palette accent " color))
        (when (= :datasource kind)
          (is (str/includes? html icon)
              (str rf-type " does not draw its kind glyph " icon)))
        (is (str/includes? html (:label data))
            (str rf-type " lost its title")))))
  (testing "the Data Lineage card: a dataset is one flat block in its colour, an
            action a black band with a hammer over a white name band, an object
            type its colour over a black name band"
    (is (re-find #"(?i)background:\s*#f0b726" (render-node "dataset" {:label "d"})))
    (let [html (render-node "actionType" {:label "freigeben"})]
      (is (str/includes? html "action-node__band"))
      (is (str/includes? html "<svg"))
      (is (str/includes? html "freigeben")))
    (let [html (render-node "objectType" {:label "rechnung" :paint "#d23f31"})]
      (is (re-find #"(?i)graph-node__symbol\"[^>]*background:\s*#d23f31" html))
      (is (str/includes? html "graph-node__name")))))

(deftest lineage-affordances-render-only-where-they-apply
  (testing "the link icon appears on an object-type WITH relations and is
            absent otherwise — the ontology canvas passes no relCount at all,
            and an icon there would toggle a lens that canvas doesn't have"
    (is (str/includes? (render-node "objectType"
                                    {:label "auftrags-ereignis" :relCount 1 :relOpen false})
                       "graph-node__rel"))
    (is (not (str/includes? (render-node "objectType" {:label "auftrags-ereignis"})
                            "graph-node__rel")))
    (is (not (str/includes? (render-node "objectType"
                                         {:label "x" :relCount 0 :relOpen false})
                            "graph-node__rel"))))
  (testing "an open axis is visually distinct from an available one"
    (is (str/includes? (render-node "objectType"
                                    {:label "x" :relCount 2 :relOpen true})
                       "graph-node__rel--on"))
    (is (not (str/includes? (render-node "objectType"
                                         {:label "x" :relCount 2 :relOpen false})
                            "graph-node__rel--on"))))
  (testing "the same object-type component is on
            the ontology canvas, where there is no lineage to expand into"
    (doseq [t ["objectType" "dataset" "datasource"]]
      (let [html (render-node t {:label "x" :expandId (str t "/x")})]
        (is (str/includes? html "graph-node__expand--in") (str t " has no ‹ arrow"))
        (is (str/includes? html "graph-node__expand--out") (str t " has no › arrow")))
      (is (not (str/includes? (render-node t {:label "x"}) "graph-node__expand"))
          (str t " draws expand arrows off the lineage canvas"))))
  (testing "the 'defines an object type' indicator rides definesOts"
    (is (str/includes? (render-node "dataset" {:label "raw_messages" :definesOts 1})
                       "graph-node__defines"))
    (is (not (str/includes? (render-node "dataset" {:label "raw_attachments" :definesOts 0})
                            "graph-node__defines")))))

(deftest legend-rows-render-for-every-paint-mode
  (testing "each Color-by mode yields a non-empty key on both canvases —
            an empty legend leaves the colours on screen unexplained"
    (doseq [mode [:resource-kind :build-status :build-duration :last-built
                  :model-usage :out-of-date :data-health :schedule-count]
            kinds [legend/lineage-kinds]]
      (swap! shell/ui-state assoc :color-mode mode)
      (let [html (rds/render-to-static-markup [legend/legend kinds])]
        (is (string? html) (str "legend blew up in mode " mode))
        (is (re-find #"lineage-legend__row" html)
            (str "no legend rows in mode " mode)))))
  (swap! shell/ui-state assoc :color-mode :resource-kind))

(defn- stub-canvas!
  [boxes]
  (reset! instances/rf-instance
          #js {:getNodes
               (fn []
                 (clj->js (for [{:keys [id x y w h]} boxes]
                            {:id id :position {:x x :y y}
                             :width (or w 180) :height (or h 60)})))}))

(deftest align-moves-the-selection-and-nothing-else
  (testing "aligning left puts every SELECTED node on the leftmost edge and
            leaves the unselected one where it was"
    (stub-canvas! [{:id "dataset/raw_messages" :x 100 :y 10}
                   {:id "dataset/raw_attachments" :x 340 :y 90}
                   {:id "auftrags-ereignis" :x 700 :y 200}])
    (selection/select-nodes! #{"dataset/raw_messages" "dataset/raw_attachments"})
    (lineage-model/align-selection! :left)
    (is (= 100 (:x (get @canvas/node-positions :dataset/raw_messages))))
    (is (= 100 (:x (get @canvas/node-positions :dataset/raw_attachments))))
    (is (nil? (get @canvas/node-positions :auftrags-ereignis))
        "an unselected node must not be moved by Align"))
  (testing "align needs two nodes — one node has nothing to align TO"
    (reset! canvas/node-positions {})
    (stub-canvas! [{:id "dataset/raw_messages" :x 100 :y 10}])
    (selection/select-nodes! #{"dataset/raw_messages"})
    (lineage-model/align-selection! :left)
    (is (empty? @canvas/node-positions)))
  (reset! instances/rf-instance nil))

(deftest clean-counts-what-it-would-drop
  (testing "the Clean rows are sized before they run — :unselected spares the
            selection, :all takes the graph"
    (stub-canvas! [{:id "dataset/raw_messages" :x 0 :y 0}
                   {:id "auftrags-ereignis" :x 0 :y 0}
                   {:id "dataset/lonely" :x 0 :y 0}])
    (selection/select-nodes! #{"auftrags-ereignis"})
    (is (= 3 (count (lineage-model/clean-candidates :all))))
    (is (= #{"dataset/raw_messages" "dataset/lonely"}
           (set (lineage-model/clean-candidates :unselected))))
    (testing "a node no edge touches is what 'unconnected' means — the two
              wired ones are not candidates"
      (is (= ["dataset/lonely"] (lineage-model/clean-candidates :isolated)))))
  (reset! instances/rf-instance nil))

(deftest removing-a-node-actually-removes-it
  (testing "a node also renders because an in-scope TRANSFORM names it, so
            un-pinning alone left it on the canvas — Remove has to take the
            transforms that touch it too"
    (is (contains? (node-ids) "dataset/raw_messages") "fixture sanity")
    (lineage-model/remove-nodes-from-lineage-scope! ["dataset/raw_messages"])
    (is (not (contains? (node-ids) "dataset/raw_messages"))
        "the removed dataset came back through its producing transform"))
  (testing "the transform's OTHER endpoints stay — the user removed one node,
            not its neighbourhood"
    (is (contains? (node-ids) "dataset/raw_attachments")
        "a sibling output of the same transform was swept away")
    (is (contains? (node-ids) "auftrags-ereignis")
        "a downstream object-type was swept away"))
  (testing "adding it back UN-hides it — otherwise every later seed and expand
            is silently refused for that node"
    (lineage-model/seed-lineage-ref! [:dataset :raw_messages])
    (is (contains? (node-ids) "dataset/raw_messages"))
    (is (empty? (canvas/lineage-hidden-ids))))
  (testing "an edge whose endpoint is off the graph is not drawn"
    (lineage-model/remove-nodes-from-lineage-scope! ["dataset/raw_messages"])
    (let [ids (node-ids)]
      (doseq [e (edges)]
        (is (and (contains? ids (:source e)) (contains? ids (:target e)))
            (str "edge " (:id e) " dangles off the canvas"))))))

(deftest undo-restores-the-graph-the-user-built
  (testing "removing a node is undoable — the node back, redo forward"
    (lineage-model/remove-nodes-from-lineage-scope! ["auftrags-ereignis"])
    (is (not (contains? (node-ids) "auftrags-ereignis")))
    (is (lineage-model/can-undo?))
    (graph/graph-undo!)
    (is (contains? (node-ids) "auftrags-ereignis") "undo did not put the node back")
    (is (lineage-model/can-redo?))
    (graph/graph-redo!)
    (is (not (contains? (node-ids) "auftrags-ereignis"))))
  (testing "a fresh mutation invalidates the redo future"
    (graph/graph-undo!)
    (lineage-model/remove-nodes-from-lineage-scope! ["bestellung"])
    (is (not (lineage-model/can-redo?)))))

(deftest selection-histogram-describes-the-set-not-a-node
  (testing "with more than one node selected the panel counts VALUES across
            the selection"
    (selection/select-nodes! #{"dataset/raw_messages" "dataset/raw_attachments"
                               "auftrags-ereignis"})
    (let [facets (into {} (lineage-model/selection-histogram))
          kinds  (into {} (for [[v ids] (get facets "Kind")] [v (count ids)]))]
      (is (= {"dataset" 2 "object-type" 1} kinds))
      (is (= ["dataset" "object-type"] (mapv first (get facets "Kind")))
          "buckets are ordered by size, so the dominant value reads first")))
  (testing "one node is not a histogram — that is the properties table"
    (selection/select-nodes! #{"auftrags-ereignis"})
    (is (nil? (lineage-model/selection-histogram))))
  (testing "Copy names hands over the full list, comma-separated"
    (selection/select-nodes! #{"dataset/raw_messages" "auftrags-ereignis"})
    (is (= "auftrags-ereignis, dataset/raw_messages" (lineage-model/selection-names))))
  (selection/clear-selection!))

(deftest a-scoped-dataset-renders-even-with-no-transform-io
  (testing "a dataset pinned into scope is a node, whatever the transforms do.
            An object-type's BACKING dataset (prod: `rechnung`) is not any
            transform's :dataset input or output, so a datasets-derived-only
            inventory could never show it — clicking it in 'Add to graph' was a
            silent no-op."
    (let [scope {:transforms #{} :datasets #{:rechnung}}
          nodes (lgraph/lineage->rf-nodes @catalog/schema {} false false)]
      (is (some? (canvas/lineage-scope)))
      (reset! canvas/current-view
              (assoc @canvas/current-view :visible-types scope))
      (let [ids (set (map :id (lgraph/lineage->rf-nodes @catalog/schema {} false false)))]
        (is (contains? ids "dataset/rechnung")
            "a pinned dataset must render with no transforms in scope")
        (is (seq nodes) "fixture sanity: the default scope renders nodes")))))

(deftest seed-search-never-truncates-a-whole-kind
  (testing "the seed box shows only the first handful, so candidates must be
            interleaved across kinds. Prod: `rechnung` matches 13 datasets and
            4 object-types; a kind-ordered list put every dataset first, so
            (take 12) hid EVERY object-type and the kind looked missing."
    (let [many-datasets (into {} (for [i (range 13)]
                                   [(keyword (str "rechnung-ds-" i)) {}]))]
      (swap! catalog/canonical-schema assoc :datasets many-datasets)
      (swap! catalog/schema assoc :objects {:rechnung {} :rechnungsposition {}})
      (let [top   (take 12 (lineage-model/lineage-seed-candidates "rechnung"))
            kinds (set (map :kind top))]
        (is (contains? kinds :object-type)
            "object-types truncated away by datasets again")
        (is (contains? kinds :dataset)))))
  (testing "an exact name wins, whatever its kind — you typed it"
    (is (= {:kind :object-type :kw :rechnung}
           (select-keys (first (lineage-model/lineage-seed-candidates "rechnung")) [:kind :kw]))))
  (reset! catalog/canonical-schema {})
  (reset! catalog/schema {}))

(deftest a-dataset-does-not-wear-a-transforms-status
  (testing "a dataset reads dataset/<name>, NOT transform/<name>. The backend
            emits an entry per dataset (orchestration.lineage/status); the FE
            mapped :dataset onto the transform token, so a dataset node wore a
            SAME-NAMED transform's build status while the real dataset entries
            went unread."
    (reset! lineage-status/lineage-status
            {:nodes {"dataset/raw_messages"   {:build-status "succeeded"}
                     "transform/raw_messages" {:build-status "failed"}}})
    (is (= "succeeded" (:build-status (lineage-status/node-status :dataset :raw_messages)))
        "the dataset picked up the same-named transform's status")
    (is (= "failed" (:build-status (lineage-status/node-status :transform :raw_messages))))
    (swap! shell/ui-state assoc :color-mode :build-status)
    (is (= (get colors/build-status-colors "succeeded")
           (lgraph/node-paint :dataset :raw_messages))
        "under build-status coloring the dataset must paint its OWN status")
    (is (= (get colors/build-status-colors "failed")
           (lgraph/node-paint :transform :raw_messages))))
  (swap! shell/ui-state assoc :color-mode :resource-kind)
  (reset! lineage-status/lineage-status {:nodes {}}))

(deftest drag-and-autolayout-agree-on-the-position-store
  (testing "a canvas's drag writer must match its auto-layout writer, or a
            dragged node is read back from an atom nobody wrote. Lineage
            dragged into @instance-positions while its layout wrote
            @node-positions."
    (doseq [mode [:ontology :lineage]]
      (is (= canvas/update-node-position! (canvas/position-update-fn mode))
          (str mode " drags into the wrong position store"))
      (is (= canvas/apply-ontology-positions! (canvas/positions-apply-fn mode))))
    (is (= canvas/update-instance-position! (canvas/position-update-fn :instances)))
    (is (= canvas/apply-instance-positions! (canvas/positions-apply-fn :instances)))))

(deftest real-datasets-are-nodes
  (testing "transform :dataset outputs surface as real dataset nodes"
    (is (contains? (node-ids) "dataset/raw_messages"))
    (is (contains? (node-ids) "dataset/raw_attachments")))
  (testing "object-types render, transforms do not"
    (is (contains? (node-ids) "auftrags-ereignis"))
    (is (not (some #(re-find #"^transform/" %) (node-ids))))))

(deftest source-lands-a-dataset
  (testing "a data source that lands a dataset renders and connects to it"
    (is (contains? (node-ids) "datasource/mailbox"))
    (is (some #(and (= "datasource/mailbox" (:source %))
                    (= "dataset/raw_messages" (:target %)))
              (edges))))
  (testing "a data source NEVER connects to an object-type"
    (is (every? (fn [e]
                  (or (not (re-find #"^datasource/" (:source e)))
                      (re-find #"^dataset/" (:target e))))
                (edges))
        "found a datasource→non-dataset edge"))
  (testing "an LLM/service grant (:xai) that lands no dataset is dropped"
    (is (not (contains? (node-ids) "datasource/xai")))
    (is (not (some #(= "datasource/xai" (:source %)) (edges))))))

(deftest source-auto-revealed-for-downstream-only-scope
  (testing "seeding only a downstream consumer still auto-reveals the dataset's
            source-landing producer — the source node + source→dataset edge"
    (reset! canvas/current-view
            {:id "ri.sutra.main.view.smoke2" :name :smoke2 :mode :lineage
             :visible-types {:transforms   #{:classify-events}
                             :object-types #{:auftrags-ereignis}}})
    (is (contains? (node-ids) "dataset/raw_messages")
        "the consumed dataset renders")
    (is (contains? (node-ids) "datasource/mailbox")
        "its source is auto-revealed even though the import wasn't seeded")
    (is (some #(and (= "datasource/mailbox"     (:source %))
                    (= "dataset/raw_messages"   (:target %)))
              (edges))
        "and the source→dataset edge is drawn")))

(deftest transform-is-the-edge
  (testing "the dataset a transform reads connects to the object-type it writes"
    (is (some #(and (= "dataset/raw_messages" (:source %))
                    (= "auftrags-ereignis" (:target %)))
              (edges)))))

(deftest relation-axis-is-gated-behind-the-link-icon
  (testing "closed by default: ontology relations do NOT clutter the data flow"
    (is (not (lineage/relations-open? :auftrags-ereignis)))
    (is (not-any? #(= "link" (get-in % [:data :kind])) (edges))
        "a relation edge was drawn without anyone asking for it"))
  (testing "the object-type node offers the axis — count, and closed state"
    (let [n (->> (nodes) (filter #(= "auftrags-ereignis" (:id %))) first)]
      (is (= 1 (get-in n [:data :relCount]))
          ":auftrags-ereignis has exactly one link-type in the fixture")
      (is (false? (get-in n [:data :relOpen])))))
  (testing "opening it draws the object-type ↔ object-type edge, named after
            the link-type, on its OWN edge kind — never a lineage hop"
    (graph/toggle-relations! :auftrags-ereignis)
    (is (lineage/relations-open? :auftrags-ereignis))
    (let [e (->> (edges) (filter #(= "link" (get-in % [:data :kind]))) first)]
      (is (some? e) "no relation edge after opening the axis")
      (is (= "auftrags-ereignis" (:source e)))
      (is (= "bestellung" (:target e)))
      (is (= "relationEdge" (:type e))
          "a relation must not reuse the data-flow edge kind")
      (is (= "ereignis-bestellung" (get-in e [:data :label]))
          "a relation carries its link-type name; a data-flow edge does not")))
  (testing "the far end reports open too — the axis is symmetric, so clicking
            either type's icon reveals the same relation"
    (is (= 1 (->> (nodes) (filter #(= "bestellung" (:id %))) first
                  :data :relCount))))
  (testing "closing it removes the edge again"
    (graph/toggle-relations! :auftrags-ereignis)
    (is (not-any? #(= "link" (get-in % [:data :kind])) (edges)))))

(deftest relation-edges-never-dangle
  (testing "a relation whose far end is off-canvas is dropped, not drawn into
            the void — the same endpoint gate the data-flow edges pass"
    (reset! canvas/current-view
            {:id "ri.sutra.main.view.smoke-rel" :name :smoke-rel :mode :lineage
             :visible-types {:transforms   #{:classify-events}
                             :object-types #{:auftrags-ereignis}}})
    (swap! shell/ui-state assoc :open-relations #{:auftrags-ereignis})
    (is (not (contains? (node-ids) "bestellung"))
        "fixture guard: the far end is genuinely off-canvas here")
    (is (not-any? #(= "link" (get-in % [:data :kind])) (edges)))))

(deftest dataset-carries-the-defines-an-object-type-indicator
  (testing ""
    (is (= [:auftrags-ereignis]
           (lineage/ots-defined-by-dataset @catalog/schema :raw_messages)))
    (is (= 1 (->> (nodes) (filter #(= "dataset/raw_messages" (:id %))) first
                  :data :definesOts))))
  (testing "a dataset that defines nothing carries no indicator"
    (is (= [] (lineage/ots-defined-by-dataset @catalog/schema :raw_attachments)))
    (is (= 0 (->> (nodes) (filter #(= "dataset/raw_attachments" (:id %))) first
                  :data :definesOts)))))

(deftest applications-are-data-lineage-artifacts
  (testing "Data Lineage draws artifacts: an
            application in the view's scope renders as a node"
    (reset! canvas/current-view
            {:id "ri.sutra.main.view.smoke3" :name :smoke3 :mode :lineage
             :visible-types {:transforms   #{:classify-events}
                             :object-types #{:auftrags-ereignis}
                             :applications #{:beschaffung-app}}})
    (is (contains? (node-ids) "application/beschaffung-app")))
  (testing "the artifact card names its kind in a coloured band over its name"
    (let [html (render-node "application" {:label "Beschaffung" :status "published"})]
      (is (str/includes? html "artifact-node__band"))
      (is (str/includes? html "Beschaffung"))))
  (testing "a writeback dataset is a flat card that says what it is"
    (let [html (render-node "writeback" {:label "rechnung"})]
      (is (str/includes? html "writeback-node"))
      (is (str/includes? html "rechnung")))))

(deftest node-indicators-follow-data-lineage
  (testing "open issues, syncs and trashed ride on the node"
    (let [html (render-node "dataset" {:label "d" :issues 2 :syncs 1 :trashed true})]
      (is (str/includes? html "graph-node__indicator--issues"))
      (is (str/includes? html "! 2"))
      (is (str/includes? html "⇄"))
      (is (str/includes? html "is-trashed"))))
  (testing "no indicator where nothing is open, synced or trashed"
    (is (not (str/includes? (render-node "dataset" {:label "d" :issues 0 :syncs 0})
                            "graph-node__indicators"))))
  (testing "an unstructured dataset has a dashed border"
    (is (re-find #"border-style:\s*dashed"
                 (render-node "dataset" {:label "m" :unstructured true})))))

(deftest autolayout-targets-node-positions
  (testing "ontology + lineage route auto-layout to @node-positions, not
            @instance-positions (the keyword-vs-string mode regression)"
    (is (= canvas/apply-ontology-positions! (canvas/positions-apply-fn :lineage)))
    (is (= canvas/apply-ontology-positions! (canvas/positions-apply-fn :ontology)))
    (is (= canvas/apply-instance-positions! (canvas/positions-apply-fn :instances))))
  (testing "end-to-end: an auto-layout result for a lineage node lands where the
            node builder reads its position back from"
    (reset! canvas/node-positions {})
    ((canvas/positions-apply-fn :lineage) {"dataset/raw_messages" {:x 700 :y 420}})
    (let [n (->> (lgraph/lineage->rf-nodes @catalog/schema @canvas/node-positions
                                           false false)
                 (filter #(= "dataset/raw_messages" (:id %)))
                 first)]
      (is (= {:x 700 :y 420} (:position n))))))

(deftest unplaced-nodes-are-reported-as-unplaced
  (testing "a node the canvas never placed has NO position — that nil is what
            makes the engine seed it instead of stacking it on the origin"
    (reset! canvas/node-positions {})
    (reset! canvas/instance-positions {})
    (let [nodes #js [#js {:id "transform/classify-events"}
                     #js {:id "dataset/raw_messages"}
                     #js {:id "auftrags-ereignis"}]]
      (is (= #{"transform/classify-events" "dataset/raw_messages" "auftrags-ereignis"}
             (canvas/unpositioned-ids :lineage nodes)))
      ((canvas/positions-apply-fn :lineage) {"transform/classify-events" {:x 40 :y 40}})
      (is (= #{"dataset/raw_messages" "auftrags-ereignis"}
             (canvas/unpositioned-ids :lineage nodes)))
      (is (= {:x 40 :y 40} (canvas/position-of :lineage "transform/classify-events"))))))

(deftest saved-layout-round-trips-for-every-node-kind
  (testing "a stored position comes back for EVERY kind the canvas can drag —
            the old apply-layout! knew three, so a dataset/action/function
            position was written and then never read back"
    (reset! canvas/node-positions {})
    (reset! canvas/instance-positions {})
    (canvas/apply-layout-entries!
     [{:node-kind :object-type :node-key "auftrags-ereignis" :x 1 :y 2}
      {:node-kind :dataset     :node-key "raw_messages"     :x 3 :y 4}
      {:node-kind :transform   :node-key "classify-events"  :x 5 :y 6}
      {:node-kind :action      :node-key "freigeben"        :x 7 :y 8}
      {:node-kind :function    :node-key "marge"            :x 9 :y 10}
      {:node-kind :application :node-key "werft"      :x 11 :y 12}
      {:node-kind :object      :node-key "abc-123"          :x 13 :y 14}])
    (is (= {:x 1 :y 2}   (canvas/position-of :ontology "auftrags-ereignis")))
    (is (= {:x 3 :y 4}   (canvas/position-of :lineage "dataset/raw_messages")))
    (is (= {:x 5 :y 6}   (canvas/position-of :lineage "transform/classify-events")))
    (is (= {:x 7 :y 8}   (canvas/position-of :lineage "action/freigeben")))
    (is (= {:x 9 :y 10}  (canvas/position-of :lineage "function/marge")))
    (is (= {:x 11 :y 12} (canvas/position-of :lineage "application/werft")))
    (is (= {:x 13 :y 14} (canvas/position-of :instances "abc-123")))))

(deftest layout-target-falls-back-to-the-mode-canvas
  (testing "with no graph open a position write still has somewhere to go —
            without it a layout lived for exactly one page life"
    (swap! shell/ui-state assoc :current-view-id nil :mode :lineage)
    (is (= "mode:lineage" (canvas/layout-target)))
    (swap! shell/ui-state assoc :current-view-id "ri.graph.main.abc")
    (is (= "ri.graph.main.abc" (canvas/layout-target)))
    (swap! shell/ui-state assoc :current-view-id nil)))

(def ^:private kind->id
  {:object-type :auftrags-ereignis
   :transform   :classify-events
   :datasource  :mailbox
   :dataset     :raw_messages})

(deftest bottom-panel-renders-every-kind
  (testing "schema-panel-kinds is the single source of truth and covers :dataset"
    (is (contains? panel/schema-panel-kinds :dataset)))
  (doseq [kind panel/schema-panel-kinds]
    (testing (str "bottom panel renders for kind " kind " (no missing case clause)")
      (let [id (kind->id kind)]
        (is (some? id) (str "fixture has no id for kind " kind))
        (doseq [tab [:preview :history :code :health :about :timeline]]
          (swap! shell/ui-state assoc
                 :detail-modal {:kind kind :id id}
                 :bottom-panel {:tab tab :collapsed? false})
          (is (string? (rds/render-to-string [bp/bottom-panel]))
              (str "render threw for kind " kind " tab " tab)))))))

(deftest a-parametric-column-type-survives-the-preview-grid
  (testing "prod dataset columns carry PARAMETRIC value-types ([:decimal 14 2],
            [:array :string]). The grid header rendered the type with plain
            `name`, which throws \"Doesn't support name: [:decimal 14 2]\" and
            drops the WHOLE lineage canvas into the error boundary — the
            preview tab is inside canvas-view. Rendering must route through
            catalog/value-type-label instead."
    (reset! data/dataset-preview-data
            {:columns [{:attribute-name :message_id :attribute-type :string}
                       {:attribute-name :betrag     :attribute-type [:decimal 14 2]}
                       {:attribute-name :marge      :attribute-type [:decimal 6 2]}
                       {:attribute-name :tags       :attribute-type [:array :string]}]
             :rows    [{:message_id "m1" :betrag 12 :marge 3 :tags "a"}]})
    (swap! shell/ui-state assoc
           :detail-modal {:kind :dataset :id :raw_messages}
           :bottom-panel {:tab :preview :collapsed? false})
    (let [html (rds/render-to-string [bp/bottom-panel])]
      (is (str/includes? html "decimal(14,2)"))
      (is (str/includes? html "decimal(6,2)"))
      (is (str/includes? html "array")
          "an [:array T] column type must render too, not throw")
      (is (str/includes? html "string"))))
  (reset! data/dataset-preview-data nil))

(deftest bottom-panel-tab-labels-are-one-vocabulary
  (testing "Preview · History · Code ·
            Data health · Build timeline. This panel used to call Preview 'Data'
            on three kinds out of four and abbreviate the last two. The labels
            come from the bundle now, so the assertion asks the bundle too —
            pinning the English words here would make the test fail on a
            translation rather than on the regression it guards. Run in BOTH
            languages: one vocabulary has to hold in each of them."
    (doseq [locale [:de :en]]
      (i18n/set-locale! locale)
      (doseq [kind panel/schema-panel-kinds]
        (swap! shell/ui-state assoc
               :detail-modal {:kind kind :id (kind->id kind)}
               :bottom-panel {:tab :preview :collapsed? false})
        (let [html (rds/render-to-string [bp/bottom-panel])
              has? (fn [k] (str/includes? html (i18n/t k)))]
          (is (has? :ui/tab-preview)
              (str kind "/" (name locale) " does not offer a Preview tab"))
          (is (has? :ui/tab-timeline)
              (str kind "/" (name locale) " still abbreviates the build timeline"))
          (is (not (str/includes? html ">Timeline<"))
              (str kind "/" (name locale) " still shows the old 'Timeline' label"))
          (when-not (= :datasource kind)
            (is (has? :ui/data-health)
                (str kind "/" (name locale) " has no Data health tab")))
          (is (str/includes? html (str ">" (i18n/t :ui/tab-history) "<"))
              (str kind "/" (name locale) " has no History tab")))))
    (i18n/set-locale! :de)))

(deftest history-sub-tabs-cover-every-panel-kind
  (testing "every bottom-panel kind has a History sub-tab set, and every sub-tab
            it names renders — a kind that keeps the PREVIOUS kind's sub-tab is
            how a blank pane happens"
    (doseq [kind panel/schema-panel-kinds]
      (let [subs (bp-history/history-subs-of kind)]
        (is (seq subs) (str "no History sub-tabs declared for " kind))
        (doseq [[sub _] subs]
          (swap! shell/ui-state assoc
                 :detail-modal {:kind kind :id (kind->id kind)}
                 :bottom-panel {:tab :history :history-sub sub :collapsed? false})
          (is (string? (rds/render-to-string [bp/bottom-panel]))
              (str "History sub-tab " sub " threw for kind " kind))))))
  (testing "a dataset now gets the per-commit Data tab too, FIRST like the other
            kinds — it was withheld only because no read could answer 'this
            table, at that commit', which `?as-of=` now does"
    (is (= [:data :logs :files :metadata :schema :job-spec]
           (mapv first (bp-history/history-subs-of :dataset)))))
  (testing "a data source owns no transaction chain, so it gets no Files tab"
    (is (not (contains? (set (map first (bp-history/history-subs-of :datasource))) :files)))))

(defn- requested-paths
  [f]
  (let [seen (atom [])]
    (with-redefs [tx/send! (fn
                             ([method path] (swap! seen conj [method path])
                                            (js/Promise.resolve {}))
                             ([method path _] (swap! seen conj [method path])
                                              (js/Promise.resolve {})))]
      (f))
    (mapv second @seen)))

(def ^:private txn-path "/api/catalog/transactions")

(deftest a-selection-asks-only-what-its-kind-can-answer
  (testing "the transaction ledger resolves a dataset/transform name and
            redirects an object-type to its producer — those three ask for it"
    (is (= panel/transaction-chain-kinds #{:object-type :transform :dataset}))
    (doseq [[kind id] [[:object-type :auftrags-ereignis]
                       [:transform   :classify-events]
                       [:dataset     :raw_messages]]]
      (let [paths (requested-paths #(panel/open-bottom-panel! kind id))]
        (is (some #(str/includes? % (str txn-path "?dataset=" (name id))) paths)
            (str kind " no longer loads its transaction chain")))))
  (testing "a data source is a CONNECTION, not a table: asking the ledger for
            its name is the prod 404 (`?dataset=mro`), for a Files tab this
            kind does not even offer. It shows its own detail instead."
    (let [paths (requested-paths #(panel/open-bottom-panel! :datasource :mailbox))]
      (is (not-any? #(str/includes? % txn-path) paths)
          (str "datasource selection still asks the dataset ledger: " (pr-str paths)))
      (is (some #(str/includes? % "/api/datasources/mailbox/preview") paths)
          "datasource selection must still load its own staging preview")
      (is (some #(str/includes? % "/api/lineage/related") paths)
          "datasource selection must still load its related artifacts")))
  (testing "an instance or a link selection carries a ROW RID, not a node name —
            the same wrong question, one canvas over"
    (doseq [[kind id] [[:object-instance "ri.sutra.main.object.abc-123"]
                       [:link            "ri.sutra.main.link.abc-123"]]]
      (is (not-any? #(str/includes? % txn-path)
                    (requested-paths #(panel/open-bottom-panel! kind id)))
          (str kind " selection asks the dataset ledger for a RID"))))
  (testing "the gate and the Files sub-tab are one decision — a kind that shows
            the tab must load it, a kind that hides it must not ask"
    (doseq [kind panel/schema-panel-kinds]
      (is (= (contains? panel/transaction-chain-kinds kind)
             (contains? (set (map first (bp-history/history-subs-of kind))) :files))
          (str kind " disagrees with its own History sub-tabs")))))

(deftest node-columns-is-one-vocabulary
  (testing "a dataset reads its declared columns"
    (is (= ["message_id" "amount"] (catalog/node-columns :dataset :raw_messages))))
  (testing "a transform reads what it WRITES — every output's columns, so
            'which node carries att_id' reaches the transform that lands it"
    (is (= ["message_id" "amount" "att_id"]
           (catalog/node-columns :transform :pull-mailbox)))
    (testing "and each column once, though two outputs may share a name"
      (is (apply distinct? (catalog/node-columns :transform :pull-mailbox)))))
  (testing "a kind with no columns answers [], never nil — callers count it"
    (is (= [] (catalog/node-columns :datasource :mailbox)))))

(deftest find-searches-columns-as-well-as-names
  (testing "the column axis names WHY each node matched"
    (let [hits (lineage-model/lineage-column-candidates "message")]
      (is (some #(= :raw_messages (:kw %)) hits))
      (is (= ["message_id"] (:columns (first (filter #(= :raw_messages (:kw %)) hits)))))))
  (testing "an empty query matches nothing rather than the whole graph"
    (is (nil? (lineage-model/lineage-column-candidates "")))
    (is (nil? (lineage-model/lineage-column-candidates nil))))
  (testing "a column nothing declares finds nothing — not everything"
    (is (empty? (lineage-model/lineage-column-candidates "zzz-no-such-column")))))

(deftest histogram-buckets-multi-valued-facets
  (selection/select-nodes! #{"dataset/raw_messages" "dataset/raw_attachments"})
  (let [h (into {} (lineage-model/selection-histogram))]
    (testing ""
      (is (contains? h "Project"))
      (is (contains? h "Last Modified"))
      (is (contains? h "Frequent Columns")))
    (testing "Frequent Columns puts a node in EVERY one of its buckets — reading
              it as a partition would drop all but one column per node"
      (let [cols (into {} (get h "Frequent Columns"))]
        (is (= ["dataset/raw_messages"] (get cols "message_id")))
        (is (= ["dataset/raw_attachments"] (get cols "att_id")))))
    (testing "a single-valued facet still partitions"
      (is (= 1 (count (get h "Kind"))))))
  (selection/clear-selection!))

(deftest histogram-drill-down-is-two-steps
  (testing "clicking a bucket HIGHLIGHTS; only Update selection narrows — one
            step destroyed the set the histogram was describing"
    (selection/select-nodes! #{"dataset/raw_messages" "dataset/raw_attachments"})
    (lineage-model/highlight-bucket! "Kind" "dataset" ["dataset/raw_messages"])
    (is (= "dataset" (:value (lineage-model/highlighted-bucket))))
    (is (= 2 (count @selection/canvas-selection)) "highlighting must not narrow")
    (lineage-model/update-selection-to-highlight!)
    (is (= #{"dataset/raw_messages"} @selection/canvas-selection))
    (is (nil? (lineage-model/highlighted-bucket)))
    (selection/clear-selection!))
  (testing "clicking the active bucket again clears the highlight"
    (lineage-model/highlight-bucket! "Kind" "dataset" ["a"])
    (lineage-model/highlight-bucket! "Kind" "dataset" ["a"])
    (is (nil? (lineage-model/highlighted-bucket)))))

(deftest categorical-color-modes-paint-and-key-themselves
  (testing "every categorical mode answers a LABEL for every node kind, so no
            mode can leave a kind unpainted"
    (doseq [[mode _] colors/categorical-color-modes
            [kind kw] [[:dataset :raw_messages] [:transform :classify-events]
                       [:object-type :auftrags-ereignis] [:datasource :mailbox]]]
      (let [v (colors/categorical-value mode kind kw)]
        (is (string? v) (str mode " gave no label for " kind "/" kw))
        (is (seq v)     (str mode " gave a blank label for " kind "/" kw)))))
  (testing "the paint routes through the registry rather than falling back to kind"
    (swap! shell/ui-state assoc :color-mode :compute)
    (is (= (colors/categorical-color "Postgres SQL")
           (lgraph/node-paint :transform :link-ereignis-bestellung)))
    (is (= (colors/categorical-color "Python")
           (lgraph/node-paint :transform :classify-events)))
    (is (not= (lgraph/node-paint :transform :classify-events)
              (lgraph/node-paint :transform :link-ereignis-bestellung))
        "two engines must not share one swatch")
    (swap! shell/ui-state assoc :color-mode :resource-kind))
  (testing "a colour is stable for a label, so the legend cannot drift from the node"
    (is (= (colors/categorical-color "Python") (colors/categorical-color "Python")))))

(deftest the-build-derived-modes-actually-distinguish-nodes
  (reset! lineage-status/lineage-status
          {:nodes {"transform/fast"   {:build-status "succeeded" :duration-ms 200
                                       :last-build "2999-01-01T00:00:00Z"
                                       :model-usage {:attempted 4 :successful 4
                                                     :rate-limited 0 :failed 0}}
                   "transform/slow"   {:build-status "succeeded" :duration-ms 600000
                                       :last-build "2000-01-01T00:00:00Z"
                                       :model-usage {:attempted 4 :successful 0
                                                     :rate-limited 4 :failed 0}}
                   "transform/absent" {:build-status "never"}}})
  (testing "build duration separates a 200 ms job from a ten-minute one"
    (swap! shell/ui-state assoc :color-mode :build-duration)
    (is (not= (lgraph/node-paint :transform :fast) (lgraph/node-paint :transform :slow)))
    (is (= colors/build-status-none (lgraph/node-paint :transform :absent))
        "no measured duration is 'no value', never the fastest bucket"))
  (testing "last built separates a fresh node from a stale one"
    (swap! shell/ui-state assoc :color-mode :last-built)
    (is (not= (lgraph/node-paint :transform :fast) (lgraph/node-paint :transform :slow)))
    (is (= colors/build-status-none (lgraph/node-paint :transform :absent))))
  (testing "model usage separates a working node from one whose calls are refused"
    (swap! shell/ui-state assoc :color-mode :model-usage)
    (is (not= (lgraph/node-paint :transform :fast) (lgraph/node-paint :transform :slow)))
    (is (= "active"   (lgraph/model-usage-grade {:attempted 4 :successful 4})))
    (is (= "refused"  (lgraph/model-usage-grade {:attempted 4 :successful 0 :rate-limited 4})))
    (is (= "degraded" (lgraph/model-usage-grade {:attempted 4 :successful 3 :rate-limited 1})))
    (is (nil? (lgraph/model-usage-grade nil))
        "never called a model ≠ every call refused — the reason this mode grades"))
  (testing "every status-backed mode refetches, or it paints an atom nothing filled"
    (doseq [m [:build-status :build-duration :last-built :model-usage
               :out-of-date :data-health :schedule-count]]
      (is (contains? colors/status-backed-color-modes m) (str m " would never load"))))
  (testing "a datasource keeps its kind accent under the build-time modes"
    (reset! lineage-status/lineage-status {:nodes {}})
    (doseq [m [:build-duration :last-built :out-of-date]]
      (swap! shell/ui-state assoc :color-mode m)
      (is (= (colors/node-kind-color :datasource)
             (lgraph/node-paint :datasource :mailbox))
          (str "datasource lost its accent under " m))))
  (swap! shell/ui-state assoc :color-mode :resource-kind)
  (reset! lineage-status/lineage-status {:nodes {}}))

(deftest selection-layouts-place-only-the-selection
  (testing "vertical stacks the selection at its own top-left, not at the origin"
    (reset! canvas/node-positions {})
    (canvas/apply-layout-entries!
     [{:node-kind :dataset :node-key "raw_messages"    :x 500 :y 300}
      {:node-kind :dataset :node-key "raw_attachments" :x 560 :y 700}])
    (selection/select-nodes! #{"dataset/raw_messages" "dataset/raw_attachments"})
    (doseq [how [:vertical :horizontal :hierarchical :by-level]]
      (is (nil? (graph/layout-selection! how))
          (str how " must be a no-op without a mounted canvas")))
    (is (= {:x 500 :y 300} (canvas/position-of :lineage "dataset/raw_messages")))
    (selection/clear-selection!)))

(deftest a-lake-snapshot-is-not-a-commit-ref
  (testing "the numeric token runs record is recognised as a lake snapshot"
    (is (panel/lake-snapshot-ref? "74113"))
    (is (panel/lake-snapshot-ref? 73464))
    (is (not (panel/lake-snapshot-ref? "a1b2c3d4e5f6g7h8i9j0k1l2m3n4o5p6")))
    (is (not (panel/lake-snapshot-ref? "main"))))
  (testing "no at-commit request is issued for a snapshot id"
    (let [sent (atom [])]
      (with-redefs [tx/send! (fn
                               ([_m url] (swap! sent conj url) (js/Promise.resolve {}))
                               ([_m url _o] (swap! sent conj url) (js/Promise.resolve {})))]
        (panel/load-panel! :data-at "73464" :buchungssatz)
        (panel/load-panel! :schema-at "73464" :buchungssatz))
      (is (empty? @sent)
          "a snapshot id must not reach /api/at/:commit — the backend can only refuse it")
      (is (:unaddressable? (panel/panel :data-at "73464" :buchungssatz)))
      (is (:unaddressable? (panel/panel :schema-at "73464" :buchungssatz))))))

(defn ^:export run []
  (run-tests 'sutra-ui.apps.lineage.lineage-smoke-test))

(deftest a-transform-node-id-has-neighbours
  (let [nbrs (lineage/lineage-neighbors @catalog/schema "transform/classify-events")]
    (testing "upstream is the transform that produces what it reads"
      (is (contains? (:upstream nbrs) [:transform :pull-mailbox])))
    (testing "downstream is the transform that consumes what it writes"
      (is (contains? (:downstream nbrs) [:transform :link-ereignis-bestellung])))
    (testing "and never itself — a self-edge would end the walk on level 2"
      (is (not (contains? (:upstream nbrs) [:transform :classify-events])))
      (is (not (contains? (:downstream nbrs) [:transform :classify-events]))))))

(deftest expand-levels-actually-differ
  (let [l1 (set (lineage/lineage-expansion-refs @catalog/schema "auftrags-ereignis" :upstream 1))
        l2 (set (lineage/lineage-expansion-refs @catalog/schema "auftrags-ereignis" :upstream 2))
        la (set (lineage/lineage-expansion-refs @catalog/schema "auftrags-ereignis" :upstream :all))]
    (is (contains? l1 [:transform :classify-events]))
    (is (not (contains? l1 [:transform :pull-mailbox]))
        "level 1 stops at the first hop")
    (is (contains? l2 [:transform :pull-mailbox])
        "level 2 reaches the source-landing transform behind the LLM step")
    (is (= l2 la) "and with only two hops of pipeline, 'all the way' agrees")))

(deftest between-is-the-path-and-common-is-the-intersection
  (testing "between two selected nodes: what lies ON the path, and never the
            producer of the boundary itself — that one is an already-built
            input, which is the whole point of the strategy"
    (let [b (lineage-model/lineage-between-refs @catalog/schema
                                                ["dataset/raw_messages" "auftrags-ereignis"])]
      (is (contains? b [:transform :classify-events]))
      (is (not (contains? b [:transform :pull-mailbox])))))
  (testing "unconnected nodes relate to nothing, rather than to everything"
    (is (empty? (lineage-model/lineage-between-refs @catalog/schema ["dataset/raw_attachments"
                                                                     "dataset/raw_messages"]))))
  (testing "common ancestors: what BOTH selections descend from"
    (is (= #{[:transform :pull-mailbox]}
           (lineage-model/lineage-common-refs @catalog/schema
                                              ["auftrags-ereignis" "dataset/raw_attachments"]
                                              :upstream)))))

(deftest select-parents-walks-to-node-kinds-not-to-transforms
  (testing "a node's parents are the INPUTS of the transforms that produce it —
            never the transform, which on this canvas is an edge"
    (let [ups (lineage/lineage-adjacent-node-ids @catalog/schema "auftrags-ereignis" :upstream)]
      (is (contains? ups "dataset/raw_messages"))
      (is (not-any? #(str/starts-with? % "transform/") ups))))
  (testing "and build targets drop the data source, which is not buildable"
    (is (= [:raw_messages :auftrags-ereignis]
           (lineage-model/build-targets-of ["dataset/raw_messages" "datasource/mailbox"
                                            "auftrags-ereignis"])))))

(deftest opening-a-graph-replaces-the-positions-it-does-not-merge
  (testing "a graph carries its OWN drawing. Positions live in one node-keyed
            atom while the server stores them per graph, so a merge left the
            previous canvas's coordinates standing for every node the graph
            does not mention. On a freshly saved graph — layout empty — that was
            ALL of them: nothing looked unplaced, the dagre seed never ran, and
            a 12-node view was drawn at the spacing of a 190-node one."
    (reset! canvas/node-positions {:rechnung {:x 4000.0 :y 3000.0}
                                   :transform/shape-rechnung {:x 9000.0 :y 20.0}})
    (canvas/apply-layout-entries! [] true)
    (is (empty? @canvas/node-positions)
        "an empty layout must leave the canvas unplaced, so the seed lays it out")
    (canvas/apply-layout-entries!
     [{:node-kind :object-type :node-key "rechnung" :x 10 :y 20}] true)
    (is (= {:rechnung {:x 10.0 :y 20.0}} @canvas/node-positions)
        "only what the graph itself stores")
    (canvas/apply-layout-entries!
     [{:node-kind :dataset :node-key "raw_messages" :x 1 :y 2}])
    (is (= 2 (count @canvas/node-positions))
        "the 1-arity is still the additive drag/seed write")
    (reset! canvas/node-positions {})))

(defn- pinned [] (canvas/pinned-node-ids :lineage))

(deftest undo-is-deterministic
  (graph/clear-graph-history!)
  (testing "with an empty history the tool is off and the click is inert"
    (is (not (lineage-model/can-undo?)))
    (is (not (lineage-model/can-redo?)))
    (let [before @canvas/current-view]
      (graph/graph-undo!)
      (graph/graph-undo!)
      (is (= before @canvas/current-view) "an undo with no history changed the graph")))

  (testing "clicking undo past the bottom of the stack is idempotent"
    (lineage-model/remove-nodes-from-lineage-scope! ["bestellung"])
    (is (lineage-model/can-undo?))
    (graph/graph-undo!)
    (let [after-first @canvas/current-view]
      (is (not (lineage-model/can-undo?)))
      (graph/graph-undo!)
      (graph/graph-undo!)
      (is (= after-first @canvas/current-view)
          "a repeated undo kept walking after the history was spent")))

  (testing "undo takes back the gesture that was actually made — Add datasets"
    (graph/clear-graph-history!)
    (let [before (pinned)]
      (graph/pin-nodes-on-graph! ["dataset/raw_attachments"])
      (is (contains? (pinned) "dataset/raw_attachments"))
      (is (lineage-model/can-undo?))
      (graph/graph-undo!)
      (is (= before (pinned)) "undo of a pin left the node on the graph")))

  (testing "the same gesture from the same state undoes to the same state"
    (let [run (fn []
                (graph/clear-graph-history!)
                (lineage-model/remove-nodes-from-lineage-scope! ["auftrags-ereignis"])
                (graph/graph-undo!)
                [(canvas/lineage-hidden-ids) (pinned) (canvas/current-visible-types)])]
      (is (= (run) (run))))))

(deftest undo-does-not-reach-across-a-view-switch
  (graph/clear-graph-history!)
  (lineage-model/remove-nodes-from-lineage-scope! ["auftrags-ereignis"])
  (is (lineage-model/can-undo?))
  (let [foreign {:id "ri.sutra.main.view.other" :name :other :mode :lineage
                 :visible-types {:object-types #{:bestellung}}
                 :payload {:hidden-nodes ["dataset/raw_messages"]}}]
    (reset! canvas/current-view foreign)
    (testing "a snapshot from another graph does not arm the tool"
      (is (not (lineage-model/can-undo?)) "undo stayed enabled after the graph was switched"))
    (testing "and clicking it anyway leaves the open graph alone"
      (graph/graph-undo!)
      (is (= foreign @canvas/current-view)
          "an undo wrote a foreign graph's scope into the open one")))
  (testing "with no graph open at all — the branch-checkout state — it is inert"
    (reset! canvas/current-view nil)
    (reset! canvas/node-positions {:bestellung {:x 1 :y 2}})
    (is (not (lineage-model/can-undo?)))
    (graph/graph-undo!)
    (is (= {:bestellung {:x 1 :y 2}} @canvas/node-positions)
        "an undo with no graph open still rewrote the layout"))
  (reset! canvas/node-positions {})
  (graph/clear-graph-history!))
