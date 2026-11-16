(ns sutra-ui.apps.object-view.matrix
  (:require [clojure.string :as str]
            [sutra-ui.ui.core :as ui]
            [sutra.i18n.core :as i18n]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.instances :as instances]
            [sutra-ui.state.panel :as panel]
            [sutra-ui.apps.object-view.model :as ov-model]
            [sutra-ui.object.card :as oc]
            [sutra-ui.ui.page :as page]
            [sutra-ui.ui.table :as table]
            [sutra-ui.apps.object-view.actions :as actions]
            [sutra-ui.apps.object-view.registry :as reg]))

(defn- conns [] (:links @catalog/schema))

(defn- expand-from
  [c]
  (let [ep (get-in @catalog/schema-derived [:endpoint-types (:name c) :from])]
    (cond
      (set? ep) ep
      (seq ep)  (set ep)
      :else
      (let [from   (:from c)
            ifaces (:interfaces @catalog/schema)]
        (if (contains? ifaces from)
          (->> (:object-types @catalog/schema)
               (keep (fn [[obj-kw spec]]
                       (when (some #{from} (:implements spec))
                         obj-kw)))
               set)
          #{from})))))

(defn- outgoing-otm
  [owner-type]
  (filter #(and (= (:cardinality %) :one-to-many)
                (contains? (expand-from %) owner-type))
          (conns)))

(defn- cell-edges-for
  [child-type other-type]
  (vec
   (filter (fn [c]
             (and (or (and (= (:from c) child-type) (= (:to c) other-type))
                      (and (= (:from c) other-type) (= (:to c) child-type)))
                  (#{:many-to-many :many-to-one :one-to-many :one-to-one}
                   (:cardinality c))))
           (conns))))

(defn- peer-edges-for
  [child-type cell-edges]
  (let [in-cell? (into #{} (map :name) cell-edges)]
    (vec
     (filter (fn [c]
               (and (= (:from c) child-type)
                    (= (:to c) child-type)
                    (#{:one-to-one :many-to-one} (:cardinality c))
                    (not (in-cell? (:name c)))))
             (conns)))))

(defn- candidate-other-types [child-type children-conn-kw]
  (->> (conns)
       (keep (fn [c]
               (when (not= (:name c) children-conn-kw)
                 (cond
                   (= (:from c) child-type) (:to c)
                   (= (:to c)   child-type) (:from c)))))
       (filter #(contains? (:object-types @catalog/schema) %))
       distinct
       vec))

(defn- candidate-owner-types []
  (->> (conns)
       (filter #(= (:cardinality %) :one-to-many))
       (mapcat expand-from)
       (filter #(contains? (:object-types @catalog/schema) %))
       distinct sort vec))

(defn- marker-edge
  [edges]
  (let [mtos (filter #(= (:cardinality %) :many-to-one) edges)]
    (when (= 1 (count mtos)) (:name (first mtos)))))

(defn- resolved-config-from-index
  [mx {:keys [owner-type children-conn other-type]}]
  (let [otm   (when owner-type (get-in mx [:outgoing-otm-by-owner owner-type] []))
        cc    (or children-conn
                  (when (= 1 (count otm)) (:name (first otm))))
        cspec (first (filter #(= (:name %) cc) otm))
        ct    (:child-type cspec)
        ots   (when (and ct cc) (get-in mx [:other-options-by-child-conn [ct cc]] []))
        ot    (or other-type (when (= 1 (count ots)) (first ots)))
        pair  (when (and ct ot) (get-in mx [:cell-edges-by-pair [ct ot]]))
        edges (or (:edges pair) [])
        marker (:marker pair)
        in-cell? (into #{} (map :name) edges)
        peers  (when ct
                 (vec (remove #(in-cell? (:name %))
                              (get-in mx [:peer-edges-by-child ct] []))))]
    {:owner-type       owner-type
     :children-conn    cc
     :child-type       ct
     :other-type       ot
     :cell-edges       edges
     :marker           marker
     :peer-edges       (or peers [])
     :owner-options    (vec (:owner-options mx))
     :children-options (mapv :name otm)
     :other-options    (vec ots)}))

(defn- resolved-config-fallback
  [{:keys [owner-type children-conn other-type]}]
  (let [otm   (when owner-type (outgoing-otm owner-type))
        cc    (or children-conn
                  (when (= 1 (count otm)) (:name (first otm))))
        cspec (first (filter #(= (:name %) cc) otm))
        ct    (:to cspec)
        ots   (when ct (candidate-other-types ct cc))
        ot    (or other-type (when (= 1 (count ots)) (first ots)))
        edges (when (and ct ot) (cell-edges-for ct ot))
        marker (when (seq edges) (marker-edge edges))
        peers  (when ct (peer-edges-for ct (or edges [])))]
    {:owner-type       owner-type
     :children-conn    cc
     :child-type       ct
     :other-type       ot
     :cell-edges       edges
     :marker           marker
     :peer-edges       (or peers [])
     :owner-options    (candidate-owner-types)
     :children-options (mapv :name otm)
     :other-options    (or ots [])}))

(defn resolved-config
  [partial]
  (if-let [mx (get @catalog/schema-derived :matrix)]
    (resolved-config-from-index mx partial)
    (resolved-config-fallback partial)))

(defn- sel-owner-id []
  (ov-model/focused-instance-id))

(defn- links
  []
  (ov-model/complete-links-of (keys @panel/complete-instances)))

(defn- owner-instances [owner-type]
  (ov-model/complete-rows-of owner-type))

(defn- children-ids [owner-id children-conn]
  (->> (links)
       (filter #(and (= (:link-type-ref %) children-conn)
                     (= (:from-id %) owner-id)))
       (mapv :to-id)))

(defn- child-rows [child-type child-id-set]
  (->> (ov-model/complete-rows-of child-type)
       (filter #(contains? child-id-set (:id %)))))

(defn- other-rows [other-type]
  (ov-model/complete-rows-of other-type))

(defn- has-children-conns [child-type]
  (filter #(and (= (:cardinality %) :one-to-many)
                (= (:to %) child-type))
          (conns)))

(defn- interface-owners-index
  [child-type]
  (let [hc-types  (into #{} (map :name) (has-children-conns child-type))
        owner-of  (->> (links)
                       (filter #(contains? hc-types (:link-type-ref %)))
                       (map (juxt :to-id :from-id))
                       (into {}))
        owner-meta
        (into {}
              (for [[type-kw rows] (:objects @instances/instances)
                    r rows]
                [(:id r) {:owner-type type-kw
                          :owner-name (catalog/instance-display-name
                                       (assoc r :object-type type-kw))}]))]
    (into {}
          (for [[child-id owner-id] owner-of
                :let [m (get owner-meta owner-id)]
                :when m]
            [child-id (assoc m :owner-id owner-id)]))))

(defn- peer-of [peer-edge-kw row-id]
  (->> (links)
       (filter #(= (:link-type-ref %) peer-edge-kw))
       (some (fn [c]
               (cond
                 (= (:from-id c) row-id) (:to-id c)
                 (= (:to-id c)   row-id) (:from-id c)
                 :else nil)))))

(defn- natural-key
  [s]
  (let [s (str s)]
    (loop [s s, acc []]
      (if (empty? s)
        acc
        (if-let [m (re-find #"^(\d+)(.*)$" s)]
          (recur (nth m 2) (conj acc [0 (js/parseInt (nth m 1) 10)]))
          (let [[lit rest] (or (some-> (re-find #"^(\D+)(.*)$" s) rest)
                               [s ""])]
            (recur rest (conj acc [1 lit]))))))))

(defn- bulk-number-field [fields]
  (let [str-fields (filter #(= :string (:type %)) fields)
        by-name    #(some (fn [f] (when (= (:name f) %) f)) str-fields)
        suffix-id  (some (fn [f]
                           (when (str/ends-with? (name (:name f)) "-id") f))
                         str-fields)]
    (:name (or (by-name :port-id) suffix-id
               (by-name :id) (by-name :number) (by-name :name)
               (first str-fields)))))

(defn- edge-index
  [cell-edges child-id-set other-id-set]
  (let [edge-names (into #{} (map :name) cell-edges)]
    (reduce
     (fn [m c]
       (let [t (:link-type-ref c)
             f (:from-id c)
             to (:to-id c)]
         (cond
           (not (contains? edge-names t)) m
           (and (contains? child-id-set f) (contains? other-id-set to))
           (assoc m [f to t] (:id c))
           (and (contains? child-id-set to) (contains? other-id-set f))
           (assoc m [to f t] (:id c))
           :else m)))
     {}
     (links))))

(defn- non-marker-edges [edges marker]
  (->> edges
       (map :name)
       (remove #(= % marker))
       vec))

(defn- current-cell-set [edge-idx child-id other-id edges]
  (into #{} (for [e edges
                  :let [k (:name e)]
                  :when (contains? edge-idx [child-id other-id k])]
              k)))

(defn- short-label [edge-kw]
  (let [n (name edge-kw)]
    (cond
      (str/starts-with? n "tagged")   "T"
      (str/starts-with? n "untagged") "U"
      :else                            (str/upper-case (subs n 0 1)))))

(defn- edge-label [edge-kw]
  (or (not-empty (:label (catalog/link-side edge-kw :to))) (name edge-kw)))

(defn- cell-glyph [cell-set marker]
  (cond
    (empty? cell-set) [:span {:class "u-muted-light"} "·"]
    :else
    (let [marker? (and marker (contains? cell-set marker))
          shown   (->> cell-set (remove #(= % marker)) sort)]
      [:span {:class (cond-> "explorer-matrix__glyph"
                       marker? (str " explorer-matrix__glyph--marker"))}
       (if (seq shown)
         (str/join "+" (map short-label shown))
         (short-label marker))])))

(defn- attr-fields [type-kw]
  (get-in @catalog/schema [:object-types type-kw :fields]))

(defn- value-cell
  [row attr-name]
  (let [v (get-in row [:values attr-name])]
    [:span {:class "explorer-matrix__cell"}
     (cond
       (nil? v)     [:span {:class "u-muted-lighter"} "—"]
       (boolean? v) (i18n/t (if v :ui/ox-yes :ui/ox-no))
       (keyword? v) (name v)
       :else        (str v))]))

(defn- picker-row [label v opts on-change disabled? & [label-of]]
  [:div {:class "u-mb-10"}
   [:label {:class "explorer-matrix__picker-label"} label]
   [ui/html-select {:value (or (some-> v name) "")
                    :disabled disabled?
                    :on-change (fn [e]
                                 (let [s (.. e -target -value)]
                                   (on-change (when (seq s) (keyword s)))))
                    :fill true}
    [:option {:value ""} "—"]
    (for [k opts]
      ^{:key (str k)}
      [:option {:value (name k)} ((or label-of name) k)])]])

(defn matrix-config-form
  [config on-change]
  (let [{:keys [owner-type children-conn other-type
                children-options other-options]}
        (resolved-config config)]
    (if (nil? owner-type)
      [:div {:class "explorer-matrix__config-error"}
       (i18n/t :ui/ov-matrix-pick-type)]
      [:div
       [:div {:class "explorer-matrix__config-owner"}
        (i18n/t :ui/ov-matrix-for {:type (oc/type-label owner-type)})]
       [picker-row (i18n/t :ui/ov-matrix-rows) children-conn children-options
        #(on-change (-> config
                        (assoc :children-conn %)
                        (dissoc :other-type)))
        false
        #(:label (catalog/link-side % :to))]
       [picker-row (i18n/t :ui/ov-matrix-cols) other-type other-options
        #(on-change (assoc config :other-type %))
        (nil? children-conn)
        #(oc/type-label % true)]])))

(defn- peer-cell
  [peer-edge-kw row owners-idx]
  (let [current-peer (peer-of peer-edge-kw (:id row))
        meta*        (when current-peer (get owners-idx current-peer))]
    [:div {:class "explorer-matrix__peer-cell"}
     (if meta*
       [oc/object-link (:owner-type meta*) (:owner-id meta*)]
       [:span {:class "u-muted-lighter"} "—"])]))

(defn- grid-pane [{:keys [owner-type children-conn child-type other-type
                          cell-edges marker peer-edges owner-id]}]
  (let [owner-id  (or owner-id (sel-owner-id))
        owner-row (first (filter #(= (:id %) owner-id)
                                 (owner-instances owner-type)))
        child-fields (attr-fields child-type)
        other-fields (attr-fields other-type)
        other-title  (some-> (get-in @catalog/schema [:object-types other-type :title-key]) keyword)
        child-title  (or (some-> (get-in @catalog/schema [:object-types child-type :title-key])
                                 keyword) :name)
        row-fields   (remove #(= (:name %) child-title) child-fields)
        child-ids (set (children-ids owner-id children-conn))
        num-field (bulk-number-field child-fields)
        row-rows  (vec (sort-by #(natural-key (get-in % [:values num-field]))
                                (child-rows child-type child-ids)))
        col-rows  (vec (other-rows other-type))
        child-id-set (into #{} (map :id) row-rows)
        other-id-set (into #{} (map :id) col-rows)
        edge-idx  (edge-index cell-edges child-id-set other-id-set)
        owners-idx   (when (seq peer-edges) (interface-owners-index child-type))]
    [:<>
     [page/toolbar
      [:span (str (oc/type-label owner-type) " ·")]
      (when owner-id [oc/object-link owner-type owner-id {:row owner-row}])
      [:span {:class (ui/cls "text-muted")} (i18n/t :ui/ov-matrix-read-only)]]
     (when (and (seq cell-edges) (nil? marker)
                (some #(= (:cardinality %) :many-to-one) cell-edges))
       [ui/callout {:intent :warning :compact true} (i18n/t :ui/ov-matrix-no-marker)])
     [:p {:class (ui/cls "text-muted")}
      (i18n/t :ui/ov-matrix-legend
              {:links (str/join " · " (for [e (non-marker-edges cell-edges marker)]
                                        (str (short-label e) " = " (edge-label e))))
               :marker (if marker
                         (i18n/t :ui/ov-matrix-legend-marker
                                 {:link (str (short-label marker) " = " (edge-label marker))})
                         "")})]
     [table/object-table
      {:rows        row-rows
       :frozen-cols 1
       :empty       {:title (i18n/t :ui/ov-matrix-empty
                                    {:rows (oc/type-label child-type true)})}
       :cols
       (concat
        [{:label  (oc/type-label child-type true)
          :render #(vector oc/object-link child-type (:id %) {:row % :icon? false})}]
        (for [f row-fields]
          {:label (actions/field-label f) :render #(vector value-cell % (:name f))})
        (for [pe peer-edges]
          {:label  (edge-label (:name pe))
           :render #(vector peer-cell (:name pe) % owners-idx)})
        (for [c col-rows]
          {:label  [:div {:class "explorer-matrix__col-head"}
                    [oc/object-link other-type (:id c) {:row c :icon? false}]
                    (for [f other-fields :when (not= (:name f) other-title)]
                      ^{:key (:name f)} [value-cell c (:name f)])]
           :align  :center
           :render #(cell-glyph (current-cell-set edge-idx (:id %) (:id c) cell-edges)
                                marker)}))}]]))

(defn- empty-state [msg]
  [ui/empty-state {:icon :grid-view :title msg}])

(defn root
  [config]
  (let [r        (resolved-config (select-keys config [:owner-type :children-conn :other-type]))
        {:keys [owner-type child-type other-type cell-edges]} r
        owner-id (or (:id config) (sel-owner-id))]
    (cond
      (nil? owner-type)
      [empty-state (i18n/t :ui/ov-matrix-incomplete)]

      (nil? child-type)
      [empty-state (i18n/t :ui/ov-matrix-incomplete)]

      (nil? other-type)
      [empty-state (i18n/t :ui/ov-matrix-incomplete)]

      (empty? cell-edges)
      [empty-state (i18n/t :ui/ov-matrix-no-links {:rows (oc/type-label child-type true)
                                                   :cols (oc/type-label other-type true)})]

      (nil? owner-id)
      [empty-state (i18n/t :ui/property-list-no-object)]

      :else
      [grid-pane (assoc r :owner-id owner-id)])))

(page/view! :matrix root)

(defmethod reg/config-form :matrix [_ config on-change]
  [matrix-config-form config on-change])

(defmethod reg/default-config :matrix [_] {})
