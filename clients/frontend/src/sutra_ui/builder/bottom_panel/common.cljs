(ns sutra-ui.builder.bottom-panel.common
  (:require [sutra-ui.ui.core :as ui]
            [sutra-ui.ui.table :as table]
            [cljs.pprint]
            [clojure.string :as str]
            [sutra.i18n.core :as i18n]
            [sutra.types.transform-io :as tio]
            [sutra-ui.builder.bottom-panel.events]
            [sutra-ui.editor.code :as ce]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.db :as db]
            [sutra-ui.state.lineage :as lineage]
            [sutra-ui.state.panel :as panel]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.ui.style :as style]))

(defn type-spec
  [kw]
  (get-in @catalog/canonical-schema [:object-types kw]))

(defn has-code? [spec] (some? (:external-source spec)))

(defn short-uuid [u]
  (let [s (str u)]
    (if (> (count s) 8) (subs s 0 8) s)))

(defn short-commit [c]
  (let [s (str c)]
    (subs s 0 (min 7 (count s)))))

(defn ts
  [t]
  (let [s (str (or t ""))]
    (cond (str/blank? s)   "—"
          (> (count s) 19) (subs s 0 19)
          :else            s)))

(defn cell-text
  [v]
  (if (nil? v) "—" (str v)))

(defn copy-rid
  [rid]
  (when rid
    [:div
     [:div {:class style/label} (i18n/t :ui/resource-id)]
     [:div {:class "u-flex-row-center-6"}
      [:code {:class "panel-rid__value u-muted"}
       rid]
      [ui/tooltip {:content (i18n/t :ui/copy-rid)}
       [ui/button
        {:small true :on-click #(some-> js/navigator .-clipboard (.writeText (str rid)))}
        (i18n/t :ui/copy)]]]]))

(defn status-pill [status]
  [ui/tag {:minimal true :round true
           :intent  (case status "succeeded" :success "failed" :danger "running" :warning nil)}
   (or status "?")])

(defn sub-tab-bar
  [{:keys [entries active pick! right]}]
  [:div {:class "panel-subtabbar"}
   [ui/tabs {:id :panel-sub :selected-tab-id active :on-change (fn [k] (pick! k))}
    (for [[k label] entries]
      [ui/tab {:id k :title (i18n/t label)}])]
   (when right [:<> [:div {:class "u-flex-1"}] right])])

(defn- column-query [k] (get-in @shell/ui-state [:bottom-panel :col-query k]))

(defn column-search
  [k]
  [ui/input-group {:value (or (column-query k) "")
                   :placeholder (i18n/t :ui/search-columns)
                   :on-change #(panel/set-panel-column-query! k (.. % -target -value))
                   :small true
                   :class "panel-colsearch"}])

(defn match-columns
  [k cols name-fn]
  (let [q (some-> (column-query k) str/trim str/lower-case)]
    (if (str/blank? q)
      (vec cols)
      (vec (filter #(str/includes? (str/lower-case (str (name-fn %))) q) cols)))))

(def ^:private rollback-kinds #{:object-type :transform :dataset})

(defn rollback-button
  [{:keys [kind node-kw commit label]}]
  (when (and commit (rollback-kinds kind))
    [ui/tooltip {:content (i18n/t :ui/rollback-hint
                                  {:kind (name kind) :commit (subs (str commit) 0 7)})}
     [:button
      {:on-click #(db/send! [:shared.panel/rollback
                             {:kind kind :node-kw node-kw :commit commit :label label}])
       :class "panel-rollback__btn"}
      (or label (i18n/t :ui/rollback-to-this))]]))

(defn force-snapshot-button
  [d-kw]
  [ui/tooltip {:content (i18n/t :ui/force-snapshot-hint)}
   [:button
    {:on-click #(db/send! [:shared.panel/force-snapshot d-kw])
     :class "panel-rollback__btn"}
    (i18n/t :ui/force-snapshot)]])

(defn code-view
  [language text]
  (let [lang (some-> language name)]
    ^{:key (str "cv-" lang)}
    [ce/code-editor {:value      (or text "(empty)")
                     :language   lang
                     :read-only? true
                     :min-height "120px"}]))

(defn mapping-pre
  [spec]
  [:pre {:class "panel-code__pre--mono"}
   (with-out-str
     (cljs.pprint/pprint
      (select-keys spec [:column-map :ops :source :primary-key
                         :from-key-column :to-key-column
                         :fk-attribute :output-columns])))])

(defn transform-spec [t-kw]
  (or (:spec (panel/panel :transform-spec t-kw))
      (get-in @catalog/canonical-schema [:transforms t-kw])))

(defn producer-of
  [kind node-kw]
  (case kind
    :transform node-kw
    :dataset   (lineage/dataset-producer-transform @catalog/schema node-kw)
    :object-type
    (some (fn [[t-kw spec]]
            (when (some #(and (= :object-type (:kind %))
                              (= node-kw (:object-type %)))
                        (tio/outputs-of spec))
              t-kw))
          (:transforms @catalog/canonical-schema))
    nil))

(defn output-attrs
  [output rows]
  (case (:kind output)
    :object-type
    (get-in @catalog/canonical-schema
            [:object-types (:object-type output) :attribute-list])

    :link-type
    [{:attribute-name :from-id :attribute-type :uuid}
     {:attribute-name :to-id   :attribute-type :uuid}]

    (when-let [r (first rows)]
      (vec (for [k (keys (:values r))]
             {:attribute-name k :attribute-type :string})))))

(defn output-pill
  [{:keys [t-kw output active?]}]
  (let [out-name (:name output)
        kind     (case (:kind output)
                   :object-type "obj"
                   :link-type   "conn"
                   "—")]
    [ui/tag {:on-click #(panel/select-panel-output! t-kw out-name)
             :intent   (when active? :primary)
             :minimal  (not active?)
             :round    true}
     (str (name out-name) " · " kind)]))

(defn output-pills
  [t-kw outs selected]
  (when (> (count outs) 1)
    [:div {:class "panel-output__pills"}
     (for [out outs]
       ^{:key (str (:name out))}
       [output-pill {:t-kw t-kw :output out
                     :active? (= (:name out) selected)}])]))

(defn output-row-label [out]
  (case (:kind out)
    :object-type (str "object-type " (some-> (:object-type out) name))
    :link-type   (str "link-type " (some-> (:link-type out) name))
    "—"))

(defn- input-row-label
  [{:keys [kind datasource table query transform object-type]}]
  (case kind
    :datasource
    (str "datasource " (some-> datasource name)
         (cond table (str " · " (i18n/t :ui/table) " " table)
               query (str " · " (i18n/t :ui/query))
               :else ""))
    :transform
    (str "transform " (some-> transform name))
    :object-type
    (str "object-type " (some-> object-type name))
    "—"))

(defn attr-table
  [head-k attrs]
  [table/data-table
   {:rows       attrs
    :row-key    #(str (:attribute-name %))
    :cols       [{:label (i18n/t head-k) :mono? true :render #(name (:attribute-name %))}
                 {:label  (i18n/t :ui/type) :mono? true
                  :render #(if-let [t (:attribute-type %)] (catalog/value-type-label t) "—")}]}])

(defn inputs-table
  [inputs]
  [table/data-table
   {:class      "u-mb-14"
    :rows       (seq inputs)
    :row-key    #(str (key %))
    :cols       [{:label (i18n/t :ui/alias) :mono? true :render #(name (key %))}
                 {:label (i18n/t :ui/source) :render #(input-row-label (val %))}]}])

(defn outputs-table
  [outs]
  [table/data-table
   {:rows       outs
    :row-key    #(str (:name %))
    :cols       [{:label (i18n/t :ui/name) :mono? true :render #(some-> (:name %) name)}
                 {:label (i18n/t :ui/target) :render output-row-label}
                 {:label  (i18n/t :ui/table) :mono? true
                  :render #(str "t_" (some-> (:name %) name))}]}])
