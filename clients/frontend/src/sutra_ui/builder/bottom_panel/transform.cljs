(ns sutra-ui.builder.bottom-panel.transform
  (:require [sutra-ui.ui.core :as ui]
            [clojure.string :as str]
            [sutra.i18n.core :as i18n]
            [sutra.types.languages :as langs]
            [sutra.types.transform-io :as tio]
            [sutra-ui.builder.bottom-panel.common :as common]
            [sutra-ui.builder.bottom-panel.grid :as grid]
            [sutra-ui.ui.table :as table]
            [sutra-ui.ui.busy :as busy]
            [sutra-ui.editor.code :as ce]
            [sutra-ui.resource.core :as res]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.panel :as panel]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.ui.style :as style]))

(defn- selected-output
  [t-kw outs]
  (or (get @panel/panel-selected-output t-kw)
      (some-> outs first :name)))

(defn data-tab [t-kw]
  (let [spec     (common/transform-spec t-kw)
        outs     (tio/outputs-of spec)
        selected (selected-output t-kw outs)
        output   (some #(when (= selected (:name %)) %) outs)
        {:keys [loading? error rows preview? input-caps]}
        (panel/panel :transform-rows selected)
        attrs    (common/match-columns [:preview t-kw] (common/output-attrs output rows)
                                       :attribute-name)]
    [:div {:class "panel-tab__body"}
     [common/output-pills t-kw outs selected]
     [:div {:class "panel-toolbar"}
      [:div {:class "colormenu__mode-label u-muted"}
       (cond loading? [busy/inline (i18n/t :ui/loading)]
             error    (i18n/t :ui/error-prefix {:error error})
             preview? (str (i18n/t :ui/preview-nothing-written)
                           (when (seq input-caps)
                             (i18n/t :ui/truncated-input-suffix
                                     {:list (str/join ", " (map #(str (or (:alias %) (:name %) %))
                                                                input-caps))})))
             :else    (str "t_" (some-> selected name)))]
      [ui/tooltip {:content (i18n/t :ui/preview-transform-hint)}
       [ui/button {:small true :on-click #(panel/preview-panel-transform! t-kw selected)}
        (i18n/t :ui/preview-run)]]
      [ui/tooltip
       {:content (i18n/t :ui/reload-built-table {:table (str "t_" (some-> selected name))})}
       [ui/button {:small true :on-click #(panel/load-panel! :transform-rows selected)}
        (i18n/t :ui/refresh)]]]
     (cond
       (and (not loading?) (empty? rows))
       [:div {:class "u-italic u-muted"}
        (i18n/t :ui/rows-none-in-table {:table (str "t_" (some-> selected name))})]

       (seq rows)
       [grid/preview-grid {:columns  (mapv (fn [{nm :attribute-name t :attribute-type}]
                                             {:name nm :type t})
                                           attrs)
                           :rows     rows
                           :value-of (fn [row c] (get (:values row) c))
                           :lead     {:label "id" :of #(common/short-uuid (:id %))}}])]))

(defn- open-source-bar
  [t-kw producer?]
  (let [repo (some-> (get-in @catalog/canonical-schema [:transforms t-kw :repository]) name)]
    [:div {:class "panel-toolbar"}
     (when producer?
       [ui/tooltip
        {:content (i18n/t :ui/open-producer-hint {:name (name t-kw)})}
        [ui/button {:small true :on-click #(panel/open-bottom-panel! :transform t-kw)}
         (str (i18n/t :ui/open-producer) " · " (name t-kw))]])
     [ui/tooltip {:content (if repo
                             (i18n/t :ui/open-repos-named {:repo repo})
                             (i18n/t :ui/open-repos))}
      [ui/button {:small true :on-click #(catalog/open-resource! :transforms t-kw)}
       (str (res/open-label :transforms t-kw) (when repo (str " · " repo)))]]
     [ui/tooltip {:content (i18n/t :ui/edit-transform-hint)}
      [ui/button {:small true :on-click #(shell/open-edit-node-modal!
                                          {:kind :transform :name t-kw})}
       (i18n/t :ui/edit-code)]]]))

(defn- inputs-section
  [inputs]
  [:<>
   [:h4 {:class "panel-section__title--sm u-muted"}
    (str (i18n/t :ui/inputs) " · " (count inputs))]
   (if (empty? inputs)
     [:div {:class "panel-section__empty u-muted"}
      (i18n/t :ui/no-inputs)]
     [common/inputs-table inputs])])

(defn code-tab
  ([t-kw] (code-tab t-kw false))
  ([t-kw producer?]
   (let [spec     (common/transform-spec t-kw)
         language (:language spec)
         compute? (or (langs/query-language? language) (= :python language))
         outs     (tio/outputs-of spec)]
     [:div {:class "panel-tab__body--mono-sm"}
      [open-source-bar t-kw producer?]
      (if compute?
        [:<>
         [inputs-section (:inputs spec)]
         [:h4 {:class "panel-section__title--sm u-muted"}
          (i18n/t (if (langs/query-language? language) :ui/sql-query :ui/python-code))]
         [:div {:class "u-mb-14"}
          [common/code-view language (get spec (langs/body-field language))]]
         [:h4 {:class "panel-section__title--sm u-muted"}
          (str (i18n/t :ui/outputs) " · " (count outs))]
         (if (empty? outs)
           [:div {:class "u-italic u-muted"}
            (i18n/t :ui/no-outputs)]
           [common/outputs-table outs])]
        [:<>
         [:div {:class "panel-code__note--mono u-muted"}
          (i18n/t :ui/mapping-transform-readonly)]
         [common/mapping-pre spec]])])))

(defn about-tab [t-kw]
  (let [spec     (common/transform-spec t-kw)
        in       (:input spec)
        outs     (tio/outputs-of spec)
        in-label (case (:kind in)
                   :datasource  (str "datasource " (some-> (:datasource in) name))
                   :transform   (str "transform " (some-> (:transform in) name))
                   :object-type (str "object-type " (some-> (:object-type in) name))
                   "—")]
    [:div {:class "panel-tab__body"}
     [:div {:class "panel-meta--mb"}
      [:div [:div {:class style/label} (i18n/t :ui/name)]
       [:div {:class "panel-meta__value"}
        (some-> t-kw name)]]
      [common/copy-rid (:id spec)]
      [:div [:div {:class style/label} (i18n/t :ui/input)]
       [:div {:class "panel-meta__value"} in-label]]
      [:div [:div {:class style/label} (i18n/t :ui/outputs)]
       [:div {:class "panel-meta__value--plain"} (count outs)]]]
     (when (seq outs)
       [:<>
        [:h4 {:class "panel-section__title u-muted"}
         (i18n/t :ui/outputs)]
        [common/outputs-table outs]])]))

(defn as-of-data
  [t-kw selected-run]
  (let [spec         (common/transform-spec t-kw)
        outs         (tio/outputs-of spec)
        selected-out (selected-output t-kw outs)
        output       (some #(when (= selected-out (:name %)) %) outs)
        commit       (:snapshot-id selected-run)
        {:keys [loading? error rows]}
        (when (and commit selected-out)
          (panel/panel :transform-data-at commit selected-out))]
    [:div
     [common/output-pills t-kw outs selected-out]
     [:div {:class "panel-asof__status u-muted"}
      (cond
        (not commit)  (i18n/t :ui/pick-run-for-rows)
        loading?      [busy/inline (i18n/t :ui/loading)]
        error         (i18n/t :ui/error-prefix {:error error})
        :else         (i18n/t :ui/rows-in-table-as-of
                              {:n (count rows)
                               :table (str "t_" (some-> selected-out name))
                               :commit (subs (str commit) 0 7)}))]
     (cond
       (or (not commit) error) nil

       (and (not loading?) (empty? rows))
       [:div {:class "u-italic u-muted"}
        (i18n/t :ui/rows-none-at-this-commit)]

       (seq rows)
       [table/data-table
        {:scroll?    true
         :rows       rows
         :row-key    #(str (:id %))
         :cols       (cons {:key    "id" :label "id" :mono? true
                            :render #(vector :span {:title (str (:id %))}
                                             (common/short-uuid (:id %)))}
                           (for [{nm :attribute-name} (common/output-attrs output rows)]
                             {:key    (str nm) :label (name nm)
                              :render #(common/cell-text (get (:values %) nm))}))}])]))

(defn as-of-code
  [t-kw selected-run]
  (let [commit (:snapshot-id selected-run)
        {:keys [loading? error spec]}
        (when commit (panel/panel :transform-spec-at commit t-kw))
        language (some-> (:language spec) name)
        query?   (langs/query-language? (keyword language))
        compute? (or query? (= "python" language))
        body-of  (fn [s] (cond query?                (:query s)
                               (= "python" language) (:code s)))
        as-of    (body-of spec)
        current  (body-of (common/transform-spec t-kw))
        changed? (and as-of current (not= as-of current))]
    [:div
     [:div {:class "panel-asof__status u-muted"}
      (cond
        (not commit) (i18n/t :ui/pick-run-for-code)
        loading?     [busy/inline (i18n/t :ui/loading)]
        error        (i18n/t :ui/error-prefix {:error error})
        (not spec)   (i18n/t :ui/transform-absent-at-commit)
        :else        (i18n/t :ui/spec-as-of {:commit (subs (str commit) 0 7)}))]
     (cond
       (or (not commit) error (not spec)) nil

       compute?
       [:<>
        [inputs-section (:inputs spec)]
        [:h4 {:class "panel-section__title--sm u-muted"}
         (str (i18n/t (if query? :ui/sql-query :ui/python-code))
              (when changed? (i18n/t :ui/changes-since-run-suffix)))]
        [:div {:class "u-m-0"}
         (if changed?
           ^{:key (str "diff-" commit)}
           [ce/code-diff {:baseline   as-of
                          :value      current
                          :language   language
                          :min-height "120px"}]
           [common/code-view language as-of])]]

       :else
       [common/mapping-pre spec])]))
