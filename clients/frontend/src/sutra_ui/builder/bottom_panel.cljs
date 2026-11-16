(ns sutra-ui.builder.bottom-panel
  (:require [sutra-ui.ui.remote :as remote]
            [sutra-ui.ui.overlay :as overlay]
            [sutra-ui.ui.core :as ui]
            [clojure.string :as str]
            [reagent.core :as r]
            [sutra.i18n.core :as i18n]
            [sutra.kit.remote :as rd]
            [sutra-ui.builder.bottom-panel.common :as common]
            [sutra-ui.builder.bottom-panel.dataset :as dataset]
            [sutra-ui.builder.bottom-panel.datasource :as datasource]
            [sutra-ui.builder.bottom-panel.events :as ev]
            [sutra-ui.builder.bottom-panel.health :as health]
            [sutra-ui.builder.bottom-panel.history :as history]
            [sutra-ui.builder.bottom-panel.instance :as instance]
            [sutra-ui.builder.bottom-panel.object-type :as object-type]
            [sutra-ui.builder.bottom-panel.timeline :as timeline]
            [sutra-ui.builder.bottom-panel.transform :as transform]
            [sutra-ui.health.badge :as hb]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.data :as data]
            [sutra-ui.state.db :as db]
            [sutra-ui.state.instances :as instances]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.panel :as panel]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.ui.page :as page]
            [sutra-ui.ui.style :as style]))

(defn- editable-node-name
  [_kind _type-kw _title]
  (let [draft   (r/atom nil)
        commit! (fn [k kw]
                  (let [v (some-> @draft str/trim)]
                    (reset! draft nil)
                    (when (and (seq v) (not= v (name kw)))
                      (db/send! [:shared.panel/rename k kw v]))))]
    (fn [kind type-kw title]
      [ui/tooltip {:content (i18n/t :ui/edit-node-name)}
       [:span
        [ui/editable-text
         {:value      (or @draft title)
          :min-width  120
          :on-change  #(reset! draft %)
          :on-confirm (fn [_] (commit! kind type-kw))
          :on-cancel  (fn [_] (reset! draft nil))}]]])))

(defn- panel-header [{:keys [type-kw kind spec collapsed?]}]
  (let [transform?  (= :transform kind)
        datasource? (= :datasource kind)
        instance?   (#{:object-instance :link} kind)
        active-tab  (get-in @shell/ui-state [:bottom-panel :tab])
        title       (case kind
                      :object-instance
                      (or (some-> (instances/object-by-id type-kw)
                                  catalog/instance-display-name)
                          (common/short-uuid type-kw))
                      :link
                      (or (some-> (instances/link-by-id type-kw)
                                  :link-type-ref catalog/name-of name)
                          "link")
                      (some-> type-kw name))]
    [:div {:class "panel-header"}
     [:div {:class "panel-header__title"}
      (case kind
        :transform       "ƒ "
        :datasource      "⛁ "
        :dataset         "▦ "
        :object-instance "◈ "
        :link            "→ "
        "")
      (if (and (ev/renameable-kinds kind) (keyword? type-kw))
        ^{:key (str kind "/" type-kw)}
        [editable-node-name kind type-kw title]
        title)]
     (when (and (not collapsed?) (not instance?))
       [ui/tabs {:id :panel-tab :selected-tab-id active-tab
                 :on-change (fn [k] (panel/set-panel-tab! k))}
        [ui/tab {:id :preview :title (i18n/t :ui/tab-preview)}]
        [ui/tab {:id :history :title (i18n/t :ui/tab-history)}]
        (when (or transform? (common/has-code? spec)
                  (and (#{:dataset :object-type} kind)
                       (some? (common/producer-of kind type-kw))))
          [ui/tab {:id :code :title (i18n/t :ui/tab-code)}])
        (when (= :object-type kind)
          [ui/tab {:id :edits :title (i18n/t :ui/tab-edits)}])
        (when-not datasource?
          [ui/tab {:id :health :title (i18n/t :ui/data-health)}
           (when (keyword? type-kw) [hb/health-dot kind type-kw])])
        [ui/tab {:id :about :title (i18n/t :ui/tab-about)}]
        [ui/tab {:id :timeline :title (i18n/t :ui/tab-timeline)}]])
     [:div {:class "u-flex-1"}]
     (when (#{:object-type :datasource :transform} kind)
       [ui/tooltip {:content (i18n/t :ui/edit-kind {:kind (name kind)})}
        [ui/button {:small true :on-click (fn [_]
                                            (shell/open-edit-node-modal!
                                             {:kind kind :name type-kw}))}
         (i18n/t :ui/edit)]])
     [ui/tooltip {:content (i18n/t (if collapsed? :ui/expand :ui/collapse))}
      [ui/button {:small true :class "u-ml-6" :on-click #(panel/toggle-panel-collapsed!)}
       (if collapsed? "▲" "▼")]]
     [ui/tooltip {:content (i18n/t :ui/close)}
      [ui/button {:icon :cross :small true :class "u-ml-6"
                  :on-click #(panel/close-bottom-panel!)}]]]))

(defn- writeback-body [p done force?]
  (if done
    [:div {:class "panel-writeback__done"}
     (i18n/t (if (:pushed done) :ui/writeback-pushed :ui/writeback-completed))]
    [remote/show p
     (fn [p]
      (let [verdict-ok? (get-in p [:verdict :allow?])
            changes     (filter (fn [[_ v]] (:changes? v)) (:changes p))]
        [:<>
         [:div {:class "panel-writeback__source u-muted"}
          (str (i18n/t :ui/source) " " (some-> (:source-id p) name)
               " · " (i18n/t :ui/table) " " (:table p)
               " · pk " (:pk-value p)
               " · " (i18n/t :ui/strategy) " " (some-> (:strategy p) name))]
         (if (seq changes)
           [ui/html-table {:compact true :class style/table-root}
            [:thead [:tr [:th (i18n/t :ui/column)]
                     [:th (i18n/t :ui/live-source)]
                     [:th (i18n/t :ui/proposed)]]]
            [:tbody
             (for [[a {:keys [live proposed]}] changes]
               ^{:key (str a)}
               [:tr [:td {:class style/table-td-mono} (name a)]
                [:td (str live)]
                [:td {:class "panel-writeback__proposed"}
                 (str proposed)]])]]
           [:div {:class "u-italic u-muted"}
            (i18n/t :ui/writeback-no-differences)])
         [:div
          {:class "panel-writeback__verdict"
           :style {:background (if verdict-ok? "var(--c-success-bg)" "var(--c-danger-bg)")
                   :color (if verdict-ok? "var(--c-success-strong)" "var(--c-danger)")}}
          (i18n/t (if verdict-ok? :ui/writeback-no-conflict :ui/writeback-conflict))]
         (when-not verdict-ok?
           [ui/checkbox {:class "panel-writeback__force"
                         :checked (boolean force?)
                         :on-change #(db/send! [:shared.panel/writeback-force
                                                (.. % -target -checked)])
                         :label (i18n/t :ui/writeback-force)}])]))
     {:loading (i18n/t :ui/loading-preview)
      :failed  (i18n/t :ui/preview-impossible {:error (rd/reason p)})}]))

(def ^:private writeback-dialog
  (machine/entering
   db/sutra (fn [object-type object-id] [:shared.panel/writeback object-type object-id])
   [:shared.panel/writeback-left]
   (fn [_ _]
     (let [{p :preview :keys [done force? sending?]} (ev/at :writeback)
           p      (or p rd/loading)
           close! shell/close-writeback-modal!]
       [overlay/modal {:on-close close!
                       :class "panel-writeback__modal"
                       :title (i18n/t :ui/writeback-to-source)
                       :foot (when (and (rd/value? p) (not done))
                               [:<>
                                [ui/button {:on-click close!} (i18n/t :ui/cancel)]
                                [ui/button
                                 {:intent   :primary
                                  :disabled (or sending?
                                                (and (not (get-in p [:verdict :allow?]))
                                                     (not force?)))
                                  :on-click #(db/send! [:shared.panel/writeback-confirm])}
                                 (i18n/t (if sending? :ui/writing :ui/writeback-confirm))]])
                       :foot-end? true}
        [writeback-body p done force?]]))))

(defn writeback-modal []
  (when-let [{:keys [object-type object-id]} (:writeback-modal @shell/ui-state)]
    ^{:key (str object-id)} [writeback-dialog object-type object-id]))

(def ^:private tab-views
  {:object-type {:default (fn [id spec] [object-type/preview-tab id spec])
                 :code    (fn [id spec] [object-type/object-code-tab id spec])
                 :edits   (fn [id _] [object-type/edits-tab id])
                 :health  (fn [id _] [health/health-tab :object-type id])
                 :about   (fn [id spec] [object-type/about-tab id spec])}
   :transform   {:default (fn [id _] [transform/data-tab id])
                 :code    (fn [id _] [transform/code-tab id])
                 :health  (fn [id _] [health/health-tab :transform id])
                 :about   (fn [id _] [transform/about-tab id])}
   :datasource  {:default (fn [id _] [datasource/data-tab id])
                 :about   (fn [id spec] [datasource/about-tab id spec])}
   :dataset     {:default (fn [id _] [dataset/data-tab id])
                 :about   (fn [id _] [dataset/about-tab id])
                 :code    (fn [id _] [dataset/code-tab id])
                 :health  (fn [id _] [health/health-tab :dataset id])}})

(defn- not-in-schema [kind]
  [:div {:class "panel-tab__empty u-muted"}
   (case kind
     :object-type (i18n/t :ui/type-not-in-schema)
     :transform   (i18n/t :ui/transform-not-in-schema)
     :datasource  (i18n/t :ui/datasource-not-on-branch)
     :dataset     (i18n/t :ui/dataset-not-on-branch)
     nil)])

(defn bottom-panel
  []
  (let [{:keys [kind id]} (:detail-modal @shell/ui-state)
        bp                (:bottom-panel @shell/ui-state)
        mode              (shell/canvas-mode)
        schema-kind?      (panel/schema-panel-kinds kind)
        instance-kind?    (#{:object-instance :link} kind)
        relevant?         (if (= :instances mode) instance-kind? schema-kind?)]
    (when (and bp relevant? id)
      (let [spec       (case kind
                         :object-type (common/type-spec id)
                         :transform   (common/transform-spec id)
                         :datasource  (get (data/datasource-map) id)
                         :dataset     (get-in @catalog/canonical-schema [:datasets id])
                         nil)
            collapsed? (boolean (:collapsed? bp))
            tab        (or (:tab bp) :preview)
            height     (if collapsed? 40 (page/height ::panel 320))]
        [:div {:class "panel-root" :style {:height (str height "px")}}
         (when-not collapsed? [page/grip-h ::panel 320])
         [panel-header {:type-kw id :kind kind :spec spec :collapsed? collapsed?}]
         (when-not collapsed?
           (let [scroll? (or instance-kind? (not= tab :history))]
             [:div {:style (merge {:flex 1}
                                  (if scroll?
                                    {:overflow-y "auto"}
                                    {:overflow "hidden"
                                     :display "flex"
                                     :flex-direction "column"}))}
              (cond
                (= kind :object-instance) [instance/instance-detail id]
                (= kind :link)            [instance/link-detail id]
                (= tab :timeline)         [timeline/build-timeline-tab]
                (and (= tab :history) schema-kind?) [history/history-tab kind id]
                (nil? spec)               [not-in-schema kind]
                :else
                (let [views (get tab-views kind (:object-type tab-views))]
                  ((get views tab (:default views)) id spec)))]))]))))
