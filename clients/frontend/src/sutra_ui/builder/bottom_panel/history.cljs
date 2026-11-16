(ns sutra-ui.builder.bottom-panel.history
  (:require [sutra-ui.ui.core :as ui]
            [clojure.string :as str]
            [sutra.i18n.core :as i18n]
            [sutra.kit.remote :as rd]
            [sutra-ui.builder.bottom-panel.common :as common]
            [sutra-ui.builder.bottom-panel.dataset :as dataset]
            [sutra-ui.builder.bottom-panel.datasource :as datasource]
            [sutra-ui.builder.bottom-panel.object-type :as object-type]
            [sutra-ui.builder.bottom-panel.timeline :as timeline]
            [sutra-ui.builder.bottom-panel.transform :as transform]
            [sutra-ui.ui.busy :as busy]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.data :as data]
            [sutra-ui.state.lineage-status :as lstatus]
            [sutra-ui.state.panel :as panel]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.ui.style :as style]
            [sutra-ui.ui.table :as table]))

(defn- timeline-row
  [type-kw selected-id {:keys [id kind status started-at
                               snapshot-id] :as run}]
  (let [selected? (= id selected-id)]
    ^{:key (str id)}
    [:div {:on-click #(panel/select-panel-run! type-kw run)
           :class "panel-timeline__row"
           :style {:border-left (str "3px solid "
                                     (if selected? "var(--c-primary)" "transparent"))
                   :background (if selected? "var(--c-primary-bg)" "transparent")}}
     [:div {:class "panel-timeline__head"}
      [:span {:class "panel-timeline__kind"} (some-> kind name)]
      [common/status-pill status]]
     [:div {:class "explorer-matrix__col-attr u-muted"}
      (when started-at (str started-at))]
     [:div {:class "panel-timeline__commit"}
      (when snapshot-id (subs (str snapshot-id) 0 7))]]))

(defn- timeline-pane [type-kw]
  (let [{:keys [show rows] :as v} (rd/list-view @data/runs)
        selected-id (get @panel/panel-selected-run type-kw)]
    [:div {:class "panel-timeline"}
     [:div {:class "panel-timeline__header u-muted"}
      (when (#{:list :empty} show) [:span (str (count rows) " runs")])
      [ui/button {:icon :refresh :small true :on-click #(data/refresh-runs!)}]]
     [:div {:class "dataconn-panel__body"}
      (case show
        :empty [:div {:class "panel-timeline__empty u-muted"}
                (i18n/t :ui/runs-none)]
        :list  (for [r rows] (timeline-row type-kw selected-id r))
        [timeline/runs-status v])]]))

(defn- logs-sub
  [run]
  (let [{:keys [loading? error jobs]} (panel/panel :run-jobs (:id run))]
    [:div
     [:div {:class "panel-asof__status u-muted"}
      (cond (nil? run)  (i18n/t :ui/pick-run-for-jobs)
            loading?    [busy/inline (i18n/t :ui/loading)]
            error       (i18n/t :ui/error-prefix {:error error})
            :else       (i18n/t (if (= 1 (count jobs)) :ui/jobs-in-run-one :ui/jobs-in-run-many)
                                {:n (count jobs) :run (common/short-uuid (:id run))}))]
     (cond
       (or (nil? run) error) nil

       (and (not loading?) (empty? jobs))
       [:div {:class "u-italic u-muted"}
        (i18n/t :ui/jobs-none-in-run)]

       (seq jobs)
       [table/data-table
        {:scroll? true
         :class   style/table-root
         :rows    jobs
         :row-key :job_rid
         :expand  (fn [j]
                    (if-let [log (not-empty (str (:job_log j)))]
                      [:pre {:class "panel-code__pre"} log]
                      [:div {:class "u-italic u-muted"} (i18n/t :ui/job-no-log)]))
         :cols    [{:label (i18n/t :ui/kind-transform) :mono? true
                    :render #(str (or (:transform_rid %) "—"))}
                   {:label  (i18n/t :ui/status)
                    :render #(vector common/status-pill (some-> (:status %) str/lower-case))}
                   {:label (i18n/t :ui/attempt) :render #(str (or (:attempt %) 1))}
                   {:label (i18n/t :ui/rows-in) :render #(str (or (:rows_in %) "—"))}
                   {:label (i18n/t :ui/rows-out) :render #(str (or (:rows_out %) "—"))}
                   {:label (i18n/t :ui/commit) :mono? true
                    :render #(common/short-uuid (:snapshot_id %))}
                   {:label (i18n/t :ui/started) :render #(common/ts (:started_at %))}
                   {:label (i18n/t :ui/error) :cell-class "panel-job__error"
                    :render #(str (or (:error %) ""))}]}])]))

(defn- txn-summary
  [kind node-kw txns]
  (let [head (first txns)
        cell (fn [label value]
               [:div {:class "panel-summary__cell"}
                [:div {:class "panel-summary__label"} label]
                [:div {:class "panel-summary__value"} value]])]
    [:div {:class "panel-summary"}
     (cell (i18n/t :ui/transactions) (str (count txns)))
     (cell (i18n/t :ui/latest) (or (some-> head :type str) "—"))
     (cell (i18n/t :ui/rows) (str (or (:row_count head) "—")))
     (cell (i18n/t :ui/row-delta) (str (or (:row_delta head) "—")))
     (cell (i18n/t :ui/at) (common/ts (:ended_at head)))
     [:div {:class "u-flex-1"}]
     (when (= :dataset kind) [common/force-snapshot-button node-kw])
     [common/rollback-button {:kind kind :node-kw node-kw :commit (:snapshot_id head)
                              :label (i18n/t :ui/rollback-to-latest)}]]))

(defn- files-sub
  [kind node-kw]
  (let [{:keys [loading? error transactions via]} (panel/panel :transactions node-kw)
        sel (get-in @shell/ui-state [:bottom-panel :selected-txn])
        pick! (fn [t] (panel/select-panel-txn!
                        (when (not= sel (:txn_rid t)) (:txn_rid t))))
        chosen (some #(when (= sel (:txn_rid %)) %) transactions)]
    [:div
     (cond
       loading? [busy/block {:class "panel-asof__status"} (i18n/t :ui/loading)]
       error    [:div {:class "panel-asof__status u-muted"}
                 (i18n/t :ui/error-prefix {:error error})]
       :else
       [:<>
        (if chosen
          [:div {:class "panel-summary"}
           [:div {:class "panel-summary__cell"}
            [:div {:class "panel-summary__label"} (i18n/t :ui/transaction-selected)]
            [:div {:class "panel-summary__value"}
             (str (:type chosen) " · " (common/short-uuid (:txn_rid chosen)))]]
           [:div {:class "u-flex-1"}]
           [common/rollback-button {:kind kind :node-kw node-kw :commit (:snapshot_id chosen)
                                    :label (i18n/t :ui/rollback-to-transaction)}]]
          [txn-summary kind node-kw transactions])
        (when via
          [:div {:class "panel-asof__status u-muted"}
           (i18n/t :ui/transactions-on-backing {:producer (:producer via)})])
        (if (empty? transactions)
          [:div {:class "u-italic u-muted"}
           (i18n/t :ui/transactions-none)]
          [table/data-table
           {:scroll?      true
            :class        style/table-root
            :rows         transactions
            :row-key      :txn_rid
            :on-row-click pick!
            :row-class    (fn [t]
                            [(when (= "failed" (some-> (:status t) str/lower-case))
                               "panel-txn--rolled-back")
                             (when (= sel (:txn_rid t)) "panel-txn--sel")])
            :cols         [{:label (i18n/t :ui/type) :mono? true :render #(str (:type %))}
                           {:label  (i18n/t :ui/status)
                            :render #(vector common/status-pill
                                             (some-> (:status %) str/lower-case))}
                           {:label (i18n/t :ui/rows) :render #(str (or (:row_count %) "—"))}
                           {:label "Δ" :render #(str (or (:row_delta %) "—"))}
                           {:label (i18n/t :ui/commit) :mono? true
                            :render #(common/short-uuid (:snapshot_id %))}
                           {:label (i18n/t :ui/started) :render #(common/ts (:started_at %))}
                           {:label (i18n/t :ui/ended) :render #(common/ts (:ended_at %))}]}])])]))

(defn- metadata-sub
  [kind node-kw]
  (let [sch    @catalog/canonical-schema
        sec    (case kind :dataset :datasets :object-type :object-types
                     :transform :transforms :datasource :sources nil)
        spec   (get-in sch [sec node-kw])
        status (lstatus/node-status kind node-kw)
        head   (first (:transactions (panel/panel :transactions node-kw)))
        cols   (catalog/node-columns kind node-kw)
        rows   [[(i18n/t :ui/name)         (name node-kw)]
                [(i18n/t :ui/kind)         (name kind)]
                [(i18n/t :ui/resource-id)  (or (:id spec) "—")]
                [(i18n/t :ui/project)      (catalog/node-project kind node-kw)]
                [(i18n/t :ui/description)  (or (:description spec) "—")]
                [(i18n/t :ui/columns)      (str (count cols))]
                [(i18n/t :ui/rows-latest-transaction) (str (or (:row_count head) "—"))]
                [(i18n/t :ui/last-write)   (common/ts (:ended_at head))]
                [(i18n/t :ui/build-status)
                 (str (or (:build-status status) (i18n/t :ui/never-built)))]
                [(i18n/t :ui/out-of-date)  (i18n/t (if (:out-of-date? status) :ui/yes :ui/no))]
                [(i18n/t :ui/last-build)   (common/ts (:last-build status))]
                [(i18n/t :ui/schedules)    (str (or (:schedules status) 0))]
                [(i18n/t :ui/data-health)  (str (or (some-> status :health :status) "—"))]]]
    [:div
     [ui/html-table {:compact true :class style/table-root}
      [:tbody
       (for [[k v] rows]
         ^{:key k}
         [:tr [:td {:style {:color "var(--c-muted)" :width "40%"}} k]
          [:td {:class style/table-td-mono} v]])]]]))

(defn- schema-sub
  [kind node-kw run]
  (if (= :object-type kind)
    [object-type/as-of-schema-tab node-kw run]
    (let [cols (common/match-columns
                [:schema node-kw]
                (case kind
                  :dataset
                  (get-in @catalog/canonical-schema [:datasets node-kw :columns])
                  (mapv (fn [c] {:attribute-name (keyword c)})
                        (catalog/node-columns kind node-kw)))
                :attribute-name)]
      [:div
       [:div {:class "panel-toolbar"}
        [:div {:class "colormenu__mode-label u-muted"}
         (i18n/t :ui/columns-current-branch {:n (count cols) :kind (name kind)})]
        [common/column-search [:schema node-kw]]]
       (if (empty? cols)
         [:div {:class "u-italic u-muted"} (i18n/t :ui/no-columns)]
         [common/attr-table :ui/column cols])])))

(defn- job-spec-sub
  [kind node-kw run]
  (if-let [t-kw (common/producer-of kind node-kw)]
    [:<>
     (when (not= t-kw node-kw)
       [:div {:class "panel-asof__status u-muted"}
        (i18n/t :ui/produced-by-transform {:name (name t-kw)})])
     [transform/as-of-code t-kw run]]
    [:div {:class "panel-tab__body"}
     [:div {:class "u-italic u-muted"}
      (i18n/t :ui/no-producing-transform)]]))

(def ^:private history-subs
  {:object-type [[:data :ui/tab-data] [:logs :ui/tab-logs] [:files :ui/tab-files]
                 [:metadata :ui/tab-metadata] [:schema :ui/tab-schema] [:job-spec :ui/tab-job-spec]]
   :transform   [[:data :ui/tab-data] [:logs :ui/tab-logs] [:files :ui/tab-files]
                 [:metadata :ui/tab-metadata] [:schema :ui/tab-schema] [:job-spec :ui/tab-job-spec]]
   :dataset     [[:data :ui/tab-data] [:logs :ui/tab-logs] [:files :ui/tab-files]
                 [:metadata :ui/tab-metadata] [:schema :ui/tab-schema] [:job-spec :ui/tab-job-spec]]
   :datasource  [[:data :ui/tab-data] [:logs :ui/tab-logs] [:metadata :ui/tab-metadata]
                 [:spec :ui/tab-spec]]})

(defn history-subs-of
  [kind]
  (get history-subs kind))

(def ^:private as-of-data
  {:object-type object-type/as-of-data-tab
   :transform   transform/as-of-data
   :dataset     dataset/as-of-data
   :datasource  datasource/as-of-data})

(defn- history-pane
  [kind node-kw run]
  (let [entries (get history-subs kind)
        valid   (set (map first entries))
        sub     (let [s (get-in @shell/ui-state [:bottom-panel :history-sub])]
                  (if (valid s) s (ffirst entries)))]
    [:div {:class "explorer-workspace"}
     [common/sub-tab-bar
      {:entries entries :active sub
       :pick! #(panel/set-panel-history-sub! %)
       :right (when (and run (not= :files sub))
                [common/rollback-button {:kind kind :node-kw node-kw
                                         :commit (:snapshot-id run)}])}]
     [:div {:class "panel-asof__pane"}
      (case sub
        :data     (when-let [view (as-of-data kind)] [view node-kw run])
        :spec     [datasource/as-of-spec node-kw run]
        :logs     [logs-sub     run]
        :files    [files-sub    kind node-kw]
        :metadata [metadata-sub kind node-kw]
        :schema   [schema-sub   kind node-kw run]
        :job-spec [job-spec-sub kind node-kw run]
        nil)]]))

(defn history-tab
  [kind node-kw]
  (let [runs   (data/run-list)
        sel-id (get @panel/panel-selected-run node-kw)
        sel    (or (some #(when (= (:id %) sel-id) %) runs)
                   (first runs))]
    (when (and sel (nil? sel-id))
      (panel/select-panel-run! kind node-kw sel))
    [:div {:class "panel-history"}
     [timeline-pane node-kw]
     [history-pane  kind node-kw sel]]))
