(ns sutra-ui.apphost.widgets.table
  (:require [sutra-ui.ui.display :as display :refer [badge]]
            [sutra-ui.ui.table :as table :refer [object-table]]
            [sutra-ui.ui.busy :as busy]
            [sutra.kit.coerce :refer [->kw]]
            [sutra-ui.ui.core :as ui]
            [sutra.capps.leaf-props :as lp]
            [sutra.capps.standard-view :as sv]
            [sutra.capps.table :as tbl]
            [sutra.capps.warn :as warn]
            [sutra-ui.render-ctx :as rctx]
            [sutra-ui.object.card :as oc]
            [sutra-ui.state.object-view :as object-view]
            [sutra-ui.apphost.events :refer [on-click-fn]]
            [sutra-ui.apphost.page-ctx :refer [ev-ctx rows-display row-cols]]))

(defn- badge-cell
  [table prop]
  (fn [row]
    (when-let [spec (tbl/badge-spec table prop row)]
      [badge (:tone spec) (or (:text spec) (:glyph spec))])))

(defn- data-cols
  [cols props]
  (mapv (fn [{:keys [prop render badge edit fmt] :as c}]
          (when-let [p (tbl/fmt-problem fmt)]
            (warn/warn-once! [::fmt fmt] p))
          (cond
            render c
            edit   c
            badge  (-> (dissoc c :prop :badge) (assoc :render (badge-cell badge prop)))
            prop   (-> (dissoc c :prop :fmt)
                       (assoc :render (fn [row] (tbl/cell-text c row (rctx/current)))))
            :else  c))
        (tbl/with-list-roles (tbl/with-prop-display cols props))))

(defn- compare-foot
  [[kind msg]]
  [:tr
   [:td {:col-span 99}
    (case kind
      :error   [ui/problem msg nil]
      :loading [busy/inline {:class "capp__hint"} "Szenario-Daten werden geladen …"])]])

(defn- compare-table-cfg
  [{:keys [on-row-click] :as cfg}]
  (let [cell (fn [c row]
               (when row
                 (cond
                   (:render c) ((:render c) row)
                   (:badge c)  ((badge-cell (:badge c) (:prop c)) row)
                   :else       (tbl/cell-text c row (rctx/current)))))
        {:keys [cols foot] :as t} (lp/compare-table cfg (rctx/current))]
    (cond-> (assoc t
                   :row-key (fn [r] (:key r))
                   :cols (mapv (fn [{c :col seg :seg label :label}]
                                 (if seg
                                   (assoc c
                                          :label label
                                          :cell-class "capp-cell--scenario"
                                          :render (fn [r] (cell c (get (:by r) seg))))
                                   (assoc c :render (fn [r] (cell c (:base r))))))
                               cols)
                   :on-row-click (when on-row-click
                                   (fn [r] (on-row-click (:base r)))))
      foot (assoc :foot (compare-foot foot)))))

(defn- viewable?
  [descriptor t]
  (boolean (or (object-view/object-view-surface?)
               (lp/type-entry descriptor :object-views t)
               (lp/type-entry descriptor :standard-views t))))

(defn- titled-row
  [desc data* rt row]
  (or (when (and data* rt)
        (let [k  (->kw (or (get (:object-names desc) rt) rt))
              id (str (:id row))]
          (some #(when (= id (str (:id %))) %) (get @data* k))))
      row))

(defn- title-col
  [{:keys [app scope]} rows label row-click?]
  (when-let [t (some-> (first rows) :object-type-ref)]
    (let [desc   (:descriptor app)
          opener (:open-object app)
          data*  (:data* scope)
          meta   (oc/descriptor-meta desc t)
          meta*  (memoize #(oc/descriptor-meta desc %))
          open*  (memoize #(when (viewable? desc %) opener))]
      {:label      label
       :cell-class "capp-table__td--title"
       :type-label (oc/type-label meta)
       :title-key  (:title-key meta)
       :render     (fn [row]
                     (let [rt (or (:object-type-ref row) t)]
                       [oc/object-link rt (:id row)
                        {:row (titled-row desc data* rt row) :meta (meta* rt)
                         :open? (not row-click?) :on-open (open* rt)}]))})))

(defn- with-title-col
  [env cols rows row-click?]
  (let [desc (:descriptor (:app env))
        cols (or (not-empty (lp/as-rows cols)) (row-cols desc rows))]
    (if-let [tc (title-col env rows nil row-click?)]
      (let [tk     (->kw (:title-key tc))
            lead   (-> tc (assoc :label (:type-label tc)) (dissoc :title-key :type-label))
            title? #(and tk (= tk (->kw (:prop %))) (not (:edit %)) (not (:render %)))]
        (cond
          (nil? cols)
          (into [lead]
                (comp (remove #{tk}) (map (fn [p] {:prop p})))
                (sv/prominent-props (rows-display desc rows)))

          (some title? cols)
          (mapv #(if (title? %)
                   (-> (title-col env rows (:label %) row-click?)
                       (dissoc :title-key :type-label)
                       (assoc :prop tk))
                   %)
                cols)

          tk    (into [lead] cols)
          :else cols))
      cols)))

(defn with-rows-shown
  [cfg env]
  (let [rows (lp/as-rows (:rows cfg))]
    (-> cfg
        (assoc :rows (tbl/sort-limit-rows rows (:sort cfg) (:limit cfg)))
        (dissoc :sort :limit)
        (update :cols #(data-cols (with-title-col env % rows (some? (:on-row-click cfg)))
                                  (rows-display (:descriptor (:app env)) rows))))))

(defn object-table-widget [cfg env]
  (let [ev (ev-ctx (:app env))]
    [object-table
     (if (:compare cfg)
       (compare-table-cfg (update cfg :on-row-click on-click-fn ev))
       (-> (with-rows-shown cfg env)
           (update :on-row-click on-click-fn ev)))]))
