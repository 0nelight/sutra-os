(ns sutra-ui.apps.source-manager.streams
  (:require [sutra-ui.ui.core :as ui :refer [tag]]
            [clojure.string :as str]
            [sutra.i18n.core :as i18n]
            [sutra-ui.apps.source-manager.events :as ev]
            [sutra-ui.apps.source-manager.model :as model]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.resource.link :as rl]
            [sutra-ui.ui.busy :as busy]
            [sutra-ui.ui.page :as page]
            [sutra-ui.ui.table :as table]
            [sutra.types.transform-io :as tio]
            [sutra-ui.state.catalog :as catalog]))

(defn- st [] (:streams @model/ui))

(def ^:private send! model/send!)

(defn- pill
  ([label kind] (pill label kind nil))
  ([label kind class]
   [tag {:minimal true :intent (case kind :key :primary :warn :warning :live :success nil)
         :class class}
    label]))

(defn- head-offset [end-offsets] (reduce + 0 (vals end-offsets)))

(defn- max-lag
  [{:keys [cursors end-offsets]}]
  (let [head (head-offset end-offsets)]
    (reduce max 0 (for [[_ c] cursors]
                    (max 0 (- head (reduce + 0 (vals (:offsets c)))))))))

(defn- stream-object-types
  [stream]
  (let [ds (keyword stream)]
    (distinct
     (for [[_ t] (:transforms @catalog/canonical-schema)
           out (tio/outputs-of t)
           :when (and (= :object-type (some-> (:kind out) name keyword))
                      (= ds (some-> (:dataset out) name keyword)))]
       (some-> (:object-type out) name keyword)))))

(defn- stream-row [{:keys [stream partitions cdc? buffered archived-rows subscribers last-record-at]
                    :as stream-info}]
  (let [lag (max-lag stream-info)]
    [:div {:on-click #(send! [:sources.stream/select stream])
           :class (str "streams-list__row"
                       (when (= stream (:selected (st))) " streams-list__row--active"))}
     [:div {:class "streams-list__name"}
      [ui/tooltip
       {:content (i18n/t (if (pos? lag) :ui/streams-behind-hint :ui/streams-current-hint))}
       [ui/icon {:icon :dot :class (if (pos? lag) "u-text-modified" "u-text-added")}]]
      stream]
     [:div {:class "streams-list__meta"}
      (when (pos? lag) [pill (i18n/t :ui/streams-lag {:n lag}) :warn])
      [pill (i18n/t :ui/streams-partitions {:n partitions}) :neutral]
      [pill (i18n/t (if cdc? :ui/streams-with-pk :ui/streams-without-pk)) (if cdc? :key :neutral)]
      (when (pos? (or buffered 0)) [pill (i18n/t :ui/streams-buffered {:n buffered}) :warn])
      (when (pos? (or subscribers 0)) [pill (i18n/t :ui/streams-watching {:n subscribers}) :live])]
     [:div {:class "streams-list__count"}
      (i18n/t :ui/streams-archived {:n (or archived-rows 0)})
      (when last-record-at
        [:div {:class "streams-list__last"}
         (i18n/t :ui/streams-last-record
                 {:at (subs (str last-record-at) 0 (min 19 (count (str last-record-at))))})])]]))

(defn- pk-columns [{:keys [spec cdc?]}]
  (when cdc? (mapv keyword (get-in spec [:primary-key :columns]))))

(defn- keys-line [{:keys [spec] :as d}]
  [:div {:class "streams-detail__keys"}
   [:div
    [:span {:class "streams-detail__keylabel"} (i18n/t :ui/streams-partition-by) " "]
    [:span {:class "streams-detail__keyval"}
     (if (seq (:partition-keys spec))
       (str/join ", " (map name (:partition-keys spec)))
       (i18n/t :ui/streams-unkeyed))]]
   [:div
    [:span {:class "streams-detail__keylabel"} (i18n/t :ui/dc-primary-key) " "]
    [:span {:class "streams-detail__keyval"}
     (if-let [pk (seq (pk-columns d))]
       (str (str/join ", " (map name pk))
            (when-let [del (get-in spec [:primary-key :deletion-column])]
              (str "   ·   " (i18n/t :ui/streams-retracted-by {:column (name del)}))))
       (i18n/t :ui/streams-no-pk))]]])

(defn- partitions-table [{:keys [end-offsets depth-by-partition]}]
  [table/data-table
   {:rows (sort-by (comp str key) end-offsets)
    :row-key (comp str key)
    :cols [{:label (i18n/t :ui/streams-partition) :render #(str (key %))}
           {:label (i18n/t :ui/streams-next-offset) :mono? true :render #(str (val %))}
           {:label (i18n/t :ui/streams-buffered-col) :mono? true
            :render #(str (get depth-by-partition (key %) 0))}]}])

(defn- consumer-cell
  [stream who]
  (case (keyword who)
    :archive [:span (i18n/t :ui/streams-consumer-archive) " "
              [rl/resource-link :dataset stream]]
    :index   (into [:span (i18n/t :ui/streams-consumer-index) " "]
                   (or (seq (for [t (stream-object-types stream)]
                              ^{:key t} [rl/resource-link :object-type t]))
                       [(i18n/t :ui/streams-ontology)]))
    [:span (name who)]))

(defn- cursors-table [{:keys [stream cursors end-offsets]}]
  (let [head (head-offset end-offsets)
        at   (fn [[_ c]] (reduce + 0 (vals (:offsets c))))]
    [table/data-table
     {:rows (sort-by (comp str key) cursors)
      :row-key (comp str key)
      :cols [{:label (i18n/t :ui/streams-consumer) :render #(vector consumer-cell stream (key %))}
             {:label (i18n/t :ui/streams-at) :mono? true
              :render #(if (val %) (str (at %)) "—")}
             {:label (i18n/t :ui/streams-behind) :mono? true
              :render #(if (val %) (str (max 0 (- head (at %)))) (str head))}
             {:label (i18n/t :ui/dc-last-run)
              :render #(or (some-> (val %) :last-run-at str) (i18n/t :ui/streams-never))}]}]))

(defn- fmt-value [v]
  (cond
    (nil? v)     "—"
    (string? v)  v
    (number? v)  (.toLocaleString v)
    (boolean? v) (i18n/t (if v :ui/yes :ui/no))
    (keyword? v) (name v)
    :else        (js/JSON.stringify (clj->js v))))

(defn- column-order
  [pk rows]
  (let [all (into #{} (mapcat keys) rows)
        pk  (filter all pk)]
    (vec (concat pk (sort (remove (set pk) all))))))

(defn- records-table [pk rows]
  (let [pk? (set pk)]
    [table/data-table
     {:rows (vec rows)
      :cols (for [c (column-order pk rows)]
              {:key (str c) :label (name c) :mono? true
               :head-attrs {:class (when (pk? c) "u-bold")}
               :render #(fmt-value (get % c))})}]))

(defn- record-line [pk record]
  (let [record (into {} (map (fn [[k v]] [(keyword k) v])) record)]
    [:span {:class "streams-tail__record"}
     (str/join "  ·  " (for [c (column-order pk [record])]
                         (str (name c) " " (fmt-value (get record c)))))]))

(defn- tail-pane [pk]
  (let [{:keys [live? tail]} (st)]
    [:div
     [:div {:class "streams-detail__cardtitle"}
      (i18n/t :ui/streams-live-tail)
      (if live?
        [pill (i18n/t :ui/streams-connected) :live "streams-pill--live"]
        [pill (i18n/t :ui/streams-reconnecting) :warn "streams-pill--reconnecting"])]
     [:div {:class "streams-tail__hint"} (i18n/t :ui/streams-tail-hint {:n ev/tail-cap})]
     (if (empty? tail)
       [ui/empty-state {:title (i18n/t :ui/streams-waiting)}]
       [:div {:class "streams-tail"}
        (for [[i {:keys [partition offset record]}] (map-indexed vector tail)]
          ^{:key i}
          [:div {:class "streams-tail__row"}
           [:span {:class "streams-tail__offset"} (str partition ":" offset)]
           [record-line pk record]])])]))

(defn detail-pane
  []
  (let [{d :detail :keys [flushing? view selected]} (st)
        pk (pk-columns d)]
    [:div {:class "streams-detail"}
     [page/header {:title selected}
      [ui/tooltip {:content (i18n/t :ui/streams-flush-hint)}
       [ui/button {:small true :disabled (or (nil? d) flushing?)
                   :on-click #(send! [:sources.stream/flush selected])}
        (i18n/t (if flushing? :ui/streams-flushing :ui/streams-flush))]]
      [ui/button {:minimal true :icon :cross :aria-label (i18n/t :ui/close)
                  :on-click #(send! [:sources.stream/deselect])}]]
     (if-not d
       [busy/block nil (i18n/t :ui/loading)]
       [:div {:class "streams-detail__body"}
        [:div {:class "streams-detail__sub u-flex-wrap-gap-6"}
         [:span (i18n/t :ui/streams-dataset)] [rl/resource-link :dataset (:stream d)]
         (for [t (stream-object-types (:stream d))]
           ^{:key t} [:span "· " [rl/resource-link :object-type t]])]
        [keys-line d]
        [:div {:class "streams-detail__card"}
         [:div {:class "streams-detail__cardtitle"} (i18n/t :ui/streams-consumers)]
         [cursors-table d]]
        [:div {:class "streams-detail__card"} [tail-pane pk]]
        [:div {:class "streams-detail__card streams-detail__card--view"}
         [:div {:class "streams-detail__cardtitle"}
          (i18n/t (if (:cdc? d) :ui/streams-current-view :ui/streams-records))
          (when-let [v view]
            [:span {:class "streams-detail__cardnote"}
             (i18n/t :ui/streams-view-note {:cold (:cold v) :hot (:hot v)})])]
         (if-let [rows (seq (:rows view))]
           [records-table pk rows]
           [ui/empty-state {:title (i18n/t :ui/streams-no-rows)}])]
        [ui/section
         {:title (i18n/t :ui/streams-partitions-title) :collapsible true :compact true
          :collapse-props {:default-is-open false} :class "streams-detail__card"}
         [ui/section-card
          [partitions-table d]]]])]))

(defn selected [] (:selected (st)))

(def streams-pane
  (machine/entering
   model/sources [:sources.streams/enter] [:sources.streams/leave]
   (fn []
     (let [{:keys [error] streams :list} (st)]
       [:div {:class "streams-list"}
        (when error [ui/problem error nil])
        [:div {:class "streams-list__title"}
         (str (i18n/t :ui/streams) " (" (count streams) ")")]
        (cond
          (nil? streams)   [busy/block nil (i18n/t :ui/loading)]
          (empty? streams) [ui/empty-state {:title (i18n/t :ui/streams-none)}]
          :else (for [s streams]
                  ^{:key (:stream s)} [stream-row s]))]))))
