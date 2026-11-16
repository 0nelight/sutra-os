(ns sutra-ui.apphost.render
  (:require [sutra-ui.ui.overlay :as overlay]
            [sutra-ui.ui.sel :as sel]
            [sutra-ui.ui.filter :as ui-filter :refer [filter-pills]]
            [sutra-ui.ui.display :as display :refer [kpi-row kpi bars badge tabs]]
            [sutra-ui.ui.table :as table]
            [sutra-ui.ui.page :as page]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.kit.remote :as rd]
            [reagent.core :as r]
            [sutra-ui.ui.size :as groesse]
            [sutra-ui.ui.core :as ui :refer [html-select empty-state]]
            [goog.object :as gobj]
            [sutra.capps.bind :as bind]
            [sutra.capps.catalog :as cat]
            [sutra.capps.format :as fmt]
            [clojure.string :as str]
            [sutra.capps.table :as tbl]
            [sutra-ui.render-ctx :as rctx]
            [sutra.capps.hyperlink :as hyperlink]
            [sutra.capps.intent :as ci]
            [sutra.capps.layout :as lay]
            [sutra.capps.leaf-props :as lp]
            [sutra.capps.visibility :as vis]
            [sutra.i18n.core :as i18n]
            [sutra.types.paths :as paths]
            [sutra-ui.ui.inputs :refer [checkbox]]
            [sutra-ui.apphost.charts :as charts]
            [sutra-ui.apphost.widgets.analysis :as analysis-widgets]
            [sutra-ui.apphost.widgets.flow :as flow-widgets]
            [sutra-ui.apphost.widgets.map :as map-widget]
            [sutra-ui.apphost.widgets.vega :as vega-widget]
            [sutra-ui.apphost.widgets.markdown :refer [markdown]]
            [sutra-ui.apphost.frame :as frame]
            [sutra-ui.apphost.model]
            [sutra-ui.apphost.widgets.freshness :as freshness]
            [sutra-ui.apphost.widgets.history :as history-widgets]
            [sutra-ui.apphost.scope-handles :refer
             [resolve-ref build-scope url-keys-on-screen]]
            [sutra-ui.apphost.widgets.media :as media-widgets]
            [sutra-ui.apphost.uploader :refer [media-uploader]]
            [sutra-ui.apphost.widgets.metric :as metric-widget]
            [sutra-ui.apphost.widgets.pivot :as pivot-widget]
            [sutra-ui.apphost.widgets.recorder :as recorder]
            [sutra-ui.apphost.widgets.resource :as resource-widgets]
            [sutra-ui.apphost.widgets.rtf :refer [rtf-text]]
            [sutra-ui.apphost.filters :refer [filter-sidebar]]
            [sutra-ui.ui.filter-model :as fm]
            [sutra-ui.apphost.widgets.filter :as filter-widgets]
            [sutra-ui.apphost.widgets.display :as dw]
            [sutra-ui.apphost.widgets.ai :as ai-widgets]
            [sutra-ui.apphost.widgets.custom :as custom-widget]
            [sutra-ui.apphost.widgets.embed :as embed-widgets]
            [sutra-ui.apphost.widgets.notebook :as notebook-widget]
            [sutra-ui.apphost.widgets.button :as button-widgets]
            [sutra-ui.apphost.widgets.device :as device-widgets]
            [sutra-ui.apphost.widgets.inline-action :as inline-action-widget]
            [sutra-ui.apphost.widgets.nav :as nav-widgets]
            [sutra-ui.apphost.widgets.pairing :as pairing-widget]
            [sutra-ui.apphost.widgets.scenario :as scenario-widgets]
            [sutra-ui.apphost.widgets.comments :as comments-widget]
            [sutra.capps.object-display :as od]
            [sutra-ui.apphost.modules :as mod]
            [sutra-ui.apphost.client :as client :refer [export!]]
            [sutra.types.set-query :as sq]
            [sutra.capps.variables :as vars :refer
             [fn-error? fn-error-msg deref*]]
            [sutra.capps.warn :as warn]
            [sutra-ui.apphost.events :refer [blocked-app on-click-fn
                                                           run-effects!]]
            [sutra-ui.apphost.state :as state]
            [sutra-ui.apphost.widgets.input :as input-widgets]
            [sutra-ui.apphost.widgets.document :as document-widgets]
            [sutra-ui.apphost.widgets.object-view :as object-view-widgets :refer
             [objekt-attrs object-view-widget]]
            [sutra-ui.apphost.widgets.table :refer
             [with-rows-shown object-table-widget]]
            [sutra-ui.apphost.widgets.action :as action-widgets :refer
             [action-label deref-vals action-form-widget action-table-widget]]
            [sutra-ui.apphost.page-ctx :refer
             [ev-ctx page media-src sourced rows-display submitted]]
            [sutra-ui.apphost.view-state :as view-state]
            [sutra-ui.object.card :as oc]
            [sutra-ui.ui.busy :as busy]
            [sutra-ui.state.atlas :as kompass]
            [sutra-ui.state.object-view :as object-view]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.debug :as dbg]))

(defn- defects
  [data]
  (into []
        (comp (filter map?)
              (mapcat (fn [row]
                        (for [[a ds] (:defects row), d ds]
                          (assoc d :attribute a :row (:id row))))))
        (tree-seq coll? seq data)))

(defn- defect-banner
  [data]
  (when-let [ds (seq (defects data))]
    (let [groups (group-by (juxt :attribute :over) ds)]
      [ui/problem
       (str (count groups)
            (if (= 1 (count groups)) " Wert ist unbeantwortet" " Werte sind unbeantwortet")
            " — nicht 0")
       (str/join "\n"
                 (for [[[a _] rs] (sort-by (comp str first key) groups)]
                   (str (name a) " — " (:why (first rs))
                        " (" (count rs) (if (= 1 (count rs)) " Zeile)" " Zeilen)"))))])))

(defn- version-banner
  [{:keys [seq published?] :as version}]
  (when (and version (not published?))
    [ui/callout {:intent :warning :compact true :role "status"}
     [:span (if seq
              (str "Nicht veröffentlichte Fassung " seq)
              "Nicht veröffentlichte Fassung")
      " — die Betrachter dieser App sehen etwas anderes."]]))

(defn app-overlay
  [ov k {:keys [kind on-close docked?] :or {kind :modal} :as opts} content]
  (when (state/open? ov k)
    (let [arg   (state/overlay-arg ov k)
          close (fn [] (when on-close (on-close arg)) (state/close! ov k))
          opts  (assoc opts :on-close close)
          body  (content arg)]
      (cond docked?          [overlay/dock opts body]
            (= kind :drawer) [overlay/drawer (merge {:side :right} opts) body]
            :else            [overlay/modal opts body]))))

(defn- compare-chart-cfg
  [{:keys [group] :as cfg}]
  (when (and group (not= group :szenario))
    (warn/warn! (str "[capp/chart] :compare and :group " (pr-str group) " together — "
                     "a scenario comparison IS the segment-by axis; :group is ignored")))
  (lp/compare-chart cfg))

(defn- resolve-for-object
  [node object handles derived data]
  (lay/resolve-item (vars/resolve-scoped node handles derived data) object))

(defn- kpi-click
  [app on-click]
  (when-let [f (on-click-fn on-click (ev-ctx app))]
    #(f nil)))

(defn- output->fn
  [on-output on-event event-fn]
  (when (or (map? on-output) (map? on-event))
    (fn [k v]
      (let [h (get on-output k)]
        (when (satisfies? IReset h) (reset! h v)))
      (when-let [f (some-> (get on-event k) event-fn)] (f v)))))

(defn- embed-app
  [{:keys [app]} _env]
  (let [h (mod/use-app app)]
    (fn [{:keys [inputs bind on-output on-event]} {:keys [event-fn]}]
      (let [{:keys [loading? module data error]} @h]
        (cond
          loading? [busy/block {:class "capp-embed capp-embed--loading"} (i18n/t :ui/capp-loading)]
          error    [ui/problem (i18n/t :ui/capp-embed-failed) (str "app/embed: " error)]
          :else    [:div.capp-embed
                    [mod/embed {:module module
                                :inputs (merge inputs (update-vals bind deref*))
                                :bound  (set (keys bind))
                                :on-output (output->fn (merge on-output bind) on-event event-fn)
                                :data data}]])))))

(defn export-button
  [f descriptor {:keys [label busy-label params filename mime class disabled? intent]}]
  (r/with-let [k (random-uuid)]
    (let [{:keys [busy? error]} (frame/deed f k)
          btn [ui/button
               {:class    class
                :intent   (:intent (ci/btn-props (or intent :none)))
                :icon     :download
                :loading  busy?
                :disabled (boolean disabled?)
                :on-click #(frame/deed! f k {:ask [export! descriptor (or params {})
                                                   {:filename filename :mime mime}]})}
               (if busy? (or busy-label "Export…") (or label "Export"))]]
      (if error
        [:div.capp-action
         btn
         [:div.capp-action__error error]]
        btn))
    (finally (frame/send! f [:capp/drop-deed k]))))

(declare render-node scope-view)

(defn upload-meta
  [meta {:keys [handles derived data*]}]
  (letfn [(value-of [n]
            (let [data (some-> data* deref)
                  v    (resolve-ref {:val n} handles derived data)]
              (if (some? v) v (resolve-ref {:val (->kw n)} handles derived data))))]
    (cond
      (map? meta)  meta
      (coll? meta) (into {} (map (fn [n] [(->kw n) (value-of n)])) meta)
      :else        nil)))

(defn- comments-widget
  [f descriptor refresh {:keys [items action param placeholder empty]}]
  (r/with-let [k (random-uuid)]
    (let [{:keys [error]} (frame/deed f k)]
      [display/comments items
       (cond-> {:placeholder placeholder :empty empty
                :error (when error (i18n/t :ui/capp-comment-failed {:reason error}))}
         action (assoc :on-submit
                       (fn [text]
                         (frame/deed! f k {:ask  [client/submit-strict! descriptor
                                                  {(or param :body) text} {:action action}]
                                           :then [[:rf/invoke
                                                   (submitted {:refresh refresh})]]}))))])
    (finally (frame/send! f [:capp/drop-deed k]))))

(defn- media-uploader-widget
  [{:keys [accept label meta multiple routes drop-label rid-output filename-output action
           file-param media-param params destination media-column branch] :as cfg}
   {:keys [descriptor refresh submit! upload-meta frame]}]
  [media-uploader descriptor
   (merge
    (select-keys cfg [:left-icon :right-icon :max-files :capture])
    {:frame frame
     :on-processed (fn [_] (client/refresh-page!))
     :accept accept :label label :meta (upload-meta meta)
     :multiple multiple :drop-label drop-label
     :intent (->kw (:intent cfg))
     :destination (or (deref* destination) (deref* (:destination-var cfg)))
     :media-column media-column
     :branch (deref* branch)
     :routes (mapv #(update % :meta upload-meta) routes)
     :on-file (fn [{:keys [hash]} filename]
                (when (satisfies? IReset rid-output) (reset! rid-output hash))
                (when (satisfies? IReset filename-output) (reset! filename-output filename))
                (when action
                  (submit! action (cond-> (deref-vals (or params {}))
                                    (or file-param (not media-param))
                                    (assoc (or (->kw file-param) :file) filename)
                                    media-param (assoc (->kw media-param) hash)))))
     :on-ok (fn [{:keys [data]}] (when (and refresh data) (refresh data)))})])

(defn- export-widget
  [{:keys [label busy-label params filename mime class disabled? intent]}
   {:keys [descriptor] :as env}]
  [export-button (:frame env) descriptor
   {:label label :busy-label busy-label :params (or params {})
    :filename filename :mime mime :class class :disabled? disabled? :intent (->kw intent)}])

(defn- prop-label-fn [disp]
  (fn [p] (if (keyword? p) (:label (tbl/prop-display (or (get disp p) {}) p)) p)))

(defn- group-label-fn [disp group]
  (if-let [g (->kw group)]
    (let [f (tbl/fmt-by (:fmt (tbl/prop-display (get disp g) g)))]
      (fn [v] (f v (rctx/current))))
    identity))

(defn- ontology-color-of
  [{:keys [rows x names]} look-of]
  (let [k    (or names x)
        look (look-of rows)
        by   (group-by #((tbl/field-fn k) %) rows)]
    (fn [value] (some-> (first (get by value)) (look k) :look :color))))

(defn- chart-widget [cfg env]
  (let [cfg  (cond-> (-> (if (:compare cfg) (compare-chart-cfg cfg) cfg)
                         (update :rows lp/as-rows)
                         (update :layers (fn [ls] (mapv #(update % :rows lp/as-rows) ls))))
               (:ontology-colors? cfg) (as-> c (assoc c :color-of
                                                      (ontology-color-of c (:look-of env)))))
        disp ((:display-of env) (:rows cfg))]
    (if (and (empty? (:rows cfg))
             (empty? (:layers cfg))
             (not-any? #(sequential? (:y %)) (:series cfg)))
      [empty-state {:icon :chart :title (i18n/t :ui/capp-no-data)}]
      [charts/chart-frame (assoc cfg
                                 :prop-label (prop-label-fn disp)
                                 :group-label (group-label-fn disp (:group cfg)))
       env])))

(defn- map-widget [cfg env]
  (let [cfg (-> cfg
                (update :rows lp/as-rows)
                (update :layers (fn [ls] (mapv #(update % :rows lp/as-rows) ls)))
                (update :zoom-to #(some-> % lp/as-rows))
                (update :follow #(some-> % lp/as-rows)))]
    (if (and (empty? (:rows cfg)) (empty? (:layers cfg)) (not (:pick cfg))
             (empty? (:draw-tools cfg)) (not= :plan (->kw (:coordinate-system cfg))))
      [empty-state {:icon :map :title (i18n/t :ui/capp-no-places)}]
      [map-widget/map-view (update cfg :src (:media-src env)) env])))

(defn- property-cards-widget [cfg env]
  (let [{:keys [no-object? items display]} (lp/property-cards (page env) cfg)]
    (if no-object?
      [:div.capp-objectview__empty (i18n/t :ui/property-list-no-object)]
      [kpi-row items {:display display}])))

(defn- comments-entry [cfg {:keys [descriptor refresh] :as env}]
  [comments-widget (:frame env) descriptor refresh (update cfg :items lp/as-rows)])

(defn- button-widget [{:keys [label class disabled? on-click intent]} {:keys [ev-ctx]}]
  (let [f      (on-click-fn on-click ev-ctx)
        locked (blocked-app on-click)]
    [ui/tooltip
     {:content (when locked (i18n/t :ui/capp-app-locked {:app locked}))}
     [ui/button
      {:class    class
       :intent   (:intent (ci/btn-props (or (->kw intent) :none)))
       :icon     (when locked :lock)
       :disabled (boolean (or disabled? locked))
       :on-click (when f #(f nil))}
      label]]))

(defn- hyperlink-widget [{:keys [label intent open-in object] :as cfg} {:keys [descriptor]}]
  (let [row    (or (first (lp/as-rows object)) (:object descriptor))
        target (->kw open-in)]
    (when-let [href (hyperlink/href cfg (or (:values row) row))]
      [ui/anchor-button
       (cond-> (merge (ci/btn-props (or (->kw intent) :none))
                      {:class "capp-hyperlink" :href href})
         (= :new-tab target) (assoc :target "_blank" :rel "noopener noreferrer")
         (= :popup target)
         (assoc :on-click (fn [e]
                            (.preventDefault e)
                            (js/window.open href "_blank"
                                            "popup,noopener,width=1000,height=700"))))
       label])))

(defn- pivot-table-widget [cfg env]
  [pivot-widget/pivot-table (-> (update cfg :rows lp/as-rows) (assoc :env env))])

(defn- metric-item
  [{:keys [sparkline] :as it} env]
  (cond-> (lp/metric-item (page env) it)
    sparkline (assoc :sparkline [metric-widget/sparkline
                                 {:descriptor (:descriptor env) :frame (:frame env)
                                  :spec sparkline}])))

(defn- kpi-row-widget [{:keys [items] :as cfg} env]
  [kpi-row (mapv #(update (metric-item % env) :on-click (partial kpi-click (:app env))) items)
   (select-keys cfg [:display :label :direction :template :size])])

(defn- page-env
  [{:keys [descriptor] :as app} scope]
  {:app           app
   :frame         (:frame app)
   :scope         scope
   :rows-shown    with-rows-shown
   :compare-table object-table-widget
   :action-table  action-table-widget
   :object-view   object-view-widget
   :render-node   render-node
   :scope-view    scope-view
   :display-of    #(rows-display descriptor %)
   :media-src     #(media-src descriptor %)
   :ev-ctx        (ev-ctx app)})

(defn- schema-env
  [{:keys [descriptor open-object refresh]}]
  {:descriptor  descriptor
   :refresh     refresh
   :open-object open-object
   :meta-of     #(oc/descriptor-meta descriptor %)
   :type-links  #(:links (lp/type-entry descriptor :standard-views %))
   :links-of    (fn [rows]
                  (let [t (some-> (first rows) :object-type-ref)]
                    (mapv (comp ->kw :api) (:links (lp/type-entry descriptor :standard-views t)))))
   :title-of    (fn [rows]
                  (let [t  (some-> (first rows) :object-type-ref)
                        tk (when t (lp/type-entry descriptor :object-titles t))]
                    (fn [row]
                      (str (or (when tk ((tbl/field-fn tk) row)) (:title row) (:id row))))))})

(defn- query-env
  [{desc :descriptor f :frame} {:keys [handles derived data*] :as scope}]
  (let [ask!       (fn [ask req] (frame/request! f (assoc req :ask ask)))
        read-ref   (fn [r] (deref* (resolve-ref r handles derived (some-> data* deref))))
        set-values (fn [nm]
                     (into {} (map (fn [h] [h (read-ref {:val h})]))
                           (mapcat #(some-> (sq/set-of derived %) sq/holes)
                                   (sq/parts derived nm))))
        clauses    (fn [nm] (:clauses (sq/set-of derived (first (sq/parts derived nm)))))
        filter-of  (fn [nm] (some (fn [[_ op r]] (when (= :filter (->kw op)) (bind/name-in :val r)))
                                  (clauses nm)))]
    {:set-values     set-values
     :upload-meta    (fn [m] (upload-meta m scope))
     :linked!        (fn [link rows on-ok]
                       (let [got (atom {})]
                         (doseq [row (take 50 rows)]
                           (ask! [client/around! desc {:type (name (:object-type-ref row))
                                                       :id (str (:id row)) :standard true}]
                                 (frame/kept
                                  (fn [a]
                                    (swap! got assoc (:id row)
                                           (get (vars/object-view-payload a row)
                                                (vars/link-key (name link))))
                                    (on-ok (vec (distinct (mapcat lp/as-rows (vals @got)))))))))))
     :applied-filter (fn [nm] (get handles (filter-of nm)))
     :locked-terms   (fn [nm]
                       (into [] (keep (fn [[_ op r :as c]]
                                        (when-not (= :filter (->kw op))
                                          (fm/clause->term c (read-ref r)))))
                             (clauses nm)))
     :facets!        (fn [{nm :set :keys [want describe]} req]
                       (ask! [client/facets! desc {:var nm :want want :describe describe
                                                   :vars (set-values nm)}]
                             req))
     :users!         (fn [body req] (ask! [client/users! desc body] req))
     :series!        (fn [spec req] (ask! [client/series! desc spec] req))
     :around!        (fn [spec req & [opts]] (ask! [client/around! desc spec opts] req))
     :search!        (fn [term rows on-rows]
                       (doseq [nm (vars/cut-of rows)]
                         (ask! [client/search! desc {:var nm :term term :vars (set-values nm)}]
                               (frame/kept #(on-rows (vec (:rows %)))))))}))

(defn- action-env
  [{:keys [descriptor frame] :as app} ctx]
  {:event-fn (fn [ev] (on-click-fn ev ctx))
   :action-label (fn [action] (action-label descriptor action))
   :action-form  (fn [opts]
                   (action-form-widget (assoc opts :cancel-label "Abbrechen") {:app app}))
   :submit!  (fn [action params & [{:keys [then else]}]]
               (frame/request! frame
                               {:ask    [client/submit-strict! descriptor params
                                         {:action action}]
                                :then   (into [[:rf/invoke (submitted app)]] then)
                                :else   else
                                :report (empty? else)}))})

(defn- format-env
  [display-of]
  {:format-of (fn [rows]
                (let [m (display-of rows) c (rctx/current)]
                  (fn [prop]
                    (when-let [f (:fmt (tbl/prop-display (get m (->kw prop)) prop))]
                      (fn [x] (f x c))))))
   :look-of   (fn [rows]
                (let [m (display-of rows)]
                  (fn [row prop]
                    (tbl/prop-look (get m (->kw prop)) prop (:values row)))))
   :prominent-of (fn [rows]
                   (into [] (keep (fn [[p d]] (when (= :prominent (->kw (:visibility d))) p)))
                         (display-of rows)))})

(defn- env-at
  [app scope]
  (let [page (page-env app scope)]
    (merge page
           (schema-env app)
           (query-env app scope)
           (action-env app (:ev-ctx page))
           (format-env (:display-of page)))))

(defn- platform-view
  [{k :view :as cfg} env]
  (if-let [f (page/view-fn k)]
    [f (dissoc cfg :view) env]
    [ui/problem (i18n/t :ui/capp-widget-unavailable) (str "app/layout: no view " (pr-str k))]))

(def ^:private own-widgets
  {:view            platform-view
   :button          button-widget
   :hyperlink       hyperlink-widget
   :property-cards  property-cards-widget
   :bars            (fn [{:keys [items]} _env] [bars (lp/as-rows items)])
   :badge           (fn [cfg env]
                      (let [{:keys [tone text look]} (sourced env cfg :tone :text)]
                        [badge tone text {:look look}]))
   :empty-state     (fn [cfg _env] [empty-state cfg])
   :filter-pills    (fn [{:keys [sel items all-label]} _env]
                      [filter-pills sel (lp/as-rows items) {:all-label all-label}])
   :rtf-text        (fn [{:keys [sel rows placeholder class]} _env]
                      [rtf-text sel {:rows rows :placeholder placeholder :class class}])
   :checkbox        (fn [{:keys [sel label class]} _env]
                      [checkbox sel {:label label :class class}])
   :filter-sidebar  (fn [cfg _env] [filter-sidebar cfg])
   :media-uploader   media-uploader-widget
   :embed            (fn [cfg env] [embed-app cfg env])
   :export           export-widget
   :chart            chart-widget
   :map              map-widget
   :pivot-table      pivot-table-widget
   :kpi-row          kpi-row-widget
   :kpi              (fn [cfg env]
                       (let [{:keys [label value sub accent on-click] :as it} (metric-item cfg env)]
                         [kpi label value sub accent
                          (assoc (select-keys it [:display :look :description :secondary
                                                  :sparkline])
                                 :size (:size cfg)
                                 :on-click (kpi-click (:app env) on-click))]))
   :comments         (fn [cfg env]
                       (if (:object cfg)
                         [comments-widget/comments cfg env]
                         (comments-entry cfg (:app env))))
   :data-freshness   (fn [cfg env] [freshness/data-freshness env cfg])
   :markdown         (fn [{:keys [text fmt] :as cfg} env]
                       (markdown (-> (dissoc cfg :fmt)
                                     (assoc :text ((tbl/fmt-by fmt) text (rctx/current))
                                            :env env))))
   :audio-recorder   (fn [cfg env] [recorder/audio-recorder (assoc cfg :env env)])
   :resource-list    (fn [cfg env]
                       [resource-widgets/resource-list
                        (-> cfg
                            (update :items (fn [items]
                                             (mapv #(cond-> %
                                                      (:thumbnail %)
                                                      (update :thumbnail (:media-src env)))
                                                   items)))
                            (assoc :env env))])
   :linked-resources (fn [cfg env]
                       [resource-widgets/linked-resources
                        (-> cfg (update :objects lp/as-rows) (assoc :env env))])})

(def ^:private views
  (merge own-widgets
         action-widgets/widgets ai-widgets/widgets analysis-widgets/widgets
         button-widgets/widgets custom-widget/widgets device-widgets/widgets dw/widgets
         document-widgets/widgets embed-widgets/widgets
         filter-widgets/widgets flow-widgets/widgets history-widgets/widgets
         inline-action-widget/widgets input-widgets/widgets media-widgets/widgets
         nav-widgets/widgets
         notebook-widget/widgets object-view-widgets/widgets pairing-widget/widgets
         scenario-widgets/widgets vega-widget/widgets))

(def widget-registry
  views)

(defn- shape-of
  [x]
  (cond
    (nil? x)                  "nil"
    (or (keyword? x) (number? x) (boolean? x)) (pr-str x)
    (string? x)               (pr-str (cond-> x (> (count x) 24) (-> (subs 0 24) (str "…"))))
    (bind/variable-ref? x)              (pr-str x)
    (fn? x)                   "<fn>"
    (satisfies? IDeref x)     "<handle>"
    (map? x)                  (str "{" (count x) " keys: "
                                   (str/join " " (map pr-str (take 6 (keys x)))) "}")
    (coll? x)                 (str "<" (count x) " items>")
    :else                     (str "<" (goog/typeOf x) ">")))

(defn leaf-signature
  [{:keys [widget] :as leaf}]
  (let [path (:sutra.capps.catalog/path leaf)
        cfg  (dissoc leaf :widget :box :when :hidden? :sutra.capps.catalog/path)]
    (str ":widget " (pr-str widget)
         (when path (str " @ " (pr-str path)))
         (when (seq cfg)
           (str " — " (str/join ", " (map (fn [[k v]] (str k " " (shape-of v)))
                                          (sort-by key cfg))))))))

(defn- report-widget-error!
  [leaf err]
  (js/console.error "[capp-widget]" (leaf-signature leaf) err)
  (dbg/record! :render-error
               (str "capp-widget " (name (or (:widget leaf) :?)))
               (str (leaf-signature leaf) " — " (.-message err))
               :stack (str (some-> err .-stack))))

(defn- error-card [leaf err]
  [ui/problem (i18n/t :ui/capp-widget-failed)
   (str (name (or (:widget leaf) :widget)) " konnte nicht gezeichnet werden: "
        (.-message err) "\n" (leaf-signature leaf))
   "capp-error--widget"])

(defn- widget-boundary
  [_leaf _child]
  (r/create-class
   {:display-name "capp-widget-boundary"
    :component-did-catch
    (fn [this err _info]
      (let [[_ leaf _] (r/argv this)]
        (report-widget-error! leaf err)
        (r/set-state this {:error err})))
    :component-did-update
    (fn [this old-argv]
      (when (and (:error (r/state this))
                 (not= (nth old-argv 1 nil) (nth (r/argv this) 1 nil)))
        (r/set-state this {:error nil})))
    :reagent-render
    (fn [leaf child]
      (if-let [err (:error (r/state (r/current-component)))]
        (error-card leaf err)
        child))}))

(defn- table-placeholder [{:keys [cols]}] [table/table-skeleton cols])

(def ^:private pending-views
  {:spinner      (fn [_ _] [busy/block {:class "capp-objectview__loading"}
                            (i18n/t :ui/capp-loading)])
   :table        (fn [cfg _] (table-placeholder cfg))
   :kpi          (fn [{:keys [label sub accent display]} _]
                   [kpi label ::display/loading sub accent {:display display}])
   :kpi-row      (fn [{:keys [items display]} env]
                   [kpi-row (mapv (fn [it] (if (some? (:value it))
                                             (sourced env it :accent :value)
                                             (assoc it :value ::display/loading)))
                                  (lp/as-rows items))
                    {:display display}])})

(defn- data-widget
  [{:keys [rows items value] :as leaf} env]
  (let [widget   (->kw (:widget leaf))
        pending? (or (::vars/pending? leaf)
                     (and (::vars/data-pending? leaf) (not (cat/control? widget))))
        leaf     (dissoc leaf ::vars/pending? ::vars/data-pending?)]
    (cond
      (some fn-error? [rows items value])
      [ui/problem (i18n/t :ui/capp-data-failed)
       (str "app/function: " (fn-error-msg (some #(when (fn-error? %) %) [rows items value])))]

      (nil? (get widget-registry widget))
      [ui/problem (i18n/t :ui/capp-widget-unavailable)
       (str "app/layout: unknown :widget " (pr-str widget))]

      pending?
      ((get pending-views (:loading (cat/widget-spec widget) :spinner)) leaf env)

      :else
      (let [body  (try ((get widget-registry widget)
                        (dissoc leaf :widget :sutra.capps.catalog/path) env)
                       (catch :default e
                         (report-widget-error! leaf e)
                         (error-card leaf e)))
            total (some-> rows vars/total-of)]
        (if (and total (not= :object-table widget))
          [:<> [:div.capp-truncated
                (i18n/t :ui/capp-truncated {:shown (count rows) :total total})]
           body]
          body)))))

(declare render-loop render-profiles render-overlay)

(defn- col-attrs [c]
  (let [box                       (:box c)
        drag                      (some-> (:resize box) page/width)
        {:keys [modifiers style]} (lay/column-box (cond-> box drag (assoc :w drag)))]
    {:class (cond-> (apply str "capp-l-col" (map #(str " capp-l-col--" (name %)) modifiers))
              (:resize box) (str " capp-l-col--resize"))
     :style style}))

(defn- shown?
  [c]
  (and (vis/visible? c)
       (or (not (map? c)) (nil? (:min-size c)) (groesse/ab? (:min-size c)))))

(defn- detail-open? [d]
  (boolean (and (satisfies? IReset d) (seq (deref* d)))))

(defn- reveal! [^js e]
  (when (and e (groesse/kompakt?)
             (neg? (.-top (.getBoundingClientRect e))))
    (.scrollIntoView e #js {:block "start"})))

(defn- leave-detail! [how d]
  (case how
    :history-back (.back js/history)
    :clear        (reset! d nil)))

(defn- split-view
  [{:keys [n items open? on-back el-ref env]}]
  (let [d (:detail n)]
    (into [:div {:ref   el-ref
                 :class (cond-> "capp-l-cols"
                          d     (str " capp-l-split")
                          open? (str " capp-l-split--offen"))}]
          (map-indexed
           (fn [i c]
             [:div (col-attrs c)
              (when (and d (= 1 i))
                [ui/button
                 {:class    "capp-l-split__back"
                  :icon     :chevron-left
                  :on-click on-back}
                 (let [l (:back-label n)]
                   (cond (keyword? l) (i18n/t l)
                         (some? l)    l
                         :else        (i18n/t :ui/capp-split-back)))])
              (render-node c env)
              (when-let [k (:resize (:box c))] (page/grip k))])
           (filter shown? items)))))

(defn- split-columns
  [_ _ _]
  (let [mem  (atom {:open? false :tapped? false})
        el   (atom nil)
        step! (fn [this]
                (let [now (detail-open? (:detail (second (r/argv this))))
                      {:keys [state reveal?]} (lay/split-step @mem now)]
                  (reset! mem state)
                  (when reveal? (reveal! @el))))]
    (r/create-class
     {:display-name "capp-split-columns"
      :component-did-mount
      (fn [this]
        (reset! mem {:open? (detail-open? (:detail (second (r/argv this)))) :tapped? false}))
      :component-did-update (fn [this _] (step! this))
      :reagent-render
      (fn [n items env]
        (let [d (:detail n)]
          (split-view
           {:n n :items items :env env :open? (detail-open? d) :el-ref #(reset! el %)
            :on-back (fn [_]
                       (leave-detail! (lay/split-back (:tapped? @mem)
                                                      (view-state/pushes? (state/inner d)))
                                      d))})))})))

(defn- box-frame
  [{:keys [icon help grow? flex border] :as box} body]
  (if (or (lay/framed-box? box) (lay/sized-box? box))
    [:div {:class (cond-> "capp-box"
                    (or flex grow?) (str " capp-box--flex")
                    (:h box)        (str " capp-box--h")
                    (lay/framed-box? box) (str " " (lay/border-class border)))
           :style (lay/box-style box)}
     (when (or icon help)
       [:div.capp-box__head
        (when icon [:span.capp-box__icon icon])
        (when help
          [ui/tooltip {:content help} [:span.capp-box__help "?"]])])
     [:div.capp-box__body body]]
    body))

(defonce inspect
  (r/atom nil))

(defonce ^:private inspector (machine/frame inspect {:name :capp-inspect}))

(machine/reg-event ::inspect (fn [_ [_ v]] {:db v}))
(machine/reg-event ::inspect-selected (fn [{:keys [db]} [_ p]] {:db (assoc db :selected p)}))

(defn inspect!
  [v]
  (machine/dispatch [::inspect v] {:frame inspector}))

(defn inspect-selected! [p] (machine/dispatch [::inspect-selected p] {:frame inspector}))

(defn- pfad-marke
  [_p _art _body]
  (let [an        (atom nil)
        frag      (atom nil)
        markiere! (fn [el]
                    (let [[p art] @an]
                      (when (<= (or (gobj/get el "cappTiefe") -1) (count p))
                        (gobj/set el "cappTiefe" (count p))
                        (.setAttribute el "data-capp-pfad" (pr-str p))
                        (.setAttribute el "data-capp-art" (some-> art name)))))
        melder    #js {:observe markiere! :unobserve (fn [_])}
        ref       (fn [^js f] (reset! frag f) (some-> f (.observeUsing melder)) nil)]
    (r/create-class {:display-name         "capp-pfad"
                     :component-did-update (fn [_]
                                             (when-let [^js f @frag]
                                               (.unobserveUsing f melder)
                                               (.observeUsing f melder)))
                     :reagent-render       (fn [p art body]
                                             (reset! an [p art])
                                             [:<> {:ref ref} body])})))

(defn- inspect-frame [node body]
  (let [p (get node cat/path-key)
        i @inspect
        f (:select i)]
    (cond
      (not p) body
      f       [:div {:class    (cond-> "capp-inspect"
                                 (= p (:selected i)) (str " capp-inspect--sel"))
                     :on-click (fn [e] (.stopPropagation e) (f p))}
               body]
      :else   [pfad-marke p (or (:widget node) (:kind node)) body])))

(defn- bg-frame
  [bg body]
  (if-let [{:keys [class style]} (lay/bg-attrs bg)]
    [:div {:class (str "capp-l-bg " class) :style style} body]
    body))

(defn- render-leaf
  [leaf env]
  (cond
    (and (map? leaf) (:widget leaf))
    (when (vis/visible? leaf)
      (inspect-frame
       leaf
       [widget-boundary leaf
        (bg-frame
         (:bg leaf)
         (box-frame (:box leaf)
                    (let [body (data-widget (dissoc leaf :box :bg :when :hidden? :mount :unmount)
                                            env)]
                      (if (or (:mount leaf) (:unmount leaf))
                        [dw/display-frame (select-keys leaf [:mount :unmount]) body]
                        body))))]))
    (fn? leaf)     [leaf]
    (vector? leaf) leaf
    (nil? leaf)    nil
    :else          (str leaf)))

(defn- section-box
  [_ _ _]
  (let [own (r/atom ::ungesetzt)]
    (fn [{:keys [title icon header-style bg class collapse tag padding]} body env]
      (let [handle?    (satisfies? ISwap collapse)
            collapsed? (if (or handle? (= ::ungesetzt @own))
                         (vis/truthy? collapse false)
                         @own)
            toggle!    (fn [] (if handle?
                                (swap! collapse not)
                                (reset! own (not collapsed?))))]
        [:div {:class (cond-> "capp-l-section"
                        header-style (str " " (lay/header-class header-style))
                        bg           (str " capp-l-bg " (:class (lay/bg-attrs bg)))
                        class        (str " " class))
               :style (:style (lay/bg-attrs bg))}
         (when title
           [:div.capp-l-section__head
            (when collapse
              [ui/button {:class      "capp-l-section__toggle"
                          :minimal    true
                          :small      true
                          :aria-label (if collapsed? "Aufklappen" "Zuklappen")
                          :on-click   toggle!}
               (if collapsed? "▸" "▾")])
            [:div.capp-l-section__title
             (when collapse {:on-click toggle! :style {:cursor "pointer"}})
             (when icon [:span.capp-l-section__icon icon])
             title]
            (when tag [:div.capp-l-section__tag (render-node tag env)])])
         (when-not collapsed?
           [:div (lay/padding-attrs "capp-l-section__body" padding) body])]))))

(defn- section-frame
  [{:keys [title bg class collapse padding] :as opts} body env]
  (if (or title bg class collapse (lay/padding-css padding) (lay/padding-inner padding))
    [section-box opts body env]
    body))

(defn- view-as-switcher
  [view-as viewer]
  (when-let [prof (:profile viewer)]
    [:label.capp-l-viewas
     [:span.capp-l-viewas__label (i18n/t :ui/capp-viewing-as)]
     (into [html-select
            {:class     "capp-l-viewas__select"
             :value     (or (some-> @prof name) "")
             :on-change #(reset! prof (keyword (.. % -target -value)))}]
           (map (fn [{:keys [id label]}] [:option {:value (name id)} (or label (name id))]))
           view-as)]))

(defn- tab-count
  [c]
  (let [c (deref* c)]
    (cond (number? c)                    c
          (or (nil? c) (fn-error? c))    nil
          (coll? c)                      (count (lp/as-rows c)))))

(defn- render-tabs [{:keys [select view-as] :as node} env]
  (let [viewer (:viewer (:app env))
        {tabs* :panes body :body active :active} (lay/active-pane select (:tabs node) viewer)
        panes (mapv #(merge % (od/keep-alive %)) tabs*)]
    [:div.capp-l-tabs
     (when (seq view-as) [view-as-switcher view-as viewer])
     (when-not (:hide-strip? node)
       [tabs select (mapv (fn [p]
                            (let [n (when (contains? p :count) (tab-count (:count p)))]
                              (cond-> (select-keys p [:id :label]) n (assoc :count n))))
                          tabs*)])
     [:div.capp-l-tabs__body
      (if (some #(or (:eager? %) (:never? %)) panes)
        [dw/keep-alive-tabs panes active
         #(render-node % env)]
        (render-node body env))]]))

(defn- id-name [x] (some-> x name))

(defn- app-nav [_ _]
  (let [prev (atom nil)]
    (fn [select items]
      (let [active (id-name (deref* select))
            lit    (lay/nav-lit (into #{} (map (comp id-name :id)) items) active @prev)]
        (reset! prev lit)
        [:nav.capp-l-nav-wrap
         (when-let [here (some #(when (= lit (id-name (:id %))) %) items)]
           [:div.capp-l-nav__here (:label here)])
         (into [ui/tabs
                {:id              :capp-l-nav
                 :class           "capp-l-nav"
                 :animate         false
                 :selected-tab-id lit
                 :on-change       (fn [nm]
                                    (when-let [it (some #(when (= nm (id-name (:id %))) %) items)]
                                      (when (satisfies? IReset select)
                                        (reset! select (:id it)))))}]
               (map (fn [{:keys [id label icon count]}]
                      [ui/tab {:id           (id-name id)
                               :class        "capp-l-nav__item"
                               :aria-current (when (= active (id-name id)) "page")
                               :title        [:<>
                                              (when icon [:span.capp-l-nav__icon icon])
                                              [:span.capp-l-nav__label label]]
                               :tag-content  (when (and (number? count) (pos? count))
                                               count)}]))
               items)]))))

(defn image-src
  [v]
  (let [s (str/trim (str v))]
    (cond (str/blank? s) nil
          (or (str/starts-with? s "/") (str/starts-with? s "data:")
              (str/includes? s "://")) s
          :else (paths/url :kompass-file {:id s}))))

(defn- header-logo
  [{:keys [icon icon-color image height collapsed-image]} folded?]
  (if-let [src (image-src (if folded? (or collapsed-image image) image))]
    [:img.capp-l-header__logo-img {:src src :alt "" :style {:height (lay/css-len (or height 24))}}]
    (when (seq (str icon))
      [:span.capp-l-header__logo-icon (when icon-color {:style {:color icon-color}}) icon])))

(defn- tab-title
  [_]
  (let [before (atom nil)
        put!   (fn [this] (let [t (second (r/argv this))]
                            (when (seq (str t)) (set! (.-title js/document) (str t)))))]
    (r/create-class
     {:component-did-mount    (fn [this] (reset! before (.-title js/document)) (put! this))
      :component-did-update   put!
      :component-will-unmount #(some->> @before (set! (.-title js/document)))
      :reagent-render         (fn [_] nil)})))

(defn- favorite-star
  [_app]
  (kompass/ensure-favorites!)
  (fn [app]
    (let [ref  (kompass/app-favorite-ref app)
          fav  (kompass/favorite-of ref)
          here (rd/value? fav)
          on?  (true? (rd/value-or fav false))]
      [ui/button {:class      "capp-l-header__favorite"
                  :minimal    true
                  :small      true
                  :disabled   (not here)
                  :icon       (if on? :star :star-empty)
                  :title      (rd/reason fav)
                  :aria-label (i18n/t (if on? :ui/capp-unfavorite :ui/capp-favorite))
                  :on-click   #(kompass/toggle-file-favorite! ref)}])))

(def ^:private header-presets #{:1 :2 :3 :4 :5})

(defn- module-header
  [{:keys [header]} _env]
  (let [folded (r/atom (boolean (:collapsed? header)))]
    (fn [{:keys [header title subtitle app back nav select view-as]} env]
      (let [{:keys [orientation bg title-color logo height width collapsible? favorite?
                    items]} header
            vertical? (= :vertical (->kw orientation))
            folded?   (boolean (and vertical? collapsible? @folded))
            pos       (->kw (or (:position logo) (if vertical? :top :left)))
            mark      (when logo (header-logo logo folded?))
            preset    (header-presets (->kw bg))
            at        (fn [ps] (when (and mark (ps pos))
                                 [:div {:class (str "capp-l-header__logo capp-l-header__logo--"
                                                    (name pos))}
                                  mark]))]
        (into
         [page/header
          {:title       (when-not folded? title)
           :subtitle    (when-not folded? subtitle)
           :title-style (when title-color {:color title-color})
           :vertical?   vertical?
           :class       (cond-> nil
                          vertical? (str " capp-l-header--vertical")
                          folded?   (str " capp-l-header--folded")
                          preset    (str " capp-l-bg--" (name preset)))
           :style       (cond-> {}
                          (and bg (not preset))        (assoc :background (str bg))
                          (and height (not vertical?)) (assoc :min-height (lay/css-len height))
                          (and width vertical? (not folded?))
                          (assoc :width (lay/css-len width)))
           :lead        [(when back
                           [ui/button {:class "capp-l-header__back" :minimal true :small true
                                       :on-click (:on-click back)}
                            (if folded? "←" (:label back))])
                         (at #{:left :top})]
           :mid         [(at #{:center})
                         (when (seq nav) [app-nav select nav])
                         (when (and (seq view-as) (not folded?))
                           [view-as-switcher view-as (:viewer (:app env))])]
           :tail        [(at #{:right :bottom})
                         (when (and favorite? app (not folded?)) [favorite-star app])
                         (when (and vertical? collapsible?)
                           [ui/button {:class      "capp-l-header__fold"
                                       :minimal    true
                                       :small      true
                                       :icon       (if folded? :chevron-right :chevron-left)
                                       :aria-label (i18n/t (if folded? :ui/capp-header-expand
                                                               :ui/capp-header-collapse))
                                       :on-click   #(swap! folded not)}])]}]
         (when-not folded? (map #(render-leaf % env) items)))))))

(defn- page-attrs
  [cls padding bg]
  (let [{pc :class ps :style} (lay/padding-attrs cls padding)
        {bc :class bs :style} (lay/bg-attrs bg)]
    {:class (cond-> pc bg (str " capp-l-bg " bc)) :style (merge ps bs)}))

(defn- render-pages
  [{:keys [select header view-as] :as node} {app :app :as env}]
  (let [viewer   (:viewer app)
        {pages* :panes body :body pane :pane} (lay/active-pane select (:pages node) viewer)
        module   (:module app)
        labelled (if (:hide-strip? node) [] (filterv :label pages*))
        nav      (mapv (fn [p]
                         (let [n (when (contains? p :count) (tab-count (:count p)))]
                           (cond-> (select-keys p [:id :label :icon]) n (assoc :count n))))
                       labelled)
        header   (merge (:header module) header)
        vertical? (= :vertical (->kw (:orientation header)))]
    [:div {:class (cond-> "capp-l-app"
                    (seq nav) (str " capp-l-app--nav")
                    (and module (not (:hidden? header)) vertical?) (str " capp-l-app--vertical"))}
     (cond
       (and module (not (:hidden? header)))
       [module-header (assoc module :header header :nav nav :select select :view-as view-as)
        env]

       (and (not module) (or (:title header) (seq labelled) (seq view-as) (:items header)))
       [:div.capp-l-header
        (when (:title header) [:div.capp-l-header__title (:title header)])
        (when (seq nav) [app-nav select nav])
        (when (seq view-as) [view-as-switcher view-as viewer])
        (when (:items header)
          (into [:div.capp-l-header__items] (map #(render-leaf % env)) (:items header)))])
     [:div (page-attrs "capp-l-page" (or (:padding pane) (:padding module)) (:bg pane))
      (render-node body (env-at (assoc app
                                       :module nil
                                       :nav-ids (into #{} (map (comp id-name :id)) nav))
                                (:scope env)))]]))

(defn- render-profiles
  [{:keys [select variants default]} env]
  (let [pick (lay/pick-variant select variants default)]
    [:div.capp-l-profiles (render-node (:body pick) env)]))

(defn- toolbar-children
  [items env]
  (let [action?  #(= :action-button (->kw (:widget %)))
        intents  (:action-intents (:descriptor (:app env)))
        primary? (fn [it] (and (#{:action-button :action-form} (->kw (:widget it)))
                               (not (get intents (->kw (:action it))))))]
    (->> (ci/one-primary primary? items)
         (partition-by action?)
         (mapcat (fn [run]
                   (if (action? (first run))
                     [(into [:div.capp-toolbar__actions] (map #(render-node % env)) run)]
                     (map #(render-node % env) run)))))))

(defn- section-children
  [n items env]
  (let [kids (cond-> []
               (contains? n :body)  (conj (:body n))
               (sequential? items)  (into items))]
    (if (= 1 (count kids))
      (render-node (first kids) env)
      (into [:div.capp-l-rows] (map #(render-node % env)) kids))))

(defn- docking? [item] (and (= :overlay (->kw (:kind item))) (some? (:dock item))))

(defn- render-rows
  [n items env]
  (let [cls   (cond-> "capp-l-rows" (:scroll? n) (str " capp-l-rows--scroll"))
        rows  (into [:div {:class cls}] (map #(render-node % env)) (remove docking? items))
        docks (filter docking? items)]
    (if (seq docks)
      (into [:div.capp-l-dockable rows] (map #(render-node % env)) docks)
      rows)))

(defn- render-node [node {app :app :as env}]
  (cond
    (nil? node)                    nil
    (and (map? node) (:min-size node)
         (not (groesse/ab? (:min-size node)))) nil
    (and (map? node)
         (or (:vars node) (:scenario node))) [scope-view node (:scope env) app]
    (not (lay/layout-node? node))      (render-leaf node env)
    :else
    (let [{:keys [items] :as n} node
          kind                   (->kw (:kind n))
          each                   (map #(render-node % env))]
      (when (and (vis/visible? n) (vis/pane-visible? n (:viewer app)))
        (inspect-frame
         n
         (box-frame (:box n)
                    (section-frame (cond-> n
                                     (= :overlay kind) (dissoc :title)
                                     (= :loop kind)    (dissoc :padding))
                                   (case kind
                                     :rows     (render-rows n items env)
                                     :columns  [split-columns n items env]
                                     :flow     (into [:div.capp-l-flow] each items)
                                     :toolbar  (when-let [items (seq (remove
                                                                      #(or (not (vis/visible? %))
                                                                           (lay/nav-duplicate?
                                                                            % (:nav-ids app)))
                                                                      items))]
                                                 (into [:div.capp-toolbar]
                                                       (toolbar-children items env)))
                                     :section  (section-children n items env)
                                     :tabs     (render-tabs n env)
                                     :pages    (render-pages n env)
                                     :profiles (render-profiles n env)
                                     :loop     (render-loop n env)
                                     :overlay  (render-overlay n env)
                                     [ui/problem (i18n/t :ui/capp-widget-unavailable)
                                      (str "app/layout: unknown :kind " (pr-str kind))])
                                   env)))))))

(defn loop-selection-after-click
  [current row-key control?]
  (if (and (= (str current) (str row-key)) (not control?)) nil row-key))

(def ^:private row-control-selector
  (str "button,a,input,select,textarea,label,"
       "[role=button],[role=combobox],[role=listbox],[role=option]"))

(defn- loop-row-attrs
  [selected row-key]
  (when selected
    (let [current (deref* selected)]
      {:class    (when (and (some? current) (= (str current) (str row-key)))
                   "capp-l-loop__item--selected")
       :on-click (fn [e]
                   (sel/wr! selected
                            (loop-selection-after-click
                             current row-key
                             (some? (.closest (.-target e) row-control-selector)))))})))

(defn- loop-instance [env node-of item selected row-key]
  [:div.capp-l-loop__item
   (merge (objekt-attrs (:descriptor (:app env)) item) (loop-row-attrs selected row-key))
   (render-node (node-of item) env)])

(defn- loop-pages
  [_ _ _]
  (let [page (r/atom 0)]
    (fn [its per-page band]
      (let [n     (count its)
            pages (max 1 (int (Math/ceil (/ n per-page))))
            p     (min @page (dec pages))
            from  (* p per-page)]
        [:div.capp-l-loop-paged
         (band (subvec (vec its) from (min n (+ from per-page))))
         (when (> pages 1)
           [:div.capp-l-loop-pager
            [ui/button {:minimal true :small true :icon "chevron-left"
                        :disabled (zero? p) :on-click #(swap! page dec)}]
            [:span.capp-l-loop-pager__at (str (inc from) "–" (min n (+ from per-page))
                                              " / " n)]
            [ui/button {:minimal true :small true :icon "chevron-right"
                        :disabled (>= p (dec pages)) :on-click #(swap! page inc)}]])]))))

(defn- render-loop
  [{:keys [items key body arrange empty limit max-columns min-card-width group-head selected
           paging per-page padding]
    group-by :group-by group-label :group-label sort-spec :sort :as n}
   env]
  (cond
    (fn-error? items)
    [ui/problem (i18n/t :ui/capp-data-failed) (str "app/function: " (fn-error-msg items))]

    (or (::vars/pending? n) (::vars/data-pending? n))
    [busy/block {:class "capp-objectview__loading"} (i18n/t :ui/capp-loading)]

    :else
    (let [sort*    (cond-> (vec sort-spec)
                     (and (seq sort-spec) (keyword? key)) (conj [key :asc]))
          paged?   (= :paged (->kw paging))
          items    (tbl/sort-limit-rows (lp/as-rows items) sort*
                                        (if paged? lay/loop-page-max limit))
          keyf     (or (tbl/field-fn key) :id)
          arrange  (or (->kw arrange) :rows)
          node-of  (if (fn? body) body #(lay/resolve-item body %))
          instance (fn [item]
                     ^{:key (str (keyf item))}
                     [loop-instance env node-of item selected (keyf item)])
          grid     (when (= :grid arrange)
                     (cond-> {}
                       max-columns    (assoc "--capp-loop-cols" max-columns)
                       min-card-width (assoc "--capp-loop-min" (str min-card-width "px"))))
          band     (fn [its]
                     (let [{:keys [class style]}
                           (lay/padding-attrs (str "capp-l-loop capp-l-loop--" (name arrange))
                                         padding)]
                       (into [:div {:class class :style (merge style grid)}]
                             (map instance its))))
          band     (if paged?
                     (fn [its] [loop-pages its (or per-page 20) band])
                     band)
          heading  (fn [gv its]
                     (cond group-head
                           [(fn [] (render-node (lay/resolve-item group-head (first its)) env))]
                           (fn? group-label)      (group-label gv)
                           (keyword? group-label) ((tbl/fmt-by group-label) gv (rctx/current))
                           :else                  (str gv)))]
      (cond
        (not (seq items)) (render-node empty env)
        group-by          (into [:div.capp-l-loop-groups]
                                (map (fn [[gv its]]
                                       ^{:key (str gv)}
                                       [:div.capp-l-loop-group
                                        [:div {:class (cond-> "capp-l-loop-group__head"
                                                        group-head
                                                        (str " capp-l-loop-group__head--node"))}
                                         (heading gv its)]
                                        (band its)])
                                     (lay/loop-groups items group-by)))
        :else             (band items)))))

(defn- render-overlay
  [{:keys [overlay as side size title on-close body visible backdrop? dock icon header?
           close-on-backdrop?]}
   {ctx :app scope :scope}]
  (let [ov    (:overlays ctx)
        kind  (or (->kw as) :modal)]
    (when (and ov overlay)
      (when (and (some? visible) (not (vars/handle? visible)))
        (warn/warn! (str "app/overlay: " (pr-str overlay) " nennt :visible "
                         (pr-str visible) " — das ist keine Variable. Eine"
                         " Sichtbarkeits-Bindung muss ein `{:var …}` sein; sonst"
                         " bleibt die Klappe an ihrem eigenen Schalter hängen,"
                         " und genau der überlebt ein Neuladen ohne die Auswahl.")))
      (state/bind-visibility! ov overlay (when (vars/handle? visible) visible))
      (when (state/open? ov overlay)
        [app-overlay ov overlay
         {:kind kind :side (or (->kw side) :right) :size (if (number? size) size (->kw size))
          :title title :icon icon :header? header? :close-on-backdrop? close-on-backdrop?
          :backdrop? (not= false backdrop?)
          :docked? (and (= :drawer kind) (some? dock) (groesse/ab? dock))
          :on-close (when on-close (fn [arg] (run-effects! on-close arg (ev-ctx ctx ov))))}
         (fn [arg] (render-node (lay/fill-arg body arg)
                                (env-at (assoc ctx :modal (when (= :modal kind) overlay))
                                        scope)))]))))

(defn layout
  ([tree] (layout tree nil))
  ([tree env] (render-node tree env)))

(defn- page-shell
  [{:keys [title subtitle fill? padding bg header kind device]} module app body env]
  (let [pages?    (= :pages (->kw kind))
        shown?    (not (:hidden? header))
        vertical? (= :vertical (->kw (:orientation header)))
        cls       (str "capp capp--data" (when fill? " capp--fill") (when module " capp--module")
                       (when (= :mobile (->kw device)) " capp--mobile"))
        kids      (if module
                    [[tab-title (:title module)]
                     (if pages?
                       body
                       [:div {:class (cond-> "capp-l-app"
                                       (and shown? vertical?) (str " capp-l-app--vertical"))}
                        (when shown? [module-header (assoc module :header header) env])
                        [:div (page-attrs "capp-l-page" padding bg) body]])]
                    [(when (or title subtitle)
                       [:div.capp-head
                        (when title [ui/h2 title])
                        (when subtitle [:div {:class (ui/cls "text-muted")} subtitle])])
                     body])
        attrs     (assoc (if module {:class cls} (page-attrs cls padding bg))
                         :data-capp-app app)]
    (into [:div attrs] kids)))

(defn scope-view
  [node parent ctx]
  (if (nil? parent)
    [ui/problem (i18n/t :ui/capp-widget-failed)
     "app/scope: no ambient scope — a loop/overlay lost its :vars binding"]
    (r/with-let [own          (when-not (:frame ctx) (frame/capp-frame))
                 f            (or own (:frame ctx))
                 sid          (keyword (gensym "scope"))
                 {:keys [descriptor data* scenario-var]} parent
                 own-var      (bind/name-in :var (:scenario node))
                 scen-var     (or own-var scenario-var)
                 child-data*  (if own-var
                                (doto (frame/cursor f [:scopes sid :data]) (reset! @data*))
                                data*)
                 {:keys [handles derived]} (build-scope {:frame f :sid sid} descriptor
                                                        (:vars node) parent scen-var child-data*)
                 _            (when-let [g (:on-scope parent)] (g handles))
                 own-h        (when own-var (get handles own-var))
                 scen-h       (or own-h (when scen-var (get handles scen-var)))
                 _            (when own-h
                                (frame/send! f [:capp/scope sid
                                                {:var  own-var :door [client/refresh! descriptor]
                                                 :into [:scopes sid :data]}
                                                handles])
                                (when-let [sc (not-empty (str (some-> own-h deref)))]
                                  (frame/send! f [:capp/set-var sid own-var nil sc nil])))
                 child-scope  (assoc parent :handles handles :derived derived
                                     :data* child-data* :scenario-var scen-var)
                 child-ctx    (cond-> (assoc ctx :handles handles :frame f)
                                own-h (assoc :refresh #(when % (reset! child-data* %))))
                 env-now      (fn [] (let [sc (some-> scen-h deref)
                                           d  (cond-> descriptor sc (assoc :scenario sc))]
                                       (env-at (assoc child-ctx :descriptor d)
                                               (assoc child-scope :descriptor d))))
                 env          (when-not scen-h (env-at child-ctx child-scope))]
      (render-node (vars/resolve-scoped (dissoc node :vars :scenario)
                                        handles derived @child-data*)
                   (or env (env-now)))
      (finally (if own (frame/leave! own) (frame/send! f [:capp/drop-scope sid]))))))

(defn object-opener
  [ov]
  (fn [t id]
    (if (object-view/object-view-surface?)
      (shell/open-object! t id)
      (when (and ov t (some? id))
        (state/open! ov ::object {:type t :id (str id)})))))

(defn- held-row
  [descriptor data t id]
  (some #(when (= (str (:id %)) id) %) (get data (lp/viewed-type descriptor {:object-type-ref t}))))

(defn- drawn-object
  [ctx _scope _t _id]
  (let [own (when-not (:frame ctx) (frame/capp-frame))]
    (fn [ctx scope t id]
      (let [ctx  (cond-> ctx own (assoc :frame own))
            desc (:descriptor ctx)
            held (held-row desc (some-> (:data* scope) deref) t id)
            {:keys [data error loading?]}
            (when (and (nil? held) (some? id))
              (frame/read! ctx ::drawn [id t (client/page-generation desc)]
                           [client/around! desc {:type (if (keyword? t) (name t) (str t))
                                                 :id id :standard true}]))
            row  (or held (when (= id (some-> data :object :id str)) (:object data)))]
        [:<>
         (when own [frame/leaving own])
         (cond
           error      [ui/problem (i18n/t :ui/capp-data-failed) error]
           (and data (not loading?) (nil? (:object data)))
           [ui/problem (i18n/t :ui/capp-data-failed) (i18n/t :ui/property-list-no-object)]
           (nil? row) [busy/block {:class "capp-objectview__loading"} (i18n/t :ui/capp-loading)]
           :else      ((get widget-registry :object-view) {:object [row]}
                                                          (env-at ctx scope)))]))))

(defn object-drawer
  [ov ctx scope]
  (when (state/open? ov ::object)
    (let [{:keys [type]} (state/overlay-arg ov ::object)
          desc (:descriptor ctx)]
      [app-overlay ov ::object
       {:kind :drawer :side :right :size :lg
        :title (oc/type-label (oc/descriptor-meta desc type))}
       (fn [{:keys [type id]}]
         [drawn-object ctx scope type id])])))

(defn- page-refresh-hook
  [capp now]
  (r/with-let [f        (fn [^js ev]
                          (.preventDefault ev)
                          (frame/send! capp [:capp/reload (now)]))
               browser? (exists? js/window)
               _        (when browser? (.addEventListener js/window "sutra:data-changed" f))]
    nil
    (finally (when browser? (.removeEventListener js/window "sutra:data-changed" f)))))

(defn- device-hook
  [device]
  (r/with-let [mobile? (= :mobile (->kw device))
               _       (when mobile? (groesse/mobil! true))]
    nil
    (finally (when mobile? (groesse/mobil! false)))))

(defn- live-watch
  [capp live sets hidden]
  (r/with-let [verdeckt (atom hidden)
               k        [:live sets]
               _        (machine/fx! {:frame capp}
                                     [:capp/watch {:key  k :on [:capp/live-changed]
                                                   :spec {:types        (map name (:types live))
                                                          :object-sets  sets
                                                          :min-seconds  (:min-seconds live)
                                                          :keep-paused? true
                                                          :verdeckt     verdeckt}}])]
    (do (when (not= hidden @verdeckt) (reset! verdeckt hidden))
        nil)
    (finally (machine/fx! {:frame capp} [:capp/unwatch k]))))

(defn- watched-sets
  [{:keys [object-sets]} vars-of on-screen handles derived data descriptor]
  (let [named (:object-names descriptor)
        whole #(mapv (fn [t] {:type "base" :objectType (name t)}) (distinct (vals named)))
        this  (:object descriptor)
        target (fn [ref]
                 (if (and (map? ref) (contains? ref :this))
                   (let [p (->kw (:this ref))]
                     (if (= :id p)
                       (str (:id this))
                       (get (fmt/v this) p (get this p))))
                   (vars/resolve-ref ref handles derived data)))
        each   (mapv (fn [s]
                       (let [spec (if (map? s) s (get vars-of s))
                             f    (or (:source spec) (:from spec))]
                         {:shown? (or (map? s) (contains? on-screen s))
                          :sets   (if (and f (not (get handles f)) (not (get derived f)))
                                    [(vars/watched-set (or (get named f) f) spec target)]
                                    (whole))}))
                     object-sets)
        shown  (into #{} (comp (filter :shown?) (mapcat :sets)) each)
        sets   (into [] (comp (mapcat :sets) (distinct)) each)]
    {:sets sets :hidden (into #{} (remove shown) sets)}))

(defn- as-wire [m] (js->clj (js/JSON.parse (js/JSON.stringify (clj->js m)))))

(defn- app-db
  [descriptor data tree scenario-var]
  (let [reload (:reload descriptor)]
    (cond-> {:data        data
             :given       data
             :asking      (when (nil? data) :first)
             :asked       0
             :generation  0
             :held        (when (and data (contains? descriptor :data-vars))
                            (:data-vars descriptor))
             :inputs      (into [] (keep ->kw) (:data-inputs descriptor))
             :door        [(or (:refresh-fn descriptor) client/refresh!) descriptor]
             :module-vars (into #{} (keep (fn [[nm sp]] (when (:input? sp) nm))) (:vars tree))
             :handed-in   (get-in descriptor [:layout :module-inputs])
             :emit        (get-in descriptor [:layout :module-emit])}
      reload       (assoc :reload [reload])
      scenario-var (assoc-in [:scopes :root :scenario]
                             {:var  scenario-var :door [client/refresh! descriptor]
                              :into [:data]}))))

(defn render-data-app
  [descriptor data]
  (client/auto-refresh! true)
  (let [tree         (cond-> (:layout descriptor) (not (:platform? descriptor)) cat/instrument)
        scenario-var (bind/name-in :var (:scenario tree))
        db0          (app-db descriptor data tree scenario-var)
        capp         (frame/capp-frame db0)
        data*        (frame/cursor capp [:data])
        pending      (machine/subscribe [:capp/pending?] {:frame capp})
        generation   (frame/cursor capp [:generation])
        {:keys [handles derived]} (build-scope {:frame capp :sid :root}
                                               (assoc descriptor :app/generation generation)
                                               (:vars tree) nil scenario-var data*)
        scenario-h   (when scenario-var (get handles scenario-var))
        input-vals   (fn []
                       (into {} (keep (fn [nm] (when-let [h (get handles nm)] [nm (deref* h)])))
                             (:inputs db0)))
        now          (fn [] {:vars (input-vals) :scenario (some-> scenario-h deref)})
        role         (some-> (get-in descriptor [:viewer :role]) keyword)
        viewer       {:role role :profile (r/atom role)}
        ov           (state/overlays capp)]
    (frame/send! capp [:capp/start handles now
                       (when (or (nil? data)
                                 (and (contains? descriptor :data-vars)
                                      (not= (as-wire (input-vals))
                                            (as-wire (:data-vars descriptor)))))
                         (now))])
    (r/create-class
     {:display-name "capp-data-app"
      :component-did-update
      (fn [this _]
        (let [[_ descriptor data] (r/argv this)
              db     @(:db capp)
              herein (get-in descriptor [:layout :module-inputs])
              emit   (get-in descriptor [:layout :module-emit])]
          (when (and (some? data) (not (identical? data (:given db))))
            (frame/send! capp [:capp/payload data]))
          (when (or (not= herein (:handed-in db)) (not (identical? emit (:emit db))))
            (frame/send! capp [:capp/module herein emit handles]))))
      :component-will-unmount (fn [_] (frame/leave! capp))
      :reagent-render
      (fn [descriptor _data]
        (let [sc    (some-> scenario-h deref)
              desc  (cond-> (assoc descriptor :app/input-vals input-vals
                                   :app/generation generation)
                      sc (assoc :scenario sc))
              app   {:descriptor  desc
                     :refresh     #(frame/send! capp [:capp/refresh % (now)])
                     :more        #(frame/send! capp [:capp/more %1 %2 (now)])
                     :overlays    ov
                     :open-object (object-opener ov)
                     :handles     handles
                     :viewer      viewer
                     :frame       capp}
              scope {:descriptor desc :handles handles :derived derived
                     :data* data* :scenario-var scenario-var}]
          (binding [vars/*data-pending?* @pending]
            (let [tree*  (resolve-for-object (dissoc tree :vars :scenario :module-inputs
                                                     :module-emit)
                                             (:object desc)
                                             handles derived @data*)
                  module (when-let [m (:app/module desc)]
                           {:title    (lay/module-title tree* (:label m))
                            :subtitle (:subtitle tree*)
                            :app      (:name desc)
                            :back     (:back m)
                            :padding  (:padding tree*)})
                  env    (env-at (assoc app :module (when (= :pages (->kw (:kind tree*))) module))
                                 scope)]
              (view-state/live! (url-keys-on-screen tree handles))
              (page-shell (select-keys tree* [:title :subtitle :fill? :padding :bg :header :kind
                                              :device])
                          module
                          (some-> (:name desc) name)
                          [:<>
                           [page-refresh-hook capp now]
                           ^{:key (str (:device tree*))} [device-hook (:device tree*)]
                           (when (seq (get-in desc [:live :object-sets]))
                             (let [live  (:live desc)
                                   specs (vars/all-var-specs tree)
                                   shown (vars/live-vars tree specs
                                                         #(some-> (get handles %) deref*))
                                   {:keys [sets hidden]}
                                   (watched-sets live specs shown handles derived @data* desc)]
                               ^{:key sets}
                               [live-watch capp live sets hidden]))
                           (version-banner (:version desc))
                           (defect-banner @data*)
                           (render-node (dissoc tree* :title :subtitle :fill? :padding :bg) env)
                           [object-drawer ov app scope]]
                          env)))))})))
