(ns sutra-ui.apphost.widgets.document
  (:require [sutra-ui.ui.sel :as sel]
            [clojure.string :as str]
            [reagent.core :as r]
            [sutra-ui.ui.core :as ui]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.capps.layout :as lay]
            [sutra.capps.leaf-props :as lp]
            [sutra.capps.media-view :as mv]
            [sutra.capps.pdf-layers :as pdf-layers]
            [sutra.capps.table :as tbl]
            [sutra.capps.variables :refer [deref*]]
            [sutra.types.value-types :as vt]
            [sutra-ui.object.card :as oc]
            [sutra-ui.apphost.frame :as frame]
            [sutra-ui.apphost.toast :as toast]
            [sutra-ui.apphost.events :refer [on-click-fn]]
            [sutra-ui.apphost.widgets.pdf :refer [pdf]]
            [sutra-ui.apphost.widgets.polygon :refer [polygon]]
            [sutra-ui.apphost.page-ctx :refer [ev-ctx media-src rows-display]]))

(defn- submit!
  [{:keys [descriptor frame]} params action on-ok on-error]
  (frame/request! frame (frame/submit-request descriptor params
                                              {:action action :on-ok on-ok :on-error on-error})))

(defn- polygon-save-fn
  [{:keys [refresh] :as app} {:keys [action shape-param points-param label-param shape-id]}]
  (fn [changes done]
    (let [id-fn  (tbl/field-fn (or shape-id :id))
          fehler (atom [])
          letzte (atom nil)]
      (-> (reduce
           (fn [pr {:keys [row points label]}]
             (.then pr
                    (fn [_]
                      (js/Promise.
                       (fn [res _]
                         (submit! app
                                  (cond-> {(->kw (or shape-param :object))  (id-fn row)
                                           (->kw (or points-param :points)) points}
                                    label-param (assoc (->kw label-param) label))
                                  action
                                  (fn [d] (reset! letzte d) (res nil))
                                  (fn [m] (swap! fehler conj m) (res nil))))))))
           (js/Promise.resolve nil)
           changes)
          (.then (fn [_]
                   (when (and refresh @letzte) (refresh @letzte))
                   (when (seq @fehler)
                     (js/console.error "app/polygon speichern:" (str/join " · " @fehler)))
                   (done @fehler)))))))

(defn- polygon-remove-fn
  [{:keys [refresh] :as app} {:keys [remove-action shape-param shape-id]}]
  (let [id-fn (tbl/field-fn (or shape-id :id))]
    (fn [row done]
      (submit! app {(->kw (or shape-param :object)) (id-fn row)} remove-action
               (fn [d] (when (and refresh d) (refresh d)) (done nil))
               (fn [m] (done m))))))

(defn- boxes-save-fn
  [{:keys [refresh] :as app} {:keys [action object-param boxes-param key]}]
  (let [kf (tbl/field-fn (or key :id))]
    (fn [row boxes]
      (submit! app {(->kw (or object-param :object)) (kf row)
                    (->kw (or boxes-param :boxes))   boxes}
               action
               (fn [d] (when (and refresh d) (refresh d)))
               (fn [m] (toast/toast! m {:error? true}))))))

(defn- with-selection-output
  [{:keys [selected key on-select] :as layer}]
  (if-not selected
    layer
    (let [kf (or (tbl/field-fn key) (tbl/field-fn :id))]
      (assoc layer
             :active (mapv kf (lp/as-rows (deref* selected)))
             :on-select (fn [row]
                          (sel/wr! selected [row])
                          (when (fn? on-select) (on-select row)))))))

(defn- layer-rows [app layers]
  (let [offered (:action-params (:descriptor app))]
    (mapv #(with-selection-output
             (cond-> (-> % (update :rows lp/as-rows) (update :active deref*))
               (contains? % :on-select)
               (update :on-select (fn [evs]
                                    (or (when (seq evs) (on-click-fn evs (ev-ctx app))) false)))
               (some->> (:action %) ->kw (contains? offered))
               (assoc :on-boxes (boxes-save-fn app %))))
          layers)))

(defn- area-text
  [{:keys [area-kind]} page box]
  (if (= :band (some-> area-kind keyword))
    (pdf-layers/band-text page box)
    (pdf-layers/box-text page box)))

(defn- area-fn
  [app {:keys [on-area area-page area-boxes collect? create] :as cfg}]
  (let [f (on-click-fn on-area (ev-ctx app))]
    (when (or f collect? (seq create))
      (fn [{:keys [page box]}]
        (let [had   (when collect? (vec (pdf-layers/boxes-of (deref* area-boxes))))
              boxes (conj (or had []) (area-text cfg page box))]
          (when-not (seq had) (sel/wr! area-page page))
          (sel/wr! area-boxes boxes)
          (when f (f {:values {:page page :boxes boxes}})))))))

(defn- area-drafts
  [{:keys [collect? area-boxes area-page]}]
  (when collect?
    (vec (keep-indexed (fn [i s]
                         (when-let [b (pdf-layers/parse-box s)]
                           {:i i :page (or (:page b) (deref* area-page)) :box b}))
                       (pdf-layers/boxes-of (deref* area-boxes))))))

(defn- search-kind
  [desc {:keys [prop type]}]
  (when prop
    (let [t  (lp/viewed-type desc (if type {:object-type-ref type} (:object desc)))
          vt (some-> (lp/display-entry desc t prop) :type ->kw)]
      (cond (#{:date :timestamp} vt) :datum
            (vt/numeric? vt)         :betrag
            vt                       :text))))

(defn- typed-search
  [desc search]
  (mapv (fn [x]
          (if-let [src (and (map? x) (::lay/source x))]
            (cond-> (dissoc x ::lay/source)
              (search-kind desc src) (assoc :kind (search-kind desc src)))
            x))
        search))

(defn- pdf-frame [_]
  (let [voll (r/atom false)]
    (fn [body]
      [:div {:class (cond-> "capp-pdf-frame" @voll (str " capp-pdf-frame--voll"))}
       [:div.capp-pdf-frame__bar
        [ui/button
         {:class    "capp-pdf-frame__toggle"
          :icon     (if @voll :cross :maximize)
          :on-click #(swap! voll not)}
         (if @voll "Schließen" "Ganzer Beleg")]]
       body])))

(defn- pdf-widget [{:keys [annotations on-select area-boxes] :as cfg} {:keys [app] :as env}]
  (let [desc (:descriptor app)]
    [pdf-frame
     [pdf (cond-> (assoc cfg
                         :env env
                         :src (media-src desc (mv/source cfg))
                         :search (cond->> (:search cfg)
                                   (sequential? (:search cfg)) (typed-search desc))
                         :annotations (into (lp/as-rows annotations)
                                            (pdf-layers/marks
                                             (mapv #(assoc % :display
                                                           (rows-display desc (:rows %)))
                                                   (layer-rows app (:layers cfg)))))
                         :on-select (on-click-fn on-select (ev-ctx app))
                         :on-area (area-fn app cfg)
                         :drafts (area-drafts cfg)
                         :on-remove-draft
                         (when (:collect? cfg)
                           (fn [i]
                             (let [v (vec (pdf-layers/boxes-of (deref* area-boxes)))]
                               (sel/wr! area-boxes (into (subvec v 0 i) (subvec v (inc i)))))))
                         :on-change-draft
                         (when (:collect? cfg)
                           (fn [i page box]
                             (let [v (vec (pdf-layers/boxes-of (deref* area-boxes)))]
                               (sel/wr! area-boxes (assoc v i (area-text cfg page box)))))))
            (contains? cfg :draw) (update :draw deref*))]]))

(defn- polygon-widget
  [{:keys [shapes on-close on-shape-click on-import action remove-action] :as cfg} {:keys [app]}]
  (let [{:keys [descriptor refresh overlays]} app
        rows (lp/as-rows shapes)
        t    (some-> (first rows) :object-type-ref)
        m    (when t (oc/descriptor-meta descriptor t))
        save (select-keys cfg [:action :shape-param :points-param :label-param :shape-id
                               :remove-action])]
    [polygon (assoc cfg
                    :src (media-src descriptor (:src cfg))
                    :shape-noun (when (:display-name m) [(:display-name m) (:plural m)])
                    :shapes rows
                    :stack (lp/as-rows (:stack cfg))
                    :layers (layer-rows app (:layers cfg))
                    :on-close (on-click-fn on-close (ev-ctx app overlays))
                    :on-shape-click (on-click-fn on-shape-click (ev-ctx app overlays))
                    :on-import (on-click-fn on-import (ev-ctx app overlays))
                    :on-save (when action (polygon-save-fn app save))
                    :on-remove (when remove-action
                                 (polygon-remove-fn app save)))]))

(def widgets
  {:pdf     pdf-widget
   :polygon polygon-widget})
