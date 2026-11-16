(ns sutra-ui.apphost.sdk
  (:require [sutra-ui.ui.overlay :as overlay]
            [sutra-ui.apphost.uploader :as uploader]
            [sutra-ui.ui.table :as table]
            [sutra-ui.ui.core :as ui]
            [sutra-ui.ui.filter :as ui-filter]
            [sutra-ui.apphost.widgets.action :as action-widgets]
            [sutra-ui.ui.display :as display]
            [sutra-ui.apphost.modules :as mod]
            [sutra-ui.apphost.interop :as interop]
            [sutra-ui.apphost.state :as state]
            [sutra-ui.apphost.client :as client]
            [sutra.capps.chart-spec :as chart-spec]
            [sutra.capps.format :as fmt]
            [sutra-ui.render-ctx :as rctx]
            [sutra.types.decimal :as d]
            [sutra-ui.ui.inputs :as inputs]
            [sutra-ui.apphost.charts :as charts]
            [sutra-ui.apphost.widgets.map :as map-widget]
            [sutra-ui.apphost.widgets.markdown :as md]
            [sutra-ui.apphost.widgets.media-viewer :as media-ns]
            [sutra-ui.apphost.filters :as filters]
            [sutra.capps.layout :as lay]
            [sutra-ui.apphost.render :as render]))

(def ->js interop/->js)
(def ->clj interop/->clj)
(def event-value interop/event-value)
(def event-checked interop/event-checked)
(def event-key interop/event-key)
(def prevent-default! interop/prevent-default!)
(def stop-propagation! interop/stop-propagation!)
(def view-state state/view-state)
(def vars state/vars)
(def overlays state/overlays)
(def open? state/open?)
(def overlay-arg state/overlay-arg)
(def open! state/open!)
(def close! state/close!)
(def toggle! state/toggle!)
(def close-all! state/close-all!)
(def module mod/module)
(def use-app mod/use-app)
(def embed mod/embed)
(def app-url client/app-url)
(def refresh! client/refresh!)
(def watch!   client/watch!)
(def submit! client/submit!)
(def upload! client/upload!)
(def aggregate! client/aggregate!)
(def function! client/function!)
(def series! client/series!)
(def blob-url client/blob-url)
(def export! client/export!)
(def scenarios-list client/scenarios-list)
(def scenario-create! client/scenario-create!)
(def scenario-drop! client/scenario-drop!)
(def v fmt/v)
(def links fmt/links)
(def link-index fmt/link-index)
(def num fmt/num)
(def calc fmt/calc)
(def lossy-number d/lossy-number)
(defn fmt [x dec] (fmt/fmt x dec (rctx/current)))
(defn eur [x] (fmt/eur x (rctx/current)))
(defn eur0 [x] (fmt/eur0 x (rctx/current)))
(defn eur1 [x] (fmt/eur1 x (rctx/current)))
(defn prozent
  ([x] (fmt/prozent x (rctx/current)))
  ([x dec] (fmt/prozent x dec (rctx/current))))
(defn menge [x] (fmt/menge x (rctx/current)))
(defn auto-fmt [x] (fmt/auto x (rctx/current)))
(defn datum [x] (fmt/datum x (rctx/current)))
(def monate fmt/monate)
(defn monat-name [m] (fmt/monat-name m (rctx/current)))
(defn i18n
  ([dict] (fmt/i18n dict (rctx/current)))
  ([dict fallback] (fmt/i18n dict (rctx/current) fallback)))
(def kpi display/kpi)
(def badge display/badge)
(def action-button action-widgets/action-button)
(def num-class display/num-class)
(def bars display/bars)
(def tabs display/tabs)
(def filter-pills ui-filter/filter-pills)
(def stepper display/stepper)
(def kpi-row display/kpi-row)
(def timeline display/timeline)
(def empty-state ui/empty-state)
(def pivot-table table/pivot-table)
(def comments display/comments)
(def media-uploader uploader/media-uploader)
(def modal overlay/modal)
(def overlay render/app-overlay)
(def object-table table/object-table)
(def text-input inputs/text-input)
(def number-input inputs/number-input)
(def date-input inputs/date-input)
(def select-input inputs/select-input)
(def combo-input inputs/combo-input)
(def checkbox inputs/checkbox)
(def chart charts/chart)
(def chart-spec chart-spec/chart->spec)
(def chart-palette chart-spec/palette)
(def map-view map-widget/map-view)
(def markdown md/markdown)
(def iframe media-ns/iframe)
(def media media-ns/media)
(def filter-sidebar filters/filter-sidebar)
(def widget-registry render/widget-registry)
(def layout render/layout)
(def data-layout? lay/data-layout?)
(def render-data-app render/render-data-app)
