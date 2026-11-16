(ns sutra.capps.catalog.data)

(def widgets
  [{:widget :object-table :label "Objekt-Tabelle" :group :daten
    :loading :table
    :params [{:key :rows :kind :object-set :required true}
             {:key :title :kind :text}
             {:key :cols :kind :cols}
             {:key :sort :kind :edn}
             {:key :limit :kind :number}
             {:key :row-key :kind :prop}
             {:key :on-row-click :kind :event}
             {:key :active :kind :handle}
             {:key :auto-active? :kind :bool :default true}
             {:key :multi-select? :kind :bool}
             {:key :selected :kind :handle}
             {:key :row-class :kind :edn}
             {:key :row-style :kind :edn}
             {:key :foot :kind :edn}
             {:key :compare :kind :value :binds #{:val}
              :needs "den Wert einer Vergleichsvariablen {:val …}"}
             {:key :wrap? :kind :bool}
             {:key :lines :kind :number}
             {:key :frozen-cols :kind :number}
             {:key :fit-columns? :kind :bool}
             {:key :empty :kind :edn}
             {:key :no-value :kind :text}
             {:key :narrow-headers? :kind :bool}
             {:key :whole-cell? :kind :bool}
             {:key :user-sort? :kind :bool :default true}
             {:key :sort-column :kind :edn}
             {:key :visible-cols :kind :handle}
             {:key :column-config? :kind :bool}
             {:key :hide-column-config? :kind :bool}
             {:key :separate-types? :kind :bool}
             {:key :icon-prop :kind :prop}
             {:key :export-csv? :kind :bool}
             {:key :export-excel? :kind :bool}
             {:key :custom-export-names? :kind :bool}
             {:key :export-name :kind :text}
             {:key :context-menu :kind :edn}
             {:key :right-clicked :kind :handle}
             {:key :action :kind :action}
             {:key :edit-mode :kind :enum :options [:button :always] :default :button}
             {:key :edit-label :kind :text}
             {:key :confirm-submit? :kind :bool}]}
   {:widget :object-list :label "Objekt-Liste" :group :daten
    :loading :spinner
    :params [{:key :rows :kind :object-set :required true}
             {:key :props :kind :cols}
             {:key :title :kind :text}
             {:key :show-count? :kind :bool}
             {:key :display :kind :enum :options [:list :grid] :default :list}
             {:key :grid :kind :edn}
             {:key :property-style :kind :enum :options [:inline :aligned] :default :inline}
             {:key :hide-null? :kind :bool}
             {:key :empty :kind :edn}
             {:key :sort :kind :edn}
             {:key :user-sort? :kind :bool}
             {:key :sort-props :kind :edn}
             {:key :row-key :kind :prop}
             {:key :active :kind :handle}
             {:key :auto-active? :kind :bool :default true}
             {:key :multi-select? :kind :bool}
             {:key :selected :kind :handle}
             {:key :on-select :kind :event}
             {:key :media :kind :edn}
             {:key :reorder :kind :edn}]}
   {:widget :links :label "Verknüpfungen" :group :daten
    :loading :spinner
    :params [{:key :rows :kind :object-set :required true}
             {:key :links :kind :edn}
             {:key :expand :kind :number}
             {:key :explore? :kind :bool}
             {:key :on-explore :kind :event}
             {:key :open-view? :kind :bool}
             {:key :preview? :kind :bool}
             {:key :scenario :kind :handle}
             {:key :empty :kind :text}]}
   {:widget :object-set-title :label "Objektmengen-Titel" :group :daten
    :params [{:key :rows :kind :object-set :required true}
             {:key :single? :kind :bool}
             {:key :show-icon? :kind :bool}
             {:key :drag? :kind :bool}
             {:key :title :kind :text}
             {:key :render-empty? :kind :bool}
             {:key :empty-type :kind :object-type}]}
   {:widget :header-text :label "Kopfzeile" :group :kennzahl
    :params [{:key :title :kind :text :required true}
             {:key :subtitle :kind :text}]}
   {:widget :divider :label "Trennlinie" :group :kennzahl
    :params [{:key :label :kind :text}
             {:key :spacing :kind :enum :options [:s :m :l] :default :m}]}
   {:widget :pivot-table :label "Pivot-Tabelle" :group :daten
    :loading :table
    :params [{:key :rows :kind :object-set :required true}
             {:key :row :kind :prop :unless :row-groups}
             {:key :row-groups :kind :items
              :item [{:key :prop :kind :prop :required true}
                     {:key :label :kind :text}
                     {:key :label-prop :kind :prop}
                     {:key :interval :kind :enum
                      :options [:exact :date :week :month :quarter :year]}
                     {:key :width :kind :number}
                     {:key :sort :kind :enum :options [:asc :desc]}]}
             {:key :col :kind :prop}
             {:key :col-interval :kind :enum :options [:exact :date :week :month :quarter :year]}
             {:key :col-sort :kind :enum :options [:asc :desc]}
             {:key :value :kind :prop :unless :aggs}
             {:key :agg :kind :enum :options [:sum :count :avg :min :max :cardinality]
              :default :sum}
             {:key :aggs :kind :items
              :item [{:key :prop :kind :prop}
                     {:key :agg :kind :enum :options [:sum :count :avg :min :max :cardinality]}
                     {:key :title :kind :text}
                     {:key :fmt :kind :fmt}
                     {:key :width :kind :number}]}
             {:key :placement :kind :enum :options [:columns :rows]}
             {:key :row-label :kind :prop}
             {:key :col-label :kind :prop}
             {:key :cell :kind :edn}
             {:key :corner :kind :text}
             {:key :totals? :kind :bool :default true}
             {:key :row-totals? :kind :bool}
             {:key :col-totals? :kind :bool}
             {:key :title :kind :text}
             {:key :selected :kind :handle}
             {:key :selected-rows :kind :handle}
             {:key :on-select :kind :event}
             {:key :group-rows :kind :edn}
             {:key :group-col :kind :prop}
             {:key :value-fields :kind :edn}
             {:key :expandable :kind :edn}
             {:key :layout :kind :enum :options [:tabular :stacked]}
             {:key :stacked-label :kind :text}
             {:key :fill-width? :kind :bool}
             {:key :table-style :kind :enum :options [:basic :outlined :banded]}
             {:key :color :kind :text}
             {:key :color-mode :kind :enum :options [:minimal :prominent]}]}
   {:widget :timeline :label "Zeitstrahl" :group :daten
    :loading :spinner
    :params [{:key :items :kind :object-set :required true :unless :layers}
             {:key :layers :kind :items :row :unknown
              :item [{:key :rows :kind :object-set :required true}
                     {:key :at :kind :prop :required true}
                     {:key :title :kind :text}
                     {:key :title-prop :kind :prop}
                     {:key :body :kind :value}
                     {:key :tone :kind :enum :options [:ok :warn :danger :info]}
                     {:key :label :kind :text}
                     {:key :title-mode :kind :enum :options [:object :property :custom]}
                     {:key :prominent? :kind :bool}
                     {:key :color :kind :text}
                     {:key :conditional-format :kind :edn}
                     {:key :icon :kind :text}
                     {:key :on-select :kind :event}]}
             {:key :orientation :kind :enum :options [:vertical :horizontal]}
             {:key :order :kind :enum :options [:oldest :newest]}
             {:key :legend? :kind :bool}
             {:key :gap-tooltip? :kind :bool}
             {:key :active :kind :handle}
             {:key :highlight? :kind :bool}
             {:key :on-select :kind :event}]}
   {:widget :status-tracker :label "Statusverlauf" :group :daten
    :loading :spinner
    :params [{:key :rows :kind :object-set :required true}
             {:key :status :kind :prop :required true}
             {:key :at :kind :prop :required true}
             {:key :label :kind :prop}
             {:key :statuses :kind :edn}]}
   {:widget :action-log-timeline :label "Action-Log-Zeitstrahl" :group :daten
    :loading :spinner
    :params [{:key :objects :kind :object-set :required true}
             {:key :logs :kind :items :required true
              :item [{:key :label :kind :text}
                     {:key :rows :kind :object-set :required true}
                     {:key :target-prop :kind :prop :required true}
                     {:key :at :kind :prop :required true}
                     {:key :properties :kind :edn}]}]}
   {:widget :gantt :label "Gantt-Diagramm" :group :daten
    :loading :spinner
    :params [{:key :layers :kind :items :required true :row :unknown
              :item [{:key :name :kind :text}
                     {:key :rows :kind :object-set :required true}
                     {:key :event-type :kind :enum :options [:range :discrete] :default :range}
                     {:key :start :kind :prop}
                     {:key :end :kind :prop}
                     {:key :at :kind :prop}
                     {:key :label :kind :prop}
                     {:key :no-end :kind :enum :options [:exclude :point :fallback]
                      :default :exclude}
                     {:key :end-fallback :kind :value}
                     {:key :group-by :kind :edn}
                     {:key :color :kind :text}
                     {:key :color-prop :kind :prop}
                     {:key :conditional-format :kind :edn}
                     {:key :hover :kind :edn}
                     {:key :on-select :kind :event}]}
             {:key :selection? :kind :bool}
             {:key :multi-select? :kind :bool}
             {:key :selected-rows :kind :handle}
             {:key :on-selection :kind :event}
             {:key :show-now? :kind :bool}
             {:key :time-zone :kind :value}
             {:key :grid-unit :kind :enum
              :options [:none :year :quarter :month :week :day :hour]}
             {:key :start-month :kind :number}
             {:key :start-day :kind :number}
             {:key :start-hour :kind :number}
             {:key :stacking :kind :enum :options [:new-rows :overlap :smart-rows]
              :default :smart-rows}
             {:key :row-ordering :kind :enum :options [:alphabetical :first-start]}
             {:key :group-width :kind :number}
             {:key :start :kind :value}
             {:key :end :kind :value}]}
   {:widget :vega-chart :label "Vega-Diagramm" :group :daten
    :loading :spinner
    :params [{:key :spec :kind :value :required true}
             {:key :grammar :kind :enum :options [:vega-lite :vega] :default :vega-lite}
             {:key :theme :kind :value}
             {:key :inputs :kind :items
              :item [{:key :name :kind :text :required true}
                     {:key :rows :kind :object-set}
                     {:key :properties :kind :edn}
                     {:key :aggregate :kind :edn}
                     {:key :values :kind :value}]}
             {:key :selections :kind :items
              :item [{:key :param :kind :text :required true}
                     {:key :output :kind :handle :required true}]}
             {:key :preview? :kind :bool}
             {:key :height :kind :number}]}
   {:widget :free-form-analysis :label "Freie Analyse" :group :daten
    :loading :spinner
    :params [{:key :rows :kind :object-set :required true}
             {:key :empty-header :kind :text}
             {:key :empty-description :kind :text}
             {:key :path-saving? :kind :bool}
             {:key :copy-notebook? :kind :bool}
             {:key :output :kind :handle}
             {:key :links :kind :edn}
             {:key :widget-key :kind :text}]}
   {:widget :time-series-analysis :label "Zeitreihen-Analyse" :group :daten
    :loading :spinner
    :params [{:key :series :kind :edn}
             {:key :selectable :kind :edn}
             {:key :event-sets :kind :items
              :item [{:key :label :kind :text}
                     {:key :rows :kind :object-set :required true}
                     {:key :start :kind :prop :required true}
                     {:key :end :kind :prop}]}
             {:key :new-plot-canvas :kind :number}
             {:key :plot-types :kind :edn}
             {:key :event-set-types :kind :edn}
             {:key :overlay-y? :kind :bool}
             {:key :collapse-y? :kind :bool}
             {:key :boundaries? :kind :bool}
             {:key :tooltip? :kind :bool}
             {:key :digits :kind :number}
             {:key :utc? :kind :bool}
             {:key :sync-x? :kind :bool}
             {:key :view-range :kind :enum :options [:full :fixed :relative]}
             {:key :view-start :kind :value}
             {:key :view-end :kind :value}
             {:key :view-relative-ms :kind :value}
             {:key :saving? :kind :bool}
             {:key :output-rid :kind :handle}
             {:key :autoload :kind :value}
             {:key :keep-on-load? :kind :bool}
             {:key :canvases :kind :number}
             {:key :widget-key :kind :text}]}
   {:widget :bars :label "Balkenliste" :group :daten
    :loading :spinner
    :params [{:key :items :kind :object-set :required true}]}
   {:widget :chart :label "Diagramm" :group :daten :open? true
    :loading :spinner
    :params [{:key :rows :kind :object-set :required true :unless :layers}
             {:key :x :kind :prop :required true :unless :layers}
             {:key :y :kind :prop :required true
              :unless [:series :layers {:agg #{:count :cardinality}}]}
             {:key :series :kind :items
              :item [{:key :name :kind :text}
                     {:key :y :kind :prop :required true}
                     {:key :kind :kind :enum
                      :options [:bar :line :area :spline :scatter :line+markers :trend]}
                     {:key :axis :kind :enum :options [:left :right] :default :left}
                     {:key :color :kind :text}
                     {:key :agg :kind :enum :options [:sum :count :avg :min :max :cardinality]}
                     {:key :mode :kind :text}
                     {:key :fill? :kind :bool}]}
             {:key :group :kind :prop}
             {:key :x-bucket :kind :enum :options [:year :quarter :month :week :day]}
             {:key :agg :kind :enum :options [:sum :count :avg :min :max :cardinality]}
             {:key :missing :kind :enum :options [:gap :zero :connect] :default :gap}
             {:key :chart-kind :kind :enum
              :options [:line :area :spline :scatter :bar :pie :waterfall :time-series]
              :default :line}
             {:key :stacked? :kind :bool}
             {:key :legend? :kind :bool}
             {:key :x-title :kind :text}
             {:key :y-title :kind :text}
             {:key :y-fmt :kind :text}
             {:key :axes :kind :edn}
             {:key :names :kind :prop}
             {:key :colors :kind :edn}
             {:key :sort? :kind :bool}
             {:key :compare :kind :value :binds #{:val}
              :needs "den Wert einer Vergleichsvariablen {:val …}"}
             {:key :height :kind :number}
             {:key :layout :kind :edn}
             {:key :layers :kind :items :row :unknown
              :item [{:key :title :kind :text}
                     {:key :rows :kind :object-set}
                     {:key :aggregation :kind :value}
                     {:key :time-series :kind :edn}
                     {:key :chart-kind :kind :enum
                      :options [:line :area :spline :scatter :bar :pie :waterfall]}
                     {:key :labels? :kind :bool}
                     {:key :x :kind :prop}
                     {:key :y :kind :prop}
                     {:key :agg :kind :enum :options [:sum :count :avg :min :max :cardinality]}
                     {:key :group :kind :prop}
                     {:key :stacked? :kind :bool}
                     {:key :missing :kind :enum :options [:gap :zero :connect]}
                     {:key :x-bucket :kind :enum :options [:year :quarter :month :week :day]}
                     {:key :names :kind :prop}
                     {:key :bar-mode :kind :enum :options [:stacked :percentage :grouped]}
                     {:key :selection-filter :kind :handle}
                     {:key :series :kind :edn}
                     {:key :segments :kind :edn}]}
             {:key :segments :kind :items
              :item [{:key :value :kind :text :required true}
                     {:key :label :kind :text}
                     {:key :color :kind :text}
                     {:key :hidden? :kind :bool}]}
             {:key :labels? :kind :bool}
             {:key :radius :kind :number}
             {:key :ontology-colors? :kind :bool}
             {:key :legend-position :kind :enum :options [:bottom :top :left :right]}
             {:key :x-fmt :kind :text}
             {:key :sort-by-x :kind :enum :options [:x-asc :x-desc :value-asc :value-desc]}
             {:key :bar-mode :kind :enum :options [:stacked :percentage :grouped]}
             {:key :orientation :kind :enum :options [:horizontal :vertical] :default :horizontal}
             {:key :selection-filter :kind :handle}
             {:key :selected :kind :handle}
             {:key :export? :kind :bool}]}
   {:widget :map :label "Karte" :group :daten :open? true
    :loading :spinner
    :params [{:key :coordinate-system :kind :enum :options [:geo :plan] :default :geo}
             {:key :src :kind :value}
             {:key :extent :kind :value}
             {:key :unit :kind :text}
             {:key :title :kind :text}
             {:key :rows :kind :object-set :required true :unless :layers}
             {:key :lat :kind :prop :required true :unless :layers}
             {:key :lon :kind :prop :required true :unless :layers}
             {:key :lng :kind :prop}
             {:key :label :kind :prop}
             {:key :color :kind :prop}
             {:key :size :kind :prop}
             {:key :group :kind :prop}
             {:key :map-kind :kind :enum :options [:markers :heat] :default :markers}
             {:key :z :kind :prop}
             {:key :radius :kind :number}
             {:key :style :kind :enum
              :options [:carto-positron :carto-darkmatter :open-street-map :white-bg :basic]}
             {:key :center :kind :edn}
             {:key :zoom :kind :number}
             {:key :pick :kind :enum :options [:point :geojson]}
             {:key :sel :kind :handle}
             {:key :height :kind :number}
             {:key :stack :kind :object-set}
             {:key :stack-key :kind :prop}
             {:key :stack-order :kind :prop}
             {:key :stack-label :kind :prop}
             {:key :stack-w :kind :prop}
             {:key :stack-h :kind :prop}
             {:key :layers :kind :items :row :unknown
              :item [{:key :label :kind :text}
                     {:key :rows :kind :object-set :required true}
                     {:key :selected :kind :handle}
                     {:key :conditional-format :kind :edn}
                     {:key :visible? :kind :value}
                     {:key :locked? :kind :bool}
                     {:key :role :kind :enum :options [:outline :rooms]}
                     {:key :color :kind :text}
                     {:key :opacity :kind :number}
                     {:key :legend? :kind :bool}
                     {:key :time-prop :kind :prop}
                     {:key :geometries :kind :items
                      :item [{:key :geometry :kind :enum
                              :options [:point :heat :cluster :connect :segment :shape
                                        :choropleth]}
                             {:key :lat :kind :prop}
                             {:key :lon :kind :prop}
                             {:key :end-lat :kind :prop}
                             {:key :end-lon :kind :prop}
                             {:key :geopoint :kind :prop}
                             {:key :xy :kind :prop}
                             {:key :shape :kind :prop}
                             {:key :label :kind :prop}
                             {:key :color :kind :prop}
                             {:key :size :kind :prop}
                             {:key :group :kind :prop}
                             {:key :z :kind :prop}
                             {:key :radius :kind :number}
                             {:key :opacity :kind :number}
                             {:key :icon :kind :text}
                             {:key :show-labels? :kind :bool}
                             {:key :cluster-px :kind :number}
                             {:key :connect-by :kind :prop}
                             {:key :order-by :kind :prop}
                             {:key :line-color :kind :text}
                             {:key :line-width :kind :number}
                             {:key :fill? :kind :bool}
                             {:key :hidden? :kind :bool}]}
                     {:key :on-select :kind :event}
                     {:key :edit-action :kind :action}
                     {:key :edit-object-param :kind :text}
                     {:key :edit-shape-param :kind :text}
                     {:key :edit-label-param :kind :text}
                     {:key :edit-id-prop :kind :prop}
                     {:key :shape-output :kind :enum
                      :options [:feature :geometry-collection :geometry :points]}
                     {:key :actions :kind :items
                      :item [{:key :action :kind :action :required true}
                             {:key :label :kind :text}
                             {:key :object-param :kind :text}
                             {:key :id-prop :kind :prop}
                             {:key :params :kind :params}]}]}
             {:key :overlays :kind :items
              :item [{:key :label :kind :text}
                     {:key :geojson :kind :value :required true}
                     {:key :color :kind :text}
                     {:key :opacity :kind :number}]}
             {:key :base-picker? :kind :bool}
             {:key :draw-tools :kind :edn}
             {:key :draw-color :kind :text}
             {:key :draw-opacity :kind :number}
             {:key :shape-output :kind :enum
              :options [:feature :geometry-collection :geometry :points]}
             {:key :drawn :kind :handle}
             {:key :on-drawn :kind :event}
             {:key :shape-actions :kind :items
              :item [{:key :action :kind :action :required true}
                     {:key :label :kind :text}
                     {:key :shape-param :kind :text}
                     {:key :shape-output :kind :enum
                      :options [:feature :geometry-collection :geometry :points]}
                     {:key :params :kind :params}]}
             {:key :import :kind :handle}
             {:key :on-import :kind :event}
             {:key :single-draw? :kind :bool}
             {:key :clear-after-op? :kind :bool}
             {:key :antimeridian? :kind :bool}
             {:key :shape-select? :kind :bool}
             {:key :shape-search :kind :handle}
             {:key :shape-edit? :kind :bool}
             {:key :measure-perimeter? :kind :bool}
             {:key :measure-area? :kind :bool}
             {:key :measure-lines? :kind :bool}
             {:key :measure-segments? :kind :bool}
             {:key :selected-shapes :kind :handle}
             {:key :timeline? :kind :bool}
             {:key :time-editable? :kind :bool}
             {:key :live-toggle? :kind :bool}
             {:key :timeline-open? :kind :bool}
             {:key :selected-time :kind :handle}
             {:key :time-start :kind :handle}
             {:key :time-end :kind :handle}
             {:key :time-zone :kind :enum :options [:local :utc]}
             {:key :time-format :kind :enum :options [:local :h12 :h24]}
             {:key :playing :kind :handle}
             {:key :playback-position :kind :handle}
             {:key :auto-pause :kind :value}
             {:key :legend? :kind :bool}
             {:key :legend-collapsed? :kind :bool}
             {:key :legend-size :kind :enum :options [:full :compact]}
             {:key :selection-tools? :kind :bool}
             {:key :selection-panel? :kind :bool}
             {:key :auto-zoom? :kind :bool}
             {:key :zoom-to :kind :object-set}
             {:key :zoom-outside-only? :kind :bool}
             {:key :bounds :kind :handle}
             {:key :follow :kind :object-set}
             {:key :locate? :kind :bool}
             {:key :selected :kind :handle}
             {:key :class :kind :text}]}])
