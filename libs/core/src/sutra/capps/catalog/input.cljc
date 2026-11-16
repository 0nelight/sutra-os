(ns sutra.capps.catalog.input)

(def widgets
  [{:widget :polygon :label "Umriss zeichnen" :group :eingabe
    :loading :spinner
    :params [{:key :src :kind :value}
             {:key :extent :kind :value}
             {:key :sel :kind :handle :required true}
             {:key :pick :kind :enum :options [:points :geojson] :default :points}
             {:key :shapes :kind :object-set}
             {:key :shape-points :kind :prop}
             {:key :shape-label :kind :prop}
             {:key :shape-labels? :kind :bool}
             {:key :shape-tooltips? :kind :bool}
             {:key :conditional-format :kind :edn}
             {:key :stack :kind :object-set}
             {:key :stack-key :kind :prop}
             {:key :stack-order :kind :prop}
             {:key :stack-label :kind :prop}
             {:key :stack-w :kind :prop}
             {:key :stack-h :kind :prop}
             {:key :layers :kind :items :row :unknown
              :item [{:key :rows :kind :object-set :required true}
                     {:key :points :kind :prop :required true}
                     {:key :label :kind :prop}
                     {:key :labels? :kind :bool}
                     {:key :tooltips? :kind :bool}
                     {:key :class :kind :text}
                     {:key :conditional-format :kind :edn}
                     {:key :except :kind :value}
                     {:key :except-key :kind :prop}]}
             {:key :on-shape-click :kind :event}
             {:key :action :kind :action}
             {:key :shape-param :kind :prop}
             {:key :points-param :kind :prop}
             {:key :label-param :kind :prop}
             {:key :shape-id :kind :prop}
             {:key :remove-action :kind :action}
             {:key :import :kind :handle}
             {:key :on-import :kind :event}
             {:key :readonly? :kind :bool}
             {:key :height :kind :number}
             {:key :on-close :kind :event}
             {:key :class :kind :text}]}
   {:widget :resource-list :label "Ressourcen-Liste" :group :daten
    :loading :spinner
    :params [{:key :resource-kind :kind :enum :options [:files :object-types :object-sets]
              :default :files}
             {:key :content :kind :enum
              :options [:static :recent :favorite :folders :tags :all :prominent]
              :default :static}
             {:key :items :kind :items
              :item [{:key :family :kind :text}
                     {:key :name :kind :text}
                     {:key :object-type :kind :object-type}
                     {:key :rows :kind :object-set}
                     {:key :title :kind :value}
                     {:key :description :kind :value}
                     {:key :style :kind :enum :options [:minimal :prominent :classic]}
                     {:key :icon :kind :text}
                     {:key :icon-color :kind :text}
                     {:key :visible :kind :value}
                     {:key :thumbnail :kind :value}
                     {:key :thumbnail-position :kind :enum :options [:top :right :bottom :left]}
                     {:key :on-click :kind :event}]}
             {:key :projects :kind :edn}
             {:key :folders :kind :edn}
             {:key :tags :kind :edn}
             {:key :format :kind :enum :options [:list :image-card] :default :list}
             {:key :style :kind :enum :options [:minimal :prominent :classic] :default :classic}
             {:key :horizontal? :kind :bool}
             {:key :selected-object-type :kind :handle}
             {:key :selected-object-set :kind :handle}
             {:key :class :kind :text}]}
   {:widget :linked-resources :label "Verknüpfte Ressourcen" :group :daten
    :loading :spinner
    :params [{:key :objects :kind :object-set :required true}
             {:key :heading :kind :text}
             {:key :hide-upload? :kind :bool}
             {:key :hide-link? :kind :bool}
             {:key :class :kind :text}]}
   {:widget :audio-recorder :label "Audio aufnehmen" :group :eingabe
    :params [{:key :action :kind :action}
             {:key :media-param :kind :text}
             {:key :name-param :kind :text}
             {:key :output :kind :handle}
             {:key :class :kind :text}]}
   {:widget :spreadsheet :label "Tabellendatei" :group :medien
    :loading :spinner
    :params [{:key :src :kind :value :required true :unless :object}
             {:key :object :kind :object-set}
             {:key :prop :kind :prop}
             {:key :layers :kind :items :row :unknown
              :item [{:key :name :kind :text}
                     {:key :rows :kind :object-set :required true}
                     {:key :sheet-prop :kind :prop :required true}
                     {:key :range-prop :kind :prop :required true}]}
             {:key :selected :kind :handle}
             {:key :actions :kind :edn}
             {:key :height :kind :number}
             {:key :class :kind :text}]}
   {:widget :image-annotation :label "Bild annotieren" :group :eingabe
    :loading :spinner
    :params [{:key :src :kind :value :required true :unless :object}
             {:key :object :kind :object-set}
             {:key :prop :kind :prop}
             {:key :sel :kind :handle}
             {:key :active :kind :handle}
             {:key :rows :kind :object-set}
             {:key :box-prop :kind :prop}
             {:key :color-prop :kind :prop}
             {:key :on-create :kind :event}
             {:key :height :kind :number}
             {:key :class :kind :text}]}
   {:widget :markdown :label "Markdown" :group :kennzahl
    :loading :spinner
    :params [{:key :text :kind :long-text :required true}
             {:key :fmt :kind :fmt}
             {:key :monospace? :kind :bool}
             {:key :scroll? :kind :bool}
             {:key :wrap? :kind :bool :default true}
             {:key :breaks? :kind :bool :default true}
             {:key :align :kind :enum :options [:left :center :right]}
             {:key :selected-text :kind :handle}
             {:key :selection-start :kind :handle}
             {:key :selection-end :kind :handle}
             {:key :selected :kind :handle}
             {:key :ref-types :kind :edn}
             {:key :selection-behavior :kind :enum :options [:none :last :selected]}
             {:key :on-ref-select :kind :event}
             {:key :layers :kind :items :row :unknown
              :item [{:key :name :kind :text}
                     {:key :rows :kind :object-set :required true}
                     {:key :start :kind :prop :required true}
                     {:key :end :kind :prop :required true}
                     {:key :active :kind :handle}
                     {:key :on-select :kind :event}
                     {:key :tooltip :kind :edn}
                     {:key :interactions :kind :edn}
                     {:key :color :kind :text}
                     {:key :conditional-format :kind :edn}
                     {:key :display :kind :enum
                      :options [:highlight :underline :dashed :both :both-dashed]}]}
             {:key :create :kind :edn}
             {:key :class :kind :text}]}
   {:widget :tabs :label "Reiter-Leiste" :group :eingabe
    :params [{:key :sel :kind :handle}
             {:key :items :kind :items :required true
              :item [{:key :id :kind :text :required true}
                     {:key :label :kind :text :required true}
                     {:key :count :kind :value}
                     {:key :badge :kind :value}
                     {:key :icon :kind :text}
                     {:key :on-click :kind :event}
                     {:key :visible :kind :value}
                     {:key :when-false :kind :enum :options [:disabled :hidden]}]}
             {:key :design :kind :enum :options [:outline :block :prominent :grouped]
              :default :outline}
             {:key :size :kind :enum :options [:small :default :large] :default :default}
             {:key :minimal? :kind :bool}
             {:key :width :kind :number}
             {:key :active-color :kind :text}
             {:key :height :kind :number}
             {:key :fill? :kind :bool}
             {:key :direction :kind :enum :options [:horizontal :vertical]
              :default :horizontal}
             {:key :alignment :kind :enum :options [:left :center] :default :left}]}
   {:widget :filter-list :label "Filterliste" :group :eingabe
    :params [{:key :set :kind :var-name :required true}
             {:key :output :kind :handle :required true}
             {:key :title :kind :text}
             {:key :filters :kind :items :required true
              :item [{:key :prop :kind :prop}
                     {:key :link :kind :text}
                     {:key :label :kind :text}
                     {:key :component :kind :enum :default :multi-select
                      :options [:keyword :histogram :single-select :multi-select :distribution
                                :single-date :multi-date :timeline :has-link]}
                     {:key :advanced? :kind :bool}
                     {:key :display :kind :enum :options [:inline :grouped] :default :inline}
                     {:key :collapsed? :kind :bool}]}
             {:key :layout :kind :enum :options [:vertical :pills] :default :vertical}
             {:key :editable? :kind :bool}]}
   {:widget :exploration-filter-pills :label "Filter-Pillen (Exploration)" :group :eingabe
    :params [{:key :set :kind :var-name :required true}
             {:key :output :kind :handle}
             {:key :mode :kind :enum :default :add-update-remove
              :options [:read-only :remove-only :update-existing :add-update-remove]}
             {:key :prevent-operator-change? :kind :bool}
             {:key :show-object-type? :kind :bool}]}
   {:widget :exploration-search-bar :label "Suchleiste (Exploration)" :group :eingabe
    :params [{:key :set :kind :var-name :required true}
             {:key :output :kind :handle}
             {:key :mode :kind :enum :default :add-update-remove
              :options [:read-only :remove-only :update-existing :add-update-remove]}
             {:key :available-properties :kind :edn}
             {:key :available-links :kind :edn}
             {:key :autocomplete? :kind :bool :default true}
             {:key :keyword? :kind :bool :default true}
             {:key :show-clear? :kind :bool}
             {:key :prevent-operator-change? :kind :bool}
             {:key :icon :kind :text}
             {:key :fill? :kind :bool}
             {:key :show-object-type? :kind :bool}
             {:key :show-help? :kind :bool}
             {:key :placeholder :kind :text}]}
   {:widget :prominent-terms :label "Prominente Begriffe" :group :eingabe
    :params [{:key :set :kind :var-name :required true}
             {:key :prop :kind :prop :required true}
             {:key :output :kind :handle :required true}
             {:key :hide-empty? :kind :bool}
             {:key :terms :kind :items :required true
              :item [{:key :value :kind :value :required true}
                     {:key :label :kind :text}
                     {:key :icon :kind :text}]}]}
   {:widget :object-dropdown :label "Objekt-Auswahl" :group :eingabe
    :params [{:key :rows :kind :object-set :required true}
             {:key :sel :kind :handle :required true}
             {:key :label :kind :text}
             {:key :allow-none? :kind :bool :default true}
             {:key :title-prop :kind :prop}
             {:key :props :kind :edn}
             {:key :hide-null? :kind :bool :default true}
             {:key :sort-by-prop :kind :prop}
             {:key :sort-dir :kind :enum :options [:asc :desc]}
             {:key :search :kind :edn}
             {:key :placeholder :kind :text}]}
   {:widget :object-selector :label "Objekt-Mehrfachauswahl" :group :eingabe
    :params [{:key :rows :kind :object-set :required true}
             {:key :sel :kind :handle :required true}
             {:key :label :kind :text}
             {:key :title-prop :kind :prop}
             {:key :props :kind :edn}
             {:key :hide-null? :kind :bool :default true}
             {:key :sort-by-prop :kind :prop}
             {:key :sort-dir :kind :enum :options [:asc :desc]}
             {:key :search :kind :edn}
             {:key :placeholder :kind :text}]}
   {:widget :string-selector :label "Textauswahl" :group :eingabe
    :params [{:key :sel :kind :handle :required true}
             {:key :items :kind :value :required true}
             {:key :label :kind :text}
             {:key :multiple? :kind :bool}
             {:key :display :kind :enum :default :dropdown
              :options [:dropdown :radio :checkboxes]}
             {:key :layout :kind :enum :options [:vertical :horizontal :grid]}
             {:key :columns :kind :number}
             {:key :clearable? :kind :bool :default true}
             {:key :creatable? :kind :bool}
             {:key :placeholder :kind :text}]}
   {:widget :user-select :label "Nutzerauswahl" :group :eingabe
    :params [{:key :sel :kind :handle :required true}
             {:key :label :kind :text}
             {:key :placeholder :kind :text}
             {:key :multiple? :kind :bool}
             {:key :allow-clear? :kind :bool :default true}
             {:key :groups :kind :value}]}
   {:widget :date-time-picker :label "Datum und Uhrzeit" :group :eingabe
    :params [{:key :sel :kind :handle :required true}
             {:key :label :kind :text}
             {:key :date-format :kind :enum :options [:iso :de :us] :default :iso}
             {:key :time-format :kind :enum :options [:h24 :h12] :default :h24}
             {:key :precision :kind :enum :options [:minute :second :millisecond]
              :default :minute}
             {:key :timezone-editable? :kind :bool}
             {:key :default-timezone :kind :value}
             {:key :class :kind :text}]}
   {:widget :filter-pills :label "Filter-Pillen" :group :eingabe
    :params [{:key :sel :kind :handle :required true}
             {:key :items :kind :value :required true}
             {:key :all-label :kind :text}]}
   {:widget :stepper :label "Schrittfolge" :group :eingabe
    :params [{:key :steps :kind :items :required true
              :item [{:key :id :kind :text :required true}
                     {:key :label :kind :text :required true}
                     {:key :on-click :kind :event}
                     {:key :completed :kind :value}
                     {:key :icon :kind :text}]}
             {:key :current :kind :value}
             {:key :active :kind :handle}
             {:key :type :kind :enum :options [:linear :non-linear] :default :linear}
             {:key :template :kind :enum :options [:text :icons] :default :text}
             {:key :show-number? :kind :bool}
             {:key :completed-color :kind :text}
             {:key :active-color :kind :text}
             {:key :on-click :kind :event}]}
   {:widget :text-input :label "Texteingabe" :group :eingabe
    :params [{:key :sel :kind :handle :required true}
             {:key :label :kind :text}
             {:key :placeholder :kind :text}
             {:key :type :kind :enum :options [:text :search :password]}
             {:key :format :kind :enum :options [:single-line :text-area :markdown]
              :default :single-line}
             {:key :on-enter :kind :event}
             {:key :height :kind :number}
             {:key :auto-size? :kind :bool}
             {:key :class :kind :text}]}
   {:widget :rtf-text :label "Brieftext (RTF)" :group :eingabe
    :params [{:key :sel :kind :handle :required true}
             {:key :rows :kind :number}
             {:key :placeholder :kind :text}
             {:key :class :kind :text}]}
   {:widget :number-input :label "Zahleneingabe" :group :eingabe
    :params [{:key :sel :kind :handle :required true :unless :action}
             {:key :action :kind :action}
             {:key :value :kind :value}
             {:key :value-param :kind :text}
             {:key :params :kind :params}
             {:key :label :kind :text}
             {:key :placeholder :kind :text}
             {:key :grouping? :kind :bool}
             {:key :reset? :kind :bool}
             {:key :prefix :kind :text}
             {:key :suffix :kind :text}
             {:key :percent? :kind :bool}
             {:key :step :kind :number}
             {:key :min :kind :number}
             {:key :max :kind :number}
             {:key :class :kind :text}]}
   {:widget :date-input :label "Datumseingabe" :group :eingabe
    :params [{:key :sel :kind :handle :required true}
             {:key :label :kind :text}
             {:key :range? :kind :bool}
             {:key :sel-to :kind :handle}
             {:key :min :kind :text}
             {:key :max :kind :text}
             {:key :class :kind :text}]}
   {:widget :select-input :label "Auswahlfeld" :group :eingabe
    :params [{:key :sel :kind :handle :required true}
             {:key :items :kind :value :required true}
             {:key :item-value :kind :prop}
             {:key :item-label :kind :prop}
             {:key :sort :kind :enum :options [:asc :desc]}
             {:key :sort-by :kind :prop}
             {:key :label :kind :text}
             {:key :clearable? :kind :bool :default true}
             {:key :placeholder :kind :text}
             {:key :strict? :kind :bool}
             {:key :display :kind :enum :options [:dropdown :radio :segmented]
              :default :dropdown}
             {:key :layout :kind :enum :options [:vertical :horizontal :grid]}
             {:key :columns :kind :number}
             {:key :selection-required? :kind :bool}
             {:key :value :kind :value}
             {:key :disabled? :kind :bool}
             {:key :on-change :kind :event}
             {:key :class :kind :text}]}
   {:widget :combo-input :label "Suchfeld" :group :eingabe
    :params [{:key :sel :kind :handle :required true :unless :action}
             {:key :action :kind :action}
             {:key :value :kind :value}
             {:key :value-param :kind :text}
             {:key :params :kind :params}
             {:key :items :kind :value :required true}
             {:key :item-value :kind :prop}
             {:key :item-label :kind :prop}
             {:key :item-search :kind :prop}
             {:key :item-match :kind :prop}
             {:key :sort :kind :enum :options [:asc :desc]}
             {:key :sort-by :kind :prop}
             {:key :label :kind :text}
             {:key :strict? :kind :bool}
             {:key :placeholder :kind :text}
             {:key :limit :kind :number}
             {:key :class :kind :text}]}
   {:widget :checkbox :label "Kontrollkästchen" :group :eingabe
    :params [{:key :sel :kind :handle :required true}
             {:key :label :kind :text}
             {:key :class :kind :text}]}
   {:widget :filter-sidebar :label "Filter-Leiste" :group :eingabe
    :params [{:key :title :kind :text}
             {:key :fields :kind :items :required true
              :item [{:key :label :kind :text}
                     {:key :kind :kind :enum
                      :options [:search :select :toggle :number :date :range :pills]
                      :default :search}
                     {:key :sel :kind :handle :required true}
                     {:key :items :kind :value}
                     {:key :placeholder :kind :text}
                     {:key :all-label :kind :text}
                     {:key :toggle-label :kind :text}
                     {:key :min-sel :kind :handle}
                     {:key :max-sel :kind :handle}
                     {:key :min-label :kind :text}
                     {:key :max-label :kind :text}
                     {:key :step :kind :number}]}
             {:key :class :kind :text}]}])
