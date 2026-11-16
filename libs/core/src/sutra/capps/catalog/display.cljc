(ns sutra.capps.catalog.display)

(def widgets
  [{:widget :kpi-row :label "Kennzahlen-Reihe" :group :kennzahl
    :loading :kpi-row
    :params [{:key :items :kind :items :required true
              :item [{:key :label :kind :text :required true}
                     {:key :value :kind :value :required true}
                     {:key :fmt :kind :fmt}
                     {:key :sub :kind :text}
                     {:key :description :kind :value}
                     {:key :conditional-format :kind :edn}
                     {:key :secondary :kind :edn}
                     {:key :sparkline :kind :edn}
                     {:key :accent :kind :enum :options [:auto :pos :neg :warn :muted]}
                     {:key :on-click :kind :event}]}
             {:key :label :kind :text}
             {:key :direction :kind :enum :options [:horizontal :vertical]}
             {:key :template :kind :enum :options [:stacked :side-by-side]}
             {:key :size :kind :enum :options [:compact :regular :large]}
             {:key :display :kind :enum :options [:card :tag :list] :default :card}]}
   {:widget :kpi :label "Kennzahl" :group :kennzahl
    :loading :kpi
    :params [{:key :label :kind :text :required true}
             {:key :value :kind :value :required true}
             {:key :fmt :kind :fmt}
             {:key :sub :kind :text}
             {:key :description :kind :value}
             {:key :conditional-format :kind :edn}
             {:key :secondary :kind :edn}
             {:key :sparkline :kind :edn}
             {:key :accent :kind :enum :options [:auto :pos :neg :warn :muted]}
             {:key :on-click :kind :event}
             {:key :size :kind :enum :options [:compact :regular :large]}
             {:key :display :kind :enum :options [:card :tag] :default :card}]}
   {:widget :property-cards :label "Property Cards" :group :kennzahl
    :loading :spinner
    :params [{:key :cards :kind :edn}
             {:key :object :kind :object-set}
             {:key :display :kind :enum :options [:card :tag] :default :card}]}
   {:widget :badge :label "Plakette" :group :kennzahl
    :params [{:key :tone :kind :enum
              :options [:auto :pos :neg :ok :warn :danger :info :muted]}
             {:key :text :kind :value :required true}
             {:key :fmt :kind :fmt}]}
   {:widget :empty-state :label "Leer-Zustand" :group :kennzahl
    :params [{:key :icon :kind :text}
             {:key :title :kind :text :required true}
             {:key :hint :kind :text}
             {:key :action :kind :edn}]}
   {:widget :object-view :label "Objekt-Ansicht" :group :daten
    :loading :spinner
    :params [{:key :object :kind :object-set :required true}
             {:key :form-factor :kind :enum :options [:full :panel] :default :full}
             {:key :panel-behavior :kind :enum :options [:instance :adaptive :set]
              :default :instance}
             {:key :hide-header? :kind :bool}
             {:key :hide-actions? :kind :bool}
             {:key :interface :kind :params}
             {:key :hide-tabs? :kind :bool}
             {:key :initial-tab :kind :text}
             {:key :initial-tab-on-switch? :kind :bool}
             {:key :mode :kind :enum :options [:auto :standard :configured] :default :auto}
             {:key :mode-toggle? :kind :bool}
             {:key :empty :kind :edn}]}
   {:widget :property-list :label "Eigenschaftsliste" :group :daten
    :loading :spinner
    :params [{:key :object :kind :object-set :required true}
             {:key :props :kind :cols}
             {:key :only-prominent? :kind :bool}
             {:key :exclude :kind :edn}
             {:key :label-position :kind :enum :options [:beside :below] :default :beside}
             {:key :columns :kind :number}
             {:key :hide-null? :kind :bool}
             {:key :inline-actions :kind :edn}]}
   {:widget :view :label "Plattform-Ansicht" :group :struktur
    :params [{:key :view :kind :edn :required true}]}
   {:widget :attributes :label "Eigenschaften (dieses Objekt)" :group :daten
    :loading :spinner
    :params []}
   {:widget :relations :label "Verknüpfungen (dieses Objekt)" :group :daten
    :loading :spinner
    :params [{:key :title :kind :text}]}
   {:widget :documents :label "Dokumente (dieses Objekt)" :group :daten
    :loading :spinner
    :params []}
   {:widget :matrix :label "Matrix (Adjazenz)" :group :daten
    :loading :spinner
    :params [{:key :owner-type :kind :prop}
             {:key :children-conn :kind :prop}
             {:key :other-type :kind :prop}]}
   {:widget :floorplan :label "Grundriss (Platzierung)" :group :daten
    :loading :spinner
    :params []}
   {:widget :pdf :label "Beleg (PDF)" :group :daten
    :loading :spinner
    :params [{:key :src :kind :value :required true :unless :object}
             {:key :object :kind :object-set}
             {:key :prop :kind :prop}
             {:key :page-var :kind :handle}
             {:key :selected-text :kind :handle}
             {:key :downloads? :kind :bool :default true}
             {:key :create :kind :edn}
             {:key :annotations :kind :object-set}
             {:key :layers :kind :items :row :unknown
              :item [{:key :rows :kind :object-set :required true}
                     {:key :page :kind :prop :required true}
                     {:key :boxes :kind :prop :required true}
                     {:key :name :kind :text}
                     {:key :selected :kind :handle}
                     {:key :tooltip :kind :edn}
                     {:key :actions :kind :edn}
                     {:key :conditional-format :kind :edn}
                     {:key :color :kind :text}
                     {:key :color-prop :kind :prop}
                     {:key :color-by :kind :prop}
                     {:key :caption :kind :prop}
                     {:key :label :kind :prop}
                     {:key :key :kind :prop}
                     {:key :active :kind :value}
                     {:key :kind :kind :enum :options [:rect :band :stamp]}
                     {:key :title :kind :text}
                     {:key :body :kind :value}
                     {:key :when :kind :prop}
                     {:key :on-select :kind :event}
                     {:key :action :kind :action}
                     {:key :object-param :kind :prop}
                     {:key :boxes-param :kind :prop}]}
             {:key :search :kind :value}
             {:key :on-select :kind :event}
             {:key :on-area :kind :event}
             {:key :area-label :kind :text}
             {:key :draw :kind :value}
             {:key :collect? :kind :bool}
             {:key :area-kind :kind :enum :options [:rect :band]}
             {:key :area-page :kind :handle}
             {:key :area-boxes :kind :handle}
             {:key :zoom :kind :number}
             {:key :height :kind :number}
             {:key :class :kind :text}
             {:key :filename :kind :value}]}])
