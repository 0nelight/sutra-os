(ns sutra.capps.catalog.action
  (:require [sutra.capps.intent :as ci]))

(def widgets
  [{:widget :button :label "Schaltfläche" :group :aktion
    :params [{:key :label :kind :text :required true}
             {:key :on-click :kind :event :required true}
             {:key :intent :kind :enum :options ci/intents :default :none}
             {:key :disabled? :kind :bool}
             {:key :class :kind :text}]}
   {:widget :hyperlink :label "Hyperlink" :group :aktion
    :params [{:key :label :kind :text :required true}
             {:key :url-type :kind :enum :options [:property :hardcoded :templated]
              :required true}
             {:key :object :kind :object-set}
             {:key :property :kind :prop}
             {:key :hide-if-null? :kind :bool}
             {:key :url :kind :text}
             {:key :template :kind :text}
             {:key :open-in :kind :enum :options [:same-tab :new-tab :popup]
              :default :same-tab}
             {:key :intent :kind :enum :options ci/intents :default :none}]}
   {:widget :action-button :label "Action-Schaltfläche" :group :aktion
    :params [{:key :label :kind :text :required true}
             {:key :action :kind :action :required true}
             {:key :intent :kind :enum :options ci/intents}
             {:key :params :kind :params}
             {:key :busy-label :kind :text}
             {:key :error-var :kind :var-name}
             {:key :on-done :kind :event}
             {:key :output :kind :handle}
             {:key :scenario :kind :handle}
             {:key :disabled? :kind :bool}
             {:key :class :kind :text}]}
   {:widget :action-form :label "Action-Formular" :group :aktion
    :params [{:key :action :kind :action :required true}
             {:key :intent :kind :enum :options ci/intents}
             {:key :defaults :kind :params}
             {:key :options :kind :params}
             {:key :label :kind :text}
             {:key :busy-label :kind :text}
             {:key :error-var :kind :var-name}
             {:key :cancel-label :kind :text}
             {:key :on-cancel :kind :event}
             {:key :on-done :kind :event}
             {:key :output :kind :handle}
             {:key :scenario :kind :handle}
             {:key :class :kind :text}]}
   {:widget :action-table :label "Aktions-Tabelle" :group :aktion
    :loading :spinner
    :params [{:key :rows :kind :object-set :required true}
             {:key :cols :kind :cols :required true}
             {:key :action :kind :action :required true}
             {:key :key-param :kind :prop}
             {:key :params :kind :params}
             {:key :row-key :kind :prop}
             {:key :on-row-click :kind :event}
             {:key :prefill? :kind :bool}
             {:key :on-done :kind :event}
             {:key :sort :kind :edn}
             {:key :limit :kind :number}
             {:key :submit-label :kind :text}
             {:key :intent :kind :enum :options ci/intents :default :success}
             {:key :busy-label :kind :text}
             {:key :error-var :kind :var-name}
             {:key :scenario :kind :handle}
             {:key :row-class :kind :edn}
             {:key :class :kind :text}]}
   {:widget :order-list :label "Reihenfolge-Liste" :group :aktion
    :loading :spinner
    :params [{:key :items :kind :object-set :required true}
             {:key :label :kind :prop :required true}
             {:key :sub :kind :prop}
             {:key :row-key :kind :prop}
             {:key :sel :kind :handle}
             {:key :action :kind :action}
             {:key :param :kind :prop}
             {:key :submit-label :kind :text}
             {:key :intent :kind :enum :options ci/intents :default :success}
             {:key :busy-label :kind :text}
             {:key :error-var :kind :var-name}
             {:key :scenario :kind :handle}
             {:key :class :kind :text}]}
   {:widget :export :label "Export" :group :aktion
    :params [{:key :label :kind :text :required true}
             {:key :intent :kind :enum :options ci/intents :default :none}
             {:key :params :kind :params}
             {:key :filename :kind :text}
             {:key :mime :kind :text}
             {:key :busy-label :kind :text}
             {:key :disabled? :kind :bool}
             {:key :class :kind :text}]}
   {:widget :media-uploader :label "Datei-Upload" :group :aktion
    :params [{:key :label :kind :text :required true}
             {:key :accept :kind :text}
             {:key :meta :kind :edn}
             {:key :multiple :kind :bool}
             {:key :max-files :kind :number}
             {:key :routes :kind :edn}
             {:key :drop-label :kind :text}
             {:key :intent :kind :enum :options ci/intents}
             {:key :left-icon :kind :text}
             {:key :right-icon :kind :text}
             {:key :capture :kind :enum :options [:environment :user]}
             {:key :destination :kind :text}
             {:key :destination-var :kind :value}
             {:key :media-column :kind :prop}
             {:key :branch :kind :value}
             {:key :rid-output :kind :handle}
             {:key :filename-output :kind :handle}
             {:key :action :kind :action}
             {:key :file-param :kind :prop}
             {:key :media-param :kind :prop}
             {:key :params :kind :params}]}
   {:widget :comments :label "Kommentare" :group :aktion
    :loading :spinner
    :params [{:key :items :kind :object-set}
             {:key :object :kind :object-set}
             {:key :action :kind :action}
             {:key :param :kind :prop}
             {:key :users-param :kind :prop}
             {:key :object-param :kind :prop}
             {:key :notify? :kind :bool :default true}
             {:key :mention-users? :kind :bool}
             {:key :references :kind :items
              :item [{:key :rows :kind :object-set :required true}
                     {:key :label :kind :prop}
                     {:key :on-select :kind :event}]}
             {:key :action-log? :kind :bool}
             {:key :placeholder :kind :text}
             {:key :empty :kind :text}]}
   {:widget :button-group :label "Knopfgruppe" :group :aktion
    :params [{:key :button-type :kind :enum :options [:inline :menu :two-part]
              :default :inline}
             {:key :label :kind :text}
             {:key :left-icon :kind :text}
             {:key :right-icon :kind :text}
             {:key :intent :kind :enum :options ci/intents}
             {:key :color :kind :text}
             {:key :minimal? :kind :bool}
             {:key :tag? :kind :bool}
             {:key :large? :kind :bool}
             {:key :fill? :kind :bool}
             {:key :buttons :kind :items :required true :row :unknown
              :item [{:key :label :kind :text}
                     {:key :intent :kind :enum :options ci/intents}
                     {:key :color :kind :text}
                     {:key :left-icon :kind :text}
                     {:key :right-icon :kind :text}
                     {:key :description :kind :text}
                     {:key :visible :kind :value}
                     {:key :when-false :kind :enum :options [:disabled :hidden]}
                     {:key :disabled? :kind :bool}
                     {:key :minimal? :kind :bool}
                     {:key :tag? :kind :bool}
                     {:key :large? :kind :bool}
                     {:key :on-click :kind :event}
                     {:key :action :kind :action}
                     {:key :params :kind :params}
                     {:key :form? :kind :bool}
                     {:key :scenario :kind :handle}
                     {:key :output :kind :handle}
                     {:key :on-start :kind :event}
                     {:key :on-done :kind :event}
                     {:key :url :kind :text}
                     {:key :open-in :kind :enum :options [:same-tab :new-tab]}
                     {:key :export :kind :edn}
                     {:key :function-export :kind :edn}
                     {:key :media-export :kind :edn}]}]}
   {:widget :inline-action :label "Inline-Action" :group :aktion
    :params [{:key :action :kind :action}
             {:key :actions :kind :items
              :item [{:key :action :kind :action :required true}
                     {:key :title :kind :text}
                     {:key :layout :kind :enum :options [:form :table]}
                     {:key :defaults :kind :params}
                     {:key :options :kind :params}
                     {:key :rows :kind :object-set}
                     {:key :key-param :kind :prop}]}
             {:key :layout :kind :enum :options [:form :table] :default :form}
             {:key :layout-switch? :kind :bool}
             {:key :row-management? :kind :bool}
             {:key :file-upload? :kind :bool}
             {:key :rows :kind :object-set}
             {:key :key-param :kind :prop}
             {:key :defaults :kind :params}
             {:key :options :kind :params}
             {:key :title :kind :text}
             {:key :label :kind :text}
             {:key :submit-label :kind :text}
             {:key :invalid-state :kind :enum :options [:disabled :hidden] :default :disabled}
             {:key :hide-header? :kind :bool}
             {:key :scenario :kind :handle}
             {:key :on-done :kind :event}
             {:key :output :kind :handle}]}
   {:widget :mobile-navbar :label "Mobile Navigationsleiste" :group :aktion
    :params [{:key :items :kind :items :required true
              :item [{:key :label :kind :text :required true}
                     {:key :icon :kind :text}
                     {:key :on-click :kind :event}
                     {:key :items :kind :items
                      :item [{:key :label :kind :text :required true}
                             {:key :icon :kind :text}
                             {:key :on-click :kind :event}]}]}
             {:key :title :kind :text}
             {:key :icon :kind :text}
             {:key :height :kind :number :default 50}
             {:key :hide-text? :kind :bool}]}
   {:widget :qr-reader :label "QR-Code-Leser" :group :eingabe
    :params [{:key :output :kind :handle :required true}
             {:key :mode :kind :enum :options [:single :multiple] :default :single}
             {:key :prompt? :kind :bool}
             {:key :label :kind :text}
             {:key :intent :kind :enum :options ci/intents}
             {:key :color :kind :text}
             {:key :left-icon :kind :text}
             {:key :right-icon :kind :text}
             {:key :description :kind :text}
             {:key :disabled? :kind :bool}
             {:key :minimal? :kind :bool}
             {:key :tag? :kind :bool}
             {:key :large? :kind :bool}
             {:key :fill? :kind :bool}]}
   {:widget :current-location :label "Standort" :group :eingabe
    :params [{:key :output :kind :handle :required true}
             {:key :label :kind :text}
             {:key :hide-display? :kind :bool}]}
   {:widget :app-pairing :label "App-Kopplung" :group :aktion
    :params [{:key :display-name :kind :text}
             {:key :shared :kind :items
              :item [{:key :name :kind :text :required true}
                     {:key :var :kind :handle :required true}
                     {:key :state :kind :enum :options [:selection :open-data :custom]
                      :default :selection}]}
             {:key :auto-open :kind :enum
              :options [:disabled :enabled :only-initially :only-on-change]
              :default :disabled}
             {:key :toasts? :kind :bool}
             {:key :pause-toggle? :kind :bool}
             {:key :enrich? :kind :bool :default true}
             {:key :scope :kind :value}]}
   {:widget :edit-history :label "Änderungs-Historie" :group :aktion
    :loading :spinner
    :params [{:key :object :kind :value}
             {:key :rows :kind :object-set}
             {:key :include-pipeline? :kind :bool}
             {:key :type :kind :prop}
             {:key :props :kind :items}
             {:key :kinds :kind :items}
             {:key :editors :kind :items}
             {:key :sort :kind :enum :options [:desc :asc] :default :desc}
             {:key :limit :kind :number}
             {:key :rev :kind :value}
             {:key :class :kind :text}]}
   {:widget :object-timeline :label "Objekt-Zeitachse" :group :aktion
    :loading :spinner
    :params [{:key :object :kind :value}
             {:key :rows :kind :object-set}
             {:key :type :kind :prop}
             {:key :rev :kind :value}]}
   {:widget :data-freshness :label "Datenaktualität" :group :daten
    :loading :spinner
    :params [{:key :items :kind :items :required true :add-label "Eintrag hinzufügen"
              :item [{:key :object-type :kind :object-type :required true}
                     {:key :sources :kind :items :add-label "Quelle hinzufügen"
                      :item [{:key :resource :kind :resource :required true}
                             {:key :override-name :kind :text}]}]}]}
   {:widget :scenario-picker :label "Szenario-Wahl" :group :aktion
    :params [{:key :sel :kind :handle :required true}
             {:key :title :kind :text}
             {:key :new-label :kind :text}
             {:key :allow-drop? :kind :bool}
             {:key :allow-apply? :kind :bool}
             {:key :apply-label :kind :text}
             {:key :multi? :kind :bool}
             {:key :class :kind :text}]}
   {:widget :media :label "Medien" :group :medien
    :loading :spinner
    :params [{:key :src :kind :value :required true :unless :object}
             {:key :object :kind :object-set}
             {:key :prop :kind :prop}
             {:key :seek :kind :value}
             {:key :key-frames :kind :edn}
             {:key :transcript :kind :edn}
             {:key :mime :kind :text}
             {:key :media-kind :kind :enum :options [:image :video :audio :pdf :iframe]}
             {:key :alt :kind :text}
             {:key :poster :kind :value}
             {:key :height :kind :number}
             {:key :class :kind :text}]}
   {:widget :iframe :label "Iframe" :group :medien
    :loading :spinner
    :params [{:key :src :kind :value}
             {:key :html :kind :value}
             {:key :mode :kind :enum :options [:url :messages :bidirectional] :default :url}
             {:key :embedded? :kind :bool}
             {:key :url-params :kind :params}
             {:key :input-params :kind :params}
             {:key :output-params :kind :params}
             {:key :variables :kind :params}
             {:key :events :kind :edn}
             {:key :title :kind :text}
             {:key :height :kind :number}
             {:key :sandbox :kind :text}
             {:key :allow :kind :text}
             {:key :class :kind :text}]}
   {:widget :custom-widget :label "Custom Widget" :group :medien
    :loading :spinner
    :params [{:key :widget-set :kind :text}
             {:key :widget-name :kind :text}
             {:key :version :kind :text}
             {:key :variables :kind :params}
             {:key :events :kind :edn}
             {:key :title :kind :text}
             {:key :height :kind :number}
             {:key :allow :kind :text}]}
   {:widget :scenario-manager :label "Szenario-Verwaltung" :group :aktion
    :params [{:key :static :kind :value}
             {:key :dynamic :kind :handle}
             {:key :active :kind :handle}
             {:key :title :kind :text}
             {:key :new-label :kind :text}
             {:key :class :kind :text}]}
   {:widget :scenario-selector :label "Szenario-Auswahl" :group :aktion
    :params [{:key :candidates :kind :value}
             {:key :sel :kind :handle :required true}
             {:key :multi? :kind :bool}
             {:key :dropdown? :kind :bool}
             {:key :title :kind :text}
             {:key :class :kind :text}]}
   {:widget :scenario-summary :label "Szenario-Übersicht" :group :aktion
    :params [{:key :scenario :kind :value :required true}
             {:key :title :kind :text}
             {:key :class :kind :text}]}
   {:widget :ai-agent :label "KI-Agent" :group :aktion
    :params [{:key :agent :kind :text :required true}
             {:key :version :kind :text}
             {:key :show-reasoning? :kind :bool}
             {:key :state :kind :params}
             {:key :textbox :kind :handle}
             {:key :answer :kind :handle}
             {:key :title :kind :text}
             {:key :placeholder :kind :text}
             {:key :class :kind :text}]}
   {:widget :ai-chatbot :label "KI-Chatbot" :group :aktion
    :params [{:key :agent :kind :text :required true}
             {:key :version :kind :text}
             {:key :show-reasoning? :kind :bool}
             {:key :state :kind :params}
             {:key :textbox :kind :handle}
             {:key :answer :kind :handle}
             {:key :title :kind :text}
             {:key :placeholder :kind :text}
             {:key :class :kind :text}]}
   {:widget :ai-interactive :label "AI Interactive" :group :aktion
    :params [{:key :agent :kind :text :required true}
             {:key :version :kind :text}
             {:key :show-reasoning? :kind :bool}
             {:key :state :kind :params}
             {:key :textbox :kind :handle}
             {:key :answer :kind :handle}
             {:key :title :kind :text}
             {:key :placeholder :kind :text}
             {:key :class :kind :text}]}
   {:widget :ai-generated-content :label "KI-Inhalt" :group :aktion
    :params [{:key :logic :kind :params}
             {:key :prompt :kind :value}
             {:key :prompt-function :kind :params}
             {:key :clear-on-change? :kind :bool}
             {:key :temperature :kind :number}
             {:key :model :kind :text}
             {:key :output :kind :handle}
             {:key :column :kind :text}
             {:key :as-rows? :kind :bool}
             {:key :icon :kind :text}
             {:key :button-label :kind :text}
             {:key :title :kind :text}
             {:key :loading-message :kind :text}
             {:key :spinner? :kind :bool}
             {:key :class :kind :text}]}
   {:widget :notebook-document :label "Notebook-Dokument" :group :medien
    :loading :spinner
    :params [{:key :document :kind :value :required true}
             {:key :editable? :kind :bool}
             {:key :class :kind :text}]}
   {:widget :embed :label "App einbetten" :group :medien
    :loading :spinner
    :params [{:key :app :kind :text :required true}
             {:key :inputs :kind :edn}
             {:key :bind :kind :params}
             {:key :on-output :kind :edn}
             {:key :on-event :kind :edn}]}])
