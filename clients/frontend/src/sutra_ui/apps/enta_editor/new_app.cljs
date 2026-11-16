(ns sutra-ui.apps.enta-editor.new-app
  (:require [sutra-ui.ui.overlay :as overlay]
            [sutra.kit.coerce :refer [->kw]]
            [clojure.string :as str]
            [sutra.capps.catalog :as cat]
            [sutra.i18n.core :as i18n]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.ui.form :as uf]
            [sutra-ui.apps.enta-editor.state :as es]
            [sutra-ui.apps.enta-editor.fields :as f]
            [sutra-ui.ui.core :as ui]
            [sutra-ui.object.card :as oc]))

(defn- object-param
  [a t]
  (some (fn [[k spec]] (when (= (->kw t) (->kw (:type spec))) k)) (:params a)))

(defn- actions-on
  [t]
  (->> (get @catalog/canonical-schema :actions {})
       (keep (fn [[k a]]
               (when-let [p (object-param a t)]
                 [(name k) (or (not-empty (str (:display-name a))) (name k)) p
                  (= (->kw t) (->kw (:on a)))])))
       (sort-by (fn [[_ label _ on?]] [(not on?) (str/lower-case label)]))
       vec))

(defn start! [id] (es/send! [:enta.new/start (->kw id)]))

(defn- ask! [m] (es/ui-set! {:asking m}))

(defn- ask-more! [m] (ask! (merge (es/ui-get :asking) m)))

(defn- create! []
  (let [{:keys [type action]} (es/ui-get :asking)
        [_ label param] (some #(when (= action (first %)) %) (actions-on type))]
    (es/send! [:enta.new/create {:type type
                                 :display-name (oc/type-label (keyword type) true)
                                 :action action :action-label label :param param}])))

(def ^:private blurbs
  {:liste-detail [:list-detail-view "Objekte suchen, eins wählen, rechts sein Detail — mit Action."]
   :liste        [:th-list "Eine Tabelle mit Suchfeld darüber."]
   :dashboard    [:dashboard "Kennzahlen oben, darunter Diagramm und Tabelle."]
   :reiter       [:folder-close "Mehrere Bereiche hinter Reitern, Reiter in der Adresse."]
   :leer         [:square "Nichts vorgegeben — selbst zusammenbauen."]})

(defn template-cards
  [on-picked]
  (into [:div {:class "ab-templates"}]
        (for [{:keys [id label]} cat/templates
              :let [[icon blurb] (get blurbs id [:square ""])]]
          [ui/card {:interactive true :compact true :class "ab-template"
                    :on-click #(do (on-picked) (start! id))}
           [:span {:class "ab-template__glyph"} [ui/icon {:icon icon :size 20}]]
           [:span {:class "ab-template__label"} label]
           [:span {:class "ab-template__blurb"} blurb]])))

(defn open-gallery! [] (es/ui-set! {:gallery? true}))

(defn gallery []
  (when (es/ui-get :gallery?)
    (let [close! #(es/ui-set! {:gallery? false})]
      [overlay/modal {:on-close #(close!)
                      :size 620
                      :title "Neue App"
                      :foot [ui/button {:on-click close!}
                             (i18n/t :ui/cancel)]
                      :foot-end? true}
       [:<>
        [:p
         "Womit anfangen? Alles lässt sich danach umbauen."]
        [template-cards close!]]])))

(defn dialog []
  (when-let [{:keys [type action]} (es/ui-get :asking)]
    (let [acts (when type (actions-on type))]
      [overlay/form-dialog
       {:on-close #(ask! nil) :style {:font-family "sans-serif"}
        :title (i18n/t :ui/app-builder-new-list-detail-title)
        :ok-label (i18n/t :ui/app-builder-new-list-detail-create)
        :ok-disabled? (nil? type) :on-ok #(create!)}
       [:<>
        [:p (i18n/t :ui/app-builder-new-list-detail-body)]
        [uf/field {:label (i18n/t :ui/app-builder-new-list-detail-type) :required? true}
         [f/select type (f/object-type-options) #(ask-more! {:type % :action nil})
          "Objekttyp…" {:fill? true}]]
        (when (seq acts)
          [uf/field {:label (i18n/t :ui/app-builder-new-list-detail-action)}
           [f/select action acts #(ask-more! {:action %})
            (i18n/t :ui/app-builder-new-list-detail-no-action) {:fill? true}]])]])))
