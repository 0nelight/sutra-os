(ns sutra-ui.apps.admin-settings.feedback-inbox
  (:require [sutra-ui.ui.core :as ui]
            [sutra.i18n.core :as i18n]
            [sutra.types.feedback :as b]
            [sutra-ui.apps.admin-settings.model :as model]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.ui.style :as style]))

(defn- zeit [s] (when s (.toLocaleString (js/Date. s))))

(defn- sitzung [{:keys [id benutzer aufgabe begonnen beendet erledigt erledigt-von]} ms]
  [ui/callout {:class ["u-mb-12" (when erledigt "admin-done")]}
   [:div {:class "u-flex-items-gap-8 u-mb-8"}
    [:div {:class "u-flex-1"}
     [:div {:class "u-bold"} benutzer " · " (zeit begonnen)
      (when-not beendet [:span {:class style/small-muted} " (" (i18n/t :ui/feedback-laeuft) ")"])]
     [:div {:class style/small-muted}
      (when aufgabe (str "„" aufgabe "“ · "))
      (some->> ms (remove b/fehler?) count (hash-map :n) (i18n/t :ui/feedback-meldungen))
      " · "
      (when-let [n (some->> ms (filter b/fehler?) count (#(when (pos? %) %)))]
        (str (i18n/t :ui/feedback-fehler-anzahl {:n n}) " · "))
      [:code id]]
     (when erledigt
       [:div {:class "u-text-added"}
        (i18n/t :ui/feedback-erledigt-am {:wann (zeit erledigt) :wer erledigt-von})])]
    [ui/button {:intent (when-not erledigt :primary)
                :on-click #(model/send! [:admin.feedback/check id (not erledigt)])}
     (i18n/t (if erledigt :ui/feedback-wieder-offen :ui/feedback-abhaken))]]
   (if (seq ms)
     [:ul {:class "admin-list"}
      (for [m ms]
        ^{:key (:id m)}
        [:li {:class "u-mb-4"}
         [:div {:class (when (b/fehler? m) "u-text-removed")} (or (:satz m) (:text m))]
         (when-let [st (:stand m)] [:div {:class style/small-muted} st])
         (for [[i z] (map-indexed vector (:herkunft m))]
           ^{:key i} [:div {:class style/small-muted} [:code z]])])]
     (when ms [:div {:class style/small-muted} (i18n/t :ui/feedback-keine-meldungen)]))])

(def ^:private filter-wahl
  [[:offen :ui/feedback-offen (complement :erledigt)]
   [:erledigt :ui/feedback-erledigt :erledigt]
   [:alle :ui/feedback-alle identity]])

(def view
  (machine/entering
   model/admin [:admin.feedback/load]
   (fn [eine alle!]
     (let [{:keys [sitzungen error] :as u} (:feedback @model/db)
           f (:filter u :offen)
           gilt (some (fn [[k _ p]] (when (= k f) p)) filter-wahl)
           gezeigt (if eine
                     (filter #(= eine (str (:id %))) sitzungen)
                     (filter gilt sitzungen))]
       [:div
        [:div {:class [style/small-muted "u-mb-12"]} (i18n/t :ui/feedback-intro)]
        (when eine
          [:div {:class "u-flex-items-gap-8 u-mb-12"}
           [:code eine]
           [ui/button {:on-click alle!}
            (i18n/t :ui/feedback-alle)]])
        [ui/segmented-control
         {:class ["u-mb-12" (when eine "admin-hidden")] :value f
          :on-value-change #(model/send! [:admin.feedback/filter %])
          :options (for [[k label p] filter-wahl]
                     {:value k
                      :label (str (i18n/t label) " (" (count (filter p sitzungen)) ")")})}]
        (when error [ui/problem error nil "u-mb-12"])
        (if (and sitzungen (empty? gezeigt))
          [:div {:class style/small-muted} (i18n/t :ui/feedback-leer)]
          (for [s gezeigt]
            ^{:key (:id s)} [sitzung s (get-in u [:meldungen (:id s)])]))]))))
