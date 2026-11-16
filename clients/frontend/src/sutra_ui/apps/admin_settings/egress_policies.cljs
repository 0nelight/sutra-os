(ns sutra-ui.apps.admin-settings.egress-policies
  (:require [sutra-ui.ui.core :as ui]
            [sutra.i18n.core :as i18n]
            [sutra.kit.remote :as rd]
            [sutra-ui.apps.admin-settings.model :as model]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.ui.style :as style]))

(defn- entscheiden! [id decision] (model/send! [:admin.egress/decide id decision]))

(def ^:private zustand-text
  {:pending :ui/egress-pending :active :ui/egress-active
   :revoked :ui/egress-revoked :paused :ui/egress-paused})

(defn- zeile [{:keys [id effective justification proposed-by decided-by used-by]}]
  (let [z (keyword effective)]
    [ui/callout {:class "u-mb-12"}
     [:div {:class "u-flex-items-gap-8"}
      [:div {:class "u-flex-1"}
       [:div {:class "u-bold"} [:code id] " · " (i18n/t (zustand-text z))]
       (when justification [:div {:class style/small-muted} justification])
       [:div {:class style/small-muted}
        (i18n/t :ui/egress-wer {:von (or proposed-by "—") :durch (or decided-by "—")})]
       (when (seq used-by)
         [:div {:class style/small-muted}
          (i18n/t :ui/egress-genutzt {:quellen (apply str (interpose ", " used-by))})])
       (when (= :paused z)
         [:div {:class style/small-muted} (i18n/t :ui/egress-paused-hint)])]
      (when (contains? #{:pending :revoked} z)
        [ui/button {:intent :primary :on-click #(entscheiden! id :approve)}
         (i18n/t :ui/egress-approve)])
      (when (contains? #{:pending :active :paused} z)
        [ui/button {:on-click #(entscheiden! id :revoke)}
         (i18n/t :ui/egress-revoke)])]]))

(def view
  (machine/entering
   model/admin [:admin.egress/load]
   (fn []
     (let [{rdata :data error :error} (:egress @model/db)
           data (rd/value-or rdata nil)
           error (or error (rd/reason rdata))]
       [:div
        [:div {:class [style/small-muted "u-mb-12"]} (i18n/t :ui/egress-intro)]
        (when error [ui/problem error nil "u-mb-12"])
        (when (and data (not (:enforced data)))
          [:div {:class [style/small-muted "u-mb-12"]} (i18n/t :ui/egress-nicht-durchgesetzt)])
        (for [p (:policies data)] ^{:key (:id p)} [zeile p])
        (when (seq (:uncovered data))
          [:div {:class style/small-muted}
           (i18n/t :ui/egress-ohne-policy
                   {:ziele (apply str (interpose ", " (:uncovered data)))})])]))))
