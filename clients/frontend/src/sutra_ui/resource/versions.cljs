(ns sutra-ui.resource.versions
  (:require [reagent.core :as r]
            [sutra.i18n.core :as i18n]
            [sutra-ui.api :as api]
            [sutra.kit.remote :as rd]
            [sutra-ui.state.loading :as loading]
            [sutra-ui.editor.code :refer [code-diff]]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.requests :as rq]
            [sutra-ui.state.staging :as staging]
            [sutra-ui.ui.core :as ui]))

(defn- ask
  [db f id params body req]
  (let [params (merge (select-keys db [:section :name]) params)]
    (rq/request db (assoc req :error [:error] :ask (cond-> [f id params] body (conj body))))))

(machine/reg-event :shared.versions/load
  (fn [{:keys [db]} _]
    {:fx [[:call {:call [loading/or-failure api/get! :resource-versions
                         (select-keys db [:section :name])]
                  :ok [:shared.versions/listed] :err [:sutra/ignored]}]]}))

(machine/reg-event :shared.versions/listed
  (fn [{:keys [db]} [_ answer]]
    (let [{:keys [status sentence] :as f} (loading/failure answer)]
      {:db (assoc db :listing (cond (= 404 status) rd/absent
                                    f              (rd/after-failure (:listing db) sentence)
                                    :else          (rd/of-answer answer)))})))

(machine/reg-event :shared.versions/set (fn [{:keys [db]} [_ m]] {:db (merge db m)}))

(machine/reg-event :shared.versions/stage
  (fn [{:keys [db]} [_ id body fields]]
    (ask (assoc db :error nil :staged nil) api/post! id nil body
         {:busy [:busy] :then [[:shared.versions/set (assoc fields :staged true)]]
          :fx   [[:invoke [staging/refresh-staged!]]]})))

(machine/reg-event :shared.versions/show
  (fn [{:keys [db]} [_ v ok]]
    (ask (assoc db :error nil) api/get! :resource-version {:version v} nil
         (if ok {:then [ok]} {:busy [:busy] :path [:shown]}))))

(machine/reg-event :shared.versions/restore
  (fn [_ [_ on-restore v]] {:fx [[:invoke [on-restore v]]]}))

(defn- release-mode? [db]
  (= "release" (some-> (rd/value-or (:listing db) nil) :mode name)))

(defn- row [{:keys [send!] :as db} on-restore {:keys [version published? note by at]}]
  (let [release? (release-mode? db)]
    [:div {:class (str "versions__row" (when published? " is-published"))}
     [:div {:class "versions__head"}
      [:strong (str "v" version)]
      (when published? [:span {:class "versions__badge"} (i18n/t :ui/versions-published)])
      [:span {:class "versions__meta"} (some-> at (subs 0 16) (.replace "T" " "))
       (when by (str " · " by))]]
     (when note [:div {:class "versions__note"} note])
     (into [:div {:class "versions__actions"}]
           (for [[text event]
                 [[:ui/versions-compare [:shared.versions/show version]]
                  (when-not (or published? release?)
                    [:ui/versions-pin [:shared.versions/stage :resource-versions-publish
                                       {:version version} {:note ""}]])
                  (when (and release? (not published?))
                    [:ui/versions-rollback [:shared.versions/stage :resource-versions-restore
                                            {:version version}]])
                  (when on-restore
                    [:ui/versions-restore
                     [:shared.versions/show version [:shared.versions/restore on-restore]]])]
                 :when text]
             [ui/button {:on-click #(send! event)} (i18n/t text)]))]))

(defn panel
  [{:keys [section name]}]
  (let [f     (machine/frame (r/atom {:section section :name name :note "" :listing rd/loading})
                             {:name :versions :lifetime :view})
        send! #(machine/dispatch % {:frame f})]
    (send! [:shared.versions/load])
    (fn [{:keys [on-restore]}]
      (let [{:keys [listing busy staged note shown] :as db} (assoc @(:db f) :send! send!)
            {:keys [versions draft? published]} (rd/value-or listing nil)
            error (or (:error db) (rd/reason listing))]
        [:div {:class "versions"}
         [machine/leaving f]
         [:div {:class "versions__title"} (i18n/t :ui/versions-title)]
         (case (rd/state listing)
           :loading [:p {:class "versions__hint"} (i18n/t :ui/loading)]
           :absent  [:p {:class "versions__hint"} (i18n/t :ui/versions-unsaved)]
           :failed  [:div {:class "versions__error"} error]
           [:<>
            [:p {:class "versions__hint"}
             (cond (nil? published) (i18n/t :ui/versions-none)
                   draft?           (i18n/t :ui/versions-draft)
                   :else            (i18n/t :ui/versions-current))]
            (when-not (release-mode? db)
              [:div {:class "versions__publish"}
               [ui/input-group
                {:class "u-flex-1-min-0" :value note :placeholder (i18n/t :ui/versions-note)
                 :on-change #(send! [:shared.versions/set {:note (.. % -target -value)}])}]
               [ui/button
                {:intent :primary :disabled (boolean busy)
                 :on-click #(send! [:shared.versions/stage :resource-versions-publish
                                    {:note note} {:note ""}])}
                (i18n/t :ui/versions-publish)]])
            (when staged [:p {:class "versions__hint"} (i18n/t :ui/versions-staged)])
            (when error [:div {:class "versions__error"} error])
            (for [v versions] ^{:key (:version v)} [row db on-restore v])
            (when shown
              [:div {:class "versions__diff"}
               [:div {:class "versions__head"}
                [:span (i18n/t :ui/versions-diff {:v (:version shown)})]
                [ui/button {:icon :cross :on-click #(send! [:shared.versions/set {:shown nil}])}]]
               ^{:key (:version shown)}
               [code-diff
                {:baseline (:content shown) :value (:head shown) :min-height "200px"}]])])]))))
