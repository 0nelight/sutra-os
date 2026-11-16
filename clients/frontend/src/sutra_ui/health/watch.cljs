(ns sutra-ui.health.watch
  (:require [clojure.string :as str]
            [sutra-ui.ui.busy :as busy]
            [reagent.core :as r]
            [sutra.i18n.core :as i18n]
            [sutra-ui.api :as api]
            [sutra-ui.state.auth :as auth]
            [sutra-ui.state.db :as db]
            [sutra-ui.state.requests :as rq]
            [sutra.kit.remote :as rd]
            [sutra-ui.ui.core :as ui]))

(def ^:private levels ["all" "critical" "none"])

(def ^:private level-key
  {"all" :ui/health-watch-all "critical" :ui/health-watch-critical
   "none" :ui/health-watch-none})

(defn- when-str [s] (when s (.toLocaleString (js/Date. s))))

(defonce ^:private groups (db/cursor [:shared.health-watch :groups] rd/loading))

(def ^:private load-groups
  (db/load [:shared.health-watch :groups] [api/get! :health-check-groups]
           {:pick (comp vec :groups)}))

(db/reg :shared.health-watch/load-groups (fn [_ _] {:fx [load-groups]}))

(db/reg :shared.health-watch/ask
        (fn [s [_ req then]] (rq/request s {:ask req :always [[:rf/invoke then]]})))

(db/reg :shared.health-watch/create-group
        (fn [s [_ group done]]
          (rq/request s {:ask  [api/post! :health-check-groups group]
                         :then [(fn [_] [:rf/invoke done])]})))

(defn- ask! [then f & args] (db/send! [:shared.health-watch/ask (into [f] args) then]))

(defn controls
  [_c _reload]
  (let [form (r/atom nil)]
    (fn [{:keys [id watching snoozed]} reload]
      (let [current (or watching "none")]
        [:div {:class "u-flex-wrap-gap-6 u-mt-8 u-fs-12"}
         [:span {:class "u-muted"} (i18n/t :ui/health-watch)]
         [ui/segmented-control
          {:small true :value current
           :on-value-change #(ask! reload api/post! :health-check-watch {:id id} {:level %})
           :options (for [l levels] {:value l :label (i18n/t (level-key l))})}]
         (if snoozed
           [:<>
            [:span {:class "u-muted"}
             (i18n/t :ui/health-snoozed-until
                     {:until (when-str (:until snoozed))
                      :reason (or (not-empty (:reason snoozed)) "—")})]
            [ui/button {:on-click #(ask! reload api/delete! :health-check-snooze {:id id})}
             (i18n/t :ui/health-unsnooze)]]
           (if-let [{:keys [hours reason]} @form]
             [:<>
              [ui/numeric-input {:min 1 :style {:width "64px"} :value hours
                                 :small true :button-position :none
                                 :on-value-change #(swap! form assoc :hours %2)}]
              [:span {:class "u-muted"} (i18n/t :ui/health-snooze-hours)]
              [ui/input-group {:placeholder (i18n/t :ui/health-snooze-reason)
                               :value reason :small true
                               :on-change #(swap! form assoc :reason
                                                  (.. % -target -value))}]
              [ui/button {:intent :primary :on-click (fn [_]
                                (reset! form nil)
                                (ask! reload api/post! :health-check-snooze {:id id}
                                      {:hours (js/parseFloat hours) :reason reason}))}
               (i18n/t :ui/health-snooze)]
              [ui/button {:on-click #(reset! form nil)}
               (i18n/t :ui/cancel)]]
             [ui/button {:on-click #(reset! form {:hours 24 :reason ""})}
              (i18n/t :ui/health-snooze)]))]))))

(def ^:private periods ["15m" "1h" "daily" "weekly"])

(defn- subscribed [{:keys [subscribers]}]
  (let [me (:user @auth/auth)]
    (some #(when (= (str me) (str (:username %))) %) subscribers)))

(defn- group-row [_g _reload]
  (let [every (r/atom "daily") only-failing? (r/atom true)]
    (fn [{:keys [id name check-ids] :as g} reload]
      (let [sub (subscribed g)]
        [:div {:class "u-flex-between u-fs-12 u-mt-8"}
         [:div [:b name] [:span {:class "u-muted"} (str " · " (count check-ids) " Checks")]]
         (if sub
           [:div {:class "u-flex-wrap-gap-6"}
            [:span {:class "u-muted"}
             (i18n/t :ui/health-group-subscribed
                     {:every (str (/ (:every-ms sub) 60000) " min")})]
            [ui/button
             {:on-click #(ask! reload api/delete! :health-check-group-subscribe {:id id})}
             (i18n/t :ui/health-group-unsubscribe)]]
           [:div {:class "u-flex-wrap-gap-6"}
            [ui/html-select {:value @every :options periods
                             :on-change #(reset! every (.. % -target -value))}]
            [ui/checkbox {:checked @only-failing? :class "u-m-0"
                          :on-change #(swap! only-failing? not)
                          :label (i18n/t :ui/health-group-only-failing)}]
            [ui/button
             {:intent   :primary
              :on-click #(ask! reload api/post! :health-check-group-subscribe {:id id}
                               {:every @every :only-failing @only-failing?})}
             (i18n/t :ui/health-group-subscribe)]])]))))

(defn groups-panel
  [_checks]
  (let [draft (r/atom nil)
        load! #(db/send! [:shared.health-watch/load-groups])]
    (load!)
    (fn [checks]
      [:div {:class "u-mt-8"}
       [:div {:class "u-flex-between"}
        [:b (i18n/t :ui/health-groups)]
        [ui/button {:on-click #(reset! draft (if @draft nil {:name "" :ids #{}}))}
         (i18n/t (if @draft :ui/cancel :ui/health-group-new))]]
       (when-let [{:keys [name ids]} @draft]
         [:div {:class "u-mt-8 u-fs-12"}
          [ui/input-group {:placeholder (i18n/t :ui/health-group-name) :value name
                           :small true
                           :on-change #(swap! draft assoc :name (.. % -target -value))}]
          (into [:div {:class "u-mt-8"}]
                (for [{:keys [id] nm :name} (if (sequential? checks) checks [])]
                  ^{:key id}
                  [ui/checkbox {:checked (contains? ids id)
                                :on-change #(swap! draft update :ids
                                                   (fn [s] (if (contains? s id) (disj s id)
                                                               (conj s id))))
                                :label nm}]))
          [ui/button
           {:intent   :primary :disabled (or (str/blank? name) (empty? ids))
            :on-click #(db/send! [:shared.health-watch/create-group
                                  {:name name :check-ids (vec ids)}
                                  (fn [] (reset! draft nil) (load!))])}
           (i18n/t :ui/health-group-create)]])
       (cond
         (rd/loading? @groups)
         [busy/block {:class "u-fs-12"} (i18n/t :ui/loading)]
         (empty? (rd/value-or @groups nil))
         [:div {:class "u-muted u-fs-12"} (i18n/t :ui/health-groups-none)]
         :else (for [g @groups] ^{:key (:id g)} [group-row g load!]))])))
