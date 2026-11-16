(ns sutra-ui.apps.enta-editor.ui
  (:require [sutra-ui.ui.core :as ui]
            [reagent.core :as r]))

(defn icon-button
  [{:keys [icon label title on-click disabled? active? class]}]
  [ui/tooltip {:content title}
   [ui/button {:minimal true :small true :active active? :class class :icon icon
               :aria-label (when-not label title)
               :disabled disabled? :on-click on-click}
    label]])

(defn popover
  [_opts _trigger _body]
  (let [open? (r/atom false)]
    (fn [{:keys [align width class]} trigger body]
      [ui/popover
       {:is-open            @open?
        :on-interaction     (fn [o _] (reset! open? o))
        :placement          (if (= :right align) :bottom-end :bottom-start)
        :class              (str "ab-pop " class)
        :popover-class-name "ab-pop__panel"
        :content            [:div {:style {:width (str (or (some-> width js/parseInt) 240) "px")}}
                             (body #(reset! open? false))]}
       (trigger @open?)])))

(defn section
  [{:keys [title open? on-toggle count badge?]} & body]
  [ui/section {:title          title
               :compact        true
               :collapsible    true
               :class          "ab-sec"
               :collapse-props {:is-open (boolean open?) :on-toggle on-toggle}
               :right-element  [:<>
                                (when count [:span {:class "ab-sec__count"} count])
                                (when badge?
                                  [ui/tooltip
                                   {:content "hier ist etwas eingestellt"}
                                   [:span {:class "ab-sec__dot"}]])]}
   (into [ui/section-card {}] body)])
