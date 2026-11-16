(ns sutra-ui.builder.bottom-panel.column-stats
  (:require [sutra.i18n.core :as i18n]
            [sutra.types.profile :as profile]
            [sutra-ui.graph-editor.shared :as gs]))

(defn stats-of
  [column values]
  (profile/column-stats (mapv (fn [v] {column v}) values) column))

(defn- stat-row [k v]
  [:div {:style {:display "flex" :justify-content "space-between" :gap "var(--pt-grid-size)"}}
   [:span {:class "u-muted"} k]
   [:span v]])

(defn- histogram
  [top]
  (let [top-n (apply max 1 (map :count top))]
    [:div {:style {:margin-top "var(--sp-1)"}}
     [:div {:style {:color gs/grey :margin-bottom "var(--sp-0)"}} (i18n/t :ui/most-frequent)]
     (for [[i {:keys [value count]}] (map-indexed vector top)]
       ^{:key i}
       [:div {:style {:display "flex" :align-items "center" :gap "var(--sp-2)"}}
        [:span {:style {:overflow "hidden" :text-overflow "ellipsis"
                        :white-space "nowrap" :max-width "110px" :flex "0 0 auto"}}
         (if (nil? value) "null" (str value))]
        [:span {:style {:flex 1 :height "6px" :background "var(--c-border-line)"
                        :border-radius "var(--radius)"}}
         [:span {:style {:display "block" :height "6px" :border-radius "var(--radius)"
                         :background gs/blue
                         :width (str (js/Math.round (* 100 (/ count top-n))) "%")}}]]
        [:span {:style {:color gs/grey :flex "0 0 auto"}} count]])]))

(defn stats-card
  [{:keys [column count nulls distinct top min max mean median stddev
           min-length max-length blank whitespace-edged]}]
  [:div {:style {:background "var(--c-surface)" :border "1px solid var(--c-border-soft)"
                 :border-radius "var(--radius)"
                 :padding "var(--sp-3) var(--pt-grid-size)" :font-size "var(--fs-sm)"
                 :min-width "190px"
                 :box-shadow "0 4px 14px rgba(17, 20, 24, 0.14)"}}
   [:div {:style {:font-weight 600 :margin-bottom "var(--sp-1)"}} (clojure.core/name column)]
   [stat-row (i18n/t :ui/rows) count]
   [stat-row "null" (str nulls (when (pos? count)
                                 (str " · " (js/Math.round (* 100 (/ nulls count))) "%")))]
   [stat-row (i18n/t :ui/distinct) distinct]
   (when (some? min) [stat-row (i18n/t :ui/min-max) (str min " … " max)])
   (when mean [stat-row (i18n/t :ui/mean-median) (str (.toFixed mean 2) " / " median)])
   (when stddev [stat-row (i18n/t :ui/stddev) (.toFixed stddev 2)])
   (when min-length [stat-row (i18n/t :ui/length) (str min-length " … " max-length)])
   (when (and blank (pos? blank)) [stat-row (i18n/t :ui/blank) blank])
   (when (and whitespace-edged (pos? whitespace-edged))
     [:div {:style {:color "var(--c-danger)" :margin-top "var(--sp-1)"}}
      (i18n/t :ui/whitespace-edged {:n whitespace-edged})])
   (when (seq top) [histogram top])])
