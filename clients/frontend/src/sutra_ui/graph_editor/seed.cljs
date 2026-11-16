(ns sutra-ui.graph-editor.seed
  (:require [reagent.core :as r]
            [clojure.string :as str]
            [sutra.i18n.core :as i18n]
            [sutra-ui.graph-editor.shared :as gs]
            [sutra-ui.ui.core :as ui]))

(defn seed-box
  [_]
  (let [q (r/atom "")]
    (fn [{:keys [placeholder candidates roots on-pick roots-label limit]
          :or   {limit 12}}]
      (let [blank? (str/blank? @q)
            items  (vec (take limit (if blank? (when roots (roots)) (candidates @q))))
            head   (when (and blank? (seq items))
                     (or roots-label (i18n/t :ui/ge-seed-start)))]
        [:div {:class           "lineage-seed"
               :on-click        #(.stopPropagation %)
               :on-context-menu #(.preventDefault %)}
         [:div {:class "lineage-seed__box" :style {:border (str "1px solid " gs/blue)}}]
         [ui/suggest
          {:fill                  true
           :input-props           {:placeholder  placeholder
                                   :left-element [:span {:class "lineage-seed__icon"
                                                         :style {:color gs/blue}}
                                                  "+"]}
           :selected-item         nil
           :input-value-renderer  :label
           :query                 @q
           :on-query-change       (fn [s _] (reset! q s))
           :items                 items
           :items-equal           (fn [a b] (= ((juxt :kind :kw) a) ((juxt :kind :kw) b)))
           :item-renderer         (fn [{:keys [kind label]} {:keys [handle-click modifiers ref id]}]
                                    [ui/menu-item
                                     {:id             id
                                      :ref            ref
                                      :role-structure :listoption
                                      :active         (:active modifiers)
                                      :text           [:span [:span {:class "lineage-seed__kind u-muted"}
                                                              (name kind)]
                                                       label]
                                      :on-click       handle-click}])
           :item-list-renderer    (fn [{:keys [items-parent-ref menu-props] :as lp}]
                                    (let [rows (ui/render-filtered-items
                                                lp {:no-results
                                                    (when-not blank?
                                                      [ui/menu-item
                                                       {:disabled true
                                                        :text     (i18n/t :ui/ge-seed-none)}])})]
                                      (when (or head (seq rows))
                                        [ui/menu (merge {:role "listbox"} menu-props
                                                        {:ref items-parent-ref})
                                         (when head [ui/menu-divider {:title head}])
                                         rows])))
           :popover-content-props {:style {:min-width "340px"}}
           :popover-props         {:minimal true}
           :on-item-select        (fn [{:keys [kind kw]} _]
                                    (reset! q "")
                                    (on-pick [kind kw]))}]]))))
