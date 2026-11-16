(ns sutra-ui.shell.default-workspace
  (:require [reagent.core :as r]
            [sutra.i18n.core :as i18n]
            [sutra-ui.api :as api]
            [sutra-ui.ui.core :as ui]))

(defn- slug [x] (if (keyword? x) (name x) (str x)))

(defn section
  [_]
  (let [loaded (r/atom nil)
        busy   (r/atom false)
        err    (r/atom nil)]
    (-> (api/get! :home-page) (.then #(reset! loaded %)) (.catch (fn [_] nil)))
    (fn [ws-name]
      (when-let [{:keys [groups workspaces default-workspaces]} @loaded]
        (when-let [ws (some #(when (= (slug ws-name) (slug (:name %))) %) workspaces)]
          (let [promoted? (:promoted ws)
                chosen (vec (or (get default-workspaces (keyword (str (:id ws))))
                                (get default-workspaces (str (:id ws)))))
                by-id  (into {} (map (juxt :id :name)) groups)
                put!   (fn [gs]
                         (reset! busy true) (reset! err nil)
                         (-> (api/post! :default-workspace-set
                                        {:workspace (str (:id ws)) :groups gs})
                             (.then #(reset! loaded %))
                             (.catch #(reset! err (or (.-message %) (str %))))
                             (.finally #(reset! busy false))))
                off?   (or @busy (not promoted?))]
            [:div {:class "ws-ed__field"}
             [:span {:class "ws-ed__label"} (i18n/t :ui/ws-ed-default-workspace)]
             [:div {:class "ws-ed__checks"}
              (for [g chosen]
                ^{:key g}
                [ui/checkbox {:checked true :disabled off? :class "u-m-0"
                              :on-change #(put! (vec (remove #{g} chosen)))
                              :label (get by-id g g)}])
              [ui/html-select {:class "ws-ed__input" :value "" :disabled off?
                               :on-change #(let [v (.. % -target -value)]
                                             (when (seq v) (put! (conj chosen v))))}
               [:option {:value ""} (i18n/t :ui/home-url-add-group)]
               (for [g groups :when (not (some #{(:id g)} chosen))]
                 ^{:key (:id g)} [:option {:value (:id g)} (:name g)])]]
             [:p {:class "ws-ed__hint"}
              (i18n/t (if promoted? :ui/ws-ed-default-workspace-hint
                          :ui/ws-ed-default-workspace-unpromoted))]
             (when @err [:p {:class "ws-ed__hint"} @err])]))))))
