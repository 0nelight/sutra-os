(ns sutra-ui.apps.admin-settings.home-page
  (:require [sutra-ui.ui.core :as ui :refer [tag]]
            [clojure.string :as str]
            [sutra.i18n.core :as i18n]
            [sutra-ui.apps.admin-settings.model :as model]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.ui.inputs :as inputs]
            [sutra-ui.ui.style :as style]))

(defn- suggestions
  [{:keys [workspaces apps]}]
  (concat [{:path "/workspace" :label (i18n/t :ui/home-url-default-workspace)}
           {:path "/apps" :label (i18n/t :ui/home-url-portal)}]
          (for [w workspaces]
            {:path (:path w) :label (str (i18n/t :ui/home-url-workspace) " · " (:label w))})
          (for [a apps]
            {:path (:path a) :label (str (i18n/t :ui/home-url-app) " · " (:label a))})))

(defn- url-input
  [{:keys [value on-change disabled? placeholder options]}]
  [:div {:class "admin-url"}
   [ui/suggest
    {:fill                 true
     :disabled             disabled?
     :input-props          {:placeholder (or placeholder "/workspace/…")}
     :selected-item        nil
     :input-value-renderer :path
     :query                (or value "")
     :on-query-change      (fn [q _] (on-change q))
     :items                (vec options)
     :item-predicate       (fn [q {:keys [path label]} _ _]
                             (let [q (str/lower-case q)]
                               (some #(str/includes? (str/lower-case (str %)) q)
                                     [path label])))
     :items-equal          :path
     :item-renderer        (fn [{:keys [path label]} {:keys [handle-click modifiers ref id]}]
                             [ui/menu-item {:id             id
                                            :ref            ref
                                            :role-structure :listoption
                                            :active         (:active modifiers)
                                            :text           label
                                            :label          path
                                            :on-click       handle-click}])
     :popover-props        {:minimal true :match-target-width true}
     :on-item-select       (fn [{:keys [path]} _] (on-change path))}]])

(defn- group-picker
  [{:keys [groups chosen on-change disabled?]}]
  (let [by-id (into {} (map (juxt :id :name)) groups)
        others (remove #(contains? (set chosen) (:id %)) groups)]
    [:div {:class "u-flex-wrap-gap-6 admin-center"}
     (for [g chosen]
       ^{:key g}
       [tag {:large true
             :on-remove (when-not disabled? #(on-change (vec (remove #{g} chosen))))}
        (get by-id g g)])
     [inputs/select-input nil
      {:value       nil
       :strict?     true
       :disabled?   (or disabled? (empty? others))
       :placeholder (i18n/t :ui/home-url-add-group)
       :items       (for [g others] {:value (:id g) :label (:name g)})
       :on-change   (fn [v _] (when (some? v) (on-change (conj (vec chosen) v))))}]]))

(defn- overrides
  [{:keys [draft choices off?]}]
  (let [ovs (vec (:overrides draft))
        op! (fn [& op] (model/send! (into [:admin.home/override] op)))]
    [:div {:class "u-mt-8"}
     [:div {:class "u-bold u-mb-4"} (i18n/t :ui/home-url-overrides)]
     [:div {:class [style/small-muted "u-mb-8"]} (i18n/t :ui/home-url-overrides-hint)]
     (for [[i o] (map-indexed vector ovs)]
       ^{:key i}
       [ui/callout {:class "u-mb-8"}
        [:div {:class "u-flex-items-gap-8 u-mb-4"}
         [:span {:class "u-bold"} (str (inc i) ".")]
         [url-input {:value (:url o) :disabled? off? :options (suggestions choices)
                     :on-change #(op! :url i %)}]
         [ui/button {:icon :arrow-up :disabled (or off? (zero? i)) :on-click #(op! :up i)}]
         [ui/button {:icon :arrow-down :disabled (or off? (= i (dec (count ovs))))
                     :on-click #(op! :down i)}]
         [ui/button
          {:outlined true :intent :danger :disabled off? :on-click #(op! :remove i)}
          (i18n/t :ui/remove)]]
        [group-picker {:groups (:groups choices) :chosen (vec (:groups o)) :disabled? off?
                       :on-change #(op! :groups i %)}]])
     [ui/button {:disabled off? :on-click #(op! :add nil)}
      (i18n/t :ui/home-url-new)]]))

(def section
  (machine/entering
   model/admin [:admin.home/enter]
   (fn [{:keys [disabled?]}]
     (let [{choices :loaded d :draft :keys [busy error saved]} (:home @model/db)
           off?    (or disabled? busy (nil? d))
           dirty?  (and d (not= d (:home-page choices)))]
       [ui/callout {:class "u-mb-12"}
        [:div {:class style/section-h3} (i18n/t :ui/home-url)]
        [:div {:class [style/small-muted "u-mb-8"]} (i18n/t :ui/home-url-hint)]
        [:div {:class "u-bold u-mb-4"} (i18n/t :ui/home-url-org-default)]
        [:div {:class "u-flex-items-gap-8 u-mb-8"}
         [url-input {:value (:default d) :disabled? off? :options (suggestions choices)
                     :placeholder (i18n/t :ui/home-url-builtin)
                     :on-change #(model/send! [:admin.home/default %])}]]
        (when (seq (:organizations choices))
          [:div {:class "u-mb-8"}
           [:div {:class "u-bold u-mb-4"} (i18n/t :ui/home-url-organizations)]
           (for [{:keys [id name]} (:organizations choices)]
             ^{:key id}
             [:div {:class "u-flex-items-gap-8 u-mb-4"}
              [:span {:class "admin-label admin-label--m"} name]
              [url-input {:value (get-in d [:organizations (keyword id)]
                                         (get-in d [:organizations id]))
                          :disabled? off?
                          :options (suggestions choices)
                          :placeholder (i18n/t :ui/home-url-inherit)
                          :on-change #(model/send! [:admin.home/organization id %])}]])])
        [overrides {:draft d :choices choices :off? off?}]
        [:div {:class "u-flex-items-gap-8 u-mt-8"}
         [ui/button {:intent :primary :disabled (or off? (not dirty?))
                     :on-click #(model/send! [:admin.home/save])}
          (i18n/t (if busy :ui/cp-saving :ui/save))]
         [ui/button
          {:disabled (or busy (not dirty?)) :on-click #(model/send! [:admin.home/discard])}
          (i18n/t :ui/cp-discard)]
         (when dirty? [:span {:class style/small-muted} (i18n/t :ui/cp-unsaved)])
         (when (and saved (not dirty?) (not error))
           [:span {:class "u-text-added"} (i18n/t :ui/saved)])]
        (when error [ui/problem error nil "u-mt-8"])
        (when (str/blank? (:default d))
          [:div {:class [style/small-muted "u-mt-8"]} (i18n/t :ui/home-url-builtin-hint)])]))))
