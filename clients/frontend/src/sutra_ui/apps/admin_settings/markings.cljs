(ns sutra-ui.apps.admin-settings.markings
  (:require [sutra-ui.ui.core :as ui]
            [sutra-ui.ui.table :as table]
            [clojure.string :as str]
            [reagent.core :as r]
            [sutra.i18n.core :as i18n]
            [sutra.kit.remote :as rd]
            [sutra-ui.apps.admin-settings.model :as model]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.ui.form :as form]
            [sutra-ui.ui.inputs :as inputs]
            [sutra-ui.ui.style :as style]))

(defn- send! [& ev] (model/send! (vec ev)))

(defn- nm [x] (if (keyword? x) (name x) (str x)))

(defn- joined [xs] (if (seq xs) (str/join ", " (map nm xs)) "—"))

(defn- detail []
  (let [{:keys [marking own inherited stops]} (:marking-detail @model/db)]
    (when marking
      [ui/callout {:class "u-mb-12"}
       [:div {:class "u-bold u-mb-8"} marking]
       [:div {:class style/small-muted} (i18n/t :ui/markings-own) ": " (joined own)]
       [:div {:class style/small-muted} (i18n/t :ui/markings-inherited) ": " (joined inherited)]
       [:div {:class style/small-muted} (i18n/t :ui/markings-stops) ": "
        (if (seq stops)
          (str/join ", " (for [{:keys [transform input]} stops] (str transform " ← " input)))
          "—")]
       [ui/button {:minimal true :class "u-mt-8"
                   :on-click #(model/send! [:admin/close :marking-detail])}
        (i18n/t :ui/close)]])))

(defn- category-table [category ms]
  [:div {:class "u-mb-12"}
   [:div {:class "u-bold u-mb-4"}
    (or category (i18n/t :ui/markings-no-category))
    (when (some :disjunctive ms) [:span {:class style/small-muted}
                                  " · " (i18n/t :ui/markings-disjunctive)])]
   [table/data-table
    {:rows         (sort-by (juxt #(- (or (:rank %) 0)) :marking) ms)
     :row-key      :marking
     :on-row-click #(send! :admin.markings/open (:marking %))
     :cols
     [{:label  (i18n/t :ui/markings-name)
       :render (fn [{:keys [marking required-on-projects]}]
                 [:<> [:a marking]
                  (when required-on-projects
                    [:span {:class style/small-muted} " · " (i18n/t :ui/markings-required)])])}
      {:label (i18n/t :ui/markings-rank) :render #(or (:rank %) "—")}
      {:label (i18n/t :ui/markings-cleared) :render #(joined (:cleared %))}
      {:label (i18n/t :ui/markings-remove-access) :render #(joined (:remove-access %))}
      {:label (i18n/t :ui/markings-carried) :render #(str (:own %) " / " (:in-force %))}]}]])

(defn- why-check []
  (let [user (r/atom "") resource (r/atom "")]
    (fn []
      (let [{:keys [why]} @model/db
            error (get-in @model/db [:markings :why-error])]
        [ui/callout {:class "u-mb-12"}
         [:div {:class style/section-h3} (i18n/t :ui/markings-why)]
         [form/row
          [inputs/text-input user {:placeholder (i18n/t :ui/markings-why-user)}]
          [inputs/text-input resource {:placeholder (i18n/t :ui/markings-why-resource)}]
          [ui/button {:disabled (or (str/blank? @user) (str/blank? @resource))
                      :on-click #(send! :admin.markings/why (str/trim @user)
                                        (str/trim @resource))}
           (i18n/t :ui/markings-why-check)]]
         (when error [ui/problem error nil])
         (when why
           [:div
            [:div {:class "u-bold"}
             (i18n/t (if (:blocked? why) :ui/markings-why-blocked :ui/markings-why-free)
                     {:user (:username why) :resource (:resource why)})]
            (when (seq (:markings-missing why))
              [:div (i18n/t :ui/markings-why-missing) ": " (joined (:markings-missing why))])
            (for [{:keys [wall any-of]} (:walls-outside why)]
              ^{:key wall}
              [:div (i18n/t :ui/markings-why-wall {:wall wall}) ": " (joined any-of)])])]))))

(def view
  (machine/entering
   model/admin [:admin.markings/load]
   (fn []
     (let [{rdata :data error :error} (:markings @model/db)
           ms    (:markings (rd/value-or rdata nil))
           error (or error (rd/reason rdata))]
       [:div
        [:div {:class [style/small-muted "u-mb-12"]} (i18n/t :ui/markings-intro)]
        (when error [ui/problem error nil "u-mb-12"])
        (if (seq ms)
          (for [[cat cms] (sort-by (comp str key) (group-by :category ms))]
            ^{:key (str cat)} [category-table (some-> cat nm) cms])
          [:div {:class [style/small-muted "u-mb-12"]} (i18n/t :ui/markings-none)])
        [detail]
        [why-check]]))))
