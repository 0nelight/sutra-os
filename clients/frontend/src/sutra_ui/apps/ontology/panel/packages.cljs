(ns sutra-ui.apps.ontology.panel.packages
  (:require [sutra-ui.ui.core :as ui :refer [tag]]
            [clojure.string :as str]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.staging :as staging]
            [sutra-ui.apps.ontology.model :as ont-model]
            [sutra-ui.ui.busy :as busy]
            [sutra.i18n.core :as i18n]
            [sutra-ui.apps.ontology.panel.common :refer [section-shell]]))

(defn- package-status [{:keys [status present total]}]
  [tag {:round true :minimal true :intent ({:installed :success :partial :warning} status)}
   (case status
     :installed (i18n/t :ui/package-installed)
     :partial   (i18n/t :ui/package-partial {:n present :total total})
     (i18n/t :ui/package-not-installed))])

(defn- package-preview [resp]
  (let [{:keys [created updated skipped staged problems advisories]} resp
        by-kind (group-by (fn [e]
                            (str (:verb e) " · "
                                 (or (not-empty (str (:type e))) (name (or (:kind e) :instance)))))
                          staged)]
    [:div {:class "ontology-package__preview"}
     [:div {:class "ontology-action__section-label"} (i18n/t :ui/package-content)]
     [:div
      (i18n/t :ui/package-tally
              {:created (or created 0) :updated (or updated 0) :skipped (or skipped 0)})]
     (for [[k es] (sort-by key by-kind)]
       ^{:key k}
       [:div {:class "ontology-package__kind"}
        [:span {:class "ontology-action__rule-tag"} (str k " (" (count es) ")")] " "
        (str/join ", "
                  (sort (keep #(some-> (get-in % [:args :name]) catalog/name-of clojure.core/name)
                              es)))])
     (when (seq problems)
       [:div
        [:div {:class "ontology-action__section-label"} (i18n/t :ui/package-review)]
        (for [[n p] (map-indexed vector (if (map? problems) (mapcat val problems) problems))]
          ^{:key n} [:div {:class "ontology-action__problem"}
                     (or (:error p) (:message p) (pr-str p))])])
     (when (seq advisories)
       (for [[n a] (map-indexed vector advisories)]
         ^{:key (str "a" n)} [:div {:class "ontology-action__problem--warn"}
                              (or (:message a) (pr-str a))]))]))

(defn- packages-view []
  (let [{:keys [packages package]} @ont-model/ui
        {:keys [preview busy error staged]} package
        send! ont-model/send!]
    [section-shell
     [:div
      [:div {:class "ontology-action__section-hint"} (i18n/t :ui/packages-hint)]
      (when error [:div {:class "ontology-action__problem"} error])
      (if (nil? packages)
        [busy/block {:class "u-text-empty"} nil]
        (doall
         (for [{:keys [id name description requires object-types] :as pkg} packages
               :let [pv (get preview id)]]
           ^{:key id}
           [:div {:class "ontology-action__rule-card"}
            [:div {:class "ontology-action__rule-head"}
             [:span {:class "ontology-action__param-title"} (or name id)]
             [:span {:class "ontology-action__mono ontology-action__rule-tag"} id]
             [package-status pkg]
             [:span {:class "u-flex-1"}]
             [ui/button
              {:on-click #(send! [:ontology.package/preview id])
               :disabled busy}
              (i18n/t :ui/package-preview)]
             [ui/tooltip {:content (i18n/t :ui/package-install-hint)}
              [ui/button
               {:intent :primary :on-click #(send! [:ontology.package/install id])
                :disabled (or busy (nil? pv) (seq (:problems pv)))}
               (i18n/t :ui/package-install)]]]
            [:div {:class "ontology-action__section-hint"} description]
            (when (seq requires)
              [:div {:class "ontology-action__section-hint"}
               (i18n/t :ui/package-requires {:names (str/join ", " requires)})])
            [:div {:class "ontology-action__section-hint"}
             (i18n/t :ui/package-object-types {:n (count object-types)})]
            (when (= busy id)
              [busy/block {:class "ontology-action__section-hint"} nil])
            (when pv [package-preview pv])
            (when (get staged id)
              [:div {:class "ontology-package__staged"}
               (i18n/t :ui/package-staged {:n (count (staging/staged-list))})
               " "
               [ui/button
                {:intent :primary :on-click #(send! [:ontology.package/commit id])}
                (i18n/t :ui/package-commit)]])])))]]))

(def packages-pane
  (machine/entering ont-model/ontology [:ontology.packages/enter] packages-view))
