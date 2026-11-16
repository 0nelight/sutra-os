(ns sutra-ui.apps.ontology.panel.common
  (:require [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.data :as data]
            [sutra-ui.state.forms :as fs]
            [sutra-ui.state.shell :as shell]
            [sutra.kit.remote :as rd]
            [sutra-ui.apps.ontology.model :as ont-model]
            [sutra-ui.ui.core :as ui]
            [sutra-ui.apps.ontology.ot-view :as ot-view]
            [sutra.i18n.core :as i18n]))

(def new-function-form   (fs/form :new-function))

(def edit-function-form  (fs/form :edit-function))

(defn confirm-delete!
  [title fx]
  (ont-model/send! [:ontology/confirm
                    {:title title :body (i18n/t :ui/om-delete-staged-body) :danger? true}
                    fx]))

(def kind-defs
  [{:id :discover         :label :ui/discover         :icon :atlas}
   {:id :object-types     :label :ui/object-types       :icon :cube
    :count-fn #(count (:object-types @catalog/schema))}
   {:id :object-type-groups :label :ui/object-type-groups :icon :group-objects
    :count-fn #(count (:object-type-groups @catalog/canonical-schema))}
   {:id :link-types       :label :ui/link-types         :icon :link
    :count-fn #(count (:links @catalog/schema))}
   {:id :action-types     :label :ui/action-types       :icon :flash
    :count-fn #(count (catalog/declared-actions))}
   {:id :shared-properties :label :ui/shared-properties :icon :property
    :count-fn #(count (:shared-properties @catalog/canonical-schema))}
   {:id :interfaces       :label :ui/interfaces         :icon :layers
    :count-fn #(count (:interfaces @catalog/schema))}
   {:id :functions        :label :ui/functions          :icon :function
    :count-fn #(count (:functions @catalog/canonical-schema))}
   {:id :transforms       :label :ui/transforms         :icon :exchange :more? true
    :count-fn #(count (:transforms @catalog/canonical-schema))}
   {:id :datasets         :label :ui/datasets           :icon :th :more? true
    :count-fn #(let [n (count (rd/value-or @data/datasets {}))] (when (pos? n) n))}
   {:id :packages         :label :ui/packages           :icon :import :more? true}
   {:id :audit            :label :ui/audit-log          :icon :history :more? true
    :count-fn #(let [n (count (rd/value-or @data/audit-entries []))] (when (pos? n) n))}])

(defn- kind-item [active {:keys [id label icon count-fn]}]
  (let [n (when count-fn (count-fn))]
    [ui/menu-item {:text (i18n/t label) :icon icon :active (= active id)
                   :label (when (and n (pos? n)) n)
                   :on-click #(ont-model/edit! [] assoc :section id :detail nil :search nil)}]))

(defn left-rail []
  (let [active      (or (:section @ont-model/ui) :discover)
        [core more] ((juxt remove filter) :more? kind-defs)]
    (into [ui/menu [ui/menu-divider {:title (i18n/t :ui/ontology)}]]
           (concat (for [k core] ^{:key (:id k)} [kind-item active k])
                   [^{:key :--more} [ui/menu-divider {:title (i18n/t :ui/om-rail-more)}]]
                   (for [k more] ^{:key (:id k)} [kind-item active k])))))

(defn- creator-menu []
  (let [go-add (fn [section panel-key]
                 (ont-model/edit! [] assoc :section section :creator-open? false)
                 (shell/show-panel! panel-key))]
    (into [ui/menu]
          [[ui/menu-item
            {:text [:<> [:div (i18n/t :ui/object-type)]
                    [:div {:class "sutra-menu__sub"} (i18n/t :ui/object-type-hint)]]
             :multiline true
             :on-click #(do (ont-model/edit! [] assoc :creator-open? false)
                            (ot-view/open-create!))}]
           [ui/menu-item
            {:text [:<> [:div (i18n/t :ui/link-type)]
                    [:div {:class "sutra-menu__sub"} (i18n/t :ui/link-type-hint)]]
             :multiline true
             :on-click #(go-add :link-types :add-link)}]
           [ui/menu-item
            {:text [:<> [:div (i18n/t :ui/kind-interface)]
                    [:div {:class "sutra-menu__sub"} (i18n/t :ui/interface-hint)]]
             :multiline true
             :on-click #(go-add :interfaces :add-interface)}]
           [ui/menu-item
            {:text [:<> [:div (i18n/t :ui/object-type-group)]
                    [:div {:class "sutra-menu__sub"} (i18n/t :ui/object-type-group-hint)]]
             :multiline true
             :on-click #(go-add :object-type-groups :add-object-type-group)}]
           [ui/menu-item
            {:text [:<> [:div (i18n/t :ui/kind-function)]
                    [:div {:class "sutra-menu__sub"} (i18n/t :ui/function-hint)]]
             :multiline true
             :on-click #(go-add :functions :add-function)}]
           [ui/menu-item
            {:text [:<> [:div (i18n/t :ui/action-type)]
                    [:div {:class "sutra-menu__sub"} (i18n/t :ui/action-type-hint)]]
             :multiline true
             :on-click (fn []
                         (ont-model/edit! [] assoc
                                :section :action-types
                                :editor-open? true
                                :creator-open? false)
                         (fs/set-form! :action-editor
                       {:editing-name nil :name "" :params []
                        :rules [] :submission-criteria []}))}]
           [ui/menu-divider]
           [ui/menu-item
            {:text [:<> [:div (i18n/t :ui/datasource)]
                    [:div {:class "sutra-menu__sub"} (i18n/t :ui/datasource-hint)]]
             :multiline true
             :on-click (fn []
                         (ont-model/edit! [] assoc :creator-open? false)
                         (shell/enter! :source-manager {:wizard? true}))}]])))

(defn new-button []
  [ui/popover {:is-open            (boolean (:creator-open? @ont-model/ui))
               :on-interaction     (fn [o _] (ont-model/edit! [] assoc :creator-open? o))
               :placement          :bottom-end
               :popover-class-name "ontology-creator__popover"
               :content            [creator-menu]}
   [ui/button {:intent :primary}
    (i18n/t :ui/new-entity)]])

(defn section-shell
  [body]
  [ui/card body])
