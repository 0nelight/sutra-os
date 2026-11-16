(ns sutra-ui.forms.interface
  (:require [sutra-ui.ui.overlay :as overlay]
            [sutra-ui.object.card :as oc]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.forms :as fs]
            [sutra-ui.state.atlas :as kompass]
            [sutra-ui.state.ontology :as ontology]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.ui.form :as form]
            [sutra-ui.ui.inputs :as inputs]
            [sutra.types.link-side :as ls]
            [sutra.i18n.core :as i18n]
            [sutra-ui.ui.core :as ui]
            [sutra-ui.forms.base :refer [extends-chips named new-interface-form
                                                via]]
            [sutra-ui.forms.external :refer [form-fields->schema-fields row-check]]
            [sutra-ui.forms.property :refer [property-base-types]]))

(defn interface-form
  [{:keys [form-atom on-save on-cancel save-label]}]
  (let [{form-name    :name
         description  :description
         status       :status
         searchable   :searchable
         properties   :properties
         links        :links} @form-atom
        set-field! (fn [k v] (fs/edit! form-atom assoc k v))
        add-prop! (fn [] (fs/edit! form-atom update :properties
                                   (fnil conj []) {:name "" :type "string" :required false
                                                   :display-name "" :description ""}))
        remove-prop! (fn [idx]
                       (fs/edit! form-atom update :properties
                                 (fn [ps] (vec (concat (subvec ps 0 idx)
                                                       (subvec ps (inc idx)))))))
        update-prop! (fn [idx k v]
                       (fs/edit! form-atom update :properties
                                 assoc-in [idx k] v))
        add-link! (fn []
                    (fs/edit! form-atom update :links (fnil conj [])
                              {:link-name "" :to "" :to-kind "object"
                               :cardinality "one" :required false}))
        remove-link! (fn [idx]
                       (fs/edit! form-atom update :links
                                 (fn [ls] (vec (concat (subvec ls 0 idx)
                                                       (subvec ls (inc idx)))))))
        update-link! (fn [idx k v]
                       (fs/edit! form-atom update :links assoc-in [idx k] v))
        obj-keys (sort (map name (keys (:objects @catalog/schema))))
        iface-keys (sort (map name (keys (:interfaces @catalog/schema))))]
    [:div {:class "form-root"}
     [form/field {:label (i18n/t :ui/api-name)}
      [inputs/text-input (fs/at form-atom :name)
       {:placeholder (i18n/t :ui/api-name-example)}]]
     [form/field {:label (i18n/t :ui/display-name)}
      [inputs/text-input (fs/at form-atom :display-name)
       {:placeholder (i18n/t :ui/display-name-example)}]]
     [form/field {:label (i18n/t :ui/description)}
      [ui/text-area {:value (or description "")
                     :placeholder (i18n/t :ui/interface-description-hint)
                     :on-change #(set-field! :description (.. % -target -value))
                     :fill true}]]
     [form/row
      [form/field {:label (i18n/t :ui/status) :class "u-flex-1"}
       [inputs/select-input (fs/at form-atom :status)
        {:value      (or status "experimental")
         :strict?    true
         :clearable? false
         :items      (named ["active" "experimental" "deprecated" "example"])}]]
      [row-check {:checked (boolean searchable)
                  :on-change #(set-field! :searchable (.. % -target -checked))
                  :label (i18n/t :ui/searchable)}]]
     [extends-chips {:form-atom form-atom :exclude form-name}]
     [form/field {:label (i18n/t :ui/properties)}
      (doall
       (for [i (range (count properties))]
         (let [{ptype :type preq :required} (nth properties i)
               prop  (fn [k] (fs/at form-atom [:properties i k]))]
           ^{:key i}
           [:div {:class "form-prop-card"}
            [:div {:class "form-field-row"}
             [inputs/text-input (prop :name)
              {:placeholder (i18n/t :ui/property-name) :small true :class "u-flex-1"}]
             [inputs/select-input (prop :type)
              {:strict?    true
               :clearable? false
               :items      (named (cond-> property-base-types
                                    (and (seq ptype) (not (some #{ptype} property-base-types)))
                                    (conj ptype)))}]
             [row-check {:title (i18n/t :ui/required-properties-hint)
                         :checked (boolean preq)
                         :on-change #(update-prop! i :required (.. % -target -checked))
                         :label (i18n/t :ui/required-short)}]
             [ui/button {:small true :minimal true :icon :cross
                         :on-click #(remove-prop! i)}]]
            [:div {:class "form-field-row"}
             [inputs/text-input (prop :display-name)
              {:placeholder (i18n/t :ui/display-name-optional) :small true :class "u-flex-1"}]
             [inputs/text-input (prop :description)
              {:placeholder (i18n/t :ui/description-optional) :small true :class "u-flex-1"}]]])))
      [ui/button {:fill true :outlined true :icon :plus :on-click add-prop!}
       (i18n/t :ui/add-property)]]
     [form/field {:label (i18n/t :ui/link-constraints) :help (i18n/t :ui/link-constraints-hint)}
      (doall
       (for [i (range (count links))]
         (let [{:keys [to to-kind cardinality required]} (nth links i)
               link    (fn [k] (fs/at form-atom [:links i k]))
               targets (case to-kind
                         "interface" iface-keys
                         obj-keys)]
           ^{:key i}
           [:div {:class "form-field-row"}
            [inputs/text-input (link :link-name)
             {:placeholder (i18n/t :ui/link-name) :small true :class "u-flex-1"}]
            [inputs/select-input (link :to-kind)
             {:value      (or to-kind "object")
              :strict?    true
              :clearable? false
              :items      [{:value "object" :label (i18n/t :ui/object-type)}
                           {:value "interface" :label (i18n/t :ui/kind-interface)}]}]
            [inputs/select-input (via to #(update-link! i :to (or % "")))
             {:placeholder (i18n/t :ui/pick-to)
              :strict?     true
              :items       (for [k targets]
                             {:value k
                              :label (if (= "interface" to-kind)
                                       (or (not-empty (get-in @catalog/schema
                                                              [:interfaces (keyword k)
                                                               :display-name]))
                                           k)
                                       (oc/type-label (keyword k)))})}]
            [inputs/select-input (link :cardinality)
             {:value      (or cardinality "one")
              :strict?    true
              :clearable? false
              :items      [{:value "one" :label (i18n/t :ui/constraint-card-one)}
                           {:value "many" :label (i18n/t :ui/constraint-card-many)}]}]
            [row-check {:title (i18n/t :ui/required-constraints-hint)
                        :checked (boolean required)
                        :on-change #(update-link! i :required (.. % -target -checked))
                        :label (i18n/t :ui/required-short)}]
            [ui/button {:small true :minimal true :icon :cross
                        :on-click #(remove-link! i)}]])))
      [ui/button {:fill true :outlined true :icon :plus :on-click add-link!}
       (i18n/t :ui/add-link-constraint)]]
     [:div {:class "u-flex-gap-8"}
      [overlay/form-actions {:on-cancel on-cancel :on-ok on-save
                             :ok-label  (or save-label (i18n/t :ui/create))}]]]))

(defn add-interface-form []
  [:div {:class "ontology-function__addform"}
   [:h4 {:class "ontology-function__addform-title"} (i18n/t :ui/om-interface-new)]
   [interface-form
    {:form-atom new-interface-form
     :on-save (fn []
                (let [{:keys [name properties] :as form} @new-interface-form]
                  (when (seq name)
                    (ontology/add-interface!
                     (assoc form :properties (form-fields->schema-fields properties)))
                    (fs/reset-form! :new-interface)
                    (shell/show-panel! :none))))
     :on-cancel (fn []
                  (fs/reset-form! :new-interface)
                  (shell/show-panel! :none))}]])

(defn- interface->form-state
  [iface]
  (let [props (mapv (fn [{n :name t :type o :opts}]
                      {:name (clojure.core/name n)
                       :type (clojure.core/name t)
                       :required (boolean (:required o))
                       :display-name (or (:name o) "")
                       :description (or (:description o) "")})
                    (:property-list iface))
        links (mapv (fn [{:keys [link-name to-object-type
                                 to-interface cardinality required]}]
                      {:link-name (clojure.core/name link-name)
                       :to (some-> (or to-object-type to-interface)
                                   clojure.core/name)
                       :to-kind (cond to-interface "interface"
                                      :else "object")
                       :cardinality (clojure.core/name (or (ls/constraint-cardinality cardinality)
                                                           :one))
                       :required (boolean required)})
                    (:link-constraints iface))]
    {:name (clojure.core/name (:name iface))
     :display-name (or (:display-name iface) "")
     :description (or (:description iface) "")
     :status (some-> (:status iface) clojure.core/name)
     :searchable (if (some? (:searchable iface)) (:searchable iface) true)
     :extends (mapv clojure.core/name (or (:extends iface) []))
     :properties props
     :links links}))

(defn load-interface-form! [iface-kw]
  (when-let [iface (get-in @catalog/schema [:interfaces iface-kw])]
    (fs/set-form! :edit-interface (interface->form-state iface))
    true))

(defn open-interface-detail! [iface-kw]
  (when (load-interface-form! iface-kw)
    (kompass/note-recent! {:kind :interface :id iface-kw
                           :label (clojure.core/name iface-kw)})
    (shell/close-detail-modal!)
    (catalog/open-resource! :interfaces iface-kw)))
