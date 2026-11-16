(ns sutra-ui.forms.object
  (:require [sutra-ui.ui.overlay :as overlay]
            [clojure.string :as str]
            [sutra-ui.ui.core :as ui]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.data :as data]
            [sutra-ui.state.forms :as fs]
            [sutra-ui.state.atlas :as kompass]
            [sutra-ui.state.ontology :as ontology]
            [sutra-ui.state.panel :as panel]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.ui.form :as form]
            [sutra-ui.ui.inputs :as inputs]
            [sutra.i18n.core :as i18n]
            [sutra.types.scaffold :as scaffold]
            [sutra-ui.forms.base :refer [via]]
            [sutra-ui.forms.external :refer [entity-resolution-section
                                                    ext->object-external-source
                                                    ext-source->form-external
                                                    object-external-section]]
            [sutra-ui.forms.property :refer [fed-by-section form-unmet-links
                                                    interfaces-section properties-section]]))

(declare object-form-problem)

(defn object-form [{:keys [form-atom on-save on-cancel save-label create?]}]
  (let [{external :external :as form} @form-atom
        ui-mode (or (:ui-mode external)
                    (if (= :query (:source-kind external)) :query :existing))
        names-locked? (and (not= :in-system (:backing-mode form))
                           (not= :fed (:backing-mode form))
                           (= ui-mode :existing)
                           (seq (:table external)))
        backing (if (= :fed (:backing-mode form))
                  [fed-by-section (:fed-by form)]
                  [object-external-section {:form-atom form-atom}])
        problem (when (:attempted form) (object-form-problem form create? (form-unmet-links form)))]
    [:div {:class "form-root"}
     (when create? backing)
     [:div {:class "form-ot__section"}
      [:div {:class "form-ot__section-title"} (i18n/t :ui/ot-metadata)]
      [:div {:class "form-row--end"}
       [form/field {:label (i18n/t :ui/ot-display-name) :class "u-flex-1"}
        [inputs/text-input (fs/at form-atom :display-name)
         {:placeholder (i18n/t :ui/ot-display-name-example)}]]
       [form/field {:label (i18n/t :ui/plural-name) :required? true :class "u-flex-1"}
        [inputs/text-input (fs/at form-atom :plural)
         {:placeholder (i18n/t :ui/object-plural-example)}]]
       [form/field {:label (i18n/t :ui/symbol)}
        [inputs/text-input (via (:symbol form) #(fs/edit! form-atom assoc :symbol %))
         {:placeholder "🛩" :maxLength 4 :class "form-object__code"}]]]
      [:div {:class "form-row--end"}
       [form/field {:label     (i18n/t :ui/api-name) :required? true :class "u-flex-1"
                    :help      (when create? (i18n/t :ui/om-ot-api-name-hint))}
        [inputs/text-input (fs/at form-atom :name)
         {:placeholder (i18n/t :ui/object-name-example) :class "form-mono"}]]
       [form/field {:label (i18n/t :ui/ot-visibility)}
        [inputs/select-input (fs/at form-atom :visibility)
         {:placeholder (i18n/t :ui/ot-visibility-unset)
          :strict?     true
          :class       "bp-fill"
          :items       [{:value :prominent :label (i18n/t :ui/ot-visibility-prominent)}
                        {:value :normal :label (i18n/t :ui/ot-visibility-normal)}
                        {:value :hidden :label (i18n/t :ui/ot-visibility-hidden)}]}]]]
      [form/field {:label (i18n/t :ui/ot-description)}
       [ui/text-area {:value (or (:description form) "")
                      :rows 2
                      :on-change #(fs/edit! form-atom assoc :description (.. % -target -value))
                      :fill true}]]
      [form/field {:label (i18n/t :ui/ot-aliases)}
       [ui/tag-input
        {:values (ontology/aliases->vec (:aliases form))
         :placeholder (i18n/t :ui/ot-aliases-hint)
         :add-on-blur true
         :fill true
         :on-change #(fs/edit! form-atom assoc :aliases (str/join ", " %))}]]]

     (when-not create? backing)
     [properties-section form-atom {:names-locked? names-locked?}]
     [interfaces-section form-atom]
     [entity-resolution-section {:form-atom form-atom}]
     (when problem
       [:div {:class "form-empty--error"} (i18n/t problem)])
     [:div {:class "u-flex-gap-8"}
      [overlay/form-actions {:on-cancel on-cancel :on-ok on-save
                             :ok-label  (or save-label (i18n/t :ui/create))}]]]))

(defn entity-detail-modal
  [body on-close]
  [overlay/modal {:on-close #(on-close)
                  :header? false
                  :class "form-detail-modal"
                  :style {:padding 0 :max-width "none" :max-height "92vh" :overflow "hidden"}}
   body])

(defn feeding-transforms
  [obj-kw]
  (->> (:transforms @catalog/canonical-schema)
       (filter (fn [[_ spec]]
                 (some #(= obj-kw (some-> (:object-type %) keyword))
                       (cons (:output spec) (:outputs spec)))))
       (map key)
       sort
       vec))

(defn default-backing-mode
  []
  (if (some (fn [[_ spec]] (not= :rest (some-> (:type spec) keyword))) (data/datasource-map))
    :synced
    :in-system))

(defn object-form-external-source
  [{:keys [backing-mode external external-orig]}]
  (when (= :synced (or backing-mode :synced))
    (if (and external-orig (= external (ext-source->form-external external-orig)))
      external-orig
      (ext->object-external-source external))))

(defn object-form->spec
  [form]
  (let [es (object-form-external-source form)]
    (cond-> (select-keys form [:name :plural :symbol :title :fields :implements
                               :display-name :description :aliases :visibility
                               :resolve-entity :match-keys :extra])
      es                      (assoc :external-source es)
      (contains? form :extra) (assoc :whole? true))))

(defn object-form-problem
  [form create? & [unmet-links]]
  (or (first (scaffold/object-type-problem
              (assoc (select-keys form [:name :plural :title])
                     :properties (map :name (:fields form)))
              create?))
      (cond
        (seq unmet-links)                                  :ui/ot-interface-links-unmet
        (and (= :synced (or (:backing-mode form) :synced))
             (nil? (object-form-external-source form)))    :ui/external-source-required)))

(defn save-object-form!
  [form-atom old-kw]
  (let [form @form-atom]
    (if (object-form-problem form (nil? old-kw) (form-unmet-links form))
      (do (fs/edit! form-atom assoc :attempted true) nil)
      (let [spec (object-form->spec form)]
        (if old-kw
          (ontology/update-object-type! old-kw spec)
          (ontology/add-object-type! spec))))))

(defn load-object-form!
  [obj-kw]
  (let [canon   (get-in @catalog/canonical-schema [:object-types obj-kw])
        es      (:external-source canon)
        fed     (feeding-transforms obj-kw)
        synced? (or (some? es)
                    (boolean (some #(= obj-kw (catalog/name-of %))
                                   (keys (:objects @data/import-plan)))))]
    (fs/set-form! :edit-object
                  (assoc (ontology/object-type->form
                          (or canon
                              {:object-type-name obj-kw
                               :attribute-list (mapv (fn [{:keys [name type opts]}]
                                                       (cond-> {:attribute-name name
                                                                :attribute-type type}
                                                         opts (assoc :opts opts)))
                                                     (get-in @catalog/schema
                                                             [:objects obj-kw]))}))
                         :loaded-as obj-kw
                         :backing-mode (cond synced?   :synced
                                             (seq fed) :fed
                                             :else     :in-system)
                         :fed-by fed
                         :external (ext-source->form-external es)
                         :external-orig es))))

(defn open-object-detail!
  [obj-kw]
  (load-object-form! obj-kw)
  (kompass/note-recent! {:kind :object-type :id obj-kw
                         :label (clojure.core/name obj-kw)})
  (shell/close-detail-modal!)
  (catalog/open-resource! :object-types obj-kw))

(defn open-edit-object!
  [obj-kw]
  (load-object-form! obj-kw)
  (panel/set-panel-editing! true)
  (shell/open-detail-modal! :object-type obj-kw))
