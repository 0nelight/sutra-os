(ns sutra-ui.apps.object-view.actions
  (:require [sutra-ui.ui.overlay :as overlay]
            [sutra-ui.ui.core :as ui]
            [clojure.string :as str]
            [sutra.capps.table :as tbl]
            [sutra.i18n.core :as i18n]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.apps.object-view.model :as ov-model]
            [sutra-ui.object.card :as oc]
            [sutra-ui.ui.busy :as busy]
            [sutra.types.link-side :as ls]
            [sutra.types.submission :as csub]
            [sutra.types.lifecycle :as lifecycle]
            [sutra-ui.apphost.action-form :as af]))

(defn action-label
  [akw spec]
  (or (not-empty (:display-name spec)) (tbl/humanize akw)))

(defn field-label
  [f]
  (or (not-empty (str (get-in f [:opts :name]))) (tbl/humanize (:name f))))

(defn prop-label [object-type prop]
  (if-let [f (some #(when (= (:name %) prop) %)
                   (get-in @catalog/schema [:object-types (catalog/name-of object-type) :fields]))]
    (field-label f)
    (tbl/humanize prop)))

(defn- params-of
  [spec]
  (vec (for [[p v] (:params spec)] [p (:type v)])))

(defn- has-param-of?
  [spec ty]
  (some (fn [[_ t]] (ls/satisfies-type? @catalog/schema ty t)) (params-of spec)))

(defn applicable
  [object-type]
  (->> (catalog/declared-actions)
       (filter (fn [[_ spec]] (has-param-of? spec object-type)))
       (sort-by (comp name key))))

(defn offered
  [object-type instance]
  (let [ot (get-in @catalog/schema [:object-types object-type])]
    (->> (applicable object-type)
         (remove (fn [[akw _]]
                   (when-let [tr (and (:lifecycle ot)
                                      (lifecycle/transition-for [[object-type ot]] akw))]
                     (not (some #{(lifecycle/state-value
                                   (get-in instance [:values (:property tr)]))}
                                (:from tr))))))
         (sort-by (fn [[akw spec]]
                    [(if (= "prominent" (some-> (:visibility spec) name)) 0 1) (name akw)])))))

(defn applicable-to-link
  [type-a type-b]
  (->> (catalog/declared-actions)
       (filter (fn [[_ spec]]
                 (and (has-param-of? spec type-a)
                      (has-param-of? spec type-b))))
       (sort-by (comp name key))))

(defn- launcher [] (:action-launcher @ov-model/ui))

(defn open-bound!
  ([bindings action-kw spec] (open-bound! bindings action-kw spec nil))
  ([bindings action-kw spec {:keys [on-applied rows]}]
   (ov-model/send! [:object-view.action/open {:action     action-kw
                                              :spec       spec
                                              :bindings   bindings
                                              :rows       rows
                                              :on-applied on-applied}])))

(defn creates?
  [spec object-type]
  (boolean (some #(and (= :create (:op %))
                       (= object-type (catalog/name-of (:object-type %))))
                 (:rules spec))))

(defn set-level
  [object-type]
  (let [applic (applicable object-type)
        known  (set (map first applic))]
    (concat (for [[akw spec] applic] [akw spec true])
            (for [[akw spec] (sort-by (comp name key) (catalog/declared-actions))
                  :when (and (not (known akw)) (creates? spec object-type))]
              [akw spec false]))))

(defn- close! [] (ov-model/send! [:object-view.action/close]))

(defn- launcher-submit
  [_descriptor params {:keys [action]}]
  (ov-model/promise! [:object-view.action/submit action params]))

(defn- launcher-evaluate
  [action values search]
  (ov-model/promise! [:object-view.action/evaluate action values search]))

(defn- plain-values [x]
  (let [m (if (map? (:values x)) (:values x) x)]
    (when (map? m) m)))

(defn- value-text [v]
  (cond
    (nil? v)     "—"
    (boolean? v) (i18n/t (if v :ui/ox-yes :ui/ox-no))
    (keyword? v) (name v)
    :else        (str v)))

(defn- changes
  [t before after]
  (let [b (plain-values before)
        a (plain-values after)]
    (for [k (distinct (concat (keys a) (keys b)))
          :when (and (keyword? k) (not (str/starts-with? (name k) "_"))
                     (not= (get b k) (get a k)))]
      [(prop-label t k) (get b k) (get a k) k])))

(defn- op-label [op]
  (case (some-> op name)
    "create" (i18n/t :ui/ov-edit-create)
    "update" (i18n/t :ui/ov-edit-update)
    "delete" (i18n/t :ui/ov-edit-delete)
    (some-> op name tbl/humanize)))

(defn- preview-entry [{:keys [op target before after]}]
  (let [t  (some-> (:type target) catalog/name-of)
        id (:id target)]
    [:li {:class "explorer-action__preview-item"}
     [:div {:class "explorer-action__preview-op"}
      [:span {:class (str "explorer-action__preview-tag explorer-action__preview-tag--"
                          (some-> op name))}
       (op-label op)]
      (when t [:span {:class "explorer-action__preview-type"} (oc/type-label t)])
      (when (and t id) [oc/object-link t id])]
     (into [:dl {:class "explorer-action__preview-changes"}]
           (for [[label b a k] (changes t before after)]
             ^{:key label}
             [:div {:class "explorer-action__preview-change"
                    :data-prop (name k) :data-value (pr-str a)}
              [:dt label]
              [:dd
               (when (some? b)
                 [:span {:class "explorer-action__preview-before"} (value-text b)])
               (when (some? b) " → ")
               (value-text a)]]))]))

(defn- outcome [error preview busy?]
  (when (or error preview busy?)
    [:<>
     (when error
       [:div {:class "explorer-action__error"} error])
     (when preview
       [:div {:class "explorer-action__preview"}
        [:div {:class "explorer-action__preview-head"}
         (i18n/t :ui/ov-action-preview-head {:n (count preview)})]
        (if (empty? preview)
          [:div {:class "explorer-action__preview-empty"} (i18n/t :ui/ov-action-preview-none)]
          (into [:ul {:class "explorer-action__preview-list"}]
                (for [[i ev] (map-indexed vector preview)]
                  ^{:key i}
                  [preview-entry ev])))])
     (when busy?
       [busy/block {:class "explorer-action__subtitle"} nil])]))

(defn launcher-modal
  []
  (when-let [{:keys [action spec bindings rows values defaults preview error busy?]}
             (launcher)]
    (let [spec  (csub/with-section-overrides spec)
          specs (csub/with-param-formats (csub/action-param-descriptors (:params spec))
                  @catalog/canonical-schema spec)]
      [overlay/modal {:on-close   #(close!)
                      :body-class "explorer-action__body"
                      :title [ui/tooltip {:content (name action)}
                              [:span (action-label action spec)]]}
       (when (seq bindings)
         (into [:div {:class "explorer-action__subtitle"} (i18n/t :ui/ov-action-for)]
               (for [[t id] (sort-by (comp str key) bindings)]
                 ^{:key (str t)}
                 [:span " " (if (sequential? id)
                              (i18n/t :ui/ox-scope-selected {:n (count id)})
                              [oc/object-link t id {:row (get rows t)}])])))
       (if (empty? specs)
         [:div {:class "explorer-action__empty"}
          (i18n/t :ui/ov-action-no-params)]
         [:div {:class "consumer__app"}
          [af/action-form
           {:object-titles {}}
           {:action     action
            :specs      specs
            :sections   (csub/form-sections (:form spec) specs)
            :defaults   (merge defaults values)
            :hidden     (set (keys values))
            :evaluate   launcher-evaluate
            :submit     launcher-submit
            :label      (i18n/t :ui/ov-action-apply)
            :busy-label (i18n/t :ui/ov-action-running)
            :cancel-label (i18n/t :ui/cancel)
            :on-cancel  close!
            :aux        [{:label (i18n/t :ui/ov-action-preview)
                          :on-click #(ov-model/send! [:object-view.action/preview action %])}]
            :issue      error
            :extra      (outcome nil preview busy?)}]])
       (when (empty? specs) (outcome error preview busy?))])))

(defn object-actions-menu
  [object-type instance & [{:keys [placed on-applied]}]]
  (let [acts (remove (fn [[akw _]] (contains? (set placed) akw))
                     (offered object-type instance))]
    [ui/popover
     {:placement :bottom-end
      :class "explorer-action__menu"
      :content
      (into [ui/menu]
            (for [[akw spec] acts]
              [ui/menu-item {:text (action-label akw spec)
                             :on-click #(open-bound! {object-type (:id instance)} akw spec
                                                     {:on-applied on-applied
                                                      :rows       {object-type instance}})}]))}
     [ui/tooltip {:content (if (empty? acts)
                             (i18n/t :ui/ov-actions-none {:type (oc/type-label object-type)})
                             (i18n/t :ui/ov-actions-hint))}
      [ui/button {:outlined true :small true :disabled (empty? acts)}
       [:span {:class "u-label-long"} (i18n/t :ui/actions-menu)]
       [:span {:class "u-label-short"} "⚡▾"]]]]))

(defn link-actions-menu
  [focused-type focused-id peer]
  (let [peer-type (:object-type peer)
        acts      (applicable-to-link focused-type peer-type)]
    (if (empty? acts)
      [:span {:class "explorer-action__none"} "—"]
      [ui/popover
       {:placement :bottom-end
        :class "explorer-action__menu"
        :content
        (into [ui/menu]
              (for [[akw spec] acts]
                [ui/menu-item {:text     (action-label akw spec)
                               :on-click #(open-bound! {focused-type focused-id
                                                        peer-type (:id peer)}
                                                       akw spec)}]))}
       [ui/tooltip {:content (i18n/t :ui/ov-link-actions-hint)}
        [ui/button {:outlined true :small true}
         (i18n/t :ui/actions-menu)]]])))
