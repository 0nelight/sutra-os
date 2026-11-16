(ns sutra-ui.forms.external
  (:require [sutra-ui.state.forms :as fs]
            [sutra-ui.ui.core :as ui]
            [clojure.string :as str]
            [reagent.core :as r]
            [sutra-ui.ui.busy :as busy]
            [sutra-ui.feature :as feature]
            [sutra-ui.forms.events]
            [sutra-ui.state.data :as data]
            [sutra-ui.state.db :as db]
            [sutra-ui.ui.form :as form]
            [sutra-ui.ui.inputs :as inputs]
            [sutra-ui.forms.base :refer [named via]]
            [sutra.i18n.core :as i18n]))

(defn datasource-options
  []
  (sort-by (comp name first) (seq (data/datasource-map))))

(defn ds-items
  [ds-list]
  (for [[k spec] ds-list]
    {:value k :label (str (name k) " · " (some-> spec :type name))}))

(defn- field-name-keywords
  [fields]
  (vec (for [{nm :name} fields
             :when (and nm (seq (clojure.string/trim nm)))]
         (keyword (clojure.string/trim nm)))))

(defn col-name
  [c]
  (if (map? c) (:name c) c))

(defn row-check
  [{:keys [title] :as props}]
  [ui/tooltip {:content title}
   [:span
    [ui/checkbox (-> props (dissoc :title) (update :class vector "u-m-0"))]]])

(defn column-select
  [{:keys [ds-key table value placeholder on-change extra-style]}]
  (let [cols (when (and ds-key (seq table))
               (get-in @data/ds-introspection [:columns [ds-key table]]))]
    [:div {:style (merge {:flex 2 :min-width 0} extra-style)}
     (if (seq cols)
       [inputs/select-input nil
        {:value       value
         :placeholder (or placeholder "—")
         :strict?     true
         :class       "bp-fill"
         :items       (named (map col-name cols))
         :on-change   (fn [v _] (on-change v))}]
       [inputs/text-input (via value on-change)
        {:placeholder (or placeholder "column") :small true :fill true}])]))

(defn ext-default
  [ds-list]
  {:datasource (some-> ds-list first first)
   :ui-mode :existing
   :source-kind :table
   :table nil
   :query nil
   :primary-key nil
   :column-map {}})

(defn- ds-picker
  [{:keys [ds-list ds-key on-change]}]
  [form/field {:label (i18n/t :ui/datasource)}
   (if (empty? ds-list)
     [:div {:class "form-empty--error"}
      (i18n/t :ui/datasources-none)
      (when (feature/command :manage-datasources)
        [:a {:href "#"
             :on-click (fn [e]
                         (.preventDefault e)
                         (feature/command! :manage-datasources))
             :class "u-underline" :style {:color "var(--c-primary)"}}
         (i18n/t :ui/datasources-manage)])]
     [:div {:class "u-flex-row-center-6"}
      [inputs/select-input (via ds-key on-change)
       {:strict?    true
        :clearable? false
        :class      "u-flex-1"
        :items      (ds-items ds-list)}]
      (when (feature/command :manage-datasources)
        [ui/tooltip {:content (i18n/t :ui/datasource-manager-open)}
         [ui/button {:small true :on-click #(feature/command! :manage-datasources)}
          (i18n/t :ui/manage)]])])])

(defn mapped-attr
  [column-map col-kw]
  (some (fn [[a c]] (when (= c col-kw) a)) column-map))

(defn import-column!
  [form-atom col-name ontology-type]
  (fs/edit! form-atom
            (fn [m]
              (let [attr-kw (keyword col-name)
                    fields  (or (:fields m) [])
                    has?    (some #(= attr-kw (keyword (:name %))) fields)
                    fields' (if has? fields
                                (conj fields {:name col-name
                                              :type (name ontology-type)}))]
                (-> m
                    (assoc :fields fields')
                    (update-in [:external :column-map] assoc attr-kw attr-kw))))))

(defn- remove-imported-column!
  [form-atom col-name]
  (fs/edit! form-atom
            (fn [m]
              (let [col-kw  (keyword col-name)
                    cm      (get-in m [:external :column-map])
                    attr-kw (some (fn [[a c]] (when (= c col-kw) a)) cm)
                    fields  (or (:fields m) [])
                    fields' (if attr-kw
                              (vec (remove #(= attr-kw (keyword (:name %))) fields))
                              fields)
                    pk      (get-in m [:external :primary-key])
                    pk'     (if (= pk attr-kw) nil pk)]
                (-> m
                    (assoc :fields fields')
                    (update-in [:external :column-map] dissoc attr-kw)
                    (assoc-in [:external :primary-key] pk'))))))

(defn- rename-imported-attr!
  [form-atom col-kw old-attr-kw new-attr-name]
  (let [trimmed (str/trim (str new-attr-name))]
    (when (seq trimmed)
      (let [new-kw (keyword trimmed)]
        (fs/edit! form-atom
                  (fn [m]
                    (let [fields  (or (:fields m) [])
                          fields' (mapv (fn [f]
                                          (if (= old-attr-kw (keyword (:name f)))
                                            (assoc f :name trimmed)
                                            f))
                                        fields)
                          cm      (get-in m [:external :column-map])
                          cm'     (-> cm
                                      (dissoc old-attr-kw)
                                      (assoc new-kw col-kw))
                          pk      (get-in m [:external :primary-key])
                          pk'     (if (= pk old-attr-kw) new-kw pk)]
                      (-> m
                          (assoc :fields fields')
                          (assoc-in [:external :column-map] cm')
                          (assoc-in [:external :primary-key] pk')))))))))

(defn- change-imported-type!
  [form-atom attr-kw new-type]
  (fs/edit! form-atom update :fields
            (fn [fs]
              (mapv (fn [f]
                      (if (= attr-kw (keyword (:name f)))
                        (assoc f :type new-type)
                        f))
                    fs))))

(defn- set-pk! [form-atom attr-kw]
  (fs/edit! form-atom assoc-in [:external :primary-key] attr-kw))

(defn- column-row
  [{:keys [form-atom col attr-kw field is-pk?]}]
  (let [cn (col-name col)
        ct (when (map? col) (:type col))
        ot (when (map? col) (:ontology-type col))]
    (if attr-kw
      [:div {:class "form-column__row--mapped"}
       [:span {:class "form-column__name"}
        cn (when ct [:span {:class "staging__kv-key"} " (" ct ")"])]
       [:span {:class "form-column__arrow"} "→"]
       [inputs/text-input
        (via (:name field) #(rename-imported-attr! form-atom (keyword cn) attr-kw %))
        {:small true :class "form-ext__input"}]
       [inputs/select-input
        (via (:type field) #(change-imported-type! form-atom (keyword (:name field)) %))
        {:strict?    true
         :clearable? false
         :class      "form-ext__select"
         :items      (named ["string" "number" "boolean"])}]
       [ui/tooltip {:content (i18n/t :ui/use-as-primary-key)}
        [:span
         [ui/radio {:checked (boolean is-pk?)
                    :on-change #(set-pk! form-atom (keyword (:name field)))
                    :label "pk"
                    :class "form-pk__label"}]]]
       [ui/tooltip {:content (i18n/t :ui/column-remove)}
        [ui/button {:icon :cross :small true :outlined true :intent :danger
                    :on-click #(remove-imported-column! form-atom cn)}]]]
      [:div {:class "form-column__row"}
       [:span {:class "form-column__name--muted"}
        cn (when ct [:span {:class "u-muted-faint"} " (" ct ")"])]
       [ui/tooltip {:content (i18n/t :ui/column-import-hint)}
        [ui/button {:small true :on-click #(import-column! form-atom cn ot)}
         (i18n/t :ui/column-import)]]])))

(defn- existing-table-mapper
  [{:keys [form-atom ds-key table tables ext]}]
  (let [cols       (when (and ds-key (seq table))
                     (get-in @data/ds-introspection [:columns [ds-key table]]))
        loading?   (when (and ds-key (seq table))
                     (data/ds-columns-loading? ds-key table))
        column-map (or (:column-map ext) {})
        pk         (:primary-key ext)
        fields     (:fields @form-atom)]
    [form/field {:label (i18n/t :ui/table)}
     (cond
       (not ds-key)
       [:div {:class "form-hint"}
        (i18n/t :ui/pick-datasource-first)]
       (nil? tables)
       [busy/block {:class "form-hint"} (i18n/t :ui/tables-loading)]
       (empty? tables)
       [:div {:class "form-hint"}
        (i18n/t :ui/tables-none)]
       :else
       [inputs/select-input
        (via table (fn [v]
                     (fs/edit! form-atom
                               (fn [m]
                                 (-> m
                                     (assoc :fields [])
                                     (assoc-in [:external :table] (or v ""))
                                     (assoc-in [:external :column-map] {})
                                     (assoc-in [:external :primary-key] nil))))))
        {:placeholder (i18n/t :ui/select-table)
         :strict?     true
         :class       "bp-fill"
         :items       (named tables)}])
     (when (seq table)
       [:div {:class "u-mt-10"}
        [:div {:class "form-importer__heading"}
         (i18n/t :ui/columns-in-table {:table table})]
        (cond
          loading?
          [busy/block {:class "form-hint"} (i18n/t :ui/columns-loading)]
          (empty? cols)
          [:div {:class "form-hint"}
           (i18n/t :ui/columns-none-reported)]
          :else
          (into [:div {:class "form-importer__list"}]
                (for [c cols
                      :let [cn (col-name c)
                            col-kw (keyword cn)
                            attr-kw (mapped-attr column-map col-kw)
                            field (when attr-kw
                                    (some (fn [f]
                                            (when (= attr-kw
                                                     (keyword (:name f))) f))
                                          fields))
                            is-pk? (and attr-kw (= pk attr-kw))]]
                  ^{:key cn}
                  [column-row {:form-atom form-atom :col c
                               :attr-kw attr-kw :field field
                               :is-pk? is-pk?}])))])]))

(defn- create-table-block
  [_]
  (let [name-atom (r/atom "")
        err-atom  (r/atom nil)]
    (fn [{:keys [form-atom ds-key fields attrs]}]
      (let [pk (some-> (:primary-key (:external @form-atom)) name)
            nm (str/trim (str @name-atom))
            cols (mapv (fn [{n :name t :type}]
                         {:name n :type (keyword t)})
                       (filter #(seq (:name %)) fields))
            ready? (and ds-key (seq nm) (seq cols))
            mapped (fn []
                     (fs/edit! form-atom update :external assoc
                               :ui-mode :existing :source-kind :table :table nm :query nil
                               :column-map (into {} (for [a attrs] [a a])))
                     (reset! name-atom ""))
            submit #(db/send! [:shared.forms/create-table ds-key
                               (cond-> {:table nm :columns cols}
                                 (seq pk) (assoc :primary-key pk))
                               err-atom mapped])]
        [:div
         [:div {:class "form-create-table__note"}
          (i18n/t :ui/create-table-note-1) [:code "CREATE TABLE"]
          (i18n/t :ui/create-table-note-2)
          (if (seq cols)
            (str/join ", "
                      (for [{n :name t :type} cols]
                        (str n " : " t)))
            (i18n/t :ui/none-yet))
          (i18n/t :ui/create-table-note-3)]
         [:div {:class "ontology-form__field-row"}
          [inputs/text-input name-atom
           {:placeholder (i18n/t :ui/new-table-name)
            :on-key-down #(when (and ready? (= "Enter" (.-key %))) (submit))
            :class       "u-flex-1"}]
          [ui/tooltip {:content (i18n/t (cond
                                          (not ds-key)  :ui/pick-datasource-first
                                          (empty? cols) :ui/declare-a-field-first
                                          (empty? nm)   :ui/enter-a-name
                                          :else         :ui/issue-create-table))}
           [ui/button {:intent :primary :on-click submit :disabled (not ready?)}
            (i18n/t :ui/create-table)]]]
         (when @err-atom
           [:div {:class "form-create-table__error" :style {:color "var(--c-danger)"}}
            (i18n/t :ui/create-failed {:error @err-atom})])]))))

(defn- query-block
  [{:keys [ext set-ext!]}]
  [ui/text-area {:value (or (:query ext) "")
                 :placeholder (i18n/t :ui/query-placeholder)
                 :on-change #(set-ext! :query (.. % -target -value))
                 :rows 3
                 :fill true
                 :class "bp-monospace-text"}])

(defn- primary-key-picker
  [{:keys [attrs primary-key on-change]}]
  (if (empty? attrs)
    [:div {:class "form-hint"}
     (i18n/t :ui/declare-a-field-first-hint)]
    [inputs/select-input (via (some-> primary-key keyword) on-change)
     {:placeholder (i18n/t :ui/select-primary-key)
      :strict?     true
      :class       "bp-fill"
      :items       (for [a attrs] {:value a :label (name a)})}]))

(defn- column-map-row
  [{:keys [attr cur ds-key table on-change]}]
  [:div {:class "form-colmap__row"}
   [:span {:class "form-colmap__attr"}
    (name attr) " →"]
   [column-select
    {:ds-key ds-key :table table :value cur
     :placeholder (i18n/t :ui/pick-a-column)
     :on-change on-change}]])

(defn- column-map-block
  [{:keys [form-atom attrs ext ds-key table]}]
  (if (empty? attrs)
    [:div {:class "form-hint"}
     (i18n/t :ui/no-ontology-fields)]
    [:div
     (for [a attrs]
       (let [cur (some-> (get-in ext [:column-map a]) name)]
         ^{:key a}
         [column-map-row
          {:attr a :cur cur :ds-key ds-key :table table
           :on-change (fn [v]
                        (let [kw (when (seq v) (keyword v))]
                          (fs/edit! form-atom update-in [:external :column-map]
                                    (fn [m]
                                      (if kw (assoc m a kw) (dissoc m a))))))}]))]))

(defn object-external-section
  [{:keys [form-atom]}]
  (let [{:keys [fields external] :as form} @form-atom
        backing (or (:backing-mode form) :synced)
        set-backing! (fn [m] (fs/edit! form-atom assoc :backing-mode m))]
    [:div {:class "form-backing" :style {:border (str "1px solid " "var(--c-primary)")}}
     [:div {:class "form-backing__title" :style {:color "var(--c-primary)"}}
      (i18n/t :ui/backing-question)]
     [ui/segmented-control
      {:fill true :intent :primary :class "u-mb-8" :value backing
       :on-value-change #(set-backing! %)
       :options [{:value :synced :label (i18n/t :ui/backing-synced)
                  :title (i18n/t :ui/backing-synced-hint)}
                 {:value :in-system :label (i18n/t :ui/backing-in-system)
                  :title (i18n/t :ui/backing-in-system-hint)}]}]
     (if (= backing :in-system)
       [:div {:class "dataconn-runs__caption"}
        (i18n/t :ui/backing-in-system-note)]
       (let [ds-list (datasource-options)
             attrs (field-name-keywords fields)
             ext (or external (ext-default ds-list))
             _ (when (and (nil? external) (seq ds-list))
                 (fs/edit! form-atom assoc :external ext))
             ui-mode  (or (:ui-mode ext)
                          (if (= :query (:source-kind ext)) :query :existing))
             ds-key   (:datasource ext)
             table    (:table ext)
             tables   (when ds-key (get-in @data/ds-introspection [:tables ds-key]))
             set-ext! (fn [k v] (fs/edit! form-atom assoc-in [:external k] v))
             set-mode! (fn [m]
                         (fs/edit! form-atom update :external
                                   (fn [x]
                                     (cond-> (assoc x :ui-mode m)
                                       (= m :existing) (assoc :source-kind :table
                                                              :query nil)
                                       (= m :create)   (assoc :source-kind :table
                                                              :query nil
                                                              :table nil
                                                              :column-map {})
                                       (= m :query)    (assoc :source-kind :query
                                                              :table nil
                                                              :column-map {})))))]
         (when ds-key
           (data/ensure-ds-tables! ds-key)
           (when (and (= ui-mode :existing) (seq table))
             (data/ensure-ds-columns! ds-key table)))
         [:<>
          [:div {:class "form-backing__hint"}
           (i18n/t :ui/sources-read-only)]
          [ds-picker {:ds-list ds-list
                      :ds-key ds-key
                      :on-change (fn [v]
                                   (fs/edit! form-atom update :external
                                             (fn [x]
                                               (assoc x :datasource v
                                                      :table nil
                                                      :column-map {}))))}]
          (when ds-key
            [:<>
             [form/field {:label (i18n/t :ui/source)}
              [ui/segmented-control
               {:fill true :intent :primary :value ui-mode
                :on-value-change #(set-mode! %)
                :options [{:value :existing :label (i18n/t :ui/map-existing-table)
                           :title (i18n/t :ui/map-existing-table-hint)}
                          {:value :create :label (i18n/t :ui/create-new-table)
                           :title (i18n/t :ui/create-new-table-hint)}
                          {:value :query :label (i18n/t :ui/sql-query)
                           :title (i18n/t :ui/sql-query-hint)}]}]]

             (case ui-mode
               :existing
               [existing-table-mapper
                {:form-atom form-atom :ds-key ds-key
                 :table table :tables tables :ext ext}]

               :create
               [create-table-block {:form-atom form-atom :ds-key ds-key
                                    :fields fields :attrs attrs}]

               :query
               [query-block {:ext ext :set-ext! set-ext!}])

             (when (not= ui-mode :existing)
               [:<>
                [:div {:class "u-spacer-8"}]
                [form/field {:label (i18n/t :ui/primary-key-field)}
                 [primary-key-picker {:attrs attrs
                                      :primary-key (:primary-key ext)
                                      :on-change #(set-ext! :primary-key %)}]]
                [form/field {:label (i18n/t (if (= ui-mode :query)
                                              :ui/column-map-query
                                              :ui/column-map-external))}
                 [column-map-block {:form-atom form-atom :attrs attrs :ext ext
                                    :ds-key ds-key :table table}]]])])]))]))

(defn entity-resolution-section
  [{:keys [form-atom]}]
  (let [{:keys [fields resolve-entity match-keys]} @form-atom
        names (->> fields (map :name) (filter seq) distinct)
        mk    (set (map clojure.core/name (or match-keys [])))
        toggle-mk! (fn [n]
                     (fs/edit! form-atom update :match-keys
                               (fn [ks] (let [s (set (map clojure.core/name (or ks [])))]
                                          (vec (if (s n) (disj s n) (conj s n)))))))]
    [:div {:class "form-resolve"}
     [ui/checkbox
      {:checked (boolean resolve-entity)
       :on-change #(fs/edit! form-atom assoc :resolve-entity (.. % -target -checked))
       :label-element [:span {:class "form-resolve__title"}
                       (i18n/t :ui/resolve-duplicates)]}]

     [:div {:class "form-resolve__hint"}
      (i18n/t :ui/resolve-duplicates-hint)]
     (when resolve-entity
       (if (empty? names)
         [:div {:class "form-empty--error"}
          (i18n/t :ui/match-keys-need-field)]
         [form/field {:label (i18n/t :ui/match-keys)}
          [:div {:class "form-chips"}
           (for [n names]
             ^{:key n}
             [ui/tag {:on-click #(toggle-mk! n) :intent :primary :minimal (not (mk n))}
              n])]]))]))

(defn form-fields->schema-fields [fields]
  (mapv (fn [{:keys [name type required display-name description]}]
          (cond-> {:name (keyword name) :type (keyword type)}
            required          (assoc :required true)
            (seq display-name) (assoc :display-name display-name)
            (seq description)  (assoc :description description)))
        (filter #(seq (:name %)) fields)))

(defn ext->object-external-source
  [external]
  (when (and external (:datasource external) (:primary-key external))
    (let [has-table? (and (= :table (:source-kind external))
                          (seq (:table external)))
          has-query? (and (= :query (:source-kind external))
                          (seq (:query external)))]
      (when (or has-table? has-query?)
        (cond-> {:datasource (:datasource external)
                 :primary-key (:primary-key external)
                 :column-map (or (:column-map external) {})}
          has-table? (assoc :table (:table external))
          has-query? (assoc :query (:query external)))))))

(defn ext->link-external-source
  [external cardinality]
  (when external
    (let [strategy (if (= (some-> cardinality keyword) :many-to-many)
                     :join-table
                     :endpoint-fk)]
      (case strategy
        :endpoint-fk
        (let [{:keys [fk-side fk-attribute on-null on-unresolved on-duplicate]} external]
          (when (and (#{:from :to} fk-side) fk-attribute)
            (cond-> {:strategy :endpoint-fk
                     :fk-side fk-side
                     :fk-attribute fk-attribute}
              (= :skip on-null)       (assoc :on-null       :skip)
              (= :skip on-unresolved) (assoc :on-unresolved :skip)
              (= :skip on-duplicate)  (assoc :on-duplicate  :skip))))
        :join-table
        (when (and (:datasource external)
                   (:from-key-column external) (:to-key-column external))
          (let [has-table? (and (= :table (:source-kind external))
                                (seq (:table external)))
                has-query? (and (= :query (:source-kind external))
                                (seq (:query external)))]
            (when (or has-table? has-query?)
              (cond-> {:strategy :join-table
                       :datasource (:datasource external)
                       :from-key-column (:from-key-column external)
                       :to-key-column (:to-key-column external)}
                has-table? (assoc :table (:table external))
                has-query? (assoc :query (:query external))))))))))

(defn ext-source->form-external
  [es]
  (when es
    (let [kind (if (:query es) :query :table)]
      (cond-> {:source-kind kind
               :ui-mode     (if (= :query kind) :query :existing)
               :datasource  (:datasource es)
               :table       (:table es)
               :query       (:query es)
               :primary-key (:primary-key es)
               :column-map  (or (:column-map es) {})
               :from-key-column (:from-key-column es)
               :to-key-column   (:to-key-column es)}
        (= :endpoint-fk (:strategy es))
        (assoc :fk-side       (:fk-side es)
               :fk-attribute  (:fk-attribute es)
               :on-null       (:on-null es)
               :on-unresolved (:on-unresolved es)
               :on-duplicate  (:on-duplicate es))))))
