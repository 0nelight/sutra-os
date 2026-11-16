(ns sutra-ui.apps.admin-settings.organizations
  (:require [sutra-ui.ui.core :as ui]
            [sutra-ui.ui.table :as table]
            [clojure.set :as set]
            [clojure.string :as str]
            [reagent.core :as r]
            [sutra.i18n.core :as i18n]
            [sutra.kit.remote :as rd]
            [sutra-ui.apps.admin-settings.model :as model]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.principals :as principals]
            [sutra-ui.state.forms :as fs]
            [sutra-ui.ui.form :as form]
            [sutra-ui.ui.inputs :as inputs]
            [sutra-ui.ui.overlay :as overlay]
            [sutra-ui.ui.style :as style]))

(defn- send! [& ev] (model/send! (vec ev)))

(def ^:private org-fields
  [:description :expand-access :discovery :markings :guests :grants :home-space
   :collaborates-with :apply :members])

(def ^:private space-fields
  [:description :organization :organizations :shared-with :grants :markings])

(defn- nm [x] (some-> x name))

(defn- rid-str [x] (str/replace (str x) #"^:" ""))

(defn- raw-by-name
  [section name-field]
  (into {} (for [[r spec] (get-in @model/db [:security-raw section])]
             [(nm (get spec name-field)) [r spec]])))

(defn- rid->name [section name-field]
  (into {} (for [[n [r _]] (raw-by-name section name-field)] [(rid-str r) n])))

(defn- names-of [section name-field xs]
  (let [m (rid->name section name-field)]
    (mapv #(get m (rid-str %) (nm %)) xs)))

(defn- rids-of [section name-field ns]
  (let [m (raw-by-name section name-field)]
    (mapv #(or (some-> (first (get m %)) rid-str) %) ns)))

(defn- usernames [ids] (mapv principals/principal-label ids))

(defn- user-ids [names]
  (let [by-name (set/map-invert @principals/principal-names)]
    (mapv #(get by-name % %) names)))

(defn- stage! [k action entity-name spec fields]
  (send! :admin.security/stage k action
         (assoc (select-keys spec fields) :name (keyword entity-name))))

(defn- tags [label values on-change]
  [form/field {:label label}
   [ui/tag-input {:values values :on-change #(on-change (vec %))
                  :placeholder (i18n/t :ui/orgs-add)}]])

(defn- description [draft]
  [form/field {:label (i18n/t :ui/orgs-description)}
   [inputs/text-input (fs/at draft :description) {:fill true}]])

(defn- stage-actions [busy on-stage close-key]
  [:div {:class "ui-form__actions"}
   [overlay/form-actions {:on-cancel    #(model/send! [:admin/close close-key])
                          :ok-label     (i18n/t :ui/orgs-stage)
                          :ok-disabled? busy
                          :on-ok        on-stage}]])

(def ^:private audiences [:everyone :collaborators :members])

(defn- ui-key [prefix k] (keyword "ui" (str prefix (name k))))

(defn- discovery-select [draft aspect]
  [form/field {:label (i18n/t (ui-key "orgs-discovery-" aspect)) :inline? true}
   [inputs/select-input (fs/at draft [:discovery aspect])
    {:clearable? false
     :items      (for [a audiences] {:value a :label (i18n/t (ui-key "orgs-audience-" a))})}]])

(defn- org-editor
  [_org-name spec]
  (let [draft (r/atom (-> (select-keys spec org-fields)
                          (update :discovery #(merge {:name :everyone :users :everyone
                                                      :groups :everyone} %))))]
    (fn [org-name _]
      (let [{:keys [busy error]} (:orgs @model/db)
            spaces (map :name (:spaces (rd/value-or (get-in @model/db [:spaces :data]) nil)))]
        [ui/callout {:class "u-mb-12"}
         [:div {:class "u-bold u-mb-8"} org-name]
         (when error [ui/problem error nil "u-mb-8"])
         [description draft]
         [tags (i18n/t :ui/orgs-members) (usernames (:members @draft))
          #(swap! draft assoc :members (user-ids %))]
         [tags (i18n/t :ui/orgs-guests) (usernames (:guests @draft))
          #(swap! draft assoc :guests (user-ids %))]
         [tags (i18n/t :ui/orgs-collaborators)
          (names-of :organizations :organization-name (:collaborates-with @draft))
          #(swap! draft assoc :collaborates-with
                  (rids-of :organizations :organization-name %))]
         [form/field {:label (i18n/t :ui/orgs-home-space)}
          [inputs/select-input nil
           {:value     (first (names-of :spaces :space-name
                                        (keep identity [(:home-space @draft)])))
            :items     (remove #{"default"} spaces)
            :on-change (fn [v _]
                         (swap! draft assoc :home-space
                                (when (seq v) (first (rids-of :spaces :space-name [v])))))}]]
         [form/field {:label (i18n/t :ui/orgs-discovery)}
          (for [a [:name :users :groups]] ^{:key a} [discovery-select draft a])]
         [tags (i18n/t :ui/orgs-apply) (mapv nm (:apply @draft))
          #(swap! draft assoc :apply %)]
         [tags (i18n/t :ui/orgs-expand-access) (mapv nm (:expand-access @draft))
          #(swap! draft assoc :expand-access %)]
         [stage-actions busy #(stage! :orgs :update-organization org-name
                                      (merge spec @draft) org-fields)
          :org-open]
         [:div {:class [style/small-muted "u-mt-8"]} (i18n/t :ui/orgs-staged-hint)]]))))

(defn- space-editor [_space-name spec]
  (let [draft (r/atom (select-keys spec space-fields))]
    (fn [space-name _]
      (let [{:keys [busy error]} (:spaces @model/db)
            detail (:space-detail @model/db)]
        [ui/callout {:class "u-mb-12"}
         [:div {:class "u-bold u-mb-8"} space-name
          (when detail
            [:span {:class style/small-muted}
             " · " (i18n/t (if (= "shared" (nm (:ontology detail)))
                             :ui/spaces-ontology-shared :ui/spaces-ontology-private))])]
         (when error [ui/problem error nil "u-mb-8"])
         [description draft]
         [tags (i18n/t :ui/spaces-organizations)
          (names-of :organizations :organization-name
                    (concat (keep identity [(:organization @draft)]) (:organizations @draft)))
          #(swap! draft assoc
                  :organization nil
                  :organizations (rids-of :organizations :organization-name %))]
         (when (seq (:project-names detail))
           [:div {:class [style/small-muted "u-mb-8"]}
            (i18n/t :ui/spaces-projects) ": " (str/join ", " (:project-names detail))])
         [stage-actions busy #(stage! :spaces :update-space space-name
                                      (merge spec @draft) space-fields)
          :space-open]]))))

(defn- create-row [_k _action _placeholder]
  (let [v (r/atom "")]
    (fn [k action placeholder]
      [form/row
       [inputs/text-input v {:placeholder placeholder}]
       [ui/button {:disabled (str/blank? @v)
                   :on-click #(do (send! :admin.security/stage k action
                                         {:name (keyword (str/trim @v))})
                                  (reset! v ""))}
        (i18n/t :ui/orgs-create)]])))

(defn- org-list []
  (let [orgs (:organizations (rd/value-or (get-in @model/db [:orgs :data]) nil))]
    (if (seq orgs)
      [table/data-table
       {:rows         orgs
        :row-key      :name
        :class        "u-mb-12"
        :on-row-click #(send! :admin.orgs/open (:name %))
        :cols         [{:label (i18n/t :ui/orgs-name) :render #(vector :a (:name %))}
                       {:label (i18n/t :ui/orgs-members) :render :members}
                       {:label (i18n/t :ui/orgs-guests) :render :guests}
                       {:label (i18n/t :ui/orgs-spaces) :render #(str/join ", " (:spaces %))}]}]
      [:div {:class [style/small-muted "u-mb-12"]} (i18n/t :ui/orgs-none)])))

(defn- space-list []
  (let [spaces (:spaces (rd/value-or (get-in @model/db [:spaces :data]) nil))]
    [table/data-table
     {:rows         spaces
      :row-key      :name
      :class        "u-mb-12"
      :on-row-click #(when (not= "default" (:name %)) (send! :admin.spaces/open (:name %)))
      :cols         [{:label  (i18n/t :ui/spaces-name)
                      :render #(let [n (:name %)] (if (= "default" n) n [:a n]))}
                     {:label  (i18n/t :ui/spaces-organizations)
                      :render #(str/join ", " (:organizations %))}
                     {:label (i18n/t :ui/spaces-ontology) :render #(nm (:ontology %))}
                     {:label (i18n/t :ui/spaces-projects) :render :projects}]}]))

(def view
  (machine/entering
   model/admin [:admin.orgs/load]
   (fn []
     (let [{:keys [org-open space-open orgs spaces]} @model/db
           error (or (:error orgs) (rd/reason (:data orgs)))
           orgs-raw   (raw-by-name :organizations :organization-name)
           spaces-raw (raw-by-name :spaces :space-name)]
       [:div
        [:div {:class [style/small-muted "u-mb-12"]} (i18n/t :ui/orgs-intro)]
        (when (and error (not org-open))
          [ui/problem error nil "u-mb-12"])
        [:div {:class style/section-h3} (i18n/t :ui/orgs-title)]
        [org-list]
        (when-let [[_ spec] (and org-open (get orgs-raw org-open))]
          ^{:key org-open} [org-editor org-open spec])
        [create-row :orgs :create-organization (i18n/t :ui/orgs-new)]
        [:div {:class style/section-h3} (i18n/t :ui/spaces-title)]
        (when-let [e (and (not space-open) (:error spaces))]
          [ui/problem e nil "u-mb-12"])
        [space-list]
        (when-let [[_ spec] (and space-open (get spaces-raw space-open))]
          ^{:key space-open} [space-editor space-open spec])
        [create-row :spaces :create-space (i18n/t :ui/spaces-new)]]))))
