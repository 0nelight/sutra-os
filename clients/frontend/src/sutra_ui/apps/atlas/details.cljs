(ns sutra-ui.apps.atlas.details
  (:require [reagent.core :as r]
            [sutra-ui.ui.core :as ui]
            [clojure.string :as str]
            [sutra.i18n.core :as i18n]
            [sutra-ui.state.data :as data]
            [sutra-ui.state.atlas :as kompass]
            [sutra-ui.state.principals :as principals]
            [sutra.kit.remote :as rd]
            [sutra-ui.state.governance :as gov]
            [sutra-ui.apps.atlas.files :as files]
            [sutra-ui.apps.atlas.model :as kompass-model]
            [sutra-ui.apphost.markdown-text :as md]
            [sutra-ui.ui.page :as page]
            [sutra-ui.ui.style :as style]
            [sutra-ui.resource.core :as res]
            [sutra.types.protection :as protection]))

(defn- can-manage?
  [pkey]
  (get-in @kompass/kompass-index [:rights (name pkey) :manage?]))

(def ^:private roles [:owner :editor :viewer :finder])
(def ^:private role-color
  {:owner "var(--orange3)" :editor "var(--blue3)" :viewer "var(--green3)"
   :finder "var(--gray3)"})

(defn- role-chip [role]
  [:span {:class "kompass-role-chip" :style {:background (role-color role "var(--gray3)")}}
   (name role)])

(defn- empty-note [txt]
  [:div {:class [style/small-muted "kompass-details__note"]} txt])

(defn markdown [text]
  [:div {:class "kompass-markdown"
         :dangerouslySetInnerHTML (r/unsafe-html (md/markdown->html text))}])

(defn documentation
  [_pkey _proj]
  (let [editing (r/atom false)
        draft   (r/atom nil)]
    (fn [pkey proj]
      (let [txt (or (kompass-model/project-doc pkey) "")]
        [:div
         [:div {:class "kompass-doc__header"}
          [:div {:class style/label} (i18n/t :ui/kompass-cover-page)]
          [ui/button {:small true :on-click #(if @editing
                             (do (kompass-model/set-project-doc! pkey @draft)
                                 (reset! editing false))
                             (do (reset! draft txt) (reset! editing true)))}
           (i18n/t (if @editing :ui/save :ui/kompass-edit))]]
         (when-let [d (:description proj)]
           [:div {:class "kompass-doc__description"} d])
         (if @editing
           [ui/text-area
            {:fill true :value @draft :on-change #(reset! draft (.. % -target -value))
             :placeholder (i18n/t :ui/kompass-cover-page-placeholder)
             :class ["bp-monospace-text" "kompass-textarea--long"]}]
           (if (str/blank? txt)
             (empty-note (i18n/t :ui/kompass-cover-page-empty))
             [markdown txt]))]))))

(defn- activity-tab []
  (r/create-class
   {:component-did-mount #(data/refresh-audit!)
    :reagent-render
    (fn []
      (let [entries (rd/value-or @data/audit-entries [])]
        [:div
         [:div {:class [style/small-muted "u-mb-8"]}
          (i18n/t :ui/kompass-activity-hint)]
         (if (empty? entries)
           (empty-note (i18n/t :ui/kompass-activity-empty))
           [:div
            (for [[i e] (map-indexed vector (take 40 entries))]
              ^{:key i}
              [:div {:class "kompass-activity__entry"}
               [:div {:class "kompass-activity__op"}
                (str (or (:op e) (:action e) (:event e) (i18n/t :ui/change)))]
               [:div {:class style/small-muted}
                (str/join " · "
                          (remove str/blank?
                                  [(some-> (or (:user e) (:principal e)) principals/principal-label)
                                   (some-> (or (:at e) (:timestamp e) (:ts e)) str)]))]])])]))}))

(defn- grant-row [proj manage? grant]
  (let [pid   (:principal grant)
        label (principals/principal-label pid)]
    [:div {:class "kompass-grant__row"}
     [:span {:class "kompass-grant__avatar"}
      (-> label str (subs 0 1) str/upper-case)]
     [ui/tooltip {:content (str pid)}
      [:span {:class "kompass-grant__principal"}
       label]]
     [role-chip (:role grant)]
     (when manage?
       [ui/tooltip {:content (i18n/t :ui/kompass-revoke)}
        [ui/button
         {:minimal true :small true :icon :cross
          :on-click #(let [next (vec (remove #{grant} (:grants proj)))]
                       (kompass-model/update-project! (:project-name proj)
                                                      (:description proj) next))
          :aria-label (i18n/t :ui/kompass-revoke)}]])]))

(defn- add-grant-form [_proj]
  (let [principal (r/atom "")
        role      (r/atom "viewer")]
    (fn [proj]
      [:div {:class "kompass-grant__add-form"}
       [ui/input-group {:value @principal :placeholder (i18n/t :ui/kompass-grant-principal)
                        :on-change #(reset! principal (.. % -target -value))}]
       [:div {:class "u-flex-gap-6"}
        [ui/html-select {:value @role :on-change #(reset! role (.. % -target -value))
                         :class "u-flex-1"}
         (for [r roles] ^{:key r} [:option {:value (name r)} (name r)])]
        [ui/button
         {:small true :class "u-flex-none" :disabled (str/blank? @principal)
          :on-click #(let [p (str/trim @principal)]
(when (seq p)
(let [next (conj (vec (:grants proj))
{:principal p :role (keyword @role)})]
(kompass-model/update-project! (:project-name proj)
(:description proj) next)
(reset! principal ""))))}
         (i18n/t :ui/kompass-grant-role)]]])))

(defn- request-access-row [_pkey]
  (let [open?  (r/atom false)
        reason (r/atom "")
        role   (r/atom "viewer")]
    (fn [pkey]
      (let [pending (kompass-model/pending-access-request pkey)]
        [:div {:class "u-mt-border-top"}
         (cond
           pending
           [:div {:class "kompass-access__pending"}
            (i18n/t :ui/kompass-access-pending {:role (name (:role pending))})]
           @open?
           [:div {:class "u-flex-col-gap-6"}
            [ui/html-select {:value @role :on-change #(reset! role (.. % -target -value))}
             (for [r roles]
               ^{:key r} [:option {:value (name r)}
                          (i18n/t :ui/kompass-request-role {:role (name r)})])]
            [ui/text-area
             {:fill true :value @reason :placeholder (i18n/t :ui/kompass-access-reason)
              :on-change #(reset! reason (.. % -target -value))
              :class ["bp-monospace-text" "kompass-textarea--short"]}]
            [:div {:class "u-flex-gap-6"}
             [ui/button
              {:small true :disabled (str/blank? @reason)
               :on-click #(do (kompass-model/request-project-access! pkey @role @reason)
(reset! open? false))}
              (i18n/t :ui/kompass-access-submit)]
             [ui/button {:small true :on-click #(reset! open? false)}
              (i18n/t :ui/cancel)]]]
           :else
           [ui/button {:outlined true :on-click #(reset! open? true)}
            (i18n/t :ui/kompass-access-request)])]))))

(defn- access-tab [pkey proj]
  (let [manage? (can-manage? pkey)
        grants  (:grants proj)]
    [:div
     [:div {:class [style/small-muted "u-mb-8"]}
      (i18n/t (if manage? :ui/kompass-access-owner-hint :ui/kompass-access-viewer-hint))]
     [:div {:class "kompass-access__roles-label"}
      (i18n/t :ui/kompass-roles {:n (count grants)})]
     (if (empty? grants)
       (empty-note (i18n/t :ui/kompass-grants-none))
       [:div (for [g grants] ^{:key (str (:principal g) (:role g))}
                  [grant-row proj manage? g])])
     (when manage? [add-grant-form proj])
     [:div {:class "u-mt-12"}
      [:div {:class "kompass-access__markings-label"}
       (i18n/t :ui/kompass-required-markings)]
      [:div {:class [style/small-muted "u-mt-2"]}
       (i18n/t :ui/kompass-required-markings-hint)]]
     (when manage?
       (let [on? (contains? (:sharing @kompass/kompass-index) (str (:id proj)))]
         [:div {:class "u-mt-12"}
          [ui/switch {:class "u-m-0" :checked on? :label (i18n/t :ui/kompass-sharing-toggle)
                      :on-change #(kompass-model/set-project-sharing! pkey (not on?))}]
          [:div {:class [style/small-muted "u-mt-2"]} (i18n/t :ui/kompass-sharing-hint)]]))
     (when-not manage? [request-access-row pkey])]))

(defn- schema-members
  [pkey]
  (for [{:keys [family nm]} (files/project-files-anywhere pkey)
        :when (and (not (res/file? family))
                   (gov/protectable-ref? (res/protection-ref family nm)))]
    [family nm]))

(defn- reviewer-list [custom]
  (str/join ", " (map (fn [{:keys [user group]}] (or group user)) (:reviewers custom))))

(defn- parse-reviewers [s]
  (vec (for [x (str/split (str s) #",\s*") :when (seq (str/trim x))]
         (if (str/starts-with? x "@")
           {:group (subs x 1)}
           {:user (str/trim x)}))))

(defn- policy-form [pname policy _manage?]
  (let [custom (r/atom (:custom policy))
        names  (r/atom (reviewer-list (:custom policy)))
        save!  #(kompass-model/send! [:kompass.policy/save pname %])]
    (fn [_ policy manage?]
      [:div
       [ui/switch {:class "u-m-0" :checked (boolean (:protect-all policy))
                   :disabled (not manage?) :label (i18n/t :ui/branch-protect-new)
                   :on-change #(save! {:protect-all (.. % -target -checked)})}]
       [:div {:class [style/small-muted "u-mt-2"]} (i18n/t :ui/branch-protect-new-hint)]
       [:div {:class "kompass-access__roles-label u-mt-12"} (i18n/t :ui/branch-policy)]
       [ui/radio-group
        {:selected-value (if (nil? @custom) "default" "custom") :disabled (not manage?)
         :on-change (fn [e]
                      (if (= "default" (.. e -target -value))
                        (do (reset! custom nil) (save! {:review nil}))
                        (reset! custom {:count 1 :contributors? true})))
         :options [{:value "default" :label (i18n/t :ui/branch-policy-default)}
                   {:value "custom" :label (i18n/t :ui/branch-policy-custom)}]}]
       (when @custom
         [:div {:class "u-mt-2"}
          [ui/input-group {:small true :value @names :disabled (not manage?)
                           :placeholder (i18n/t :ui/branch-policy-reviewers)
                           :on-change #(reset! names (.. % -target -value))}]
          [:div {:class "u-flex-items-gap-8"}
           (i18n/t :ui/branch-policy-count)
           [ui/numeric-input
            {:small true :min 1 :minor-step-size nil :value (:count @custom 1)
             :disabled (not manage?)
             :on-value-change (fn [_ s _]
                                (let [v (js/parseInt s 10)]
                                  (when-not (js/isNaN v)
                                    (swap! custom assoc :count (max 1 v)))))}]]
          [ui/checkbox
           {:checked (boolean (:contributors? @custom)) :disabled (not manage?)
            :label (i18n/t :ui/branch-policy-contributors)
            :on-change #(swap! custom assoc :contributors? (.. % -target -checked))}]
          (when manage?
            [ui/button {:small    true
                        :on-click #(save! {:custom (assoc @custom :reviewers
                                                          (parse-reviewers @names))})}
             (i18n/t :ui/save)])])])))

(defn- bulk-protection [pkey manage?]
  (let [picked (r/atom #{})]
    (fn [pkey manage?]
      (let [rows (sort-by (fn [[f n]] [(name f) (name n)]) (schema-members pkey))
            ref  (fn [[f n]] (res/protection-ref f n))]
        [:div {:class "u-mt-12"}
         [:div {:class "kompass-access__roles-label"} (i18n/t :ui/branch-protected-resources)]
         (for [[f n :as row] rows
               :let [r (ref row)]]
           ^{:key r}
           [:div {:class "u-flex-items-gap-8"}
            (let [text [:span (when (gov/protected-ref? r) "⑂🔒 ") (res/display-name f n)]]
              (if manage?
                [ui/checkbox
                 {:class "u-m-0" :checked (contains? @picked r) :label-element text
                  :on-change #(swap! picked (if (.. % -target -checked) conj disj) r)}]
                text))])
         (when manage?
           [:div {:class "u-flex-items-gap-8 u-mt-2"}
            (for [[on? k] [[true :ui/branch-protect] [false :ui/branch-unprotect]]]
              ^{:key k}
              [ui/button {:small true :disabled (empty? @picked) :on-click (fn []
                                (gov/set-protection! @picked on?)
                                (reset! picked #{}))}
               (i18n/t k)])])]))))

(def ^:private branch-protection-tab
  (r/create-class
   {:component-did-mount
    (fn [this] (kompass-model/send! [:kompass.policy/load (name (second (r/argv this)))]))
    :component-did-update
    (fn [this [_ old]]
      (let [pkey (second (r/argv this))]
        (when (not= old pkey) (kompass-model/send! [:kompass.policy/load (name pkey)]))))
    :reagent-render
    (fn [pkey]
      (let [pname   (name pkey)
            policy  (kompass-model/at :policy)
            manage? (:may-manage policy)]
        (when (= pname (:project policy))
          [:div {:style {:font-size "var(--fs-md)"}}
           (let [{:keys [show reason]} (protection/trunk-view @gov/protections)]
             (case show
               :protected [:div {:class [style/small-muted "u-mb-8"]}
                           (i18n/t :ui/branch-trunk-protected)]
               :loading   [:div {:class [style/small-muted "u-mb-8"]}
                           (i18n/t :ui/branch-protection-loading)]
               :failed    [:div {:class [style/small-muted "u-mb-8"]}
                           (i18n/t :ui/branch-protection-failed {:reason (str reason)})]
               nil))
           ^{:key (pr-str (:custom policy))} [policy-form pname policy manage?]
           [bulk-protection pkey manage?]])))}))

(defn- queues-tab []
  [:div
   (empty-note (i18n/t :ui/kompass-queues-empty))
   [:div {:class style/small-muted}
    (i18n/t :ui/kompass-queues-hint)]])

(defn- details-tab [pkey proj]
  [:div
   [:dl {:class "kompass-viewer__facts"}
    [:div {:class "kompass-viewer__fact"}
     [:dt (i18n/t :ui/resource-id)] [:dd [:code (str (:id proj))]]]
    [:div {:class "kompass-viewer__fact"}
     [:dt (i18n/t :ui/kompass-files)] [:dd (files/project-count pkey)]]]
   [:div {:class "u-mt-12"} [documentation pkey proj]]])

(def tabs
  [[:details           :ui/kompass-documentation :info-sign]
   [:access            :ui/access                :lock]
   [:activity          :ui/kompass-activity      :feed]
   [:queues            :ui/kompass-queues        :flag]
   [:branch-protection :ui/branch-protection     :git-branch]])

(defn rail
  [open]
  (into [ui/button-group {:vertical true :minimal true :role "tablist"}]
        (for [[id label-k glyph] tabs]
          ^{:key id}
          [ui/tooltip {:content (i18n/t label-k) :placement :left}
           [ui/button {:role "tab" :aria-selected (= id open)
                       :active (= id open) :icon [ui/icon {:icon glyph :size 18}]
                       :on-click #(kompass-model/send! [:kompass/details-tab
                                                        (when (not= id open) id)])
                       :aria-label (i18n/t label-k)}]])))

(defn panel [pkey proj tab]
  [:<>
   [page/header {:title (some (fn [[id label-k _]] (when (= tab id) (i18n/t label-k))) tabs)}
    [ui/button {:minimal true :icon :cross :aria-label (i18n/t :ui/close)
                :on-click #(kompass-model/send! [:kompass/details-tab nil])}]]
   (case tab
      :details           [details-tab pkey proj]
      :activity          [activity-tab]
      :access            [access-tab pkey proj]
      :branch-protection [branch-protection-tab pkey]
      :queues            [queues-tab]
      nil)])
