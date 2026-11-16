(ns sutra-ui.apps.operations.core
  (:require [sutra-ui.ui.core :as ui :refer [link]]
            [clojure.string :as str]
            [sutra.i18n.core :as i18n]
            [sutra.kit.remote :as rd]
            [sutra-ui.apps.operations.events :as ev]
            [sutra-ui.ui.display :as display]
            [sutra-ui.ui.page :as page]
            [sutra-ui.ui.remote :as remote]
            [sutra-ui.health.watch :as health-watch]
            [sutra-ui.object.card :as oc]
            [sutra-ui.resource.link :as rl]
            [sutra-ui.apphost.platform :as platform]
            [sutra-ui.feature :as feature]
            [sutra-ui.state.auth :as auth]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.ui.style :as style]))

(defonce ^:private ui (shell/slice :operations))

(defonce ^:private ops (machine/frame ui {:name :operations}))

(defn- send! [event] (machine/dispatch event {:frame ops}))

(defn- name-or [x] (if (keyword? x) (name x) (str x)))

(defn- status-class [s]
  (case (some-> s name)
    ("passed" "ok" "succeeded") "u-text-added"
    ("warning")                 "u-text-modified"
    ("failed" "error")          "u-text-removed"
    "u-muted"))

(defn- status-label [s]
  (case (some-> s name)
    "passed"    (i18n/t :ui/ops-status-passed)
    "ok"        (i18n/t :ui/ops-status-ok)
    "succeeded" (i18n/t :ui/status-succeeded)
    "warning"   (i18n/t :ui/ops-status-warning)
    "failed"    (i18n/t :ui/ops-status-failed)
    "error"     (i18n/t :ui/ops-status-error)
    (some-> s name)))

(defn- status-text [s]
  [:span {:class ["u-bold" (status-class s)]} (status-label s)])

(defn- head-row [hint & actions]
  [page/toolbar
   [:div {:class ["ops-hint" style/small-muted]} hint]
   (into [:<>] actions)])

(defn- error-line [msg] (when msg [ui/problem msg nil "u-mb-8"]))

(defn- object-type-picker
  [value on-change]
  [ui/html-select {:fill true :class "u-mb-8" :value (or value "")
                   :on-change #(on-change (.. % -target -value))}
   [:option {:value ""} (i18n/t :ui/ops-pick-object-type)]
   (for [k (sort-by oc/type-label (keys (:object-types @catalog/schema)))]
     ^{:key k} [:option {:value (name k)} (oc/type-label k)])])

(defn- node-options [kind]
  (case kind
    "object-type" (for [k (sort-by oc/type-label (keys (:object-types @catalog/schema)))]
                    [(name k) (oc/type-label k)])
    "transform"   (for [k (sort (map name (keys (:transforms @catalog/canonical-schema))))] [k k])
    "dataset"     (for [k (sort (map name (keys (:datasets @catalog/canonical-schema))))] [k k])
    []))

(defn- result-box
  [{:keys [matched applied affected-ids errors invalid dry?]}]
  [ui/callout {:class "u-mb-10"}
   [:div {:class "u-bold u-mb-4"} (i18n/t (if dry? :ui/ops-preview-result :ui/ops-run-result))]
   [:div (i18n/t :ui/ops-found-applied {:m (or matched 0) :a (or applied 0)})
    (when (seq affected-ids)
      (str " · " (i18n/t :ui/ops-objects-touched {:n (count affected-ids)})))]
   (when (seq errors)
     [:div {:class "u-text-removed u-mt-2"}
      (i18n/t :ui/ops-errors {:n (count errors)}) " " (str/join "; " (take 3 errors))])
   (when (seq invalid)
     [:div {:class "u-text-modified u-mt-2"} (i18n/t :ui/ops-unresolved {:n (count invalid)})])])

(defn- action-picker [value on-change]
  [ui/html-select {:fill true :class "u-mb-8" :value (or value "")
                   :on-change #(on-change (.. % -target -value))}
   [:option {:value ""} (i18n/t :ui/ops-pick-action)]
   (for [k (sort-by #(rl/label :action %) (keys (catalog/declared-actions)))]
     ^{:key k} [:option {:value (name k)} (rl/label :action k)])])

(defn- automation-form []
  (let [{nm :name :keys [on action when args every err busy]} (:automation-form @ui)
        field  (fn [k] #(send! [:ops.automation-form/field k %]))
        typed  (fn [k] #(send! [:ops.automation-form/field k (.. % -target -value)]))]
    [ui/callout {:class "u-mb-10"}
     [:div {:class "u-bold u-mb-8"} (i18n/t :ui/ops-automation-new)]
     [:label {:class style/label} (i18n/t :ui/name)]
     [ui/input-group {:class "u-mb-8" :value nm
                      :placeholder (i18n/t :ui/ops-automation-name-example)
                      :on-change (typed :name)}]
     [:label {:class style/label} (i18n/t :ui/ops-watches)]
     [object-type-picker on (field :on)]
     [:label {:class style/label} (i18n/t :ui/ops-then-action)]
     [action-picker action (field :action)]
     [:label {:class style/label} (i18n/t :ui/ops-every-ms)]
     [ui/numeric-input
      {:class "u-mb-8" :value every :min 0 :minor-step-size nil :major-step-size 1000
       :placeholder (i18n/t :ui/ops-every-ms-hint)
       :on-value-change (fn [_ s _] (send! [:ops.automation-form/field :every s]))}]
     [ui/section
      {:title (i18n/t :ui/ops-advanced) :collapsible true :compact true
       :collapse-props {:default-is-open false} :class "u-mb-8"}
      [ui/section-card
       [:label {:class style/label} (i18n/t :ui/ops-condition-edn)]
       [ui/text-area {:fill true :class ["bp-monospace-text" "u-mb-8"] :value when
                      :placeholder "{:contains-any {:attr :labels :values [\"versand\"]}}"
                      :on-change (typed :when)}]
       [:label {:class style/label} (i18n/t :ui/ops-args-edn)]
       [ui/text-area {:fill true :class "bp-monospace-text" :value args
                      :placeholder "{:from-id {:self :id}}"
                      :on-change (typed :args)}]]]
     [error-line err]
     [ui/button
      {:intent   :primary
       :disabled (or busy (str/blank? nm) (str/blank? on) (str/blank? action))
       :on-click #(send! [:ops.automation-form/submit])}
      (i18n/t (if busy :ui/ops-saving :ui/create))]]))

(defn- effect-action [effect-spec]
  (some-> (ev/try-read-edn effect-spec) :ok :action))

(defn- listed
  [items what row]
  [remote/show items
   (fn [xs] (if (empty? xs)
              [ui/empty-state {:title (i18n/t what)}]
              (into [:<>] (map row) xs)))
   {:failed (i18n/t :ui/ops-load-failed)}])

(def ^:private proposal-columns [:open :applied :rejected :failed])

(defn- param-type [action p]
  (let [t (get-in @catalog/canonical-schema [:actions (keyword action) :params (keyword p) :type])]
    (some-> (if (map? t) (:type t) t) name)))

(defn- media-param? [action p]
  (contains? #{"media-reference" "attachment"} (param-type action p)))

(defn- param-value [action p v ref]
  (cond
    ref [ui/tooltip {:content (str v)} [:span (:title ref)]]
    (and (media-param? action p) (string? v) (seq v))
    [link {:href (str "/api/blob/" v) :target "_blank" :rel "noopener"}
     (i18n/t :ui/ops-proposal-document)]
    (or (map? v) (vector? v)) [:code (pr-str v)]
    (nil? v) [:span {:class "u-muted"} "—"]
    :else (str v)))

(defn- proposal-card
  [aid {pid :id :keys [status action args refs reason decision-log provenance accepted-args error
                       decided-by decided-at created-at object-id]}]
  (let [{:keys [editing busy err log?]} (get-in @ui [:proposals aid :cards pid])
        open?  (= "open" status)
        params (sort (into (set (keys args))
                           (keys (get-in @catalog/canonical-schema
                                         [:actions (keyword action) :params]))))]
    [ui/callout {:class "u-mb-10"}
     [:div {:class "u-flex-between"}
      [:div [rl/resource-link :action action]
       (when object-id [:span {:class "u-muted u-fs-12"} " · " object-id])]
      [:span {:class "u-muted u-fs-12"} (str (or decided-at created-at))
       (when decided-by (str " · " decided-by))]]
     (when reason
       [:div {:class "u-mt-4"} [:span {:class "u-muted"} (i18n/t :ui/ops-proposal-reason) ": "]
        reason])
     [display/property-list
      (for [p params :let [v (get args p)]]
        {:prop  p
         :label (name p)
         :render
         (if editing
           [ui/input-group
            {:fill      true
             :value     (get editing p "")
             :on-change #(send! [:ops.card/edit-field aid pid p (.. % -target -value)])}]
           [param-value action p v (get refs p)])})
      nil]
     (when (seq accepted-args)
       [:div {:class "u-mt-4 u-fs-12"}
        [:span {:class "u-muted"} (i18n/t :ui/ops-proposal-accepted-with) " "]
        [:code (pr-str accepted-args)]])
     (when (or (seq provenance) (seq decision-log))
       [:div {:class "u-mt-4 u-fs-12"}
        [link {:href     "#"
               :on-click (fn [e] (.preventDefault e) (send! [:ops.card/toggle-log aid pid]))}
         (i18n/t :ui/ops-proposal-decision-log)]
        (when log?
          [:pre {:class "u-fs-12 ops-log"}
           (pr-str (cond-> {} (seq provenance) (assoc :provenance provenance)
                           (seq decision-log) (assoc :decision-log decision-log)))])])
     (when error [:div {:class "u-text-removed u-mt-4"} error])
     (error-line err)
     (when open?
       [:div {:class "u-flex-wrap-gap-6 u-mt-8"}
        (if editing
          [:<>
           [ui/button {:intent :primary :disabled busy
                       :on-click #(send! [:ops.card/accept aid pid args])}
            (i18n/t :ui/ops-proposal-accept-edited)]
           [ui/button {:on-click #(send! [:ops.card/close aid pid])}
            (i18n/t :ui/ops-close)]]
          [:<>
           [ui/button {:intent :primary :disabled busy
                       :on-click #(send! [:ops.card/accept aid pid args])}
            (i18n/t :ui/ops-proposal-accept)]
           [ui/button {:disabled busy :on-click #(send! [:ops.card/edit aid pid params args])}
            (i18n/t :ui/ops-proposal-edit)]
           [ui/button {:outlined true :intent :danger :disabled busy
                       :on-click #(send! [:ops.card/reject aid pid])}
            (i18n/t :ui/ops-proposal-reject)]])])]))

(defn- proposals-panel [aid]
  (let [{:keys [column items counts]} (get-in @ui [:proposals aid])
        total #(get counts (name %) (get counts % 0))]
    [:div {:class "u-mt-8"}
     [:div {:class "u-flex-wrap-gap-6 u-mb-8"}
      (doall
       (for [c proposal-columns]
         ^{:key c}
         [ui/button {:intent   (when (= c column) :primary)
                     :on-click #(send! [:ops.proposals/column aid c])}
          (str (i18n/t (keyword "ui" (str "ops-proposal-" (name c)))) " (" (total c) ")")]))]
     [listed items :ui/ops-proposals-none
      (fn [p] ^{:key (:id p)} [proposal-card aid p])]
     (when (and (vector? items) (< (count items) (total column)))
       [ui/button {:on-click #(send! [:ops.proposals/load aid true])}
        (i18n/t :ui/ops-proposal-more)])]))

(defn- selected? [id] (= (str id) (str (:focus @ui))))

(defn- automation-row
  [{:keys [id name on paused last-fired-at last-matched muted muted-by muted-at subscribers
           every-ms effect-spec]
    :as a}]
  (let [{:keys [result busy]} (get-in @ui [:rows id])]
    [ui/card {:class        ["ops-row" (when (selected? id) "ops-row--on")]
              :id           (str "automation-" id)
              :interactive  true
              :compact      true
              :on-click     #(send! [:ops/select id])}
     [:div {:class "u-flex-between"}
      [:div [:b name]
       (when paused [:span {:class "u-text-modified u-fs-12"} " · " (i18n/t :ui/ops-paused)])
       (when muted
         [:span {:class "u-text-modified u-fs-12"} " · "
          (i18n/t (if (= "auto-mute" muted-by) :ui/ops-muted-auto :ui/ops-muted))
          (when muted-at (str " (" (.toLocaleString (js/Date. muted-at)) ")"))])]
      [:span {:class "u-flex-wrap-gap-6 u-fs-12"}
       [:span {:class "u-muted"} (i18n/t :ui/ops-watches)] [rl/resource-link :object-type on]
       (when-let [act (effect-action effect-spec)]
         [:<> [ui/icon {:icon :arrow-right :size 12 :class "u-muted"}]
          [rl/resource-link :action act]])]]
     [:div {:class [style/small-muted "u-mt-2"]}
      (if every-ms
        (i18n/t :ui/ops-fires-every {:s (js/Math.round (/ every-ms 1000))})
        (i18n/t :ui/ops-fires-manually))
      (when last-fired-at (str " · " (i18n/t :ui/ops-last-fired {:at last-fired-at})))
      (when last-matched (str " · " (i18n/t :ui/ops-last-matched {:n last-matched})))]
     [:div {:class "u-flex-wrap-gap-6 u-mt-8"}
      [ui/button {:intent :primary :disabled busy :on-click #(send! [:ops.row/run id])}
       (i18n/t :ui/run)]
      [ui/tooltip {:content (i18n/t :ui/ops-preview-hint)}
       [ui/button {:disabled busy :on-click #(send! [:ops.row/preview id])}
        (i18n/t :ui/ops-preview)]]
      [ui/button {:on-click #(send! [:ops.row/toggle-pause a])}
       (i18n/t (if paused :ui/ops-resume :ui/ops-pause))]
      [ui/tooltip {:content (i18n/t :ui/ops-mute-hint)}
       [ui/button {:on-click #(send! [:ops.row/mute id muted])}
        (i18n/t (if muted :ui/ops-unmute :ui/ops-mute))]]
      (let [me  (str (:user @auth/auth))
            on? (boolean (some #{me} (map str subscribers)))]
        [ui/tooltip {:content (i18n/t :ui/ops-subscribe-hint)}
         [ui/button {:on-click #(send! [:ops.row/subscribe id on?])}
          (i18n/t (if on? :ui/ops-unsubscribe :ui/ops-subscribe))]])
      [ui/button {:outlined true :intent :danger :on-click #(send! [:ops.row/delete id name])}
       (i18n/t :ui/delete)]]
     (when result [result-box result])]))

(defn- fires [runs none]
  (cond
    (nil? runs)   nil
    (empty? runs) [ui/empty-state {:title (i18n/t none)}]
    :else
    (into [:div {:class "u-fs-12"}]
          (for [{:keys [seq matched applied status detail fired-at run-at value]} (take 15 runs)]
            ^{:key seq}
            [:div {:class "u-flex-gap-8"}
             [status-text status]
             [:span (if (some? matched)
                      (i18n/t :ui/ops-found-applied {:m (or matched 0) :a (or applied 0)})
                      (str value))]
             [:span {:class "u-muted"} (str (or fired-at run-at))]
             (when detail
               [:span {:class "u-text-removed"} (subs detail 0 (min 60 (count detail)))])]))))

(defn- automation-detail [{:keys [id name]}]
  [:div {:class "ops-detail"}
   [page/header {:title name}
    [ui/button {:minimal true :icon :cross :aria-label (i18n/t :ui/ops-close)
                :on-click #(send! [:ops/deselect])}]]
   [:div {:class "u-section-label"} (i18n/t :ui/ops-proposals)]
   [proposals-panel id]
   [:div {:class "u-section-label"} (i18n/t :ui/ops-recent-fires)]
   [fires (get-in @ui [:rows id :runs]) :ui/ops-no-fires]])

(defn focus-automation!
  [id]
  (send! [:ops/focus id])
  (shell/enter! :operations {})
  (js/setTimeout #(some-> (.getElementById js/document (str "automation-" id))
                          (.scrollIntoView #js {:block "center"}))
                 600))

(machine/reg-sub ::automations (fn [db _] (select-keys db [:automations :automation-form])))

(def ^:private automations-pane
  (machine/entering
   ops [:ops.automations/enter]
   (fn []
     (let [{:keys [automations] form? :automation-form}
           @(machine/subscribe [::automations] {:frame ops})]
       [:<>
        [head-row (i18n/t :ui/ops-automate-hint)
         [ui/button {:on-click #(send! [:ops.automation-form/toggle])}
          (i18n/t (if form? :ui/ops-close :ui/ops-automation-new))]]
        (when form? [automation-form])
        [listed (or automations rd/loading) :ui/ops-automations-none
         (fn [a] ^{:key (:id a)} [automation-row a])]]))))

(def ^:private node-kind-label
  {"object-type" :ui/ops-node-object-type
   "transform"   :ui/ops-node-transform
   "dataset"     :ui/ops-node-dataset})

(defn- health-form []
  (let [{nm :name :keys [node-kind node-key kind query op value busy err]} (:health-form @ui)
        typed (fn [k] #(send! [:ops.health-form/field k (.. % -target -value)]))]
    [ui/callout {:class "u-mb-10"}
     [:div {:class "u-bold u-mb-8"} (i18n/t :ui/ops-check-new)]
     [:label {:class style/label} (i18n/t :ui/name)]
     [ui/input-group {:class "u-mb-8" :value nm
                      :placeholder (i18n/t :ui/ops-check-name-example)
                      :on-change (typed :name)}]
     [:label {:class style/label} (i18n/t :ui/ops-checks)]
     [:div {:class "u-flex-gap-8"}
      [ui/html-select {:class "u-mb-8" :value node-kind :on-change (typed :node-kind)}
       (for [[v k] node-kind-label] ^{:key v} [:option {:value v} (i18n/t k)])]
      [ui/html-select {:class ["u-flex-1" "u-mb-8"] :value node-key
                       :on-change (typed :node-key)}
       [:option {:value ""} (i18n/t :ui/ops-pick-node)]
       (for [[v label] (node-options node-kind)] ^{:key v} [:option {:value v} label])]]
     [:label {:class style/label} (i18n/t :ui/ops-expectation)]
     [:div {:class "u-flex-gap-8"}
      [ui/html-select {:class "u-mb-8" :value kind :on-change (typed :kind)}
       [:option {:value "row-count"} (i18n/t :ui/ops-kind-row-count)]
       [:option {:value "custom"} (i18n/t :ui/ops-kind-custom)]]
      [ui/html-select {:class "u-mb-8" :value op :on-change (typed :op)}
       (for [o [">=" ">" "=" "<=" "<"]] ^{:key o} [:option {:value o} o])
       [:option {:value "nonzero"} (i18n/t :ui/ops-op-nonzero)]]
      [ui/input-group {:class "u-mb-8 ops-value" :value value
                       :disabled (= op "nonzero")
                       :on-change (typed :value)}]]
     (when (= kind "custom")
       [ui/text-area {:fill true :class ["bp-monospace-text" "u-mb-8"] :value query
                      :placeholder (i18n/t :ui/ops-custom-query-hint)
                      :on-change (typed :query)}])
     [error-line err]
     [ui/button
      {:intent   :primary
       :disabled (or busy (str/blank? nm) (str/blank? node-key))
       :on-click #(send! [:ops.health-form/submit])}
      (i18n/t (if busy :ui/ops-saving :ui/create))]]))

(defn- node-link [node-kind node-key]
  (case (some-> node-kind name)
    "object-type" [rl/resource-link :object-type node-key]
    "transform"   [rl/resource-link :transform node-key]
    "dataset"     [rl/resource-link :dataset node-key]
    [:span (str node-key)]))

(defn- reload-checks! [] (send! [:ops.checks/reload]))

(defn- health-row [{:keys [id node-kind node-key kind expects latest] nm :name :as c}]
  (let [{:keys [busy]} (get-in @ui [:check-rows id])]
    [ui/card {:class       ["ops-row" (when (selected? id) "ops-row--on")]
              :interactive true
              :compact     true
              :on-click    #(send! [:ops/select id])}
     [:div {:class "u-flex-between"}
      [:div {:class "u-flex-wrap-gap-6"}
       [:b nm]
       [:span {:class "u-fs-12 u-flex-wrap-gap-6"}
        [:span {:class "u-muted"}
         (i18n/t (get node-kind-label (some-> node-kind name) :ui/ops-checks))]
        [node-link node-kind node-key]
        [:span {:class "u-muted"}
         (str "· "
              (i18n/t (if (= "custom" (some-> kind name)) :ui/ops-kind-custom
                          :ui/ops-kind-row-count))
              (when expects
                (str " " (if (= "nonzero" (name-or (:op expects)))
                           (i18n/t :ui/ops-op-nonzero)
                           (str (name-or (:op expects)) " " (:value expects))))))]]]
      (when latest
        [:span [status-text (:status latest)]
         (when (:value latest) (str " (" (:value latest) ")"))])]
     [:div {:class "u-flex-wrap-gap-6 u-mt-8"}
      [ui/button {:intent :primary :disabled busy :on-click #(send! [:ops.check/run id])}
       (i18n/t :ui/run)]
      [ui/button {:outlined true :intent :danger :on-click #(send! [:ops.check/delete id nm])}
       (i18n/t :ui/delete)]]
     [health-watch/controls c reload-checks!]]))

(defn- check-detail [{:keys [id] nm :name}]
  [:div {:class "ops-detail"}
   [page/header {:title nm}
    [ui/button {:minimal true :icon :cross :aria-label (i18n/t :ui/ops-close)
                :on-click #(send! [:ops/deselect])}]]
   [:div {:class "u-section-label"} (i18n/t :ui/ops-results)]
   [fires (get-in @ui [:check-rows id :results]) :ui/ops-results-none]])

(def ^:private health-pane
  (machine/entering
   ops [:ops.checks/enter]
   (fn []
     (let [{:keys [checks run-all-busy] form? :health-form} @ui
           checks (or checks rd/loading)]
       [:<>
        [head-row (i18n/t :ui/ops-health-hint)
         [ui/button {:disabled run-all-busy :on-click #(send! [:ops.checks/run-all])}
          (i18n/t (if run-all-busy :ui/running :ui/ops-run-all))]
         [ui/button {:on-click #(send! [:ops.health-form/toggle])}
          (i18n/t (if form? :ui/ops-close :ui/ops-check-new))]]
        (when form? [health-form])
        [listed checks :ui/ops-checks-none (fn [c] ^{:key (:id c)} [health-row c])]
        [health-watch/groups-panel (rd/value-or checks [])]]))))

(machine/reg-sub ::tab (fn [db _] (:top-tab db :automate)))

(defn- tab [] @(machine/subscribe [::tab] {:frame ops}))

(defn- contributed [t] (some #(when (= t (:id %)) %) (feature/contributions :operations-tab)))

(defn- list-view
  []
  (let [t       (tab)
        current (contributed t)]
    [:<>
     [page/header {:title (i18n/t :ui/app-operations)}
      [ui/tabs {:id :ops-top :selected-tab-id t :on-change #(send! [:ops/tab %])}
       [ui/tab {:id :automate :title (i18n/t :ui/ops-tab-automate)}]
       (for [{:keys [id label-key]} (feature/contributions :operations-tab)]
         [ui/tab {:id id :title (i18n/t label-key)}])
       [ui/tab {:id :health :title (i18n/t :ui/ops-tab-health)}]]]
     [:div {:class (if (:bleed? current) "ops-body ops-body--bleed" "ops-body")}
      (cond
        current        [(:view current)]
        (= t :health)  [health-pane]
        :else          [automations-pane])]]))

(defn- detail-view []
  (let [db  @ui
        sel (fn [xs] (some #(when (= (str (:id %)) (str (:focus db))) %) (rd/value-or xs [])))]
    (case (tab)
      :automate (some-> (sel (:automations db)) automation-detail)
      :health   (some-> (sel (:checks db)) check-detail)
      nil)))

(def ^:private detail
  (reify
    IDeref
    (-deref [_] (when (#{:automate :health} (:top-tab @ui :automate))
                  (some-> (:focus @ui) vector)))
    IReset
    (-reset! [_ _] (send! [:ops/deselect]) nil)))

(page/view! :ops/list list-view)
(page/view! :ops/detail detail-view)

(def ^:private tree
  (page/liste-detail {:list :ops/list :detail :ops/detail :open detail
                      :detail-w 460 :resize :ops/detail}))

(defn operations-app [] [platform/module :operations tree])
