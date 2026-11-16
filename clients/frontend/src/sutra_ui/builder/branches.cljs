(ns sutra-ui.builder.branches
  (:require [sutra-ui.ui.core :as ui]
            [clojure.string :as str]
            [sutra-ui.builder.model :as model]
            [sutra-ui.state.portal :as pstate]
            [sutra-ui.state.branches :as branches]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.state.staging :as staging]
            [sutra-ui.state.sync :as sync]
            [sutra-ui.resource.core :as res]
            [sutra-ui.feature :as feature]
            [sutra.i18n.core :as i18n]))

(defn branch-label [b]
  (str (:name b)
       (when (:parent b) (str " ⤴ " (:parent b)))))

(defn branch-select
  [class title]
  (let [bs        (branches/branch-list)
        cur       (:current-branch @shell/ui-state)
        in-scope? (boolean (and cur (some #(= (:name %) cur) bs)))
        new-opt   "__new__"
        {inactive true active false} (group-by #(= "inactive" (:status %)) (sort-by :name bs))
        option    (fn [b]
                    ^{:key (:name b)}
                    [:option {:value (:name b) :title (branch-label b)} (branch-label b)])]
    [ui/html-select
     {:value (if in-scope? cur "")
      :title title
      :aria-label (i18n/t :ui/branch-label)
      :on-change (fn [e]
                   (let [v (.. e -target -value)]
                     (cond
                       (= v new-opt) (model/send! [:builder.branch/new cur])
                       (seq v)       (sync/checkout-branch! v))))
      :class class}
     (when-not in-scope?
       [:option {:value "" :disabled true} (i18n/t :ui/branch-pick)])
     (map option active)
     (when (seq inactive)
       [:optgroup {:label (i18n/t :ui/branch-inactive-group {:n (count inactive)})}
        (map option inactive)])
     (when in-scope?
       [:option {:value new-opt} (str "+ " (i18n/t :ui/branch-new-title))])]))

(defn planned-button [class]
  (let [n (count (staging/staged-list))]
    (when (pos? n)
      [ui/tooltip {:content (i18n/t :ui/planned-open)}
       [ui/button {:on-click (fn [_]
                               (staging/refresh-staged!)
                               (shell/open-staging!))
                   :minimal  true
                   :class    class}
        (i18n/t :ui/planned-n {:n n})]])))

(defn protection-chip
  [{:keys [protected land-pushes]}]
  [ui/tooltip {:content (i18n/t (cond protected   :ui/branch-protected-hint
                                      land-pushes :ui/branch-open-lands-hint
                                      :else       :ui/branch-open-hint))}
   [ui/tag {:intent  (when protected :warning)
            :minimal true}
    (i18n/t (if protected :ui/branch-protected :ui/branch-open))]])

(defn branch-bar
  []
  (let [cur (:current-branch @shell/ui-state)
        bs  (branches/branch-list)
        b   (some #(when (= (:name %) cur) %) bs)]
    (when (or (nil? cur) (= "main" cur))
      [:div {:class "shell-bar__context"}
       [branch-select "shell-bar__branch-select"
        (if b (branch-label b) (i18n/t :ui/branch-pick-hint))]
       (when b [protection-chip b])
       [planned-button "shell-bar__planned-btn"]])))

(def change-glyph {"added" "+" "modified" "~" "removed" "−"})

(defn open-changed!
  [{:keys [section name]}]
  (let [k (keyword name)]
    (case section
      "datasets" (feature/command! :open-in-lineage [:dataset k])
      (res/open! (keyword section) k))))

(defn- taskbar! [m] (model/send! [:builder.taskbar/set m]))

(defn merge-choice
  [id ui]
  [:<>
   [ui/html-select
    {:class "branch-taskbar__input" :value (:build ui "none")
     :aria-label (i18n/t :ui/branch-merge-build)
     :on-change #(taskbar! {:build (.. % -target -value)})
     :options [{:value "none" :label (i18n/t :ui/branch-build-none)}
               {:value "modified" :label (i18n/t :ui/branch-build-modified)}
               {:value "affected" :label (i18n/t :ui/branch-build-affected)}]}]
   [ui/button {:class "branch-taskbar__btn" :small true
               :on-click #(taskbar! {:merging? false})}
    (i18n/t :ui/cancel)]
   [ui/button {:class "branch-taskbar__btn" :small true :intent :primary
               :on-click #(model/send! [:builder.merge/go id])}
    (i18n/t :ui/branch-proposal-merge)]])

(defn branch-resources [tasks]
  [ui/tooltip {:content (i18n/t :ui/branch-changes-hint)}
   [:div {:class "branch-taskbar__panel"}
    (for [[section rows] (sort-by key (group-by :section tasks))]
      ^{:key section}
      [:div {:class "branch-taskbar__group"}
       [:div {:class "branch-taskbar__group-head"}
        (str (res/family-label (keyword section)) " · " (count rows))]
       (for [{:keys [task name status protected] :as t} (sort-by :name rows)]
         ^{:key task}
         [:span {:class "branch-taskbar__row"}
          [ui/button {:class      (str "branch-taskbar__item branch-taskbar__item--" status)
                      :minimal    true
                      :small      true
                      :align-text :start
                      :disabled   (= "removed" status)
                      :icon       [:span {:class "branch-taskbar__glyph"}
                                   (change-glyph status "•")]
                      :right-icon (when protected
                                    [ui/tooltip
                                     {:content (i18n/t :ui/branch-resource-protected-hint)}
                                     [:span {:class "branch-taskbar__lock"} "🔒"]])
                      :on-click   #(open-changed! t)}
           name]
          [ui/tooltip {:content (i18n/t :ui/branch-remove-resource)}
           [ui/button {:class      "branch-taskbar__remove"
                       :minimal    true
                       :small      true
                       :icon       :cross
                       :aria-label (i18n/t :ui/branch-remove-resource)
                       :on-click   #(model/send! [:builder.branch/remove
                                                  (:current-branch @shell/ui-state)
                                                  task name])}]]])])]])

(defn policy-of-branch [nm]
  (some #(when (= nm (:name %)) %) (branches/branch-list)))

(defn merge-block
  [{:keys [stage preview checks]} require-checks?]
  (cond
    (= "unjudgeable" stage)
    {:stage :ui/branch-stage-unjudgeable :hint :ui/branch-unjudgeable-hint}

    (or (not= "merge" stage) (seq (:conflicts preview)))
    {:hint :ui/branch-proposal-merge-hint}

    (and require-checks? checks (not= "passed" checks))
    {:stage :ui/branch-stage-checks :hint :ui/branch-checks-hint}

    (and (= "empty" (:status preview)) (nil? checks))
    {:stage :ui/branch-stage-empty :hint :ui/branch-empty-hint}))

(defn changes-label
  [{:keys [target tasks proposal]}]
  (let [n (count tasks)]
    (cond
      (= 1 n)                            (i18n/t :ui/branch-changes-one {:target target})
      (pos? n)                           (i18n/t :ui/branch-changes-n {:n n :target target})
      (= "unjudgeable" (:stage proposal)) (i18n/t :ui/branch-changes-unknown {:target target})
      (:checks proposal)                 (i18n/t :ui/branch-changes-code {:target target})
      :else                              (i18n/t :ui/branch-changes-none {:target target}))))

(defn branch-proposal-controls [cur {:keys [target tasks proposal]} ui]
  (let [{:keys [id stage preview judgement-error]} proposal
        conflicts (count (:conflicts preview))
        policy    (policy-of-branch target)
        block     (merge-block proposal (:require-checks policy))
        why       (or judgement-error (:error preview))
        view!     #(feature/command! :open-proposal id)]
    (cond
      id
      [:<>
       [:span {:class "branch-taskbar__proposal"}
        (str (:title proposal) " · "
             (i18n/t (or (:stage block) (keyword "ui" (str "branch-stage-" stage)))))
        (when (pos? conflicts)
          [ui/button {:class "branch-taskbar__conflicts" :minimal true :small true
                      :intent :danger :on-click view!}
           (i18n/t :ui/branch-conflicts {:n conflicts :target target})])]
       [ui/button {:class "branch-taskbar__btn" :small true :on-click view!}
        (i18n/t :ui/branch-proposal-view)]
       (if (:merging? ui)
         [merge-choice id ui]
         [ui/tooltip {:content (when block
                                 (str (i18n/t (:hint block)) (when why (str " (" why ")"))))}
          [ui/button {:class    "branch-taskbar__btn"
                      :small    true
                      :intent   :primary
                      :icon     :git-merge
                      :disabled (some? block)
                      :on-click #(taskbar! {:merging? true})}
           (i18n/t :ui/branch-proposal-merge)]])]

      (:naming? ui)
      (let [title (str/trim (str (:title ui)))]
        [:<>
         [ui/input-group {:class       "branch-taskbar__input"
                          :small       true
                          :auto-focus  true
                          :value       (:title ui)
                          :placeholder (i18n/t :ui/branch-proposal-title)
                          :on-change   #(taskbar! {:title (.. % -target -value)})}]

         [ui/button {:class "branch-taskbar__btn" :small true
                     :on-click #(taskbar! {:naming? false})}
          (i18n/t :ui/cancel)]
         [ui/button {:class    "branch-taskbar__btn"
                     :small    true
                     :intent   :primary
                     :disabled (str/blank? title)
                     :on-click #(model/send! [:builder.propose/go cur target title id])}
          (i18n/t :ui/proposal-create)]])

      :else
      (let [lands? (and (:auto-land policy) (seq tasks))]
        [ui/tooltip
         {:content (cond lands?      (i18n/t :ui/branch-auto-land-hint {:target target})
                         (empty? tasks) (i18n/t :ui/branch-proposal-empty-hint))}
         [ui/button {:class    "branch-taskbar__btn"
                     :small    true
                     :on-click #(taskbar! {:naming? true :title cur})}
          (i18n/t (if lands? :ui/branch-auto-land :ui/proposal-create))]]))))

(defn branch-taskbar
  []
  (let [ui    (:taskbar @model/ui)
        cur   (:current-branch @shell/ui-state)
        open? (:branch-panel? @shell/ui-state)
        bc    (branches/branch-changes-view)
        {:keys [target tasks]} (when (= :changes (:show bc)) bc)
        n     (count tasks)
        supported? (pstate/reads-branch?)]
    (when (and cur (not= cur "main"))
      [:div {:class "branch-taskbar-wrap"}
       (when (and open? (pos? n)) [branch-resources tasks])
       [:div {:class (cond-> "branch-taskbar"
                       (not supported?) (str " branch-taskbar--unsupported"))}
        [branch-select "branch-taskbar__select" (i18n/t :ui/branch-taskbar {:name cur})]
        (when-not supported?
          [:span {:class "branch-taskbar__note"}
           (i18n/t :ui/branch-taskbar-unsupported)])
        (case (:show bc)
          :loading [:span {:class "branch-taskbar__note"}
                    (i18n/t :ui/branch-changes-loading)]
          :failed  [:span {:class "branch-taskbar__note"}
                    (i18n/t :ui/branch-changes-failed {:reason (:reason bc)})]
          nil)
        (when target
          [ui/button {:class      "branch-taskbar__changes"
                      :minimal    true
                      :small      true
                      :disabled   (zero? n)
                      :right-icon (when (pos? n) (if open? :caret-down :caret-up))
                      :on-click   #(shell/toggle-branch-panel!)}
           (changes-label bc)])
        [:span {:class "branch-taskbar__spacer"}]
        [planned-button "branch-taskbar__btn"]
        (when target [branch-proposal-controls cur bc ui])
        [ui/button {:class "branch-taskbar__btn" :small true :intent :danger
                    :minimal true :on-click #(model/send! [:builder.branch/drop cur])}
         (i18n/t :ui/drop)]
        [ui/button {:on-click #(sync/checkout-branch! "main") :small true
                    :class "branch-taskbar__main"}
         (i18n/t :ui/branch-taskbar-main)]]])))
