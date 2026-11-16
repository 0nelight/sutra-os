(ns sutra-ui.apps.repository.proposals
  (:require [sutra-ui.ui.core :as ui :refer [link tag]]
            [sutra.kit.remote :as rd]
            [clojure.string :as str]
            [sutra.i18n.core :as i18n]
            [sutra-ui.feature :as feature]
            [sutra-ui.state.auth :as auth]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.principals :as principals]
            [sutra-ui.state.governance :as gov]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.apps.repository.model :as repo-model]
            [sutra-ui.resource.core :as res]
            [sutra-ui.ui.busy :as busy]
            [sutra-ui.ui.page :as page]
            [sutra-ui.ui.table :as table]
            [sutra-ui.apps.repository.ui :refer [action-btn empty-state]]))

(def ^:private send! repo-model/send!)

(defn open! [id] (send! [:repository.proposal/open id]))

(defn- review [pid] (get (repo-model/at :review) pid))

(def ^:private stage-label
  {"preparing" "Preparing" "in-review" "In Review" "merge" "Merge proposal"
   "unjudgeable" "Cannot be judged"})

(defn- stage-badge [stage]
  (let [s (str stage)]
    [tag {:minimal true :intent (case s "in-review" :warning "merge" :success nil)}
     (get stage-label s "—")]))

(defn- list-stage-badge
  [stage]
  [ui/tooltip
   {:content
    "Whether review has been asked for. Full approval is established on the proposal's own page."}
   [:span
    {}
    [stage-badge (if (= "merge" (str stage)) "in-review" stage)]]])

(defn- status-intent [status]
  (case (some-> status name)
    ("passed" "merged") :success
    ("failed" "rejected") :danger
    "running" :warning
    "open" :primary
    nil))

(defn- status-pill [status]
  [tag {:minimal true :intent (status-intent status)
        :class (str "prop-pill--" (name (or status "unknown")))}
   (name (or status "unknown"))])

(defn- who
  [principals]
  (when (seq principals)
    (str/join ", " (map principals/principal-label principals))))

(defn- me
  []
  (str (:user @auth/auth)))

(defn- checks-panel
  [checks]
  [ui/card {:class "prop-panel"}
   [:div {:class "prop-panel__title"} "Checks"]
   (if-not checks
     [:div {:class "prop-panel__muted"}
      "No repository commits on this branch — nothing to check. (A change that is
       only ontology carries no repository checks; the merge gate treats that the
       same way.)"]
     [:div
      [:div {:class "prop-checks__head"}
       [status-pill (:status checks)]
       [:span {:class "prop-mono"}
        (subs (str (:commit checks)) 0 (min 12 (count (str (:commit checks)))))]]
      (if (empty? (:checks checks))
        [:div {:class "prop-panel__muted"}
         "No check has run on this commit yet."]
        (into [:ul {:class "prop-checks__list"}]
              (for [c (:checks checks)]
                ^{:key (str (:check c))}
                [:li
                 [status-pill (:status c)]
                 [:span {:class "prop-mono"} (str (:check c))]
                 (when (:detail c)
                   [:span {:class "prop-panel__muted"} (str " — " (:detail c))])])))])])

(defn- merge-history-panel
  [attempts]
  (when (seq attempts)
    [ui/card {:class "prop-panel"}
     [:div {:class "prop-panel__title"} (i18n/t :ui/proposal-merge-history)]
     (into [:ul {:class "prop-checks__list"}]
           (for [{:keys [seq at actor outcome error]} attempts]
             ^{:key seq}
             [:li
              [status-pill outcome]
              [:span {:class "prop-mono"} (str " " at " · " (principals/principal-label actor))]
              (when error [:span {:class "prop-panel__muted"} (str " — " error)])]))]))

(defn- side-text [{:keys [value removed absent secret]}]
  (cond removed (i18n/t :ui/conflict-removed)
        absent  (i18n/t :ui/conflict-absent)
        secret  (i18n/t :ui/conflict-secret)
        :else   (str value)))

(defn- open-conflicting! [{:keys [section name]}]
  (when name
    (case section
      "action-types" (catalog/open-resource! :actions (keyword name))
      "datasets"     (feature/command! :open-in-lineage [:dataset (keyword name)])
      (res/open! (keyword section) (keyword name)))))

(defn- conflict-card [pid c target choice]
  (let [{:keys [table section name fields]} c
        open? (contains? (:open-conflicts (review pid)) table)
        choose! #(send! [:repository.conflict/choose pid table %])]
    [:div {:class "prop-conflict"}
     [ui/button {:minimal true :fill true :align-text :start :class "prop-conflict__head"
                 :icon (if open? :chevron-down :chevron-right)
                 :on-click #(send! [:repository.conflict/toggle pid table])}
      [:span {:class "prop-conflict__kind"} (res/family-label (keyword section))]
      [:span {:class "prop-conflict__name"} (or name table)]
      [:span
       {:class (str "prop-conflict__state" (when choice " prop-conflict__state--decided"))}
       (case choice
         "mine"   (i18n/t :ui/conflict-keeps-branch)
         "theirs" (i18n/t :ui/conflict-keeps-target {:target target})
         (i18n/t :ui/conflict-undecided))]]
     [ui/collapse {:is-open open?}
      [:div {:class "prop-conflict__body"}
       [table/data-table
        {:class   "prop-conflict__table"
         :rows    fields
         :row-key :field
         :cols    [{:label  (i18n/t :ui/conflict-field) :mono? true
                    :render #(or (:field %) (i18n/t :ui/conflict-whole))}
                   {:label  (i18n/t :ui/conflict-base)
                    :render #(vector :code (side-text (:base %)))}
                   {:label target :render #(vector :code (side-text (:theirs %)))}
                   {:label  (i18n/t :ui/conflict-this-branch)
                    :render #(vector :code (side-text (:mine %)))}]}]
       [:div {:class "prop-conflict__actions"}
        [ui/button
         {:small true :active (= "theirs" choice)
          :on-click #(choose! "theirs")}
         (i18n/t :ui/conflict-take-target {:target target})]
        [ui/button
         {:small true :active (= "mine" choice)
          :on-click #(choose! "mine")}
         (i18n/t :ui/conflict-keep-branch)]
        (when name
          [ui/button {:small true :minimal true :on-click #(open-conflicting! c)}
           (i18n/t :ui/conflict-open)])]]]]))

(defn- preview-panel
  [p]
  (let [choices   (:choices (review (:id p)))
        preview   (:preview p)
        status        (str (:status preview))
        conflicts (:conflicts preview)
        target    (str (:target-branch p))
        source    (str (:source-branch p))
        decided   (count (filter #(get choices (:table %)) conflicts))
        all?      (= decided (count conflicts))]
    [ui/card {:class "prop-panel"}
     [:div {:class "prop-panel__title"} "Merge preview"]
     (case status
       "conflicts"
       [:div
        [:div {:class "prop-conflict__title"}
         (i18n/t :ui/conflicts-n {:n (count conflicts) :target target})]
        [:div {:class "prop-panel__muted"} (i18n/t :ui/conflicts-hint {:target target})]
        (for [c conflicts]
          ^{:key (str (:table c))}
          [conflict-card (:id p) c target (get choices (:table c))])
        [:div {:class "prop-conflict__footer"}
         [:span {:class "prop-panel__muted"}
          (i18n/t :ui/conflicts-decided {:n decided :of (count conflicts)})]
         [ui/button {:intent   :primary
                     :disabled (not all?)
                     :on-click #(send! [:repository.conflict/rebase (:id p) source])}
          (i18n/t :ui/conflicts-rebase {:target target})]]]
       "clean" [:div {:class "prop-panel__ok"} "✓ Merges cleanly."]

       "no-base"
       [:div {:class "prop-panel__muted"}
        (str "This branch has no recorded fork base, so what it would change cannot be "
             "computed — which is not the same as nothing to change. Re-fork it from "
             target " and re-apply the change.")]

       "gone"
       [:div {:class "prop-panel__muted"}
        (str "The source branch " source " no longer exists, so there is nothing left "
             "to merge from and nothing to compare against. Close this proposal.")]

       "base-unreadable"
       [:div {:class "prop-panel__muted"}
        (str "This branch has a fork base but it could not be read, so what it would "
             "change is unknown — which is not nothing: " (:error preview))]

       "unreadable"
       [:div {:class "prop-panel__muted"}
        (str "The merge preview could not be computed, so what this would change is "
             "unknown: " (:error preview))]

       [:div {:class "prop-panel__muted"}
        "Nothing ontological to merge — this change's content is data, not schema."])]))

(defn- comment-list [pid comments]
  (when (seq comments)
    (into [:div {:class "prop-thread"}]
          (for [c comments]
            ^{:key (str (:id c))}
            [:div {:class "prop-comment"}
             [:div {:class "prop-comment__meta"}
              [:span {:class "prop-comment__author"} (principals/principal-label (:author c))]
              [:span (str (:created-at c))]
              (when (= (str (:author c)) (me))
                [ui/tooltip {:content "Delete this comment"}
                 [ui/button {:minimal true :small true :icon :cross
                             :on-click #(send! [:repository.comment/delete pid (:id c)])
                             :aria-label "Delete this comment"}]])]
             [:div {:class "prop-comment__body"} (str (:body c))]]))))

(defn- comment-box
  [pid task]
  (let [draft (get-in (review pid) [:drafts task] "")]
    [:div {:class "prop-comment-box"}
     [ui/text-area
      {:value draft
       :rows 2
       :placeholder (if task "Comment on this resource…" "Comment on this change…")
       :on-change #(send! [:repository.comment/draft pid task (.. % -target -value)])
       :class "prop-comment-box__input"}]
     [action-btn {:label "Comment"
                  :disabled? (str/blank? draft)
                  :on-click #(send! [:repository.comment/add pid task])}]]))

(defn- requirement
  [{:keys [by count]}]
  (let [by (set (map keyword by))]
    (str (if (< 1 (or count 1)) (str count " ") "")
         (cond (by :owner) "an owner of this resource"
               (by :other) "someone other than the author"
               :else       (str/join ", " (map name by))))))

(defn- verdict-summary
  [t open?]
  [:div {:class "prop-task__verdicts"}
   (cond
     (seq (:rejections t))
     [:span {:class "prop-verdict prop-verdict--rejected"}
      (str "✗ rejected by " (who (:rejections t)))]

     (and (not open?) (empty? (:requires t)))
     [:span {:class "prop-verdict prop-verdict--approved"}
      (if (= "accretion" (some-> (:kind t) name))
        "✓ an addition — waits for no one"
        "✓ the author owns it — no review needed")]

     (not open?)
     [:span {:class "prop-verdict prop-verdict--approved"}
      (str "✓ approved by " (who (:approvals t)))]

     :else
     [:span {:class "prop-verdict prop-verdict--open"}
      (str "awaiting " (str/join " and " (map requirement (:requires t)))
           (when (seq (:approvals t)) (str " — so far " (who (:approvals t)))))])
   (when (seq (:consumers t))
     [:span {:class "prop-verdict prop-verdict--stale"}
      (str "breaks a resource used by " (str/join ", " (map name (:consumers t))))])
   (when (seq (:stale t))
     [:span {:class "prop-verdict prop-verdict--stale"}
      (str "⟳ " (who (:stale t)) " approved an earlier version — changed since")])])

(defn- task-resource
  [t]
  (let [[section k] (str/split (str (:task t)) #"/" 2)
        family      (some-> section keyword)
        nm          (or (some-> k catalog/name-of name keyword) (some-> (:name t) keyword))]
    (when (and family nm (res/spec-of family nm))
      [family nm])))

(defn- task-changes
  [changes]
  (when (seq changes)
    [table/data-table
     {:class   "prop-diff"
      :rows    changes
      :row-key :field
      :cols    [{:label (i18n/t :ui/repo-task-field) :cell-class "prop-diff__field"
                 :render :field}
                {:label  (i18n/t :ui/repo-task-before) :cell-class "prop-diff__before"
                 :render #(or (:before %) "—")}
                {:label  (i18n/t :ui/repo-task-after) :cell-class "prop-diff__after"
                 :render #(or (:after %) "—")}]}]))

(defn- task-row [pid waiting? open? toggle! t]
  [:div {:class "prop-task"}
   [:div {:class "prop-task__head"}
    [tag {:minimal true
          :intent  (case (:status t) "added" :success "modified" :warning
                         "removed" :danger nil)}
     (str (:status t))]
    (if-let [[family nm] (task-resource t)]
      [:span {:class "prop-task__name" :title (str (:task t))}
       [res/resource-link family nm]]
      [:span {:class "prop-task__name" :title (str (:task t))}
       (or (some-> (:name t) str) (str (:task t)))])
    [ui/tooltip {:content (i18n/t :ui/repo-task-discuss-hint)}
     [ui/button {:minimal true :small true :icon :comment :on-click toggle!}
      (i18n/t :ui/repo-task-discuss)]]
    (when (seq (:comments t))
      [:span {:class "repo-helper__tab-badge"} (count (:comments t))])
    [action-btn {:label "Approve"
                 :title "Approve this one resource"
                 :on-click #(send! [:repository.review/approve pid (:task t) nil])}]
    [action-btn {:label "Reject"
                 :title "Object to this resource — blocks the merge, leaves the proposal open"
                 :on-click #(send! [:repository.review/reject pid (:task t) nil])}]]
   [task-changes (:changes t)]
   [verdict-summary t waiting?]
   (when open?
     [:div {:class "prop-task__thread"}
      [comment-list pid (:comments t)]
      [comment-box pid (:task t)]])])

(defn- tasks-panel [p]
  (let [expanded (:expanded (review (:id p)))
        tasks   (:tasks p)
        waiting (set (map str (:open-tasks p)))]
    [ui/card {:class "prop-panel"}
     [:div {:class "prop-panel__title"}
      (str "Review changes · " (count tasks) " task"
           (when (not= 1 (count tasks)) "s"))
      (when (seq waiting)
        [:span {:class "prop-panel__muted"}
         (str "  " (count waiting) " waiting for review")])]
     (if (empty? tasks)
       [:div {:class "prop-panel__muted"}
        "No modified ontology resources. A proposal with no tasks still merges —
         its content is data or repository code, which is reviewed by its checks."]
       (into [:div {:class "prop-tasks"}]
             (for [t tasks]
               ^{:key (str (:task t))}
               [task-row (:id p) (contains? waiting (str (:task t)))
                (contains? expanded (str (:task t)))
                #(send! [:repository.review/expand (:id p) (str (:task t))])
                t])))]))

(defn- ask-review-bar
  [p]
  [:div {:class "prop-ask"}
   [:div {:class "prop-panel__muted"}
    "This change is saved but nobody has been asked to look at it yet."]
   [ui/input-group {:value (:reviewers (review (:id p)) "")
                    :placeholder "reviewers (comma-separated, optional)"
                    :class "prop-ask__reviewers"
                    :on-change #(send! [:repository.review/reviewers (:id p)
                                        (.. % -target -value)])}]
   [action-btn {:label "Ask for review" :primary? true
                :on-click #(send! [:repository.review/ask (:id p)])}]])

(defn- proposal-page [p]
  (let [stage  (str (:stage p))
        open?  (= "open" (str (:status p)))]
    [:div {:class "prop-page"}
     [page/header {:title (str (:title p))}
      [stage-badge stage]
      [status-pill (:status p)]
      [action-btn
       {:label (i18n/t :ui/repo-all-pull-requests)
        :on-click #(send! [:repository.proposal/close])}]]

     [:div {:class "prop-page__meta"}
      [:span {:class "prop-mono"} (str (:source-branch p) " → " (:target-branch p))]
      [:span (str "opened by " (principals/principal-label (:author p)))]
      (when (seq (:reviewers p))
        [:span (str "asked: " (who (:reviewers p)))])
      (when (seq (:rejections p))
        [:span {:class "prop-verdict prop-verdict--rejected"}
         (str "objections from " (who (:rejections p)))])]

     (when (seq (str (:description p)))
       [:div {:class "prop-page__desc"} (str (:description p))])

     (when (and open? (= "preparing" stage))
       [ask-review-bar p])

     [page/toolbar
      [action-btn {:label "Merge proposal"
                   :primary? (= "merge" stage)
                   :disabled? (not open?)
                   :title (if (= "merge" stage)
                            "Every task is approved — land the change"
                            "The server decides: if something still blocks this, it will say what")
                   :on-click #(send! [:repository.proposal/merge (:id p)])}]
      [action-btn {:label "Approve all tasks"
                   :disabled? (not open?)
                   :title "every task this proposal currently has"
                   :on-click #(send! [:repository.review/approve (:id p) nil nil])}]
      [action-btn {:label "Withdraw"
                   :disabled? (not open?)
                   :title "Close without merging — the branch and its work stay"
                   :on-click #(send! [:repository.proposal/withdraw (:id p)])}]]

     [:div {:class "prop-page__panels"}
      [checks-panel (:checks p)]
      [merge-history-panel (:merge-history p)]
      ^{:key (str (:id p))} [preview-panel p]]

     [tasks-panel p]

     [ui/card {:class "prop-panel"}
      [:div {:class "prop-panel__title"} "Discussion"]
      (let [orphan? (let [ids (into #{} (map #(str (:task %))) (:tasks p))]
                      (fn [c] (and (:task c) (not (contains? ids (str (:task c)))))))
            general (remove :task (:comments p))
            dropped (filter orphan? (:comments p))]
        [:div
         [comment-list (:id p) general]
         (when (seq dropped)
           [:div
            [:div {:class "prop-panel__muted"}
             "About resources no longer in this change:"]
            [comment-list (:id p) dropped]])
         [comment-box (:id p) nil]])]]))

(defn- proposal-row [p]
  [:div {:class "repo-branches__row"}
   [link {:on-click #(send! [:repository.proposal/show (:id p)]) :underline :hover}
    (str (:title p))]
   [:span {:class "prop-mono prop-list__branches"}
    (str (:source-branch p) " → " (:target-branch p))]
   [list-stage-badge (:stage p)]
   [status-pill (:status p)]
   (when-let [c (:checks p)]
     [ui/tooltip {:content (str "checks on " (:commit c))}
      [:span [status-pill (:status c)]]])
   [:span {:class "prop-panel__muted"}
    (str (or (:approval-count p) 0) " approvals"
         (when (seq (:open-tasks p)) (str " · " (count (:open-tasks p)) " open")))]])

(defn- proposals-status
  [{:keys [show reason]}]
  (case show
    :loading [busy/block {:class "prop-panel__muted"} (i18n/t :ui/proposals-loading)]
    :failed  [ui/problem (i18n/t :ui/proposals-failed {:reason reason}) nil]
    nil))
(def proposals-view
  (machine/entering
   repo-model/repository [:repository.proposals/enter]
   (fn []
     (let [open @gov/open-proposal
           seen (rd/list-view @gov/proposals)
           ps   (:rows seen)]
       (if open
         (let [failed (fn [reason]
                        [:div {:class "prop-page"}
                         [action-btn
                          {:label (i18n/t :ui/repo-all-pull-requests)
                           :on-click #(send! [:repository.proposal/close])}]
                         [:div {:class "repo-problems__error"} (str reason)]])]
           (rd/fold (:proposal open)
                    {:loading (fn []
                                [:div {:class "prop-page"}
                                 [busy/block {:class "prop-panel__muted"} "Loading proposal…"]])
                     :absent  #(failed nil)
                     :failed  failed
                     :value   (fn [p] [proposal-page p])}))
         [:div {:class "repo-branches"}
          [page/toolbar
           [:span {:class "u-muted"} (i18n/t :ui/repo-pull-requests-count {:n (count ps)})]
           [action-btn {:label "Open" :on-click #(send! [:repository.proposals/refresh "open"])}]
           [action-btn {:label "All"  :on-click #(send! [:repository.proposals/refresh "all"])}]]
          (cond
            (#{:loading :failed} (:show seen))
            [proposals-status seen]

            (empty? ps)
            [:div
             [empty-state :git-pull (i18n/t :ui/repo-no-pull-requests)
              (i18n/t :ui/repo-no-pull-requests-hint)]
             [:div {:class "prop-empty__cta"}
              [action-btn {:label (i18n/t :ui/review-changes) :primary? true
                           :on-click shell/open-staging!}]]]
            :else
            (into [:div] (for [p ps] ^{:key (str (:id p))} [proposal-row p])))])))))

(def checks-view
  (machine/entering
   repo-model/repository [:repository.proposals/enter]
   (fn []
     (let [seen (rd/list-view @gov/proposals)
           ps   (:rows seen)]
       [:div {:class "repo-branches"}
        [page/toolbar
         [:span {:class "u-muted"} (str "Checks · " (count ps) " in flight")]
         [ui/button {:small true :icon :refresh
                     :on-click #(send! [:repository.proposals/refresh "open"])}
          "Refresh"]]
        [:div {:class "u-muted"}
         "Schema validity is enforced at commit time by the kernel; these are the
          checks the PUSH runs — scope, retire, lint, plan and ci/sutra-publish."]
        (cond
          (#{:loading :failed} (:show seen))
          [proposals-status seen]

          (empty? ps)
          [empty-state :tick-circle "Nothing in flight"
           "No open proposal, so no commit is waiting on a verdict."]
          :else
          (into [:div]
                (for [p ps]
                  ^{:key (str (:id p))}
                  [ui/card {:class "prop-panel"}
                   [:div {:class "prop-checks__head"}
                    [link {:on-click #(open! (:id p)) :underline :hover}
                     (str (:title p))]
                    [:span {:class "prop-mono"} (str (:source-branch p))]
                    (if-let [c (:checks p)]
                      [:<>
                       [status-pill (:status c)]
                       [:span {:class "prop-mono"}
                        (subs (str (:commit c)) 0 (min 12 (count (str (:commit c)))))]]
                      [:span {:class "prop-panel__muted"} "no repository commits on this branch"])]
                   (when-let [rows (seq (:checks (:checks p)))]
                     (into [:ul {:class "prop-checks__list"}]
                           (for [c rows]
                             ^{:key (str (:check c))}
                             [:li [status-pill (:status c)]
                              [:span {:class "prop-mono"} (str (:check c))]
                              (when (:detail c)
                                [:span {:class "prop-panel__muted"}
                                 (str " — " (:detail c))])])))])))]))))
