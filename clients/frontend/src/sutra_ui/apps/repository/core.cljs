(ns sutra-ui.apps.repository.core
  (:require [sutra-ui.ui.core :as ui :refer [link tag]]
            [sutra.types.languages :as langs]
            [clojure.string :as str]
            [sutra.i18n.core :as i18n]
            [sutra-ui.feature :as feature]
            [sutra-ui.state.branches :as branches]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.data :as data]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.state.staging :as staging]
            [sutra.kit.remote :as rd]
            [sutra-ui.state.governance :as gov]
            [sutra-ui.apps.repository.model :as repo-model]
            [sutra-ui.resource.core :as res]
            [sutra-ui.ui.overlay :as overlay]
            [sutra-ui.ui.busy :as busy]
            [sutra-ui.ui.page :as page]
            [sutra-ui.ui.table :as table]
            [sutra-ui.apphost.platform :as platform]
            [sutra-ui.apps.repository.proposals :as proposals]
            [sutra-ui.apps.repository.ui :refer [action-btn empty-state]]))

(def ^:private send! repo-model/send!)

(defn- output-dataset-rid
  [tname]
  (some (fn [[rid ds]]
          (when (and (= :transform-output (:kind ds))
                     (= tname (:dataset-name ds)))
            rid))
        (rd/value-or @data/datasets {})))

(defn- code-transforms []
  (->> (:transforms @catalog/canonical-schema)
       (filter (comp repo-model/code-transform? val))
       (sort-by (comp str :transform-name val))))

(defn- file-name [spec]
  (str (some-> (:transform-name spec) name)
       (some->> (langs/file-ext (some-> (:language spec) keyword)) (str "."))))

(defn- selected-spec []
  (when-let [id (repo-model/at :selected)]
    (catalog/hydrate-body! :transforms id)
    (get-in @catalog/canonical-schema [:transforms id])))

(defn- breadcrumb []
  (let [spec (selected-spec)
        segs (remove nil?
                     [(if-let [p (:project spec)]
                        (name (or (catalog/name-of p) p))
                        (feature/app-label :repository))
                      (some-> (:repository spec) catalog/name-of name)
                      (when spec (file-name spec))])]
    [ui/breadcrumbs {:items (mapv (fn [seg] {:text seg}) segs)}]))

(defn- reset-all! [] (send! [:repository.staging/reset (count (staging/staged-list))]))

(defn- header []
  (let [staged (count (staging/staged-list))]
    [page/header {:title [breadcrumb]}
     [:span {:class "u-muted"} (i18n/t :ui/repo-uncommitted {:n staged})]
     [action-btn {:label (i18n/t :ui/commit) :primary? true :disabled? (zero? staged)
                  :title (i18n/t :ui/repo-commit-hint)
                  :on-click #(send! [:repository/commit])}]
     (when (pos? staged)
       [overlay/more-menu [[(i18n/t :ui/repo-reset) reset-all! true]]])]))

(defn- file-tree []
  (let [xforms   (code-transforms)
        groups   (group-by (fn [[_ spec]] (some-> (:repository spec) catalog/name-of name)) xforms)
        many?    (> (count groups) 1)
        selected (repo-model/at :selected)]
    [:div {:class "repo-tree"}
     [:div {:class "repo-tree__header"}
      (i18n/t :ui/repo-files {:n (count xforms)})]
     (if (empty? xforms)
       [:div {:class "repo-tree__empty"} (i18n/t :ui/repo-no-files)]
       (let [file-node (fn [[id spec]]
                         {:id id :icon :function :label (file-name spec)
                          :class "repo-tree__file" :is-selected (= id selected)
                          :node-data id})
             groups    (sort-by (comp str key) groups)]
         [ui/tree
          {:compact       true
           :contents      (if many?
                            (mapv (fn [[repo files]]
                                    {:id          (str "repo:" repo)
                                     :label       (or repo (i18n/t :ui/repo-no-repository))
                                     :is-expanded true
                                     :has-caret   false
                                     :child-nodes (mapv file-node files)})
                                  groups)
                            (into [] (comp (mapcat val) (map file-node)) groups))
           :on-node-click (fn [{:keys [node-data]} _ _]
                            (when node-data (send! [:repository/select node-data])))}]))]))

(defn- output-links
  [spec]
  (let [ots (:object-types @catalog/canonical-schema)]
    (into [:span {:class "repo-preview__outputs"}]
          (for [n (res/transform-writes spec)]
            (if (contains? ots n)
              ^{:key (str n)} [res/resource-link :object-types n]
              ^{:key (str n)}
              [ui/tooltip {:content (i18n/t :ui/repo-open-datasets)}
               [ui/button {:minimal true :small true :icon :th
                           :on-click #(shell/enter! :ontology
                                                    {:section :datasets :detail nil})}
                (name n)]])))))

(defn- col-key [c] (or (:attribute-name c) (:name c) (and (keyword? c) c) c))

(defn- preview-table [preview]
  [table/data-table
   {:class "repo-preview__table"
    :rows  (vec (take 25 (:rows preview)))
    :cols  (mapv (fn [c]
                   {:key    (str c)
                    :label  (name c)
                    :mono?  true
                    :render (fn [row] (str (or (get row c) (get-in row [:values c]) "")))})
                 (map col-key (:columns preview)))}])

(defn- preview-pane [spec ds-rid]
  (let [preview @data/dataset-preview-data]
    (when (and preview ds-rid (= ds-rid (or (:dataset preview) (:id preview))))
      [:div {:class "repo-preview"}
       [:div {:class "repo-preview__header"}
        (if (:error preview)
          (i18n/t :ui/repo-preview)
          (i18n/t :ui/repo-output-preview {:n (count (:rows preview))}))
        [output-links spec]]
       (cond
         (:loading? preview)      [busy/block nil (i18n/t :ui/repo-loading)]
         (:error preview)         [ui/problem (str (:error preview)) nil]
         (empty? (:rows preview)) [:div {:class "u-muted"} (i18n/t :ui/repo-preview-empty)]
         :else                    [preview-table preview])])))

(defn- editor-pane []
  (let [id   (repo-model/at :selected)
        spec (selected-spec)]
    (if-not spec
      [empty-state :code (i18n/t :ui/repo-no-file-open) (i18n/t :ui/repo-no-file-open-hint)]
      (let [lang     (some-> (:language spec) name)
            compute? (some? (langs/body-field (keyword lang)))
            field-k  (langs/body-field (keyword lang))
            here?    (catalog/bodies-here? :transforms id)
            body     (get (repo-model/at :drafts) id (get spec field-k ""))
            ds-rid   (output-dataset-rid (:transform-name spec))]
        [:div {:class "repo-editor"}
         [page/toolbar
          [:span {:class "repo-editor__filename"} (file-name spec)]
          [action-btn {:label (i18n/t :ui/repo-preview) :disabled? (nil? ds-rid)
                       :title (i18n/t (if ds-rid :ui/repo-preview-hint :ui/repo-preview-none))
                       :on-click #(send! [:repository.file/preview ds-rid])}]
          [action-btn {:label (i18n/t :ui/repo-build)
                       :title (i18n/t :ui/repo-build-hint)
                       :on-click #(send! [:repository/build [(:transform-name spec)]])}]
          [action-btn {:label (i18n/t :ui/save) :primary? true
                       :disabled? (or (not compute?) (not here?))
                       :title (i18n/t (if compute? :ui/repo-save-hint :ui/repo-save-none))
                       :on-click #(send! [:repository.file/save spec field-k body])}]]
         (if-not compute?
           [:div {:class "repo-editor__note"} (i18n/t :ui/repo-mapping-note)]
           [ui/text-area
            {:value       body
             :on-change   #(send! [:repository.draft/edit id (.. % -target -value)])
             :spell-check false
             :fill        true
             :class       "repo-editor__textarea"}])
         [preview-pane spec ds-rid]]))))

(defn- verb-pill [verb]
  [tag {:minimal true
        :intent  (case verb "create" :success "update" :warning "delete" :danger nil)}
   (or verb "?")])

(def ^:private type->family
  {"object-type" :object-types "link-type" :link-types "interface" :interfaces
   "transform" :transforms "function" :functions "shared-property" :shared-properties
   "source" :sources "graph" :graphs})

(defn- family-of
  [x]
  (let [s (some-> x name)]
    (or (type->family s) (when (some #(= s (name (:family %))) res/families) (keyword s)))))

(defn- existing [family x]
  (when-let [nm (some-> x catalog/name-of name keyword)]
    (when (and family (res/spec-of family nm)) nm)))

(defn- staged-label [i]
  (or (some-> (get-in i [:args :name]) name)
      (some-> (:type-name i) name)
      (some-> (:type i) str)
      (i18n/t :ui/change)))

(defn- changes-panel []
  (let [{:keys [show reason rows]} (staging/staged-view)]
    [:div {:class "repo-changes"}
     (case show
       :loading [busy/block nil (i18n/t :ui/staged-loading)]
       :failed  [ui/problem (i18n/t :ui/staged-failed {:reason reason}) nil]
       :empty   [:div {:class "u-muted"} (i18n/t :ui/repo-changes-empty)]
       (into
        [:div {:class "repo-changes__list"}]
        (for [i rows
              :let [family (type->family (:type i))
                    nm     (when (not= "delete" (:verb i))
                             (existing family (get-in i [:args :name])))]]
          ^{:key (:seq i)}
          [:div {:class "repo-changes__row"}
           [verb-pill (:verb i)]
           [:span {:class "repo-changes__label"}
            (if nm [res/resource-link family nm] (staged-label i))]
           [ui/tooltip {:content (i18n/t :ui/repo-discard-change)}
            [ui/button {:minimal true :small true :icon :cross
                        :on-click #(send! [:repository.staging/discard (:seq i)])
                        :aria-label (i18n/t :ui/repo-discard-change)}]]])))]))

(defn- findings [p]
  (let [fs (:findings p)]
    (if (seq fs)
      fs
      (for [e (get-in p [:problems :structural])]
        {:severity :error :entity-class (:entity-class e) :entity (or (:name e) (:key e))
         :message (str/join "; " (keep identity
                                       [(:key-error e) (:id-error e)
                                        (some->> (:shape-error e) seq (str/join "; "))]))}))))

(defn- problem-count [p]
  (if (or (nil? p) (:error p)) 0
      (count (filter #(not= :warn (keyword (:severity %))) (findings p)))))

(defn- finding-row [{:keys [entity-class entity message severity rule]}]
  (let [family (family-of entity-class)
        nm     (existing family entity)]
    [:div {:class (cond-> "repo-problems__item"
                    (= :warn (some-> severity keyword)) (str " repo-problems__item--warning"))}
     [:div {:class "repo-problems__item-key"}
      (cond
        nm     [res/resource-link family nm]
        entity (str (some-> entity catalog/name-of name))
        :else  (some-> rule name))]
     (when (seq (str message))
       [:div {:class "repo-problems__item-detail"} (str message)])]))

(defn- problems-panel []
  (let [p @data/schema-problems]
    [:div {:class "repo-problems"}
     [page/toolbar
      [ui/button {:small true :icon :refresh :on-click #(send! [:repository/recheck])}
       (i18n/t :ui/repo-recheck)]]
     (cond
       (nil? p)    [:div {:class "u-muted"} (i18n/t :ui/repo-not-checked)]
       (:error p)  [ui/problem (str (:error p)) nil]
       (and (:valid? p) (empty? (findings p)))
       [:div {:class "repo-problems__ok"} (i18n/t :ui/repo-no-problems)]
       :else
       (into [:div {:class "repo-problems__list"}]
             (for [[i f] (map-indexed vector (findings p))]
               ^{:key i} [finding-row f])))]))

(defn- run-branch?
  [nm]
  (str/starts-with? (str nm) "run/"))

(defn- branch-row [b current pr-of]
  (let [nm   (:name b)
        cur? (= nm current)]
    [:div {:class "repo-branches__row"}
     [:span {:class (cond-> "repo-branches__name" cur? (str " repo-branches__name--current"))}
      nm
      (when (:parent b)
        [:span {:class "repo-branches__parent"}
         " " [ui/icon {:icon :git-branch :size 12}] " " (:parent b)])
      (when cur? [:span {:class "repo-branches__current"} (i18n/t :ui/repo-current)])]
     (for [p (get pr-of (str nm))]
       ^{:key (str (:id p))}
       [ui/tooltip {:content (i18n/t :ui/repo-open-pull-request)}
        [link {:href "#" :underline :hover
               :on-click #(do (.preventDefault %)
                              (send! [:repository.proposal/open (:id p)]))}
         [ui/icon {:icon :git-pull :size 12}] " " (:title p)]])
     (when-not cur?
       [action-btn {:label (i18n/t :ui/repo-checkout) :title (i18n/t :ui/repo-checkout-hint)
                    :on-click #(send! [:repository.branch/checkout nm false])}])
     (when-not cur?
       [action-btn
        {:label (i18n/t :ui/repo-view-code) :title (i18n/t :ui/repo-view-code-hint)
         :on-click #(send! [:repository.branch/checkout nm true])}])]))

(defn- branches-view []
  (let [current   (:current-branch @shell/ui-state)
        all       (branches/branch-list)
        scoped    (->> all (remove #(run-branch? (:name %))) (sort-by :name))
        in-scope? (boolean (and current (some #(= (:name %) current) all)))
        n-run     (count (filter #(run-branch? (:name %)) all))
        pr-of     (group-by (comp str :source-branch) (gov/proposal-list))]
    [:div {:class "repo-branches"}
     [page/toolbar
      [:span {:class "u-muted"} (i18n/t :ui/repo-branches-count {:n (count scoped)})]
      [action-btn {:label (i18n/t :ui/repo-new-branch) :primary? true :disabled? (not in-scope?)
                   :title (i18n/t (if in-scope? :ui/repo-new-branch-hint
                                      :ui/repo-pick-branch-first))
                   :on-click #(send! [:repository.branch/new current])}]]
     [:div {:class "u-muted"} (i18n/t :ui/repo-run-refs-hidden {:n n-run})]
     (if (empty? scoped)
       [empty-state :git-branch (i18n/t :ui/repo-no-branches) (i18n/t :ui/repo-no-branches-hint)]
       (for [b scoped]
         ^{:key (:name b)} [branch-row b current pr-of]))]))

(defn- settings-view []
  [empty-state :cog (i18n/t :ui/repo-tab-settings) (i18n/t :ui/repo-settings-hint)])

(defn- badge [n] (when (pos? n) n))

(def ^:private staged-n
  (reify IDeref (-deref [_] (badge (count (staging/staged-list))))))

(def ^:private problems-n
  (reify IDeref (-deref [_] (badge (problem-count @data/schema-problems)))))

(page/view! :repository/header header)
(page/view! :repository/files file-tree)
(page/view! :repository/editor editor-pane)
(page/view! :repository/changes changes-panel)
(page/view! :repository/problems problems-panel)
(page/view! :repository/branches branches-view)
(page/view! :repository/proposals (fn [] [proposals/proposals-view]))
(page/view! :repository/checks (fn [] [proposals/checks-view]))
(page/view! :repository/settings settings-view)

(defn- pane [k] {:kind :flow :padding :compact :items [(page/view k)]})

(defn- code-tree []
  (page/werkzeug
   {:sidebar       :repository/files
    :sidebar-label :ui/repo-files-label
    :sidebar-w     240
    :main          {:kind :rows :box {:flex 2} :items [(page/view :repository/editor)]}
    :bottom        {:kind   :tabs
                    :box    {:flex 1}
                    :select (repo-model/handle :helper :changes :repository/helper)
                    :tabs   [{:id    :changes :label (i18n/t :ui/repo-changes)
                              :count staged-n :body (pane :repository/changes)}
                             {:id    :problems :label (i18n/t :ui/repo-problems)
                              :count problems-n :body (pane :repository/problems)}]}}))

(defn- tree []
  {:kind  :rows
   :fill? true
   :items [(page/view :repository/header)
           {:kind   :tabs
            :box    {:grow? true}
            :select (repo-model/handle :tab :code :repository/tab)
            :tabs   [{:id :code :label (i18n/t :ui/repo-tab-code) :body (code-tree)}
                     {:id   :branches :label (i18n/t :ui/repo-tab-branches)
                      :body (pane :repository/branches)}
                     {:id   :pull-requests :label (i18n/t :ui/repo-tab-pull-requests)
                      :body (pane :repository/proposals)}
                     {:id   :checks :label (i18n/t :ui/repo-tab-checks)
                      :body (pane :repository/checks)}
                     {:id   :settings :label (i18n/t :ui/repo-tab-settings)
                      :body (pane :repository/settings)}]}]})

(def repository-app
  (machine/entering
   repo-model/repository [:repository/enter]
   (fn [] [platform/module :repository (tree)])))
