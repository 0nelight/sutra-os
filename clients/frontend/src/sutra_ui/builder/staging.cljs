(ns sutra-ui.builder.staging
  (:require [sutra-ui.ui.overlay :as overlay]
            [sutra-ui.ui.table :as table]
            [sutra-ui.ui.core :as ui]
            [clojure.string :as str]
            [sutra.capps.table :as tbl]
            [sutra-ui.builder.model :as model]
            [sutra-ui.state.branches :as branches]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.data :as data]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.state.staging :as staging]
            [sutra-ui.state.sync :as sync]
            [sutra-ui.object.card :as oc]
            [sutra.i18n.core :as i18n]))

(def verb-intents
  {"create"     :success
   "update"     :primary
   "delete"     :danger
   "connect"    :warning
   "disconnect" :danger})

(def verb-label-keys
  {"create"     :ui/verb-create
   "update"     :ui/verb-update
   "delete"     :ui/verb-delete
   "connect"    :ui/verb-connect
   "disconnect" :ui/verb-disconnect})

(defn verb-pill [verb]
  [ui/tag {:minimal true :round true :intent (verb-intents verb) :html-title verb}
   (if-let [k (verb-label-keys verb)] (i18n/t k) (or verb "?"))])

(def summary-row
  {:font-size "var(--fs-sm)" :color "var(--c-text-2)" :margin-top "var(--sp-0)"})

(defn fmt-value [v]
  (cond (nil? v)     "—"
        (keyword? v) (name v)
        (string? v)  (if (str/blank? v) "—" v)
        :else        (str v)))

(defn prop-label
  [meta k]
  (or (not-empty (get-in meta [:display (keyword k) :name]))
      (tbl/humanize (name k))))

(defn prop-key [meta k]
  [:span {:class "staging__kv-key" :title (name k)} (prop-label meta k) ": "])

(defn render-values [meta vs]
  (when (seq vs)
    [:div {:style summary-row}
     (for [[k v] vs]
       ^{:key (str k)}
       [:span {:class "u-mr-10"}
        [prop-key meta k]
        [:span {:class "staging__kv-value"} (fmt-value v)]])]))

(defn render-changes [meta changes]
  (when (seq changes)
    [:div {:style summary-row}
     (for [{:keys [attr before after]} changes]
       ^{:key (str attr)}
       [:div {:class "u-py-1"}
        [prop-key meta attr]
        [:span {:class "staging__before"} (fmt-value before)]
        [:span {:class "staging__kv-key"} " → "]
        [:span {:class "staging__after"} (fmt-value after)]])]))

(defn render-attrs [attrs]
  (when (seq attrs)
    [:div {:style summary-row}
     (for [[a t] attrs]
       ^{:key (str a)}
       [:span {:class "u-mr-10" :title (name a)}
        [:span {:class "staging__kv-value"} (tbl/humanize (name a))]
        [:span {:class "staging__kv-key"} " · " (catalog/value-type-label t)]])]))

(defn render-link-constraints [links]
  (when (seq links)
    [:div {:style (assoc summary-row :margin-top "var(--sp-1)")}
     (for [{:keys [link-name to-object-type to-interface cardinality]} links
           :let [tgt (or to-object-type to-interface)]]
       ^{:key (str link-name)}
       [:div
        [:span {:class "staging__kv-key"} (i18n/t :ui/kind-link) " "]
        [:span {:class "staging__kv-value" :title (some-> link-name name)}
         (some-> link-name name tbl/humanize)]
        [:span {:class "staging__kv-key"} " → "]
        [:span {:class "staging__kv-value"}
         (if to-interface (some-> tgt name tbl/humanize) (some-> tgt oc/type-label))]
        (when to-interface
          [:span {:class "staging__note"} " (" (i18n/t :ui/kind-interface) ")"])
        [:span {:class "staging__kv-key"} " · "]
        [:span {:class "staging__kv-value"} (some-> cardinality name)]])]))

(def kind-label-keys
  {:object-type :ui/kind-object-type
   :link-type   :ui/link-type
   :interface   :ui/kind-interface})

(defn type-row-head
  [kind type-name]
  [:div
   [:span {:class "staging__kv-key"} (i18n/t (kind-label-keys kind)) " "]
   [:strong {:title (some-> type-name name)}
    (when type-name
      (if (= kind :object-type)
        (oc/type-label type-name)
        (tbl/humanize (name type-name))))]])

(defn staged-object
  [meta id values created?]
  (let [row {:id id :values values}]
    [:<>
     [:span {:class "staging__kv-key"} (oc/type-label meta) " "]
     (if (or created? (nil? id))
       [:strong (or (some-> (:title-key meta) (as-> tk (get values (keyword tk))) fmt-value)
                    (i18n/t :ui/kind-object))]
       [oc/object-link (:type meta) id {:meta meta :row (when (seq values) row)}])]))

(defn link-of-name [link-name]
  (some #(when (= (str link-name) (name (:name %))) %) (:links @catalog/schema)))

(defn staged-row-summary [{:keys [kind verb type type-name attributes
                                   from to cardinality
                                   target-id values changes before-values
                                   from-id to-id
                                   property-list link-constraints]}]
  (let [editing? (#{"create" "update"} verb)]
    (case kind
      :object-type
      [:div [type-row-head kind type-name]
       (when editing? [render-attrs attributes])]

      :link-type
      [:div [type-row-head kind type-name]
       (when editing?
         [:div {:style summary-row}
          [:span {:class "staging__kv-value"} (some-> from oc/type-label)]
          [:span {:class "staging__kv-key"} " → "]
          [:span {:class "staging__kv-value"} (some-> to oc/type-label)]
          [:span {:class "staging__kv-key"} " · " (i18n/t :ui/cardinality) " "]
          [:span {:class "staging__kv-value"} (some-> cardinality name)]])]

      :interface
      [:div [type-row-head kind type-name]
       (when editing?
         [:<>
          [render-attrs property-list]
          [render-link-constraints link-constraints]])]

      (cond
        (nil? type)
        [:div [:span {:class "u-muted"} (str verb)]]

        (#{"connect" "disconnect"} verb)
        (let [lt (link-of-name type)]
          [:div
           [:span {:class "staging__kv-key" :title (str type)}
            (or (not-empty (:forward-name lt)) (some-> type tbl/humanize)) ": "]
           (cond
             (and lt from-id) [oc/object-link (:from lt) from-id]
             from-id          [:span {:class "u-muted"} (str from-id)]
             :else            [:span {:class "u-muted" :title (str target-id)}
                               (i18n/t :ui/kind-link)])
           (when to-id
             [:<>
              [:span {:class "staging__kv-key"} " → "]
              (if lt
                [oc/object-link (:to lt) to-id]
                [:span {:class "u-muted"} (str to-id)])])])

        :else
        (let [meta (oc/type-meta (keyword type))]
          (case verb
            "create" [:div [:div [staged-object meta target-id values true]]
                      [render-values meta values]]
            "update" [:div [:div [staged-object meta target-id before-values false]]
                      (if (seq changes)
                        [render-changes meta changes]
                        [:div {:class "lens__empty"}
                         (i18n/t :ui/no-fields-changed)])]
            "delete" [:div [staged-object meta target-id nil false]]
            [:div [:span {:class "u-muted"} (str verb " " type)]]))))))

(defn- discard-button [{seq-no :seq}]
  [ui/tooltip {:content (i18n/t :ui/discard-action)}
   [ui/button {:on-click   (fn [_] (sync/discard-staged! seq-no))
               :minimal    true
               :small      true
               :icon       :cross
               :aria-label (i18n/t :ui/discard-action)}]])

(defn- staged-table [items]
  [table/data-table
   {:class   "staging__table"
    :rows    items
    :row-key :seq
    :cols    [{:label "#" :head-attrs {:class "staging__th-seq"} :cell-class "staging__cell-seq"
               :render #(str (:seq %))}
              {:label (i18n/t :ui/verb) :head-attrs {:class "staging__th-verb"}
               :render #(vector verb-pill (:verb %))}
              {:label (i18n/t :ui/change) :render #(vector staged-row-summary %)}
              {:label "" :head-attrs {:class "staging__th-actions"}
               :cell-class "staging__cell-actions" :render #(vector discard-button %)}]}])

(defn staging-validity
  []
  (let [p  @data/schema-problems
        pr (:problems p)
        n  (when pr (+ (count (:structural pr))
                       (count (:relational-checks-failed pr))
                       (count (keys (:interfaces pr)))))]
    (cond
      (nil? p) nil
      (:error p)
      [ui/callout {:intent :warning :class "staging__banner"}
       (i18n/t :ui/validity-unknown {:error (:error p)})]
      (:valid? p)
      [ui/callout {:intent :success :class "staging__banner"}
       (i18n/t :ui/schema-valid)]
      :else
      [ui/callout
       {:intent :danger :class "staging__banner"
        :title  (i18n/t (if (= n 1) :ui/schema-problem-one :ui/schema-problems-many) {:n n})}
       (when (seq (:structural pr))
         (into [:ul {:class "staging__problem-list"}]
               (for [e (take 6 (:structural pr))]
                 [:li {:class "repo-problems__check-item"}
                  (str (name (:entity-class e)) " · " (pr-str (:key e)))])))
       (when (seq (:relational-checks-failed pr))
         [:div {:class "dataconn-activity"}
          (i18n/t :ui/relational-checks-failed
                  {:list (str/join ", " (map str (:relational-checks-failed pr)))})])
       (when (seq (:interfaces pr))
         [:div {:class "dataconn-activity"}
          (i18n/t :ui/interface-issues
                  {:list (str/join ", " (map name (keys (:interfaces pr))))})])])))

(defn- staging! [m] (model/send! [:builder.staging/set m]))

(def staging-modal
  (machine/entering
   model/builder [:builder.staging/enter]
   (fn []
      (let [seen   (staging/staged-view)
            items  (:rows seen)
            n      (count items)
            branch (some-> (:branch @sync/snapshot) name)
            ust    (:staging @model/ui)
            close  (fn [] (shell/close-staging!))
            pname  (str/trim (str (:pname ust)))]
        [overlay/modal {:on-close #(close)
                        :class "staging"
                        :title (i18n/t :ui/review-changes)
                        :foot (if (:proposal? ust)
                                [:div {:class "u-flex-items-gap-8"}
                                 [ui/input-group
                                  {:value (:pname ust)
                                   :placeholder (i18n/t :ui/review-branch-name)
                                   :auto-focus true
                                   :on-change #(staging! {:pname (.. % -target -value)})
                                   :class "staging__field"}]
                                 [ui/input-group
                                  {:value (:ptitle ust)
                                   :placeholder (i18n/t :ui/change-does-what)
                                   :on-change #(staging! {:ptitle (.. % -target -value)})
                                   :class "staging__field"}]
                                 [ui/button {:on-click #(staging! {:proposal? false})}
                                  (i18n/t :ui/cancel)]
                                 [ui/button {:on-click (fn [_]
                                                         (model/send!
                                                          [:builder.proposal/make branch pname
                                                           (str/trim (str (:ptitle ust)))
                                                           (mapv #(select-keys % [:action :args])
                                                                 (staging/staged-list))])
                                                         (close))
                                             :disabled (or (zero? n) (empty? pname))
                                             :intent   :primary}
                                  (i18n/t :ui/proposal-create)]]
                                [:div {:class "u-flex-gap-8"}
                                 [ui/tooltip
                                  {:content (i18n/t :ui/proposal-create-hint)}
                                  [ui/button {:on-click #(staging! {:proposal? true})
                                              :disabled (zero? n)
                                              :minimal  true
                                              :intent   :primary}
                                   (i18n/t :ui/proposal-create-open)]]
                                 [ui/tooltip
                                  {:content
                                   (i18n/t :ui/commit-to-branch
                                           {:branch (or branch (i18n/t :ui/the-ontology))})}
                                  [ui/button {:on-click (fn [_] (sync/commit-staged!) (close))
                                              :disabled (zero? n)
                                              :intent   :success}
                                   (str (i18n/t :ui/save-to-ontology)
                                        (when (pos? n) (str " (" n ")")))]]])
                        :foot-end? true}
         [:<>
          [:div {:class "staging__body"}
           [:div {:class "staging__subtitle"}
            (str (i18n/t (if (= n 1) :ui/queued-one :ui/queued-many) {:n n})
                 (when branch (i18n/t :ui/on-branch {:branch branch})))]
           (cond
             (#{:loading :failed} (:show seen))
             [ui/empty-state {:icon (if (= :failed (:show seen)) :error :inbox)
                              :title (if (= :failed (:show seen))
                                       (i18n/t :ui/staged-failed {:reason (:reason seen)})
                                       (i18n/t :ui/staged-loading))}]

             (zero? n)
             (let [{:keys [show tasks target]} (branches/branch-changes-view)
                   saved (when (and branch (= :changes show)) (count tasks))]
               [ui/empty-state
                {:icon   :inbox
                 :title  (i18n/t :ui/planned-none)
                 :action
                 (when (pos? saved)
                   [ui/callout {:intent :primary :class "staging__saved-hint"}
                    (i18n/t :ui/planned-saved-on-branch {:n saved :branch branch :target target})
                    [ui/button {:minimal  true
                                :intent   :primary
                                :on-click (fn [] (close)
                                            (shell/set-branch-panel! true))}
                     (i18n/t :ui/planned-saved-show)]])}])

             :else
             [staged-table items])
           (when (pos? n) [staging-validity])]]]))))
