(ns sutra-ui.apps.explorer.object-set
  (:require [sutra-ui.ui.chart :as chart]
            [sutra-ui.ui.overlay :as overlay]
            [sutra-ui.download :as dl]
            [sutra-ui.ui.busy :as busy]
            [sutra-ui.ui.size :as groesse]
            [sutra-ui.storage :as storage]
            [sutra-ui.ui.core :as ui]
            [sutra-ui.ui.filter :as ui-filter]
            [sutra-ui.ui.page :as page]
            [sutra-ui.ui.table :as table]
            [sutra.kit.paging :as pg]
            [sutra.kit.coerce :refer [->kw]]
            [clojure.set]
            [clojure.string :as str]
            [reagent.core :as r]
            [sutra.ref.tokens :as rt]
            [sutra.types.decimal :as d]
            [sutra.types.query :as q]
            [sutra.capps.table :as tbl]
            [sutra.i18n.core :as i18n]
            [sutra-ui.apps.object-view.actions :as oa]
            [sutra-ui.object.card :as oc]
            [sutra-ui.object.open-in :as open-in]
            [sutra-ui.resource.core :as res]
            [sutra-ui.state.atlas :as ks]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.panel :as panel]
            [sutra-ui.apps.explorer.address :as address]
            [sutra-ui.apps.explorer.model :as explorer-model]))

(defn- visibility [f] (some-> (get-in f [:opts :visibility]) ->kw))

(defn field-defs
  [object-type]
  (->> (get-in @catalog/schema [:object-types object-type :fields])
       (remove #(#{:name-plural :markings} (:name %)))
       (remove #(= :hidden (visibility %)))
       vec))

(def ^:private field-label oa/field-label)

(declare linked-field-defs)

(defn- label-of
  [object-type field]
  (if (vector? field)
    (or (some #(when (= (:name %) field) (:label %)) (linked-field-defs object-type))
        (tbl/humanize (nth field 2)))
    (oa/prop-label object-type field)))

(defn- title-key [object-type]
  (some-> (get-in @catalog/schema [:object-types object-type :title-key]) ->kw))

(defn- link-sides
  [object-type]
  (->> (catalog/link-sides @catalog/schema object-type)
       (keep (fn [{:keys [api-name label far]}]
               (when-let [peer-type (some-> far catalog/name-of)]
                 (when (and api-name
                            (not= :hidden (some-> (get-in @catalog/schema
                                                          [:object-types peer-type :visibility])
                                                  name keyword)))
                   {:side api-name :label label :peer-type peer-type}))))
       (sort-by :label)
       vec))

(defn linked-field-defs
  [object-type]
  (vec (sort-by :label
                (for [{:keys [side label peer-type]} (link-sides object-type)
                      {:keys [name type] :as f} (field-defs peer-type)]
                  {:name  [:link side name]
                   :type  type
                   :label (str label " › " (field-label f))}))))

(defn numeric-fields [object-type]
  (filter (comp d/numeric-value-type? :type) (field-defs object-type)))

(defn- to-num
  [x]
  (when-not (and (string? x) (str/blank? x))
    (d/lossy-number x)))

(defn rows-of [object-type]
  (panel/rows-of object-type))

(def ops-for-string
  [[:eq :ui/ox-op-is] [:ne :ui/ox-op-is-not] [:contains :ui/ox-op-contains]
   [:empty :ui/ox-op-empty] [:not-empty :ui/ox-op-not-empty]])

(def ops-for-number
  [[:eq "="] [:ne "≠"] [:gt ">"] [:gte "≥"] [:lt "<"] [:lte "≤"]
   [:between :ui/ox-op-between] [:empty :ui/ox-op-empty] [:not-empty :ui/ox-op-not-empty]])

(defn ops-for [field-type]
  (if (d/numeric-value-type? field-type) ops-for-number ops-for-string))

(defn- op-text [op]
  (let [l (or (some #(when (= op (first %)) (second %)) (concat ops-for-number ops-for-string))
              (name op))]
    (if (keyword? l) (i18n/t l) l)))

(defn- needs-value?  [op] (not (#{:empty :not-empty} op)))
(defn- needs-value2? [op] (= op :between))

(defn filtered-rows
  [object-type _view-state]
  (vec (rows-of object-type)))

(defn set-state [object-type]
  (or (get-in @explorer-model/ui [:set object-type]) address/blank-set))

(def page-size 1000)

(declare op->wire typed-value)

(defn- server-filter
  [object-type {:keys [field op value value2]}]
  (let [v  #(typed-value object-type field %)
        v? (not (str/blank? (str value)))]
    (case op
      :empty     {:nil? true}
      :not-empty {:nil? false}
      :contains  (when v? {:contains-ci (str/lower-case (str/trim (str value)))})
      :between   (when (and v? (not (str/blank? (str value2))))
                   (let [a (v value) b (v value2)]
                     (if (and (number? a) (number? b))
                       {:>= (min a b) :<= (max a b)}
                       {:>= a :<= b})))
      (when (and v? (op->wire op)) {(op->wire op) (v value)}))))

(defn server-query
  [object-type view-state]
  (let [restrict (:restrict view-state)
        where (reduce (fn [acc {:keys [field] :as f}]
                        (if-let [m (when field (server-filter object-type f))]
                          (if (vector? field)
                            (update-in acc [(nth field 1) (nth field 2)] merge m)
                            (update acc field merge m))
                          acc))
                      {} (:filters view-state))
        q     (str/trim (str (:search view-state)))
        {:keys [field dir]} (:sort view-state)]
    (not-empty
     (cond-> {}
       restrict         (assoc :ids (vec (sort (:ids restrict))))
       (seq where)      (assoc :where where)
       (seq q)          (assoc :keywords (or (rt/all-words q) q))
       (and (seq q) (not (keyword? field))) (assoc :order-by q/relevance :sort-dir :desc)
       (and (str/blank? q) (not (keyword? field))) (assoc :order-by q/title :sort-dir :asc)
       (keyword? field) (assoc :order-by field :sort-dir (or dir :asc))))))

(defn set-output
  [object-type view-state]
  (let [sel (:selection view-state)]
    {:kind :object-set :object-type object-type
     :query (if (seq sel)
              {:ids (vec (sort (map str sel)))}
              (dissoc (server-query object-type view-state) :order-by :sort-dir))}))

(defn- server-read
  [object-type q]
  (let [{:keys [reads reading]} @explorer-model/ui]
    (when-not (or (contains? (get reads object-type) q) (contains? reading [object-type q]))
      (explorer-model/send! [:explorer.read/need object-type q]))
    (get-in reads [object-type q])))

(defn query-base
  [object-type view-state]
  (-> (server-query object-type view-state)
      (dissoc :order-by :sort-dir)
      (assoc :find object-type)))

(defn- set-base [object-type] (query-base object-type (set-state object-type)))

(defn- over-set
  [object-type extra]
  (server-read object-type (merge (set-base object-type) extra)))

(defn load-set! [object-type] (explorer-model/send! [:explorer.set/load object-type]))

(defn- update-set! [object-type f & args]
  (explorer-model/send! (into [:explorer.set/change object-type f] args)))

(defn clear-selection! [object-type] (update-set! object-type assoc :selection #{}))

(defn add-filter!
  ([object-type] (add-filter! object-type {}))
  ([object-type m]
   (update-set! object-type update :filters
                (fnil conj [])
                (merge {:id (str (random-uuid)) :field nil :op :eq :value "" :value2 ""} m))))

(defn add-eq-filter!
  [object-type field value]
  (update-set! object-type update :filters
               (fn [fs]
                 (conj (vec (remove #(and (= (:field %) field) (= (:op %) :eq)) fs))
                       {:id (str (random-uuid)) :field field :op :eq
                        :value (str value) :value2 ""}))))

(defn add-between-filter! [object-type field lo hi]
  (update-set! object-type update :filters
               (fn [fs]
                 (conj (vec (remove #(and (= (:field %) field) (= (:op %) :between)) fs))
                       {:id (str (random-uuid)) :field field :op :between
                        :value (str lo) :value2 (str hi)}))))

(declare field->str*)

(declare wire-query)

(defn- pivot-to!
  [from-type {:keys [label] :as g} from-rows]
  (let [n (or (:total (explorer-model/page-info-of from-type)) (count from-rows))]
    (explorer-model/send!
     [:explorer/pivot from-type g
      (i18n/t :ui/ox-pivot-chip
              {:n n :type (oc/type-label from-type (not= 1 n)) :link label})])))

(defn linked-objects
  [object-type]
  (let [view-state   (set-state object-type)
        rows (filtered-rows object-type view-state)
        gs   (link-sides object-type)]
    (when (seq gs)
      [:div {:class "explorer-objectset__linked"}
       (for [{:keys [side label peer-type] :as g} gs
             :let [tname (oc/type-label peer-type true)]]
         ^{:key (str side)}
         [ui/tooltip
          {:content (i18n/t :ui/ox-pivot-hint {:n (count rows) :type tname})}
          [:button {:class "explorer-objectset__linked-tile"
                    :data-side (name side)
                    :on-click #(pivot-to! object-type g rows)}
           [:span {:class "explorer-objectset__linked-count"} (str "→ " tname)]
           (when (and (seq label) (not= label tname))
             [:span {:class "explorer-objectset__linked-side"} label])]])])))

(defn restriction-chip [object-type]
  (let [{:keys [label pending?]} (:restrict (set-state object-type))]
    (when label
      [:div (cond-> {:class "explorer-objectset__pivot-chip"}
              pending? (assoc :data-pending "true"))
       [:span {:class "u-muted"} (if pending? (str label " …") label)]
       [ui/button {:small true :outlined true :intent :danger
                   :on-click #(explorer-model/send! [:explorer.restrict/clear object-type])}
        (i18n/t :ui/ox-restrict-clear)]])))

(def op->wire
  {:eq := :ne :not= :gt :> :gte :>= :lt :< :lte :<=})

(defn- typed-value [object-type field v]
  (let [t (some #(when (= (:name %) field) (:type %))
                (if (vector? field)
                  (linked-field-defs object-type)
                  (field-defs object-type)))]
    (if (d/numeric-value-type? t) (or (to-num v) v) v)))

(defn savable
  [view-state]
  (let [fs (filter (comp some? :field) (:filters view-state))]
    (cond
      (not (str/blank? (:search view-state)))
      (i18n/t :ui/ox-unsavable-search)

      (and (:restrict view-state) (not (get-in view-state [:restrict :query])))
      (i18n/t :ui/ox-unsavable-ids)

      (and (get-in view-state [:restrict :query])
           (seq (filter (comp some? :field) (:filters view-state))))
      (i18n/t :ui/ox-unsavable-pivot-filters)

      (let [sides (keep #(when (vector? (:field %)) (nth (:field %) 1)) fs)]
        (not= (count sides) (count (distinct sides))))
      (i18n/t :ui/ox-unsavable-same-side)

      (some #(not (contains? op->wire (:op %))) fs)
      (i18n/t :ui/ox-unsavable-ops))))

(declare ->where)

(defn wire-query
  [object-type view-state]
  (when-not (savable view-state)
    (or (get-in view-state [:restrict :query])
        {:find object-type :where (->where object-type view-state)})))

(defn ->where [object-type view-state]
  (reduce (fn [acc {:keys [field op value]}]
            (if (vector? field)
              (let [[_ side attr] field]
                (assoc-in acc [side attr] {(op->wire op) (typed-value object-type field value)}))
              (assoc acc field {(op->wire op) (typed-value object-type field value)})))
          {}
          (filter (comp some? :field) (:filters view-state))))

(def ^:private file-keys
  [:filters :search :sort :viz :columns :chart])

(defn file-body
  [object-type view-state]
  {:query (wire-query object-type (dissoc view-state :search))
   :state (select-keys view-state file-keys)})

(defn- savable-exploration [view-state] (savable (dissoc view-state :search)))

(defn- save-file! [object-type dialog]
  (explorer-model/send! [:explorer.save/file object-type dialog]))

(defn- save-changes! [object-type file]
  (explorer-model/send! [:explorer.save/changes object-type file]))

(defn- open-save-dialog!
  [object-type kind]
  (update-set! object-type assoc :save-dialog
               {:kind kind :name "" :description "" :project "" :folder ""
                :private? (boolean (:home-folders @ks/kompass-index))}))

(defn- save-dialog
  [object-type {:keys [kind name description private? project folder] :as dialog}]
  (let [field! #(update-set! object-type assoc-in [:save-dialog %1] %2)
        ready? (and (not (str/blank? name)) (or private? (seq project)))
        home?  (:home-folders @ks/kompass-index)
        folders (when (seq project)
                  (->> (ks/kompass-node-list)
                       (filter #(and (= :folder (keyword (:node-kind %)))
                                     (= project
                                        (some-> (:project %) catalog/project-of
                                                clojure.core/name))))
                       (sort-by :name)))]
    [:div
     [:div {:class "u-mb-8"}
      (i18n/t (if (= :list kind) :ui/ox-save-list :ui/ox-save-exploration))]
     [ui/input-group {:value name :auto-focus true
                      :placeholder (i18n/t :ui/ox-save-title)
                      :on-change #(field! :name (.. % -target -value)) :fill true}]
     [ui/input-group {:value description
                      :placeholder (i18n/t :ui/ox-save-description)
                      :on-change #(field! :description (.. % -target -value)) :fill true}]
     (for [[v label hint] [[true :ui/ox-save-private :ui/ox-save-private-hint]
                           [false :ui/ox-save-public :ui/ox-save-public-hint]]]
       ^{:key (str v)}
       [ui/tooltip
        {:content (i18n/t (if (and v (not home?)) :ui/ox-save-private-off hint))}
        [:div {:style {:margin-top "var(--sp-2)"}}
         [ui/radio {:name "explorer-save-place" :checked (= v private?)
                    :disabled (and v (not home?))
                    :on-change #(field! :private? v)
                    :label (i18n/t label)}]]])
     (when-not private?
       [res/project-select project #(do (field! :project %) (field! :folder "")) false])
     (when (and (not private?) (seq folders))
       (into [ui/html-select {:value folder
                              :aria-label (i18n/t :ui/ox-save-folder)
                              :on-change #(field! :folder (.. % -target -value))}
              [:option {:value ""} (i18n/t :ui/ox-save-folder-top)]]
             (for [{:keys [rid] nm :name} folders] [:option {:key rid :value rid} nm])))
     [:div {:style {:margin-top "var(--sp-3)" :display "flex" :gap "var(--sp-2)"}}
      [ui/button
       {:small true :disabled (not ready?)
        :on-click #(save-file! object-type (update dialog :name str/trim))}
       (i18n/t :ui/ox-save-open)]
      [ui/button {:small true :on-click #(update-set! object-type assoc :save-dialog nil)}
       (i18n/t :ui/cancel)]]]))

(defn- dropdown
  [object-type view-state k button-attrs button-label items]
  (let [open? (get view-state k)
        close! #(update-set! object-type assoc k false)]
    [ui/popover
     {:is-open        (boolean open?)
      :on-interaction (fn [o _] (if o (update-set! object-type assoc k true) (close!)))
      :placement      :bottom-end
      :class          "explorer-action__menu"
      :content
      (into [ui/menu]
            (for [[label on-click title disabled?] (remove nil? items)]
              [ui/menu-item {:text       label
                             :disabled   disabled?
                             :html-title title
                             :on-click   #(on-click)}]))}
     [ui/button (merge {:small true} button-attrs) button-label]]))

(defn save-controls [object-type]
  (let [view-state  (set-state object-type)
        why (savable view-state)
        why-exploration (savable-exploration view-state)
        {:keys [rid kind error]} (:saved view-state)]
    [:span {:class "explorer-objectset__save"}
     [dropdown object-type view-state :save-open?
      {:disabled (boolean (:saving? view-state))
       :title    (or why (i18n/t :ui/ox-save-hint))}
      (i18n/t :ui/ox-save)
      [(when-let [file (:file view-state)]
         (let [list? (= "list" (name (:kind file)))
               why   (if list? (savable (dissoc view-state :restrict)) why-exploration)]
           [(i18n/t (if list? :ui/ox-update-list :ui/ox-save-changes) {:name (:name file)})
            #(save-changes! object-type file)
            (or why (when list? (i18n/t :ui/ox-update-list-hint))) (boolean why)]))
       [(i18n/t :ui/ox-save-exploration) #(open-save-dialog! object-type :exploration)
        (or why-exploration (i18n/t :ui/ox-save-exploration-hint)) (boolean why-exploration)]
       [(i18n/t :ui/ox-save-list) #(open-save-dialog! object-type :list)
        (or why (i18n/t :ui/ox-save-list-hint)) (boolean why)]]]
     (when-let [dialog (:save-dialog view-state)]
       [ui/popover
        {:is-open        true
         :on-interaction (fn [o _]
                           (when-not o (update-set! object-type assoc :save-dialog nil)))
         :placement      :bottom-end
         :content        [:div {:class "explorer-objectset__save-pop"}
                          [save-dialog object-type dialog]]}
        [:span]])
     (when rid
       [:span {:class "explorer-objectset__saved-flash" :data-rid (str rid)}
        (if (= :list kind) (i18n/t :ui/ox-saved-list-flash)
            (i18n/t :ui/ox-saved-exploration-flash))])
     (when error
       [:span {:class "explorer-objectset__saved-error"} error])]))

(defn- filter-label [object-type {:keys [field op value value2]}]
  (str (label-of object-type field) " " (op-text op)
       (when-not (#{:empty :not-empty} op) (str " " value))
       (when (= :between op) (str "…" value2))))

(defn set-label
  [object-type view-state]
  (let [fs (filter (comp some? :field) (:filters view-state))
        q  (:search view-state)
        l  (str/join " · " (cond-> (mapv #(filter-label object-type %) fs)
                             (not (str/blank? q)) (conj (i18n/t :ui/ox-search-label {:q q}))))]
    (if (str/blank? l) (i18n/t :ui/ox-all-objects) l)))

(defn id-projection
  [object-type]
  {:show [(or (title-key object-type) (:name (first (field-defs object-type))))]})

(defn- snapshot-compare! [object-type view-state]
  (explorer-model/send!
   [:explorer.compare/snapshot object-type (set-label object-type view-state)]))

(defn comparison-bar
  [object-type]
  (let [view-state (set-state object-type)]
    (when-let [{:keys [ids label count]} (:compare-a view-state)]
      (let [b-rows (:rows (over-set object-type (id-projection object-type)))
            b-ids  (set (map #(str (:id %)) b-rows))
            both   (clojure.set/intersection ids b-ids)
            only-a (clojure.set/difference ids b-ids)
            only-b (clojure.set/difference b-ids ids)]
        [:div {:class "explorer-objectset__compare"}
         [:span {:class "explorer-objectset__compare-a"} (str "A · " label " (" count ")")]
         [:span {:class "explorer-objectset__compare-b"}
          (str "B · " (set-label object-type view-state) " (" (clojure.core/count b-rows) ")")]
         [:span {:class "explorer-objectset__compare-both"}
          (i18n/t :ui/ox-compare-both {:n (clojure.core/count both)})]
         [:span {:class "explorer-objectset__compare-onlya"}
          (i18n/t :ui/ox-compare-only {:side "A" :n (clojure.core/count only-a)})]
         [:span {:class "explorer-objectset__compare-onlyb"}
          (i18n/t :ui/ox-compare-only {:side "B" :n (clojure.core/count only-b)})]
         [ui/button
          {:small true :outlined true :intent :danger
           :on-click #(update-set! object-type dissoc :compare-a)}
          (i18n/t :ui/ox-compare-clear)]]))))

(defn- field->str* [f]
  (if (vector? f)
    (str (name (nth f 1)) ">" (name (nth f 2)))
    (some-> f name)))

(defn- str->field [x]
  (when-not (str/blank? x)
    (if (str/includes? x ">")
      (let [[lt attr] (str/split x #">")]
        [:link (keyword lt) (keyword attr)])
      (keyword x))))

(defn- results-under-search! [^js e]
  (let [^js bar  (.closest (.-target e) ".explorer-objectset__filterbar")
        ^js ws   (.-parentElement bar)
        ^js body (.querySelector ws ".explorer-workspace__body")]
    (set! (.-scrollTop ws) (+ (.-scrollTop ws)
                              (- (.-top (.getBoundingClientRect body))
                                 (.-bottom (.getBoundingClientRect bar)))))))

(defn- dismiss-keyboard! []
  (let [^js el (.-activeElement js/document)]
    (when (= "search" (.-type el)) (.blur el))))

(defn- filter-row [object-type fields linked f]
  (let [all   (concat fields linked)
        ftype (some #(when (= (:name %) (:field f)) (:type %)) all)]
    [:div {:class "explorer-objectset__filter-row"}
     [ui/html-select
      {:class "explorer-filter__op"
       :value (str (field->str* (:field f)))
       :on-change
       (fn [e]
         (let [fk (str->field (.. e -target -value))]
           (update-set! object-type update :filters
                        (fn [fs] (mapv #(if (= (:id %) (:id f))
                                          (assoc % :field fk :op :eq) %) fs)))))}
      [:option {:value ""} (i18n/t :ui/ox-filter-property)]
      (for [{:keys [name] :as fd} fields]
        ^{:key name} [:option {:value (clojure.core/name name)} (field-label fd)])
      (when (seq linked)
        [:optgroup {:label (i18n/t :ui/ox-filter-linked)}
         (for [{:keys [name label]} linked]
           ^{:key (field->str* name)}
           [:option {:value (field->str* name)} label])])]
     [ui/html-select
      {:value (str (clojure.core/name (:op f)))
       :on-change
       (fn [e]
         (let [op (keyword (.. e -target -value))]
           (update-set! object-type update :filters
                        (fn [fs] (mapv #(if (= (:id %) (:id f)) (assoc % :op op) %) fs)))))}
      (for [[op _] (ops-for ftype)]
        ^{:key op} [:option {:value (clojure.core/name op)} (op-text op)])]
     (when (needs-value? (:op f))
       [ui/input-group
        {:class "explorer-filter__value" :small true
         :value (:value f) :placeholder (i18n/t :ui/ox-filter-value)
         :on-change
         (fn [e]
           (let [v (.. e -target -value)]
             (update-set! object-type update :filters
                          (fn [fs]
                            (mapv #(if (= (:id %) (:id f)) (assoc % :value v) %) fs)))))}])
     (when (needs-value2? (:op f))
       [ui/input-group
        {:class "explorer-filter__value--narrow" :small true
         :value (:value2 f) :placeholder (i18n/t :ui/ox-filter-value2)
         :on-change
         (fn [e]
           (let [v (.. e -target -value)]
             (update-set! object-type update :filters
                          (fn [fs]
                            (mapv #(if (= (:id %) (:id f)) (assoc % :value2 v) %) fs)))))}])
     [ui/tooltip {:content (i18n/t :ui/ox-filter-remove)}
      [ui/button {:icon :cross :small true :outlined true :intent :danger
                  :on-click #(update-set! object-type update :filters
          (fn [fs] (vec (remove (fn [x] (= (:id x) (:id f))) fs))))}]]]))

(defn filter-bar [object-type]
  (let [view-state     (set-state object-type)
        fields (field-defs object-type)
        linked (linked-field-defs object-type)
        actions (fn []
                  [:<>
                   [ui/button
                    {:small true :on-click #(do (update-set! object-type assoc :filters-open? true)
                              (add-filter! object-type))}
                    (i18n/t :ui/ox-filter-add)]
                   (when (seq (:filters view-state))
                     [ui/tooltip
                      {:content (i18n/t :ui/ox-filter-toggle-hint)}
                      [ui/button
                       {:small true :outlined true :intent :danger
                        :on-click #(update-set! object-type update :filters-open?
(fn [o] (if (false? o) true false)))}
                       (str (i18n/t :ui/ox-filter-count {:n (count (:filters view-state))})
                            (if (false? (:filters-open? view-state)) " \u25b8" " \u25be"))]])
                   (when (or (seq (:filters view-state)) (not (str/blank? (:search view-state))))
                     [ui/button
                      {:small true :outlined true :intent :danger
                       :on-click #(update-set! object-type assoc :filters [] :search "")}
                      (i18n/t :ui/ox-filter-clear-all)])])]
    [:div {:class "explorer-objectset__filterbar"}
     [ui-filter/bar
      {:value       (or (:search view-state) "")
       :on-search   #(update-set! object-type assoc :search %)
       :on-focus    results-under-search!
       :placeholder (i18n/t :ui/ox-keyword-placeholder)
       :chips       (when (or (groesse/kompakt?) (false? (:filters-open? view-state)))
                      (for [f (:filters view-state)]
                        {:key       (:id f)
                         :text      (filter-label object-type f)
                         :on-remove #(update-set! object-type update :filters
                                                  (fn [fs] (filterv (fn [x] (not= (:id x) (:id f)))
                                                                    fs)))}))}
      [:div {:class "explorer-objectset__filter-actions"} [actions]]]
     (let [sheet [:<>
                  [:div {:class "explorer-objectset__filter-sheet-head"} (i18n/t :ui/ox-filters)]
                  (when-not (false? (:filters-open? view-state))
                    (into [:div {:class "explorer-objectset__filterrows"}]
                          (for [f (:filters view-state)]
                            ^{:key (:id f)} [filter-row object-type fields linked f])))
                  [:div {:class "explorer-objectset__filter-actions--sheet"} [actions]]]]
       (if (groesse/kompakt?)
         (when (boolean (:filter-sheet? view-state))
           [overlay/drawer {:on-close #(update-set! object-type assoc :filter-sheet? false)
                            :side :bottom
                            :size :standard
                            :class "explorer-objectset__filter-sheet"
                            :header? false}
            [:div {:class "bp-drawer-body"} sheet]])
         [:div {:class "explorer-objectset__filter-sheet"} sheet]))]))

(defn- sort-indicator [view-state field]
  (when (= (get-in view-state [:sort :field]) field)
    (if (= (get-in view-state [:sort :dir]) :desc) " ▼" " ▲")))

(defn- toggle-sort! [object-type field]
  (update-set! object-type update :sort
               (fn [s] (cond
                         (not= (:field s) field) {:field field :dir :asc}
                         (= (:dir s) :asc)        {:field field :dir :desc}
                         :else                    nil))))

(def ^:private default-column-count 5)

(defn- default-columns
  [fields]
  (let [prom (filterv #(= :prominent (visibility %)) fields)]
    (mapv :name (if (seq prom) prom (take default-column-count fields)))))

(def ^:private card-props-key "sutra.explorer.card-props")

(defonce ^:private card-props
  (r/atom (or (storage/get-edn card-props-key) {})))

(defn- save-card-props! [object-type names]
  (swap! card-props #(if names (assoc % object-type names) (dissoc % object-type)))
  (storage/set-edn! card-props-key @card-props))

(defn- chosen-columns [object-type view-state]
  (if (groesse/kompakt?) (get @card-props object-type) (:columns view-state)))

(defn- choose-columns! [object-type names]
  (cond
    (groesse/kompakt?) (save-card-props! object-type names)
    names    (update-set! object-type assoc :columns names)
    :else    (update-set! object-type dissoc :columns)))

(defn- table-columns
  [object-type view-state]
  (let [tk     (title-key object-type)
        fields (remove #(= tk (:name %)) (field-defs object-type))
        shown  (set (or (chosen-columns object-type view-state) (default-columns fields)))]
    [fields (filterv #(contains? shown (:name %)) fields)]))

(defn- column-picker [object-type view-state fields cols]
  (let [open?  (:columns-open? view-state)
        shown  (set (map :name cols))
        close! #(update-set! object-type assoc :columns-open? false)]
    [ui/popover
     {:is-open            (boolean open?)
      :on-interaction     (fn [o _]
                            (if o (update-set! object-type assoc :columns-open? true) (close!)))
      :placement          :bottom-end
      :class          "explorer-action__menu"
      :popover-class-name "explorer-objectset__columns"
      :content
      (into [:<>
             [:div {:class "explorer-objectset__columns-bar"}
              [ui/button
               {:small true :on-click #(choose-columns! object-type (mapv :name fields))}
               (i18n/t :ui/ox-columns-all)]
              [ui/button {:small true :on-click #(choose-columns! object-type nil)}
               (i18n/t :ui/ox-columns-default)]]]
            (for [{:keys [name] :as f} fields]
              ^{:key name}
              [:label {:class "explorer-objectset__columns-item" :title (clojure.core/name name)}
               [ui/checkbox
                {:checked (contains? shown name)
                 :tag-name :span
                 :class "u-m-0"
                 :on-change #(choose-columns! object-type
                                              (let [now (if (contains? shown name)
                                                          (disj shown name)
                                                          (conj shown name))]
                                                (filterv now (map :name fields))))}]
               (field-label f)]))}
     [ui/tooltip {:content (i18n/t :ui/ox-columns-hint)}
      [ui/button {:small true}
       (i18n/t :ui/ox-columns)]]]))

(defn- cell-text [type v]
  (cond
    (nil? v)          "—"
    (= type :boolean) (i18n/t (if v :ui/ox-yes :ui/ox-no))
    (keyword? v)      (clojure.core/name v)
    :else             (str v)))

(defn- picked-rows
  [object-type rows sel]
  (reify
    IDeref
    (-deref [_] (into #{} (filter #(contains? sel (:id %))) rows))
    IReset
    (-reset! [_ v] (update-set! object-type assoc :selection (into #{} (map :id) v)))
    ISwap
    (-swap! [this f] (reset! this (f @this)))
    (-swap! [this f a] (reset! this (f @this a)))
    (-swap! [this f a b] (reset! this (f @this a b)))
    (-swap! [this f a b xs] (reset! this (apply f @this a b xs)))))

(defn- sort-head [object-type view-state {:keys [field label]}]
  (if field
    [:span {:class    "u-pointer"
            :title    (clojure.core/name field)
            :on-click #(toggle-sort! object-type field)}
     label (sort-indicator view-state field)]
    label))

(defn results-table [object-type on-open]
  (let [view-state    (set-state object-type)
        [_ cols]      (table-columns object-type view-state)
        tk            (title-key object-type)
        rows          (vec (filtered-rows object-type view-state))
        page          (get @panel/panel-instances object-type)]
    [table/object-table
     {:rows          rows
      :cols          (into [{:label  (if tk (label-of object-type tk) (i18n/t :ui/ox-col-title))
                             :field  tk
                             :width  240
                             :copy   #(or (get-in % [:values tk]) (:title %) (:id %))
                             :render (fn [r] [oc/object-link object-type (:id r)
                                              {:row r :on-open on-open}])}]
                           (for [{:keys [name type] :as f} cols]
                             {:label  (field-label f)
                              :field  name
                              :align  (when (d/numeric-value-type? type) :right)
                              :copy   #(let [v (get-in % [:values name])]
                                         (if (nil? v) "" (cell-text type v)))
                              :render (fn [r] (cell-text type (get-in r [:values name])))}))
      :head-cell     #(sort-head object-type view-state %)
      :multi-select? (or (not (groesse/kompakt?)) (boolean (:select-mode? view-state)))
      :select-all?   true
      :results?      true
      :selected      (picked-rows object-type rows (or (:selection view-state) #{}))
      :frozen-cols   (when-not (groesse/kompakt?) 1)
      :on-row-click  #(on-open object-type (:id %))
      :more          (pg/sentinel page (fn [_] (panel/load-more-instances! object-type)))
      :empty         {:title (i18n/t :ui/ox-table-empty)}}]))

(defn- value->bucket [v] (if (or (nil? v) (and (string? v) (str/blank? v))) "—" (str v)))

(def ^:private bucket-limit 25)

(defn- category-counts
  [object-type field]
  (when-let [{:keys [aggregations]} (over-set object-type {:aggs [{:fn :count}] :group-by [field]
                                                           :order-by :count :sort-dir :desc
                                                           :limit bucket-limit})]
    (->> aggregations
         (reduce (fn [m {:keys [group metrics]}]
                   (update m (value->bucket (get group field)) (fnil + 0)
                           (or (to-num (:count metrics)) 0)))
                 {})
         (sort-by (comp - val))
         vec)))

(defn- set-total [object-type] (:total (explorer-model/page-info-of object-type)))

(defn- loading []
  [busy/block {:class "explorer-objectset__chart-empty"} (i18n/t :ui/ox-loading-more)])

(defn- bar-list
  [object-type field counts]
  [chart/chart {:rows       (mapv (fn [[v c]] {:v v :c c}) counts)
                :x :v :y :c :chart-kind :bar
                :height     (+ 48 (* 24 (count counts)))
                :on-point   #(when-not (= (:v %) "—") (add-eq-filter! object-type field (:v %)))}])

(defn- pie
  [counts total]
  (let [shown  (reduce + (map val counts))
        total  (max shown (or total 0))
        counts (cond-> counts
                 (> total shown) (conj [(i18n/t :ui/ox-chart-other) (- total shown)]))]
    (if (zero? total)
      [:div {:class "explorer-objectset__chart-empty"} (i18n/t :ui/ox-chart-no-data)]
      [chart/chart {:rows (mapv (fn [[v c]] {:v v :c c}) counts) :x :v :y :c
                    :chart-kind :pie :legend? true :height 240}])))

(defn- nice-width
  [span]
  (let [raw (/ span 10)
        e   (js/Math.pow 10 (js/Math.floor (js/Math.log10 raw)))
        m   (/ raw e)]
    (* e (cond (<= m 1) 1 (<= m 2) 2 (<= m 5) 5 :else 10))))

(defn- field-stats
  [object-type fields]
  (when-let [{:keys [aggregations]}
             (over-set object-type {:aggs (vec (for [f fields fun [:count :min :max :avg :sum]]
                                                 {:fn fun :attr f}))})]
    (let [m (:metrics (first aggregations))]
      (into {} (for [f fields]
                 [f (into {} (for [fun [:count :min :max :avg :sum]]
                               [fun
                                (to-num (get m (keyword (str (name f) "-" (name fun)))))]))])))))

(defn- histogram
  [object-type field]
  (let [stats (get (field-stats object-type [field]) field)]
    (cond
      (nil? stats) [loading]
      (or (nil? (:min stats)) (zero? (or (:count stats) 0)))
      [:div {:class "explorer-objectset__chart-empty"} (i18n/t :ui/ox-chart-no-numbers)]
      :else
      (let [w    (nice-width (max 1e-9 (- (:max stats) (:min stats))))
            resp (over-set object-type {:aggs [{:fn :count}] :group-by [[:width field w]]})
            bins (->> (:aggregations resp)
                      (keep (fn [{:keys [group metrics]}]
                              (when-let [b0 (to-num (first (vals group)))]
                                [b0 (or (to-num (:count metrics)) 0)])))
                      (sort-by first))]
        (if-not resp
          [loading]
          [chart/chart {:rows        (mapv (fn [[b0 c]] {:b (str b0) :b0 b0 :c c}) bins)
                        :x :b :y :c :chart-kind :bar :orientation :vertical :height 200
                        :on-point    #(add-between-filter! object-type field (:b0 %)
                                                           (+ (:b0 %) w))}])))))

(defn- stats-table [object-type]
  (let [nfields (numeric-fields object-type)
        stats   (when (seq nfields) (field-stats object-type (mapv :name nfields)))
        fmt     #(if (some? %) (.toFixed % 2) "—")]
    (cond
      (empty? nfields)
      [:div {:class "explorer-objectset__chart-empty"} (i18n/t :ui/ox-chart-no-numeric-props)]
      (nil? stats) [loading]
      :else
      [table/data-table
       {:rows    nfields
        :row-key :name
        :cols    (into [{:label (i18n/t :ui/ox-stat-property) :render field-label}]
                       (for [[h k f] [[:ui/ox-stat-count :count #(or % 0)]
                                      [:ui/ox-stat-min :min fmt] [:ui/ox-stat-max :max fmt]
                                      [:ui/ox-stat-mean :avg fmt] [:ui/ox-stat-sum :sum fmt]]]
                         {:label  (i18n/t h)
                          :align  :right
                          :render #(f (get-in stats [(:name %) k]))}))}])))

(defn- chart-field-picker [object-type kind sel on-pick]
  (let [fields (if (= kind :numeric) (numeric-fields object-type) (field-defs object-type))]
    [ui/html-select {:class "explorer-set__sort"
                     :value (str (some-> sel name))
                     :on-change #(on-pick (keyword (.. % -target -value)))}
     (for [{:keys [name] :as f} fields]
       ^{:key name} [:option {:value (clojure.core/name name)} (field-label f)])]))

(defn- default-cat-field
  [rows fields]
  (let [n (count rows)
        score (fn [{:keys [name type]}]
                (let [distinct (count (distinct (map #(get-in % [:values name]) rows)))]
                  [(if (d/numeric-value-type? type) 1 0)
                   (if (< distinct n) 0 1)
                   distinct]))]
    (:name (first (sort-by score fields)))))

(defn charts-panel [object-type]
  (let [view-state     (set-state object-type)
        rows   (filtered-rows object-type view-state)
        fields (field-defs object-type)
        nfields (numeric-fields object-type)
        cat-f  (or (get-in view-state [:chart :cat]) (default-cat-field rows fields))
        num-f  (or (get-in view-state [:chart :num])  (:name (first nfields)))
        counts (when cat-f (category-counts object-type cat-f))
        card   (fn [title body]
                 [:div {:class "explorer-objectset__chart-card"}
                  title body])]
    [:div
     (card [:div {:class "explorer-objectset__chart-card-head"}
            (i18n/t :ui/ox-chart-breakdown)
            [chart-field-picker object-type :categorical cat-f
             #(update-set! object-type assoc-in [:chart :cat] %)]
            [:span {:class "explorer-objectset__chart-hint"}
             (i18n/t :ui/ox-chart-click-value)]]
           (if counts [bar-list object-type cat-f counts] [loading]))
     (card [:div {:class "explorer-matrix__addform-title"}
            (i18n/t :ui/ox-chart-pie)]
           (if counts [pie counts (set-total object-type)] [loading]))
     (when (seq nfields)
       (card [:div {:class "explorer-objectset__chart-card-head"}
              (i18n/t :ui/ox-chart-histogram)
              [chart-field-picker object-type :numeric num-f
               #(update-set! object-type assoc-in [:chart :num] %)]
              [:span {:class "explorer-objectset__chart-hint"}
               (i18n/t :ui/ox-chart-click-bin)]]
             [histogram object-type num-f]))
     (card [:div {:class "explorer-matrix__addform-title"}
            (i18n/t :ui/ox-chart-stats)]
           [stats-table object-type])]))

(defn range-scrubber
  [object-type]
  (let [view-state      (set-state object-type)
        nfields (set (map :name (numeric-fields object-type)))
        field   (some #(when (and (#{:between :gt :gte :lt :lte} (:op %))
                                  (keyword? (:field %))
                                  (nfields (:field %)))
                         (:field %))
                      (:filters view-state))]
    (when field
      (let [m     (some-> (server-read object-type
                                       {:find object-type
                                        :aggs [{:fn :min :attr field} {:fn :max :attr field}]})
                          :aggregations first :metrics)
            mn    (to-num (get m (keyword (str (name field) "-min"))))
            mx    (to-num (get m (keyword (str (name field) "-max"))))
            lo    (if mn (js/Math.floor mn) 0)
            hi    (if mx (js/Math.ceil mx) 100)
            cur   (when (= field (get-in view-state [:scrub :field]))
                    (get-in view-state [:scrub :range]))
            a     (or (first cur) lo)
            b     (or (second cur) hi)]
        [:div {:class "explorer-objectset__scrubber"}
         [:span (label-of object-type field)]
         [:span (str a)]
         [ui/range-slider
          {:class "u-flex-1" :min lo :max (max hi (inc lo)) :value [a b]
           :label-renderer false
           :on-change #(update-set! object-type assoc :scrub {:field field :range %})}]
         [:span (str b)]
         [ui/button {:small true :on-click #(add-between-filter! object-type field a b)}
          (i18n/t :ui/ox-scrub-apply)]]))))

(defn geo-fields
  [object-type]
  (let [fs   (field-defs object-type)
        find (fn [pats]
               (some (fn [{:keys [name type]}]
                       (when (and (d/numeric-value-type? type)
                                  (some #(re-find % (str/lower-case (clojure.core/name name)))
                                        pats))
                         name))
                     fs))]
    {:lat (find [#"^lat" #"latitude"])
     :lng (find [#"^lng" #"^lon" #"^long" #"longitude"])}))

(def ^:private map-cap 10000)

(defn map-view [object-type on-open]
  (let [{:keys [lat lng]} (geo-fields object-type)
        tk   (title-key object-type)
        resp (when (and lat lng)
               (over-set object-type {:show (vec (distinct (remove nil? [lat lng tk])))
                                      :limit map-cap :count? true}))
        rows (:rows resp)]
    (if-not (and lat lng)
      [ui/empty-state {:icon :map
                       :title (i18n/t :ui/ox-map-none {:type (oc/type-label object-type)})
                       :hint (i18n/t :ui/ox-map-none-hint)}]
      (let [pts (keep (fn [r]
                        (let [la (to-num (get-in r [:values lat]))
                              lo (to-num (get-in r [:values lng]))]
                          (when (and la lo) {:id (:id r) :lat la :lng lo :row r})))
                      rows)]
        [:div {:class "explorer-objectset__map"}
         [:div {:class "explorer-objectset__map-caption"}
          (i18n/t :ui/ox-map-caption {:n (count pts) :total (or (:count resp) (count rows))
                                      :lat (label-of object-type lat)
                                      :lng (label-of object-type lng)})]
         [chart/chart {:rows       (vec pts) :x :lng :y :lat :chart-kind :scatter
                       :x-title    (label-of object-type lng)
                       :y-title    (label-of object-type lat)
                       :height     410
                       :on-point   #(on-open object-type (:id %))}]]))))

(defn- csv-cell [v]
  (let [s (if (nil? v) "" (str v))]
    (if (re-find #"[\",\n]" s) (str "\"" (str/replace s "\"" "\"\"") "\"") s)))

(defn- export-csv! [object-type rows]
  (let [fields (mapv :name (field-defs object-type))
        head   (str/join "," (map name fields))
        body   (str/join "\n"
                         (for [r rows]
                           (str/join "," (map #(csv-cell (get-in r [:values %])) fields))))]
    (dl/text! (str head "\n" body) "text/csv;charset=utf-8"
              (str (name object-type) ".csv"))))

(defn- export-json! [object-type rows]
  (dl/text! (.stringify js/JSON
                       (clj->js (mapv #(assoc (:values %) :id (:id %)) rows))
                       nil 2)
            "application/json"
            (str (name object-type) ".json")))

(defn- open-set-action!
  [object-type sel bind?]
  (fn [akw spec]
    (update-set! object-type assoc :actions-open? false)
    (oa/open-bound! (if (and bind? (seq sel))
                      {object-type (vec sel)}
                      {})
                    akw spec
                    {:on-applied #(load-set! object-type)})))

(defn- set-actions-menu [object-type view-state sel]
  (let [items (oa/set-level object-type)
        open? (:actions-open? view-state)]
    [ui/popover
     {:is-open        (boolean open?)
      :on-interaction (fn [o _] (update-set! object-type assoc :actions-open? o))
      :placement      :bottom-end
      :class          "explorer-action__menu explorer-objectset__actions"
      :content
      (into [ui/menu]
            (for [[akw spec bind?] items]
              [ui/menu-item
               {:text       [:<> [:div (oa/action-label akw spec)]
                             [:div {:class "sutra-menu__sub"}
                              (or (:description spec)
                                  (if bind?
                                    (i18n/t :ui/ox-action-on-selected)
                                    (i18n/t :ui/ox-action-creates
                                            {:type (oc/type-label object-type)})))]]
                :multiline  true
                :html-title (name akw)
                :on-click   #((open-set-action! object-type sel bind?) akw spec)}]))}
     [ui/tooltip {:content (if (empty? items)
                             (i18n/t :ui/ox-actions-none)
                             (i18n/t :ui/ox-actions-hint))}
      [ui/button {:small true :disabled (empty? items)}
       (i18n/t :ui/actions-menu)]]]))

(defn- peer-items
  [object-type view-state]
  (let [rows (filtered-rows object-type view-state)]
    (for [{:keys [label peer-type] :as g} (link-sides object-type)
          :let [tname (oc/type-label peer-type true)]]
      [(str "→ " tname
            (when (and (seq label) (not= label tname)) (str " · " label)))
       #(pivot-to! object-type g rows)
       (i18n/t :ui/ox-pivot-hint {:n (count rows) :type tname})])))

(defn sort-bar
  [object-type]
  (let [view-state       (set-state object-type)
        [cols]   (table-columns object-type view-state)
        tk       (title-key object-type)
        rows     (filtered-rows object-type view-state)
        loaded   (count (rows-of object-type))
        total    (:total (explorer-model/page-info-of object-type))
        sf       (get-in view-state [:sort :field])
        label-of* (fn [k]
                    (if (= k tk)
                      (if tk (label-of object-type tk) (i18n/t :ui/ox-col-title))
                      (or (some #(when (= (:name %) k) (field-label %)) cols)
                          (clojure.core/name k))))]
    [:div {:class "explorer-objectset__sortbar"}
     [:span {:class "explorer-objectset__sortbar-count"}
      (str (i18n/t :ui/ox-count {:n (count rows) :loaded loaded
                                 :type (oc/type-label object-type (not= 1 loaded))})
           (when (and total (> total loaded)) (str " / " total)))]
     [dropdown object-type view-state :sort-open?
      {:title (i18n/t :ui/ox-sort)}
      (str (if sf (label-of* sf) (i18n/t :ui/ox-sort-none))
           (case (get-in view-state [:sort :dir]) :asc " ▲" :desc " ▼" "")
           " ▾")
      (cons [(i18n/t :ui/ox-sort-none) #(update-set! object-type assoc :sort nil)]
            (for [k (cons tk (map :name cols)) :when k]
              [(label-of* k) #(toggle-sort! object-type k)]))]]))

(defn set-toolbar [object-type]
  (let [view-state    (set-state object-type)
        rows  (filtered-rows object-type view-state)
        sel   (:selection view-state)
        sel-rows (filter #(contains? sel (:id %)) rows)
        total (or (set-total object-type) (count rows))
        export! (fn [f]
                  (if (seq sel) (f object-type sel-rows)
                      (explorer-model/send! [:explorer/export object-type f])))
        scope (if (seq sel)
                (i18n/t :ui/ox-scope-selected {:n (count sel)})
                (i18n/t :ui/ox-scope-all {:n total}))
        graph-rows (if (seq sel) sel-rows rows)
        graph-scope (if (seq sel) scope (i18n/t :ui/ox-scope-all {:n (count rows)}))
        viz   (:viz view-state)
        [fields cols] (table-columns object-type view-state)]
    [page/toolbar
     [ui/segmented-control
      {:class           "explorer-objectset__viz-toggle"
       :small           true
       :value           viz
       :on-value-change #(update-set! object-type assoc :viz %)
       :options         (for [[k lbl] [[:table :ui/ox-viz-table] [:charts :ui/ox-viz-charts]
                                       [:map :ui/ox-viz-map]]]
                          {:value k :label (i18n/t lbl)})}]
     [:div {:class "explorer-objectset__viz-menu"}
      [dropdown object-type view-state :viz-open?
       {:title (i18n/t :ui/view-label)}
       (str (i18n/t (case viz
                      :charts :ui/ox-viz-charts
                      :map    :ui/ox-viz-map
                      :ui/ox-viz-table))
            " ▾")
       [[(i18n/t :ui/ox-viz-table)  #(update-set! object-type assoc :viz :table)]
        [(i18n/t :ui/ox-viz-charts) #(update-set! object-type assoc :viz :charts)]
        [(i18n/t :ui/ox-viz-map)    #(update-set! object-type assoc :viz :map)]]]]
     [:div {:class "explorer-objectset__filter-menu"}
      [ui/tooltip {:content (i18n/t :ui/ox-filters)}
       [ui/button {:small true :on-click #(update-set! object-type update :filter-sheet? not)}
        (str "⌕ " (i18n/t :ui/ox-filters)
             (when (seq (:filters view-state)) (str " (" (count (:filters view-state)) ")")))]]]
     (let [loaded (count (rows-of object-type))
           total  (:total (explorer-model/page-info-of object-type))
           n      (if (and total (> total loaded) (= (count rows) loaded)) total (count rows))]
       [:span {:class "explorer-objectset__count"
               :data-count (count rows) :data-loaded loaded :data-total (max loaded (or total 0))
               :title (str (i18n/t :ui/ox-count {:n (count rows) :loaded loaded
                                                 :type (oc/type-label object-type
                                                                      (not= 1 loaded))})
                           (when (and total (> total loaded))
                             (i18n/t :ui/ox-count-window {:total total})))}
        [ui/tag {:minimal true :intent :primary} (i18n/t :ui/ox-results {:n n})]
        (when (seq sel)
          [:span.u-muted (i18n/t :ui/ox-count-selected {:n (count sel)})])])
     [:div {:class "u-flex-1"}]
     (when (seq sel)
       [ui/button {:small true :on-click #(update-set! object-type assoc :selection #{})}
        (i18n/t :ui/ox-selection-clear)])
     (when (= :table viz) [column-picker object-type view-state fields cols])
     [set-actions-menu object-type view-state sel]
     [save-controls object-type]
     [dropdown object-type view-state :more-open?
      {:title (i18n/t :ui/ox-more)}
      "⋯"
      (concat
       [[(i18n/t (if (groesse/kompakt?) :ui/ox-card-props-open :ui/ox-columns-open))
         #(update-set! object-type assoc :columns-open? true)
         (i18n/t (if (groesse/kompakt?) :ui/ox-card-props-hint :ui/ox-columns-hint))]
        [(i18n/t :ui/ox-save-open) #(update-set! object-type assoc :save-open? true)
         (i18n/t :ui/ox-save-hint)]
        [(if (:select-mode? view-state) (i18n/t :ui/ox-select-off) (i18n/t :ui/ox-select-on))
         #(update-set! object-type (fn [s] (-> s (update :select-mode? not)
                                               (cond-> (:select-mode? s) (assoc :selection #{})))))
         (i18n/t :ui/ox-select-hint)]]
       (peer-items object-type view-state)
       [[(i18n/t :ui/ox-explore-graph {:scope graph-scope})
         #(explorer-model/show-set-on-graph! (map :id graph-rows))
         (i18n/t :ui/ox-explore-graph-hint)]
        [(i18n/t :ui/ox-export-csv {:scope scope}) #(export! export-csv!)]
        [(i18n/t :ui/ox-export-json {:scope scope}) #(export! export-json!)]
        [(i18n/t :ui/ox-compare) #(snapshot-compare! object-type view-state)
         (i18n/t :ui/ox-compare-hint)]])]]))

(defn workspace
  [object-type on-open]
  (let [view-state (set-state object-type)
        m  (oc/type-meta object-type)]
    [:div {:class "explorer-workspace"}
     [page/header {:title    (str (some-> (:symbol m) (str " ")) (oc/type-label m true))
                   :subtitle [ui/tooltip {:content (i18n/t :ui/ox-api-name)} (:api-name m)]}
      [open-in/menu (set-output object-type view-state)]]
     [linked-objects object-type]
     [filter-bar object-type]
     [restriction-chip object-type]
     [comparison-bar object-type]
     [range-scrubber object-type]
     [set-toolbar object-type]
     [sort-bar object-type]
     [:div {:class "explorer-workspace__body" :on-touch-move dismiss-keyboard!}
      (case (:viz view-state)
        :charts [charts-panel object-type]
        :map    [map-view object-type on-open]
        [results-table object-type on-open])]
     [oa/launcher-modal]]))
