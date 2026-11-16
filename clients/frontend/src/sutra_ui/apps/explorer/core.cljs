(ns sutra-ui.apps.explorer.core
  (:require [sutra-ui.ui.core :as ui]
            [clojure.string :as str]
            [reagent.core :as r]
            [sutra.capps.table :as tbl]
            [sutra.i18n.core :as i18n]
            [sutra-ui.state.atlas :as ks]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.data :as data]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.panel :as panel]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.ui.size :as groesse]
            [sutra-ui.ui.page :as page]
            [sutra-ui.apphost.platform :as platform]
            [sutra.kit.remote :as rd]
            [sutra-ui.apps.explorer.events]
            [sutra-ui.apps.explorer.model :as explorer-model]
            [sutra.types.rid :as rid]
            [sutra-ui.object.card :as oc]
            [sutra-ui.resource.ontology-selector :as ontology-selector]
            [sutra-ui.ui.busy :as busy]
            [sutra-ui.apps.explorer.object-set :as oset]
            [sutra-ui.apps.object-view.core :as object-view]))

(defn- close-drawer!
  []
  (explorer-model/send! [:explorer/drawer false]))

(defn- select-type! [object-type] (explorer-model/send! [:explorer/select-type object-type]))

(def ^:private saved-label explorer-model/saved-label)

(defn open-saved-set!
  [entry]
  (explorer-model/send! [:explorer/open-saved entry]))

(defn- saved-entries []
  (let [{:keys [object-sets temporary]} (:saved-sets @explorer-model/ui)]
    (concat (ks/rows :explorations) object-sets temporary)))

(defn- file? [entry] (= :exploration (rid/rid-kind (:rid entry))))

(defn- file-ref [entry] (str "exploration/" (:rid entry)))

(defn- saved-row [entry]
  [:div {:class "explorer-types__saved-row"
         :data-kind (some-> (:kind entry) name)
         :on-click #(open-saved-set! entry)}
   (when (file? entry)
     (let [on? (ks/file-favorite? (file-ref entry))
           k   (if on? :ui/ox-saved-unfavorite :ui/ox-saved-favorite)]
       [ui/tooltip {:content (i18n/t k)}
        [ui/button {:minimal true :small true :icon (if on? :star :star-empty)
                    :class "explorer-types__saved-star"
                    :aria-pressed on? :aria-label (i18n/t k)
                    :on-click (fn [e]
                                (.stopPropagation e)
                                (ks/toggle-file-favorite! (file-ref entry)))}]]))
   (saved-label entry)
   (when (= "temporary" (some-> (:lifecycle entry) name))
     [:span {:class "u-muted"} (str " · " (i18n/t :ui/ox-saved-temporary))])
   (when (= "list" (some-> (:kind entry) name))
     [:span {:class "u-muted"} (str " · " (i18n/t :ui/ox-saved-is-list))])
   (when (:private entry)
     [:span {:class "u-muted"} (str " · " (i18n/t :ui/ox-save-private))])])

(defn- saved-sets-section []
  (let [q     (r/atom "")
        scope (r/atom "all")]
    (fn []
      (let [all  (saved-entries)
            term (str/lower-case (str/trim @q))
            hits (filter #(and (or (str/blank? term)
                                   (str/includes? (str/lower-case (saved-label %)) term))
                               (case @scope
                                 "mine"    (:mine %)
                                 "private" (:private %)
                                 "favorites" (and (file? %) (ks/file-favorite? (file-ref %)))
                                 true))
                         all)]
        (when (seq all)
          [:div {:class "explorer-types__saved"}
           [:div {:class "explorer-types__title"} (i18n/t :ui/ox-saved-title)]
           [ui/input-group {:type "search" :value @q
                            :placeholder (i18n/t :ui/ox-saved-search)
                            :on-change #(reset! q (.. % -target -value)) :fill true}]
           [ui/html-select {:value @scope
                            :on-change #(reset! scope (.. % -target -value))}
            [:option {:value "all"} (i18n/t :ui/ox-saved-all)]
            [:option {:value "mine"} (i18n/t :ui/ox-saved-mine)]
            [:option {:value "favorites"} (i18n/t :ui/ox-saved-favorites)]
            [:option {:value "private"} (i18n/t :ui/ox-save-private)]]
           (for [entry hits]
             ^{:key (str (:rid entry))}
             [saved-row entry])])))))

(defn- sidebar-type-row
  [{:keys [object-type expanded? on-toggle set-active?
           instances instance-count loading? truncated?]}]
  (let [m (oc/type-meta object-type)]
    [:div {:class "explorer-types__type-row"}
     [:div
      {:class "explorer-types__type-head"
       :style {:background (cond set-active? "var(--c-primary)"
                                 expanded?   "var(--c-surface-soft)"
                                 :else       "transparent")
               :color      (if set-active? "var(--c-on-accent)" "var(--c-text)")}}
      [:span {:on-click on-toggle
              :class "explorer-types__caret"
              :style {:color (if set-active? "var(--c-on-accent)" "var(--c-muted)")}}
       (if expanded? "▾" "▸")]
      [ui/tooltip
       {:content (i18n/t :ui/ox-open-set-hint {:api (:api-name m)})}
       [:span {:on-click #(select-type! object-type)
               :data-type (:api-name m)
               :class "explorer-types__type-name"}
        (when-let [sym (:symbol m)] (str sym " "))
        (oc/type-label m true)]]
      [:span
       {:class "explorer-types__type-count"
        :style {:color (if set-active? "var(--c-on-accent)" "var(--c-muted)")}}
       instance-count]]
     (when expanded?
       [:div {:class "explorer-types__instances"}
        (if (empty? instances)
          (if loading?
            [busy/block {:class "explorer-types__empty"} (i18n/t :ui/ox-loading)]
            [:div {:class "explorer-types__empty"} (i18n/t :ui/ox-no-objects)])
          (into [:div]
                (for [r (sort-by catalog/instance-display-name instances)
                      :let [iid (:id r)]]
                  ^{:key iid}
                  [:div {:class "explorer-types__instance"}
                   [oc/object-link object-type iid
                    {:row r :icon? false
                     :on-open (fn [t id] (close-drawer!)
                                (shell/open-object! t id))}]])))
        (when truncated?
          [:div {:class "explorer-types__empty"}
           (i18n/t :ui/ox-sidebar-truncated {:n (count instances) :total instance-count})])])]))

(def ^:private sidebar-peek 50)

(defn- visible-types
  []
  (sort-by #(str/lower-case (oc/type-label % true))
           (for [[k ot] (:object-types @catalog/schema)
                 :when (not= :hidden (some-> (:visibility ot) name keyword))]
             k)))

(defn- implementers-of [iface]
  (or (get-in @catalog/schema-derived [:implementers iface])
      (for [[k ot] (:object-types @catalog/schema)
            :when (some #{iface} (:implements ot))]
        k)))

(defn- type-groups
  [ots]
  (let [ifaces (:interfaces @catalog/schema)]
    (if (empty? ifaces)
      [[nil ots]]
      (let [groups  (for [[ik spec] ifaces
                          :let [impl    (set (implementers-of ik))
                                members (filter impl ots)]
                          :when (seq members)]
                      [(or (not-empty (:display-name spec)) (tbl/humanize ik)) members])
            grouped (into #{} (mapcat second) groups)
            rest-ts (remove grouped ots)]
        (cond-> (vec (sort-by (comp str/lower-case first) groups))
          (seq rest-ts) (conj [(i18n/t :ui/ox-group-other) rest-ts]))))))

(defn- sidebar []
  (let [expanded (r/atom #{})]
    (fn []
      (let [ots         (visible-types)
            pages       @panel/panel-instances
            counts      (rd/value-or @data/object-counts {})
            active-type (:object-type @explorer-model/ui)
            expanded*   @expanded
            type-row    (fn [ot]
                          [sidebar-type-row
                           {:object-type      ot
                            :expanded?        (contains? expanded* ot)
                            :set-active?      (= active-type ot)
                            :on-toggle        #(if (contains? @expanded ot)
                                                 (swap! expanded disj ot)
                                                 (do (explorer-model/ensure-explorer-rows! ot)
                                                     (swap! expanded conj ot)))
                            :instances        (vec (take sidebar-peek (get-in pages [ot :rows] [])))
                            :loading?         (get-in pages [ot :loading?])
                            :truncated?       (or (some? (get-in pages [ot :next-offset]))
                                                  (> (count (get-in pages [ot :rows] []))
                                                     sidebar-peek))
                            :instance-count   (get counts ot 0)}])]
        [:div {:class "explorer-types"}
         [ui/tooltip {:content (i18n/t :ui/ox-home-hint)}
          [:div {:class "explorer-types__title explorer-types__home"
                 :on-click #(do (close-drawer!)
                                (explorer-model/send! [:explorer/home]))}
           (i18n/t :ui/ox-object-types)]]
         [ontology-selector/selector]
         (if (empty? ots)
           [:div {:class "explorer-types__no-schema"}
            (i18n/t :ui/ox-no-types)]
           (into [:div]
                 (for [[label members] (type-groups ots)]
                   ^{:key (str label)}
                   [:div {:class "explorer-types__group"}
                    (when label [:div {:class "explorer-types__group-title"} label])
                    (for [ot members]
                      ^{:key ot} [type-row ot])])))
         [saved-sets-section]]))))

(defn- type-tile [ot counts]
  (let [m (oc/type-meta ot)]
    [ui/tooltip
     {:content (i18n/t :ui/ox-open-set-hint {:api (:api-name m)})}
     [:button {:class "explorer-home__type"
               :on-click #(select-type! ot)}
      [:span {:class "explorer-home__type-name"}
       (when-let [s (:symbol m)] [:span {:class "explorer-home__type-icon"} s])
       (oc/type-label m true)]
      [:span {:class "explorer-home__type-count"} (get counts ot 0)]]]))

(defn- matches? [q & texts]
  (let [q (str/lower-case q)]
    (some #(and % (str/includes? (str/lower-case (str %)) q)) texts)))

(def ^:private sample-size 6)

(defn- open-with-keywords! [t q] (explorer-model/send! [:explorer/open-with-keywords t q]))

(defn- section [title body]
  [:div {:class "explorer-home__section"}
   (when title [:div {:class "explorer-types__title"} title])
   body])

(def ^:private home
  (machine/entering
   explorer-model/explorer nil [:explorer.home/leave]
   (fn []
      (let [{kw :keyword {:keys [q hits handed]} :home} @explorer-model/ui
            _      (when (and (seq kw) (not= kw handed))
                     (explorer-model/send! [:explorer.home/hand kw]))
            q      (or q "")
            ots    (visible-types)
            counts (rd/value-or @data/object-counts {})
            query  (str/trim q)
            active (>= (count query) 2)
            tiles  (fn [ts] (into [:div {:class "explorer-home__types"}]
                                  (for [ot ts] ^{:key ot} [type-tile ot counts])))]
        [:div {:class "explorer-home"}
         [ui/input-group {:class "explorer-home__search"
                          :type "search"
                          :value q
                          :auto-focus true
                          :placeholder (i18n/t :ui/ox-search-placeholder)
                          :on-change #(explorer-model/send!
                                       [:explorer.home/type (.. % -target -value)])
                          :fill true :large true}]
         (if active
           (let [types (filter #(let [m (oc/type-meta %)]
                                  (matches? query (:display-name m) (:plural m) (:api-name m)))
                               ots)
                 saved (filter #(matches? query (saved-label %)) (saved-entries))]
             [:div {:class "explorer-home__results"}
              (when (seq types) [section (i18n/t :ui/ox-object-types) (tiles types)])
              (when (seq saved)
                [section (i18n/t :ui/ox-saved-title)
                 (into [:div] (for [e saved] ^{:key (str (:rid e))} [saved-row e]))])
              [section (i18n/t :ui/ox-objects)
               (cond
                 (nil? hits)   [:div {:class "u-muted"} (i18n/t :ui/ox-searching)]
                 (empty? hits) [:div {:class "u-muted"} (i18n/t :ui/ox-no-matches)]
                 :else
                 (into [:div]
                       (for [{:keys [t n rows]} hits]
                         ^{:key (str t)}
                         [:div {:class "explorer-home__section"}
                          [:div {:class "explorer-types__title"}
                           (str (oc/type-label t (not= 1 n)) " · " n " ")
                           [:a
                            {:href "#"
                             :on-click (fn [e] (.preventDefault e) (open-with-keywords! t query))}
                            (i18n/t :ui/ox-show-all {:n n})]]
                          (into [:div {:class "explorer-home__objects"}]
                                (for [row (take sample-size rows)]
                                  ^{:key (str t "/" (:id row))}
                                  [oc/object-card t (:id row) {:row row :compact? true}]))])))]])
           [:<>
            [saved-sets-section]
            (into [:div {:class "explorer-home__groups"}]
                  (for [[label members] (type-groups ots)]
                    ^{:key (str label)}
                    [section label (tiles members)]))])]))))

(defn- preview-instance [object-type id]
  (some #(when (= (:id %) id) %) (panel/rows-of object-type)))

(defn- set-view []
  (if-let [ot (:object-type @explorer-model/ui)]
    [oset/workspace ot shell/open-object!]
    [home]))

(defn- panel-view []
  (when-let [ot (:object-type @explorer-model/ui)]
    (let [sel (:selection (oset/set-state ot))]
      (case (count sel)
        0 nil
        1 [object-view/panel ot (preview-instance ot (first sel))]
        [object-view/set-panel ot (filterv #(contains? sel (:id %)) (panel/rows-of ot))]))))

(def ^:private detail
  (reify
    IDeref
    (-deref [_]
      (when-let [ot (:object-type @explorer-model/ui)]
        (let [{:keys [selection select-mode?]} (oset/set-state ot)]
          (when-not (and select-mode? (groesse/kompakt?)) (seq selection)))))
    IReset
    (-reset! [_ _]
      (some-> (:object-type @explorer-model/ui) oset/clear-selection!)
      nil)))

(def ^:private drawer
  (reify
    IDeref
    (-deref [_] (:drawer? @explorer-model/ui))
    IReset
    (-reset! [_ open?] (explorer-model/send! [:explorer/drawer (boolean open?)]) open?)))

(page/view! :explorer/sidebar (fn [] [sidebar]))
(page/view! :explorer/set set-view)
(page/view! :explorer/panel panel-view)

(def ^:private tree
  (page/werkzeug {:sidebar         :explorer/sidebar
                  :sidebar-label   :ui/ox-object-types
                  :sidebar-open    drawer
                  :sidebar-button? false
                  :main          (page/liste-detail {:list   :explorer/set
                                                     :detail :explorer/panel
                                                     :open   detail
                                                     :resize :explorer/panel})}))

(def root
  (machine/entering
   explorer-model/explorer [:explorer.saved/ensure]
   (fn [] [platform/module :explorer tree])))
