(ns sutra-ui.shell.consumer
  (:require [sutra-ui.ui.busy :as busy]
            [sutra-ui.ui.overlay :as overlay]
            [sutra-ui.shell.default-workspace :as default-workspace]
            [sutra-ui.ui.core :as ui]
            [sutra-ui.shell.carbon :as carbon]
            [sutra-ui.ui.page :as page]
            [clojure.string :as str]
            [reagent.core :as r]
            [sutra-ui.state.portal :as pstate]
            [sutra-ui.state.auth :as auth]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.platform :as platform]
            [sutra-ui.apphost.registry :as capps]
            [sutra-ui.apphost.render :as render]
            [sutra-ui.apphost.state :as sdk-state]
            [sutra-ui.apphost.view-state :as view-state]
            [sutra.capps.layout :as lay]
            [sutra.capps.portal :as portal]
            [sutra.kit.decisions :as decisions]
            [sutra.types.paths :as paths]
            [sutra-ui.object.card :as oc]
            [sutra-ui.api :as api]
            [sutra.i18n.core :as i18n]
            [sutra-ui.shell.unavailable :refer [unavailable]]))

(declare workspace-strip workspace-shell workspace-editor-panel)

(defn- ws-label [ws]
  (or (not-empty (:display-name ws))
      (let [n (:name ws)] (if (keyword? n) (name n) (str n)))))

(defn- consumer-card
  [app]
  [ui/card {:class       "consumer__card"
            :interactive true
            :role        "button"
            :tab-index   0
            :on-key-down #(when (= "Enter" (.-key %)) (pstate/open-consumer-app! app))
            :on-click    #(pstate/open-consumer-app! app)}
   [:div {:class "consumer__card-icon"} (or (:icon app) "📋")]
   [:div {:class "consumer__card-name"} (pstate/app-label app)]
   (when-let [d (:description app)]
     [:div {:class "consumer__card-desc"} d])])

(defn- consumer-portal
  []
  (let [{:keys [show reason shelves loose]} (decisions/decide :view/portal
                                                              @pstate/consumer-apps
                                                              @pstate/workspaces)]
    [:div {:class "consumer__body"}
     [:h1 {:class "consumer__title"} (i18n/t :ui/my-apps)]
     [workspace-editor-panel]
     [workspace-strip]
     (case show
       :loading
       [busy/block {:class "loading"} (i18n/t :ui/loading)]

       :failed
       [unavailable (i18n/t :ui/apps-failed {:reason reason})]

       :no-apps
       [ui/empty-state {:icon :application :title (i18n/t :ui/no-apps-granted)}]

       :all-assigned
       [ui/empty-state {:icon :tick-circle :title (i18n/t :ui/apps-all-assigned)}]

       [:div
        (when shelves
          [:div {:class "consumer__tray-head"}
           [:h2 {:class "consumer__tray-title"} (i18n/t :ui/apps-unassigned)]
           [:p {:class "consumer__tray-hint"} (i18n/t :ui/apps-unassigned-hint)]])
        [:div {:class "consumer__grid"}
         (for [app loose] ^{:key (:id app)} [consumer-card app])]])]))

(defn consumer-app-view
  ([open] (consumer-app-view open (i18n/t :ui/my-apps)))
  ([{:keys [card branch descriptor data loading? error]} back-label]
   (let [label  (pstate/app-label (merge card (select-keys descriptor [:display-name])))
         back   (when back-label {:label back-label :on-click #(pstate/close-consumer-app!)})
         module? (lay/data-layout? (:layout descriptor))]
     (capps/frame {}
                  (when (and back (not (and descriptor module?)))
                    [page/header {:title (when descriptor label) :icon (:icon card)}
                     [ui/button {:minimal true :icon :arrow-left :on-click (:on-click back)}
                      back-label]])
                  (cond
                    loading? [busy/block (i18n/t :ui/loading)]
                    error    [unavailable error]
                    :else    ^{:key (str branch)}
                    [:<> (capps/render (cond-> descriptor
                                         module? (assoc :app/module {:label label :back back}))
                                       data)])))))

(defonce ^:private workspace-query (r/atom ""))

(defonce ^:private workspace-set (r/atom nil))

(add-watch pstate/workspace-object-type ::workspace-set
           (fn [_ _ _ t] (when (nil? t) (reset! workspace-set nil))))

(defn- kw-name [x] (if (keyword? x) (name x) (some-> x str)))

(defn- open-workspace-set!
  [{:keys [root name]}]
  (pstate/open-workspace-object-type! root)
  (reset! workspace-set (kw-name name)))

(defn- workspace-item-click
  [ws open it]
  (case (:kind it)
    :app         #(pstate/open-workspace-app! (:target it))
    :object-type #(pstate/open-workspace-object-type! (get-in it [:target :id]))
    :object      (when open
                   #(open (kw-name (get-in it [:target :name])) (:object it)))
    :object-set  #(open-workspace-set! (:target it))
    :link        (when (:nav-out ws)
                   #(js/window.open (:url it) "_blank" "noopener"))
    nil))

(defn- workspace-item-label [it]
  (or (:label it)
      (when (= :object (:kind it)) (:object it))
      (not-empty (str (get-in it [:target :display-name])))
      (kw-name (get-in it [:target :name]))
      (:url it)))

(defn- workspace-panel
  [ws open {:keys [title description display items widget]}]
  (let [cards? (= :cards display)]
    [ui/card {:class "ws-panel"}
     (when title [:h2 {:class "ws-panel__title"} title])
     (when description [:div {:class "ws-panel__desc"} description])
     (when (and widget (empty? items))
       [:div {:class "ws-panel__empty"}
        (i18n/t (if (= :favorites widget) :ui/ws-home-no-favorites :ui/ws-home-no-sets))])
     [:div {:class (if cards? "ws-panel__cards" "ws-panel__list")}
      (for [[i it] (map-indexed vector items)
            :let [on (workspace-item-click ws open it)]]
        ^{:key i}
        [:div {:class (str (if cards? "ws-card" "ws-row")
                           (when-not on " ws-item--inert"))
               :on-click on}
         [:span {:class "ws-item__icon"}
          (or (:icon it) (get-in it [:target :icon])
              (case (:kind it)
                :object-type "◇" :object "◆" :object-set "▤" :link "↗" "📋"))]
         [:span {:class "ws-item__label"} (workspace-item-label it)]
         (when cards?
           (when-let [d (get-in it [:target :description])]
             [:span {:class "ws-item__desc"} d]))])]]))

(defn logo-src
  [v]
  (let [s (str/trim (str v))]
    (cond (str/blank? s) nil
          (or (str/starts-with? s "/") (str/starts-with? s "data:")
              (str/includes? s "://")) s
          :else (paths/url :kompass-file {:id s}))))

(defn- workspace-logo
  [ws]
  (if-let [src (logo-src (:logo ws))]
    (let [{:keys [width height]} (:logo-size ws)]
      [:img {:class "ws-home__logo-img" :src src :alt (ws-label ws)
             :style (cond-> {}
                      width  (assoc :max-width (str width "px"))
                      height (assoc :max-height (str height "px")))}])
    [:div {:class "ws-home__logo"} (or (:icon ws) "🧭")]))

(defn- workspace-objects
  [_tables _body]
  (let [ov    (sdk-state/overlays)
        data* (r/atom {})
        open  (render/object-opener ov)]
    (fn [{:keys [descriptor]} body]
      [:<>
       (body open)
       (when descriptor
         [render/object-drawer ov
          {:descriptor descriptor :overlays ov :open-object open :refresh (fn [_])}
          {:descriptor descriptor :handles {} :derived {} :data* data*}])])))

(defn- workspace-tables
  [ws]
  (portal/workspace-tables (pstate/workspace-runtime-of (:id ws))))

(defn- tables-status
  [ws {:keys [show reason]}]
  (case show
    :loading [busy/block {:class "ws-item--inert ws-row"} (i18n/t :ui/shell-tables-loading)]
    :failed  [:div {:class "ws-item--inert ws-row"}
              (i18n/t :ui/shell-tables-failed
                      {:reason (or reason (i18n/t :ui/shell-tables-absent))})
              " "
              [ui/button {:minimal  true
                          :small    true
                          :icon     :refresh
                          :on-click #(pstate/load-workspace-runtime! (:id ws))}
               (i18n/t :ui/shell-tables-retry)]]
    nil))

(defn- object-cards
  [desc open t rows]
  (let [meta (oc/descriptor-meta desc t)]
    [:div {:class "ws-objects"}
     (for [row rows]
       ^{:key (str (:id row))}
       [oc/object-card t (:id row) {:row row :meta meta :on-open open :compact? true}])]))

(defn- search-workspace!
  [target rid params]
  (swap! target assoc :loading? true :error nil)
  (-> (api/my-workspace-search rid params)
      (.then (fn [r] (swap! target assoc :loading? false :params params
                            :groups (vec (:groups r)))))
      (.catch (fn [_] (swap! target assoc :loading? false :params params
                             :groups [] :error true)))))

(defonce ^:private object-hits (r/atom nil))
(defonce ^:private search-timer (atom nil))

(defn- debounced-search! [target rid params]
  (when-let [t @search-timer] (js/clearTimeout t))
  (reset! search-timer (js/setTimeout #(search-workspace! target rid params) 250)))

(defn- search-status [{:keys [loading? error groups]}]
  (cond (or loading? (nil? groups))
        [:div {:class "ws-item--inert ws-row"} (i18n/t :ui/workspace-searching)]
        error    [:div {:class "ws-item--inert ws-row"} (i18n/t :ui/workspace-search-failed)]
        (empty? groups) [:div {:class "ws-item--inert ws-row"} (i18n/t :ui/no-matches)]))

(def ^:private type-page 100)

(defonce ^:private type-view-seed (atom nil))

(defn- type-params
  [rid oset q]
  (cond-> {:type rid :limit type-page}
    oset                 (assoc :set oset)
    (not (str/blank? q)) (assoc :q q)))

(defn- load-more-of-type!
  [hits ws-id rid oset q]
  (let [have (count (:rows (first (:groups @hits))))]
    (swap! hits assoc :more? true)
    (-> (api/my-workspace-search ws-id (assoc (type-params rid oset q) :offset have))
        (.then (fn [r]
                 (when (= (:params @hits) (type-params rid oset q))
                   (swap! hits update-in [:groups 0 :rows] (fnil into [])
                          (:rows (first (:groups r)))))
                 (swap! hits dissoc :more?)))
        (.catch (fn [_] (swap! hits dissoc :more?))))))

(defn- workspace-type-view
  [ws rid oset]
  (let [seed (let [v @type-view-seed] (reset! type-view-seed nil) v)
        hits (r/atom nil)
        q    (r/atom (or seed ""))]
    (search-workspace! hits (:id ws) (type-params rid oset seed))
    (fn [ws rid oset]
      (let [tables (workspace-tables ws)
            desc   (:descriptor tables)
            meta   (oc/descriptor-meta desc rid)
            group  (first (:groups @hits))
            rows   (:rows group)]
        [workspace-objects tables
         (fn [open]
           [:div {:class "ws-home"}
            (if-not desc
              [ui/card {:class "ws-panel"} [tables-status ws tables]]
              [ui/input-group
               {:class "ws-home__search"
                :type "search"
                :placeholder (i18n/t :ui/workspace-type-filter
                                     {:type (oc/type-label meta true)})
                :value @q
                :on-change (fn [e]
                             (let [v (-> e .-target .-value)]
                               (reset! q v)
                               (debounced-search! hits (:id ws)
                                                  (type-params rid oset v))))}])
            (cond
              (not desc) nil
              (seq rows)
              [ui/card {:class "ws-panel"}
               (when-let [n (:count group)]
                 (when (> n (count rows))
                   [:div {:class "ws-panel__desc"}
                    (i18n/t :ui/workspace-type-shown {:shown (count rows) :count n})]))
               [object-cards desc open (:type group) rows]
               (when (and (:count group) (> (:count group) (count rows)))
                 [ui/button {:class    "ws-row"
                             :minimal  true
                             :fill     true
                             :loading  (boolean (:more? @hits))
                             :on-click #(load-more-of-type! hits (:id ws) rid oset @q)}
                  (i18n/t :ui/workspace-type-more {:shown (count rows) :count (:count group)})])]
              :else
              [ui/card {:class "ws-panel"} [search-status @hits]])])]))))

(defn- workspace-home
  [ws]
  (let [raw    @workspace-query
        q      (str/lower-case (str/trim (str raw)))
        hits   (when (seq q)
                 (filter #(str/includes?
                           (str/lower-case (str (name (or (:name %) ""))
                                                " " (or (:display-name %) "")
                                                " " (or (:description %) "")))
                           q)
                         (:apps ws)))
        found  @object-hits
        tables (workspace-tables ws)
        desc   (:descriptor tables)
        by-col (group-by :column (:home ws))]
    [workspace-objects tables
     (fn [open]
       [:div {:class "ws-home"}
        [:div {:class "ws-home__head"}
         [workspace-logo ws]
         [:h1 {:class "ws-home__name"} (ws-label ws)]
         (when-let [sub (:subtitle ws)]
           [:div {:class "ws-home__subtitle"} sub])]
        [ui/input-group
         {:class "ws-home__search"
          :type "search"
          :placeholder (i18n/t :ui/workspace-search)
          :value raw
          :on-change (fn [e]
                       (let [v (-> e .-target .-value)]
                         (reset! workspace-query v)
                         (if (str/blank? v)
                           (reset! object-hits nil)
                           (debounced-search! object-hits (:id ws) {:q v :limit 20}))))}]
        (if (seq q)
          [:div {:class "ws-home__results"}
           (when (seq hits)
             [ui/card {:class "ws-panel"}
              [:h2 {:class "ws-panel__title"} (i18n/t :ui/workspace-search-apps)]
              [:div {:class "ws-panel__list"}
               (for [a hits]
                 ^{:key (:id a)}
                 [:div {:class "ws-row" :on-click #(pstate/open-workspace-app! a)}
                  [:span {:class "ws-item__icon"} (or (:icon a) "📋")]
                  [:span {:class "ws-item__label"} (pstate/app-label a)]])]])
           (when-not desc
             [ui/card {:class "ws-panel"} [tables-status ws tables]])
           (for [{:keys [type rows count]} (when desc (:groups found))]
             ^{:key type}
             [ui/card {:class "ws-panel"}
              [:h2 {:class "ws-panel__title"}
               (str (oc/type-label (oc/descriptor-meta desc type) (not= 1 count)) " · " count)]
              [object-cards desc open type rows]
              (when (> count (clojure.core/count rows))
                [:div {:class "ws-row"
                       :on-click #(do (reset! type-view-seed (str/trim (str raw)))
                                      (pstate/open-workspace-object-type! type))}
                 (i18n/t :ui/ox-show-all {:n count})])])
           (when (and desc (empty? hits))
             [ui/card {:class "ws-panel"} [search-status found]])]
          [:div {:class "ws-home__cols"}
           (for [c [:a :b :c]
                 :let [panels (get by-col c)]
                 :when (seq panels)]
             ^{:key c}
             [:div {:class "ws-home__col"}
              (for [[i p] (map-indexed vector panels)]
                ^{:key i} [workspace-panel ws open p])])])])]))

(defonce ^:private ws-menu-open? (r/atom false))

(defn- landing-card
  [ws]
  (when-let [id (:landing ws)]
    (some #(when (= id (:id %)) %) (:apps ws))))

(defn- open-landing!
  [ws card]
  (view-state/handover!
   (into {} (map (fn [[k v]] [(kw-name k) (view-state/encode-value v)]))
         (:landing-params ws)))
  (pstate/open-workspace-app! card))

(defn- landing-home
  [ws card]
  (r/create-class
   {:display-name "landing-home"
    :component-did-mount (fn [_] (open-landing! ws card))
    :reagent-render (fn [_ _]
                      [busy/block (i18n/t :ui/loading)])}))

(defn- workspace-tools
  [ws nav-out? others shut]
  (list
   (when (and nav-out? (seq others))
     [ui/html-select
      {:class "shell-bar__switch"
       :value ""
       :on-change (fn [e]
                    (let [v (.. e -target -value)]
                      (when (seq v)
                        (shut)
                        (some->> others
                                 (filter #(= v (str (:id %))))
                                 first
                                 pstate/enter-workspace!))))}
      [:option {:value ""} (i18n/t :ui/workspace-switch)]
      (for [o others]
        ^{:key (:id o)} [:option {:value (str (:id o))} (ws-label o)])])
   (when (get-in ws [:rights :edit])
     [ui/button {:small true :on-click #(do (shut)
                          (pstate/open-workspace-editor!
                           (let [nm (:name ws)]
                             (if (keyword? nm) (name nm) (str nm)))))}
      (i18n/t :ui/ws-ed-edit)])
   (when nav-out?
     [ui/button {:small true :on-click #(do (shut) (pstate/leave-workspace!))}
      (i18n/t :ui/workspace-leave)])))

(defn- workspace-menu-bar
  [ws]
  (let [open     @pstate/current-consumer-app
        open-id  (get-in open [:card :id])
        nav-out? (:nav-out ws)
        others   (remove #(= (:id %) (:id ws))
                         (filter :promoted (pstate/workspace-list)))
        landing  (landing-card ws)
        on-home? (or (and (not open) (not @pstate/workspace-object-type))
                     (and landing (= open-id (:id landing))))
        anchored (remove #(= (:id %) (:id landing)) (:anchored ws))
        rest-ids  (set (remove (set (cons (:id landing) (map :id anchored)))
                               (map :id (:apps ws))))
        rest-apps (filter #(rest-ids (:id %)) (:apps ws))
        shut     #(reset! ws-menu-open? false)
        utils    (workspace-tools ws nav-out? others shut)
        app-tab  (fn [a rest?]
                   [ui/tab {:id    (:id a)
                            :class (str "shell-tab"
                                        (when rest? " shell-tab--rest")
                                        (when (= open-id (:id a)) " shell-tab--on"))
                            :title (str (when-let [i (:icon a)] (str i " "))
                                        (if (= open-id (:id a))
                                          (lay/module-title (get-in open [:descriptor :layout])
                                                            (pstate/app-label a))
                                          (pstate/app-label a)))}])
        pick!    (fn [id]
                   (if (= id ::home)
                     (pstate/close-consumer-app!)
                     (some->> (concat anchored rest-apps) (filter #(= id (:id %))) first
                              pstate/open-workspace-app!)))]
    [carbon/menu-bar {:variant :workspace}
     [ui/navbar-group {:class "shell-bar__brand"}
      [carbon/home {:on-click #(pstate/close-consumer-app!)}
       [:span {:class "shell-bar__icon"} (or (:icon ws) "🧭")]
       [:span {:class "shell-bar__name"} (ws-label ws)]]
      [carbon/region :workspace :bar-home]
      [ui/popover
       {:is-open        @ws-menu-open?
        :on-interaction (fn [o _] (reset! ws-menu-open? o))
        :placement      :bottom-end
        :content        (into [:div {:class "shell-bar__end shell-bar__end--open"}
                               [carbon/region :workspace :overflow]]
                              utils)}
       [ui/button {:class      "shell-bar__burger"
                   :minimal    true
                   :icon       :menu
                   :aria-label (i18n/t :ui/workspace-menu)}]]]
     [ui/tabs {:id :workspace-tabs :class "shell-bar__tabs" :animate false
               :selected-tab-id (if on-home? ::home open-id) :on-change pick!}
      [ui/tab {:id ::home :title (i18n/t :ui/workspace-home)
               :class (str "shell-tab" (when on-home? " shell-tab--on"))}]
      (for [a anchored] (app-tab a false))
      (for [a rest-apps] (app-tab a true))
      (when (seq rest-apps)
        [ui/html-select
         {:class "shell-bar__more"
          :value ""
          :on-change (fn [e]
                       (let [v (.. e -target -value)]
                         (when (seq v)
                           (some->> (:apps ws)
                                    (filter #(= v (str (:id %))))
                                    first
                                    pstate/open-workspace-app!))))}
         [:option {:value ""} (i18n/t :ui/workspace-more-apps)]
         (for [a rest-apps]
           ^{:key (:id a)}
           [:option {:value (str (:id a))}
            (pstate/app-label a)])])]
     (when-not @ws-menu-open?
       (conj (into [ui/navbar-group {:align :end :class "shell-bar__end"}] utils)
             [carbon/region :workspace :bar-end]))]))

(defn- ws-ed-field
  [label k placeholder]
  (let [{:keys [draft]} @pstate/workspace-editor]
    [:label {:class "ws-ed__field"}
     [:span {:class "ws-ed__label"} label]
     [ui/input-group
      {:class "ws-ed__input" :value (str (get draft k ""))
       :placeholder (or placeholder "")
       :on-change #(pstate/edit-workspace-field! k (.. % -target -value))}]]))

(defn- ws-ed-app-list
  [slot label multi?]
  (let [{:keys [draft]} @pstate/workspace-editor
        chosen (get draft slot)
        on?    (fn [id] (if multi? (boolean (some #{id} chosen)) (= id chosen)))
        toggle (fn [id]
                 (pstate/edit-workspace-field!
                  slot (if multi?
                         (if (on? id) (vec (remove #{id} chosen)) (vec (conj (vec chosen) id)))
                         (if (= id chosen) nil id))))]
    [:div {:class "ws-ed__field"}
     [:span {:class "ws-ed__label"} label]
     [:div {:class "ws-ed__checks"}
      (for [a (pstate/consumer-app-list)
            :let [id (:name a)]]
        ^{:key (str slot "-" id)}
        [(if multi? ui/checkbox ui/radio)
         {:checked (on? id) :class "u-m-0"
          :on-change #(toggle id)
          :label (pstate/app-label a)}])]]))

(defn- ws-ed-general []
  [:<>
   [ws-ed-field (i18n/t :ui/ws-ed-name) :name ""]
   [ws-ed-field (i18n/t :ui/ws-ed-subtitle) :subtitle ""]
   [ws-ed-field (i18n/t :ui/ws-ed-icon) :icon "🧭"]
   [ws-ed-app-list :apps (i18n/t :ui/ws-ed-modules) true]])

(defn- ws-ed-menu-bar []
  [:<>
   [ws-ed-app-list :anchored (i18n/t :ui/ws-ed-anchored) true]
   [ui/checkbox {:checked (not (false? (:nav-out (:draft @pstate/workspace-editor))))
                 :on-change #(pstate/edit-workspace-field!
                              :nav-out (.. % -target -checked))
                 :label (i18n/t :ui/ws-ed-nav-out)}]])

(defn- move [v i d]
  (let [j (+ i d)]
    (if (< -1 j (count v))
      (assoc v i (nth v j) j (nth v i))
      v)))

(defn- drop-at [v i] (into (subvec v 0 i) (subvec v (inc i))))

(defn- ed-text
  [value placeholder on]
  [ui/input-group
   {:class "ws-ed__input" :value (str (or value ""))
    :placeholder (or placeholder "")
    :on-change #(on (let [v (.. % -target -value)] (when-not (str/blank? v) v)))}])

(defn- ed-names
  [id names]
  [:datalist {:id id}
   (for [n names] ^{:key n} [:option {:value n}])])

(defn- schema-names
  [section name-key]
  (sort (keep #(kw-name (get % name-key)) (vals (get @catalog/canonical-schema section)))))

(def ^:private item-kinds [:app :object-type :object :object-set :link])

(def ^:private item-target {:app :app :object-type :object-type :object :object-type
                            :object-set :object-set})

(defn- ed-item
  [it on-change on-move on-drop]
  (let [kind (:kind it)
        tk   (item-target kind)
        list (case kind
               :app        "ws-ed-apps"
               (:object-type :object) "ws-ed-types"
               :object-set "ws-ed-sets"
               nil)]
    [:div {:class "ws-ed__item"}
     [ui/html-select
      {:class "ws-ed__input" :value (name kind)
       :on-change #(on-change {:kind (keyword (.. % -target -value))
                               :label (:label it) :icon (:icon it)})}
      (for [k item-kinds]
        ^{:key k} [:option {:value (name k)} (i18n/t (keyword "ui" (str "ws-ed-kind-" (name k))))])]
     (if (= :link kind)
       [ed-text (:url it) "https://…" #(on-change (assoc it :url %))]
       [ui/input-group
        {:class "ws-ed__input" :list list :value (str (or (kw-name (get it tk)) ""))
         :placeholder (i18n/t :ui/ws-ed-item-target)
         :on-change #(let [v (str/trim (.. % -target -value))]
                       (on-change (if (str/blank? v) (dissoc it tk)
                                      (assoc it tk (keyword v)))))}])
     (when (= :object kind)
       [ed-text (:object it) (i18n/t :ui/ws-ed-item-object) #(on-change (assoc it :object %))])
     [ed-text (:label it) (i18n/t :ui/ws-ed-item-label)
      #(on-change (if % (assoc it :label %) (dissoc it :label)))]
     [ui/input-group
      {:class "ws-ed__icon" :value (str (or (:icon it) ""))
       :placeholder (i18n/t :ui/ws-ed-icon)
       :on-change #(let [v (.. % -target -value)]
                     (on-change (if (str/blank? v) (dissoc it :icon) (assoc it :icon v))))}]
     [ui/button {:icon :arrow-up :small true :on-click #(on-move -1)}]
     [ui/button {:icon :arrow-down :small true :on-click #(on-move 1)}]
     [ui/button {:icon :cross :small true :on-click on-drop}]]))

(defn- ed-widget
  [p on-change on-move on-drop]
  (let [items  (vec (:items p))
        widget (:widget p)
        put    (fn [k v] (on-change (if (some? v) (assoc p k v) (dissoc p k))))]
    [:div {:class "ws-ed__widget"}
     [:div {:class "ws-ed__inline"}
      [ed-text (:title p) (i18n/t :ui/ws-ed-widget-title) #(put :title %)]
      [ui/button {:icon :arrow-up :small true :on-click #(on-move -1)}]
      [ui/button {:icon :arrow-down :small true :on-click #(on-move 1)}]
      [ui/button {:icon :cross :small true :on-click on-drop}]]
     [ed-text (:description p) (i18n/t :ui/ws-ed-widget-description) #(put :description %)]
     [:div {:class "ws-ed__inline"}
      [ui/html-select
       {:class "ws-ed__input" :value (if widget (name widget) "items")
        :on-change #(let [v (.. % -target -value)]
                      (on-change (if (= "items" v)
                                   (-> p (dissoc :widget) (assoc :items (or (:items p) [])))
                                   (-> p (dissoc :items) (assoc :widget (keyword v))))))}
       [:option {:value "items"} (i18n/t :ui/ws-ed-widget-items)]
       [:option {:value "favorites"} (i18n/t :ui/ws-ed-widget-favorites)]
       [:option {:value "object-sets"} (i18n/t :ui/ws-ed-widget-object-sets)]]
      [ui/html-select {:class "ws-ed__input" :value (name (or (:display p) :list))
                       :on-change #(put :display (keyword (.. % -target -value)))}
       [:option {:value "list"} (i18n/t :ui/ws-ed-display-list)]
       [:option {:value "cards"} (i18n/t :ui/ws-ed-display-cards)]]]
     (when-not widget
       [:<>
        (for [[i it] (map-indexed vector items)]
          ^{:key i}
          [ed-item it
           #(put :items (assoc items i %))
           #(put :items (move items i %))
           #(put :items (drop-at items i))])
        [ui/button {:small true :on-click #(put :items (conj items {:kind :app}))}
         (i18n/t :ui/ws-ed-item-add)]])]))

(defonce ^:private image-files (r/atom nil))

(def ^:private image-name #"(?i).*\.(png|jpe?g|gif|svg|webp)$")

(defn- load-image-files! []
  (when (nil? @image-files)
    (reset! image-files [])
    (-> (api/get! :kompass-files)
        (.then #(reset! image-files (filterv (fn [f] (re-matches image-name (str (:name f))))
                                             (:files %))))
        (.catch (fn [_] nil)))))

(defn- ws-ed-logo
  [draft]
  (load-image-files!)
  (let [size (:logo-size draft)
        dim  (fn [k v]
               (let [n (js/parseInt v 10)
                     s (if (pos? n) (assoc size k n) (dissoc size k))]
                 (pstate/edit-workspace-field! :logo-size (not-empty s))))]
    [:div {:class "ws-ed__field"}
     [:span {:class "ws-ed__label"} (i18n/t :ui/ws-ed-logo)]
     [:div {:class "ws-ed__inline"}
      (when-let [src (logo-src (:logo draft))]
        [:img {:src src :alt "" :style {:max-height "36px" :max-width "90px"}}])
      [ui/html-select
       {:class "ws-ed__input" :value (str (or (:logo draft) ""))
        :on-change #(let [v (.. % -target -value)]
                      (pstate/edit-workspace-field! :logo (when-not (str/blank? v) v)))}
       [:option {:value ""} (i18n/t :ui/ws-ed-logo-default)]
       (when-let [v (:logo draft)]
         (when-not (some #(= v (:id %)) @image-files)
           [:option {:value v} v]))
       (for [f @image-files]
         ^{:key (:id f)} [:option {:value (:id f)} (:name f)])]]
     [:div {:class "ws-ed__inline"}
      [ui/numeric-input
       {:class "ws-ed__input" :min 1 :value (str (or (:width size) ""))
        :placeholder (i18n/t :ui/ws-ed-logo-width)
        :on-value-change #(dim :width %2)}]
      [ui/numeric-input
       {:class "ws-ed__input" :min 1 :value (str (or (:height size) ""))
        :placeholder (i18n/t :ui/ws-ed-logo-height)
        :on-value-change #(dim :height %2)}]]
     [:p {:class "ws-ed__hint"} (i18n/t :ui/ws-ed-logo-hint)]]))

(defn- ws-ed-landing
  [draft]
  (let [landing (:landing draft)
        params  (vec (:landing-params draft))
        put     (fn [ps] (pstate/edit-workspace-field! :landing-params (not-empty (into {} ps))))]
    [:div {:class "ws-ed__field"}
     [:span {:class "ws-ed__label"} (i18n/t :ui/ws-ed-landing)]
     [ui/html-select
      {:class "ws-ed__input" :value (str (or (kw-name landing) ""))
       :on-change #(let [v (.. % -target -value)]
                     (pstate/edit-workspace-field! :landing
                                                  (when-not (str/blank? v) (keyword v))))}
      [:option {:value ""} (i18n/t :ui/ws-ed-landing-none)]
      (for [a (:apps draft)]
        ^{:key a} [:option {:value (kw-name a)} (kw-name a)])]
     (when landing
       [:<>
        (for [[i [k v]] (map-indexed vector params)]
          ^{:key i}
          [:div {:class "ws-ed__inline"}
           [ed-text (kw-name k) (i18n/t :ui/ws-ed-landing-param)
            #(put (assoc params i [(if % (keyword %) k) v]))]
           [ed-text (str v) (i18n/t :ui/ws-ed-landing-value)
            #(put (assoc params i [k (or % "")]))]
           [ui/button {:icon :cross :small true :on-click #(put (drop-at params i))}]])
        [:div {:class "ws-ed__inline"}
         [ui/button {:small true :on-click #(put (conj params [:param ""]))}
          (i18n/t :ui/ws-ed-landing-param-add)]
         [ui/button {:small true :on-click #(do (pstate/edit-workspace-field! :landing nil)
                              (pstate/edit-workspace-field! :landing-params nil))}
          (i18n/t :ui/ws-ed-landing-reset)]]])
     [:p {:class "ws-ed__hint"} (i18n/t :ui/ws-ed-landing-hint)]]))

(defn- ws-ed-home
  []
  (let [{:keys [draft]} @pstate/workspace-editor
        panels (vec (or (:home draft) []))
        put    (fn [ps] (pstate/edit-workspace-field! :home (vec ps)))
        in-col (fn [col] (keep-indexed (fn [i p] (when (= col (:column p)) i)) panels))
        swap-in-col (fn [col i d]
                      (let [idx (vec (in-col col))
                            at  (.indexOf idx i)
                            j   (get idx (+ at d))]
                        (when j (put (assoc panels i (nth panels j) j (nth panels i))))))]
    [:<>
     [ed-names "ws-ed-apps" (map kw-name (:apps draft))]
     [ed-names "ws-ed-types" (schema-names :object-types :object-type-name)]
     [ed-names "ws-ed-sets" (schema-names :object-sets :object-set-name)]
     [ws-ed-logo draft]
     [ws-ed-landing draft]
     [:p {:class "ws-ed__hint"} (i18n/t :ui/ws-ed-home-hint)]
     (for [col [:a :b :c]]
       ^{:key col}
       [:div {:class "ws-ed__col"}
        [:span {:class "ws-ed__label"}
         (str (i18n/t :ui/ws-ed-column) " " (str/upper-case (name col)))]
        (for [i (in-col col)]
          ^{:key i}
          [ed-widget (nth panels i)
           #(put (assoc panels i (assoc % :column col)))
           #(swap-in-col col i %)
           #(put (drop-at panels i))])
        [ui/button
         {:small true :on-click #(put (conj panels {:column col :display :list :items []}))}
         (i18n/t :ui/ws-ed-widget-add)]])]))

(defn- ws-ed-access
  []
  (let [{:keys [draft rights projects]} @pstate/workspace-editor]
    [:<>
     [:div {:class "ws-ed__field"}
      [:span {:class "ws-ed__label"} (i18n/t :ui/ws-ed-project)]
      [ui/html-select {:class "ws-ed__input"
                       :value (str (or (:project draft) ""))
                       :on-change #(pstate/edit-workspace-field!
                                    :project (keyword (.. % -target -value)))}
       [:option {:value ""} "—"]
       (for [p projects]
         ^{:key (:id p)}
         [:option {:value (str (clojure.core/name (:name p)))}
          (clojure.core/name (:name p))])]
      [:p {:class "ws-ed__hint"} (i18n/t :ui/ws-ed-project-hint)]]
     [ui/checkbox {:disabled (not (:promote rights))
                   :checked (boolean (:promoted draft))
                   :on-change #(pstate/edit-workspace-field!
                                :promoted (.. % -target -checked))
                   :label (i18n/t :ui/ws-ed-promoted)}]
     [:p {:class "ws-ed__hint"}
      (i18n/t (if (:promote rights) :ui/ws-ed-promote-hint :ui/ws-ed-promote-denied))]
     (when-let [nm (:name @pstate/workspace-editor)]
       ^{:key (str nm)} [default-workspace/section nm])]))

(defn- ws-ed-create []
  (let [{:keys [can-create]} @pstate/workspace-editor
        draft-name (r/atom "")]
    (fn []
      (when can-create
        [:div {:class "ws-ed__create"}
         [ui/input-group {:class "ws-ed__input" :value @draft-name
                          :placeholder (i18n/t :ui/ws-ed-new-name)
                          :on-change #(reset! draft-name (.. % -target -value))}]

         [ui/button {:class "ws-ed__save"
                     :intent :primary
                     :disabled (str/blank? @draft-name)
                     :on-click #(pstate/load-workspace-draft!
                                 (-> @draft-name str/trim str/lower-case
                                     (str/replace #"[^a-z0-9]+" "-")))}
          (i18n/t :ui/ws-ed-create)]]))))

(defn workspace-editor-panel
  []
  (let [{:keys [open? tab name draft rights list error staged saving?]}
        @pstate/workspace-editor]
    (when (boolean open?)
      [overlay/drawer {:on-close #(pstate/close-workspace-editor!)
                       :backdrop? false
                       :close-on-backdrop? false
                       :class "ws-ed__drawer"
                       :header? false}
       [:div {:class "ws-ed"}
        [:div {:class "ws-ed__head"}
         [:strong (i18n/t :ui/ws-ed-title)]
         [:div {:class "ws-ed__head-actions"}
          (when (pstate/workspace-editor-dirty?)
            [ui/button {:small true :on-click #(pstate/discard-workspace-draft!)}
             (i18n/t :ui/ws-ed-discard)])
          [ui/button {:class    "ws-ed__save"
                      :intent   :primary
                      :small    true
                      :loading  saving?
                      :disabled (or (not draft) (not (:edit rights))
                                    (not (pstate/workspace-editor-dirty?)))
                      :on-click #(pstate/stage-workspace!)}
           (i18n/t :ui/ws-ed-save)]
          [ui/button {:icon :cross :small true :on-click #(pstate/close-workspace-editor!)}]]]

        (when error [:div {:class "ws-ed__error"} error])
        (when staged
          [:div {:class "ws-ed__staged"}
           (i18n/t :ui/ws-ed-staged {:neu (or (:created staged) 0)
                                     :geaendert (or (:updated staged) 0)})])

        [:div {:class "ws-ed__list"}
         [:span {:class "ws-ed__label"} (i18n/t :ui/ws-ed-all)]
         (if (seq list)
           (for [w list]
             ^{:key (:id w)}
             (let [on? (= name (clojure.core/name (:name w)))]
               [ui/button {:class      (str "ws-ed__row" (when on? " ws-ed__row--on"))
                           :minimal    true
                           :fill       true
                           :align-text :start
                           :active     on?
                           :on-click   #(pstate/load-workspace-draft!
                                         (clojure.core/name (:name w)))}
                [:span (str (or (:icon w) "🧭") " " (:name w))]
                (when-not (:promoted w)
                  [:span {:class "ws-ed__muted"} (i18n/t :ui/ws-ed-unpromoted)])]))
           [:p {:class "ws-ed__hint"} (i18n/t :ui/ws-ed-none)])]
        [ws-ed-create]

        (when draft
          [:<>
           [ui/tabs {:id :ws-ed :class "ws-ed__tabs" :selected-tab-id tab
                     :on-change (fn [k] (pstate/set-workspace-editor-tab! k))}
            (for [[k label] [[:general (i18n/t :ui/ws-ed-tab-general)]
                             [:home    (i18n/t :ui/ws-ed-tab-home)]
                             [:menu    (i18n/t :ui/ws-ed-tab-menu)]
                             [:access  (i18n/t :ui/ws-ed-tab-access)]]]
              [ui/tab {:id k :title label}])]
           [:div {:class "ws-ed__body"}
            (case tab
              :general [ws-ed-general]
              :home    [ws-ed-home]
              :menu    [ws-ed-menu-bar]
              :access  [ws-ed-access]
              nil)]])]])))

(defn workspace-shell
  []
  (let [ws    @auth/current-workspace
        open  @pstate/current-consumer-app
        bare? (get-in open [:descriptor :layout :header :hidden?])
        own-back? (some #(and (map? %) (= :close-app (:do %)))
                        (tree-seq coll? seq (get-in open [:descriptor :layout])))]
    [carbon/frame {:variant :workspace :class (when bare? "shell--app-bare")}
     [workspace-menu-bar ws]
     [workspace-editor-panel]
     (if open
       [:div {:class "ws-body ws-body--app"}
        (when (and bare? (not own-back?))
          [carbon/back-row (ws-label ws) #(pstate/close-app-back!)])
        [consumer-app-view open nil]]
       [:div {:class "ws-body"}
        (if-let [t @pstate/workspace-object-type]
          (let [oset @workspace-set]
            ^{:key (str t "|" oset)} [workspace-type-view ws t oset])
          (if-let [card (when-not @pstate/workspace-home-shown? (landing-card ws))]
            ^{:key (str (:id ws) "|" (:id card))} [landing-home ws card]
            [workspace-home ws]))])]))

(defn workspace-strip
  []
  (when-let [wss (seq (pstate/workspace-list))]
    [:div {:class "ws-strip"}
     [:h2 {:class "ws-strip__title"} (i18n/t :ui/workspaces)]
     [:div {:class "ws-strip__row"}
      (for [w wss]
        ^{:key (:id w)}
        [:div {:class "ws-strip__card" :on-click #(pstate/enter-workspace! w)}
         [:span {:class "ws-strip__icon"} (or (:icon w) "🧭")]
         [:span {:class "ws-strip__name"} (ws-label w)]
         (when-let [d (or (:subtitle w) (:description w))]
           [:span {:class "ws-strip__desc"} d])])]]))

(defn consumer-shell
  []
  [carbon/frame {:variant :portal}
   [carbon/menu-bar {:variant :portal}
    [ui/navbar-group {:class "shell-bar__brand"}
     [carbon/home {:on-click #(pstate/close-consumer-app!)} [platform/portal-brand]]]
    [ui/navbar-group {:align :end :class "shell-bar__end"}
     [carbon/region :portal :bar-end]]]
   (if-let [open @pstate/current-consumer-app]
     [consumer-app-view open]
     [consumer-portal])])
