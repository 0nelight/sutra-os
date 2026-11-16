(ns sutra-ui.apps.atlas.workspace
  (:require [sutra-ui.ui.core :as ui]
            [reagent.core :as r]
            [clojure.string :as str]
            [sutra.i18n.core :as i18n]
            [sutra-ui.apphost.platform :as platform]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.atlas :as kompass]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.ui.filter :as ui-filter]
            [sutra-ui.ui.page :as page]
            [sutra-ui.ui.style :as style]
            [sutra-ui.apps.atlas.files :as files]
            [sutra-ui.apps.atlas.model :as model]
            [sutra-ui.apps.atlas.project :as project]
            [sutra-ui.apps.atlas.table :as table]
            [sutra-ui.resource.core :as res]))

(def ^:private tabs
  [[:files      :ui/kompass-all-files     :folder-close true]
   [:catalog    :ui/kompass-data-catalog  :tick-circle  true]
   [:portfolios :ui/kompass-portfolios    :briefcase    false]
   [:projects   :ui/kompass-projects      :projects     false]
   [:your-files :ui/kompass-your-files    :person       false]
   [:shared     :ui/kompass-shared        :people       false]])

(defn- area []
  (let [a (or (model/at :area) :files)]
    (if (some #(= a (first %)) tabs) a :files)))

(defn- collection []
  (let [rid (model/at :collection)]
    (some #(when (= rid (:rid %)) %) (:collections @kompass/kompass-index))))

(defn- scope
  []
  (case (area)
    :catalog    (if-let [c (collection)]
                  [:collection (:rid c)]
                  (when (= :files (model/at :catalog-tab)) [:catalog :files]))
    :portfolios nil
    (area)))

(defn- send! [& event] (model/send! (vec event)))

(defn- family-title [family]
  (if (= :projects family) (i18n/t :ui/kompass-projects) (res/family-label family)))

(defn- ordered-projects
  [rows]
  (let [recents (zipmap @kompass/kompass-recents (range))]
    (sort-by (fn [{:keys [nm name] :as r}]
               [(if (kompass/file-favorite? (files/fav-ref r)) 0 1)
                (get recents nm 99)
                (str/lower-case name)])
             rows)))

(defn- granted-projects [me]
  (set (for [[pk spec] (catalog/project-list)
             {:keys [principal role]} (:grants spec)
             :when (and (= (str me) (str principal)) (not= :owner (keyword role)))]
         pk)))

(def ^:private newest {:col :modified :dir :desc})
(def ^:private by-name {:col :name :dir :asc})

(defn- list-of
  [sc]
  (let [rows (files/live-rows)
        me   (:me @kompass/kompass-index)]
    (case (if (vector? sc) (first sc) sc)
      :files      {:rows rows :quick? true :default-sort newest}
      :projects   {:rows (ordered-projects (filter #(= :projects (:family %)) rows))
                   :empty-text (i18n/t :ui/kompass-no-resources)}
      :your-files {:rows (filter #(or (:private? %) (:mine? %)
                                      (and (= :documents (:family %))
                                           (= me (:author (res/spec-of :documents (:nm %))))))
                                 rows)
                   :notice (when-not (:home-folders @kompass/kompass-index)
                             (i18n/t :ui/kompass-home-off))
                   :empty-text (i18n/t :ui/kompass-your-files-hint)
                   :default-sort newest}
      :shared     (let [granted (granted-projects me)]
                    {:rows (filter #(or (contains? (:shared @kompass/kompass-index)
                                                   (:placed-ref %))
                                        (and (contains? granted (:project %))
                                             (not= :projects (:family %))
                                             (not (:mine? %))))
                                   rows)
                     :empty-text (i18n/t :ui/kompass-shared-empty)
                     :default-sort newest})
      :catalog    {:rows (filter #(or (:promoted? %) (:curated? %)) rows)
                   :group-tags? true :default-sort by-name
                   :empty-text (i18n/t :ui/kompass-data-catalog-empty)}
      :collection (let [in? (set (:refs (collection)))]
                    {:rows (filter #(in? (files/fav-ref %)) rows)
                     :group-tags? true :default-sort by-name
                     :empty-text (i18n/t :ui/kompass-collection-empty)}))))

(defn- matches-text? [q {:keys [name path description]}]
  (or (str/blank? q)
      (str/includes? (str/lower-case (str name " " (str/join "/" path) " " description))
                     (str/lower-case q))))

(defn- keep-row?
  [{:keys [types promoted? favorites? portfolios projects tags quick]} row]
  (and (or (empty? types) (contains? types (:family row)))
       (or (not promoted?) (:promoted? row))
       (or (not favorites?) (kompass/file-favorite? (files/fav-ref row)))
       (or (empty? portfolios) (contains? portfolios (get-in row [:portfolio :rid])))
       (or (empty? projects) (contains? projects (:project row)))
       (or (empty? tags) (some tags (:tags row)))
       (case quick
         :portfolios (some? (:portfolio row))
         :projects   (= :projects (:family row))
         :promoted   (:promoted? row)
         true)))

(defn- searched [sc] (filter #(matches-text? (:q (model/filters-of sc)) %) (:rows (list-of sc))))

(defn- filter-section [title open? & body]
  [ui/section {:title title :compact true :collapsible true
               :collapse-props {:default-is-open open?}}
   (into [ui/section-card] body)])

(defn- check-row [checked? on-change content cnt]
  [ui/checkbox {:checked (boolean checked?) :on-change on-change}
   content (when cnt [:span {:class style/small-muted} " " cnt])])

(defn- searchable-checks
  [_opts]
  (let [q (r/atom "") all? (r/atom false)]
    (fn [{:keys [placeholder items]}]
      (let [hits (filter #(str/includes? (str/lower-case (str (:text %)))
                                         (str/lower-case @q))
                         items)
            cut  (if @all? hits (take 5 hits))]
        [:<>
         [ui/input-group {:type "search" :small true :fill true :value @q
                          :placeholder placeholder
                          :on-change #(reset! q (.. % -target -value))}]
         (doall
          (for [{:keys [id text cnt checked? on-change]} cut]
            ^{:key (str id)} [check-row checked? on-change text cnt]))
         (when (> (count hits) 5)
           [ui/button {:minimal true :small true :intent :primary :on-click #(swap! all? not)}
            (i18n/t (if @all? :ui/kompass-show-fewer :ui/kompass-view-all)
                    {:n (count hits)})])]))))

(defn- active-count [{:keys [types promoted? favorites? portfolios projects tags]}]
  (+ (count types) (count portfolios) (count projects) (count tags)
     (if promoted? 1 0) (if favorites? 1 0)))

(defn- filter-panel []
  (when-let [sc (scope)]
    (let [fv      (model/filters-of sc)
          counts  (frequencies (map :family (searched sc)))
          tags    (model/all-tags)
          folios  (:portfolios @kompass/kompass-index)
          toggle! (fn [k v] #(send! :kompass.filter/toggle-in sc k v))]
      [:<>
       [page/toolbar
        [ui/icon {:icon :filter :size 14}]
        [:strong (i18n/t :ui/kompass-filters)]
        [ui/tag {:round true :minimal true} (active-count fv)]
        (when (pos? (active-count fv))
          [ui/button {:minimal true :small true :intent :primary
                      :on-click #(send! :kompass.filter/clear sc)}
           (i18n/t :ui/kompass-clear)])]
       [filter-section (i18n/t :ui/kompass-types) true
        [searchable-checks
         {:placeholder (i18n/t :ui/kompass-search-types)
          :items (for [[family n] (sort-by (comp - val) counts)]
                   {:id family :text (family-title family) :cnt n
                    :checked? (contains? (:types fv) family)
                    :on-change (toggle! :types family)})}]]
       [filter-section (i18n/t :ui/kompass-status) true
        [check-row (:promoted? fv) #(send! :kompass.filter/flip sc :promoted?)
         [:<> [ui/icon {:icon :tick-circle :size 14 :class "kompass-icon--promoted"}] " "
          (i18n/t :ui/kompass-promoted-items)] nil]
        [check-row (:favorites? fv) #(send! :kompass.filter/flip sc :favorites?)
         [:<> [ui/icon {:icon :star :size 14}] " " (i18n/t :ui/kompass-favorites)] nil]]
       [filter-section (i18n/t :ui/kompass-portfolios) true
        [searchable-checks
         {:placeholder (i18n/t :ui/kompass-search-portfolios)
          :items (for [p folios]
                   {:id (:rid p) :text (:name p)
                    :checked? (contains? (:portfolios fv) (:rid p))
                    :on-change (toggle! :portfolios (:rid p))})}]]
       [filter-section (i18n/t :ui/kompass-projects) false
        [searchable-checks
         {:placeholder (i18n/t :ui/kompass-search-projects)
          :items (for [pk (sort-by name (keys (catalog/project-list)))]
                   {:id pk :text (name pk)
                    :checked? (contains? (:projects fv) pk)
                    :on-change (toggle! :projects pk)})}]]
       [filter-section (i18n/t :ui/kompass-col-tags) true
        (if (seq tags)
          (doall (for [t tags]
                   ^{:key t} [check-row (contains? (:tags fv) t) (toggle! :tags t) t nil]))
          [:div {:class style/small-muted} (i18n/t :ui/kompass-no-tags)])]])))

(def ^:private filter-shown
  (reify IDeref (-deref [_] (some? (scope)))))

(defn- quick-filter-card [sc on id glyph title-k body-k]
  (let [on? (= id on)]
    [ui/card {:compact true :selected on?}
     [page/toolbar
      [ui/icon {:icon glyph :size 16}]
      [:strong (i18n/t title-k)]
      [ui/button {:minimal true :small true :intent :primary
                  :on-click #(send! :kompass.filter/set sc :quick (when-not on? id))}
       (i18n/t (if on? :ui/kompass-remove :ui/kompass-apply))]]
     [:div {:class style/small-muted} (i18n/t body-k)]]))

(defn- quick-filters [sc]
  (let [hidden? (model/at :quick-hidden?)
        on      (:quick (model/filters-of sc))]
    [ui/section {:title (i18n/t :ui/kompass-quick-filters) :compact true
                 :right-element [ui/button {:minimal true :small true :intent :primary
                                            :on-click #(send! :kompass/quick-hidden)}
                                 (i18n/t (if hidden? :ui/kompass-show :ui/kompass-hide))]}
     (when-not hidden?
       [ui/section-card {:class "kompass-quick"}
        [quick-filter-card sc on :portfolios :briefcase :ui/kompass-portfolios
         :ui/kompass-quick-portfolios]
        [quick-filter-card sc on :projects :projects :ui/kompass-projects
         :ui/kompass-quick-projects]
        [quick-filter-card sc on :promoted :tick-circle :ui/kompass-promoted-items
         :ui/kompass-quick-promoted]])]))

(defn- chips [sc fv]
  (let [chip (fn [k v text] {:key [k v] :text text
                             :on-remove #(send! :kompass.filter/toggle-in sc k v)})
        flag (fn [k text] {:key k :text text :on-remove #(send! :kompass.filter/flip sc k)})]
    (concat (for [t (:types fv)] (chip :types t (family-title t)))
            (when (:promoted? fv) [(flag :promoted? (i18n/t :ui/kompass-promoted-items))])
            (when (:favorites? fv) [(flag :favorites? (i18n/t :ui/kompass-favorites))])
            (for [p (:portfolios fv)]
              (chip :portfolios p (or (some #(when (= p (:rid %)) (:name %))
                                            (:portfolios @kompass/kompass-index))
                                      p)))
            (for [p (:projects fv)] (chip :projects p (name p)))
            (for [t (:tags fv)] (chip :tags t t)))))

(defn- tag-group
  [t]
  (first (str/split (str t) #":\s*" 2)))

(defn- by-tag
  [rows]
  (let [groups (reduce (fn [m r]
                         (if-let [ts (seq (distinct (map tag-group (:tags r))))]
                           (reduce #(update %1 %2 (fnil conj []) r) m ts)
                           (update m nil (fnil conj []) r)))
                       {} rows)]
    (concat (sort-by key (dissoc groups nil))
            (when-let [none (get groups nil)] [[nil none]]))))

(defn- grouped-tables [rows opts]
  (into [:<>]
        (for [[g rs] (by-tag rows)]
          ^{:key (str g)}
          [ui/section {:title (str (or g (i18n/t :ui/kompass-no-tag)) " · " (count rs))
                       :compact true}
           [table/file-table (assoc opts :rows rs)]])))

(defn- browser [sc]
  (let [{:keys [quick? notice group-tags? default-sort empty-text]} (list-of sc)
        fv    (model/filters-of sc)
        shown (filter #(keep-row? fv %) (searched sc))
        opts  {:rows shown :show-path? true :relative? true :portfolio? true
               :default-sort default-sort
               :empty-text (or empty-text (i18n/t :ui/kompass-no-match))}]
    [:<>
     (when notice [ui/callout {:compact true :intent :primary} notice])
     (when quick? [quick-filters sc])
     [ui-filter/bar {:value       (:q fv)
                     :placeholder (i18n/t :ui/kompass-search-all)
                     :on-search   #(send! :kompass.filter/set sc :q %)
                     :chips       (chips sc fv)}
      (when group-tags?
        [ui/checkbox {:checked (:grouped? fv) :label (i18n/t :ui/kompass-group-by-tag)
                      :on-change #(send! :kompass.filter/flip sc :grouped?)}])]
     (if (and group-tags? (:grouped? fv) (seq shown))
       [grouped-tables shown opts]
       ^{:key (pr-str sc)} [table/file-table opts])]))

(defn- card-grid [cards]
  (into [:div {:class "kompass-cards"}] cards))

(defn- resource-card [{:keys [icon title text sub on-click]}]
  [ui/card {:interactive true :compact true :on-click on-click}
   [ui/entity-title {:icon [ui/icon {:icon icon :size 22}] :title title :subtitle sub}]
   [:div {:class style/small-muted} text]])

(defn- crumbs [items & end]
  (into [page/toolbar [ui/breadcrumbs {:items items}]] end))

(defn- collection-page [{:keys [rid description] nm :name}]
  [:<>
   [crumbs [{:text (i18n/t :ui/kompass-collections) :on-click #(send! :kompass/collection nil)}
            {:text nm}]
    [ui/button {:outlined true :on-click (fn [] (model/delete-collection! rid)
                                           (send! :kompass/collection nil))}
     (i18n/t :ui/kompass-delete-collection)]]
   (when (seq description) [:p {:class style/small-muted} description])
   [browser [:collection rid]]])

(defn- data-catalog []
  (if-let [c (collection)]
    [collection-page c]
    (let [all (:collections @kompass/kompass-index)
          tab (or (model/at :catalog-tab) :collections)]
      [:<>
       [page/toolbar
        [ui/tabs {:id :kompass-catalog :selected-tab-id tab
                  :on-change #(send! :kompass/catalog-tab %)}
         (for [[id k] [[:collections :ui/kompass-collections] [:files :ui/kompass-files]]]
           [ui/tab {:id id :title (i18n/t k)}])]
        [ui/button {:outlined true :on-click #(table/open-dialog! :new-collection {})}
         (i18n/t :ui/kompass-new-collection)]]
       (cond
         (= :files tab) [browser [:catalog :files]]
         (empty? all)   [ui/empty-state {:icon  :folder-open
                                         :title (i18n/t :ui/kompass-collections-empty)}]
         :else (card-grid
                (for [{:keys [rid description refs] nm :name} all]
                  ^{:key rid}
                  [resource-card {:icon :tick-circle :title nm :text description
                                  :sub (i18n/t :ui/kompass-n-items {:n (count refs)})
                                  :on-click #(send! :kompass/collection rid)}])))])))

(defn- portfolio-page [p]
  (let [in-it? (set (map str (:projects p)))
        rows   (files/live-rows)
        ids    (into {} (for [[pk s] (catalog/project-list)] [pk (str (:id s))]))
        mine   (filter #(and (= :projects (:family %)) (in-it? (ids (:nm %)))) rows)
        pinned (filter #(and (:pinned? %) (in-it? (ids (:project %)))) rows)]
    [:<>
     [crumbs [{:text (i18n/t :ui/kompass-portfolios) :on-click #(send! :kompass/portfolio nil)}
              {:text (:name p)}]
      [ui/button
       {:outlined true :on-click #(table/open-dialog! :portfolio-projects {:portfolio p})}
       (i18n/t :ui/kompass-add-projects)]
      [ui/button {:outlined true :on-click (fn []
                                             (model/delete-portfolio! (:rid p))
                                             (send! :kompass/portfolio nil))}
       (i18n/t :ui/kompass-delete-portfolio)]]
     (when (seq (:description p)) [:p {:class style/small-muted} (:description p)])
     [table/file-table {:rows mine :relative? true :default-sort by-name
                        :empty-text (i18n/t :ui/kompass-portfolio-empty)}]
     [ui/section {:title (i18n/t :ui/kompass-portfolio-catalog) :compact true}
      [table/file-table {:rows pinned :relative? true :show-path? true :default-sort by-name
                         :empty-text (i18n/t :ui/kompass-portfolio-catalog-empty)}]]]))

(defn- portfolios-tab []
  (let [folios (:portfolios @kompass/kompass-index)
        rid    (model/at :portfolio)]
    (if-let [sel (some #(when (= rid (:rid %)) %) folios)]
      [portfolio-page sel]
      (if (empty? folios)
        [ui/empty-state {:icon :folder-open :title (i18n/t :ui/kompass-portfolios-empty)}]
        (card-grid
         (for [p folios]
           ^{:key (:rid p)}
           [resource-card {:icon :briefcase :title (:name p) :text (:description p)
                           :sub (i18n/t :ui/kompass-n-projects {:n (count (:projects p))})
                           :on-click #(send! :kompass/portfolio (:rid p))}]))))))

(defn- area-label [a] (i18n/t (some (fn [[id k]] (when (= id a) k)) tabs)))

(defn- head []
  (let [a (area)]
    [:<>
     [page/toolbar
      [ui/tabs {:id :kompass-area :selected-tab-id a :on-change #(send! :kompass/area %)}
       (for [[id label-k glyph icon-only?] tabs]
         [ui/tab {:id id :aria-label (i18n/t label-k)
                  :title [ui/tooltip {:content (i18n/t label-k)}
                          [:span [ui/icon {:icon glyph :size 15}]
                           (when-not icon-only? [:span " " (i18n/t label-k)])]]}])]]
     [page/header {:title (area-label a) :icon [ui/icon {:icon :document :size 15}]}
      (if (= :portfolios a)
        [ui/button {:intent :primary :icon :plus
                    :on-click #(table/open-dialog! :new-portfolio {})}
         (i18n/t :ui/kompass-new-portfolio)]
        [ui/button {:intent :primary :icon :plus
                    :on-click #(table/open-dialog! :new-project {})}
         (i18n/t :ui/kompass-new-project)])]]))

(defn- body []
  ^{:key (area)}
  [:<>
   (case (area)
     :catalog    [data-catalog]
     :portfolios [portfolios-tab]
     [browser (area)])])

(page/view! :kompass/head head)
(page/view! :kompass/filters filter-panel)
(page/view! :kompass/body body)

(def ^:private landing
  (page/werkzeug {:sidebar       :kompass/filters
                  :sidebar-label :ui/kompass-filters
                  :sidebar-when  filter-shown
                  :sidebar-w     216
                  :main          {:kind  :rows
                                  :box   {:grow? true}
                                  :items [(page/view :kompass/head)
                                          {:kind :flow :padding :compact :box {:grow? true}
                                           :items [(page/view :kompass/body)]}]}}))

(defn root []
  [:<>
   (if (:selected-project @shell/ui-state)
     ^{:key :project} [platform/module :kompass-project project/tree]
     ^{:key :landing} [platform/module :kompass landing])
   [table/dialogs]])
