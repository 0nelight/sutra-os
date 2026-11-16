(ns sutra-ui.apps.atlas.project
  (:require [sutra-ui.ui.core :as ui]
            [reagent.core :as r]
            [clojure.string :as str]
            [sutra.i18n.core :as i18n]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.apps.atlas.details :as details]
            [sutra-ui.apps.atlas.files :as files]
            [sutra-ui.apps.atlas.model :as model]
            [sutra-ui.apps.atlas.table :as ktable]
            [sutra-ui.resource.core :as res]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.atlas :as kompass]
            [sutra-ui.state.principals :as principals]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.ui.page :as page]
            [sutra-ui.ui.style :as style]
            [sutra-ui.ui.table :as table]))

(defn- empty-note [txt]
  [ui/empty-state {:icon :inbox :small? true :hint txt}])

(defn- rights [pkey] (get-in @kompass/kompass-index [:rights (when (keyword? pkey) (name pkey))]))

(defn- send! [& event] (model/send! (vec event)))

(defn- area-head
  [title hint & end]
  (into [page/header {:title title :subtitle hint}] end))

(defn- upload-all! [pkey folder file-list]
  (when (pos? (.-length file-list))
    (ktable/open-dialog! :upload {:pkey pkey :folder folder
                                 :files (vec (for [i (range (.-length file-list))]
                                               (.item file-list i)))})))

(defn- pinned-bar [pkey]
  (let [pins (files/project-pinned pkey)]
    [ui/section {:icon :pin :compact true :collapsible true
                 :collapse-props {:default-is-open false}
                 :title (i18n/t :ui/kompass-pinned)
                 :subtitle (i18n/t :ui/kompass-pinned-hint)
                 :right-element [:span {:class style/small-muted}
                                 (i18n/t :ui/kompass-n-items {:n (count pins)})]}
     (when (seq pins)
       [ui/section-card
        [ktable/file-table {:rows pins :default-sort {:col :name :dir :asc}}]])]))

(declare project-export-url)

(defn- selection-actions
  [pkey folder selected rows]
  (let [keys*  (set (map :key @selected))
        picked (filter #(contains? keys* (:key %)) rows)
        one    (when (= 1 (count picked)) (first picked))
        on-all (fn [f] (fn [] (doseq [r picked] (f r)) (reset! selected #{})))]
    (remove nil?
            [(when one [(i18n/t :ui/kompass-move) #(ktable/open-dialog! :move {:row one})])
             (when one [(i18n/t :ui/kompass-share) #(ktable/open-dialog! :share {:row one})])
             (when one [(i18n/t :ui/kompass-edit-tags) #(ktable/open-dialog! :tags {:row one})])
             (when (seq picked)
               [(i18n/t :ui/kompass-pin) (on-all #(model/toggle-pinned! (:handle %)))])
             (when (seq picked)
               [(i18n/t :ui/kompass-to-trash) (on-all #(model/trash-resource! (:handle %))) true])
             (when folder
               [(i18n/t :ui/kompass-add-description)
                #(ktable/open-dialog! :describe {:row {:handle [:folders folder]}})])
             [(i18n/t :ui/kompass-export-zip)
              #(js/window.open (project-export-url pkey "zip"))]
             [(i18n/t :ui/kompass-export-ent)
              #(js/window.open (project-export-url pkey "ent"))]])))

(defn- project-export-url
  [pkey fmt]
  (str "/api/kompass/export?format=" fmt
       (when-not (= pkey kompass/default-project)
         (str "&project=" (js/encodeURIComponent (name pkey))))))

(defn- new-menu [pkey folder]
  (let [input (atom nil)
        may?  (and (not= pkey kompass/default-project) (:update? (rights pkey)))]
    [:<>
     [:input {:type "file" :multiple true :hidden true :aria-hidden true
              :ref #(reset! input %)
              :on-change (fn [e]
                           (upload-all! pkey folder (.. e -target -files))
                           (set! (.. e -target -value) ""))}]
     [ktable/dropdown {:intent :primary :icon :plus :disabled (not may?)
                      :title (when-not may? (i18n/t :ui/kompass-new-needs-editor))}
      (i18n/t :ui/kompass-new)
      [[(i18n/t :ui/kompass-new-folder) #(ktable/open-dialog! :new-folder {:pkey pkey
                                                                          :folder folder})]
       [(i18n/t :ui/kompass-new-document) #(model/create-document! pkey)]
       [(i18n/t :ui/kompass-upload) #(some-> @input .click)]
       [(i18n/t :ui/kompass-new-exploration) #(shell/go! :object-workspace)]]]]))

(defn- files-area [_pkey _folder]
  (let [selected (r/atom #{})
        drag?    (r/atom false)]
    (fn [pkey folder]
      (let [rows (files/project-files pkey folder)
            doc  (when folder (model/annotation-doc [:folders folder]))]
        [:div {:class (when @drag? "kompass-drop")
               :on-drag-over #(do (.preventDefault %) (reset! drag? true))
               :on-drag-leave #(reset! drag? false)
               :on-drop (fn [e]
                          (.preventDefault e)
                          (reset! drag? false)
                          (when (not= pkey kompass/default-project)
                            (upload-all! pkey folder (.. e -dataTransfer -files))))}
         (when-not folder [pinned-bar pkey])
         (when (seq doc) [details/markdown doc])
         [area-head (i18n/t :ui/kompass-files) nil
          [ktable/dropdown {:outlined true} (i18n/t :ui/kompass-actions)
           (selection-actions pkey folder selected rows)]
          [new-menu pkey folder]]
         [ktable/file-table {:rows rows :selected selected :folders-first? true
                            :default-sort {:col :name :dir :asc}
                            :empty-text (files/files-empty-text)}]]))))

(defn- autosaved-area []
  [:<>
   [area-head (i18n/t :ui/kompass-autosaved) (i18n/t :ui/kompass-autosaved-hint)]
   [empty-note (i18n/t :ui/kompass-autosaved-empty)]])

(defn- catalog-area [pkey]
  (let [curated (files/project-curated pkey)]
    [:<>
     [area-head (i18n/t :ui/kompass-project-catalog) (i18n/t :ui/kompass-project-catalog-hint)]
     [ktable/file-table {:rows curated :default-sort {:col :name :dir :asc}
                        :empty-text (i18n/t :ui/kompass-project-catalog-empty)}]
     [:p {:class style/small-muted}
      (i18n/t :ui/kompass-catalog-summary {:curated (count curated)
                                           :total (files/project-count pkey)})]]))

(defn- remove-reference!
  [pkey declared to reasons]
  (model/confirm!
   {:title         (i18n/t :ui/kompass-remove-reference-title {:name (name to)})
    :body          (if (seq reasons)
                     (i18n/t :ui/kompass-remove-reference-used {:n (count reasons)})
                     (i18n/t :ui/kompass-remove-reference-unused))
    :confirm-label (i18n/t :ui/kompass-remove-reference)
    :danger?       true}
   [:kompass.project/imports pkey (vec (remove #{to} declared))]))

(defn- reference-rows
  [pkey deps]
  (let [me       (name pkey)
        declared (set (map keyword (get (:declared deps) (keyword me)
                                        (get (:declared deps) me))))
        edges    (filter #(= me (some-> (:from %) name)) (:edges deps))
        used     (into {} (map (juxt (comp keyword name :to) :reasons)) edges)]
    (sort-by (comp name :to)
             (for [to (into declared (keys used))]
               {:to to :declared? (contains? declared to) :reasons (get used to)}))))

(defn- project-link [pk]
  [:a {:href "#" :on-click #(do (.preventDefault %) (ktable/enter-project! pk))} (name pk)])

(defn- reference-cols [pkey declared]
  (let [may? (:manage? (rights pkey))]
    [{:label (i18n/t :ui/kompass-owning-project) :render #(project-link (:to %))}
     {:label  (i18n/t :ui/kompass-status)
      :render #(i18n/t (if (:declared? %) :ui/kompass-ref-declared :ui/kompass-ref-undeclared))}
     {:label  (i18n/t :ui/kompass-used-via)
      :render (fn [{:keys [reasons]}]
                (if (seq reasons)
                  [ui/section
                   {:title (i18n/t :ui/kompass-ref-uses {:n (count reasons)}) :collapsible true
                    :compact true :collapse-props {:default-is-open false}}
                   [ui/section-card
                    (into [:ul] (for [w reasons] [:li {:key w} w]))]]
                  [:span {:class style/small-muted} (i18n/t :ui/kompass-ref-unused)]))}
     {:label  ""
      :render (fn [{:keys [to declared? reasons]}]
                (when may?
                  (if declared?
                    [ui/button {:small true
                                :on-click #(remove-reference! pkey declared to reasons)}
                     (i18n/t :ui/kompass-remove-reference)]
                    [ui/button
                     {:small    true
                      :on-click #(model/set-project-imports! pkey (conj (vec declared) to))}
                     (i18n/t :ui/kompass-declare-reference)])))}]))

(defn- file-references-area [_pkey]
  (let [seen   (atom nil)
        check! (fn [_]
                 (let [schema @catalog/canonical-schema]
                   (when-not (identical? schema @seen)
                     (reset! seen schema)
                     (send! :kompass.deps/load))))]
    (r/create-class
     {:component-did-mount  check!
      :component-did-update check!
      :reagent-render
      (fn [pkey]
        (let [_        @catalog/canonical-schema
              rows     (reference-rows pkey (model/at :deps))
              declared (vec (keep #(when (:declared? %) (:to %)) rows))]
          [:<>
           [area-head (i18n/t :ui/kompass-file-references) (i18n/t :ui/kompass-references-hint)
            (when (:manage? (rights pkey))
              [ui/button
               {:intent   :primary :icon :plus
                :on-click #(ktable/open-dialog! :add-reference {:pkey pkey :declared declared})}
               (i18n/t :ui/kompass-add-reference)])]
           [table/data-table
            {:cols    (reference-cols pkey declared)
             :rows    rows
             :row-key :to
             :empty   [empty-note (i18n/t :ui/kompass-references-empty)]}]]))})))

(defn- package-types
  [pkey {:keys [object-types]}]
  (let [sch (:object-types @catalog/canonical-schema)]
    (filter #(= pkey (get-in sch [(keyword (name %)) :project])) object-types)))

(def ^:private external-references-area
  (machine/entering
   model/kompass [:kompass.packages/load]
   (fn [pkey]
     (let [rows (for [p (model/at :packages)
                      :let [ts (package-types pkey p)]
                      :when (and (:installed-version p) (seq ts))]
                  (assoc p :here ts))]
       [:<>
        [area-head (i18n/t :ui/kompass-external-references) (i18n/t :ui/kompass-external-hint)]
        [table/data-table
         {:cols    [{:label (i18n/t :ui/kompass-package) :render #(name (:id %))}
                    {:label (i18n/t :ui/kompass-version) :render #(str (:installed-version %))}
                    {:label  (i18n/t :ui/kompass-used-via)
                     :render #(str/join ", " (map name (:here %)))}]
          :rows    rows
          :row-key :id
          :empty   [empty-note (i18n/t :ui/kompass-external-references-empty)]}]]))))

(defn- trash-area [pkey]
  (let [trashed (files/project-trash pkey)]
    [:<>
     [area-head (i18n/t :ui/kompass-trash) (i18n/t :ui/kompass-trash-hint)]
     [ktable/file-table
      {:rows trashed :default-sort {:col :name :dir :asc}
       :empty-text (i18n/t :ui/kompass-trash-empty)
       :row-items (fn [row]
                    [[(i18n/t :ui/kompass-restore) #(model/restore-resource! (:handle row))]
                     (when-not (= :widget-sets (:family row))
                       [(i18n/t :ui/kompass-delete-forever)
                        #(ktable/confirm-delete-forever! row) true])])}]]))

(defn- scanner-area []
  [:<>
   [area-head (i18n/t :ui/kompass-scanner) (i18n/t :ui/kompass-scanner-hint)]
   [empty-note (i18n/t :ui/kompass-scanner-empty)]])

(defn- usage-area [pkey]
  (let [rows (files/project-files-anywhere pkey)]
    [:<>
     [area-head (i18n/t :ui/kompass-project-usage) (i18n/t :ui/kompass-usage-hint)]
     [table/data-table
      {:cols    [{:label (i18n/t :ui/type) :render #(res/family-label (first %))}
                 {:label (i18n/t :ui/kompass-count) :align :right :render second}]
       :rows    (sort-by (comp - val) (frequencies (map :family rows)))
       :row-key first}]]))

(defn- link-list [pks]
  (if (seq pks)
    (into [:ul] (for [pk pks] ^{:key pk} [:li [project-link pk]]))
    [:div {:class style/small-muted} "—"]))

(defn- access-graph-area [pkey proj]
  (let [upstream (distinct (keep :owner (files/references-for pkey)))
        readers  (files/readers-of pkey)]
    [:<>
     [area-head (i18n/t :ui/kompass-access-graph) nil]
     [:div {:class "kompass-cards"}
      [ui/card {:compact true}
       [:h4 (i18n/t :ui/kompass-graph-reads-from)] [link-list upstream]]
      [ui/card {:compact true :selected true}
       [:h4 (name pkey)]
       (into [:ul]
             (for [{:keys [principal role]} (:grants proj)]
               [:li {:key (str principal role)}
                (principals/principal-label principal) " · " (name role)]))]
      [ui/card {:compact true}
       [:h4 (i18n/t :ui/kompass-graph-read-by)] [link-list readers]]]]))

(defn- current []
  (let [pkey (:selected-project @shell/ui-state)]
    {:pkey pkey
     :proj (when (and pkey (not= pkey kompass/default-project))
             (-> (get (catalog/project-list) pkey) (assoc :project-name pkey)))}))

(defn- nav-item [tab id glyph label & [{:keys [sub? external?]}]]
  [ui/menu-item {:text label :active (= tab id) :aria-current (when (= tab id) "page")
                 :icon (when glyph [ui/icon {:icon glyph :size 16}])
                 :class (when sub? "kompass-nav__sub")
                 :label-element (when external? [ui/icon {:icon :share :size 14}])
                 :on-click #(send! :kompass/dashboard id)}])

(defn- navigation []
  (let [{:keys [pkey proj]} (current)
        tab (or (model/at :dashboard-tab) :files)]
    [:nav {:aria-label (i18n/t :ui/kompass-project)}
     (when-let [d (:description proj)]
       [ui/callout {:compact true} d])
     [ui/menu
      [ui/menu-divider {:title (i18n/t :ui/kompass-preview)}]
      [nav-item tab :cover :book (i18n/t :ui/kompass-cover-page-nav)]
      [ui/menu-divider {:title (i18n/t :ui/kompass-project)}]
      [nav-item tab :files :folder-close (i18n/t :ui/kompass-files)]
      [nav-item tab :autosaved :time (i18n/t :ui/kompass-autosaved)]
      [nav-item tab :project-catalog :tick-circle (i18n/t :ui/kompass-project-catalog)]
      [nav-item tab :file-references :link (i18n/t :ui/kompass-references)]
      [nav-item tab :file-references nil (i18n/t :ui/kompass-file-references) {:sub? true}]
      [nav-item tab :external-references nil (i18n/t :ui/kompass-external-references)
       {:sub? true}]
      [nav-item tab :trash :trash (i18n/t :ui/kompass-trash)]
      [nav-item tab :scanner :shield (i18n/t :ui/kompass-scanner)]
      [ui/menu-divider]
      (when (:manage? (rights pkey))
        [nav-item tab :usage :pie-chart (i18n/t :ui/kompass-project-usage) {:external? true}])
      [nav-item tab :access-graph :graph (i18n/t :ui/kompass-access-graph)
       {:external? true}]]]))

(defn- header []
  (let [{:keys [pkey proj]} (current)
        folder   (model/at :folder)
        default? (= pkey kompass/default-project)
        fav?     (and (not default?) (model/project-favorite? pkey))
        folio    (some #(when (some #{(:id proj)} (:projects %)) %)
                       (:portfolios @kompass/kompass-index))]
    [page/header
     {:title    (files/project-label pkey)
      :icon     [ui/icon {:icon :projects :size 15}]
      :subtitle (when (or folio folder)
                 [ui/breadcrumbs
                  {:items (concat
                           (when folio
                             [{:icon [ui/icon {:icon :briefcase :size 15}] :text (:name folio)}])
                           [{:text     (files/project-label pkey)
                             :on-click #(send! :kompass/dashboard :files)}]
                           (for [f (files/folder-chain folder)]
                             {:text (:name f) :on-click #(send! :kompass/folder (:rid f))}))}])}
     [ui/tooltip {:content (i18n/t :ui/kompass-all-files)}
      [ui/button {:minimal true :icon :folder-close :on-click ktable/leave-project!
                  :aria-label (i18n/t :ui/kompass-all-files)}]]
     (when-not default?
       [ui/tooltip
        {:content (i18n/t (if fav? :ui/kompass-unfavorite :ui/kompass-favorite))}
        [ui/button {:minimal true :icon (if fav? :star :star-empty)
                    :intent (when fav? :warning)
                    :on-click #(model/toggle-project-favorite! pkey)
                    :aria-label (i18n/t (if fav? :ui/kompass-unfavorite
                                          :ui/kompass-favorite))}]])
     [ui/tooltip {:content (i18n/t :ui/access)}
      [ui/button {:minimal true :icon :cog :on-click #(send! :kompass/details-tab :access)
                  :aria-label (i18n/t :ui/access)}]]]))

(defn- area []
  (let [{:keys [pkey proj]} (current)
        tab    (or (model/at :dashboard-tab) :files)
        folder (model/at :folder)]
    ^{:key (str pkey)}
    [:<>
     (case tab
       :cover               [details/documentation pkey proj]
       :autosaved           [autosaved-area]
       :project-catalog     [catalog-area pkey]
       :file-references     [file-references-area pkey]
       :external-references [external-references-area pkey]
       :trash               [trash-area pkey]
       :scanner             [scanner-area]
       :usage               [usage-area pkey]
       :access-graph        [access-graph-area pkey proj]
       ^{:key (str folder)} [files-area pkey folder])]))

(defn- details-pane []
  (let [{:keys [pkey proj]} (current)
        open (model/at :details-tab)]
    (when (and open proj) [details/panel pkey proj open])))

(defn- rail []
  (when (:proj (current)) [details/rail (model/at :details-tab)]))

(def ^:private details-open
  (reify
    IDeref (-deref [_] (when-let [t (and (:proj (current)) (model/at :details-tab))] [t]))
    IReset (-reset! [_ _] (send! :kompass/details-tab nil) nil)))

(def ^:private has-rail (reify IDeref (-deref [_] (some? (:proj (current))))))

(page/view! :kompass.project/nav navigation)
(page/view! :kompass.project/header header)
(page/view! :kompass.project/area area)
(page/view! :kompass.project/details details-pane)
(page/view! :kompass.project/rail rail)

(def tree
  (page/werkzeug
   {:sidebar       :kompass.project/nav
    :sidebar-label :ui/kompass-project
    :sidebar-w     220
    :main          {:kind  :rows
                    :box   {:grow? true}
                    :items [(page/view :kompass.project/header)
                            {:kind  :columns
                             :box   {:grow? true}
                             :items [(assoc
                                      (page/liste-detail
                                       {:list   {:kind :flow :padding :regular
                                                 :items [(page/view :kompass.project/area)]}
                                        :detail {:kind :rows :padding :compact
                                                 :items [(page/view :kompass.project/details)]}
                                        :open   details-open
                                        :detail-w 300
                                        :resize :kompass.project/details})
                                      :box {:grow? true})
                                     {:kind :rows :when has-rail :bg :2 :box {:w 42}
                                      :items [(page/view :kompass.project/rail)]}]}]}}))
