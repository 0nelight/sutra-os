(ns sutra-ui.apps.enta-editor.core
  (:require [sutra-ui.ui.busy :as busy]
            [sutra-ui.ui.core :as ui :refer [tag divider]]
            [sutra-ui.ui.inputs :as inputs]
            [clojure.string :as str]
            [reagent.core :as r]
            [sutra.capps.catalog :as cat]
            [sutra.capps.editing :as ed]
            [sutra-ui.routing :as routing]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.state.staging :as staging]
            [sutra-ui.apphost.sdk :as app]
            [sutra-ui.apphost.render :as render]
            [sutra-ui.apps.enta-editor.state :as es]
            [sutra-ui.apps.enta-editor.form :as form]
            [sutra-ui.apps.enta-editor.fields :as f]
            [sutra-ui.apps.enta-editor.names :as names]
            [sutra-ui.apps.enta-editor.new-app :as new-app]
            [sutra-ui.apps.enta-editor.tree :as tree]
            [sutra-ui.apps.enta-editor.overview :as overview]
            [sutra-ui.apps.enta-editor.ui :as eui]
            [sutra-ui.apps.enta-editor.view-as :as view-as]
            [sutra-ui.apps.enta-editor.view-preview :as vp]
            [sutra-ui.apphost.platform :as platform]
            [sutra-ui.ui.size :as groesse]
            [sutra-ui.ui.page :as page]
            [sutra-ui.ui.hotkeys :as hotkeys]
            [sutra-ui.resource.versions :as versions]
            [sutra.i18n.core :as i18n]))

(defn- app-groups
  [query]
  (let [needle (str/lower-case (str/trim query))
        hit?   (fn [{:keys [id label]}]
                 (or (str/blank? needle)
                     (str/includes? (str/lower-case label) needle)
                     (str/includes? (name id) needle)))
        all    (filter hit? (es/app-options))
        by-id  (into {} (map (juxt :id identity)) (es/app-options))]
    {:recent (when (str/blank? needle) (keep by-id (es/recent-apps)))
     :apps   (remove :view? all)
     :views  (filter :view? all)}))

(defn- app-list
  [_on-pick]
  (let [q (r/atom "")]
    (fn [on-pick]
      (let [{:keys [recent apps views]} (app-groups @q)
            in (fn [g xs] (map #(assoc % ::group g) xs))]
        [ui/query-list
         {:query              @q
          :on-query-change    (fn [s _] (reset! q s))
          :items              (vec (concat (in "Zuletzt geöffnet" recent) (in "Apps" apps)
                                           (in "Objektansichten" views)))
          :items-equal        (fn [a b] (= ((juxt ::group :id) a) ((juxt ::group :id) b)))
          :item-renderer      (fn [{:keys [id label]} {:keys [handle-click modifiers ref]
                                                       :as   o}]
                                [ui/menu-item {:id             (:id o)
                                               :ref            ref
                                               :role-structure :listoption
                                               :active         (:active modifiers)
                                               :selected       (= id (:app @es/draft))
                                               :text           label
                                               :on-click       handle-click}])
          :item-list-renderer
          (fn [{:keys [filtered-items render-item items-parent-ref menu-props]}]
            (into [ui/menu (merge {:role "listbox"} menu-props
                                  {:ref items-parent-ref :class "ab-applist__scroll"})]
                  (if (empty? filtered-items)
                    [[ui/menu-item {:disabled true :text "Keine App gefunden."}]]
                    (mapcat (fn [[[_ x] :as xs]]
                              (cons ^{:key (::group x)} [ui/menu-divider {:title (::group x)}]
                                    (map (fn [[i y]] (render-item y i)) xs)))
                            (partition-by (comp ::group second)
                                          (map-indexed vector filtered-items))))))
          :on-item-select     (fn [{:keys [id]} _] (on-pick id))
          :renderer
          (fn [{:keys [item-list handle-key-down handle-key-up]}]
            [:div {:class "ab-applist" :on-key-up handle-key-up}
             [inputs/text-input q {:type        "search"
                                   :left-icon   :search
                                   :auto-focus  true
                                   :placeholder "App suchen…"
                                   :on-key-down handle-key-down}]
             item-list])}]))))

(defn- app-switcher []
  (let [{:keys [app]} @es/draft
        label (if (es/file) (names/app-label (es/file)) "App öffnen…")]
    [eui/popover {:width "320px"}
     (fn [open?]
       [ui/tooltip {:content (if app (name app) "eine App öffnen")}
        [ui/button {:minimal true :active open? :end-icon :caret-down}
         label]])
     (fn [close!]
       [:<>
        [app-list (fn [id] (close!) (es/load-app! id))]
        [divider]
        (into [ui/menu]
              [[ui/menu-item {:text "Neue App…"
                              :icon :plus
                              :on-click #(new-app/open-gallery!)}]])])]))

(defn- all-problems
  []
  (if (nil? (es/tree))
    []
    (into (mapv #(assoc % :label (names/node-label (es/tree) (cat/node-at (es/tree) (:path %))))
                (es/problems))
          (for [{:keys [type name error]} (overview/problems)]
            {:derived [type name]
             :label (str (f/type-label type) " · " (clojure.core/name name))
             :error error}))))

(defn- problems-chip []
  (let [probs (all-problems)]
    (when (seq probs)
      [eui/popover {:align :right :width "360px"}
       (fn [_]
         [ui/tooltip {:content "Stellen, die noch etwas brauchen"}
          [tag {:minimal true :interactive true :intent :warning :icon :warning-sign}
           (count probs)]])
       (fn [_close!]
         [:div {:class "ab-menu"}
          (into [ui/menu]
                (cons [ui/menu-divider
                       {:title (str (count probs) " Stelle(n) brauchen noch etwas")}]
                      (for [{:keys [path derived label error]} probs]
                        [ui/menu-item
                         {:text [:<> [:div label] [:div {:class "sutra-menu__sub"} error]]
                          :multiline true
                          :on-click #(if-let [[t nm] derived]
                                       (overview/focus-derived! t nm)
                                       (do (es/ui-set! {:pane :preview :left :struktur})
                                           (es/select! path)))}])))])])))

(defn- save-button []
  (let [{:keys [staged error loading?]} @es/draft
        dirty? (es/dirty?)
        auto?  (true? (:auto-publish (es/file)))]
    [:<>
     (cond
       error    [ui/tooltip {:content error}
                 [tag {:minimal true :intent :danger} "Fehler beim Speichern"]]
       loading? [busy/inline {:size 12 :class "ab-muted"} "speichert…"]
       (and staged (not dirty?))
       [ui/tooltip {:content (i18n/t :ui/app-builder-staged
                                     {:created (or (:created staged) 0)
                                      :updated (or (:updated staged) 0)})}
        [tag {:minimal true :icon :tick} "geplant"]])
     [ui/tooltip {:content (if dirty?
                             (i18n/t :ui/app-builder-save-hint)
                             "Keine ungespeicherten Änderungen")}
      [ui/button {:intent   (when dirty? :primary)
                  :disabled (not dirty?)
                  :on-click #(es/stage!)}
       (cond (not dirty?) "Gespeichert"
             auto?        "Speichern & veröffentlichen"
             :else        "Speichern")]]]))

(defn- more-menu []
  (let [pending (count (staging/staged-list))
        k       (es/app-key)]
    [eui/popover {:align :right :width "260px"}
     (fn [open?]
       [eui/icon-button {:icon :more :title "Weitere" :active? open?}])
     (fn [_close!]
       [:div {:class "ab-menu"}
        (into [ui/menu]
              [[ui/menu-item {:text "Versionen"
                              :disabled (nil? (es/tree))
                              :on-click #(es/ui-set! {:pane :versions})}]
               [ui/menu-item {:text "Als .enta-Text ansehen"
                              :disabled (nil? (es/tree))
                              :on-click #(es/ui-set! {:pane :enta})}]
               [ui/menu-item
                {:text "Entwicklungsfassung in neuem Tab"
                 :disabled (nil? (:app @es/draft))
                 :on-click #(js/window.open
                             (routing/dev-app-href (:current-branch @shell/ui-state) (name k))
                             "_blank" "noopener")}]
               [ui/menu-divider]
               [ui/menu-item {:text "Geplante Änderungen"
                              :label (when (pos? pending) (str pending))
                              :on-click #(shell/open-staging!)}]])])]))

(defn- header []
  [page/header {:title [app-switcher]}
   [eui/icon-button {:icon :plus :label "Neu" :title "Neue App aus Vorlage"
                     :on-click new-app/open-gallery!}]
   [eui/icon-button {:icon :undo
                     :title (if-let [l (es/undo-label)] (str "Rückgängig: " l) "Rückgängig")
                     :disabled? (not (es/can-undo?)) :on-click #(es/undo!)}]
   [eui/icon-button {:icon :redo
                     :title (if-let [l (es/redo-label)] (str "Wiederholen: " l) "Wiederholen")
                     :disabled? (not (es/can-redo?)) :on-click #(es/redo!)}]
   (when (:loading? @es/draft)
     [busy/inline {:size 12 :class "ab-muted"} "lädt…"])
   [problems-chip]
   [save-button]
   [more-menu]
   [new-app/gallery]
   [new-app/dialog]])

(def ^:private rail-items
  [[:struktur :list-detail-view "Struktur — Aufbau der Seite"]
   [:daten :database "Daten — Objekttypen, Variablen, abgeleitete Eigenschaften"]
   [:app :cog "App — Name, Status, Veröffentlichung"]])

(defn- rail-button [[k icon title] large?]
  [ui/tooltip {:content title}
   [ui/button {:minimal true :large large? :icon icon :active (= k (es/left))
               :aria-label title
               :on-click #(es/ui-set! {:left k :pane :preview})}]])

(defn- rail []
  (into [ui/button-group {:vertical true :minimal true}] (map #(rail-button % true)) rail-items))

(defn- left-panel []
  [:<>
   (when (groesse/kompakt?)
     (into [page/toolbar] (map #(rail-button % false)) rail-items))
   (case (es/left)
     :daten [overview/data-panel]
     :app   [overview/app-panel]
     [tree/outline-panel])])

(defn- data-source-chip
  [src err]
  (let [[intent txt title]
        (case src
          :draft     [:success "live" (i18n/t :ui/app-builder-preview-live)]
          :loading   [nil "lädt…" (i18n/t :ui/app-builder-preview-loading)]
          :published [:warning "veröffentlichte Fassung"
                      (i18n/t :ui/app-builder-preview-published {:reason (str err)})]
          [:warning "keine Daten"
           (i18n/t :ui/app-builder-preview-no-data {:reason (str (or err "–"))})])]
    [:span {:class "enta-preview-source" :data-source (name src)}
     [ui/tooltip {:content title}
      [tag {:minimal true :intent intent} txt]]
     [eui/icon-button {:icon :refresh :title (i18n/t :ui/app-builder-preview-refresh)
                       :disabled? (= :loading src) :on-click #(es/refresh-preview!)}]]))

(defn- data-source
  []
  (r/create-class
   {:display-name "data-source"
    :component-did-mount    (fn [_] (es/preview-shown! true))
    :component-will-unmount (fn [_] (es/preview-shown! false))
    :reagent-render         (fn [] [data-source-chip (es/preview-source) (es/preview-error)])}))

(defn- start-screen []
  [:div {:class "ab-start"}
   [:div {:class "ab-start__col"}
    [:div {:class "ab-empty__title"} "App öffnen"]
    [app-list es/load-app!]]
   [:div {:class "ab-start__col"}
    [:div {:class "ab-empty__title"} "Neu anfangen"]
    [new-app/template-cards (fn [])]]])

(defn- canvas
  []
  (let [{:keys [data tree version]} @es/draft
        as (view-as/active)]
    (if-not (app/data-layout? tree)
      [ui/empty-state {:icon :application
                       :hint (str "Diese App hat kein Daten-:layout (sie wird aus :source "
                                  "interpretiert) — der Editor kann sie nicht darstellen.")}]
      (let [{ot :object-type ff :form-factor} (es/file)
            ot     (some-> ot name keyword)
            ff     (some-> ff name keyword)
            k      (es/app-key)
            panel? (contains? #{:panel :panel-set} ff)]
        [:div {:class "ab-canvas"}
         [page/toolbar
          [vp/size-picker k panel?]
          (when (and ot ff) [vp/object-picker k ot])
          [view-as/picker]
          (when-not ot [data-source])
          (when (groesse/kompakt?)
            [ui/button {:icon :properties :on-click #(es/ui-set! {:inspect? true})}
             "Eigenschaften"])]
         [vp/sized k panel?
          (cond
            (and ot ff)
            [vp/object-view k ot ff (view-as/descriptor)]

            (nil? (:id (:consumer @es/draft)))
            (if (= :loading (es/preview-source))
              [busy/block "Vorschau wird geladen …"]
              [ui/empty-state {:icon :eye-off :title (or (es/preview-error) "Keine Vorschau")}])

            :else
            ^{:key (str version "/" as "/" (:id (:consumer @es/draft)))}
            [:div {:class "consumer__app enta-inspect ab-canvas__app"}
             [app/render-data-app (view-as/descriptor) (or data {})]])]]))))

(defn- back-bar [title]
  [page/toolbar
   [ui/button {:icon :arrow-left :on-click #(es/ui-set! {:pane :preview})} "zurück zum Editor"]
   [:span {:class "ab-panel-title"} title]])

(defn- enta-view []
  [:div {:class "ab-canvas"}
   [back-bar ".enta"]
   [page/toolbar
    [ui/button {:icon :duplicate :on-click #(es/copy-enta!)} "Kopieren"]
    [:span {:class "ab-muted"} (i18n/t :ui/app-builder-enta-hint)]]
   [:pre {:class "ab-enta"} (es/enta-text)]])

(defn- versions-view []
  (let [k (es/app-key)]
    [:div {:class "ab-canvas"}
     [back-bar (i18n/t :ui/app-builder-versions)]
     [:div {:class "ab-versions"}
      ^{:key (name k)}
      [versions/panel {:section "applications" :name (name k)
                       :on-restore (fn [{:keys [spec]}] (es/restore-file! spec))}]]]))

(defn- main-view []
  (case (es/pane)
    :enta     [enta-view]
    :versions [versions-view]
    [canvas]))

(def ^:private editor-keys
  (into [{:combo "esc" :label [i18n/t :ui/hotkeys-cancel] :group "App-Builder"
          :prevent-default true
          :on-key-down (fn [_] (es/mode! :cancel))}]
        (for [{:keys [keys label group]} ed/intents, k keys]
          {:combo       (str/replace k #"^ctrl\+" "mod+")
           :label       label
           :group       (str "App-Builder · " (str/capitalize (name group)))
           :on-key-down (fn [e] (when (es/on-chord! k) (.preventDefault e)))})))

(defn- pick-on-canvas! [path]
  (es/ui-set! {:left :struktur})
  (es/select! path))

(defn- sync-selection! []
  (render/inspect-selected! (get-in @es/ui [:draft :cursor])))

(def ^:private inspector-open
  (reify
    IDeref
    (-deref [_]
      (when (and (= :preview (es/pane))
                 (or (not (groesse/kompakt?)) (es/ui-get :inspect?)))
        [:inspector]))
    IReset
    (-reset! [_ _] (es/ui-set! {:inspect? false}) nil)))

(page/view! :app-builder/header header)
(page/view! :app-builder/start start-screen)
(page/view! :app-builder/rail rail)
(page/view! :app-builder/left left-panel)
(page/view! :app-builder/main main-view)
(page/view! :app-builder/inspector (fn [] [form/inspector]))

(def ^:private start-tree
  {:kind  :rows
   :fill? true
   :items [(page/view :app-builder/header)
           {:kind :flow :box {:grow? true} :items [(page/view :app-builder/start)]}]})

(def ^:private editor-tree
  {:kind  :rows
   :fill? true
   :items [(page/view :app-builder/header)
           (page/werkzeug
            {:rail          :app-builder/rail
             :sidebar       :app-builder/left
             :sidebar-label "Struktur"
             :sidebar-w     280
             :main          (page/liste-detail {:list     :app-builder/main
                                                :detail   :app-builder/inspector
                                                :open     inspector-open
                                                :detail-w 320
                                                :resize   :app-builder/inspector})})]})

(defn- surface []
  (let [loaded? (some? (es/tree))]
    ^{:key loaded?}
    [platform/module :app-builder (if loaded? editor-tree start-tree)]))

(defn app-builder []
  (r/create-class
   {:display-name "app-builder"
    :component-did-mount
    (fn [^js this]
      (render/inspect! {:select pick-on-canvas! :selected (es/cursor)})
      (set! (.-selectionTrack this) (r/track! sync-selection!)))
    :component-will-unmount
    (fn [^js this]
      (some-> (.-selectionTrack this) r/dispose!)
      (render/inspect! nil))
    :reagent-render (fn [] [:<> [hotkeys/global {:hotkeys editor-keys}] [surface]])}))
