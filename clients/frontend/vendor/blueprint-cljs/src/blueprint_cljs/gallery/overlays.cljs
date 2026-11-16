(ns blueprint-cljs.gallery.overlays
  (:require [reagent.core :as r]
            [blueprint-cljs.alert :as alert]
            [blueprint-cljs.button :as button]
            [blueprint-cljs.context-menu :as context-menu]
            [blueprint-cljs.dialog :as dialog]
            [blueprint-cljs.drawer :as drawer]
            [blueprint-cljs.hotkeys :as hotkeys]
            [blueprint-cljs.menu :as menu]
            [blueprint-cljs.overlay :as overlay]
            [blueprint-cljs.popover :as popover]
            [blueprint-cljs.toast :as toast]
            [blueprint-cljs.tooltip :as tooltip]))

(defn- row [& children]
  (into [:div {:style {:display "flex" :gap 16 :flex-wrap "wrap" :align-items "center"}}]
        children))

(defn- file-menu []
  [menu/menu
   [menu/menu-item {:icon :new-text-box :text "New text box"}]
   [menu/menu-item {:icon :new-object :text "New object"}]
   [menu/menu-item {:icon :new-link :text "New link"}]
   [menu/menu-divider]
   [menu/menu-item {:icon :cog :text "Settings…" :label "⌘,"}]])

(defn- popover-demo []
  [:div {:style {:display "flex" :flex-direction "column" :gap 24}}
   [row
    [popover/popover {:content [:div {:style {:padding 20}} [:h5.bp-heading "Popover"]
                                [:p "Content with an arrow, open by default."]
                                [button/button {:class popover/dismiss-class :text "Dismiss"}]]
                      :placement :bottom :default-is-open true}
     [button/button {:intent :primary :text "Popover (bottom)"}]]
    [:div {:style {:width 240}}]
    [popover/popover {:content [file-menu] :placement :bottom-start :minimal true
                      :default-is-open true}
     [button/button {:right-icon :caret-down :text "Minimal menu"}]]]
   [:div {:style {:height 150}}]
   [row
    [popover/popover {:content [:div {:style {:padding 12}} "Hover popover"]
                      :interaction-kind :hover :placement :right}
     [button/button {:text "Hover"}]]
    [popover/popover {:content [:div {:style {:padding 12}} "Matches the target width"]
                      :match-target-width true :minimal true :placement :bottom}
     [button/button {:text "Match target width (click)" :style {:width 260}}]]
    [popover/popover {:content [:div {:style {:padding 12}} "Disabled"] :disabled true}
     [button/button {:text "Disabled popover"}]]
    [popover/popover {:content [:div {:style {:padding 12}} "With backdrop"]
                      :has-backdrop true}
     [button/button {:text "Backdrop"}]]]])

(defn- tooltip-demo []
  [:div {:style {:display "flex" :flex-direction "column" :gap 50 :padding "40px 0"}}
   [row
    [tooltip/tooltip {:content "Default tooltip" :default-is-open true}
     [button/button {:text "Default"}]]
    [tooltip/tooltip {:content "Primary" :intent :primary :default-is-open true
                      :placement :bottom}
     [button/button {:text "Primary (bottom)"}]]
    [tooltip/tooltip {:content "Compact" :compact true :default-is-open true}
     [button/button {:text "Compact"}]]
    [tooltip/tooltip {:content "Danger, right" :intent :danger :placement :right
                      :default-is-open true}
     [button/button {:text "Danger"}]]]
   [row
    [:span "Hover the "
     [tooltip/tooltip {:content "I am an indicator"}
      [:span.bp-tooltip-indicator "dotted text"]] "."]
    [tooltip/tooltip {:content "Success" :intent :success}
     [button/button {:text "Hover me"}]]
    [tooltip/tooltip {:content "Warning" :intent :warning :minimal true}
     [button/button {:text "Minimal"}]]]])

(defn- menu-demo []
  [row
   [:div {:style {:align-self "flex-start"}}
    [menu/menu
     [menu/menu-divider {:title "Edit"}]
     [menu/menu-item {:icon :cut :text "Cut" :label "⌘X"}]
     [menu/menu-item {:icon :duplicate :text "Copy" :label "⌘C"}]
     [menu/menu-item {:icon :clipboard :text "Paste" :label "⌘V" :disabled true}]
     [menu/menu-divider {:title "Text"}]
     [menu/menu-item {:icon :align-left :text "Alignment"}
      [menu/menu-item {:icon :align-left :text "Left"}]
      [menu/menu-item {:icon :align-center :text "Center"}]
      [menu/menu-item {:icon :align-right :text "Right"}]]
     [menu/menu-item {:icon :style :text "Style"}
      [menu/menu-item {:icon :bold :text "Bold"}]
      [menu/menu-item {:icon :italic :text "Italic"}]]
     [menu/menu-divider]
     [menu/menu-item {:icon :layers :text "Active" :active true}]
     [menu/menu-item {:icon :trash :text "Delete" :intent :danger}]]]
   [:div {:style {:align-self "flex-start"}}
    [menu/menu {:small true}
     [menu/menu-item {:text "Small menu" :icon :small-cross}]
     [menu/menu-item {:text "Primary" :intent :primary}]
     [menu/menu-item {:text "Success" :intent :success}]
     [menu/menu-item {:text "Warning active" :intent :warning :active true}]]]
   [:div {:style {:align-self "flex-start"}}
    [menu/menu {:large true :role "listbox"}
     [menu/menu-item {:text "Selectable" :role-structure :listoption :selected true}]
     [menu/menu-item {:text "Option B" :role-structure :listoption :selected false}]
     [menu/menu-item {:text "A very long menu item text that ellipsizes" :style {:max-width 220}
                      :role-structure :listoption}]]]
   [:div {:style {:align-self "flex-start" :min-width 420 :min-height 200}}
    [menu/menu
     [menu/menu-item {:icon :folder-open :text "Open recent"
                      :popover-props {:default-is-open true}}
      [menu/menu-item {:icon :document :text "report.pdf"}]
      [menu/menu-item {:icon :document :text "notes.md"}]]
     [menu/menu-item {:icon :export :text "Export"}]]]])

(defn- dialog-demo []
  (let [open? (r/atom true)]
    (fn []
      [:div
       [button/button {:text "Open dialog" :on-click #(reset! open? true)}]
       [dialog/dialog {:is-open @open? :on-close #(reset! open? false) :icon :info-sign
                       :title "Palantir" :use-portal false}
        [dialog/dialog-body
         [:p [:strong "Data integration is the seminal problem of the digital age."]]
         [:p "the reference design gives you the power to integrate, analyse and act on your data."]]
        [dialog/dialog-footer
         {:actions [:<> [button/button {:text "Close" :on-click #(reset! open? false)}]
                    [button/button {:intent :primary :text "Visit"}]]}
         "Footer note"]]])))

(defn- multistep-demo []
  (let [open? (r/atom true)]
    (fn []
      [:div
       [button/button {:text "Open multistep dialog" :on-click #(reset! open? true)}]
       [dialog/multistep-dialog {:is-open @open? :on-close #(reset! open? false)
                                 :title "Multistep dialog" :icon :info-sign
                                 :use-portal false :show-close-button-in-footer true}
        [dialog/dialog-step {:id :select :title "Select"
                             :panel [dialog/dialog-body [:p "Pick a response."]]}]
        [dialog/dialog-step {:id :confirm :title "Confirm"
                             :panel [dialog/dialog-body [:p "Confirm it."]]}]
        [dialog/dialog-step {:id :done :title "Done"
                             :panel [dialog/dialog-body [:p "All set."]]}]]])))

(defn- drawer-demo []
  (let [open? (r/atom true)
        pos   (r/atom :right)]
    (fn []
      [:div
       [row
        (for [p [:right :left :top :bottom]]
          ^{:key p}
          [button/button {:text (str "Open " (name p))
                          :on-click #(do (reset! pos p) (reset! open? true))}])]
       [drawer/drawer {:is-open @open? :on-close #(reset! open? false) :position @pos
                       :size :small :icon :info-sign :title "Drawer"}
        [:div.bp-drawer-body
         [:div {:style {:padding 20}} [:p "Drawer content."] [:p "Press ESC to close."]]]
        [:div.bp-drawer-footer "Footer"]]])))

(defn- alert-demo []
  (let [open? (r/atom true)]
    (fn []
      [:div
       [button/button {:text "Open alert" :intent :danger :on-click #(reset! open? true)}]
       [alert/alert {:is-open @open? :intent :danger :icon :trash
                     :confirm-button-text "Move to Trash" :cancel-button-text "Cancel"
                     :can-escape-key-cancel true :use-portal false
                     :on-close #(reset! open? false)}
        [:p "Are you sure you want to move " [:b "filename"] " to Trash?"]]])))

(defn- toast-demo []
  (let [api (atom nil)
        n   (atom 0)]
    (fn []
      [:div
       [row
        [button/button {:text "Default toast"
                        :on-click #(toast/show! @api {:message (str "Toast " (swap! n inc))})}]
        (for [i [:primary :success :warning :danger]]
          ^{:key i}
          [button/button {:intent i :text (name i)
                          :on-click #(toast/show! @api {:message (str (name i) " toast")
                                                        :intent i :icon :info-sign})}])
        [button/button {:text "Clear" :on-click #(toast/clear! @api)}]]
       [:div {:style {:position "relative" :height 300 :overflow "hidden" :margin-top 12
                      :border "1px dashed rgba(128,128,128,.4)"}}
        [toast/overlay-toaster
         {:use-portal false :position :top
          :toaster-ref (fn [a]
                         (reset! api a)
                         (js/setTimeout
                          (fn []
                            (toast/show! a {:message "Saved." :intent :success :icon :tick
                                            :timeout 0})
                            (toast/show! a {:message "Copied link." :icon :link :timeout 0
                                            :action {:text "Undo"}}))
                          0)
                         nil)}]]])))

(defn- context-menu-demo []
  [:div {:style {:display "flex" :gap 24}}
   [context-menu/context-menu
    {:content [file-menu]
     :style {:width 260 :height 120 :display "flex" :align-items "center"
             :justify-content "center" :border "1px dashed rgba(128,128,128,.6)"}}
    "Right-click here"]
   [:div {:style {:position "relative" :width 260 :height 220}}
    [context-menu/context-menu-popover
     {:is-open true :target-offset {:left 520 :top 150} :content [file-menu]
      :use-portal false}]]])

(defn- hotkeys-demo []
  (let [log (r/atom [])]
    (fn []
      [hotkeys/hotkeys-target
       {:hotkeys [{:combo "shift + a" :global true :label "Say A"
                   :on-key-down #(swap! log conj "shift+a")}
                  {:combo "mod + s" :global true :label "Save" :prevent-default true
                   :on-key-down #(swap! log conj "save")}
                  {:combo "r" :group "Local" :label "Refresh (focus box)"
                   :on-key-down #(swap! log conj "r")}]
        :style {:padding 12 :border "1px dashed rgba(128,128,128,.6)" :max-width 520}}
       [:p "Press " [:kbd.bp-key "?"] " for the hotkeys dialog; shift+a, mod+s globally, "
        "r with focus here."]
       [row [hotkeys/key-combo-tag {:combo "mod + shift + p"}]
        [hotkeys/key-combo-tag {:combo "alt + enter"}]
        [hotkeys/key-combo-tag {:combo "mod + k" :minimal true}]]
       [:p "Log: " (pr-str @log)]
       [:div {:style {:margin-top 12}}
        [hotkeys/hotkeys-list {:hotkeys [{:combo "mod+s" :label "Save" :global true}
                                         {:combo "shift+?" :label "Help" :global true}
                                         {:combo "up" :label "Move up" :group "Table"}]}]]])))

(defn- overlay-demo []
  (let [open? (r/atom false)]
    (fn []
      [:div
       [button/button {:text "Open raw overlay" :on-click #(reset! open? true)}]
       [overlay/overlay {:is-open @open? :on-close #(reset! open? false)
                         :class "bp-overlay-scroll-container"}
        [:div {:class "bp-card bp-elevation-4"
               :style {:left "calc(50vw - 200px)" :top 100 :width 400 :padding 20
                       :background "var(--white)"}}
         [:h3.bp-heading "Overlay"] [:p "Backdrop, focus trap, ESC to close."]
         [button/button {:intent :danger :text "Close" :on-click #(reset! open? false)}]]]])))

(def sections
  "Each: {:id kw :title str :demo (fn [] hiccup)}."
  [{:id :popover :title "Popover" :demo popover-demo}
   {:id :tooltip :title "Tooltip" :demo tooltip-demo}
   {:id :menu :title "Menu" :demo menu-demo}
   {:id :overlay :title "Overlay" :demo overlay-demo}
   {:id :dialog :title "Dialog" :demo dialog-demo}
   {:id :multistep-dialog :title "MultistepDialog" :demo multistep-demo}
   {:id :drawer :title "Drawer" :demo drawer-demo}
   {:id :alert :title "Alert" :demo alert-demo}
   {:id :toast :title "Toast" :demo toast-demo}
   {:id :context-menu :title "ContextMenu" :demo context-menu-demo}
   {:id :hotkeys :title "Hotkeys" :demo hotkeys-demo}])
