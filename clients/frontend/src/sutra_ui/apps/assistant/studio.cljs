(ns sutra-ui.apps.assistant.studio
  (:require [clojure.string :as str]
            [sutra-ui.ui.core :as ui]
            [sutra-ui.ui.page :as page]
            [reagent.core :as r]
            [sutra.i18n.core :as i18n]
            [sutra-ui.apps.assistant.studio-events :as se]
            [sutra-ui.editor.code :refer [code-editor]]
            [sutra-ui.resource.versions :as versions]
            [sutra-ui.state.machine :as machine]))

(defonce ^:private ui (r/atom se/initial))

(defonce ^:private studio (machine/frame ui {:name :assistant/studio}))

(defn- send! [event] (machine/dispatch event {:frame studio}))

(machine/reg-sub :studio/db (fn [db _] db))

(defonce ^:private db (machine/subscribe [:studio/db] {:frame studio}))

(def ^:private new-agent-form
  (machine/entering
   studio [:studio.projects/load]
   (fn [model]
     (let [{:keys [projects project new-name]} @db]
       [:form {:class "studio__new"
               :on-submit (fn [e] (.preventDefault e) (send! [:studio/create model]))}
        [:div {:class "studio__new-row"}
         [ui/input-group {:value new-name :placeholder (i18n/t :ui/studio-new-name)
                          :on-change #(send! [:studio/new-name (.. % -target -value)])}]
         [ui/button {:icon :plus :type "submit"
                     :disabled (or (str/blank? new-name) (nil? project))}]]
        [:label {:class "studio__new-project"} (i18n/t :ui/studio-project)
         [ui/html-select {:fill true :value (or project "")
                          :on-change #(send! [:studio/project (.. % -target -value)])}
          (for [p projects] ^{:key p} [:option {:value p} p])]]]))))

(defn- result-line [{:keys [preview? staged]}]
  [:div {:class "studio__result"}
   (if (seq staged)
     [:<>
      [:div (i18n/t (if preview? :ui/studio-would-change :ui/studio-staged))]
      [:ul (for [{:keys [seq verb type args]} staged]
             ^{:key (str seq verb)} [:li verb " " type " " (some-> (:name args) name)])]]
     [:div (i18n/t :ui/studio-no-change)])])

(defn agent-list
  [{:keys [agents models]}]
  (let [{:keys [name new?]} @db
        model (:id (or (some #(when (:default %) %) models) (first models)))]
    [:<>
     [ui/menu
      (for [{nm :name dn :display-name} (remove :builtin agents)]
        ^{:key nm}
        [ui/menu-item {:text          (or dn nm)
                       :label-element [:span {:class "studio__item-name"} nm]
                       :active        (= nm name)
                       :on-click      #(send! [:studio/open nm])}])
      (when (and new? name)
        [ui/menu-item {:text (str name " " (i18n/t :ui/studio-unsaved)) :active true}])]
     [new-agent-form model]]))

(defn view
  [{:keys [on-change on-try]}]
  (let [{:keys [name text saved new? busy error result]} @db]
    [:<>
     [:div {:class "studio__editor"}
      (if-not name
        [:p {:class "studio__hint"} (i18n/t :ui/studio-intro)]
        [:<>
         [page/toolbar
          [:strong name]
          (when (not= text saved) [:span {:class "studio__dirty"} " •"])
          [:span.u-flex-1]
          (when (and on-try (not new?))
            [ui/button {:on-click #(on-try name)} (i18n/t :ui/studio-try)])
          [ui/button {:disabled busy :on-click #(send! [:studio/stage true on-change])}
           (i18n/t :ui/studio-check)]
          [ui/button
           {:intent :primary :disabled (or busy (= text saved))
            :on-click #(send! [:studio/stage false on-change])}
           (i18n/t :ui/studio-stage)]]
         [code-editor {:value text :min-height "420px" :on-change #(send! [:studio/text %])}]
         (when error [:div {:class "studio__error"} error])
         (when result [result-line result])
         [:p {:class "studio__hint"} (i18n/t :ui/studio-staged-hint)]
         (when-not new?
           ^{:key name}
           [versions/panel {:section "agents" :name name
                            :on-restore #(send! [:studio/restore (:content %)])}])])]]))
