(ns sutra-ui.builder.command-palette
  (:require [sutra-ui.ui.core :as ui]
            [clojure.string :as str]
            [sutra-ui.builder.model :as model]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.object.card :as oc]
            [sutra-ui.feature :as feature]
            [sutra.i18n.core :as i18n]
            [sutra-ui.ui.hotkeys :as hotkeys]))

(defn go-app!
  [app-id]
  (some-> (feature/app-default-nav app-id) shell/go!))

(defn cmdk-entries
  []
  (let [canon @catalog/canonical-schema]
    (concat
     (for [{:keys [id label-key]} (feature/apps)]
       {:kind (i18n/t :ui/kind-app) :glyph "▦" :label (i18n/t label-key)
        :nav #(go-app! id)})
     (for [[pkey _] (sort-by (comp str key) (:projects canon))]
       {:kind (i18n/t :ui/kind-project) :glyph "🗂" :label (name pkey)
        :nav #(feature/command! :open-project pkey)})
     (mapcat #((:entries %)) (feature/contributions :search))
     (for [k (sort (keys (:transforms canon)))]
       {:kind (i18n/t :ui/kind-transform) :glyph "ƒ" :label (name k)
        :nav (fn [] (catalog/open-resource! :transforms k))})
     (for [[k spec] (sort-by key (:functions canon))]
       {:kind (i18n/t :ui/kind-function) :glyph "▶" :label (oc/spec-label spec k) :api (name k)
        :nav (fn [] (catalog/open-resource! :functions k))})
     (for [[k spec] (sort-by key (:shared-properties canon))]
       {:kind (i18n/t :ui/kind-shared-property) :glyph "•" :label (oc/spec-label spec k)
        :api (name k)
        :nav (fn [] (catalog/open-resource! :shared-properties k))})
     (for [k (sort (keys (:sources canon)))]
       {:kind (i18n/t :ui/kind-source) :glyph "▤" :label (name k)
        :nav (fn [] (catalog/open-resource! :sources k))})
     (for [[k spec] (sort-by key (:actions canon))]
       {:kind (i18n/t :ui/kind-action) :glyph "⚡" :label (oc/spec-label spec k) :api (name k)
        :nav (fn [] (catalog/open-resource! :actions k))}))))

(defn cmdk-matches [q entries]
  (if (str/blank? q)
    (take 50 entries)
    (let [ql  (str/lower-case (str/trim q))
          hit #(some-> % str/lower-case (str/includes? ql))]
      (->> entries
           (filter (fn [{:keys [label kind api]}]
                     (or (hit label) (hit kind) (hit api))))
           (take 50)))))

(defn palette-key [{:keys [kind api label object]}]
  [kind (if object (second object) (str api label))])

(defn command-palette []
  (when (:cmdk-open? @shell/ui-state)
    (let [q       (:cmdk-query @shell/ui-state "")
          results (vec (concat (:cmdk-objects @shell/ui-state [])
                               (cmdk-matches q (cmdk-entries))))
          close!  #(shell/close-cmdk!)
          run!    (fn [nav] (when nav (nav)) (close!))]
      [ui/omnibar
       {:is-open            true
        :on-close           (fn [_] (close!))
        :class              "palette"
        :input-props        {:placeholder      (i18n/t :ui/palette-placeholder)
                             :aria-label       (i18n/t :ui/palette-placeholder)
                             :input-class-name "palette__input"}
        :query              q
        :on-query-change    (fn [v _] (model/send! [:builder.cmdk/query v]))
        :items              results
        :items-equal        (fn [a b] (= (palette-key a) (palette-key b)))
        :item-renderer
        (fn [{:keys [kind glyph label api object nav]} {:keys [handle-click modifiers ref id]}]
          (let [[ot oid row] object]
            [ui/menu-item
             {:id             id
              :ref            ref
              :role-structure :listoption
              :active         (:active modifiers)
              :multiline      true
              :class          "palette__row"
              :label          [:span {:class "palette__kind"} kind]
              :text           [:div {:class "palette__entry"}
                               [:span {:class "palette__glyph"} glyph]
                               (if object
                                 [:div {:class "palette__object"}
                                  [oc/object-card ot oid {:row      row
                                                          :compact? true
                                                          :on-open  (fn [_ _] (run! nav))}]]
                                 [ui/tooltip {:content api}
                                  [:span {:class "dataconn-importrow__name"} label]])]
              :on-click       handle-click}]))
        :item-list-renderer
        (fn [{:keys [filtered-items render-item items-parent-ref menu-props]}]
          [:<>
           (into [ui/menu (merge menu-props {:ref items-parent-ref})]
                 (if (empty? filtered-items)
                   [[ui/menu-item {:disabled true :text (i18n/t :ui/no-matches)}]]
                   (mapcat (fn [[[_ x] :as xs]]
                             (let [g (or (:group x) (:kind x))]
                               (cons ^{:key g} [ui/menu-divider {:title g}]
                                     (map (fn [[i y]] (render-item y i)) xs))))
                           (partition-by (fn [[_ x]] (or (:group x) (:kind x)))
                                         (map-indexed vector filtered-items)))))
           [:div {:class "palette__footer"} (i18n/t :ui/palette-footer)]])
        :on-item-select     (fn [{:keys [nav]} _] (run! nav))}])))

(defonce _cmdk-keys
  (hotkeys/register!
   [{:combo           "mod+k"
     :label           [i18n/t :ui/hotkeys-palette]
     :allow-in-input  true
     :in-overlay      true
     :prevent-default true
     :on-key-down
     (fn [_] (shell/toggle-cmdk!))}]))
