(ns sutra-ui.apps.enta-editor.palette
  (:require [sutra-ui.ui.core :as ui]
            [sutra-ui.ui.inputs :as inputs]
            [clojure.string :as str]
            [reagent.core :as r]
            [sutra.capps.catalog :as cat]
            [sutra-ui.apps.enta-editor.state :as es]
            [sutra-ui.apps.enta-editor.names :as names]
            [sutra-ui.apps.enta-editor.ui :as eui]))

(defn- entries-by-id []
  (into {} (for [{:keys [items]} (cat/palette) e items] [(:id e) e])))

(defn- tile [{:keys [type label]} _]
  [:<>
   [:span {:class (str "ab-tile__glyph" (when (= :container type) " ab-tile__glyph--box"))}
    (if (= :container type) "▣" "◻")]
   [:span {:class "ab-tile__label"} label]])

(defn picker
  [_on-pick]
  (let [query (r/atom "")
        all?  (r/atom false)]
    (fn [on-pick]
      (let [q      (str/trim @query)
            groups (cat/palette q)
            hits   (mapcat :items groups)
            by-id  (entries-by-id)
            few?   (and (str/blank? q) (not @all?))
            items  (if few?
                     (for [id (:ids cat/palette-common) :let [e (by-id id)] :when e]
                       (assoc e ::group (:label cat/palette-common)))
                     (for [{:keys [label items]} groups e items] (assoc e ::group label)))]
        [:div {:class "ab-picker"}
         [ui/query-list
          {:query              @query
           :on-query-change    (fn [s _] (reset! query s))
           :items              (vec items)
           :items-equal        (fn [a b] (= ((juxt ::group :id) a) ((juxt ::group :id) b)))
           :item-renderer      (fn [item {:keys [handle-click modifiers ref id]}]
                                 (let [active? (boolean (:active modifiers))]
                                   [:li {:id            id
                                         :ref           ref
                                         :role          "option"
                                         :aria-selected active?
                                         :class         (cond-> "ab-tile"
                                                          active? (str " ab-tile--active"))
                                         :on-mouse-down #(.preventDefault %)
                                         :on-click      handle-click}
                                    (tile item active?)]))
           :item-list-renderer
           (fn [{:keys [filtered-items render-item items-parent-ref menu-props]}]
             (into [:ul {:id    (:id menu-props)
                         :role  "listbox"
                         :ref   items-parent-ref
                         :class "ab-picker__list ab-picker__grid"}]
                   (if (empty? filtered-items)
                     [[:li {:role "presentation" :class "ab-picker__group"}
                       [:span {:class "ab-muted"} "Nichts gefunden."]]]
                     (mapcat (fn [[[_ x] :as xs]]
                               (cons ^{:key (::group x)}
                                [:li {:role "presentation" :class "ab-picker__group"}
                                 (::group x)]
                                     (map (fn [[i y]] (render-item y i)) xs)))
                             (partition-by (comp ::group second)
                                           (map-indexed vector filtered-items))))))
           :on-item-select     (fn [{:keys [id]} _] (on-pick id))
           :renderer
           (fn [{:keys [item-list handle-key-down handle-key-up]}]
             [:div {:class "ab-picker__search" :on-key-up handle-key-up}
              [inputs/text-input query {:type        "search"
                                        :left-icon   :search
                                        :auto-focus  true
                                        :placeholder "Baustein suchen…"
                                        :on-key-down handle-key-down}]
              item-list])}]
         (when (and few? (seq hits))
           [ui/button {:minimal true :small true :on-click #(reset! all? true)}
            (str "Alle " (count hits) " Bausteine zeigen")])]))))

(defn- target-label []
  (when-let [{:keys [parent slot]} (es/container-target)]
    (str (names/node-label (es/tree) (cat/node-at (es/tree) parent))
         " · " (names/slot-label slot))))

(defn add-button
  []
  [eui/popover {:width "340px"}
   (fn [open?]
     [ui/tooltip
      {:content (if-let [t (target-label)] (str "fügt ein in: " t) "")}
      [ui/button {:icon :plus :active open? :disabled (nil? (es/tree))} "Hinzufügen"]])
   (fn [close!]
     [:<>
      (when-let [t (target-label)]
        [:div {:class "ab-picker__target"} (str "in " t)])
      [picker (fn [id] (close!) (es/add-at-cursor! id))]])])
