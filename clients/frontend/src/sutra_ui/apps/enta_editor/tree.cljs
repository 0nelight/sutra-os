(ns sutra-ui.apps.enta-editor.tree
  (:require [sutra-ui.ui.core :as ui]
            [sutra.capps.catalog :as cat]
            [sutra.capps.tree-move :as tm]
            [sutra-ui.apps.enta-editor.state :as es]
            [sutra.i18n.core :as i18n]
            [sutra-ui.apps.enta-editor.fields :as f]
            [sutra-ui.apps.enta-editor.names :as names]
            [sutra-ui.apps.enta-editor.palette :as palette]
            [sutra-ui.graph-editor.shared :as gs]))

(defn- hover [] (es/ui-get :tree-hover))

(defn- hover! [h] (es/ui-set! {:tree-hover h}))

(defn- toggled [] (es/ui-get :tree-toggled))

(defn- closed? [path depth] (not= (>= depth 2) (contains? (toggled) (vec path))))

(defn- flip! [path] (es/ui-flip! :tree-toggled (vec path)))

(defn- prefix? [a b] (= (vec a) (vec (take (count a) b))))

(defn- offene-vorfahren
  [cursor]
  (into #{} (map #(vec (take % cursor))) (range (inc (count cursor)))))

(defn- versteckt?
  [path zu]
  (boolean (some #(and (not= (vec %) (vec path)) (prefix? % path)) zu)))

(defn- adding? [token] (and (= :adding (es/mode)) (= token (es/pending))))
(defn- menu-on? [path] (and (= :menu (es/mode)) (= path (es/pending))))
(defn- dragged [] (when (= :dragging (es/mode)) (es/pending)))

(defn- add-menu [path slot]
  [:div {:class "ab-inline-picker"}
   [:div {:class "ab-inline-picker__head"}
    [:span (str "in " (names/slot-label slot))]
    [f/row-button :cross "abbrechen" #(es/mode! :cancel)]]
   [palette/picker (fn [kind] (es/mode! :commit) (es/add! path slot kind))]])

(defn- slot-adders [path node]
  (into [:div {:style {:margin-left "var(--sp-5)"}}]
        (for [{:keys [key many panes]} (cat/child-slots node)
              :let [token [path key]
                    filled? (and (not many) (not panes) (some? (get node key)))]
              :when (not filled?)]
          ^{:key key}
          [:div
           (if (adding? token)
             [add-menu path key]
             [f/dashed-button (str "+ " (names/slot-label key))
              #(es/mode! :add-start token)])])))

(defn- clip-bar []
  (when-let [clip (es/clip-label)]
    [:div {:class "ab-clip"}
     [ui/tooltip {:content clip}
      [:span {:class "ab-clip__label"} (str "Ablage: " clip)]]
     [f/dashed-button "Einfügen" #(es/paste!)]]))

(defn- drop-style [path]
  (let [{h :path zone :zone} (hover)]
    (when (= h path)
      (case zone
        :into   {:box-shadow (str "inset 0 0 0 2px " gs/blue) :background "var(--c-primary-bg)"}
        :before {:box-shadow (str "inset 0 2px 0 0 " gs/blue)}
        :after  {:box-shadow (str "inset 0 -2px 0 0 " gs/blue)}
        nil))))

(defn- pointer-zone
  [e]
  (let [r (.getBoundingClientRect (.-currentTarget e))
        h (.-height r)
        y (- (.-clientY e) (.-top r))]
    (cond (< y (* 0.25 h)) :before
          (> y (* 0.75 h)) :after
          :else            :into)))

(defn- legal-zone
  [path gewuenscht {:keys [src]}]
  (first (for [z (cons gewuenscht (remove #{gewuenscht} tm/zones))
               :when (if src (es/drop-plan src path z) (es/drop-target path z))]
           z)))

(defn- drag-handlers [path depth]
  {:draggable (and (seq path) (es/movable? path))
   :on-drag-start (fn [e]
                    (es/mode! :drag-start {:src (vec path)})
                    (hover! nil)
                    (when-let [dt (.-dataTransfer e)]
                      (set! (.-effectAllowed dt) "move")
                      (.setData dt "text/plain" (pr-str path))))
   :on-drag-end (fn [_] (es/mode! :cancel) (hover! nil))
   :on-drag-over (fn [e]
                   (if-let [zone (some->> (dragged) (legal-zone path (pointer-zone e)))]
                     (do (.preventDefault e)
                         (when-let [dt (.-dataTransfer e)]
                           (set! (.-dropEffect dt) (if (:kind (dragged)) "copy" "move")))
                         (when (and (= :into zone) (closed? path depth))
                           (flip! path))
                         (when (not= (hover) {:path (vec path) :zone zone})
                           (hover! {:path (vec path) :zone zone})))
                     (when (= (:path (hover)) (vec path)) (hover! nil))))
   :on-drag-leave (fn [_] (when (= (:path (hover)) (vec path)) (hover! nil)))
   :on-drop (fn [e]
              (.preventDefault e)
              (let [{:keys [src kind]} (dragged)
                    zone (or (:zone (hover)) (legal-zone path (pointer-zone e) (dragged)))]
                (cond
                  src  (es/move-to! src path zone)
                  kind (es/add-at! path zone kind)))
              (es/mode! :commit)
              (hover! nil))})

(defn- row-menu
  [path depth]
  (into [:div {:style {:display "flex" :flex-wrap "wrap" :gap "var(--sp-1)"
                       :margin (str "2px 0 4px " (+ 18 (* depth 12)) "px")}}]
        (for [{:keys [id label keys]} (filter :menu (es/available))]
          ^{:key id}
          [f/dashed-button (if-let [k (first keys)] (str label " · " k) label)
           (fn [e]
             (.stopPropagation e)
             (es/mode! :cancel)
             (es/run-at! path id))])))

(defn outline-panel []
  (let [tree    (es/tree)
        cursor  (es/cursor)
        gezogen (dragged)
        probs   (into {} (map (juxt :path identity)) (es/problems))
        alle    (if tree (names/labelled-entries tree) [])
        zu      (into #{}
                      (comp (filter #(seq (cat/children-of (:node %))))
                            (filter #(closed? (:path %) (:depth %)))
                            (map (comp vec :path))
                            (remove (offene-vorfahren cursor)))
                      alle)
        entries (remove #(versteckt? (:path %) zu) alle)]
    [:div {:class "ab-outline"}
     (if-not tree
       [:div {:class "ab-muted"} "Keine App geladen."]
       [:<>
        [:div {:class "ab-outline__head"}
         [:span {:class "ab-panel-title"} "Struktur"]
         [palette/add-button]]
        [clip-bar]
        (doall
         (for [{:keys [path depth label node type slot]} entries
               :let [sel?  (= path cursor)
                     prob  (get probs path)
                     src?  (= (:src gezogen) (vec path))]]
           ^{:key (str path)}
           [:div
            [:div (merge
                   (drag-handlers path depth)
                   {:class (cond-> "ab-row" sel? (str " ab-row--sel") prob (str " ab-row--prob"))
                    :style (merge {:padding-left (str (+ 4 (* depth 12)) "px")
                                   :opacity (if src? 0.45 1)}
                                  (drop-style path))
                    :title (:error prob)
                    :on-click #(es/select! path)})
             (when (seq path)
               [ui/tooltip {:content (if (es/movable? path)
                     "ziehen zum Verschieben"
                     "der Rumpf eines Reiters wandert nicht — der Reiter tut es")}
                [:span {:class "ab-row__handle"
                        :style {:cursor (if (es/movable? path) "grab" "default")}}
                 "⠿"]])
             (let [kinder (count (cat/children-of node))]
               (if (pos? kinder)
                 [ui/tooltip
                  {:content (if (contains? zu (vec path)) "aufklappen" "zuklappen")}
                  [:span {:on-click #(do (.stopPropagation %) (flip! path))
                          :class    "ab-row__caret"}
                   [ui/icon {:icon (if (contains? zu (vec path)) :caret-right :caret-down)
                             :size 12}]]]
                 [:span {:class "ab-row__caret"}]))
             [:span {:style {:color (if (= :container type) gs/blue gs/grey)
                             :font-size "var(--fs-xs)"}}
              (if (= :container type) "▣" "◻")]
             [:span {:style {:font-weight (if sel? 600 400) :flex 1
                             :overflow "hidden" :text-overflow "ellipsis"
                             :white-space "nowrap"}}
              label]
             (when (or (:when node) (:hidden? node) (:visibility node))
               [ui/tooltip {:content (str "bedingt sichtbar: "
                        (pr-str (or (:when node) (:visibility node)
                                    (when (:hidden? node) :hidden))))}
                [:span {:style {:color "var(--c-warning)" :font-size "var(--fs-xs)"}}
                 "◐"]])
             (when (contains? zu (vec path))
               [:span {:style {:color gs/grey :font-size "var(--fs-xs)"}}
                (str "(" (count (remove #(= (vec path) (:path %))
                                        (filter #(prefix? path (:path %)) alle)))
                     ")")])
             (when (and slot (pos? depth) (not (#{"items" "body"} (str slot))))
               [:span {:class "ab-row__slot"} (names/slot-label slot)])
             [:span {:class "ab-row__tools"}
              (when (es/can-paste?)
                [f/row-button :import (i18n/t :ui/app-builder-paste-here)
                 #(do (.stopPropagation %) (es/paste-at! path))])
              (when (seq path)
                [f/row-button :more (i18n/t :ui/app-builder-more)
                 #(do (.stopPropagation %)
                      (es/select! path)
                      (if (menu-on? path) (es/mode! :cancel) (es/mode! :menu-open path)))])]]
            (when (menu-on? path)
              [row-menu path depth])
            (when (and sel? (= :container type))
              [:div {:style {:margin-left (str (* depth 12) "px")}}
               [slot-adders path node]])]))])]))
