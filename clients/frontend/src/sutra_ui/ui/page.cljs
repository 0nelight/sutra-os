(ns sutra-ui.ui.page
  (:require [reagent.core :as r]
            [sutra.i18n.core :as i18n]
            [sutra-ui.ui.size :as groesse]
            [sutra-ui.storage :as storage]
            [sutra-ui.ui.core :as ui]
            [sutra-ui.ui.overlay :as overlay]
            [sutra-ui.ui.table :as table]))

(defonce ^:private views (atom {}))

(defn view!
  [k f]
  (swap! views assoc k f))

(defn view-fn [k] (get @views k))

(defn view
  ([k] (view k nil))
  ([k cfg] (assoc cfg :widget :view :view k)))

(defn- node [x] (if (keyword? x) (view x) x))

(def ^:private widths-key "sutra.ui.page/widths")

(def min-width 160)

(defonce widths
  (let [a (r/atom (or (storage/get-edn widths-key) {}))]
    (add-watch a ::keep (fn [_ _ old new]
                          (when (not= old new) (storage/set-edn! widths-key new))))
    a))

(defn width
  [k]
  (some-> (get @widths k) (max min-width)))

(defn grip [k] [table/col-grip widths k])

(def min-height 120)

(defn height
  [k default]
  (max min-height (or (get @widths [:h k]) default)))

(defn grip-h
  [k default]
  [:div.capp-grip-h
   {:on-pointer-down
    (fn [^js e]
      (.preventDefault e)
      (let [y0   (.-clientY e)
            h0   (height k default)
            move (fn [^js m]
                   (swap! widths assoc [:h k] (min 800 (max min-height
                                                           (+ h0 (- y0 (.-clientY m)))))))
            up   (atom nil)]
        (reset! up (fn []
                     (js/window.removeEventListener "pointermove" move)
                     (js/window.removeEventListener "pointerup" @up)))
        (js/window.addEventListener "pointermove" move)
        (js/window.addEventListener "pointerup" @up)))}])

(defn- header-more
  [items]
  (when (seq items)
    [ui/popover {:placement          :bottom-end
                 :portal-class-name  overlay/portal-class
                 :popover-class-name "capp-l-header__more"
                 :content            (into [:div.capp-l-header__more-items] items)}
     [ui/button {:minimal true :icon :more :aria-label (i18n/t :ui/capp-more)}]]))

(defn- header-items
  [items vertical?]
  (cond
    (empty? items) nil
    vertical?      (into [:div.capp-l-header__items] items)
    :else [ui/overflow-list {:class                 "capp-l-header__items"
                             :items                 (vec (range (count items)))
                             :collapse-from         :start
                             :visible-item-renderer (fn [i _] ^{:key i} [:<> (items i)])
                             :overflow-renderer     #(header-more (mapv items %))}]))

(defn header
  [{:keys [title subtitle icon lead mid tail vertical? class style title-style]} & end]
  (-> [:div {:class (cond-> "capp-l-header capp-l-header--module" class (str " " class))
             :style style}]
      (into lead)
      (conj (when (or title subtitle)
              [:div.capp-l-header__titles
               [:div.capp-l-header__title (when title-style {:style title-style})
                (when icon [:span.capp-l-header__icon icon]) title]
               (when subtitle [:div.capp-l-header__subtitle subtitle])]))
      (into mid)
      (conj (header-items (vec (remove nil? end)) vertical?))
      (into tail)))

(defn toolbar
  [& items]
  (into [:div.capp-toolbar] items))

(defonce ^:private drawn (r/atom nil))

(defn close-sidebar! [] (reset! drawn nil))

(defn- sidebar-toggle
  [{:keys [sidebar label open button?]}]
  (when (groesse/kompakt?)
    (let [text   (if (keyword? label) (i18n/t label) label)
          open?  (if open (boolean @open) (= sidebar @drawn))
          close! (if open #(reset! open false) close-sidebar!)]
      [:<>
       (when-not (false? button?)
         [:div.capp-toolbar
          [ui/button {:icon :menu :on-click #(if open (reset! open true) (reset! drawn sidebar))}
           text]])
       (when open?
         (when-let [f (view-fn sidebar)]
           [overlay/drawer {:side :left :size 300 :title text :on-close close!} [f]]))])))

(view! ::sidebar-toggle sidebar-toggle)

(defn werkzeug
  [{:keys [rail sidebar sidebar-label sidebar-open sidebar-button? sidebar-w sidebar-when main
           bottom resize]}]
  {:kind  :columns
   :fill? true
   :items (cond-> []
            rail    (conj {:kind :rows :min-size :mittel :bg :2 :box {:w 48}
                           :items [(node rail)]})
            sidebar (conj (cond-> {:kind :rows :scroll? true :min-size :mittel :bg :2
                                   :box {:w (or sidebar-w 260) :resize (or resize sidebar)}
                                   :items [(node sidebar)]}
                            sidebar-when (assoc :when sidebar-when)))
            :always (conj {:kind  :rows
                           :box   {:grow? true}
                           :items (cond-> []
                                    sidebar (conj (cond-> (view ::sidebar-toggle
                                                                {:sidebar sidebar
                                                                 :label   sidebar-label
                                                                 :open    sidebar-open
                                                                 :button? sidebar-button?})
                                                    sidebar-when (assoc :when sidebar-when)))
                                    :always (conj (node main))
                                    bottom  (conj (node bottom)))}))})

(defn liste-detail
  [{:keys [list detail open detail-w list-w resize back-label]}]
  {:kind   :columns
   :fill?  true
   :detail open
   :back-label back-label
   :items  [{:kind :rows :box (if list-w {:w list-w :resize resize} {:grow? true})
             :items [(node list)]}
            {:kind :rows :scroll? true :when open
             :box  (if list-w {:grow? true} {:w (or detail-w 340) :resize resize})
             :items [(node detail)]}]})

(defn dokument
  [{:keys [header toolbar body flow?] :or {flow? true}}]
  {:kind  :rows
   :fill? true
   :items (cond-> []
            header  (conj (node header))
            toolbar (conj (node toolbar))
            :always (conj {:kind (if flow? :flow :rows) :box {:grow? true}
                           :items [(node body)]}))})

(defn einstellungen
  [{:keys [nav sections nav-w]}]
  {:kind  :columns
   :items [{:kind :rows :scroll? true :bg :2 :box {:w (or nav-w 220)} :items [(node nav)]}
           {:kind :flow :padding :regular :box {:grow? true} :items (mapv node sections)}]})
