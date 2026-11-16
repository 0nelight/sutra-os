(ns sutra-ui.ui.filter
  (:require [reagent.core :as r]
            [sutra-ui.ui.core :as ui :refer [tag]]
            [sutra-ui.ui.filter-model :as fm]
            [sutra-ui.ui.sel :refer [rd wr! as-set]]))

(defn filter-pills
  ([sel items] (filter-pills sel items nil))
  ([sel items {:keys [all-label]}]
   (let [active (as-set (rd sel))
         toggle (fn [value]
                  (when (satisfies? ISwap sel)
                    (swap! sel (fn [s] (let [s (as-set s)]
                                         (if (contains? s value) (disj s value) (conj s value)))))))
         pill   (fn [k on? f text]
                  ^{:key k} [tag {:interactive true :round true :minimal (not on?)
                                  :intent (when on? :primary) :on-click f}
                             text])
         clear  (when all-label [(pill "__all" (empty? active) #(wr! sel #{}) all-label)])
         pills  (map (fn [{:keys [value label count]}]
                       (pill (str value) (contains? active value) #(toggle value)
                             (if count (str label " " count) label)))
                     items)]
     (into [:div.capp-pills] (concat clear pills)))))

(def ^:private quiet-ms
  200)

(defn- coarse? [] (.-matches (js/matchMedia "(pointer: coarse)")))

(defn- search-field
  [{:keys [value]}]
  (let [draft (r/atom value)
        seen  (atom value)
        timer (atom nil)
        take! (fn [on-search]
                (some-> @timer js/clearTimeout)
                (reset! timer nil)
                (let [v @draft]
                  (when (not= v @seen) (reset! seen v) (on-search v))))]
    (r/create-class
     {:component-will-unmount (fn [_] (some-> @timer js/clearTimeout))
      :reagent-render
      (fn [{:keys [value on-search placeholder on-focus on-key-down]}]
        (when (and (not= value @seen) (nil? @timer))
          (reset! seen value)
          (reset! draft value))
        [ui/input-group
         {:class       "ui-filterbar__search"
          :type        "search"
          :small       true
          :value       (or @draft "")
          :placeholder placeholder
          :on-focus    on-focus
          :on-key-down (fn [^js e]
                         (when on-key-down (on-key-down e @draft))
                         (when (and (= "Enter" (.-key e)) (not (.-defaultPrevented e)))
                           (take! on-search)
                           (when (coarse?) (.blur (.-target e)))))
          :on-blur     (fn [_] (take! on-search))
          :on-change   (fn [^js e]
                         (reset! draft (.. e -target -value))
                         (some-> @timer js/clearTimeout)
                         (reset! timer (js/setTimeout #(take! on-search) quiet-ms)))}])})))

(defn bar
  [{:keys [chips] :as opts} & end]
  (into [:div.ui-filterbar
         [search-field opts]
         (for [{:keys [key label text term on-remove]} chips]
           ^{:key (str key)}
           [tag {:class "ui-filterbar__chip" :minimal true :on-remove on-remove}
            (str (when label (str label ": ")) (or text (fm/term-text term)))])]
        end))
