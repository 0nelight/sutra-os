(ns blueprint-cljs.overflow-list
  "OverflowList: renders as many items as fit in one line and hands the rest to
   `:overflow-renderer`. Ported from Blueprint's binary-chop repartitioning: a 1px spacer
   that shrinks below 1px means the line overflows."
  (:require [reagent.core :as r]
            [blueprint-cljs.core :as bp]
            [blueprint-cljs.resize-sensor :as rs]))

(defn- halve [n] (js/Math.ceil (/ n 2)))

(defn- shift-elements
  "Moves `n` items from the end of `left` to the start of `right` (negative `n`: back)."
  [left right n]
  (let [all      (into (vec left) right)
        new-left (- (count left) n)]
    (cond
      (<= new-left 0)           [[] all]
      (>= new-left (count all)) [all []]
      :else                     [(subvec all 0 new-left) (subvec all new-left)])))

(defn- initial-state
  [items]
  {:chop (halve (count items)) :last-chop nil :last-overflow-count 0
   :overflow [] :visible (vec items) :repartitioning false})

(def ^:private own-keys
  [:items :visible-item-renderer :overflow-renderer :collapse-from :min-visible-items
   :always-render-overflow :observe-parents :on-overflow :tag-name :navigable
   :navigation-aria-label])

(defn overflow-list
  "Props: `:items`, `:visible-item-renderer` `(fn [item index])`, `:overflow-renderer`
   `(fn [overflow-items])`, `:collapse-from` `:start` (default) | `:end`,
   `:min-visible-items` (0), `:always-render-overflow`, `:observe-parents`,
   `:on-overflow` `(fn [overflow-items])`, `:tag-name` (`:div`), `:navigable`,
   `:navigation-aria-label`, `:class`, `:style`."
  [{:keys [items]}]
  (let [state  (r/atom (initial-state items))
        prev   (atom @state)
        props  (atom nil)
        spacer (atom nil)
        root   (atom nil)
        ;; the state the DOM shows: measuring before a newer state is drawn chops twice
        drawn  (atom nil)
        repartition!
        (fn []
          (when-let [sp (when (identical? @drawn @state) @spacer)]
            (let [{:keys [collapse-from min-visible-items items]
                   :or   {min-visible-items 0}} @props
                  {:keys [visible overflow last-chop chop]} @state
                  w          (.-offsetWidth sp)
                  exhausted? (= last-chop 1)
                  shrink?    (and (< w 0.9) (> (count visible) min-visible-items))
                  grow?      (and (or (>= w 1) (< (count visible) min-visible-items))
                                  (pos? (count overflow))
                                  (not exhausted?))]
              (if (or shrink? grow?)
                (swap! state
                       (fn [{:keys [visible overflow chop] :as s}]
                         (let [n (* chop (if shrink? 1 -1))
                               [v o] (if (= collapse-from :end)
                                       (shift-elements visible overflow n)
                                       (let [[o v] (shift-elements overflow visible (- n))]
                                         [v o]))]
                           (assoc s
                                  :chop (halve chop) :last-chop chop
                                  :last-overflow-count
                                  (if (= chop (halve (count items)))
                                    (count overflow)
                                    (:last-overflow-count s))
                                  :overflow o :visible v :repartitioning true))))
                (when (or (not= chop (halve (count items))) last-chop
                          (:repartitioning @state))
                  (swap! state assoc :chop (halve (count items)) :last-chop nil
                         :repartitioning false))))))]
    (r/create-class
     {:display-name "OverflowList"
      :component-did-mount (fn [_] (repartition!))
      :component-did-update
      (fn [_ [_ old]]
        (let [p         @props
              s         @state
              old-state @prev]
          (reset! prev s)
          (cond
            (not= (select-keys old [:items :collapse-from :min-visible-items
                                    :always-render-overflow])
                  (select-keys p [:items :collapse-from :min-visible-items
                                  :always-render-overflow]))
            (reset! state (assoc (initial-state (:items p)) :repartitioning true))

            (and (false? (:repartitioning s)) (true? (:repartitioning old-state)))
            (when (not= (count (:overflow s)) (:last-overflow-count s))
              (some-> (:on-overflow p) (apply [(:overflow s)])))

            (not= old-state s)
            (repartition!))))
      :reagent-render
      (fn [{:keys [visible-item-renderer overflow-renderer collapse-from tag-name
                   navigable navigation-aria-label always-render-overflow
                   observe-parents]
            :or   {tag-name :div collapse-from :start}
            :as   p}]
        (reset! props p)
        (reset! drawn @state)
        (let [{:keys [visible overflow]} @state
              ;; keyed like the items: unkeyed, React matches it by position, and every
              ;; repartition would remount it (and close the popover it carries)
              of   (when (or (seq overflow) always-render-overflow)
                     (with-meta [:<> (overflow-renderer overflow)] {:key "overflow"}))
              list (into [tag-name (merge (bp/dom-props p own-keys)
                                          {:class (bp/merge-class (bp/cls "overflow-list") p)})]
                         (concat
                          [(when (= collapse-from :start) of)]
                          (map-indexed (fn [i it]
                                         (with-meta [:<> (visible-item-renderer it i)]
                                           {:key (str "item-" i)}))
                                       visible)
                          [(when (= collapse-from :end) of)
                           ^{:key "spacer"}
                           [:div {:class (bp/cls "overflow-list-spacer")
                                  :ref   #(reset! spacer %)}]]))]
          [rs/resize-sensor {:on-resize       #(repartition!)
                             :observe-parents observe-parents
                             :target-ref      #(deref root)}
           (if navigable
             [:nav {:aria-label (or navigation-aria-label "") :ref #(reset! root %)} list]
             (update list 1 assoc :ref #(reset! root %)))]))})))
