(ns blueprint-cljs.panel-stack
  "PanelStack2: a stack of panels, each `{:title :html-title :render-panel :props}`.
   `:render-panel` is a Reagent component called with `(:props panel)` plus
   `:open-panel` `(fn [panel])` and `:close-panel` `(fn [])`."
  (:require [reagent.core :as r]
            [blueprint-cljs.core :as bp]
            [blueprint-cljs.button :as button]
            [blueprint-cljs.text :as text]))

(def ^:private duration 400)

(defn- transition-view
  "Keeps one panel's DOM node and plays the enter/exit transition classes on it."
  [_ _]
  (let [node   (atom nil)
        timers (atom [])
        play!  (fn [phase]
                 (when-let [el @node]
                   (let [base   (str (bp/cls "panel-stack2") "-" phase)
                         active (str base "-active")
                         cl     (.-classList el)]
                     (run! js/clearTimeout @timers)
                     (.remove cl (str (bp/cls "panel-stack2") "-enter")
                              (str (bp/cls "panel-stack2") "-enter-active"))
                     (.add cl base)
                     (.-offsetWidth el)
                     (.add cl active)
                     (reset! timers
                             [(js/setTimeout #(when (= phase "enter") (.remove cl base active))
                                             duration)]))))]
    (r/create-class
     {:display-name "PanelStackTransition"
      :component-did-mount (fn [this]
                             (when (:animate-in? (second (r/argv this))) (play! "enter")))
      :component-did-update
      (fn [this [_ old]]
        (let [{:keys [exiting?]} (second (r/argv this))]
          (when (and exiting? (not (:exiting? old))) (play! "exit"))))
      :component-will-unmount (fn [_] (run! js/clearTimeout @timers))
      :reagent-render
      (fn [_ child]
        [:div {:class (bp/cls "panel-stack2-view") :ref #(reset! node %)} child])})))

(defn- panel-view
  [{:keys [panel previous-panel show-header on-open on-close]}]
  (let [close (fn [] (when previous-panel (on-close panel)))]
    [:<>
     (when show-header
       [:div {:class (bp/cls "panel-stack2-header")}
        [:span
         (when previous-panel
           [button/button {:aria-label "Back"
                           :class      (bp/cls "panel-stack2-header-back")
                           :icon       :chevron-left
                           :on-click   close
                           :size       :small
                           :text       (:title previous-panel)
                           :title      (:html-title previous-panel)
                           :variant    :minimal}])]
        [text/text {:class (bp/cls "heading") :ellipsize true :title (:html-title panel)}
         (:title panel)]
        [:span]])
     [(:render-panel panel) (merge (:props panel) {:close-panel close :open-panel on-open})]]))

(defn panel-stack
  "Props: `:initial-panel` (uncontrolled) or `:stack` (controlled, bottom first),
   `:on-open` `(fn [panel])`, `:on-close` `(fn [panel])`, `:render-active-panel-only`
   (true), `:show-panel-header` (true), `:class`."
  [{:keys [initial-panel]}]
  (let [local     (r/atom (if initial-panel [initial-panel] []))
        exiting   (r/atom {})
        last-rows (atom {})
        last-len  (atom nil)
        direction (atom "push")]
    (fn [{:keys [stack on-open on-close render-active-panel-only show-panel-header class]
          :or   {render-active-panel-only true show-panel-header true}}]
      (let [controlled? (some? stack)
            stk         (if controlled? (vec (reverse stack)) (vec @local))
            n           (count stk)
            open!       (fn [p]
                          (when on-open (on-open p))
                          (when-not controlled? (swap! local #(into [p] %))))
            close!      (fn [p]
                          (when on-close (on-close p))
                          (when-not controlled? (swap! local #(vec (rest %)))))
            _           (when (and @last-len (not= n @last-len))
                          (reset! direction (if (< n @last-len) "pop" "push")))
            first?      (nil? @last-len)
            _           (reset! last-len n)
            shown       (if render-active-panel-only (take 1 stk) stk)
            rows        (into {}
                              (map-indexed
                               (fn [i p]
                                 (let [k (if render-active-panel-only n (- n i))]
                                   [k {:panel p :previous (get stk (inc i))}])))
                              shown)
            gone        (apply dissoc @last-rows (keys rows))]
        (when (seq gone)
          (swap! exiting merge gone)
          (doseq [k (keys gone)]
            (js/setTimeout #(swap! exiting dissoc k) duration)))
        (reset! last-rows rows)
        (when (pos? (+ n (count @exiting)))
          [:div {:class (bp/cx (bp/cls "panel-stack2")
                               (str (bp/cls "panel-stack2") "-" @direction) class)}
           (for [[k {:keys [panel previous]}] (sort-by key (merge @exiting rows))
                 :let [exiting? (and (contains? @exiting k) (not (contains? rows k)))]]
             ^{:key k}
             [transition-view {:exiting? exiting? :animate-in? (not first?)}
              [panel-view {:panel          panel
                           :previous-panel previous
                           :show-header    show-panel-header
                           :on-open        open!
                           :on-close       close!}]])])))))
