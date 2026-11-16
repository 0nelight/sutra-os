(ns blueprint-cljs.toast
  "Toast and the toaster that stacks them (Blueprint OverlayToaster).
   `(create {:position :top})` mounts a toaster and returns its api map with
   `:show!` `:dismiss!` `:clear!` `:get-toasts`; `show!`/`dismiss!`/`clear!` below call it.
   `[overlay-toaster {:toaster-ref (fn [api])}]` embeds one declaratively."
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [reagent.dom.client :as rdc]
            [blueprint-cljs.core :as bp]
            [blueprint-cljs.button :as button]
            [blueprint-cljs.icon :as icon]
            [blueprint-cljs.overlay :as overlay]
            [blueprint-cljs.portal :as portal]))

(defn toast
  "Props: `:message`, `:icon`, `:intent`, `:action` (anchor-button props, e.g. {:text
   :icon :href :on-click}), `:is-close-button-shown` (true), `:timeout` ms (5000; 0 =
   never), `:on-dismiss (fn [timeout-expired?])`, `:class`; other keys go to the element."
  [_props]
  (let [timer  (atom nil)
        cur    (atom {})
        clear! (fn [] (some-> @timer js/clearTimeout) (reset! timer nil))
        dismiss! (fn [expired?]
                   (clear!)
                   (when-let [f (:on-dismiss @cur)] (f expired?)))
        start! (fn []
                 (clear!)
                 (let [t (:timeout @cur 5000)]
                   (when (and t (pos? t))
                     (reset! timer (js/setTimeout #(dismiss! true) t)))))]
    (r/create-class
     {:display-name "blueprint-cljs.toast"
      :component-did-mount (fn [_] (start!))
      :component-did-update
      (fn [_ [_ old]]
        (when (not= (:timeout old) (:timeout @cur)) (start!)))
      :component-will-unmount (fn [_] (clear!))
      :reagent-render
      (fn [props]
        (reset! cur props)
        (let [{:keys [action class icon intent is-close-button-shown message]} props]
          [:div (merge (bp/dom-props props [:action :class :icon :intent :timeout :message
                                            :is-close-button-shown :on-dismiss])
                       {:class          (bp/cx (bp/cls "toast") (bp/intent-class intent) class)
                        :on-blur        start!
                        :on-focus       clear!
                        :on-mouse-enter clear!
                        :on-mouse-leave start!
                        :tab-index      0})
           (icon/render-icon icon)
           [:span {:class (bp/cls "toast-message") :role "alert"} message]
           [button/button-group {:variant :minimal}
            (when action
              [button/anchor-button
               (merge (dissoc action :intent)
                      {:on-click (fn [e]
                                   (when-let [f (:on-click action)] (f e))
                                   (dismiss! false))})])
            (when-not (false? is-close-button-shown)
              [button/button {:aria-label "Close" :icon :cross
                              :on-click #(dismiss! false)}])]]))})))

;; ------------------------------------------------------------------ toaster

(defn- make-api
  "The toaster api over `state` (an r/atom {:toasts [...]})."
  [state opts]
  (let [counter (atom 0)
        set-phase! (fn [k ph]
                     (swap! state update :toasts
                            (fn [ts] (mapv #(if (= k (:key %)) (assoc % :phase ph) %) ts)))
                     (r/flush))
        phase-of  (fn [k] (some #(when (= k (:key %)) (:phase %)) (:toasts @state)))
        dismiss!
        (fn [k expired?]
          (when-let [t (some #(when (= k (:key %)) %) (:toasts @state))]
            (when-not (contains? #{:exit :exit-active} (:phase t))
              (when-let [f (get-in t [:props :on-dismiss])] (f (boolean expired?)))
              (set-phase! k :exit)
              (overlay/next-frame
               (fn []
                 (set-phase! k :exit-active)
                 (js/setTimeout
                  (fn []
                    (swap! state update :toasts
                           (fn [ts] (into [] (remove #(= k (:key %))) ts)))
                    (r/flush))
                  350))))))
        show!
        (fn show!
          ([props] (show! props nil))
          ([props k]
           (let [k (or k (str "toast-" (swap! counter inc)))]
             (if (some #(= k (:key %)) (:toasts @state))
               (swap! state update :toasts
                      (fn [ts] (mapv #(if (= k (:key %)) (assoc % :props props) %) ts)))
               (do
                 (when-let [m (:max-toasts @opts)]
                   (let [live (remove #(contains? #{:exit :exit-active} (:phase %))
                                      (:toasts @state))]
                     (when (>= (count live) m) (dismiss! (:key (last live)) false))))
                 (swap! state update :toasts #(into [{:key k :props props :phase :enter}] %))
                 (overlay/next-frame
                  (fn []
                    (when (= :enter (phase-of k))
                      (set-phase! k :enter-active)
                      (js/setTimeout #(when (= :enter-active (phase-of k))
                                        (set-phase! k :open))
                                     350))))))
             k)))]
    {:show!      show!
     :dismiss!   (fn ([k] (dismiss! k false)) ([k expired?] (dismiss! k expired?)))
     :clear!     (fn [] (doseq [t (:toasts @state)] (dismiss! (:key t) false)))
     :get-toasts (fn [] (mapv #(assoc (:props %) :key (:key %))
                              (remove #(contains? #{:exit :exit-active} (:phase %))
                                      (:toasts @state))))}))

(defn- toaster-view
  [state opts api]
  (let [{:keys [position use-portal class can-escape-key-clear portal-container]
         :or   {position :top use-portal true can-escape-key-clear true}} @opts
        toasts (:toasts @state)
        sides  (str/split (name position) #"-")
        root   [:div {:class       (bp/cx (bp/cls "toast-container")
                                          (map #(bp/cls (str "toast-container-" %)) sides)
                                          (bp/cls (str "toast-container-"
                                                       (if use-portal "in-portal" "inline")))
                                          class)
                      :aria-live   "polite"
                      :on-key-down (fn [e]
                                     (when (and can-escape-key-clear (= "Escape" (.-key e)))
                                       ((:clear! api))))}
                (for [{:keys [key props phase]} toasts]
                  ^{:key key}
                  [toast (assoc props
                                :class (bp/cx (:class props)
                                              (overlay/transition-classes (bp/cls "toast") phase))
                                :on-dismiss (fn [expired?]
                                              ((:dismiss! api) key expired?)))])]]
    (if use-portal
      [portal/portal {:container portal-container} root]
      root)))

(defn overlay-toaster
  "Declarative toaster. Props: `:position` (:top :top-left :top-right :bottom …),
   `:max-toasts`, `:use-portal` (true), `:can-escape-key-clear` (true), `:class`,
   `:portal-container`, `:toaster-ref (fn [api])` receives the api map."
  [props]
  (let [state (r/atom {:toasts []})
        opts  (r/atom props)
        api   (make-api state opts)]
    (when-let [f (:toaster-ref props)] (f api))
    (fn [props]
      (when (not= props @opts) (reset! opts props))
      [toaster-view state opts api])))

(defn create
  "Mounts a toaster into a new element under `:container` (default body) and returns its
   api map. Options as `overlay-toaster` (without `:use-portal`), plus `:z-index` of the
   layer (default 40, Blueprint's overlay level) for a host whose own chrome sits higher."
  ([] (create {}))
  ([opts]
   (let [state (r/atom {:toasts []})
         o     (r/atom (merge opts {:use-portal false}))
         api   (make-api state o)
         el    (doto (.createElement js/document "div")
                 (.setAttribute "class" (bp/cls "portal")))]
     (.appendChild (or (:container opts) (.-body js/document)) el)
     (set! (.. el -style -cssText)
           (str "position:fixed;left:0;right:0;top:0;bottom:0;pointer-events:none;z-index:"
                (:z-index opts 40)))
     (rdc/render (rdc/create-root el) [toaster-view state o api])
     api)))

(defn show! ([api props] ((:show! api) props)) ([api props k] ((:show! api) props k)))
(defn dismiss! [api k] ((:dismiss! api) k))
(defn clear! [api] ((:clear! api)))
(defn get-toasts [api] ((:get-toasts api)))

(defonce ^:private default (atom nil))

(defn default-toaster
  "The shared toaster at the top of the page, created on first use."
  []
  (or @default (reset! default (create {:position :top}))))
