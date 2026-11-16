(ns sutra-ui.ui.overlay
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [sutra.i18n.core :as i18n]
            [sutra-ui.ui.core :as ui]))

(def portal-class "consumer__app capp-portal")

(def combo
  {:selected-item        nil
   :input-value-renderer str
   :popover-props        {:minimal            true
                          :match-target-width true
                          :position-strategy  :fixed
                          :portal-class-name  portal-class}})

(defn combo-option
  ([render] (combo-option render nil))
  ([render selected?]
   (fn [item {:keys [handle-click modifiers ref]}]
     [ui/menu-item {:text          (render item)
                    :ref           ref
                    :active        (:active modifiers)
                    :selected      (when selected? (boolean (selected? item)))
                    :role-structure :listoption
                    :on-mouse-down #(.preventDefault %)
                    :on-click      handle-click}])))

(defn window
  [{:keys [on-close backdrop]} frame]
  [ui/overlay {:is-open                 true
               :on-close                (fn [_] (when on-close (on-close)))
               :has-backdrop            true
               :autofocus               true
               :enforce-focus           true
               :can-outside-click-close (some? on-close)
               :can-escape-key-close    (some? on-close)
               :transition-duration     0
               :portal-class-name       portal-class
               :class                   "capp-layer"
               :backdrop-class-name     (cond-> "capp-backdrop"
                                          backdrop (str " " backdrop))}
   frame])

(def ^:private placements
  {:start   {:placement :bottom-start}
   :end     {:placement :bottom-end}
   :stretch {:placement :bottom-start :match-target-width true}
   :above   {:placement :top-start}})

(defn popover
  [{:keys [on-close anchor place at placement] :or {place :start}} child]
  [ui/popover
   (merge {:is-open            true
           :on-interaction     (fn [o e] (when (and (not o) on-close) (on-close e)))
           :minimal            true
           :autofocus          false
           :enforce-focus      false
           :portal-class-name  portal-class
           :popover-class-name "capp-layer"
           :content            (if (keyword? (first child)) child [:div child])
           :render-target
           (fn [{:keys [ref]}]
             (if at
               [ui/portal
                [:div {:class (ui/cls "context-menu-virtual-target")
                       :style {:left (first at) :top (second at)}
                       :ref   ref}]]
               [:span {:hidden true
                       :ref    #(ref (when % (if anchor (anchor) (.-parentElement %))))}]))}
          (if at
            {:placement (or placement :bottom-start) :position-strategy :fixed}
            (placements place)))])

(defn- frame-props
  ([close] (frame-props close true))
  ([close backdrop?]
   {:is-open                 true
    :on-close                (when close (fn [_] (close)))
    :is-close-button-shown   (some? close)
    :has-backdrop            backdrop?
    :autofocus               backdrop?
    :enforce-focus           backdrop?
    :can-escape-key-close    (some? close)
    :can-outside-click-close (and backdrop? (some? close))
    :transition-duration     0
    :portal-class-name       portal-class
    :backdrop-class-name     "capp-backdrop"}))

(defn- keyword-or-string? [x] (or (keyword? x) (string? x)))

(defn- overlay-head
  [props {:keys [icon header? close-on-backdrop?]}]
  (cond-> props
    icon                        (assoc :icon icon)
    (false? header?)            (assoc :title nil :is-close-button-shown false)
    (false? close-on-backdrop?) (assoc :can-outside-click-close false)))

(defn- px
  [size]
  (cond (number? size) size
        (string? size) (some-> (re-matches #"(\d+)(?:px)?" size) second js/parseInt)))

(defn form-actions
  [{:keys [on-cancel ok-label on-ok ok-intent ok-disabled?]}]
  [:<>
   [ui/button {:on-click on-cancel} (i18n/t :ui/cancel)]
   [ui/button {:intent (or ok-intent :primary) :disabled (boolean ok-disabled?)
               :on-click on-ok}
    ok-label]])

(defn- foot-of
  [{:keys [foot foot-end? ok-label on-close] :as opts}]
  (if ok-label
    [(form-actions (assoc opts :on-cancel on-close)) true]
    [foot foot-end?]))

(defn modal
  [{:keys [on-close title size class body-class style] :as opts} & body]
  (let [w           (px size)
        [foot end?] (foot-of opts)]
    [ui/dialog (cond-> (overlay-head (assoc (frame-props on-close)
                                            :title (or title "")
                                            :class (ui/cx "capp-modal" class
                                                          (when (and (not w)
                                                                     (keyword-or-string? size))
                                                            (str "capp-modal--" (name size)))))
                                     opts)
                 w     (assoc :style {:width w :max-width "calc(100vw - 32px)"})
                 style (update :style merge style))
     (into [ui/dialog-body {:class (ui/cx "capp-modal__body" body-class)
                            :use-overflow-scroll-container false}]
           body)
     (when foot
       (if end?
         [ui/dialog-footer {:class "capp-modal__foot" :minimal true :actions foot}]
         [ui/dialog-footer {:class "capp-modal__foot" :minimal true} foot]))]))

(defn drawer
  [{:keys [on-close title side size backdrop?] :or {side :right backdrop? true} :as opts}
   & body]
  (let [w           (px size)
        [foot end?] (foot-of opts)]
    [ui/drawer (cond-> (overlay-head
                        (assoc (frame-props on-close backdrop?)
                               :title (or title "")
                               :position side
                               :class (cond-> (str "capp-drawer capp-drawer--" (name side))
                                        (and (not w) (keyword-or-string? size))
                                        (str " capp-drawer--" (name size))))
                        opts)
                 w (assoc :size (str w "px")))
     (into [:div {:class (ui/cx (ui/cls "drawer-body") "capp-drawer__body")}] body)
     (when foot
       [:div {:class (ui/cx (ui/cls "drawer-footer") "capp-drawer__foot"
                            {"capp-drawer__foot--end" end?})}
        foot])]))

(defn dock
  [{:keys [title size icon header? on-close]} & body]
  [:aside {:class (cond-> "capp-dock"
                    (keyword-or-string? size) (str " capp-dock--" (name size)))
           :style (when (number? size) {:flex-basis (str size "px")})}
   (when-not (false? header?)
     [:div.capp-dock__head
      [:span.capp-dock__title (when icon [:span.capp-dock__icon icon]) title]
      [ui/button {:minimal true :icon "cross" :aria-label (i18n/t :ui/close)
                  :on-click on-close}]])
   (into [:div.capp-dock__body] body)])

(defn bottom-drawer
  [{:keys [on-close title]} child]
  [ui/drawer (assoc (frame-props on-close)
                    :title (or title "")
                    :position :bottom
                    :size "auto"
                    :class "capp-drawer capp-drawer--bottom")
   [:div {:class (ui/cx (ui/cls "drawer-body") "capp-drawer__body")} child]])

(defonce ^:private pending (r/atom nil))
(defonce ^:private shown (r/atom nil))

(defn- settle! [v]
  (when-let [{:keys [resolve]} @pending]
    (reset! pending nil)
    (resolve v)))

(defn- ask! [kind opts cancel-value]
  (js/Promise.
   (fn [resolve _]
     (when-let [prev @pending] ((:resolve prev) (:cancel prev)))
     (reset! pending (assoc opts
                            :kind kind
                            :cancel cancel-value
                            :value (str (or (:default opts) ""))
                            :resolve resolve))
     (reset! shown @pending))))

(defn confirm! [opts] (ask! :confirm opts false))

(defn prompt! [opts] (ask! :prompt opts nil))

(defn- ok! []
  (let [{:keys [kind value]} @pending]
    (settle! (if (= kind :prompt) value true))))

(defn- cancel! [] (settle! (:cancel @pending)))

(defn- focus-button!
  [danger? ^js container]
  (when-let [bs (some-> container
                        (.querySelectorAll (str "." (ui/cls "alert-footer") " button")))]
    (some-> (aget bs (if danger? 1 0)) .focus)))

(defn- confirm-alert [{:keys [title body danger? confirm-label]}]
  [ui/alert {:is-open                  (some? @pending)
             :intent                   (if danger? :danger :primary)
             :icon                     (if danger? :trash :help)
             :confirm-button-text      (or confirm-label
                                           (if danger? (i18n/t :ui/delete)
                                               (i18n/t :ui/confirm-ok)))
             :cancel-button-text       (i18n/t :ui/cancel)
             :can-escape-key-cancel    true
             :can-outside-click-cancel true
             :on-opened                #(focus-button! danger? %)
             :on-confirm               #(ok!)
             :on-cancel                #(cancel!)}
   (when-not (str/blank? title) [:h4 {:class (ui/cls "heading")} title])
   (when-not (str/blank? body) [:p {:class "sutra-confirm__body"} body])])

(defn form-footer
  [opts]
  [ui/dialog-footer {:actions (form-actions opts)}])

(defn form-dialog
  [{:keys [open? on-close] :or {open? true} :as opts} & body]
  (when open?
    (into [modal (assoc opts :close-on-backdrop? (get opts :close-on-backdrop? true)
                        :on-close on-close)]
          body)))

(defn- prompt-dialog [{:keys [title body label placeholder confirm-label]}]
  (let [value (:value @pending)]
    [form-dialog {:open?    (some? @pending)
                  :on-close #(cancel!)
                  :title    (when-not (str/blank? title) title)
                  :ok-label (or confirm-label (i18n/t :ui/confirm-ok))
                  :on-ok    #(ok!)}
     (when-not (str/blank? body) [:p {:class "sutra-confirm__body"} body])
     [ui/form-group {:label label}
      [ui/input-group
       {:auto-focus  true
        :fill        true
        :value       (or value "")
        :placeholder placeholder
        :on-change   #(swap! pending assoc :value (.. % -target -value))
        :on-key-down #(when (= "Enter" (.-key %)) (.preventDefault %) (ok!))}]]]))

(defn host []
  (let [{:keys [kind] :as s} @shown]
    (when kind
      (if (= kind :prompt)
        [prompt-dialog s]
        [confirm-alert s]))))

(defn menu-items
  [items]
  (into [ui/menu]
        (for [[label f danger?] (remove nil? items)]
          [ui/menu-item {:text label :on-click #(f) :intent (when danger? :danger)}])))

(defn menu-button
  [attrs label items]
  [:span {:class "ui-menu" :on-click #(.stopPropagation %)}
   [ui/popover {:placement          :bottom-end
                :popover-class-name "ui-menu__list"
                :content            [menu-items items]}
    [ui/button (merge {:aria-haspopup "menu"} attrs) label]]])

(defn more-menu
  [items]
  [menu-button {:minimal true :small true :icon :more :aria-label (i18n/t :ui/kompass-more)}
   nil items])
