(ns blueprint-cljs.suggest
  "Suggest (Blueprint select package): a text input whose query filters a popover list;
   the selected item is shown in the input through `:input-value-renderer`.
   Props: everything `query-list` takes, plus `:input-value-renderer (fn [item] string)`,
   `:selected-item` (controlled) / `:default-selected-item`, `:close-on-select` (true),
   `:open-on-key-down` (false), `:reset-on-close` (false), `:fill`, `:disabled`,
   `:input-props`, `:menu-props`, `:popover-props`, `:popover-content-props`."
  (:require [reagent.core :as r]
            [blueprint-cljs.core :as bp]
            [blueprint-cljs.input-group :as input-group]
            [blueprint-cljs.popover :as popover]
            [blueprint-cljs.query-list :as ql]))

(defonce ^:private id-counter (atom 0))

(def ^:private own-keys
  [:input-value-renderer :selected-item :default-selected-item :close-on-select
   :open-on-key-down :reset-on-close :fill :disabled :input-props :menu-props
   :popover-props :popover-content-props])

(def ^:private defaults
  {:close-on-select true :fill false :open-on-key-down false :reset-on-close false})

(defn- call [f & args] (when f (apply f args)))

(defn suggest
  [init]
  (let [state      (r/atom {:open?    (boolean (get-in init [:popover-props :is-open]))
                            :selected (if (contains? init :selected-item)
                                        (:selected-item init)
                                        (:default-selected-item init))})
        input-el   (atom nil)
        api        (atom nil)
        cur        (atom nil)
        listbox-id (str "listbox-" (swap! id-counter inc))
        props      (fn [] @cur)
        set-api!   #(reset! api %)]
    (letfn [(select-text! []
              (js/requestAnimationFrame
               #(when-let [el @input-el]
                  (.setSelectionRange el 0 (count (.-value el))))))
            (input-focused? [] (and @input-el (= @input-el (.-activeElement js/document))))
            (handle-input-ref [target-ref el]
              (reset! input-el el)
              (call target-ref el)
              (call (get-in (props) [:input-props :input-ref]) el)
              nil)
            (handle-input-focus [e]
              (select-text!)
              (when-not (:open-on-key-down (props)) (swap! state assoc :open? true))
              (call (get-in (props) [:input-props :on-focus]) e))
            (handle-item-select [item e]
              (let [p     (props)
                    open? (if (:close-on-select p)
                            (do (some-> @input-el .blur) false)
                            (do (some-> @input-el .focus) (select-text!) true))]
                (if (contains? p :selected-item)
                  (swap! state assoc :open? open?)
                  (swap! state assoc :open? open? :selected item))
                (call (:on-item-select p) item e)))
            (handle-interaction [open? e]
              (js/requestAnimationFrame
               (fn []
                 (when (and @input-el (not (input-focused?)))
                   (swap! state assoc :open? false))
                 (call (get-in (props) [:popover-props :on-interaction]) open? e))))
            (handle-opening [& args]
              (when (and (:reset-on-close (props)) @api) ((:set-query! @api) "" true))
              (apply call (get-in (props) [:popover-props :on-opening]) args))
            (handle-opened [& args]
              (some-> @api :scroll-active-item-into-view! (apply []))
              (apply call (get-in (props) [:popover-props :on-opened]) args))
            (reset-active-to-selected! []
              (let [p (props) sel (:selected @state)]
                (when (and @api (not (contains? p :active-item)) (some? sel)
                           (not (:reset-on-select p)))
                  ((:set-active-item! @api) (if (contains? p :selected-item)
                                              (:selected-item p)
                                              sel)))))
            (key-down-handler [ql-key-down]
              (fn [e]
                (let [k (.-key e)]
                  (cond
                    (contains? #{"Escape" "Tab"} k)
                    (do (when (= k "Escape") (some-> @input-el .blur))
                        (swap! state assoc :open? false))

                    (and (:open-on-key-down (props))
                         (not (contains? #{"Backspace" "ArrowLeft" "ArrowRight"} k)))
                    (swap! state assoc :open? true))
                  (when (:open? @state) (ql-key-down e))
                  (call (get-in (props) [:input-props :on-key-down]) e))))
            (key-up-handler [ql-key-up]
              (fn [e]
                (when (:open? @state) (ql-key-up e))
                (call (get-in (props) [:input-props :on-key-up]) e)))
            (target-renderer [lp open?]
              (fn [{:keys [ref class] :as tp}]
                (let [{:keys [disabled fill input-props input-value-renderer popover-props
                              reset-on-close]} (props)
                      sel   (:selected @state)
                      text  (if (nil? sel) "" (input-value-renderer sel))
                      ph    (or (:placeholder input-props) "Search...")
                      value (cond open?          (:query lp)
                                  (= text "")    (if reset-on-close "" (:query lp))
                                  :else          text)]
                  [input-group/input-group
                   (merge {:aria-controls listbox-id
                           :auto-complete "off"
                           :disabled      disabled
                           :tag-name      (or (some-> (:target-tag-name popover-props)
                                                      keyword)
                                              :div)}
                          (dissoc tp :ref :class :is-open :aria-haspopup :aria-expanded)
                          (dissoc input-props :input-ref :on-focus :on-key-down :on-key-up
                                  :class)
                          {:aria-autocomplete "list"
                           :aria-expanded     open?
                           :class             (bp/cx class (:class input-props))
                           :fill              fill
                           :input-ref         #(handle-input-ref ref %)
                           :on-change         (:handle-query-change lp)
                           :on-focus          handle-input-focus
                           :on-key-down       (key-down-handler (:handle-key-down lp))
                           :on-key-up         (key-up-handler (:handle-key-up lp))
                           :placeholder       (if (and open? (seq text)) text ph)
                           :role              "combobox"
                           :value             value})])))
            (render-query-list [lp]
              (let [{:keys [popover-content-props popover-props]} (props)
                    open? (:open? @state)]
                [popover/popover
                 (merge {:autofocus     false
                         :enforce-focus false
                         :placement     (if (or (:position popover-props)
                                                (:placement popover-props))
                                          nil :bottom-start)}
                        (dissoc popover-props :on-interaction :on-opening :on-opened)
                        {:is-open            open?
                         :class              (bp/cx (:class lp) (:class popover-props))
                         :content            [:div (merge popover-content-props
                                                          {:on-key-down (:handle-key-down lp)
                                                           :on-key-up   (:handle-key-up lp)})
                                              (:item-list lp)]
                         :interaction-kind   :click
                         :on-interaction     handle-interaction
                         :on-opened          handle-opened
                         :on-opening         handle-opening
                         :popover-class-name (bp/cx (bp/cls "suggest-popover")
                                                    (:popover-class-name popover-props))
                         :popup-kind         "listbox"
                         :render-target      (target-renderer lp open?)})]))]
      (r/create-class
       {:display-name "blueprint-cljs.suggest"
        :component-did-mount
        (fn [_]
          (add-watch state ::close
                     (fn [_ _ old new]
                       (when (and (:open? old) (not (:open? new)))
                         (js/setTimeout reset-active-to-selected!
                                        (or (get-in (props) [:popover-props
                                                             :transition-duration])
                                            300))))))
        :component-will-unmount
        (fn [_] (remove-watch state ::close))
        :component-did-update
        (fn [_ _]
          (let [p (props)]
            (when (and (contains? p :selected-item)
                       (not= (:selected-item p) (:selected @state)))
              (swap! state assoc :selected (:selected-item p)))))
        :reagent-render
        (fn [p]
          (let [p (merge defaults p)]
            (reset! cur p)
            [ql/query-list (merge (apply dissoc p own-keys)
                                  {::outer              p
                                   :menu-props          (merge {:aria-label
                                                                "selectable options"}
                                                               (:menu-props p)
                                                               {:id listbox-id})
                                   :initial-active-item (:selected @state)
                                   :on-item-select      handle-item-select
                                   :api-ref             set-api!
                                   :renderer            render-query-list})]))}))))
