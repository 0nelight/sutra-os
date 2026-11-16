(ns blueprint-cljs.select
  "Select (Blueprint select package): a target (the children) opening a popover with a
   filter input and the item list.
   `[select {:items films :item-renderer render-film :item-predicate filter-film
             :on-item-select #(reset! sel %)}
     [button {:text (:title @sel) :end-icon :double-caret-vertical}]]`
   Props: everything `query-list` takes, plus `:filterable` (true), `:disabled`,
   `:fill`, `:placeholder` (\"Filter...\"), `:input-props`, `:menu-props`,
   `:popover-props`, `:popover-content-props`, `:popover-target-props`,
   `:reset-on-close`."
  (:require [reagent.core :as r]
            [blueprint-cljs.core :as bp]
            [blueprint-cljs.button :as button]
            [blueprint-cljs.input-group :as input-group]
            [blueprint-cljs.popover :as popover]
            [blueprint-cljs.query-list :as ql]))

(defonce ^:private id-counter (atom 0))

(def ^:private own-keys
  [:filterable :input-props :menu-props :popover-props :popover-content-props
   :popover-target-props :placeholder :fill :reset-on-close])

(defn- call [f & args] (when f (apply f args)))

(defn select
  [& args]
  (let [state      (r/atom {:open? (boolean (when (map? (first args))
                                              (get-in (first args) [:popover-props :is-open])))})
        input-el   (atom nil)
        prev-focus (atom nil)
        api        (atom nil)
        cur        (atom nil)
        listbox-id (str "listbox-" (swap! id-counter inc))
        props      (fn [] @cur)]
    (letfn [(reset-query! [] (some-> @api :set-query! (apply ["" true])))
            (handle-input-ref [el]
              (reset! input-el el)
              (call (get-in (props) [:input-props :input-ref]) el)
              nil)
            (handle-item-select [item e]
              (let [t         (some-> e .-target)
                    mi        (when (instance? js/Element t)
                                (.closest t (str "." (bp/cls "menu-item"))))
                    dismiss?  (if mi (.matches mi (str "." popover/dismiss-class)) true)]
                (swap! state assoc :open? (not dismiss?))
                (call (:on-item-select (props)) item e)))
            (handle-interaction [open? e]
              (swap! state assoc :open? open?)
              (call (get-in (props) [:popover-props :on-interaction]) open? e))
            (handle-opening [& args]
              (reset! prev-focus (.-activeElement js/document))
              (when (:reset-on-close (props)) (reset-query!))
              (apply call (get-in (props) [:popover-props :on-opening]) args))
            (handle-opened [& args]
              (some-> @api :scroll-active-item-into-view! (apply []))
              (js/requestAnimationFrame
               #(when-not (false? (get-in (props) [:input-props :auto-focus]))
                  (some-> @input-el .focus)))
              (apply call (get-in (props) [:popover-props :on-opened]) args))
            (handle-closing [& args]
              (js/requestAnimationFrame
               #(when-let [el @prev-focus]
                  (reset! prev-focus nil)
                  (when (.-isConnected el) (.focus el))))
              (apply call (get-in (props) [:popover-props :on-closing]) args))
            (handle-target-key-down [e]
              (cond
                (contains? #{"ArrowUp" "ArrowDown"} (.-key e))
                (do (.preventDefault e) (swap! state assoc :open? true))

                (and (ql/keyboard-click? e) (not (ql/target-self-activates-on-key-up? e)))
                (swap! state assoc :open? true)))
            (target-renderer [lp open?]
              (fn [{:keys [ref class] :as tp}]
                (let [{:keys [disabled filterable fill popover-props popover-target-props
                              children]
                       :or   {filterable true}} (props)
                      tag     (keyword (or (:target-tag-name popover-props) :div))
                      tp-keys (dissoc tp :ref :class :is-open :aria-haspopup :aria-expanded)]
                  (into [tag (merge {:aria-controls listbox-id}
                                    popover-target-props
                                    tp-keys
                                    {:aria-disabled disabled
                                     :aria-expanded open?
                                     :aria-haspopup (when filterable "listbox")
                                     :class (bp/cx class (:class popover-target-props)
                                                   {(bp/cls "fill") fill})
                                     :on-key-down
                                     (fn [e]
                                       (if open?
                                         ((:handle-key-down lp) e)
                                         (handle-target-key-down e))
                                       (call (:on-key-down popover-target-props) e))
                                     :on-key-up
                                     (fn [e]
                                       (when open? ((:handle-key-up lp) e))
                                       (call (:on-key-up popover-target-props) e))
                                     :ref ref
                                     :role (when-not filterable "combobox")})]
                        children))))
            (render-query-list [lp]
              (let [{:keys [filterable disabled input-props placeholder popover-content-props
                            popover-props]
                     :or   {filterable true placeholder "Filter..."}} (props)
                    open? (:open? @state)
                    query (:query lp)
                    input [input-group/input-group
                           (merge {:aria-activedescendant (:active-item-id lp)
                                   :aria-autocomplete     "list"
                                   :aria-expanded         open?
                                   :left-icon             :search
                                   :placeholder           placeholder
                                   :right-element
                                   (when (seq query)
                                     [button/button {:aria-label "Clear filter query"
                                                     :icon       :cross
                                                     :on-click   reset-query!
                                                     :title      "Clear filter query"
                                                     :variant    :minimal}])
                                   :role                  "combobox"}
                                  (dissoc input-props :input-ref)
                                  {:input-ref handle-input-ref
                                   :on-change (:handle-query-change lp)
                                   :value     query})]]
                [popover/popover
                 (merge {:autofocus     false
                         :enforce-focus false
                         :disabled      disabled
                         :placement     (if (or (:position popover-props)
                                                (:placement popover-props))
                                          nil :bottom-start)}
                        (dissoc popover-props :on-interaction :on-opening :on-opened
                                :on-closing)
                        {:is-open            open?
                         :class              (bp/cx (:class lp) (:class popover-props))
                         :content            [:div (merge popover-content-props
                                                          {:on-key-down (:handle-key-down lp)
                                                           :on-key-up   (:handle-key-up lp)})
                                              (when filterable input)
                                              (:item-list lp)]
                         :on-closing         handle-closing
                         :on-interaction     handle-interaction
                         :on-opened          handle-opened
                         :on-opening         handle-opening
                         :popover-class-name (bp/cx (bp/cls "select-popover")
                                                    (:popover-class-name popover-props))
                         :popup-kind         "listbox"
                         :render-target      (target-renderer lp open?)})]))]
      (fn [& args]
        (let [[p children] (if (map? (first args)) [(first args) (rest args)] [{} args])
              p (assoc p :children children)]
          (reset! cur p)
          @state
          [ql/query-list (merge (apply dissoc p :children own-keys)
                                {::outer         p
                                 :list-id        listbox-id
                                 :menu-props     (merge {:aria-label "selectable options"}
                                                        (:menu-props p))
                                 :on-item-select handle-item-select
                                 :api-ref        #(reset! api %)
                                 :renderer       render-query-list})])))))
