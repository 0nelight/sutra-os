(ns blueprint-cljs.multi-select
  "MultiSelect (Blueprint select package): selected items as tags in a TagInput whose
   text filters a popover list.
   Props: everything `query-list` takes, plus `:selected-items`, `:tag-renderer
   (fn [item] node)`, `:on-remove (fn [item index])`, `:on-clear (fn [])` (shows a clear
   button), `:placeholder` (\"Search...\"), `:tag-input-props`, `:custom-target
   (fn [selected-items open?])`, `:open-on-key-down`, `:fill`, `:disabled`,
   `:menu-props`, `:popover-props`, `:popover-content-props`, `:popover-target-props`."
  (:require [reagent.core :as r]
            [blueprint-cljs.core :as bp]
            [blueprint-cljs.button :as button]
            [blueprint-cljs.popover :as popover]
            [blueprint-cljs.query-list :as ql]
            [blueprint-cljs.tag-input :as tag-input]))

(defonce ^:private id-counter (atom 0))

(def ^:private own-keys
  [:selected-items :tag-renderer :on-remove :on-clear :placeholder :tag-input-props
   :custom-target :open-on-key-down :fill :disabled :menu-props :popover-props
   :popover-content-props :popover-target-props])

(def ^:private defaults {:disabled false :fill false :placeholder "Search..."})

(def tag-input-input-class (bp/cls "multi-select-tag-input-input"))

(defn- call [f & args] (when f (apply f args)))

(defn multi-select
  [init]
  (let [state      (r/atom {:open? (boolean (get-in init [:popover-props :is-open]))})
        input-el   (atom nil)
        api        (atom nil)
        cur        (atom nil)
        listbox-id (str "listbox-" (swap! id-counter inc))
        props      (fn [] @cur)
        set-api!   #(reset! api %)]
    (letfn [(input-focused? [] (and @input-el (= @input-el (.-activeElement js/document))))
            (handle-input-ref [el]
              (reset! input-el el)
              (call (get-in (props) [:tag-input-props :input-ref]) el)
              nil)
            (handle-item-select [item e]
              (some-> @input-el .focus)
              (call (:on-item-select (props)) item e))
            (handle-query-change [query e]
              (let [p (props)]
                (swap! state assoc :open? (or (pos? (count query))
                                              (and (nil? (:custom-target p))
                                                   (not (:open-on-key-down p)))))
                (call (:on-query-change p) query e)))
            (handle-interaction [open? e]
              (let [p (props)]
                (if (some? (:custom-target p))
                  (do (swap! state assoc :open? open?)
                      (call (get-in p [:popover-props :on-interaction]) open? e))
                  (js/requestAnimationFrame
                   (fn []
                     (cond
                       (and @input-el (not (input-focused?))) (swap! state assoc :open? false)
                       (not (:open-on-key-down p))            (swap! state assoc :open? true))
                     (call (get-in p [:popover-props :on-interaction]) open? e))))))
            (handle-opened [& args]
              (let [p (props)]
                (some-> @api :scroll-active-item-into-view! (apply []))
                (when (and (some? (:custom-target p)) @input-el
                           (not (false? (get-in p [:tag-input-props :input-props
                                                   :auto-focus]))))
                  (.focus @input-el))
                (apply call (get-in p [:popover-props :on-opened]) args)))
            (handle-tag-remove [tag index]
              (let [{:keys [selected-items on-remove tag-input-props]} (props)]
                (call on-remove (nth (vec selected-items) index) index)
                (call (:on-remove tag-input-props) tag index)))
            (key-down-handler [ql-key-down]
              (fn [e]
                (let [k (.-key e) p (props)]
                  (cond
                    (contains? #{"Escape" "Tab"} k)
                    (do (when (= k "Escape")
                          (some-> @input-el .blur)
                          (.stopPropagation e)
                          (.preventDefault e))
                        (swap! state assoc :open? false))

                    (contains? #{"Backspace" "ArrowLeft" "ArrowRight"} k) nil

                    (some? (:custom-target p))
                    (when-not (ql/target-self-activates-on-key-up? e)
                      (case k
                        " "     (do (.preventDefault e) (swap! state assoc :open? true))
                        "Enter" (swap! state assoc :open? true)
                        nil))

                    :else (swap! state assoc :open? true))
                  (when (and (:open? @state)
                             (nil? (.closest (.-target e) (str "." (bp/cls "tag-remove")))))
                    (ql-key-down e))
                  (call (get-in p [:popover-target-props :on-key-down]) e))))
            (key-up-handler [ql-key-up]
              (fn [e]
                (when (and (:open? @state)
                           (.. e -target -classList (contains tag-input-input-class)))
                  (ql-key-up e))
                (call (get-in (props) [:popover-target-props :on-key-up]) e)))
            (tag-input-el [lp extra-class]
              (let [{:keys [disabled fill on-clear placeholder selected-items tag-input-props
                            tag-renderer]} (props)
                    clear (when (and on-clear (seq selected-items))
                            [button/button {:aria-label "Clear selected items"
                                            :disabled   disabled
                                            :icon       :cross
                                            :on-click   (fn [_] (on-clear))
                                            :title      "Clear selected items"
                                            :variant    :minimal}])]
                [tag-input/tag-input
                 (merge {:placeholder placeholder :right-element clear}
                        tag-input-props
                        {:class           (bp/cx extra-class (bp/cls "multi-select")
                                                 (:class tag-input-props))
                         :disabled        disabled
                         :fill            fill
                         :input-ref       handle-input-ref
                         :input-props     (assoc (:input-props tag-input-props)
                                                 :class (bp/cx (get-in tag-input-props
                                                                       [:input-props :class])
                                                               tag-input-input-class))
                         :input-value     (:query lp)
                         :on-add          (fn [values method]
                                            (when (= method :paste) ((:handle-paste lp) values))
                                            nil)
                         :on-input-change (:handle-query-change lp)
                         :on-remove       handle-tag-remove
                         :values          (mapv tag-renderer selected-items)})]))
            (target-renderer [lp open?]
              (fn [{:keys [ref class] :as tp}]
                (let [{:keys [custom-target disabled fill popover-props popover-target-props
                              selected-items]} (props)
                      tag (keyword (or (:target-tag-name popover-props) :div))]
                  [tag (merge {:aria-autocomplete "list" :aria-controls listbox-id}
                              popover-target-props
                              (dissoc tp :ref :class :is-open :aria-haspopup :aria-expanded)
                              {:aria-disabled disabled
                               :aria-expanded open?
                               :class         (bp/cx class (:class popover-target-props)
                                                     {(bp/cls "fill") fill})
                               :on-key-down   (key-down-handler (:handle-key-down lp))
                               :on-key-up     (key-up-handler (:handle-key-up lp))
                               :ref           ref
                               :role          "combobox"})
                   (if (some? custom-target)
                     (custom-target selected-items open?)
                     (tag-input-el lp nil))])))
            (render-query-list [lp]
              (let [{:keys [custom-target disabled popover-content-props popover-props]} (props)
                    open? (:open? @state)]
                [popover/popover
                 (merge {:autofocus            false
                         :can-escape-key-close true
                         :disabled             disabled
                         :enforce-focus        false
                         :placement            (if (or (:position popover-props)
                                                       (:placement popover-props))
                                                 nil :bottom-start)}
                        (dissoc popover-props :on-interaction :on-opened)
                        {:is-open            open?
                         :class              (bp/cx (:class lp) (:class popover-props))
                         :content
                         [:div (merge {:class (when (and (some? custom-target)
                                                         (not (:match-target-width
                                                               popover-props)))
                                                (bp/cls "multi-select-popover-default-width"))}
                                      popover-content-props
                                      {:on-key-down (:handle-key-down lp)
                                       :on-key-up   (:handle-key-up lp)})
                          (when (some? custom-target)
                            (tag-input-el lp (bp/cx (bp/cls "fill")
                                                    (bp/cls (str "multi-select-popover-"
                                                                 "tag-input-margin")))))
                          (:item-list lp)]
                         :interaction-kind   :click
                         :on-interaction     handle-interaction
                         :on-opened          handle-opened
                         :popover-class-name (bp/cx (bp/cls "multi-select-popover")
                                                    (:popover-class-name popover-props))
                         :popup-kind         "listbox"
                         :render-target      (target-renderer lp open?)})]))]
      (fn [p]
        (let [p (merge defaults p)]
          (reset! cur p)
          [ql/query-list (merge (apply dissoc p own-keys)
                                {::outer          p
                                 :menu-props      (merge {:aria-label "selectable options"}
                                                         (:menu-props p)
                                                         {:aria-multiselectable true
                                                          :id listbox-id})
                                 :on-item-select  handle-item-select
                                 :on-query-change handle-query-change
                                 :api-ref         set-api!
                                 :renderer        render-query-list})])))))
