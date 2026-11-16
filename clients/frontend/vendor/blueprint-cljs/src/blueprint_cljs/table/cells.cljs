(ns blueprint-cljs.table.cells
  "Cell and EditableCell2 of Blueprint's table. The table hands each cell its
   `:style` (position and size), `:row-index`, `:column-index` and classes."
  (:require [reagent.core :as r]
            [blueprint-cljs.core :as bp]
            [blueprint-cljs.common :as c]
            [blueprint-cljs.editable-text :refer [editable-text]]))

(def edit-event "bp-table-edit")

(defn skeleton-width
  "LoadableContent's variable length, stable per cell."
  [row col]
  (str (+ 40 (mod (+ (* 37 (or row 0)) (* 17 (or col 0))) 50)) "%"))

(defn loadable
  [loading? row col & children]
  (if loading?
    [:div {:class (bp/cls "skeleton") :style {:width (skeleton-width row col) :height 8}}]
    (into [:<>] children)))

(def ^:private cell-keys
  [:intent :interactive :loading :tooltip :truncated :wrap-text :cell-ref :row-index
   :column-index :class :editable-text-props :value :on-cancel :on-change :on-confirm])

(defn cell
  "Props: `:intent`, `:interactive`, `:loading`, `:tooltip` (title), `:truncated` (true),
   `:wrap-text`, `:tab-index`, `:cell-ref (fn [el])`, `:class`, `:style`, DOM props."
  [& args]
  (let [[{:keys [intent interactive loading tooltip truncated wrap-text cell-ref row-index
                 column-index]
          :or   {truncated true}
          :as   props}
         children] (c/split-args args)]
    [:div (merge (bp/dom-props props cell-keys)
                 {:class (bp/cx (bp/cls "table-cell") (bp/intent-class intent)
                                {(bp/cls "table-cell-interactive") interactive
                                 (bp/cls "loading")                loading
                                 (bp/cls "table-truncated-cell")   truncated}
                                (:class props))
                  :title tooltip}
                 (when cell-ref {:ref cell-ref}))
     [loadable loading row-index column-index
      (into [:div {:class (bp/cx {(bp/cls "table-truncated-text") truncated
                                  (bp/cls "table-no-wrap-text")   (not wrap-text)})}]
            children)]]))

(defn editable-cell
  "EditableCell2: double-click, F2 or typing on the focused cell edits; Enter or blur
   confirms, Escape cancels. Props: `:value`, `:on-change`, `:on-confirm`, `:on-cancel`
   (each `(fn [value row-index column-index])`), `:editable-text-props`, plus cell props."
  [& _]
  (let [st   (r/atom {:editing? false :dirty nil :saved nil :seen ::none})
        el   (atom nil)
        cur  (atom {})
        edit! (fn [initial]
                (swap! st (fn [s] (assoc s :editing? true
                                         :dirty (if (some? initial) initial (:saved s))))))
        on-edit-event (fn [e] (.preventDefault e) (edit! (.-detail e)))
        call (fn [k v] (when-let [f (get @cur k)]
                         (f v (:row-index @cur) (:column-index @cur))))]
    (r/create-class
     {:display-name "EditableCell2"
      :component-did-mount
      (fn [_] (some-> @el (.addEventListener edit-event on-edit-event)))
      :component-will-unmount
      (fn [_] (some-> @el (.removeEventListener edit-event on-edit-event)))
      :reagent-render
      (fn [& args]
        (let [[{:keys [value intent truncated wrap-text editable-text-props interactive]
                :as props} _] (c/split-args args)]
          (reset! cur props)
          (when (not= value (:seen @st))
            (swap! st assoc :seen value :saved value
                   :dirty (if (:editing? @st) (:dirty @st) value)))
          (let [{:keys [editing? dirty saved]} @st]
            [cell (merge (dissoc props :value :on-change :on-confirm :on-cancel
                                 :editable-text-props)
                         {:interactive    (or interactive editing?)
                          :truncated      false
                          :tab-index      (or (:tab-index props) 0)
                          :cell-ref       (fn [e] (reset! el e) nil)
                          :on-double-click (fn [e] (.stopPropagation e) (edit! nil))})
             (if editing?
               [editable-text
                (merge editable-text-props
                       {:is-editing           true
                        :class                (bp/cx (bp/cls "table-editable-text")
                                                     (bp/cls "table-editable-name")
                                                     (:class editable-text-props))
                        :intent               intent
                        :min-width            0
                        :placeholder          ""
                        :select-all-on-focus  false
                        :default-value        dirty
                        :on-change            (fn [v] (swap! st assoc :dirty v)
                                                (call :on-change v))
                        :on-cancel            (fn [_]
                                                (swap! st assoc :editing? false :dirty saved)
                                                (call :on-cancel saved)
                                                (some-> @el .focus))
                        :on-confirm           (fn [v]
                                                (swap! st assoc :editing? false :saved v
                                                       :dirty v)
                                                (call :on-confirm v)
                                                (some-> @el .focus))})]
               [:div {:class (bp/cx (bp/cls "table-editable-text")
                                    {(bp/cls "table-truncated-text") truncated
                                     (bp/cls "table-no-wrap-text")   (not wrap-text)})}
                saved])])))})))
