(ns blueprint-cljs.table.headers
  "ColumnHeaderCell, RowHeaderCell, EditableName and HorizontalCellDivider. The table
   hands each header `:index`, `:style`, selection state and its resize handle."
  (:require [reagent.core :as r]
            [blueprint-cljs.core :as bp]
            [blueprint-cljs.common :as c]
            [blueprint-cljs.context-menu :as context-menu]
            [blueprint-cljs.editable-text :refer [editable-text]]
            [blueprint-cljs.icon :refer [icon]]
            [blueprint-cljs.popover :refer [popover]]
            [blueprint-cljs.table.cells :as cell]))

(def ^:private header-keys
  [:index :is-active :is-selected :is-column-selected :is-row-selected :loading :class
   :menu-renderer :menu-icon :menu-popover-props :name :name-renderer :resize-handle
   :enable-column-interaction-bar :enable-column-reordering :enable-row-reordering
   :reorder-handle :select-cells-on-menu-click :is-dark-theme])

(defn- header-cell
  "Blueprint's HeaderCell: the shared header box; right-click shows `:menu-renderer`."
  [{:keys [index is-active is-selected loading menu-renderer reorderable] :as props}
   & children]
  (into [:div (merge (bp/dom-props props (conj header-keys :reorderable))
                     {:class (bp/cx (bp/cls "table-header")
                                    {(bp/cls "table-header-active")      is-active
                                     (bp/cls "table-header-selected")    is-selected
                                     (bp/cls "table-header-reorderable") reorderable
                                     (bp/cls "loading")                  loading}
                                    (:class props))
                      :on-context-menu
                      (when menu-renderer
                        (fn [e]
                          (.preventDefault e)
                          (.stopPropagation e)
                          (context-menu/show-context-menu!
                           {:content       (menu-renderer index)
                            :target-offset {:left (.-clientX e) :top (.-clientY e)}
                            :is-dark-theme (some? (.closest (.-target e)
                                                            (str "." bp/dark-class)))})))})]
        children))

(defn horizontal-cell-divider
  []
  [:div {:class (bp/cls "table-horizontal-cell-divider")}])

(defn- th-menu
  [_]
  (let [open? (r/atom false)]
    (fn [{:keys [index menu-renderer menu-icon menu-popover-props select-cells-on-menu-click]}]
      [:div {:class (bp/cx (bp/cls "table-th-menu-container")
                           {(bp/cls "table-th-menu-open")         @open?
                            (bp/cls "table-th-menu-select-cells") select-cells-on-menu-click})}
       [:div {:class (bp/cls "table-th-menu-container-background")}]
       [popover (merge {:placement :bottom}
                       menu-popover-props
                       {:class     (bp/cx (bp/cls "table-th-menu") (:class menu-popover-props))
                        :content   (menu-renderer index)
                        :on-opened (fn [& _] (reset! open? true))
                        :on-closing (fn [& _] (reset! open? false))})
        [icon {:icon (or menu-icon :chevron-down)}]]])))

(defn column-header-cell
  "Props: `:name`, `:name-renderer (fn [name index])`, `:menu-renderer (fn [index] menu)`,
   `:menu-icon` (:chevron-down), `:menu-popover-props`, `:select-cells-on-menu-click`,
   `:loading`, `:class`, `:style`; children render below the name (header content)."
  [& args]
  (let [[{:keys [name name-renderer index loading menu-renderer is-column-selected
                 enable-column-interaction-bar enable-column-reordering resize-handle
                 reorder-handle]
          :as props}
         children] (c/split-args args)
        menu      (when menu-renderer [th-menu props])
        name-comp [cell/loadable loading 0 index
                   (if name-renderer
                     (name-renderer name index)
                     [:div {:class (bp/cls "table-truncated-text")} name])]
        name-text [:div {:class (bp/cls "table-column-name-text")} name-comp]]
    [header-cell (merge (dissoc props :menu-renderer)
                        {:is-selected is-column-selected
                         :reorderable enable-column-reordering
                         :class (bp/cx (bp/cls "table-column-header-cell")
                                       {(bp/cls "table-has-interaction-bar")
                                        enable-column-interaction-bar
                                        (bp/cls "table-has-reorder-handle") reorder-handle}
                                       (:class props))}
                        ;; the th menu doubles as the header's context menu
                        (when menu-renderer {:menu-renderer menu-renderer}))
     (if enable-column-interaction-bar
       [:div {:class (bp/cls "table-column-name") :title name}
        [:div {:class (bp/cls "table-interaction-bar")} reorder-handle menu]
        [horizontal-cell-divider]
        name-text]
       [:div {:class (bp/cls "table-column-name") :title name}
        reorder-handle menu name-text])
     (when (seq children)
       (into [:div {:class (bp/cls "table-header-content")}] children))
     (when-not loading resize-handle)]))

(defn row-header-cell
  "Props: `:name`, `:name-renderer (fn [name index])`, `:menu-renderer`, `:loading`,
   `:class`, `:style`; children render after the name."
  [& args]
  (let [[{:keys [name name-renderer index loading is-row-selected enable-row-reordering
                 resize-handle]
          :as props}
         children] (c/split-args args)]
    (-> [header-cell (merge props {:is-selected is-row-selected
                                   :reorderable enable-row-reordering})
         [:div {:class (bp/cls "table-row-name")}
          [cell/loadable loading index 0
           (if name-renderer
             (name-renderer name index)
             [:div {:class (bp/cls "table-row-name-text")} name])]]]
        (into children)
        (conj (when-not loading resize-handle)))))

(defn editable-name
  "EditableName: a column name edited in place on double-click. Props: `:name`,
   `:index`, `:intent`, `:on-change`, `:on-cancel`, `:on-confirm` (each
   `(fn [value index])`), `:editable-text-props`."
  [_]
  (let [editing? (r/atom false)]
    (fn [{:keys [name index intent on-change on-cancel on-confirm editable-text-props]}]
      [:div {:on-double-click (fn [e] (.stopPropagation e) (reset! editing? true))
             :on-mouse-down   (fn [e] (when @editing? (.stopPropagation e)))}
       ;; remounted per edit: an uncontrolled editor never confirms a value one render old
       ^{:key (str @editing?)}
       [editable-text
        (merge editable-text-props
               {:class      (bp/cx (bp/cls "table-editable-name") (:class editable-text-props))
                :intent     intent
                :is-editing @editing?}
               (if @editing? {:default-value name} {:value name})
               {:on-change  (fn [v] (when on-change (on-change v index)))
                :on-cancel  (fn [_] (reset! editing? false)
                              (when on-cancel (on-cancel name index)))
                :on-confirm (fn [v] (reset! editing? false)
                              (when on-confirm (on-confirm v index)))})]])))
