(ns blueprint-cljs.menu
  "Menu, MenuItem (with hover submenus), MenuDivider."
  (:require [blueprint-cljs.core :as bp]
            [blueprint-cljs.icon :as icon]
            [blueprint-cljs.popover :as popover]))

(defn- split-args [args]
  (if (map? (first args)) [(first args) (rest args)] [{} args]))

(defn menu
  "Props: `:size` (:small :medium :large) or `:small`/`:large`, `:class`; children are
   menu items. Renders a <ul role=menu>."
  [& args]
  (let [[{:keys [size small large class] :as props} children] (split-args args)
        size (some-> size name)]
    (into [:ul (merge {:role "menu"}
                      (bp/dom-props props [:size :small :large :class])
                      {:class (bp/cx (bp/cls "menu")
                                     {(bp/cls "small") (or (= size "small") small)
                                      (bp/cls "large") (or (= size "large") large)}
                                     class)})]
          children)))

(def ^:private item-keys
  [:text :active :disabled :icon :intent :label :label-class-name :label-element
   :role-structure :multiline :popover-props :selected :should-dismiss-popover
   :submenu-props :tag-name :text-class-name :html-title :class :li-class])

(defn menu-item
  "Props (Blueprint MenuItem): `:text`, `:icon`, `:label`, `:label-element`,
   `:label-class-name`, `:intent`, `:active`, `:selected`, `:disabled`,
   `:should-dismiss-popover` (default true), `:href`, `:on-click`, `:role-structure`
   (:menuitem :listoption :listitem :none), `:multiline`, `:tag-name` (default :a),
   `:text-class-name`, `:html-title`, `:popover-props`, `:submenu-props`; the rest goes to
   the target element. Children are submenu items, opened by hover to the right."
  [& args]
  (let [[props children] (split-args args)
        {:keys [text active disabled intent label label-element label-class-name
                role-structure multiline popover-props selected should-dismiss-popover
                submenu-props tag-name text-class-name html-title class li-class]
         :or   {role-structure :menuitem should-dismiss-popover true tag-name :a}} props
        icon-x       (:icon props)
        children     (seq (keep identity children))
        rs           (keyword role-structure)
        [li-role target-role aria-selected]
        (case rs
          :listoption ["option" nil (boolean selected)]
          :menuitem   ["none" "menuitem" nil]
          :none       ["none" nil nil]
          [nil nil nil])
        selectable?  (= rs :listoption)
        selected?    (and selectable? selected)
        submenu?     (some? children)
        target-attrs (merge
                      {:on-key-down (fn [e]
                                      (when (contains? #{"Enter" " "} (.-key e))
                                        (.preventDefault e)
                                        (.click (.-currentTarget e))))
                       :role        (if submenu? "none" target-role)}
                      (bp/dom-props props item-keys)
                      {:tab-index (if submenu? -1 (or (:tab-index props) 0))}
                      (when disabled
                        {:aria-disabled true :href nil :on-click nil :on-mouse-down nil
                         :on-mouse-enter nil :on-mouse-leave nil :tab-index -1})
                      {:class (bp/cx (bp/cls "menu-item") (bp/intent-class intent)
                                     {(bp/cls "active")   active
                                      (bp/cls "disabled") disabled
                                      popover/dismiss-class
                                      (and should-dismiss-popover (not disabled) (not submenu?))
                                      (bp/cls "menu-item-is-selectable") selectable?
                                      (bp/cls "selected") selected?}
                                     class)})
        target       [(keyword tag-name) target-attrs
                      (when selected?
                        [icon/icon {:icon :small-tick
                                    :class (bp/cls "menu-item-selected-icon")}])
                      (when icon-x
                        [:span {:class (bp/cls "menu-item-icon") :aria-hidden true}
                         (icon/render-icon icon-x {:tab-index -1})])
                      [:div {:class (bp/cx (bp/cls "fill") text-class-name
                                           {(bp/cls "text-overflow-ellipsis") (not multiline)})
                             :title html-title}
                       text]
                      (when (or label label-element)
                        [:span {:class (bp/cx (bp/cls "menu-item-label") label-class-name)}
                         label label-element])
                      (when submenu?
                        [icon/icon {:icon :caret-right :title "Open sub menu"
                                    :class (bp/cls "submenu-icon")}])]]
    [:li {:class         (bp/cx {(bp/cls "submenu") submenu?} li-class)
          :role          li-role
          :aria-selected aria-selected}
     (if-not submenu?
       target
       [popover/popover
        (merge {:autofocus          false
                :capture-dismiss    false
                :disabled           disabled
                :enforce-focus      false
                :hover-close-delay  0
                :interaction-kind   :hover
                :placement          :right-start
                :use-portal         false
                :target-props       {:role target-role :tab-index 0}
                :modifiers          {:flip   {:padding 20 :rootBoundary "viewport"}
                                     :offset {:crossAxis -5}
                                     :shift  {:padding 20 :rootBoundary "viewport"}
                                     :arrow  false}
                :transition-duration 100}
               popover-props
               {:content            (into [menu submenu-props] children)
                :minimal            true
                :popover-class-name (bp/cx (bp/cls "submenu")
                                           (bp/cls "popover-minimal-animation")
                                           (:popover-class-name popover-props))})
        target])]))

(defn menu-divider
  "Props: `:title` (a header instead of a line), `:title-id`, `:class`."
  [& args]
  (let [[{:keys [title title-id class]} _] (split-args args)]
    (if title
      [:li {:class (bp/cx (bp/cls "menu-header") class) :role "separator"}
       [:h6 {:class (bp/cls "heading") :id title-id} title]]
      [:li {:class (bp/cx (bp/cls "menu-divider") class) :role "separator"}])))
