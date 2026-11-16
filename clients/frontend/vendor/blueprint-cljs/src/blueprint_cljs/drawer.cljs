(ns blueprint-cljs.drawer
  "Drawer: a panel sliding in from an edge of the screen."
  (:require [blueprint-cljs.core :as bp]
            [blueprint-cljs.dialog :as dialog]
            [blueprint-cljs.icon :as icon]
            [blueprint-cljs.overlay :as overlay]))

(def sizes {:small "360px" :standard "50%" :large "90%"})

(def ^:private own-keys
  [:is-open :on-close :icon :is-close-button-shown :position :size :style :title :class])

(defn- real-position [p]
  (let [p (name (or p :right))]
    (cond
      (re-find #"^top" p) "top" (re-find #"^bottom" p) "bottom"
      (re-find #"^left" p) "left" :else "right")))

(defn drawer
  "Props: `:is-open`, `:on-close`, `:position` (:top :bottom :left :right, default :right),
   `:size` (CSS length, or :small \"360px\" / :standard \"50%\" / :large \"90%\"), `:title`,
   `:icon`, `:is-close-button-shown`, `:style`, `:class`, plus overlay props
   (`:has-backdrop`, `:can-outside-click-close`, `:autofocus`, …). Children: content,
   typically `[:div.bp-drawer-body …]` and `[:div.bp-drawer-footer …]`."
  [& args]
  (let [[props children] (if (map? (first args)) [(first args) (rest args)] [{} args])
        {:keys [is-open on-close icon is-close-button-shown position size style title class
                has-backdrop]} props
        pos   (real-position position)
        size  (get sizes size size)
        style (cond-> (or style {})
                size (assoc (if (contains? #{"top" "bottom"} pos) :height :width) size))]
    [overlay/overlay
     (merge {:can-outside-click-close true}
            (apply dissoc props own-keys)
            {:is-open  (boolean is-open)
             :on-close on-close
             :class    (bp/cx {(bp/cls "overlay-container") has-backdrop})})
     (into [:div {:class (bp/cx (bp/cls "drawer") (bp/cls (str "position-" pos)) class)
                  :style style
                  :role "dialog"
                  :aria-label (when (string? title) (not-empty title))}
            (when title
              [:div {:class (bp/cls "drawer-header")}
               (icon/render-icon icon {:size 20})
               [:h4 {:class (bp/cls "heading")} title]
               (when-not (false? is-close-button-shown)
                 [dialog/close-button on-close 20])])]
           children)]))
