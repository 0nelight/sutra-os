(ns sutra-ui.graph-editor.shared
  (:require [reagent.core :as r]
            [sutra-ui.ui.core :as ui]
            [sutra.i18n.core :as i18n]
            [sutra-ui.state.colors :as colors]
            [sutra-ui.state.graph :as graph]
            ["reactflow" :refer [Handle Position]]))

(def green "var(--c-success)")
(def blue  "var(--c-primary)")
(def grey  "var(--c-muted)")

(def default-cardinality
  "many-to-one")

(defn kind-accent
  [kind paint]
  (or paint (colors/node-kind-color kind)))

(def ink-dark "#1c2127")

(defn ink-on
  [bg]
  (if-let [[_ h] (re-matches #"#?([0-9a-fA-F]{6}|[0-9a-fA-F]{3})" (str bg))]
    (let [h   (if (= 3 (count h)) (apply str (mapcat #(repeat 2 %) h)) h)
          ch  #(js/parseInt (subs h % (+ % 2)) 16)
          lum (/ (+ (* 0.299 (ch 0)) (* 0.587 (ch 2)) (* 0.114 (ch 4))) 255)]
      (if (> lum 0.6) ink-dark "#fff"))
    "#fff"))

(defn hammer-icon
  [size]
  [:svg {:width size :height size :viewBox "0 0 24 24" :fill "currentColor"
         :aria-hidden true}
   [:g {:transform "rotate(-45 12 10)"}
    [:rect {:x 6 :y 2 :width 12 :height 6 :rx 1}]
    [:rect {:x 11 :y 8 :width 2 :height 11 :rx 1}]]
   [:rect {:x 3 :y 20 :width 11 :height 2 :rx 1}]])

(defn as-node
  [component]
  (fn [p] (r/as-element [component p])))

(def handle-base-style
  {:background "transparent" :border "none"
   :width "8px" :height "8px" :opacity 0})

(def full-handle-style
  {:position "absolute" :top 0 :left 0 :right 0 :bottom 0
   :width "100%" :height "100%"
   :background "transparent" :border "none"
   :transform "none" :z-index 10
   :cursor "crosshair" :opacity 0})

(def menu-wrapper-style
  {:background "var(--c-surface)" :border "1px solid var(--c-border)"
   :color "var(--c-text)" :border-radius "var(--radius)"
   :box-shadow "0 4px 12px rgba(17,20,24,0.15)"
   :font-family "sans-serif" :font-size "var(--fs-md)"
   :min-width "160px" :z-index 1000
   :padding "var(--sp-1) 0"
   :overflow-y "auto"})

(def menu-header-style
  {:padding "var(--sp-1) var(--sp-4)" :color "var(--c-muted)"
   :font-size "var(--fs-sm)" :font-weight "600"
   :text-transform "uppercase" :letter-spacing "0.05em"
   :border-bottom "1px solid var(--c-border-line)" :margin-bottom "var(--sp-1)"})

(def form-label-style
  {:font-size "var(--fs-xs)" :color "var(--c-muted)"
   :font-weight "600"
   :text-transform "uppercase"
   :letter-spacing "0.05em"})

(def edge-label-style
  {:position "absolute"
   :background "var(--c-surface)" :border "1px solid var(--c-border)"
   :border-radius "var(--radius)"
   :pointer-events "all" :display "flex"
   :align-items "center"
   :font-family "sans-serif"})

(def edge-delete-btn-style
  {:background "transparent" :border "none" :color "var(--c-danger)"
   :cursor "pointer" :padding "0" :line-height "1"})

(defn box-shadow-for
  [{:keys [staged? ctrl?]}]
  (cond
    ctrl?   (str "0 0 0 3px color-mix(in srgb, var(--c-success) 20%, transparent),"
                 " 0 2px 8px rgba(17,20,24,0.10)")
    staged? (str "0 0 0 2px color-mix(in srgb, var(--c-success) 18%, transparent),"
                 " 0 2px 8px rgba(17,20,24,0.10)")
    :else   "0 2px 8px rgba(17,20,24,0.10)"))

(defn get-node-center [^js node]
  (let [pos (or (.-positionAbsolute node) (.-position node))
        w   (or (.-width node) 180)
        h   (or (.-height node) 80)]
    {:x (+ (.-x pos) (/ w 2))
     :y (+ (.-y pos) (/ h 2))
     :w w :h h}))

(defn get-edge-position [nx ny cx cy]
  (let [dx (- cx nx) dy (- cy ny)]
    (if (> (abs dx) (abs dy))
      (if (pos? dx) Position.Right  Position.Left)
      (if (pos? dy) Position.Bottom Position.Top))))

(defn get-edge-params [source target]
  (let [{sx :x sy :y sw :w sh :h} (get-node-center source)
        {tx :x ty :y tw :w th :h} (get-node-center target)
        src-pos (get-edge-position sx sy tx ty)
        tgt-pos (get-edge-position tx ty sx sy)
        [src-x src-y] (condp = src-pos
                        Position.Left   [(- sx (/ sw 2)) sy]
                        Position.Right  [(+ sx (/ sw 2)) sy]
                        Position.Top    [sx (- sy (/ sh 2))]
                        Position.Bottom [sx (+ sy (/ sh 2))])
        [tgt-x tgt-y] (condp = tgt-pos
                        Position.Left   [(- tx (/ tw 2)) ty]
                        Position.Right  [(+ tx (/ tw 2)) ty]
                        Position.Top    [tx (- ty (/ th 2))]
                        Position.Bottom [tx (+ ty (/ th 2))])]
    {:sx src-x :sy src-y :sp src-pos
     :tx tgt-x :ty tgt-y :tp tgt-pos}))

(defn direction-handles
  [src-prefix tgt-prefix]
  (for [[role prefix] [["source" src-prefix] ["target" tgt-prefix]]
        [pos suffix]  [["top" "t"] ["bottom" "b"] ["left" "l"] ["right" "r"]]]
    ^{:key (str prefix "-" suffix)}
    [:> Handle {:type role :position pos
                :id (str prefix "-" suffix)
                :style handle-base-style}]))

(defn expand-arrows
  [node-id]
  (when (seq (str node-id))
    [:<>
     (for [[dir cls glyph title]
           [[:upstream   "graph-node__expand--in"  "‹" :ui/ge-expand-parents]
            [:downstream "graph-node__expand--out" "›" :ui/ge-expand-children]]]
       ^{:key (str dir)}
       [ui/tooltip {:content (i18n/t title) :class (str "graph-node__expand-slot " cls)}
        [:button {:class "graph-node__expand"
                  :on-click (fn [e]
                              (.stopPropagation e)
                              (graph/expand-node! node-id dir 1))}
         glyph]])]))

(defn full-overlay-handle
  [id]
  [:> Handle {:type "source" :position "top" :id id
              :is-connectable true :style full-handle-style}])
