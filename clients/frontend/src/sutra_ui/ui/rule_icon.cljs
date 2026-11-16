(ns sutra-ui.ui.rule-icon
  (:require [sutra-ui.ui.core :as ui]))

(def ^:private handset
  (str "M22 16.92v3a2 2 0 0 1-2.18 2 19.79 19.79 0 0 1-8.63-3.07 19.5 19.5 0 0 1-6-6 "
       "19.79 19.79 0 0 1-3.07-8.67A2 2 0 0 1 4.11 2h3a2 2 0 0 1 2 1.72 12.84 12.84 0 0 0 "
       ".7 2.81 2 2 0 0 1-.45 2.11L8.09 9.91a16 16 0 0 0 6 6l1.27-1.27a2 2 0 0 1 2.11-.45 "
       "12.84 12.84 0 0 0 2.81.7A2 2 0 0 1 22 16.92z"))

(def ^:private lines
  {"phone-incoming" [handset "M16 2v6h6" "M22 2l-6 6"]
   "phone-outgoing" [handset "M22 8V2h-6" "M16 8l6-6"]
   "phone-missed"   [handset "M22 2l-6 6" "M16 2l6 6"]
   "phone-off"
   [(str "M10.68 13.31a16 16 0 0 0 3.41 2.6l1.27-1.27a2 2 0 0 1 2.11-.45 12.84 12.84 0 0 0 "
         "2.81.7 2 2 0 0 1 1.72 2v3a2 2 0 0 1-2.18 2 19.79 19.79 0 0 1-8.63-3.07 19.42 "
         "19.42 0 0 1-3.33-2.67m-2.67-3.34a19.79 19.79 0 0 1-3.07-8.63A2 2 0 0 1 4.11 2h3a2 "
         "2 0 0 1 2 1.72 12.84 12.84 0 0 0 .7 2.81 2 2 0 0 1-.45 2.11L8.09 9.91")
    "M22 2L2 22"]})

(defn icon
  [x]
  (let [n (some-> x name)]
    (cond
      (nil? n) nil

      (contains? lines n)
      [:svg {:width 16 :height 16 :view-box "0 0 24 24" :fill "none" :stroke "currentColor"
             :stroke-width 2 :stroke-linecap "round" :stroke-linejoin "round"
             :aria-hidden true :style {:vertical-align "text-bottom"}}
       (for [d (get lines n)] ^{:key d} [:path {:d d}])]

      (get-in @ui/registry [16 n])
      [ui/icon {:icon n}]

      :else n)))

(defn before
  [x]
  (when-let [i (icon x)]
    [:span {:style {:margin-right "0.3em"}} i]))
