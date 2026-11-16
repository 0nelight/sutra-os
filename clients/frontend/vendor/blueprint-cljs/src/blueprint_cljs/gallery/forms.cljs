(ns blueprint-cljs.gallery.forms
  (:require [reagent.core :as r]
            [blueprint-cljs.button :as button]
            [blueprint-cljs.checkbox :refer [checkbox]]
            [blueprint-cljs.control-card :refer [checkbox-card radio-card switch-card]]
            [blueprint-cljs.control-group :refer [control-group]]
            [blueprint-cljs.editable-text :refer [editable-text]]
            [blueprint-cljs.file-input :refer [file-input]]
            [blueprint-cljs.form-group :refer [form-group]]
            [blueprint-cljs.html-select :refer [html-select]]
            [blueprint-cljs.input-group :refer [input-group]]
            [blueprint-cljs.label :refer [label]]
            [blueprint-cljs.multi-slider :refer [multi-slider]]
            [blueprint-cljs.numeric-input :refer [numeric-input]]
            [blueprint-cljs.radio :refer [radio]]
            [blueprint-cljs.radio-group :refer [radio-group]]
            [blueprint-cljs.range-slider :refer [range-slider]]
            [blueprint-cljs.segmented-control :refer [segmented-control]]
            [blueprint-cljs.slider :refer [slider]]
            [blueprint-cljs.switch :refer [switch]]
            [blueprint-cljs.tag-input :refer [tag-input]]
            [blueprint-cljs.text-area :refer [text-area]]))

(def ^:private intents [:none :primary :success :warning :danger])

(defn- row [& xs] (into [:div {:style {:display "flex" :gap 12 :align-items "center"
                                       :flex-wrap "wrap"}}] xs))

(defn- col [w & xs] (into [:div {:style {:display "flex" :flex-direction "column"
                                         :gap 10 :width w}}] xs))

(defn- form-group-demo
  []
  [row
   [col 300
    [form-group {:label "Label" :label-for "fg-1" :label-info "(required)"
                 :helper-text "Helper text with details…" :sub-label "Sub label"}
     [input-group {:id "fg-1" :placeholder "Placeholder text"}]]
    [form-group {:label "Danger" :intent :danger :helper-text "This field is required"}
     [input-group {:intent :danger :placeholder "Placeholder text"}]]
    [form-group {:label "Disabled" :disabled true :helper-text "Can't touch this"}
     [input-group {:disabled true :placeholder "Placeholder text"}]]]
   [col 380
    [form-group {:label "Inline" :inline true :label-info "(optional)"}
     [input-group {:placeholder "Inline"}]]]])

(defn- label-demo
  []
  [col 300
   [label "Label A" [:input {:class "bp-input" :placeholder "Text input"}]]
   [label {:label-info "(required)"} "Label B"
    [:input {:class "bp-input" :placeholder "Text input"}]]
   [label {:inline true} "Inline" [:input {:class "bp-input" :placeholder "Text input"}]]
   [label {:disabled true} "Disabled"
    [:input {:class "bp-input" :disabled true :placeholder "Disabled"}]]])

(defn- input-group-demo
  []
  (let [v    (r/atom "")
        show (r/atom false)]
    (fn []
      [col 360
       [input-group {:left-icon :filter :placeholder "Filter histogram…" :value @v
                     :async-control true
                     :on-value-change #(reset! v %)}]
       [input-group {:type (if @show "text" "password") :placeholder "Enter your password…"
                     :right-element [button/button {:icon     (if @show :unlock :lock)
                                                    :variant  :minimal
                                                    :on-click #(swap! show not)}]}]
       [input-group {:left-icon :search :round true :placeholder "Round, search"}]
       [input-group {:left-element [:span {:style {:padding "0 8px" :line-height "30px"}}
                                    "@"]
                     :placeholder "Left element"}]
       (into [:<>]
             (for [i intents]
               ^{:key i}
               [input-group {:intent i :left-icon :tag :placeholder (str "Intent " (name i))}]))
       [input-group {:size :small :left-icon :search :placeholder "Small"}]
       [input-group {:size :large :left-icon :search :placeholder "Large"
                     :right-element [button/button {:icon :arrow-right :variant :minimal}]}]
       [input-group {:read-only true :value "Read only"}]
       [input-group {:disabled true :left-icon :lock :placeholder "Disabled"}]
       [input-group {:fill true :placeholder "Fill"}]])))

(defn- text-area-demo
  []
  (let [v (r/atom "Type here, the field grows with its content.")]
    (fn []
      [col 360
       [text-area {:auto-resize true :fill true :value @v
                   :on-change #(reset! v (.. % -target -value))}]
       [text-area {:placeholder "Primary, small" :intent :primary :size :small}]
       [text-area {:placeholder "Large" :size :large}]
       [text-area {:placeholder "Disabled" :disabled true}]
       [text-area {:value "Read only" :read-only true}]])))

(defn- html-select-demo
  []
  (let [v (r/atom "b")]
    (fn []
      [col 260
       [html-select {:options ["Choose an item…" {:value "a" :label "One"}
                               {:value "b" :label "Two"} {:value "c" :label "Three"}]
                     :value @v :on-change #(reset! v (.. % -target -value))}]
       [html-select {:minimal true :options ["Minimal" "Two"]}]
       [html-select {:large true :options ["Large" "Two"] :icon-name :caret-down}]
       [html-select {:fill true :options ["Fill"]}]
       [html-select {:disabled true :options ["Disabled"]}]])))

(defn- checkbox-demo
  []
  (let [a (r/atom true)
        ind (r/atom true)]
    (fn []
      [row
       [col 220
        [checkbox {:label "Enabled" :checked @a :on-change #(swap! a not)}]
        [checkbox {:label "Indeterminate" :indeterminate @ind
                   :on-change #(reset! ind false)}]
        [checkbox {:label "Disabled" :disabled true}]
        [checkbox {:label "Disabled, checked" :disabled true :checked true}]
        [checkbox {:label "Large" :size :large :default-checked true}]
        [checkbox {:label "Right aligned" :align-indicator :end}]]
       [col 300
        [checkbox {:label "Inline A" :inline true}]
        [checkbox {:label "Inline B" :inline true :default-checked true}]
        [checkbox {:label-element [:span "With " [:strong "label element"]]}]]])))

(defn- radio-demo
  []
  (let [v (r/atom "two")]
    (fn []
      [row
       [col 200
        [radio {:label "One" :name "r" :value "one" :checked (= @v "one")
                :on-change #(reset! v "one")}]
        [radio {:label "Two" :name "r" :value "two" :checked (= @v "two")
                :on-change #(reset! v "two")}]
        [radio {:label "Disabled" :disabled true}]
        [radio {:label "Large" :large true :checked true :read-only true}]]])))

(defn- radio-group-demo
  []
  (let [v (r/atom "lunch")
        w (r/atom "b")]
    (fn []
      [row
       [radio-group {:label "Determine lunch" :selected-value @v
                     :on-change #(reset! v (.. % -target -value))
                     :options [{:label "Soup" :value "soup"}
                               {:label "Salad" :value "salad"}
                               {:label "Sandwich" :value "lunch"}
                               {:label "Nothing" :value "none" :disabled true}]}]
       [radio-group {:label "Inline, children" :inline true :selected-value @w
                     :on-change #(reset! w (.. % -target -value))}
        [radio {:label "A" :value "a"}]
        [radio {:label "B" :value "b"}]
        [radio {:label "C" :value "c"}]]])))

(defn- switch-demo
  []
  (let [a (r/atom true)]
    (fn []
      [col 260
       [switch {:label "Privacy setting" :checked @a :on-change #(swap! a not)}]
       [switch {:label "Inner labels" :inner-label "off" :inner-label-checked "on"
                :default-checked true}]
       [switch {:label "Large" :large true :default-checked true}]
       [switch {:label "Disabled" :disabled true}]
       [switch {:label "Disabled, on" :disabled true :checked true}]
       [switch {:label "Right aligned" :align-indicator :end}]])))

(defn- control-group-demo
  []
  [col 520
   [control-group
    [html-select {:options ["Filter…" "Name" "Size"]}]
    [input-group {:placeholder "Find filters…"}]
    [button/button {:icon :arrow-right}]]
   [control-group {:fill true}
    [button/button {:icon :filter} "Filter"]
    [input-group {:placeholder "Find filters…"}]]
   [control-group {:vertical true :style {:width 240}}
    [input-group {:left-icon :person :placeholder "Username"}]
    [input-group {:left-icon :lock :placeholder "Password" :type "password"}]
    [button/button {:intent :primary} "Login"]]])

(defn- control-card-demo
  []
  (let [chk (r/atom #{"email"})
        rad (r/atom "b")]
    (fn []
      [row
       [col 260
        (for [k ["email" "sms" "push"]]
          ^{:key k}
          [checkbox-card {:label (str "Notify via " k) :checked (contains? @chk k)
                          :on-change #(swap! chk (fn [s] (if (s k) (disj s k) (conj s k))))}])]
       [radio-group {:selected-value @rad :on-change #(reset! rad (.. % -target -value))}
        [radio-card {:value "a" :label "Plan A"}]
        [radio-card {:value "b" :label "Plan B"}]
        [radio-card {:value "c" :label "Plan C (disabled)" :disabled true}]]
       [col 260
        [switch-card {:label "Wifi" :default-checked true}]
        [switch-card {:label "Bluetooth"}]
        [switch-card {:label "Compact, elevated" :compact true :elevation 2}]]])))

(defn- numeric-input-demo
  []
  (let [v (r/atom 5)]
    (fn []
      [col 260
       [numeric-input {:value @v :min 0 :max 10 :on-value-change (fn [n _ _] (reset! v n))
                       :left-icon :dollar}]
       [:div "Value: " (str @v)]
       [numeric-input {:placeholder "Buttons left, primary" :button-position :left
                       :intent :primary}]
       [numeric-input {:placeholder "No buttons" :button-position :none}]
       [numeric-input {:placeholder "Small" :size :small :minor-step-size 0.01}]
       [numeric-input {:placeholder "Large, fill" :size :large :fill true}]
       [numeric-input {:value 3 :disabled true}]
       [numeric-input {:default-value 2 :clamp-value-on-blur true :min 0 :max 5
                       :intent :danger}]])))

(defn- slider-demo
  []
  (let [a (r/atom 2.5)
        b (r/atom 70)
        c (r/atom 30)]
    (fn []
      [col 420
       [slider {:min 0 :max 10 :step-size 0.1 :label-step-size 2 :value @a
                :on-change #(reset! a %) :label-precision 1}]
       [slider {:min 0 :max 100 :label-step-size 25 :value @b :on-change #(reset! b %)
                :intent :success :label-renderer #(str % "%")}]
       [slider {:min -50 :max 50 :label-step-size 25 :value @c :initial-value 0
                :on-change #(reset! c %) :intent :warning}]
       [slider {:value 4 :disabled true}]
       [:div {:style {:height 200 :display "flex" :gap 40}}
        [slider {:vertical true :value 6}]
        [slider {:vertical true :value 3 :label-renderer false :intent :danger}]]])))

(defn- range-slider-demo
  []
  (let [v (r/atom [3 7])]
    (fn []
      [col 420
       [range-slider {:min 0 :max 10 :value @v :on-change #(reset! v %)}]
       [range-slider {:min 0 :max 100 :label-step-size 20 :value [10 80] :intent :success}]
       [range-slider {:value [2 6] :disabled true}]])))

(defn- multi-slider-demo
  []
  (let [v (r/atom [20 40 70 90])]
    (fn []
      (let [[a b c d] @v]
        [col 460
         [multi-slider {:min 0 :max 100 :label-step-size 20 :default-track-intent :success
                        :on-change #(reset! v %)
                        :handles [{:value a :type :start :intent-before :danger}
                                  {:value b :type :start :intent-before :warning}
                                  {:value c :type :end :intent-after :warning}
                                  {:value d :type :end :intent-after :danger}]}]
         [:div "Values: " (pr-str @v)]]))))

(defn- segmented-control-demo
  []
  (let [v (r/atom "list")]
    (fn []
      [col 420
       [segmented-control {:value @v :on-value-change (fn [x _] (reset! v x))
                           :options [{:label "List" :value "list" :icon :list}
                                     {:label "Grid" :value "grid" :icon :grid-view}
                                     {:label "Gallery" :value "gallery" :icon :media}]}]
       [segmented-control {:default-value "b" :intent :primary :size :small
                           :options [{:label "A" :value "a"} {:label "B" :value "b"}
                                     {:label "C" :value "c" :disabled true}]}]
       [segmented-control {:default-value "x" :large true :fill true
                           :options [{:label "Large" :value "x"} {:label "Fill" :value "y"}]}]
       [segmented-control {:default-value "1" :disabled true
                           :options [{:label "Disabled" :value "1"} {:label "Two" :value "2"}]}]])))

(defn- file-input-demo
  []
  (let [n (r/atom nil)]
    (fn []
      [col 320
       [file-input {:text (or @n "Choose file...") :has-selection (some? @n)
                    :on-input-change #(reset! n (some-> % .-target .-files (aget 0) .-name))}]
       [file-input {:button-text "Upload" :fill true}]
       [file-input {:size :small}]
       [file-input {:size :large}]
       [file-input {:disabled true}]])))

(defn- editable-text-demo
  []
  (let [t (r/atom "")
        m (r/atom "")]
    (fn []
      [col 420
       [:h2 {:style {:margin 0}}
        [editable-text {:placeholder "Edit title…" :max-length 40 :value @t
                        :on-change #(reset! t %) :intent :primary}]]
       [editable-text {:multiline true :min-lines 3 :max-lines 12 :value @m
                       :placeholder "Edit report… (multiline, Ctrl+Enter confirms)"
                       :on-change #(reset! m %)}]
       [editable-text {:default-value "Always render input" :always-render-input true}]
       [editable-text {:default-value "Danger" :intent :danger}]
       [editable-text {:default-value "Disabled" :disabled true}]])))

(defn- tag-input-demo
  []
  (let [vs (r/atom ["Albert" "Bose" "Curie" "Dirac"])]
    (fn []
      [col 440
       [tag-input {:values @vs :on-change #(reset! vs %) :left-icon :user
                   :placeholder "Separate values with commas…"
                   :right-element [button/button {:icon :cross :variant :minimal
                                                  :on-click #(reset! vs [])}]
                   :tag-props {:minimal true}}]
       [tag-input {:values ["primary" "large"] :intent :primary :size :large
                   :on-change identity :tag-props {:intent :primary :minimal true}}]
       [tag-input {:values ["small"] :size :small :on-change identity
                   :placeholder "Auto-resize" :auto-resize true}]
       [tag-input {:values ["fill" "auto"] :fill true :on-change identity}]
       [tag-input {:values ["disabled"] :disabled true}]])))

(def sections
  "Each: {:id kw :title str :demo (fn [] hiccup)}."
  [{:id :form-group :title "FormGroup" :demo form-group-demo}
   {:id :label :title "Label" :demo label-demo}
   {:id :input-group :title "InputGroup" :demo input-group-demo}
   {:id :text-area :title "TextArea" :demo text-area-demo}
   {:id :html-select :title "HTMLSelect" :demo html-select-demo}
   {:id :checkbox :title "Checkbox" :demo checkbox-demo}
   {:id :radio :title "Radio" :demo radio-demo}
   {:id :radio-group :title "RadioGroup" :demo radio-group-demo}
   {:id :switch :title "Switch" :demo switch-demo}
   {:id :control-group :title "ControlGroup" :demo control-group-demo}
   {:id :control-card :title "ControlCard" :demo control-card-demo}
   {:id :numeric-input :title "NumericInput" :demo numeric-input-demo}
   {:id :slider :title "Slider" :demo slider-demo}
   {:id :range-slider :title "RangeSlider" :demo range-slider-demo}
   {:id :multi-slider :title "MultiSlider" :demo multi-slider-demo}
   {:id :segmented-control :title "SegmentedControl" :demo segmented-control-demo}
   {:id :file-input :title "FileInput" :demo file-input-demo}
   {:id :editable-text :title "EditableText" :demo editable-text-demo}
   {:id :tag-input :title "TagInput" :demo tag-input-demo}])
