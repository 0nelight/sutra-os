(ns blueprint-cljs.segmented-control
  "SegmentedControl: a row of buttons, one selected.
   `[segmented-control {:options [{:label \"List\" :value \"list\" :icon :list} …]
                        :value v :on-value-change (fn [value el])}]`
   Props: `:default-value` (uncontrolled), `:intent`, `:size`/`:large`/`:small`, `:fill`,
   `:inline`, `:disabled`, `:role` (\"radiogroup\" default, \"menu\", \"group\",
   \"toolbar\"); option keys `:label` `:value` `:disabled` `:icon`."
  (:require [reagent.core :as r]
            [blueprint-cljs.core :as bp]
            [blueprint-cljs.common :as c]
            [blueprint-cljs.button :as button]))

(defn- arrow-direction
  [e]
  (case (.-key e)
    ("ArrowLeft" "ArrowUp")    -1
    ("ArrowRight" "ArrowDown") 1
    nil))

(defn segmented-control
  [& args]
  (let [[init] (c/split-args args)
        local  (r/atom (:default-value init))
        outer  (atom nil)]
    (fn [& args]
      (let [[{:keys [class disabled fill inline intent large on-value-change options role
                     size small value]
              :or   {intent :none role "radiogroup" size :medium}
              :as   props}] (c/split-args args)
            selected (if (some? value) value @local)
            any?     (some #(= selected (:value %)) options)
            radio?   (#{"radiogroup" "menu"} role)
            btn-role ({"radiogroup" "radio" "menu" "menuitemradio"} role)]
        (into
         [:div (merge (bp/dom-props props [:class :default-value :disabled :fill :inline
                                           :intent :large :on-value-change :options :role
                                           :size :small :value])
                      {:role        role
                       :ref         #(reset! outer %)
                       :class       (bp/cx (bp/cls "segmented-control") class
                                           {(bp/cls "fill")   fill
                                            (bp/cls "inline") inline})
                       :on-key-down
                       (fn [e]
                         (when radio?
                           (when-let [d (arrow-direction e)]
                             (when-let [o @outer]
                               (let [focused (some-> js/document.activeElement
                                                     (.closest "button"))
                                     enabled (vec (array-seq
                                                   (.querySelectorAll
                                                    o "button:not(:disabled)")))
                                     idx     (.indexOf (clj->js enabled) focused)]
                                 (when (and focused (>= idx 0))
                                   (.preventDefault e)
                                   (let [nxt (nth enabled (mod (+ idx d) (count enabled)))]
                                     (.click nxt)
                                     (.focus nxt))))))))})]
         (for [[i {:keys [label icon] v :value option-disabled :disabled :as option}]
               (map-indexed vector options)
               :let [sel? (= selected v)]]
           ^{:key (str v)}
           [button/button
            (merge (dissoc option :label :value)
                   {:disabled (or option-disabled disabled)
                    :intent   intent
                    :size     size
                    :large    large
                    :small    small
                    :role     btn-role
                    :icon     icon
                    :text     (or label v)
                    :variant  (when-not sel? :minimal)
                    :on-click (fn [e]
                                (reset! local v)
                                (when on-value-change
                                  (on-value-change v (.-currentTarget e))))}
                   (if radio?
                     {:aria-checked sel?
                      :tab-index    (if (or sel? (and (zero? i) (not any?))) 0 -1)}
                     {:aria-pressed sel?}))]))))))
