(ns blueprint-cljs.async-input
  "Blueprint's AsyncControllableInput: a controlled text field whose `:value` may arrive
   late (an `on-change` that updates state asynchronously) or not at all while an IME
   composes. The field shows what was typed until the props catch up, so the caret stays."
  (:require [reagent.core :as r]))

(def composition-end-delay 10)

(defn- derive-state
  "Blueprint's getDerivedStateFromProps over our local state."
  [{:keys [composing? value next-value pending?] :as st} prop-value]
  (if (or composing? (undefined? prop-value))
    st
    (if (not= next-value value)
      (cond
        (= prop-value next-value)
        (if pending?
          (assoc st :pending? false :value prop-value)
          (assoc st :value next-value))

        (= prop-value value)
        (assoc st :pending? true)

        :else
        (assoc st :pending? false :next-value prop-value :value prop-value))
      (assoc st :pending? false :next-value prop-value :value prop-value))))

(defn- controllable
  [tag]
  (fn [_props]
    (let [st    (atom {:composing? false :pending? false})
          timer (atom nil)
          force (r/atom 0)
          init? (atom false)]
      (r/create-class
       {:display-name (str "AsyncControllable-" (name tag))
        :component-will-unmount #(some-> @timer js/clearTimeout)
        :reagent-render
        (fn [{:keys [value input-ref on-change on-composition-start on-composition-end]
              :as props}]
          @force
          (when-not @init?
            (reset! init? true)
            (swap! st assoc :value value :next-value value))
          (swap! st derive-state value)
          (let [{:keys [composing? pending? value next-value]} @st
                shown (if (or composing? pending?) next-value value)]
            [tag (merge (dissoc props :input-ref)
                        {:value      (if (nil? shown) "" shown)
                         :ref        input-ref
                         :on-composition-start
                         (fn [e]
                           (some-> @timer js/clearTimeout)
                           (swap! st assoc :composing? true)
                           (swap! force inc)
                           (when on-composition-start (on-composition-start e)))
                         :on-composition-end
                         (fn [e]
                           (reset! timer
                                   (js/setTimeout
                                    (fn []
                                      (swap! st assoc :composing? false)
                                      (swap! force inc))
                                    composition-end-delay))
                           (when on-composition-end (on-composition-end e)))
                         :on-change
                         (fn [e]
                           (swap! st assoc :next-value (.. e -target -value))
                           (swap! force inc)
                           (when on-change (on-change e)))})]))}))))

(def async-input
  "Props like a reagent `[:input]` (kebab-case handlers, `:class`, `:style`), plus
   `:input-ref` (fn of the node)."
  (controllable :input))

(def async-text-area
  (controllable :textarea))
