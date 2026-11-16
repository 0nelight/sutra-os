(ns blueprint-cljs.dialog
  "Dialog, DialogBody, DialogFooter, MultistepDialog with DialogStep."
  (:require [reagent.core :as r]
            [blueprint-cljs.core :as bp]
            [blueprint-cljs.button :as button]
            [blueprint-cljs.icon :as icon]
            [blueprint-cljs.overlay :as overlay]))

(defn- split-args [args]
  (if (map? (first args)) [(first args) (rest args)] [{} args]))

(defonce ^:private ids (atom 0))

(defn close-button
  "The header's close button (shared with Drawer)."
  [on-close size]
  [button/button {:aria-label "Close" :class (bp/cls "dialog-close-button")
                  :icon [icon/icon {:icon :cross :size size}]
                  :on-click on-close :variant :minimal}])

(def ^:private dialog-keys
  [:is-open :on-close :icon :is-close-button-shown :role :style :title :title-tag-name
   :class :container-ref :aria-labelledby :aria-describedby :can-outside-click-close])

(defn dialog
  "Props: `:is-open`, `:on-close (fn [e])`, `:title`, `:icon`, `:is-close-button-shown`
   (default true), `:title-tag-name` (:h2), `:role` (\"dialog\"), `:style`, `:class`,
   `:container-ref`, `:aria-labelledby`, `:aria-describedby`, plus the overlay props
   (`:autofocus`, `:enforce-focus`, `:can-escape-key-close`, `:can-outside-click-close`,
   `:use-portal`, `:portal-container`, `:transition-duration`, `:on-opened`, …)."
  [& _]
  (let [title-id (str "title-bp-dialog-" (swap! ids inc))]
    (fn [& args]
      (let [[props children] (split-args args)
            {:keys [is-open on-close icon is-close-button-shown role style title
                    title-tag-name class container-ref can-outside-click-close]
             :or   {role "dialog" title-tag-name :h2 can-outside-click-close true}} props
            overlay-props (apply dissoc props dialog-keys)]
        [overlay/overlay
         (merge overlay-props
                {:is-open                 (boolean is-open)
                 :can-outside-click-close can-outside-click-close
                 :class                   (bp/cls "overlay-scroll-container")
                 :has-backdrop            true
                 :on-close                on-close})
         [:div {:class (bp/cls "dialog-container") :ref container-ref}
          (into
           [:div {:aria-describedby (:aria-describedby props)
                  :aria-labelledby  (or (:aria-labelledby props) (when title title-id))
                  :aria-modal       (if (some? (:enforce-focus props))
                                      (:enforce-focus props) true)
                  :class            (bp/cx (bp/cls "dialog") class)
                  :role             role
                  :style            style}
            (when title
              [:div {:class (bp/cls "dialog-header")}
               (icon/render-icon icon {:size 16 :aria-hidden true :tab-index -1})
               [(keyword title-tag-name) {:id title-id :class (bp/cls "heading")} title]
               (when-not (false? is-close-button-shown)
                 [close-button on-close 16])])]
           children)]]))))

(defn dialog-body
  "Props: `:use-overflow-scroll-container` (default true), `:class`."
  [& args]
  (let [[{:keys [use-overflow-scroll-container class] :as props
          :or   {use-overflow-scroll-container true}} children] (split-args args)]
    (into [:div (merge (bp/dom-props props [:use-overflow-scroll-container :class])
                       {:role  "dialogbody"
                        :class (bp/cx (bp/cls "dialog-body") class
                                      {(bp/cls "dialog-body-scroll-container")
                                       use-overflow-scroll-container})})]
          children)))

(defn dialog-footer
  "Props: `:actions` (hiccup, right-aligned), `:minimal` (no fixed bar), `:class`; children
   go to the main (left) section."
  [& args]
  (let [[{:keys [actions minimal class] :as props} children] (split-args args)]
    [:div (merge (bp/dom-props props [:actions :minimal :class])
                 {:class (bp/cx (bp/cls "dialog-footer") class
                                {(bp/cls "dialog-footer-fixed") (not minimal)})})
     (into [:div {:class (bp/cls "dialog-footer-main-section")}] children)
     (when actions [:div {:class (bp/cls "dialog-footer-actions")} actions])]))

(defn dialog-step
  "A step of a multistep dialog — data only. Props: `:id`, `:title`, `:panel`,
   `:panel-class-name`, `:back-button-props`, `:next-button-props`."
  [& _]
  nil)

(defn- step-props [step]
  (when (and (vector? step) (= dialog-step (first step)) (map? (second step)))
    (second step)))

(def ^:private multistep-keys
  [:back-button-props :close-button-props :final-button-props :navigation-position
   :next-button-props :on-change :reset-on-close :show-close-button-in-footer
   :initial-step-index])

(defn multistep-dialog
  "Props: Dialog props plus `:navigation-position` (:left :top :right), `:on-change
   (fn [new-id prev-id event])`, `:initial-step-index`, `:reset-on-close` (true),
   `:show-close-button-in-footer`, `:back-button-props`, `:next-button-props`,
   `:final-button-props`, `:close-button-props` (button props incl. `:text`). Children
   are `[dialog-step {...}]`."
  [& args]
  (let [[props0 children0] (split-args args)
        init  (fn [props children]
                (let [n (count (keep step-props children))
                      i (if-let [i (:initial-step-index props)]
                          (max 0 (min i (dec n))) 0)]
                  {:last-viewed i :selected i}))
        state (r/atom (init props0 children0))]
    (r/create-class
     {:display-name "blueprint-cljs.multistep-dialog"
      :component-did-update
      (fn [this [_ old]]
        (let [[props children] (split-args (rest (r/argv this)))
              old (if (map? old) old {})]
          (when (and (or (:reset-on-close old true)
                         (not= (:initial-step-index old) (:initial-step-index props)))
                     (not (:is-open old)) (:is-open props))
            (reset! state (init props children)))))
      :reagent-render
      (fn [& args]
        (let [[props children] (split-args args)
              {:keys [navigation-position show-close-button-in-footer on-close
                      close-button-props on-change class style]
               :or   {navigation-position :left}} props
              steps   (vec (keep step-props children))
              {:keys [selected last-viewed]} @state
              go      (fn [i]
                        (fn [e]
                          (when on-change
                            (on-change (:id (get steps i)) (:id (get steps selected)) e))
                          (reset! state {:last-viewed (max last-viewed i) :selected i})))
              cur     (get steps selected)
              pos     (keyword navigation-position)
              step-btn (fn [defaults overrides]
                         (let [p (merge defaults overrides)]
                           [button/button (dissoc p :text) (:text p)]))]
          [dialog (merge (apply dissoc props multistep-keys)
                         {:class (bp/cx {(bp/cls "multistep-dialog-nav-right") (= pos :right)
                                         (bp/cls "multistep-dialog-nav-top") (= pos :top)}
                                        class)
                          :style (merge {:min-width 800 :padding-bottom 0} style)})
           [:div {:class (bp/cls "multistep-dialog-panels")}
            (into [:div {:class (bp/cls "multistep-dialog-left-panel") :role "tablist"
                         :aria-label "steps"}]
                  (map-indexed
                   (fn [i step]
                     (let [viewed?  (>= last-viewed i)
                           current? (= selected i)
                           click    (when (<= i last-viewed) (go i))]
                       ^{:key i}
                       [:div {:class         (bp/cx (bp/cls "dialog-step-container")
                                                    {(bp/cls "active") current?
                                                     (bp/cls "dialog-step-viewed") viewed?})
                              :aria-disabled (and (not current?) (not viewed?))
                              :aria-selected current?
                              :role          "tab"}
                        [:div {:class     (bp/cls "dialog-step")
                               :on-click  click
                               :tab-index (if click 0 -1)
                               :role      "button"
                               :on-key-down #(when (and click (contains? #{"Enter" " "}
                                                                         (.-key %)))
                                               (click %))}
                         [:div {:class (bp/cls "dialog-step-icon")} (inc i)]
                         [:div {:class (bp/cls "dialog-step-title")} (:title step)]]]))
                   steps))
            (when cur
              [:div {:class (bp/cx (bp/cls "multistep-dialog-right-panel")
                                   (:panel-class-name cur))}
               (:panel cur)
               [dialog-footer
                {:actions
                 [:<>
                  (when (pos? selected)
                    ^{:key "back"}
                    [step-btn {:text "Back" :on-click (go (dec selected))}
                     (or (:back-button-props cur) (:back-button-props props))])
                  (if (= selected (dec (count steps)))
                    ^{:key "final"}
                    [step-btn {:intent :primary :text "Submit"} (:final-button-props props)]
                    ^{:key "next"}
                    [step-btn {:intent :primary :text "Next" :on-click (go (inc selected))}
                     (or (:next-button-props cur) (:next-button-props props))])]}
                (when show-close-button-in-footer
                  [step-btn {:text "Close" :on-click on-close} close-button-props])]])]]))})))
