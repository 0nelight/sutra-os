(ns blueprint-cljs.overlay
  "Overlay (Blueprint Overlay2): renders children over the page with an optional
   backdrop, focus trap, ESC / outside-click close, body scroll lock and CSS transitions.
   Several open overlays form a stack: ESC and enforced focus belong to the top one,
   a click inside a later (nested) overlay is not outside of an earlier one."
  (:require [reagent.core :as r]
            [blueprint-cljs.core :as bp]
            [blueprint-cljs.portal :as portal]))

;; ------------------------------------------------------------------ transitions

(defn transition-classes
  "react-transition-group class names for `phase` of transition `tname`."
  [tname phase]
  (case phase
    :enter        [(str tname "-enter") (str tname "-appear")]
    :enter-active [(str tname "-enter") (str tname "-enter-active")
                   (str tname "-appear") (str tname "-appear-active")]
    :open         [(str tname "-enter-done")]
    :exit         [(str tname "-exit")]
    :exit-active  [(str tname "-exit") (str tname "-exit-active")]
    nil))

(defn next-frame
  "Calls `f` after the browser applied the current styles (two animation frames)."
  [f]
  (let [done (atom false)
        once #(when-not @done (reset! done true) (f))]
    (js/requestAnimationFrame #(js/requestAnimationFrame once))
    ;; rAF does not run in background tabs or headless captures; a transition must
    ;; still reach its end state there.
    (js/setTimeout once 100)))

;; ------------------------------------------------------------------ hiccup helpers

(defn- melden!
  "Wie ein Fenster zuging, für eine Feedback-Aufzeichnung: \"zustand\" oder \"abgebaut\"."
  [wie]
  (when (exists? js/window)
    (.dispatchEvent js/window (js/CustomEvent. "sutra:fenster-zu" #js {:detail wie}))))

(defn merge-child-props
  "Adds `attrs` to a hiccup `child`: `:class` is joined, other keys only fill in what the
   child does not set itself. A string or a component without props map is wrapped in
   `wrap-tag`."
  ([child attrs] (merge-child-props child attrs :span))
  ([child attrs wrap-tag]
   (let [merge-attrs (fn [m]
                       (merge (dissoc attrs :class) m
                              {:class (bp/cx (:class attrs) (:class m))}))]
     (cond
       (and (vector? child) (map? (second child)))
       (update child 1 merge-attrs)

       (and (vector? child) (keyword? (first child)))
       (into [(first child) (merge-attrs {})] (rest child))

       :else
       [wrap-tag attrs child]))))

;; ------------------------------------------------------------------ stack

(defonce ^:private stack (atom []))
(defonce ^:private id-counter (atom 0))

(defn- top? [id] (= id (:id (peek @stack))))

(defn- this-and-descendants [id]
  (let [s @stack]
    (drop-while #(not= id (:id %)) s)))

(defn- body-lock! []
  (let [locked? (some (fn [o] (let [p ((:props o))]
                                (and (:use-portal p true) (:has-backdrop p true))))
                      @stack)]
    (.toggle (.-classList (.-body js/document)) (bp/cls "overlay-open") (boolean locked?))))

(def ^:private focusable-selector
  (str "a[href]:not([tabindex=\"-1\"]), button:not([disabled]):not([tabindex=\"-1\"]), "
       "details:not([tabindex=\"-1\"]), input:not([disabled]):not([tabindex=\"-1\"]), "
       "select:not([disabled]):not([tabindex=\"-1\"]), "
       "textarea:not([disabled]):not([tabindex=\"-1\"]), "
       "[tabindex]:not([tabindex=\"-1\"])"))

(defn keyboard-focusable-elements
  "Keyboard-focusable elements inside `container`, without the overlay focus traps."
  [container]
  (if container
    (->> (array-seq (.querySelectorAll container focusable-selector))
         (remove #(let [cl (.-classList %)]
                    (or (.contains cl (bp/cls "overlay-start-focus-trap"))
                        (.contains cl (bp/cls "overlay-end-focus-trap"))))))
    []))

;; ------------------------------------------------------------------ overlay

(def ^:private own-keys
  [:is-open :on-close :has-backdrop :can-escape-key-close :can-outside-click-close
   :autofocus :enforce-focus :use-portal :portal-container :portal-class-name
   :backdrop-class-name :backdrop-props :transition-name :transition-duration
   :should-return-focus-on-close :on-opening :on-opened :on-closing :on-closed :lazy
   :dark-source :child-class])

(def defaults
  {:has-backdrop true :can-escape-key-close true :can-outside-click-close true
   :autofocus true :enforce-focus true :use-portal true :should-return-focus-on-close true
   :transition-duration 300 :lazy true})

(defn overlay
  "Props (Blueprint names): `:is-open`, `:on-close (fn [event])`, `:has-backdrop`,
   `:can-escape-key-close`, `:can-outside-click-close`, `:autofocus`, `:enforce-focus`,
   `:use-portal`, `:portal-container`, `:portal-class-name`, `:class`,
   `:backdrop-class-name`, `:backdrop-props`, `:transition-name` (default \"bp-overlay\"),
   `:transition-duration` ms, `:should-return-focus-on-close`, `:lazy`, `:on-opening`,
   `:on-opened`, `:on-closing`, `:on-closed`, `:child-class` (added to every child),
   `:dark-source` (see portal). Every child gets `bp-overlay-content` and the transition
   classes; give them a keyword tag."
  [_props & _children]
  (let [id          (str "bp-overlay-" (swap! id-counter inc))
        phase       (r/atom :closed)
        ever-opened (r/atom false)
        container   (atom nil)
        start-trap  (atom nil)
        end-trap    (atom nil)
        last-active (atom nil)
        autofocusing (atom false)
        timer       (atom nil)
        cur-props   (atom {})
        props       (fn [] @cur-props)
        on-close    (fn [e] (when-let [f (:on-close (props))] (f e)))
        bring-focus-inside!
        (fn bring-focus-inside!
          ([] (bring-focus-inside! 10))
          ([tries]
           (js/requestAnimationFrame
            (fn []
              (let [c @container
                    a (.-activeElement js/document)]
                (cond
                  ;; a portal renders its children a frame after it mounts
                  (and (nil? c) (pos? tries) (#{:enter :enter-active :open} @phase))
                  (bring-focus-inside! (dec tries))

                  (and c (or (nil? a) (not (.contains c a))))
                  (do (some-> @start-trap (.focus #js {:preventScroll true}))
                      (reset! autofocusing false))))))))
        on-document-focus
        (fn [e]
          (let [t (if (.-composed e) (aget (.composedPath e) 0) (.-target e))
                c @container]
            (when (and c (instance? js/Node t) (not (.contains c t)))
              (.preventDefault e)
              (.stopImmediatePropagation e)
              (bring-focus-inside!))))
        on-document-mousedown
        (fn [e]
          (let [t (if (.-composed e) (aget (.composedPath e) 0) (.-target e))
                inside? (some (fn [o] (when-let [c ((:container o))]
                                        (and (.contains c t) (not (.isSameNode c t)))))
                              (this-and-descendants id))]
            (when-not inside? (on-close e))))
        on-document-keydown
        (fn [e]
          (when (and (= "Escape" (.-key e)) (top? id))
            (on-close e)
            (.stopPropagation e)
            (.preventDefault e)))
        listeners   (atom #{})
        listen!     (fn [k ev f capture?]
                      (when-not (contains? @listeners k)
                        (.addEventListener js/document ev f capture?)
                        (swap! listeners conj k)))
        unlisten!   (fn [k ev f capture?]
                      (when (contains? @listeners k)
                        (.removeEventListener js/document ev f capture?)
                        (swap! listeners disj k)))
        sync-listeners!
        (fn []
          (let [{:keys [can-outside-click-close has-backdrop autofocus
                        can-escape-key-close enforce-focus]} (props)
                open? (contains? #{:enter :enter-active :open} @phase)]
            (if (and open? can-outside-click-close (not has-backdrop))
              (listen! :mousedown "mousedown" on-document-mousedown false)
              (unlisten! :mousedown "mousedown" on-document-mousedown false))
            (if (and open? (false? autofocus) can-escape-key-close)
              (listen! :keydown "keydown" on-document-keydown false)
              (unlisten! :keydown "keydown" on-document-keydown false))
            (if (and open? enforce-focus (top? id))
              (listen! :focus "focus" on-document-focus true)
              (unlisten! :focus "focus" on-document-focus true))))
        clear-timer! (fn [] (some-> @timer js/clearTimeout) (reset! timer nil))
        open!
        (fn []
          (clear-timer!)
          (reset! ever-opened true)
          (when-let [prev (peek @stack)]
            ((:release-focus prev)))
          (when-not (some #(= id (:id %)) @stack)
            (swap! stack conj {:id id
                               :container (fn [] @container)
                               :props props
                               :bring-focus bring-focus-inside!
                               :release-focus #(unlisten! :focus "focus" on-document-focus true)
                               :sync! sync-listeners!}))
          ;; a reopened overlay's portal moves to the end, so it paints above those
          ;; opened since: the DOM order is the stacking order
          (when-let [p (some-> @container (.closest (str "." (bp/cls "portal"))))]
            (when (.-nextElementSibling p)
              (.appendChild (.-parentNode p) p)))
          (body-lock!)
          (reset! last-active (.-activeElement js/document))
          (reset! phase :enter)
          (sync-listeners!)
          (when-let [f (:on-opening (props))] (f))
          (when (:autofocus (props))
            (reset! autofocusing true)
            (bring-focus-inside!))
          (next-frame
           (fn []
             (when (= @phase :enter)
               (reset! phase :enter-active) (r/flush)
               (reset! timer
                       (js/setTimeout
                        (fn []
                          (when (= @phase :enter-active)
                            (reset! phase :open) (r/flush)
                            (when-let [f (:on-opened (props))] (f @container))))
                        (:transition-duration (props))))))))
        unregister!
        (fn []
          (doseq [[k ev f c] [[:mousedown "mousedown" on-document-mousedown false]
                              [:keydown "keydown" on-document-keydown false]
                              [:focus "focus" on-document-focus true]]]
            (unlisten! k ev f c))
          (let [was-top? (top? id)]
            (swap! stack (fn [s] (into [] (remove #(= id (:id %))) s)))
            (body-lock!)
            (when-let [last (and was-top? (peek @stack))]
              (let [p ((:props last))]
                (when (and (:autofocus p) (:enforce-focus p))
                  ((:bring-focus last))))
              ((:sync! last)))))
        close!
        (fn []
          (clear-timer!)
          (unregister!)
          (reset! phase :exit)
          (when-let [f (:on-closing (props))] (f))
          (next-frame
           (fn []
             (when (= @phase :exit)
               (reset! phase :exit-active) (r/flush)
               (reset! timer
                       (js/setTimeout
                        (fn []
                          (when (= @phase :exit-active)
                            (reset! phase :closed) (r/flush)
                            (let [a @last-active]
                              (when (and (:should-return-focus-on-close (props))
                                         (instance? js/HTMLElement a))
                                (.focus a)))
                            (when-let [f (:on-closed (props))] (f))))
                        (:transition-duration (props))))))))
        on-container-keydown
        (fn [e]
          ;; below the top (e.g. a popover that left focus here) the key goes on to it
          (when (and (= "Escape" (.-key e)) (:can-escape-key-close (props)) (top? id))
            (on-close e)
            (.stopPropagation e)
            (.preventDefault e)))
        on-backdrop-mousedown
        (fn [e]
          (let [p (props)]
            (when (:can-outside-click-close p) (on-close e))
            (when (:enforce-focus p) (bring-focus-inside!))
            (when-let [f (get-in p [:backdrop-props :on-mouse-down])] (f e))))
        on-start-trap-focus
        (fn [e]
          (when (and (:enforce-focus (props)) (not @autofocusing))
            (let [rel (.-relatedTarget e)
                  c   @container]
              (when (and rel c (.contains c rel) (not= rel @end-trap))
                (some-> @end-trap (.focus #js {:preventScroll true}))))))
        on-start-trap-keydown
        (fn [e]
          (when (and (:enforce-focus (props)) (.-shiftKey e) (= "Tab" (.-key e)))
            (if-let [el (last (keyboard-focusable-elements @container))]
              (.focus el)
              (some-> @end-trap (.focus #js {:preventScroll true})))))
        on-end-trap-focus
        (fn [e]
          (let [rel (.-relatedTarget e)
                c   @container
                els (keyboard-focusable-elements c)]
            (if (and rel c (.contains c rel) (not= rel @start-trap))
              (if-let [f (and (not @autofocusing) (first els))]
                (if (not= f rel)
                  (.focus f)
                  (some-> @start-trap (.focus #js {:preventScroll true})))
                (some-> @start-trap (.focus #js {:preventScroll true})))
              (if-let [l (last els)]
                (.focus l)
                (some-> @start-trap (.focus #js {:preventScroll true}))))))]
    (r/create-class
     {:display-name "blueprint-cljs.overlay"
      :component-did-mount
      (fn [_] (when (:is-open (props)) (open!)))
      :component-did-update
      (fn [_ [_ old-props]]
        (let [was (boolean (:is-open old-props))
              now (boolean (:is-open (props)))]
          (cond
            (and now (not was)) (open!)
            (and was (not now)) (do (melden! "zustand") (close!))
            :else (sync-listeners!))))
      :component-will-unmount
      (fn [_]
        (when (contains? #{:enter :enter-active :open} @phase) (melden! "abgebaut"))
        (clear-timer!)
        (unregister!))
      :reagent-render
      (fn [p & children]
        (let [p      (merge defaults (into {} (remove (comp nil? val)) (when (map? p) p)))
              _      (reset! cur-props p)
              ph     @phase
              tname  (or (:transition-name p) (bp/cls "overlay"))
              tcls   (transition-classes tname ph)
              open?  (boolean (:is-open p))
              shown? (not= ph :closed)
              {:keys [use-portal has-backdrop autofocus enforce-focus lazy]} p
              content
              (when shown?
                (let [kids (keep identity children)]
                  (cond-> []
                    (and open? (or autofocus enforce-focus) (seq kids))
                    (conj ^{:key "__start"}
                     [:div {:class       (bp/cx (bp/cls "overlay-start-focus-trap") tcls)
                            :tab-index   0
                            :ref         #(do (reset! start-trap %) nil)
                            :on-focus    on-start-trap-focus
                            :on-key-down on-start-trap-keydown}])
                    has-backdrop
                    (conj ^{:key "__backdrop"}
                     [:div (merge (dissoc (:backdrop-props p) :on-mouse-down)
                                  {:class (bp/cx (bp/cls "overlay-backdrop")
                                                 (:backdrop-class-name p)
                                                 (get-in p [:backdrop-props :class])
                                                 tcls)
                                   :on-mouse-down on-backdrop-mousedown})])
                    true
                    (into (map-indexed
                           (fn [i child]
                             (with-meta
                               (merge-child-props
                                child
                                (cond-> {:class (bp/cx (bp/cls "overlay-content")
                                                       (:child-class p) tcls)}
                                  (or enforce-focus autofocus) (assoc :tab-index 0)))
                               {:key (or (some-> child meta :key) (str "c" i))}))
                           kids))
                    (and open? enforce-focus (seq kids))
                    (conj ^{:key "__end"}
                     [:div {:class     (bp/cx (bp/cls "overlay-end-focus-trap") tcls)
                            :tab-index 0
                            :ref       #(do (reset! end-trap %) nil)
                            :on-focus  on-end-trap-focus}]))))
              root
              [:div (merge (bp/dom-props p own-keys)
                           {:aria-live   "polite"
                            :class       (bp/cx (bp/cls "overlay")
                                                {(bp/cls "overlay-open") open?
                                                 (bp/cls "overlay-inline") (not use-portal)}
                                                (:class p))
                            :on-key-down on-container-keydown
                            :ref         #(do (reset! container %) nil)})
               (seq content)]]
          (cond
            (and lazy (not @ever-opened) (not open?)) nil
            use-portal [portal/portal {:container   (:portal-container p)
                                       :class-name  (:portal-class-name p)
                                       :dark-source (:dark-source p)}
                        root]
            :else root)))})))
