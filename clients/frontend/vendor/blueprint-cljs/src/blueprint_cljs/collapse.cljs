(ns blueprint-cljs.collapse
  "Collapse: shows and hides its children with a height animation."
  (:require [reagent.core :as r]
            [blueprint-cljs.core :as bp]
            [blueprint-cljs.common :as c]))

(defn- props-of
  [argv]
  (first (c/split-args (rest argv))))

(defn collapse
  "Props: `:is-open`, `:keep-children-mounted`, `:transition-duration` ms (default 200),
   `:component` (default :div), `:class`."
  [& args]
  (let [open0 (boolean (:is-open (first (c/split-args args))))
        st    (r/atom {:anim   (if open0 :open :closed)
                       :height (if open0 "auto" "0px")
                       :hwo    nil})
        body  (atom nil)
        timer (atom nil)
        later (fn [ms f] (js/clearTimeout @timer) (reset! timer (js/setTimeout f ms)))
        done! (fn []
                (case (:anim @st)
                  :opening (swap! st assoc :anim :open :height "auto")
                  :closing (swap! st assoc :anim :closed)
                  nil))
        step! (fn [this]
                (when-let [n @body]
                  (let [dur (:transition-duration (props-of (r/argv this)) 200)
                        h   (.-clientHeight n)]
                    (case (:anim @st)
                      :open-start
                      (do (swap! st assoc :anim :opening :height (str h "px") :hwo h)
                          (later dur done!))
                      :closing-start
                      (do (js/setTimeout #(swap! st assoc :anim :closing :height "0px" :hwo h))
                          (later dur done!))
                      nil))))]
    (r/create-class
     {:display-name "Collapse"
      :component-did-mount
      (fn [this]
        (when-let [n @body]
          (let [h (.-clientHeight n)]
            (swap! st assoc
                   :anim (if (:is-open (props-of (r/argv this))) :open :closed)
                   :height (when-not (zero? h) (str h "px"))
                   :hwo (when-not (zero? h) h)))))
      :component-did-update
      (fn [this old-argv]
        (let [was (boolean (:is-open (props-of old-argv)))
              is  (boolean (:is-open (props-of (r/argv this))))
              {:keys [anim hwo]} @st]
          (cond
            (and is (not was) (not (#{:open :opening} anim)))
            (swap! st assoc :anim :open-start)

            (and was (not is) (not (#{:closed :closing} anim)))
            (swap! st assoc :anim :closing-start :height (str hwo "px"))))
        (step! this))
      :component-will-unmount (fn [_] (js/clearTimeout @timer))
      :reagent-render
      (fn [& args]
        (let [[{:keys [keep-children-mounted component class] :or {component :div}} children]
              (c/split-args args)
              {:keys [anim height hwo]} @st
              visible? (not= anim :closed)
              eff-h    (if (= anim :open) "auto" height)
              auto?    (= eff-h "auto")]
          [component {:class (bp/cx (bp/cls "collapse") class)
                      :style {:height     (when visible? eff-h)
                              :overflow-y (when auto? "visible")
                              :transition (when auto? "none")}}
           (into [:div {:class       (bp/cls "collapse-body")
                        :ref         #(reset! body %)
                        :aria-hidden (not visible?)
                        :style       {:transform  (if (and visible? (not= anim :closing))
                                                    "translateY(0)"
                                                    (str "translateY(-" hwo "px)"))
                                      :transition (when auto? "none")}}]
                 (when (or visible? keep-children-mounted) children))]))})))
