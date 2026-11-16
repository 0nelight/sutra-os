(ns blueprint-cljs.entity-title
  "EntityTitle: icon, title (optionally a link), tags and subtitle of an entity."
  (:require [blueprint-cljs.core :as bp]
            [blueprint-cljs.icon :as icon]
            [blueprint-cljs.text :as text]))

(def ^:private headings #{:h1 :h2 :h3 :h4 :h5 :h6})

(defn entity-title
  "Props: `:title`, `:title-url`, `:subtitle`, `:icon`, `:tags`, `:heading` (:h1–:h6 or
   nil for plain text), `:ellipsize`, `:fill`, `:loading`."
  [{:keys [class ellipsize fill heading loading subtitle tags title title-url]
    icon-name :icon}]
  (let [skel      {(bp/cls "skeleton") loading}
        title-el  (if title-url
                    [:a {:target "_blank" :href title-url :rel "noreferrer"} title]
                    title)
        heading?  (contains? headings heading)]
    [:div {:class (bp/cx class (bp/cls "entity-title")
                         (when heading? (bp/cls (str "entity-title-heading-" (name heading))))
                         {(bp/cls "entity-title-ellipsize") ellipsize
                          (bp/cls "fill")                   fill})}
     (when (some? icon-name)
       [:div {:class (bp/cx (bp/cls "entity-title-icon-container")
                            {(bp/cls "entity-title-has-subtitle") (some? subtitle)})}
        (icon/render-icon (if loading :square icon-name)
                          {:aria-hidden true :tab-index -1
                           :class (bp/cx (bp/cls "text-muted") skel)})])
     [:div {:class (bp/cls "entity-title-text")}
      [:div {:class (bp/cx (bp/cls "entity-title-title-and-tags") skel)}
       (if heading?
         [heading {:class (bp/cx (bp/cls "heading") (bp/cls "entity-title-title") skel
                                 {(bp/cls "text-overflow-ellipsis") ellipsize})}
          title-el]
         [text/text {:class (bp/cx (bp/cls "entity-title-title") skel) :ellipsize ellipsize}
          title-el])
       (when (some? tags) [:div {:class (bp/cls "entity-title-tags-container")} tags])]
      (when (some? subtitle)
        [text/text {:class     (bp/cx (bp/cls "text-muted") (bp/cls "entity-title-subtitle") skel)
                    :ellipsize ellipsize}
         subtitle])]]))
