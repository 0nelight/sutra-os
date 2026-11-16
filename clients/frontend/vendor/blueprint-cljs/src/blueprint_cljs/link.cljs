(ns blueprint-cljs.link
  (:require [blueprint-cljs.core :as bp]
            [blueprint-cljs.common :as c]))

(defn link
  "An <a>. Props: `:underline` (:always :hover :none, default :always), `:color` an intent
   or :inherit (default :primary)."
  [& args]
  (let [[{:keys [underline color class] :or {underline :always color :primary} :as props}
         children] (c/split-args args)
        underline (name underline)
        inherit?  (= "inherit" (name color))]
    (into [:a (merge (bp/dom-props props [:underline :color :class])
                     {:class (bp/cx (bp/cls "link")
                                    {(bp/cls "link-underline-always") (= underline "always")
                                     (bp/cls "link-underline-hover")  (= underline "hover")
                                     (bp/cls "link-underline-none")   (= underline "none")
                                     (bp/cls "link-color-inherit")    inherit?}
                                    (when-not inherit? (bp/intent-class (keyword color)))
                                    class)})]
          children)))
