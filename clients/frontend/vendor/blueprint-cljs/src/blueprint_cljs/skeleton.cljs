(ns blueprint-cljs.skeleton
  "Skeleton is a class, as in Blueprint: put it on any element while its content loads."
  (:require [blueprint-cljs.core :as bp]))

(def skeleton-class (bp/cls "skeleton"))

(defn skeleton
  "`(skeleton loading? \"my-class\")` → the class string, with the skeleton class while
   `loading?`."
  ([loading?] (skeleton loading? nil))
  ([loading? class] (bp/cx class (when loading? skeleton-class))))
