(ns blueprint-cljs.html
  "Plain HTML elements with Blueprint's typography classes (core/components/html,
   html-table): `[html/h1 {} \"Title\"]`, `[html/html-table {:striped true} …]`.
   Props go to the element as given; `:class` is added to the kit's class."
  (:require [blueprint-cljs.core :as bp]))

(defn- element
  [tag kit-class]
  (fn [& args]
    (let [[props children] (if (map? (first args))
                             [(first args) (rest args)]
                             [{} args])]
      (into [tag (assoc props :class (bp/merge-class (bp/cls kit-class) props))]
            children))))

(def h1 (element :h1 "heading"))
(def h2 (element :h2 "heading"))
(def h3 (element :h3 "heading"))
(def h4 (element :h4 "heading"))
(def h5 (element :h5 "heading"))
(def h6 (element :h6 "heading"))

(def blockquote (element :blockquote "blockquote"))
(def code (element :code "code"))
(def pre (element :pre "code-block"))
(def label (element :label "label"))
(def ol (element :ol "list"))
(def ul (element :ul "list"))

(defn html-table
  "Props: `:bordered`, `:compact`, `:interactive`, `:striped`; the rest goes to <table>."
  [& args]
  (let [[{:keys [bordered compact interactive striped] :as props} children]
        (if (map? (first args)) [(first args) (rest args)] [{} args])]
    (into [:table (assoc (bp/dom-props props [:bordered :compact :interactive :striped])
                         :class (bp/cx (bp/cls "html-table")
                                       {(bp/cls "compact")             compact
                                        (bp/cls "html-table-bordered") bordered
                                        (bp/cls "html-table-striped")  striped
                                        (bp/cls "interactive")         interactive}
                                       (:class props)))]
          children)))
