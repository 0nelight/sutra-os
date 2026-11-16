(ns blueprint-cljs.table.formats
  "TruncatedFormat and JSONFormat: cell contents that cut long text and offer the whole
   value in a popover."
  (:require [reagent.core :as r]
            [blueprint-cljs.core :as bp]
            [blueprint-cljs.common :as c]
            [blueprint-cljs.icon :refer [icon]]
            [blueprint-cljs.popover :refer [popover]]))

(defn- truncated?
  [el]
  (and el (or (> (.-scrollWidth el) (.-clientWidth el))
              (> (.-scrollHeight el) (.-clientHeight el)))))

(defn- show-popover?
  [{:keys [detect-truncation show-popover truncate-length]} content measured?]
  (case (some-> show-popover name)
    "always" true
    "never"  false
    (if detect-truncation
      measured?
      (and (pos? truncate-length) (> (count content) truncate-length)))))

(def ^:private defaults
  {:detect-truncation false :preformatted false :show-popover :when-truncated
   :truncate-length 2000 :truncation-suffix "..."})

(defn truncated-format
  "Props: `:detect-truncation` (measure the rendered text), `:show-popover`
   :always|:never|:when-truncated, `:truncate-length` (2000), `:truncation-suffix`,
   `:preformatted` (keep whitespace in the popover), `:class`. Child: the text."
  [& _]
  (let [st  (r/atom {:truncated? false :open? false})
        el  (atom nil)
        measure! (fn [this]
                   (let [{:keys [detect-truncation]}
                         (merge defaults (first (c/split-args (rest (r/argv this)))))]
                     (when detect-truncation
                       (let [t (boolean (truncated? @el))]
                         (when (not= t (:truncated? @st)) (swap! st assoc :truncated? t))))))]
    (r/create-class
     {:display-name         "TruncatedFormat"
      :component-did-mount  measure!
      :component-did-update measure!
      :reagent-render
      (fn [& args]
        (let [[props children] (c/split-args args)
              {:keys [detect-truncation truncate-length truncation-suffix preformatted]
               :as p} (merge defaults props)
              content (apply str children)
              shown   (if (and (not detect-truncation) (pos? truncate-length)
                               (> (count content) truncate-length))
                        (str (subs content 0 truncate-length) truncation-suffix)
                        content)
              ref     (fn [e] (reset! el e) nil)]
          (if (show-popover? p content (:truncated? @st))
            [:div {:class (bp/cx (:class props) (bp/cls "table-truncated-format"))}
             [:div {:class (bp/cls "table-truncated-value") :ref ref} shown]
             [popover {:is-open (:open? @st)
                       :class (bp/cls "table-truncated-popover-target")
                       :target-tag-name :span
                       :placement :bottom
                       :should-return-focus-on-close false
                       :on-interaction (fn [open? _] (swap! st assoc :open? open?))
                       :content [:div {:class (bp/cx (bp/cls "table-truncated-popover")
                                                     (if preformatted
                                                       (bp/cls "table-popover-whitespace-pre")
                                                       (bp/cls
                                                        "table-popover-whitespace-normal")))}
                                 content]}
              [icon {:icon :more}]]]
            [:div {:class (bp/cx (:class props) (bp/cls "table-truncated-format-text"))
                   :ref   ref}
             shown])))})))

(defn- default-stringify [x]
  (js/JSON.stringify (clj->js x) nil 2))

(defn json-format
  "Props: `:omit-quotes-on-strings` (true), `:stringify` (fn [value] string; JSON with
   2-space indent), plus truncated-format props. Child: any value; nil shows as null."
  [& args]
  (let [[props [value]] (c/split-args args)
        {:keys [omit-quotes-on-strings stringify]
         :or   {omit-quotes-on-strings true stringify default-stringify}} props
        nully? (nil? value)
        text   (if (and omit-quotes-on-strings (string? value)) value (stringify value))]
    [truncated-format (merge (dissoc props :omit-quotes-on-strings :stringify)
                             {:class (bp/cx (:class props) {(bp/cls "table-null") nully?})}
                             (when nully? {:show-popover :never}))
     text]))
