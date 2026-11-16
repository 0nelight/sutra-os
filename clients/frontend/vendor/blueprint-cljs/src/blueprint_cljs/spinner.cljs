(ns blueprint-cljs.spinner
  "Spinner: an SVG ring; with `:value` (0–1) it shows progress and stops spinning."
  (:require [clojure.string :as str]
            [blueprint-cljs.core :as bp]))

(def size-small 20)
(def size-standard 50)
(def size-large 100)

(def ^:private r 45)
(def ^:private track
  (str "M 50,50 m 0,-" r " a " r "," r " 0 1 1 0," (* r 2)
       " a " r "," r " 0 1 1 0,-" (* r 2)))
(def ^:private path-length 280)

(defn- size-px
  [size class]
  (cond
    (some? size)                                    (max 10 size)
    (and class (str/includes? class (bp/cls "small"))) size-small
    (and class (str/includes? class (bp/cls "large"))) size-large
    :else                                           size-standard))

(defn spinner
  "Props: `:size` px (or class bp-small/bp-large), `:intent`, `:value` 0–1, `:tag-name`."
  [{:keys [intent value tag-name size] :or {tag-name :div} :as props}]
  (let [class   (:class props)
        px      (size-px size class)
        stroke  (min 16 (/ (* 4 size-large) px))
        offset  (- path-length (* path-length (if (nil? value) 0.25 (-> value (max 0) (min 1)))))
        radius  (+ r (/ stroke 2))
        vb-x    (.toFixed (- 50 radius) 2)
        vb-w    (.toFixed (* 2 radius) 2)]
    [tag-name
     (merge {:aria-label    "loading"
             :aria-valuemax 100
             :aria-valuemin 0
             :aria-valuenow (when (some? value) (* 100 value))
             :role          "progressbar"}
            (bp/dom-props props [:intent :value :tag-name :size :class])
            {:class (bp/cx (bp/cls "spinner") (bp/intent-class intent)
                           {(bp/cls "no-spin") (some? value)} class)})
     [tag-name {:class (bp/cls "spinner-animation")}
      [:svg {:width px :height px :stroke-width (.toFixed stroke 2)
             :view-box (str vb-x " " vb-x " " vb-w " " vb-w)}
       [:path {:class (bp/cls "spinner-track") :d track}]
       [:path {:class             (bp/cls "spinner-head")
               :d                 track
               :path-length       path-length
               :stroke-dasharray  (str path-length " " path-length)
               :stroke-dashoffset offset}]]]]))
