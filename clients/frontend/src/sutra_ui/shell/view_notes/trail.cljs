(ns sutra-ui.shell.view-notes.trail
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [sutra-ui.feature :as feature]
            [sutra-ui.state.shell :as shell]))

(def ^:private max-schritte 25)

(defonce ^:private schritte (r/atom []))

(defn- jetzt-pfad []
  (str (.. js/window -location -pathname) (.. js/window -location -search)))

(defn- marke
  []
  (let [ui  @shell/ui-state
        nav (feature/current-nav ui)]
    (or (feature/subject ui nav) (some-> nav name))))

(defn schritt!
  []
  (let [p (jetzt-pfad)]
    (when (not= p (:pfad (peek @schritte)))
      (swap! schritte (fn [s]
                        (let [s (conj (vec s) {:at (js/Date.now) :pfad p :marke (marke)})]
                          (vec (take-last max-schritte s))))))))

(defn weg
  []
  @schritte)

(defn- verstrichen [von bis]
  (let [s (js/Math.round (/ (- bis von) 1000))]
    (cond
      (< s 60) (str s " s")
      (< s 3600) (str (js/Math.round (/ s 60)) " min")
      :else (str (js/Math.round (/ s 3600)) " h"))))

(defn als-markdown
  []
  (let [ws  (weg)
        now (js/Date.now)]
    (when (seq ws)
      (->> (map-indexed
            (fn [i {:keys [at pfad marke]}]
              (let [bis (or (:at (nth ws (inc i) nil)) now)]
                (str "- `" pfad "`"
                     (when marke (str " — " marke))
                     " · " (verstrichen at bis))))
            ws)
           (str/join "\n")))))

(defonce uhr
  (do (schritt!)
      (js/setInterval schritt! 600)))
