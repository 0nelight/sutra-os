(ns sutra-ui.ui.size
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [sutra.kit.size :as g]))

(defn- breite [] (if (exists? js/window) (.-innerWidth js/window) 1280))

(defonce ^:private fenster (r/atom (g/klasse (breite))))

(defonce ^:private mobil (r/atom 0))

(defn klasse
  []
  (if (pos? @mobil) :kompakt @fenster))

(defn kompakt?
  []
  (= :kompakt (klasse)))

(defn ab?
  [mindestens]
  (g/reicht? (klasse) (keyword mindestens)))

(defn mobil!
  [shown?]
  (swap! mobil (if shown? inc #(max 0 (dec %)))))

(def ^:private token-prefixes ["--c-" "--sp-" "--fs-" "--radius" "--w-"])

(defn- root-token-names []
  (into (sorted-set)
        (for [^js sheet (array-seq (.-styleSheets js/document))
              ^js rule  (try (array-seq (.-cssRules sheet)) (catch :default _ nil))
              :when (= ":root" (.-selectorText rule))
              i     (range (.. rule -style -length))
              :let  [p (.item (.-style rule) i)]
              :when (some #(str/starts-with? p %) token-prefixes)]
          p)))

(defn tokens
  []
  (let [cs (js/getComputedStyle js/document.documentElement)]
    (into {} (map (fn [p] [p (str/trim (.getPropertyValue cs p))])) (root-token-names))))

(defn- apply! [& _]
  (let [k (g/klasse (breite))]
    (when (not= k @fenster) (reset! fenster k))))

(defn install! []
  (.addEventListener js/window "resize" apply!)
  (apply!))
