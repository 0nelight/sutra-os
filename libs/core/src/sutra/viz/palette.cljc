(ns sutra.viz.palette
  (:require [sutra.viz.lineage-projection :as lp]))

(def node-kinds
  (into {} (map (juxt :kind #(select-keys % [:label :color :icon]))) lp/node-kinds))

(def default-accent "#404854")

(defn kind-color [kind] (:color (lp/row kind)))

(defn kind-icon [kind] (:icon (lp/row kind)))

(defn accent-of [n] (or (:accent n) (kind-color (:kind n)) default-accent))

(def class-colors
  [["#215db0" "#e0eaf8"]
   ["#1c6e42" "#deede5"]
   ["#935610" "#f7eadc"]
   ["#ac2f33" "#f8e3e3"]
   ["#7c327c" "#f0e2f0"]
   ["#007067" "#d9f1ef"]
   ["#c22762" "#fadfe9"]
   ["#5a701a" "#eef3de"]])

(defn class-palette
  [classes]
  (into {}
        (map-indexed (fn [i c]
                       (let [[line fill] (nth class-colors (mod i (count class-colors)))]
                         [c {:accent line :fill fill}]))
                     (sort (remove nil? (distinct classes))))))
