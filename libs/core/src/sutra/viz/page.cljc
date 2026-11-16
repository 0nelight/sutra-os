(ns sutra.viz.page
  (:require [clojure.string :as str]))

(def ^:private sheets
  {:a1      [1683.78 2383.94]
   :a2      [1190.55 1683.78]
   :a3      [841.89 1190.55]
   :a4      [595.28 841.89]
   :a5      [419.53 595.28]
   :letter  [612.0 792.0]
   :legal   [612.0 1008.0]
   :tabloid [792.0 1224.0]})

(def ^:private default-size :a4)
(def ^:private default-margin "18mm 16mm")

(def ^:private per-unit
  {"pt" 1.0 "px" 0.75 "mm" (/ 72.0 25.4) "cm" (/ 72.0 2.54) "in" 72.0 "pc" 12.0 "q" (/ 72.0 101.6)})

(defn- ->pt
  [token]
  (when-let [[_ n u] (re-matches #"(?i)\s*(-?[0-9]*\.?[0-9]+)\s*([a-z%]*)\s*" (str token))]
    (let [v #?(:clj (try (Double/parseDouble n) (catch NumberFormatException _ nil))
               :cljs (let [x (js/parseFloat n)] (when-not (js/isNaN x) x)))]
      (when v
        (if (str/blank? u)
          (when (zero? v) 0.0)
          (some->> (get per-unit (str/lower-case u)) (* v)))))))

(defn margins
  [margin]
  (let [vs (->> (str/split (str/trim (str margin)) #"\s+")
                (remove str/blank?)
                (mapv #(or (->pt %) 0.0)))]
    (case (count vs)
      1 (vec (repeat 4 (vs 0)))
      2 [(vs 0) (vs 1) (vs 0) (vs 1)]
      3 [(vs 0) (vs 1) (vs 2) (vs 1)]
      4 vs
      (margins default-margin))))

(defn content-box
  [{:keys [size orientation margin]}]
  (let [[w h]           (get sheets (or size default-size) (get sheets default-size))
        [pw ph]         (if (= :landscape orientation) [h w] [w h])
        [mt mr mb ml]   (margins (or margin default-margin))]
    [(max 1.0 (- pw ml mr)) (max 1.0 (- ph mt mb))]))

(def px-per-pt
  (/ 1.0 0.75))

(defn content-box-px
  [page]
  (mapv #(* % px-per-pt) (content-box page)))
