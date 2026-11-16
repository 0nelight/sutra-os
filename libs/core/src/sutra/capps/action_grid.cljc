(ns sutra.capps.action-grid
  (:require [clojure.string :as str]
            [sutra.kit.coerce :refer [->kw]]))

(defn blank [] {:next 1 :rows [{:row 0 :values {}}]})

(defn add-row
  [{:keys [next] :as g} values]
  (-> g
      (update :rows conj {:row next :values (or values {})})
      (assoc :next (inc next))))

(defn remove-row
  [g row]
  (update g :rows (fn [rs] (filterv #(not= row (:row %)) rs))))

(defn set-cell
  [g row param v]
  (update g :rows (fn [rs] (mapv #(if (= row (:row %)) (assoc-in % [:values param] v) %) rs))))

(defn with-rows
  [value-maps]
  (reduce add-row {:next 0 :rows []} value-maps))

(defn prefilled
  [rows key-param]
  (with-rows (mapv (fn [r] {(->kw key-param) (or (:id r) r)}) rows)))

(defn- empty-row? [{:keys [values]}] (every? #(or (nil? %) (= "" %)) (vals values)))

(defn to-send
  [{:keys [rows]} defaults]
  (into [] (comp (remove empty-row?) (map #(merge defaults (:values %)))) rows))

(defn after-send
  [{:keys [rows] :as g} n]
  (let [sent (set (map :row (take n (remove empty-row? rows))))
        left (filterv #(not (sent (:row %))) rows)]
    (if (seq left) (assoc g :rows left) (blank))))

(defn parse-csv
  [text]
  (loop [cs (seq (str text)) cell [] row [] rows [] q? false]
    (if-let [c (first cs)]
      (let [more (next cs)
            done #(conj row (str/join cell))]
        (cond
          (and q? (= c \") (= \" (first more))) (recur (next more) (conj cell \") row rows true)
          (= c \")                              (recur more cell row rows (not q?))
          q?                                    (recur more (conj cell c) row rows true)
          (= c \,)                              (recur more [] (done) rows false)
          (= c \return)                         (recur more cell row rows false)
          (= c \newline)                        (recur more [] [] (conj rows (done)) false)
          :else                                 (recur more (conj cell c) row rows false)))
      (cond-> rows
        (or (seq cell) (seq row)) (conj (conj row (str/join cell)))))))

(defn- header-index
  [specs]
  (into {}
        (mapcat (fn [[p spec]]
                  (keep (fn [x] (when x [(str/lower-case (str x)) p]))
                        [(name p) (:label spec) (:display-name spec)])))
        specs))

(defn csv-values
  [text specs]
  (let [[header & rows] (parse-csv text)
        idx    (header-index specs)
        params (mapv #(get idx (str/lower-case (str/trim %))) header)]
    {:ignored (vec (keep (fn [[h p]] (when-not p h)) (map vector header params)))
     :rows    (mapv (fn [row]
                      (into {} (keep (fn [[p v]] (when (and p (not= "" v)) [p v])))
                            (map vector params row)))
                    rows)}))

(defn next-cell
  [[r c] key rows cols]
  (case key
    "ArrowUp"    [(max 0 (dec r)) c]
    "ArrowDown"  [(min (dec rows) (inc r)) c]
    "ArrowLeft"  [r (max 0 (dec c))]
    "ArrowRight" [r (min (dec cols) (inc c))]
    [r c]))

(defn action-entries
  [{:keys [actions action]}]
  (cond
    (seq actions) (vec actions)
    action        [{:action action}]
    :else         []))
