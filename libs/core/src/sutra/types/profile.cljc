(ns sutra.types.profile
  (:require [clojure.string :as str]
            [sutra.types.decimal :as d]))

(def top-values 8)

(defn- as-number
  [v]
  (cond
    (number? v)  v
    (d/exact? v) (d/host-number v)
    (string? v)  #?(:clj  (try (bigdec (str/trim v)) (catch NumberFormatException _ nil))
                    :cljs (let [n (js/parseFloat v)] (when-not (js/isNaN n) n)))
    :else nil))

(defn- mean [nums]
  (when (seq nums)
    (double (/ (reduce + (map double nums)) (count nums)))))

(defn- quantile [sorted q]
  (when (seq sorted)
    (double (nth sorted (min (dec (count sorted))
                             (int (* q (count sorted))))))))

(defn column-stats
  [rows col]
  (let [vs      (mapv #(get % col) rows)
        n       (count vs)
        present (remove nil? vs)
        nums    (keep as-number present)
        strs    (filter string? present)
        lens    (map count strs)
        sorted  (sort (map double nums))]
    (cond-> {:column   col
             :count    n
             :nulls    (- n (count present))
             :distinct (count (distinct present))
             :top      (->> (frequencies present)
                            (sort-by (juxt (comp - val) (comp str key)))
                            (take top-values)
                            (mapv (fn [[v c]] {:value v :count c})))}

      (and (seq nums) (= (count nums) (count present)))
      (assoc :min (apply min sorted)
             :max (apply max sorted)
             :mean (mean nums)
             :stddev (let [m (mean nums)]
                       (Math/sqrt (/ (reduce + (map #(let [d (- (double %) m)] (* d d))
                                                    nums))
                                     (count nums))))
             :median (quantile (vec sorted) 0.5))

      (seq strs)
      (assoc :min-length (apply min lens)
             :max-length (apply max lens)
             :blank (count (filter str/blank? strs))
             :whitespace-edged (count (filter #(not= % (str/trim %)) strs))))))

(defn profile
  [rows cols]
  (mapv #(column-stats rows %) cols))
