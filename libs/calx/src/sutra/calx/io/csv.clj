(ns sutra.calx.io.csv
  (:require [clojure.string :as str]
            [sutra.calx.i18n :as i18n]
            [sutra.calx.value :as v]
            [sutra.calx.workbook :as wb]))

(set! *warn-on-reflection* true)

(defn- locale-of [w] (i18n/locale (get-in w [:config :locale])))

(defn parse
  ([s] (parse s \,))
  ([^String s sep]
   (let [n (count s)]
     (loop [i 0 field (StringBuilder.) row [] rows [] quoted? false]
       (if (>= i n)
         (let [row (conj row (str field))]
           (if (and (= row [""]) (seq rows)) rows (conj rows row)))
         (let [ch (.charAt s i)]
           (if quoted?
             (cond
               (and (= ch \") (< (inc i) n) (= \" (.charAt s (inc i))))
               (recur (+ i 2) (.append field \") row rows true)
               (= ch \") (recur (inc i) field row rows false)
               :else (recur (inc i) (.append field ch) row rows true))
             (cond
               (= ch \") (recur (inc i) field row rows true)
               (= (Character/valueOf ch) sep)
               (recur (inc i) (StringBuilder.) (conj row (str field)) rows false)
               (= ch \return) (recur (inc i) field row rows false)
               (= ch \newline) (recur (inc i) (StringBuilder.) [] (conj rows (conj row (str field)))
                                      false)
               :else (recur (inc i) (.append field ch) row rows false)))))))))

(defn- quote-field [s sep]
  (if (re-find (re-pattern (str "[\"\\r\\n" (java.util.regex.Pattern/quote (str sep)) "]")) s)
    (str "\"" (str/replace s "\"" "\"\"") "\"")
    s))

(defn write
  ([rows] (write rows \,))
  ([rows sep]
   (str/join "\r\n"
             (map (fn [row] (str/join (str sep) (map #(quote-field (str %) sep) row))) rows))))

(defn import-csv
  ([w sheet csv] (import-csv w sheet csv {}))
  ([w sheet csv {:keys [sep row col formulas?] :or {row 0 col 0 formulas? true}}]
   (let [loc (locale-of w)
         id (wb/sheet-id w sheet)
         rows (parse csv (or sep (:list-sep loc \,)))]
     (wb/transact w (for [[i r] (map-indexed vector rows)
                          [j x] (map-indexed vector r)
                          :when (not= "" x)]
                      (if (or formulas? (not (str/starts-with? x "=")))
                        [:set-cell [id (+ row i) (+ col j)] (i18n/canonical-input x loc)]
                        [:set-value [id (+ row i) (+ col j)] x]))))))

(defn export-csv
  ([w sheet] (export-csv w sheet {}))
  ([w sheet {:keys [sep mode] :or {mode :values}}]
   (let [loc (locale-of w)
         id (wb/sheet-id w sheet)
         [nr nc] (get-in w [:sheets id :extent])
         cell-str (fn [k]
                    (case mode
                      :values (let [x (wb/value w k)] (if (nil? x) "" (v/display loc x)))
                      :inputs (let [x (wb/input w k)]
                                (if (nil? x) ""
                                    (str (i18n/cell-input x (:style (wb/cell w k)) loc))))))]
     (write (for [r (range nr)] (for [c (range nc)] (cell-str [id r c])))
            (cond (= :rfc4180 sep) \,
                  (char? sep)      sep
                  :else            (:list-sep loc \,))))))
