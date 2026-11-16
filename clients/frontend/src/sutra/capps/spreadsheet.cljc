(ns sutra.capps.spreadsheet
  (:require [clojure.string :as str]
            [sutra.kit.coerce :refer [->kw]]))

(defn- int*
  [c]
  #?(:clj (int c) :cljs (.charCodeAt (str c) 0)))

(defn- char*
  [n]
  #?(:clj (char n) :cljs (js/String.fromCharCode n)))

(defn column-index
  [letters]
  (dec (reduce (fn [n c] (+ (* 26 n) (- (int* c) 64))) 0 (str/upper-case letters))))

(defn column-name
  [i]
  (loop [n (inc i) acc ""]
    (if (pos? n)
      (let [r (mod (dec n) 26)]
        (recur (quot (dec n) 26) (str (char* (+ 65 r)) acc)))
      acc)))

(defn cell-position
  [ref]
  (when-let [[_ col row] (re-matches #"([A-Za-z]+)(\d+)" (str ref))]
    [(dec (parse-long row)) (column-index col)]))

(defn cell-ref
  [[r c]]
  (str (column-name c) (inc r)))

(defn grid
  [cells]
  (reduce (fn [g {:keys [ref value]}]
            (if-let [[r c :as at] (cell-position ref)]
              (-> g
                  (assoc-in [:cells at] value)
                  (update :rows max (inc r))
                  (update :cols max (inc c)))
              g))
          {:cells {} :rows 0 :cols 0}
          cells))

(defn- json-int
  [s k]
  (some-> (re-find (re-pattern (str "\"" k "\"\\s*:\\s*(-?\\d+)")) s) second parse-long))

(defn parse-range
  [x]
  (let [read-field (if (map? x)
                     (fn [k] (let [v (or (get x (keyword k)) (get x k))] (when (number? v) v)))
                     (let [s (str x)] (fn [k] (json-int s k))))
        m {:start-row (read-field "startRow") :start-col (read-field "startCol")
           :end-row   (read-field "endRow")   :end-col   (read-field "endCol")}]
    (when (every? some? (vals m)) m)))

(defn range-text
  [{:keys [start-row start-col end-row end-col]}]
  (str "{\"startRow\": " start-row ", \"startCol\": " start-col
       ", \"endRow\": " end-row ", \"endCol\": " end-col "}"))

(defn in-range?
  [{:keys [start-row start-col end-row end-col]} [r c]]
  (and (<= start-row r end-row) (<= start-col c end-col)))

(defn within-sheet?
  [{:keys [end-row end-col]} {:keys [rows cols]}]
  (and (< end-row rows) (< end-col cols)))

(defn drag-range
  [[r0 c0] [r1 c1]]
  {:start-row (min r0 r1) :start-col (min c0 c1) :end-row (max r0 r1) :end-col (max c0 c1)})

(defn range-cells
  [{:keys [start-row start-col end-row end-col]}]
  (for [r (range start-row (inc end-row)) c (range start-col (inc end-col))] [r c]))

(defn selection-range [{:keys [a b]}] (drag-range a b))

(defn selected?
  [sel at]
  (boolean (and sel (in-range? (selection-range sel) at))))

(defn select
  [sel at extend?]
  (cond-> (assoc sel :b at) (or (not extend?) (nil? (:a sel))) (assoc :a at)))

(defn step
  [{:keys [a b] :as sel} [dr dc] extend?]
  (let [[r c] (if extend? b a)]
    (select sel [(max 0 (+ r dr)) (max 0 (+ c dc))] extend?)))

(defn range-label
  [sel]
  (let [{:keys [start-row start-col end-row end-col]} (selection-range sel)]
    (if (and (= start-row end-row) (= start-col end-col))
      (cell-ref [start-row start-col])
      (str (cell-ref [start-row start-col]) ":" (cell-ref [end-row end-col])))))

(defn range-tsv
  [{:keys [start-row start-col end-row end-col]} cell]
  (str/join "\n" (for [r (range start-row (inc end-row))]
                   (str/join "\t" (for [c (range start-col (inc end-col))]
                                    (str/replace (str (cell [r c])) #"[\t\r\n]+" " "))))))

(defn parse-tsv
  [t [r0 c0]]
  (let [rows (mapv #(str/split % #"\t" -1)
                   (str/split (str/replace (str t) #"\r?\n$" "") #"\r?\n"))]
    (for [[i row] (map-indexed vector rows)
          [j x]   (map-indexed vector row)]
      [[(+ r0 i) (+ c0 j)] x])))

(defn- row-field
  [row k]
  (let [k (->kw k)]
    (if (contains? row k) (get row k) (get (:values row) k))))

(defn annotations
  [layers]
  (into []
        (mapcat (fn [{:keys [rows sheet-prop range-prop] :as layer}]
                  (keep (fn [row]
                          (when-let [r (parse-range (row-field row range-prop))]
                            {:sheet (str (row-field row sheet-prop))
                             :range r
                             :row   row
                             :layer layer}))
                        rows)))
        layers))

(defn annotation-at
  [annos sheet at]
  (some #(when (and (= sheet (:sheet %)) (in-range? (:range %) at)) %) annos))

(defn seek-target
  [{:keys [sheet range]} sheets grid-of]
  (when (some #{sheet} sheets)
    {:sheet sheet
     :range (when (within-sheet? range (grid-of sheet)) range)}))

(defn selection-row
  [sheet selected annotation]
  (let [text (when selected (range-text selected))]
    (merge (:row annotation)
           {:sheet sheet :range text
            :values (merge (:values (:row annotation)) {:sheet sheet :range text})})))
