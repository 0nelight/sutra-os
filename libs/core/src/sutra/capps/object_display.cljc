(ns sutra.capps.object-display
  (:require [sutra.kit.coll :as coll]
            [clojure.string :as str]
            [sutra.types.decimal :as d]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.capps.table :as tbl]
            [sutra.ref.expr :as rexpr]
            [sutra.kit.url :as url]))

(defn col-id
  [{:keys [id prop]}]
  (some-> (or id prop) ->kw name))

(defn visible-cols
  [cols visible]
  (if (empty? visible)
    (vec cols)
    (let [by-id (into {} (keep (fn [c] (when-let [i (col-id c)] [i c]))) cols)]
      (into (filterv (comp nil? col-id) cols)
            (keep #(get by-id (some-> % ->kw name)))
            visible))))

(defn toggle-visible
  [all-ids visible id]
  (let [shown (if (seq visible) (vec visible) (vec all-ids))]
    (if (some #{id} shown)
      (let [left (filterv #(not= id %) shown)] (if (empty? left) (vec all-ids) left))
      (conj shown id))))

(defn- row-env
  [row]
  (reduce-kv (fn [m k x] (assoc m (->kw k) (if (d/exact? x) (d/host-number x) x)))
             {} (or (:values row) row)))

(def ^:private math {:+ + :- - :* * :/ /})

(defn- calc
  [env e]
  (if-let [op (and (vector? e) (get math (first e)))]
    (let [args (mapv #(calc env %) (rest e))]
      (when (every? number? args)
        (when-not (and (= :/ (first e)) (some zero? (rest args)))
          (apply op args))))
    (rexpr/eval env e)))

(defn- agg-of
  [agg vals*]
  (if (= :count (->kw agg))
    (count vals*)
    (tbl/pivot-agg agg (filterv number? vals*))))

(defn- joined-reader
  [{:keys [from on match value agg]}]
  (let [on*    (or (tbl/field-fn on) :id)
        match* (or (tbl/field-fn match) :id)
        value* (or (tbl/field-fn value) identity)
        index  (group-by (comp str match*) (if (map? from) [from] from))]
    (fn [row]
      (let [hits (get index (str (on* row)))]
        (if agg
          (agg-of agg (mapv value* hits))
          (some-> (first hits) value*))))))

(defn col-reader
  [{:keys [from expr prop value-fn] :as c}]
  (cond
    value-fn            value-fn
    (contains? c :from) (joined-reader (assoc c :from (or from [])))
    expr                (fn [row]
                          (try (calc (row-env row) expr)
                               (catch #?(:clj Exception :cljs :default) _ nil)))
    prop                (tbl/field-fn prop)
    :else               (constantly nil)))

(defn safe-url
  [v]
  (url/safe-url v))

(defn cell-look
  [{:keys [conditional-format]} entry row]
  (tbl/cond-format (or (seq conditional-format) (:conditional-format entry))
                   (or (:values row) row)))

(defn next-sort
  [{:keys [id dir]} clicked]
  (cond
    (not= id clicked) {:id clicked :dir :asc}
    (= :asc dir)      {:id clicked :dir :desc}
    :else             nil))

(defn sorted-by
  [rows reader dir]
  (let [cmp (if (= :desc dir) #(tbl/cmp-vals %2 %1) tbl/cmp-vals)]
    (vec (sort-by reader cmp rows))))

(defn type-groups
  [rows]
  (let [ts (distinct (map :object-type-ref rows))]
    (mapv (fn [t] {:type t :rows (filterv #(= t (:object-type-ref %)) rows)}) ts)))

(defn export-sheet
  [rows cols custom-names?]
  (let [cols (filterv col-id cols)]
    {:header (mapv #(or (when custom-names? (not-empty (str (:label %)))) (col-id %)) cols)
     :rows   (let [rs (mapv col-reader cols)]
               (mapv (fn [row] (mapv #(% row) rs)) rows))}))

(defn- csv-cell
  [v]
  (let [s (if (nil? v) "" (str v))
        s (if (and (not (number? v)) (re-find #"^[=+\-@\t\r]" s)) (str "'" s) s)]
    (if (re-find #"[\",\n\r]" s)
      (str "\"" (str/replace s "\"" "\"\"") "\"")
      s)))

(defn csv-text
  [{:keys [header rows]}]
  (str/join "\r\n" (map #(str/join "," (map csv-cell %)) (cons header rows))))

(defn- xml-text
  [s]
  (-> (str s)
      (str/replace "&" "&amp;") (str/replace "<" "&lt;") (str/replace ">" "&gt;")
      (str/replace #"[\u0000-\u0008\u000B\u000C\u000E-\u001F]" "")))

(defn- sheet-cell
  [v]
  (cond
    (nil? v)    "<c/>"
    (number? v) (str "<c><v>" v "</v></c>")
    :else       (str "<c t=\"inlineStr\"><is><t xml:space=\"preserve\">" (xml-text v)
                     "</t></is></c>")))

(defn xlsx-parts
  [{:keys [header rows]}]
  (let [xml  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
        pkg  "http://schemas.openxmlformats.org/package/2006/"
        off  "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
        main "http://schemas.openxmlformats.org/spreadsheetml/2006/main"
        ct   "application/vnd.openxmlformats-officedocument.spreadsheetml"
        line #(str "<row>" (str/join (map sheet-cell %)) "</row>")]
    [["[Content_Types].xml"
      (str xml "<Types xmlns=\"" pkg "content-types\">"
           "<Default Extension=\"rels\" ContentType=\"application/vnd.openxmlformats-package."
           "relationships+xml\"/><Default Extension=\"xml\" ContentType=\"application/xml\"/>"
           "<Override PartName=\"/xl/workbook.xml\" ContentType=\"" ct ".sheet.main+xml\"/>"
           "<Override PartName=\"/xl/worksheets/sheet1.xml\" ContentType=\"" ct
           ".worksheet+xml\"/></Types>")]
     ["_rels/.rels"
      (str xml "<Relationships xmlns=\"" pkg "relationships\"><Relationship Id=\"rId1\" Type=\""
           off "/officeDocument\" Target=\"xl/workbook.xml\"/></Relationships>")]
     ["xl/workbook.xml"
      (str xml "<workbook xmlns=\"" main "\" xmlns:r=\"" off "\"><sheets><sheet name=\"Export\""
           " sheetId=\"1\" r:id=\"rId1\"/></sheets></workbook>")]
     ["xl/_rels/workbook.xml.rels"
      (str xml "<Relationships xmlns=\"" pkg "relationships\"><Relationship Id=\"rId1\" Type=\""
           off "/worksheet\" Target=\"worksheets/sheet1.xml\"/></Relationships>")]
     ["xl/worksheets/sheet1.xml"
      (str xml "<worksheet xmlns=\"" main "\"><sheetData>"
           (str/join (map line (cons header rows))) "</sheetData></worksheet>")]]))

(defn split-by-order
  [rows keys* keyf]
  (let [by-key (into {} (map (fn [r] [(str (keyf r)) r])) rows)
        named  (into #{} (map str) keys*)]
    {:sorted   (into [] (keep #(get by-key (str %))) keys*)
     :unsorted (filterv #(not (contains? named (str (keyf %)))) rows)}))

(defn reorder-keys
  [keys* visible moved index]
  (let [keys*   (mapv str keys*)
        moved   (str moved)
        visible (filterv #(not= moved %) (mapv str visible))
        left    (filterv #(not= moved %) keys*)
        pos     (fn [k] (first (keep-indexed (fn [i x] (when (= k x) i)) left)))
        at      (cond
                  (< index (count visible)) (pos (nth visible index))
                  (seq visible)             (some-> (pos (peek visible)) inc)
                  :else                     (count left))
        at      (or at (count left))]
    (into (conj (subvec left 0 at) moved) (subvec left at))))

(defn drop-key
  [keys* moved]
  (filterv #(not= (str moved) (str %)) keys*))

(defn sort-options
  [props extra]
  (into [] (comp (keep #(if (map? %) (:prop %) %)) (map ->kw) (distinct))
        (concat props extra)))

(defn set-title
  [{:keys [title single? count* type-label row-title render-empty?]}]
  (cond
    (and (zero? count*) (not render-empty?)) nil
    (not (str/blank? (str title)))           {:text title}
    (and single? (pos? count*))              {:text row-title}
    :else                                    {:text type-label :count count*}))

(defn link-specs
  [links specified]
  (if (empty? specified)
    (mapv (fn [l] {:api (->kw (:api l)) :label (or (:label l) (name (:api l)))}) links)
    (let [by-api (coll/index-by (comp ->kw :api) links)]
      (into []
            (keep (fn [{:keys [link label sort-by sort-dir preview]}]
                    (when-let [l (get by-api (->kw link))]
                      {:api      (->kw link)
                       :label    (or label (:label l) (name link))
                       :sort-by  sort-by
                       :sort-dir sort-dir
                       :preview  preview})))
            specified))))

(defn link-peers
  [{:keys [sort-by sort-dir]} rows]
  (if-let [f (tbl/field-fn sort-by)]
    (sorted-by rows f (->kw sort-dir))
    (vec rows)))

(defn initially-open
  [specs n]
  (into #{} (map :api) (take (or n 0) specs)))

(defn keep-alive
  [{:keys [body]}]
  (let [leaves (filter #(and (map? %) (:widget %)) (tree-seq coll? seq body))]
    {:eager? (boolean (some #(= :eager (->kw (:mount %))) leaves))
     :never? (boolean (some #(= :never (->kw (:unmount %))) leaves))}))

(defn- ->ms
  [t]
  (cond
    (number? t) t
    :else #?(:cljs (.parse js/Date (str t))
             :clj  (.toEpochMilli (java.time.Instant/parse (str t))))))

(defn points
  [raw]
  (->> raw
       (keep (fn [[t x]]
               (let [x (if (d/exact? x) (d/host-number x) x)]
                 (when (and (number? x) #?(:cljs (js/isFinite x) :clj true))
                   [(->ms t) x]))))
       (sort-by first)
       vec))

(defn- sqrt [x] #?(:cljs (js/Math.sqrt x) :clj (Math/sqrt x)))

(defn aggregate
  [agg xs]
  (when (seq xs)
    (case (->kw agg)
      :count               (count xs)
      :first               (first xs)
      :last                (last xs)
      :min                 (apply min xs)
      :max                 (apply max xs)
      :sum                 (reduce + xs)
      :product             (reduce * xs)
      :difference          (- (last xs) (first xs))
      :relative-difference (when-not (zero? (first xs))
                             (/ (- (last xs) (first xs)) (first xs)))
      :std-dev             (let [m (/ (reduce + xs) (count xs))]
                             (sqrt (/ (reduce + (map #(let [d (- % m)] (* d d)) xs))
                                      (count xs))))
      (/ (reduce + xs) (count xs)))))

(defn in-range
  [pts {:keys [last from until]} now]
  (let [lo (cond last (- now last) from (->ms from))
        hi (when until (->ms until))]
    (filterv (fn [[t]] (and (or (nil? lo) (>= t lo)) (or (nil? hi) (<= t hi)))) pts)))

(defn- values-of [pts] (mapv second pts))

(defn- periodic
  [pts {:keys [window agg align end?]}]
  (let [align (or (some-> align ->ms) 0)]
    (->> pts
         (partition-by (fn [[t]] (quot (- t align) window)))
         (mapv (fn [part]
                 (let [start (+ align (* window (quot (- (ffirst part) align) window)))]
                   [(if end? (+ start window) start) (aggregate agg (values-of part))]))))))

(defn transform
  [pts {:keys [kind window agg unit by] :as spec} now]
  (let [unit (or unit 1000)]
    (case (->kw kind)
      :cumulative (into [] (map-indexed
                            (fn [i [t]]
                              [t (aggregate agg (values-of (subvec pts 0 (inc i))))]))
                        pts)
      :rolling    (mapv (fn [[t]]
                          [t (aggregate agg (values-of (filterv (fn [[u]] (and (<= u t)
                                                                               (> u (- t window))))
                                                                pts)))])
                        pts)
      :periodic   (periodic pts spec)
      :derivative (mapv (fn [[t0 x0] [t1 x1]] [t1 (* unit (/ (- x1 x0) (- t1 t0)))])
                        pts (rest pts))
      :integral   (:out (reduce (fn [{:keys [sum prev out]} [t x :as p]]
                                  (let [sum (if prev
                                              (+ sum (/ (* (+ x (second prev)) (- t (first prev)))
                                                        (* 2 unit)))
                                              sum)]
                                    {:sum sum :prev p :out (conj out [t sum])}))
                                {:sum 0 :prev nil :out []} pts))
      :range      (in-range pts spec now)
      :shift      (mapv (fn [[t x]] [(+ t by) x]) pts)
      :formula    (into [] (keep (fn [[t x]]
                                   (let [y (try (calc {:value x} (:expr spec))
                                                (catch #?(:clj Exception :cljs :default) _ nil))]
                                     (when (number? y) [t y]))))
                        pts)
      pts)))

(defn apply-transforms
  [pts transforms now]
  (reduce #(transform %1 %2 now) pts transforms))

(defn summarize
  [pts {:keys [agg range change? base]} now]
  (let [one (fn [r] (aggregate (or agg :last) (values-of (if r (in-range pts r now) pts))))]
    (if change?
      (let [a (one range) b (one base)] (when (and a b) (apply - [a b])))
      (one range))))

(defn sparkline
  [pts w h baseline]
  (when (seq pts)
    (let [xs   (map second pts)
          lo   (apply min (cond-> xs baseline (conj baseline)))
          hi   (apply max (cond-> xs baseline (conj baseline)))
          t0   (ffirst pts)
          span (let [s (- (first (peek pts)) t0)] (if (pos? s) s 1))
          dy   (let [s (- hi lo)] (if (pos? s) s 1))
          y    (fn [v] (- h (* h (/ (- v lo) dy))))]
      {:d (str/join " " (map-indexed (fn [i [t v]]
                                       (str (if (zero? i) "M" "L")
                                            (* w (/ (- t t0) span)) " " (y v)))
                                     pts))
       :baseline-y (when baseline (y baseline))})))
