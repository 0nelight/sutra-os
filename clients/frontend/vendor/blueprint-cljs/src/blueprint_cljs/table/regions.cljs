(ns blueprint-cljs.table.regions
  "Regions as data, as Blueprint's `Regions`: `{:rows [r0 r1] :cols [c0 c1]}` is a block
   of cells, `{:rows [r0 r1]}` full rows, `{:cols [c0 c1]}` full columns, `{}` the whole
   table. Ends are inclusive. Pure.")

(defn cell [r c] {:rows [r r] :cols [c c]})
(defn cells [r0 c0 r1 c1] {:rows [(min r0 r1) (max r0 r1)] :cols [(min c0 c1) (max c0 c1)]})
(defn row [r0 r1] {:rows [(min r0 r1) (max r0 r1)]})
(defn column [c0 c1] {:cols [(min c0 c1) (max c0 c1)]})
(def table {})

(defn cardinality
  [{:keys [rows cols]}]
  (cond
    (and rows cols) :cells
    rows            :full-rows
    cols            :full-columns
    :else           :full-table))

(def selection-modes
  "Blueprint's `SelectionModes` presets."
  {:all               #{:full-table :full-columns :full-rows :cells}
   :columns-and-cells #{:full-columns :cells}
   :columns-only      #{:full-columns}
   :none              #{}
   :rows-and-cells    #{:full-rows :cells}
   :rows-only         #{:full-rows}})

(defn modes
  "A `:selection-modes` prop (preset keyword or collection of cardinalities) as a set."
  [m]
  (cond
    (nil? m)     (:all selection-modes)
    (keyword? m) (get selection-modes m #{})
    :else        (set m)))

(defn bounds
  "Concrete inclusive [[r0 r1] [c0 c1]] of `region` in a table of n-rows × n-cols."
  [{:keys [rows cols]} n-rows n-cols]
  [(or rows [0 (dec n-rows)]) (or cols [0 (dec n-cols)])])

(defn- in? [[a b] i] (and (<= a i) (<= i b)))

(defn contains-cell?
  [{:keys [rows cols]} r c]
  (and (or (nil? rows) (in? rows r)) (or (nil? cols) (in? cols c))))

(defn any-contains-cell? [regions r c] (some #(contains-cell? % r c) regions))

(defn column-selected?
  "True when a full-column (or full-table) region covers column `c`."
  [regions c]
  (some (fn [{:keys [rows cols]}] (and (nil? rows) (or (nil? cols) (in? cols c)))) regions))

(defn row-selected?
  [regions r]
  (some (fn [{:keys [rows cols]}] (and (nil? cols) (or (nil? rows) (in? rows r)))) regions))

(defn column-touched?
  "True when any region includes column `c` (headers show this as active)."
  [regions c]
  (some (fn [{:keys [cols]}] (or (nil? cols) (in? cols c))) regions))

(defn row-touched?
  [regions r]
  (some (fn [{:keys [rows]}] (or (nil? rows) (in? rows r))) regions))

(defn expand
  "Region `kind` spanning from anchor cell [ar ac] to [r c]."
  [kind [ar ac] [r c]]
  (case kind
    :cells        (cells ar ac r c)
    :full-rows    (row ar r)
    :full-columns (column ac c)
    :full-table   table))

(defn clamp
  "`region` cut to the table; nil when nothing remains."
  [{:keys [rows cols] :as region} n-rows n-cols]
  (let [cut (fn [[a b] n] (let [a (max 0 a) b (min (dec n) b)] (when (<= a b) [a b])))
        rows' (when rows (cut rows n-rows))
        cols' (when cols (cut cols n-cols))]
    (when (and (or (nil? rows) rows') (or (nil? cols) cols'))
      (cond-> region rows (assoc :rows rows') cols (assoc :cols cols')))))

(defn enumerate-cells
  "Every [r c] the regions cover, unique, in row-major order. Beware full columns of
   big tables: this realises every cell."
  [regions n-rows n-cols]
  (->> regions
       (mapcat (fn [reg]
                 (let [[[r0 r1] [c0 c1]] (bounds reg n-rows n-cols)]
                   (for [r (range r0 (inc r1)) c (range c0 (inc c1))] [r c]))))
       distinct
       (sort-by (juxt first second))))

(defn reorder
  "`v` with the block [from, from+len) moved so it starts at `to` (after removal)."
  [v from to len]
  (let [v     (vec v)
        block (subvec v from (+ from len))
        rest' (into (subvec v 0 from) (subvec v (+ from len)))]
    (into (into (subvec rest' 0 to) block) (subvec rest' to))))
