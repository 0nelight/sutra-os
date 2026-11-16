(ns sutra.capps.pivot
  (:require [clojure.string :as str]
            [sutra.capps.format :refer [v]]
            [sutra.kit.coerce :refer [->kw]]))

(defn- field
  [row k]
  (let [k (->kw k)]
    (if (contains? row k) (get row k) (get (v row) k))))

(defn- date-parts
  [x]
  (when-let [[_ y m d] (re-find #"^(\d{4})-(\d{2})-(\d{2})" (str x))]
    [(parse-long y) (parse-long m) (parse-long d)]))

(defn- days-from-civil
  [y m d]
  (let [y   (if (<= m 2) (dec y) y)
        era (quot y 400)
        yoe (- y (* era 400))
        doy (+ (quot (+ (* 153 (+ m (if (> m 2) -3 9))) 2) 5) (dec d))
        doe (+ (* yoe 365) (quot yoe 4) (- (quot yoe 100)) doy)]
    (+ (* era 146097) doe -719468)))

(defn- civil-from-days
  [z]
  (let [z   (+ z 719468)
        era (quot z 146097)
        doe (- z (* era 146097))
        yoe (quot (- doe (quot doe 1460) (- (quot doe 36524)) (quot doe 146096)) 365)
        doy (- doe (- (+ (* 365 yoe) (quot yoe 4)) (quot yoe 100)))
        mp  (quot (+ (* 5 doy) 2) 153)
        d   (inc (- doy (quot (+ (* 153 mp) 2) 5)))
        m   (if (< mp 10) (+ mp 3) (- mp 9))
        y   (+ yoe (* era 400) (if (<= m 2) 1 0))]
    [y m d]))

(defn- pad2 [n] (if (< n 10) (str "0" n) (str n)))

(defn- date-text [[y m d]] (str y "-" (pad2 m) "-" (pad2 d)))

(defn- monday-of
  [[y m d]]
  (let [days (days-from-civil y m d)
        wd   (mod (+ days 3) 7)]
    (civil-from-days (- days wd))))

(defn bucket
  [x interval]
  (let [iv (->kw interval)]
    (if-let [[y m _ :as p] (when (and iv (not= :exact iv)) (date-parts x))]
      (case iv
        :date    (date-text p)
        :week    (date-text (monday-of p))
        :month   (str y "-" (pad2 m))
        :quarter (str y "-Q" (inc (quot (dec m) 3)))
        :year    (str y)
        x)
      x)))

(defn aggregate
  [agg prop rows]
  (let [a    (or (->kw agg) :sum)
        vals (when prop (keep #(field % prop) rows))
        nums (filter number? vals)]
    (case a
      :count       (if prop (count vals) (count rows))
      :cardinality (count (distinct vals))
      :avg         (when (seq nums) (/ (reduce + nums) (count nums)))
      :min         (when (seq nums) (reduce min nums))
      :max         (when (seq nums) (reduce max nums))
      (reduce + 0 nums))))

(defn normalize
  [{:keys [row row-groups value agg aggs col col-interval col-totals? row-totals? totals?
           row-label col-label]
    :as cfg}]
  (let [all? (not (false? totals?))]
    (assoc cfg
           :row-groups  (or (seq row-groups)
                            (when row [{:prop row :label-prop row-label}]))
           :aggs        (or (seq aggs) [{:prop value :agg agg}])
           :col-group   (when col {:prop col :interval col-interval :label-prop col-label})
           :row-totals? (if (some? row-totals?) row-totals? all?)
           :col-totals? (if (some? col-totals?) col-totals? all?))))

(defn- order
  [xs dir]
  (let [cmp (fn [a b]
              (cond (and (nil? a) (nil? b)) 0
                    (nil? a) 1
                    (nil? b) -1
                    (and (number? a) (number? b)) (compare a b)
                    :else (compare (str a) (str b))))
        s   (sort cmp xs)]
    (if (= :desc (->kw dir)) (vec (reverse s)) (vec s))))

(defn- group-value
  [row {:keys [prop interval]}]
  (let [x (bucket (field row prop) interval)]
    (if (and (map? x) (contains? x :id)) (:id x) x)))

(defn- label-of
  [v rep {:keys [label-prop prop]}]
  (let [raw (field rep prop)]
    (cond
      (and (map? raw) (contains? raw :id))
      (or (:title raw) (str (:id raw)))

      (and label-prop (some? (field rep label-prop)))
      (str (field rep label-prop))

      (nil? v) "—"
      :else    (str v))))

(defn- sel-of
  [groups path]
  (into {} (map (fn [g v] [(->kw (:prop g)) v]) groups path)))

(defn- cell
  [agg rows sel]
  {:value (aggregate (:agg agg) (:prop agg) rows) :fmt (:fmt agg) :sel sel :rows rows})

(defn- leaf-paths
  [rows groups]
  (let [paths  (distinct (map (fn [r] (mapv #(group-value r %) groups)) rows))
        sorted (reduce (fn [ps i]
                         (let [dir (or (:sort (nth groups i)) :asc)
                               os  (order (distinct (map #(nth % i) ps)) dir)
                               at  (into {} (map-indexed (fn [n x] [x n])) os)]
                           (sort-by #(get at (nth % i)) ps)))
                       paths
                       (reverse (range (count groups))))]
    (vec sorted)))

(defn- measure-cells
  [{:keys [aggs col-group col-totals? placement-rows?]} row-rows row-sel col-keys]
  (let [by-col (when col-group (group-by #(group-value % col-group) row-rows))
        cells-of (fn [agg]
                   (if col-group
                     (cond-> (mapv (fn [ck]
                                     (cell agg (get by-col ck [])
                                           (assoc row-sel (->kw (:prop col-group)) ck)))
                                   col-keys)
                       col-totals? (conj (assoc (cell agg row-rows row-sel) :total? true)))
                     [(cell agg row-rows row-sel)]))]
    (if placement-rows?
      (map cells-of aggs)
      [(into [] (mapcat cells-of) aggs)])))

(defn- expand-row
  [opts {:keys [groups-shown key depth total?] :as base} rows sel col-keys]
  (let [per (measure-cells opts rows sel col-keys)]
    (map-indexed
     (fn [i cells]
       (cond-> (assoc base
                      :key (str key "/" i)
                      :heads groups-shown
                      :cells cells
                      :depth depth
                      :total? total?)
         (:placement-rows? opts) (assoc :measure (or (:title (nth (:aggs opts) i))
                                                     (name (or (->kw (:agg (nth (:aggs opts) i)))
                                                               :sum))))))
     per)))

(defn- tabular-rows
  [opts rows groups col-keys]
  (let [paths (leaf-paths rows groups)
        reps  (group-by (fn [r] (mapv #(group-value r %) groups)) rows)]
    (into []
          (mapcat
           (fn [[prev path]]
             (let [rs    (get reps path)
                   rep   (first rs)
                   heads (mapv (fn [g v i]
                                 (let [same? (and prev (= (take (inc i) prev) (take (inc i) path)))]
                                   {:label (when-not same? (label-of v rep g))
                                    :group? (zero? i)
                                    :sel   (sel-of (take (inc i) groups) (take (inc i) path))}))
                               groups path (range))
                   first-of-group? (not= (first prev) (first path))]
               (expand-row opts {:groups-shown heads :key (pr-str path) :depth 0
                                 :top? first-of-group?}
                           rs (sel-of groups path) col-keys))))
          (map vector (cons nil paths) paths))))

(defn- stacked-rows
  [opts rows groups col-keys]
  (letfn [(level [rows groups prefix-groups prefix depth]
            (let [g     (first groups)
                  byv   (group-by #(group-value % g) rows)
                  vs    (order (keys byv) (or (:sort g) :asc))]
              (mapcat
               (fn [v]
                 (let [rs    (get byv v)
                       gs    (conj prefix-groups g)
                       path  (conj prefix v)
                       sel   (sel-of gs path)
                       self  (expand-row opts
                                         {:groups-shown [{:label (label-of v (first rs) g)
                                                          :sel sel}]
                                          :key (pr-str path) :depth depth
                                          :top? (zero? depth)}
                                         rs sel col-keys)]
                   (concat self
                           (when (next groups)
                             (level rs (rest groups) gs path (inc depth))))))
               vs)))]
    (vec (level rows groups [] [] 0))))

(defn- column-keys
  [rows {:keys [col-group col-sort]}]
  (when col-group
    (order (distinct (map #(group-value % col-group) rows)) (or col-sort :asc))))

(defn- col-reps
  [rows col-group]
  (when col-group
    (into {} (map (fn [r] [(group-value r col-group) r])) (reverse rows))))

(defn- agg-title
  [{:keys [title agg prop]}]
  (or title
      (when prop (str (name (->kw prop))
                      (when agg (str " · " (name (->kw agg))))))
      (some-> agg ->kw name)
      "count"))

(defn- object-header
  [{:keys [aggs col-group col-totals? corner stacked? placement-rows? stacked-label]}
   groups col-keys reps]
  (let [head-cols (if stacked?
                    [{:label (or stacked-label corner "") :kind :group
                      :width (:width (first groups))}]
                    (map-indexed (fn [i g] {:label (or (:label g) (when (zero? i) corner)
                                                       (name (->kw (:prop g))))
                                            :kind :group :width (:width g) :i i})
                                 groups))
        head-cols (cond-> (vec head-cols)
                    placement-rows? (conj {:label "" :kind :measure}))
        col-cells (when col-group
                    (cond-> (mapv (fn [ck]
                                    {:label (label-of ck (get reps ck) col-group)
                                     :kind :col
                                     :sel {(->kw (:prop col-group)) ck}
                                     :span (if placement-rows? 1 (count aggs))})
                                  col-keys)
                      col-totals? (conj {:label "Σ" :kind :total
                                         :span (if placement-rows? 1 (count aggs))})))
        agg-cells (when-not placement-rows?
                    (let [one (map (fn [a] {:label (agg-title a) :kind :agg :width (:width a)})
                                   aggs)
                          n   (if col-group (+ (count col-keys) (if col-totals? 1 0)) 1)]
                      (into [] (mapcat (fn [_] one)) (range n))))]
    (cond
      (and col-group (or placement-rows? (= 1 (count aggs))))
      [(into head-cols col-cells)]

      col-group
      [(into (mapv #(assoc % :rowspan 2) head-cols) col-cells) agg-cells]

      placement-rows?
      [(conj head-cols {:label "" :kind :agg})]

      :else
      [(into head-cols agg-cells)])))

(defn- totals-foot
  [{:keys [aggs placement-rows? stacked?] :as opts} rows groups col-keys]
  (let [width (if stacked? 1 (count groups))
        per   (measure-cells opts rows {} col-keys)
        foot  (fn [i cells]
                {:key     (str "total/" i)
                 :total?  true
                 :heads   (into [{:label "Σ" :total? true}]
                                (repeat (dec (max width 1)) {:label nil}))
                 :measure (when placement-rows? (agg-title (nth aggs i)))
                 :cells   cells})]
    (vec (map-indexed foot per))))

(defn object-table
  [cfg]
  (let [{:keys [row-groups col-group row-totals? placement layout rows] :as opts} (normalize cfg)
        groups   (vec row-groups)
        rows     (vec rows)
        stacked? (= :stacked (->kw layout))
        opts     (assoc opts
                        :stacked? stacked?
                        :placement-rows? (= :rows (->kw placement)))
        col-keys (column-keys rows opts)
        reps     (col-reps rows col-group)]
    {:head (object-header opts groups col-keys reps)
     :body (cond
             (empty? groups) (vec (expand-row opts {:groups-shown [] :key "all" :depth 0}
                                              rows {} col-keys))
             stacked?        (stacked-rows opts rows groups col-keys)
             :else           (tabular-rows opts rows groups col-keys))
     :foot (when (and row-totals? (seq groups))
             (totals-foot opts rows groups col-keys))}))

(defn- struct-values
  [row]
  (if (map? (:values row)) (:values row) row))

(defn- struct-field?
  [row k]
  (contains? row (->kw k)))

(defn- struct-path
  [row groups]
  (mapv #(when (struct-field? row %) [(get row (->kw %))]) groups))

(defn- visible-path?
  [path groups expandable expanded]
  (let [expandable (set (map ->kw expandable))]
    (every? (fn [i]
              (let [g (->kw (nth groups i))]
                (or (nil? (nth path i))
                    (not (expandable g))
                    (zero? i)
                    (contains? expanded (subvec path 0 i)))))
            (range (count groups)))))

(defn- has-children?
  [path rows groups]
  (let [n     (count (take-while some? path))
        same? (fn [p] (and (= (subvec p 0 n) (subvec path 0 n))
                           (< n (count p))
                           (some? (nth p n))))]
    (boolean (and (< n (count groups))
                  (some #(same? (struct-path % groups)) rows)))))

(defn- tree-order
  [paths]
  (let [rank (reduce (fn [m p]
                       (reduce (fn [m i]
                                 (let [k (subvec p 0 (inc i))]
                                   (if (or (nil? (nth p i)) (contains? m k))
                                     m
                                     (assoc m k (count m)))))
                               m (range (count p))))
                     {} paths)
        at   (fn [p] (mapv (fn [i] (if (nth p i) (get rank (subvec p 0 (inc i))) -1))
                           (range (count p))))]
    (sort-by at paths)))

(defn function-table
  [{:keys [rows group-rows group-col value-fields expandable col-sort expanded corner]}]
  (let [groups  (mapv ->kw group-rows)
        col     (some-> group-col ->kw)
        fields  (mapv (fn [f] (if (map? f) f {:field f})) value-fields)
        expanded (or expanded #{})
        ckeys   (when col
                  (order (distinct (keep (fn [r] (when (contains? r col) (get r col))) rows))
                         (or col-sort :asc)))
        total-col? (and col (some #(not (contains? % col)) rows))
        col-slots (if col
                    (cond-> (mapv (fn [k] {:k [k] :label (if (nil? k) "—" (str k))}) ckeys)
                      total-col? (conj {:k nil :label "Σ"}))
                    [{:k nil :label nil}])
        at      (into {}
                      (map (fn [r] [[(struct-path r groups)
                                     (when (and col (contains? r col)) [(get r col)])]
                                    r]))
                      rows)
        paths   (tree-order (distinct (map #(struct-path % groups) rows)))
        leaf?   (fn [p] (some some? p))
        cells   (fn [p]
                  (into []
                        (mapcat
                         (fn [{:keys [k]}]
                           (let [r (get at [p k])]
                             (map (fn [{:keys [field fmt]}]
                                    {:value (when r (get (struct-values r) (->kw field)))
                                     :fmt   fmt
                                     :total? (and col (nil? k))
                                     :struct r})
                                  fields))))
                        col-slots))
        body-row (fn [p]
                   (let [n     (count (take-while some? p))
                         g     (when (pos? n) (nth groups (dec n)))
                         label (when g (first (nth p (dec n))))
                         open? (has-children? p rows groups)]
                     {:key    (pr-str p)
                      :depth  (max 0 (dec n))
                      :heads  [{:label (if (and g (nil? label)) "—" (some-> label str))
                                :struct (get at [p nil])}]
                      :path   p
                      :expandable? (and open? (some #{(get groups n)} (map ->kw expandable)))
                      :expanded?   (contains? expanded (subvec p 0 n))
                      :cells  (cells p)}))
        visible (filter #(visible-path? % groups expandable expanded) paths)]
    {:head (let [heads [{:label (or corner "") :kind :group}]
                 span  (count fields)]
             (cond
               (and col (> span 1))
               [(into (mapv #(assoc % :rowspan 2) heads)
                      (map (fn [{:keys [label]}] {:label label :kind :col :span span}) col-slots))
                (into []
                      (mapcat (fn [_]
                                (map (fn [f] {:label (or (:title f) (name (->kw (:field f))))
                                              :kind  :agg})
                                     fields)))
                      col-slots)]
               col
               [(into heads (map (fn [{:keys [label]}] {:label label :kind :col}) col-slots))]
               :else
               [(into heads (map (fn [f] {:label (or (:title f) (name (->kw (:field f))))
                                          :kind :agg})
                                 fields))]))
     :body (mapv body-row (filter leaf? visible))
     :foot (when-let [t (some #(when-not (leaf? %) %) paths)]
             [(assoc (body-row t) :total? true
                     :heads [{:label "Σ" :total? true}])])}))

(defn table
  [cfg]
  (if (seq (:value-fields cfg))
    (function-table cfg)
    (object-table cfg)))

(defn toggle-selection
  [selected sel multi?]
  (let [selected (or selected [])]
    (cond
      (nil? sel)                  selected
      (not multi?)                (if (= selected [sel]) [] [sel])
      (some #(= sel %) selected)  (filterv #(not= sel %) selected)
      :else                       (conj selected sel))))

(defn- matches?
  [row groups {:keys [row-groups col-group]}]
  (every? (fn [[k v]]
            (let [g (or (some #(when (= k (->kw (:prop %))) %) row-groups)
                        (when (= k (->kw (:prop col-group))) col-group)
                        {:prop k})]
              (= v (group-value row g))))
          groups))

(defn selected-rows
  [cfg selected]
  (let [opts (normalize cfg)]
    (filterv (fn [row] (some #(matches? row % opts) selected)) (:rows cfg))))

(defn style-class
  [{:keys [table-style fill-width? layout]}]
  (str/join " "
            (cond-> ["capp-table__table" "capp-pivot"]
              (= :stacked (->kw layout)) (conj "capp-pivot--stacked")
              fill-width?                (conj "capp-pivot--fill")
              (#{:outlined :banded} (->kw table-style)) (conj "capp-pivot--outlined")
              (= :banded (->kw table-style))            (conj "capp-pivot--banded"))))
