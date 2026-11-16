(ns sutra.capps.chart-spec
  (:require [sutra.kit.coerce :refer [->kw]]
            [sutra.types.aggregate :as agg]
            [sutra.types.decimal :as d]
            [sutra.capps.format :refer [v num]]))

(defn prop
  [row k]
  (if (contains? row k) (get row k) (get (v row) k)))

(defn axis-vals [rows k] (mapv #(prop % k) rows))

(defn y-vals
  [rows k]
  (mapv #(d/lossy-number (num (prop % k))) rows))

(defn- by-x
  [rows k]
  (let [n?  (fn [r] (some? (num (prop r k))))]
    (if (every? n? rows)
      (vec (sort-by #(d/lossy-number (num (prop % k))) rows))
      (vec (sort-by #(str (prop % k)) rows)))))

(def palette
  ["#1f6e63" "#b87914" "#4f6bd8" "#b3446c" "#3f7f3f" "#6b4a34" "#7a5ea8" "#8a8f98"])

(def ^:private kind->trace
  {:line        {:type "scatter" :mode "lines"}
   :time-series {:type "scatter" :mode "lines"}
   :spline      {:type "scatter" :mode "lines" :line {:shape "spline"}}
   :area        {:type "scatter" :mode "lines" :fill "tozeroy"}
   :scatter     {:type "scatter" :mode "markers"}
   :line+markers {:type "scatter" :mode "lines+markers"}
   :bar         {:type "bar"}
   :waterfall   {:type "waterfall"}
   :pie         {:type "pie"}})

(defn aggregation->rows
  [{:keys [buckets]}]
  (vec (mapcat (fn [{:keys [key value]}]
                 (if (sequential? value)
                   (map (fn [s] {:key key :segment (:key s) :value (:value s)}) value)
                   [{:key key :value value}]))
               buckets)))

(defn- with-aggregation
  [{:keys [aggregation] :as cfg}]
  (if-not (map? aggregation)
    cfg
    (let [rows (aggregation->rows aggregation)]
      (cond-> (assoc (dissoc cfg :aggregation) :rows rows :x :key :y :value)
        (some :segment rows) (assoc :group :segment)))))

(defn- segment-of
  [{:keys [segments]} value]
  (some #(when (= (str (:value %)) (str value)) %) segments))

(defn- named-segment
  [cfg value fallback]
  (or (:label (segment-of cfg value)) fallback))

(defn- colored
  [trace color]
  (if color
    (assoc trace
           :marker (merge (:marker trace) {:color color})
           :line   (merge (:line trace) {:color color}))
    trace))

(defn- stacked
  [trace stacked?]
  (if-not stacked?
    trace
    (if (= "bar" (:type trace))
      trace
      (assoc trace :stackgroup "one"
             :fill (if (contains? trace :fill) "tonexty" "none")))))

(defn- labelled
  [trace labels?]
  (if-not labels?
    trace
    (case (:type trace)
      "bar"     (assoc trace :text (mapv str (:y trace)) :textposition "auto")
      "scatter" (assoc trace :text (mapv str (:y trace)) :textposition "top center"
                       :mode (str (:mode trace "lines") "+text"))
      trace)))

(defn- segment-trace
  [trace cfg value]
  (let [{:keys [color hidden?]} (segment-of cfg value)]
    (cond-> (colored trace color)
      hidden? (assoc :visible "legendonly"))))

(defn- y-axis-of
  [{:keys [axis y kind]} series]
  (let [a (or (->kw axis)
              (when (= :trend (->kw kind))
                (some (fn [o]
                        (when (and (= y (:y o)) (not= :trend (->kw (:kind o))))
                          (->kw (:axis o))))
                      series)))]
    (if (= :right a) "y2" "y")))

(defn- fit-endpoints
  [ys]
  (let [pts (vec (for [[i y] (map-indexed vector ys) :when (number? y)] [i y]))
        n   (count pts)]
    (when (> n 1)
      (let [sx  (reduce + (map first pts))
            sy  (reduce + (map second pts))
            sxx (reduce + (map #(* (first %) (first %)) pts))
            sxy (reduce + (map #(* (first %) (second %)) pts))
            dn  (- (* n sxx) (* sx sx))]
        (when-not (zero? dn)
          (let [b (/ (- (* n sxy) (* sx sy)) dn)
                a (/ (- sy (* b sx)) n)]
            [a (+ a (* b (dec (count ys))))]))))))

(defn- one-series-trace
  [{:keys [stacked? labels?] :as cfg} rows x kind s color]
  (let [{:keys [name mode fill? type]} s
        k    (or (->kw (:kind s)) kind)
        xk   (:x s)
        xs   (cond (sequential? xk) (vec xk)
                   (some? xk)       (axis-vals rows xk)
                   :else            (axis-vals rows x))
        yk   (:y s)
        ys   (if (sequential? yk) (vec yk) (y-vals rows yk))
        base (get kind->trace k (kind->trace :line))
        ax   (y-axis-of s (:series cfg))]
    (cond-> (colored
             (if (= :trend k)
               (let [[y0 y1] (fit-endpoints ys)]
                 (cond-> {:type "scatter" :mode "lines"
                          :x [(first xs) (last xs)] :y [y0 y1]
                          :line {:dash "dot" :width 2}
                          :hoverinfo "skip"}
                   name (assoc :name name)))
               (-> (cond-> (merge base {:x xs :y ys})
                     name  (assoc :name name)
                     type  (assoc :type type)
                     mode  (assoc :mode mode)
                     fill? (assoc :fill "tozeroy"))
                   (stacked stacked?)
                   (labelled labels?)))
             color)
      (not= "y" ax) (assoc :yaxis ax))))

(defn- layer-traces
  [{:keys [series rows x group chart-kind colors sort? group-label] :as cfg}]
  (let [kind  (or (->kw chart-kind) :line)
        rows  (cond-> (vec rows) (and x (not= false sort?)) (by-x x))
        pal   (if (seq colors) (vec colors) palette)
        color (fn [i] (nth pal (mod i (count pal)) nil))
        label (or group-label identity)]
    (if group
      (let [segs (group-by #(prop % group) rows)
            many (next series)]
        (vec (map-indexed
              (fn [i [s [g grows]]]
                (let [seg (named-segment cfg g (str (label g)))
                      nm  (if many (str (:name s) " · " seg) seg)]
                  (-> (one-series-trace cfg grows x kind (assoc s :name nm)
                                        (or (:color s) (color i)))
                      (segment-trace cfg g))))
              (for [s series, seg segs] [s seg]))))
      (vec (map-indexed (fn [i s] (one-series-trace cfg rows x kind s (or (:color s) (color i))))
                        series)))))

(defn- pie-trace
  [{:keys [rows x y names radius color-of] :as cfg} base pal]
  (let [labels (axis-vals rows (or names x))
        shown  (mapv #(named-segment cfg % (str %)) labels)
        color  (fn [i l] (or (:color (segment-of cfg l))
                             (when color-of (color-of l))
                             (nth pal (mod i (count pal)) nil)))]
    (cond-> (merge base {:labels shown
                         :values (y-vals rows y)
                         :marker {:colors (vec (map-indexed color labels))}
                         :customdata (mapv str labels)
                         :sort false})
      radius (assoc :hole radius))))

(defn- rows->traces
  [{:keys [rows x y group chart-kind colors stacked? sort? prop-label group-label labels?]
    :as cfg}]
  (let [kind     (or (->kw chart-kind) :line)
        prop-label  (or prop-label #(if (keyword? %) (name %) %))
        group-label (or group-label identity)
        base     (get kind->trace kind (kind->trace :line))
        rows     (cond-> (vec rows) (and x (not= false sort?)) (by-x x))
        pal      (if (seq colors) (vec colors) palette)
        color-at (fn [i] (nth pal (mod i (count pal)) nil))]
    (cond
      (= kind :pie)
      [(pie-trace (assoc cfg :rows rows) base pal)]

      group
      (->> (group-by #(prop % group) rows)
           (map-indexed
            (fn [i [g grows]]
              (-> (merge base {:x (axis-vals grows x)
                               :y (y-vals grows y)
                               :name (named-segment cfg g (str (group-label g)))})
                  (stacked stacked?)
                  (labelled labels?)
                  (colored (color-at i))
                  (segment-trace cfg g))))
           vec)

      :else
      (let [ys (if (sequential? y) y [y])]
        (vec
         (map-indexed
          (fn [i yk]
            (-> (merge base {:x (axis-vals rows x)
                             :y (y-vals rows yk)
                             :name (str (prop-label yk))})
                (stacked stacked?)
                (labelled labels?)
                (colored (color-at i))))
          ys))))))

(defn- value-axis
  [base {:keys [title fmt scale range]}]
  (cond-> base
    title                (assoc :title {:text title})
    fmt                  (assoc :tickformat fmt)
    (= :log (->kw scale)) (assoc :type "log")
    (seq range)          (assoc :range (vec range))))

(defn- measures [{:keys [y series]}]
  (distinct (concat (if (sequential? y) y (when y [y]))
                    (keep #(let [k (:y %)] (when-not (sequential? k) k)) series))))

(defn- fold-measure
  [f rs k]
  (agg/fold-values f (cond
                       (and (nil? k) (#{:count :cardinality} f)) (mapv (constantly 1) rs)
                       (#{:count :cardinality} f) (into [] (keep #(prop % k)) rs)
                       :else (into [] (keep #(some-> (num (prop % k)) d/lossy-number)) rs))))

(defn- series-measures
  [{:keys [series]}]
  (mapv (fn [i s] (assoc s ::of (:y s) :y (keyword "chart-measure" (str i))))
        (range) series))

(defn- folded
  [{:keys [rows x x-bucket group series] :as cfg}]
  (let [per-series? (some :agg series)]
    (if-not (or x-bucket (:agg cfg) per-series?)
      cfg
      (let [f     (or (->kw (:agg cfg)) :sum)
            unit  (->kw x-bucket)
            ss    (when per-series? (series-measures cfg))
            count-y (when (and (not per-series?) (empty? (measures cfg))
                               (#{:count :cardinality} f))
                      ::count)
            ys    (if count-y [nil] (measures cfg))
            x-of  (fn [r] (let [xv (prop r x)] (if unit (agg/bucket-label unit xv) xv)))
            cells (fn [rs]
                    (if ss
                      (map (fn [s] [(:y s) (fold-measure (or (->kw (:agg s)) f) rs (::of s))]) ss)
                      (map (fn [k] [(or k count-y) (fold-measure f rs k)]) ys)))
            rows' (vec (for [[[xv gv] rs] (group-by (fn [r] [(x-of r) (when group (prop r group))])
                                                    rows)
                             :when (some? xv)]
                         (into (cond-> {x xv} group (assoc group gv)) (cells rs))))]
        (cond-> (assoc cfg :rows rows')
          ss      (assoc :series ss)
          count-y (assoc :y count-y))))))

(defn- densified
  [{:keys [rows x x-bucket group chart-kind missing] :as cfg}]
  (let [xs   (map #(prop % x) rows)
        unit (when (and x (not= :pie (->kw chart-kind)))
               (or (->kw x-bucket) (agg/label-unit xs)))
        all  (when unit (agg/bucket-range unit xs))]
    (if-not all
      cfg
      (let [zero (when (= :zero (->kw missing)) (zipmap (measures cfg) (repeat 0)))
            have (set (map (fn [r] [(prop r x) (when group (prop r group))]) rows))
            gs   (if group (distinct (map #(prop % group) rows)) [nil])
            fill (for [g gs, xv all :when (not (have [xv g]))]
                   (merge (cond-> {x xv} group (assoc group g)) zero))
            pos  (zipmap all (range))]
        (assoc cfg ::time-axis? true
               :rows (vec (sort-by #(pos (prop % x) -1) (concat rows fill))))))))

(defn series->traces
  [{:keys [series]} {:keys [name colors]}]
  (let [pal (if (seq colors) (vec colors) palette)]
    (vec (map-indexed
          (fn [i {:keys [series-id points objects]}]
            (colored {:type "scatter" :mode "lines"
                      :x (mapv first points)
                      :y (mapv #(d/lossy-number (num (second %))) points)
                      :name (or name (some :label objects) (str series-id))}
                     (nth pal (mod i (count pal)) nil)))
          series))))

(defn- layer-spec
  [i layer]
  (let [cfg    (-> (with-aggregation layer) folded densified)
        traces (cond
                 (:series-data cfg) (series->traces (:series-data cfg) cfg)
                 (:series cfg)      (layer-traces cfg)
                 :else              (rows->traces cfg))
        meta   {:layer i :prop (or (:names layer) (:x layer)) :segment (:group layer)}]
    {:cfg    cfg
     :traces (cond->> (mapv #(assoc % :meta meta) traces)
               (= :connect (->kw (:missing cfg)))
               (mapv #(cond-> % (= "scatter" (:type %)) (assoc :connectgaps true))))}))

(def ^:private layer-keys
  [:title :rows :aggregation :series-data :time-series :chart-kind :labels? :x :y :series :agg
   :group :stacked? :missing :x-bucket :names :segments :bar-mode :selection-filter])

(defn- layers-of
  [{:keys [layers] :as cfg}]
  (if (seq layers)
    (let [common (apply dissoc cfg :layers layer-keys)]
      (mapv #(merge common %) layers))
    [(dissoc cfg :layers)]))

(def ^:private legend-positions
  {:bottom {:orientation "h" :y -0.18 :yanchor "top"}
   :top    {:orientation "h" :y 1.12 :yanchor "bottom"}
   :left   {:orientation "v" :x -0.2 :xanchor "right" :y 1}
   :right  {:orientation "v" :x 1.02 :xanchor "left" :y 1}})

(def ^:private category-orders
  {:x-asc      "category ascending"
   :x-desc     "category descending"
   :value-asc  "total ascending"
   :value-desc "total descending"})

(defn- bar-mode-layout
  [layout {:keys [bar-mode stacked?]}]
  (case (->kw bar-mode)
    :stacked    (assoc layout :barmode "stack")
    :percentage (assoc layout :barmode "stack" :barnorm "percent")
    :grouped    (assoc layout :barmode "group")
    (cond-> layout stacked? (assoc :barmode "stack"))))

(defn- horizontal
  [{:keys [traces layout] :as spec} orientation]
  (if (and (not= :vertical (->kw orientation)) (seq traces)
           (every? #(= "bar" (:type %)) traces))
    {:traces (mapv (fn [t] (assoc t :x (:y t) :y (:x t) :orientation "h")) traces)
     :layout (assoc layout :xaxis (:yaxis layout) :yaxis (:xaxis layout))}
    spec))

(defn- pie-hidden
  [{:keys [segments chart-kind] :as cfg}]
  (when-let [hidden (and (= :pie (->kw chart-kind)) (seq (filter :hidden? segments)))]
    {:hiddenlabels (mapv #(named-segment cfg (:value %) (str (:value %))) hidden)}))

(defn chart->spec
  [cfg]
  (let [specs  (vec (map-indexed layer-spec (layers-of cfg)))
        cfg    (:cfg (first specs))
        {:keys [layout legend? x-title y-title y-fmt x-fmt axes sort-by-x
                legend-position orientation]} cfg
        multi? (boolean (or (:series cfg) (:group cfg) (sequential? (:y cfg)) (next specs)))
        axis   {:gridcolor "rgba(128,128,128,0.18)" :zerolinecolor "rgba(128,128,128,0.35)"
                :linecolor "rgba(128,128,128,0.35)" :automargin true
                :tickfont {:size 11}}
        traces (vec (mapcat :traces specs))
        right? (boolean (some #(= "y2" (:yaxis %)) traces))
        left   (merge (cond-> {} y-title (assoc :title y-title) y-fmt (assoc :fmt y-fmt))
                      (:left axes))
        order  (get category-orders (->kw sort-by-x))]
    (horizontal
     {:traces traces
      :layout (merge
               (cond-> (bar-mode-layout
                        {:margin  {:l 56 :r (if right? 56 16) :t 16 :b 40}
                         :showlegend (boolean (if (nil? legend?) multi? legend?))
                         :paper_bgcolor "transparent"
                         :plot_bgcolor  "transparent"
                         :separators    ",."
                         :font    {:size 11}
                         :hovermode (if multi? "x unified" "closest")
                         :hoverlabel {:namelength -1}
                         :xaxis (cond-> axis
                                  x-title  (assoc :title {:text x-title})
                                  x-fmt    (assoc :tickformat x-fmt)
                                  order    (assoc :categoryorder order)
                                  (or (:x-bucket cfg) (::time-axis? cfg))
                                  (assoc :type "category"))
                         :yaxis (value-axis axis left)
                         :legend (get legend-positions (->kw legend-position)
                                      (:bottom legend-positions))}
                        cfg)
                 right? (assoc :yaxis2 (merge (value-axis axis (:right axes))
                                              {:overlaying "y" :side "right"
                                               :showgrid false :zeroline false})))
               (pie-hidden cfg)
               layout)}
     orientation)))

(defn selection
  [{:keys [prop segment layer]} {:keys [x label custom]} pie?]
  (when prop
    {:prop prop :segment segment :layer layer :value (if pie? (or custom label) x)}))
