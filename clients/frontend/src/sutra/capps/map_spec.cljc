(ns sutra.capps.map-spec
  (:require [clojure.string :as str]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.capps.chart-spec :as cs :refer [prop axis-vals y-vals]]
            [sutra.capps.format :refer [num]]
            [sutra.types.decimal :as d]
            [sutra.capps.polygon :as poly]
            [sutra.capps.map-geo :as geo]
            [sutra.capps.plan-faces :as pf]
            [sutra.capps.table :as tbl]
            [sutra.capps.snap :refer [angle-at angle-gap along edge-angle snap-directions
                                      snap-point snap-tolerance]]))

(def base-layers
  [:carto-positron :carto-darkmatter :carto-voyager :open-street-map :white-bg :basic])

(defn- blank-style
  [color]
  {:version 8 :sources {}
   :layers [{:id "background" :type "background" :paint {:background-color color}}]})

(def ^:private osm-style
  {:version 8
   :sources {:osm {:type "raster" :tileSize 256
                   :tiles ["https://tile.openstreetmap.org/{z}/{x}/{y}.png"]
                   :attribution "© OpenStreetMap contributors"}}
   :layers [{:id "osm" :type "raster" :source "osm"}]})

(defn style-of
  [style]
  (let [k (if (string? style) (->kw style) style)]
    (case k
      :carto-positron   "https://basemaps.cartocdn.com/gl/positron-gl-style/style.json"
      :carto-darkmatter "https://basemaps.cartocdn.com/gl/dark-matter-gl-style/style.json"
      :carto-voyager    "https://basemaps.cartocdn.com/gl/voyager-gl-style/style.json"
      :open-street-map  osm-style
      (:white-bg :basic) (blank-style "#ffffff")
      :plan             (blank-style "#eef1f4")
      (if (and (string? style) (str/includes? style "://"))
        style
        "https://basemaps.cartocdn.com/gl/positron-gl-style/style.json"))))

(defn crs-of [{:keys [coordinate-system]}] (if (= :plan (->kw coordinate-system)) :plan :geo))

(defn- js-date
  [x]
  #?(:cljs (js/Date. x)
     :clj  (if (number? x)
             (java.util.Date. (long x))
             (java.util.Date/from (java.time.Instant/parse (str x))))))

(defn- ms [x] (when (some? x) (if (number? x) x (.getTime (js-date x)))))

(defn- within-window?
  [row prop-k {:keys [cursor start end]}]
  (let [t (some-> (prop row prop-k) ms)]
    (or (nil? t)
        (and (or (nil? start) (>= t start))
             (or (nil? end) (<= t end))
             (or (nil? cursor) (<= t cursor))))))

(defn in-time
  [{:keys [rows time-prop]} clock]
  (if (and time-prop clock)
    (filterv #(within-window? % time-prop clock) rows)
    (vec rows)))

(defn layers-of
  [{:keys [layers map-kind rows lat lon lng label color size group z radius stack-key]
    ::keys [dy] :as cfg}]
  (cond->> (if (seq layers)
             (mapv (fn [i l] (assoc l ::i i)) (range) layers)
             [{::i 0
               :rows rows
               :selected (:selected cfg)
               :geometries [(cond-> {:geometry (if (= :heat (->kw map-kind)) :heat :point)
                                     :lat lat :lon (or lon lng)
                                     :label label :color color :size size :group group}
                              z (assoc :z z)
                              radius (assoc :radius radius))]}])
    dy (mapv #(assoc % ::dy dy ::stack-key stack-key))))

(defn- number-of [x] (let [n (num x)] (when (some? n) (d/lossy-number n))))

(defn- stack-plan
  [{:keys [stack-key stack-order stack-label stack-w stack-h]} row]
  (let [at (fn [k] (when k (prop row k)))
        w  (number-of (at stack-w))
        h  (number-of (at stack-h))]
    (when (and w h (pos? w) (pos? h))
      {:id (str (at stack-key)) :label (some-> (at stack-label) str)
       :order (number-of (at stack-order)) :w w :h h})))

(defn stacked
  [{:keys [stack stack-key coordinate-system] :as cfg}]
  (if-let [{:keys [frames extent]}
           (when (and stack-key (= :plan (->kw coordinate-system)))
             (poly/stack-layout (keep #(stack-plan cfg %) stack)))]
    (cond-> (assoc cfg :extent extent :src nil :import nil :draw-tools []
                   ::frames frames ::dy (into {} (map (juxt :id :y)) frames))
      (seq (:layers cfg)) (update :layers (fn [ls] (mapv #(dissoc % :edit-action :actions) ls))))
    cfg))

(defn- row-dy
  [{::keys [dy stack-key]} row]
  (if dy (get dy (str (prop row stack-key))) 0))

(defn- look-color
  [{rules :conditional-format} row]
  (when (seq rules)
    (let [look (tbl/cond-format rules (or (:values row) row))]
      (or (:background look) (:color look)))))

(defn- looked [layer rows colors] (mapv #(or (look-color layer %1) %2) rows colors))

(defn point-of
  [{:keys [lat lon geopoint xy]} row]
  (cond
    xy       (geo/parse-xy (prop row xy))
    geopoint (geo/parse-point (prop row geopoint))
    :else    (let [la (number-of (prop row lat)) lo (number-of (prop row lon))]
               (when (and (some? la) (some? lo)) [lo la]))))

(defn- points-of
  [geometry rows]
  (into [] (keep (fn [r] (when-let [p (point-of geometry r)] {:at p :row r}))) rows))

(defn- ring->polygon
  [points]
  {:type "Polygon" :coordinates [(geo/open-ring points)]})

(defn- close-polygon
  [{:keys [coordinates] :as g}]
  (let [r (vec (first coordinates))]
    (assoc g :coordinates (into [(cond-> r (not= (first r) (peek r)) (conj (first r)))]
                                (rest coordinates)))))

(defn row-shapes
  [crs v]
  (if (= :plan crs)
    (let [rings (filter poly/ring? (poly/parse-rings v))]
      (if (seq rings)
        (mapv #(close-polygon (ring->polygon (:points %))) rings)
        (geo/geometries v)))
    (geo/geometries v)))

(def ^:private ylorrd ["#ffffb2" "#fecc5c" "#fd8d3c" "#f03b20" "#bd0026"])

(defn- hex-int [s] #?(:clj (Integer/parseInt s 16) :cljs (js/parseInt s 16)))

(defn- rgb [h] (mapv #(hex-int (subs h % (+ % 2))) [1 3 5]))

(defn ramp
  [t]
  (let [t (max 0.0 (min 1.0 (double t)))
        n (dec (count ylorrd))
        x (* t n)
        i (min (dec n) (int (Math/floor x)))
        f (- x i)
        a (rgb (nth ylorrd i))
        b (rgb (nth ylorrd (inc i)))]
    (str "rgb(" (str/join "," (map (fn [u w] (Math/round (double (+ u (* f (- w u)))))) a b))
         ")")))

(defn colors-of
  [rows color fallback]
  (cond
    (keyword? color)
    (let [vs (mapv #(prop % color) rows)
          ns (mapv number-of vs)
          xs (remove nil? ns)]
      (if (and (seq xs) (= (count xs) (count (remove nil? vs))))
        (let [lo (apply min xs) hi (apply max xs) span (max 1e-12 (- hi lo))]
          (mapv #(if (some? %) (ramp (/ (- % lo) span)) fallback) ns))
        (let [cats (vec (distinct vs))
              at   (zipmap cats (range))]
          (mapv #(nth cs/palette (mod (get at % 0) (count cs/palette))) vs))))
    (string? color) (vec (repeat (count rows) color))
    :else           (vec (repeat (count rows) fallback))))

(defn- radii-of
  [rows size]
  (cond
    (keyword? size) (mapv #(max 2 (min 30 (/ (or % 10) 2))) (y-vals rows size))
    (number? size)  (vec (repeat (count rows) (/ size 2)))
    :else           (vec (repeat (count rows) 5))))

(defn- labels-of
  [rows label]
  (if label (mapv str (axis-vals rows label)) (vec (repeat (count rows) nil))))

(defn- fc [features] {:type "FeatureCollection" :features (vec features)})

(defn- feature
  ([geometry props] {:type "Feature" :geometry geometry :properties props}))

(defn- project [crs g] (if (= :plan crs) (geo/map-coords geo/plan->lnglat g) g))

(defn- project-pt [crs p] (if (= :plan crs) (geo/plan->lnglat p) p))

(defn row-key
  [row i]
  (or (:id row) (get-in row [:values :id]) i))

(defn- layer-color [layer g] (or (:color layer) (first cs/palette) (:line-color g)))

(defn- point-part
  [crs id {:keys [label color size show-labels? opacity] :as g} layer rows selected editing]
  (let [idx (into {} (map-indexed (fn [i r] [r i])) (:rows layer))
        pts (->> (points-of g rows)
                 (remove #(contains? editing [(::i layer) (row-key (:row %) (get idx (:row %)))]))
                 (map (fn [{[x y] :at :keys [row] :as p}]
                        (assoc p :at [x (+ y (row-dy layer row))]))))
        rs  (mapv :row pts)
        cl  (looked layer rs (colors-of rs color (layer-color layer g)))
        rd  (radii-of rs size)
        lb  (labels-of rs label)]
    {:sources {id {:type "geojson"
                   :data (fc (map-indexed
                              (fn [i {:keys [at row]}]
                                (feature {:type "Point" :coordinates (project-pt crs at)}
                                         {:row (get idx row -1) :color (nth cl i)
                                          :r (nth rd i) :tip (nth lb i)
                                          :sel (contains? selected row)}))
                              pts))}}
     :layers [{:id id :type "circle" :source id :metadata {:hit (::i layer)}
               :paint {:circle-color ["get" "color"] :circle-radius ["get" "r"]
                       :circle-opacity (or opacity (:opacity layer) 1)
                       :circle-stroke-color ["case" ["==" ["get" "sel"] true] "#d64541" "#ffffff"]
                       :circle-stroke-width ["case" ["==" ["get" "sel"] true] 3 1]}}]
     :labels (when show-labels?
               (keep-indexed (fn [i {:keys [at]}]
                               (when-let [t (nth lb i)]
                                 {:at (project-pt crs at) :text t :kind :point}))
                             pts))}))

(defn- heat-part
  [crs id {:keys [z radius] :as g} layer rows]
  (let [pts (points-of g rows)
        ws  (if z (y-vals (mapv :row pts) z) (repeat 1))
        hi  (max 1e-12 (reduce max 1e-12 (map #(or % 0) (take (count pts) ws))))]
    {:sources {id {:type "geojson"
                   :data (fc (map (fn [{:keys [at]} w]
                                    (feature {:type "Point" :coordinates (project-pt crs at)}
                                             {:w (/ (or w 0) hi)}))
                                  pts ws))}}
     :layers [{:id id :type "heatmap" :source id
               :paint {:heatmap-radius (or radius 20) :heatmap-weight ["get" "w"]
                       :heatmap-opacity (or (:opacity layer) 0.8)}}]}))

(defn- cluster-part
  [crs id g layer rows zoom]
  (let [cl (geo/clusters (points-of g rows) zoom (or (:cluster-px g) 40))]
    {:sources {id {:type "geojson"
                   :data (fc (map (fn [{:keys [at count]}]
                                    (feature {:type "Point" :coordinates (project-pt crs at)}
                                             {:r (+ 6 (* 3 (Math/log count)))}))
                                  cl))}}
     :layers [{:id id :type "circle" :source id
               :paint {:circle-color (layer-color layer g) :circle-radius ["get" "r"]
                       :circle-opacity (or (:opacity layer) 0.8)
                       :circle-stroke-color "#ffffff" :circle-stroke-width 1}}]
     :labels (map (fn [{:keys [at count]}] {:at (project-pt crs at) :text (str count)
                                            :kind :cluster})
                  cl)}))

(defn- line-paint
  [g layer]
  {:line-color (or (:line-color g) (layer-color layer g)) :line-width (or (:line-width g) 2)
   :line-opacity (or (:opacity g) (:opacity layer) 1)})

(defn- connection-part
  [crs id {:keys [connect-by order-by] :as g} layer rows]
  {:sources {id {:type "geojson"
                 :data (fc (keep (fn [[_ rs]]
                                   (let [rs  (if order-by (sort-by #(str (prop % order-by)) rs) rs)
                                         pts (points-of g rs)]
                                     (when (next pts)
                                       (feature {:type "LineString"
                                                 :coordinates (mapv #(project-pt crs (:at %)) pts)}
                                                {}))))
                                 (if connect-by
                                   (group-by #(prop % connect-by) rows)
                                   [[nil rows]])))}}
   :layers [{:id id :type "line" :source id :paint (line-paint g layer)}]})

(defn- segment-part
  [crs id {:keys [lat lon end-lat end-lon] :as g} layer rows]
  {:sources {id {:type "geojson"
                 :data (fc (keep (fn [r]
                                   (let [a (point-of {:lat lat :lon lon} r)
                                         b (point-of {:lat end-lat :lon end-lon} r)]
                                     (when (and a b)
                                       (feature {:type "LineString"
                                                 :coordinates [(project-pt crs a)
                                                               (project-pt crs b)]}
                                                {}))))
                                 rows))}}
   :layers [{:id id :type "line" :source id :paint (line-paint g layer)}]})

(defn- shape-part
  [crs id {:keys [shape label color show-labels? opacity fill?] :as g} layer rows selected
   editing]
  (let [idx  (into {} (map-indexed (fn [i r] [r i])) (:rows layer))
        cl   (looked layer rows (colors-of rows color (layer-color layer g)))
        lb   (labels-of rows label)
        li   (::i layer)
        fill (if (false? fill?) 0 (or opacity (:opacity layer) 0.45))
        hits (vec (mapcat (fn [i r]
                            (when-not (contains? editing [li (row-key r (get idx r))])
                              (let [dy (row-dy layer r)]
                                (map (fn [s] {:shape (cond-> s (pos? dy) (geo/translate [0 dy]))
                                              :i i :row r})
                                     (row-shapes crs (prop r shape))))))
                          (range) rows))]
    {:sources {id {:type "geojson"
                   :data (fc (map (fn [{:keys [shape i row]}]
                                    (feature (project crs shape)
                                             {:row (get idx row -1) :color (nth cl i)
                                              :tip (nth lb i) :sel (contains? selected row)}))
                                  hits))}}
     :layers [{:id id :type "fill" :source id :metadata {:hit li}
               :filter ["==" ["geometry-type"] "Polygon"]
               :paint {:fill-color ["get" "color"] :fill-opacity fill}}
              {:id (str id "-line") :type "line" :source id :metadata {:hit li}
               :paint {:line-color ["case" ["==" ["get" "sel"] true] "#d64541"
                                    (or (:line-color g) ["get" "color"])]
                       :line-width ["case" ["==" ["get" "sel"] true] 3 (or (:line-width g) 1.5)]}}]
     :labels (when show-labels?
               (keep (fn [{:keys [shape i]}]
                       (when-let [t (nth lb i)]
                         (when-let [c (geo/centroid (geo/open-ring (first (:coordinates shape))))]
                           {:at (project-pt crs c) :text t :kind :shape})))
                     (filter #(= "Polygon" (:type (:shape %))) hits)))}))

(defn- geometry-part
  [crs id g layer rows {:keys [zoom selected editing]}]
  (case (->kw (:geometry g))
    :heat                (heat-part crs id g layer rows)
    :cluster             (cluster-part crs id g layer rows zoom)
    :connect             (connection-part crs id g layer rows)
    :segment             (segment-part crs id g layer rows)
    (:shape :choropleth) (shape-part crs id g layer rows selected editing)
    (point-part crs id g layer rows selected editing)))

(defn- merge-parts
  [parts]
  {:sources (apply merge (map :sources parts))
   :layers  (vec (mapcat :layers parts))
   :labels  (vec (mapcat :labels parts))})

(defn layer-parts
  [crs layer {:keys [hidden view clock selections editing]}]
  (when-not (or (false? (:visible? layer)) (contains? hidden (::i layer)))
    (let [rows (cond->> (in-time layer clock) (::dy layer) (filterv #(row-dy layer %)))
          sel  (set (get selections (::i layer)))]
      (merge-parts
       (keep-indexed (fn [j g]
                       (when-not (:hidden? g)
                         (geometry-part crs (str "sutra-l" (::i layer) "-g" j) g layer rows
                                        {:zoom (:zoom view) :selected sel
                                         :editing (set (keys editing))})))
                     (or (:geometries layer) [{}]))))))

(defn measure-labels
  ([shape opts] (measure-labels :geo nil shape opts))
  ([crs unit {:keys [type coordinates]} {:keys [perimeter? area? lines? segments?]}]
   (let [len (if (= :plan crs) #(geo/format-plan-length % unit) geo/format-length)
         ar  (if (= :plan crs) #(geo/format-plan-area % unit) geo/format-area)]
     (case type
       "Polygon"
       (let [ring (first coordinates)]
         (cond-> []
           perimeter? (conj (str "Umfang " (len (geo/perimeter crs ring))))
           (and perimeter? segments?)
           (into (map len (geo/segment-lengths crs ring)))
           area?      (conj (str "Fläche " (ar (geo/area crs ring))))))
       "LineString"
       (when lines?
         (if segments?
           (mapv len (geo/segment-lengths crs coordinates))
           [(str "Länge " (len (geo/line-length crs coordinates)))]))
       nil))))

(defn- all-coords
  [{:keys [type coordinates]}]
  (case type
    "Point"      [coordinates]
    "LineString" coordinates
    "Polygon"    (geo/open-ring (first coordinates))
    "MultiPolygon" (mapcat #(geo/open-ring (first %)) coordinates)
    []))

(defn- center-of [coords] (when-let [[w s e n] (geo/bbox coords)] [(/ (+ w e) 2) (/ (+ s n) 2)]))

(defn- measure-labels-of
  [crs unit shapes measurements]
  (keep (fn [s]
          (when-let [t (seq (measure-labels crs unit s measurements))]
            (when-let [c (center-of (all-coords s))]
              {:at (project-pt crs c) :text (str/join " · " t) :kind :measure})))
        shapes))

(defn- span-zoom
  [xs]
  (if (< (count xs) 2)
    9
    (let [d (- (apply max xs) (apply min xs))]
      (cond (<= d 0.05) 12 (< d 0.5) 9 (< d 2) 7 (< d 10) 5 (< d 40) 3 :else 1))))

(defn points-of-layers
  [layers clock]
  (into [] (mapcat (fn [l]
                     (mapcat #(map :at (points-of % (in-time l clock)))
                             (remove #(#{:shape :choropleth} (->kw (:geometry %)))
                                     (:geometries l)))))
        layers))

(defn- fit
  [coords]
  (when (seq coords)
    (let [n (count coords)]
      {:center {:lon (/ (reduce + (map first coords)) n)
                :lat (/ (reduce + (map second coords)) n)}
       :zoom   (span-zoom (concat (map first coords) (map second coords)))})))

(defn target-coords
  [{:keys [zoom-to follow zoom-geometry]} layers clock]
  (let [from-set (fn [rows]
                   (let [g (or zoom-geometry (some #(first (:geometries %)) layers))]
                     (mapv :at (points-of g rows))))]
    (cond
      (seq follow)  (from-set follow)
      (seq zoom-to) (from-set zoom-to)
      :else         (points-of-layers layers clock))))

(defn- geometry-coords
  [crs g rows]
  (if (#{:shape :choropleth} (->kw (:geometry g)))
    (into [] (comp (mapcat #(row-shapes crs (prop % (:shape g)))) (mapcat all-coords)) rows)
    (mapv :at (points-of g rows))))

(defn target-bounds
  [crs {:keys [zoom-to follow zoom-geometry]} layers clock]
  (let [g      (or zoom-geometry (some #(first (:geometries %)) layers))
        coords (cond
                 (seq follow)  (geometry-coords crs g follow)
                 (seq zoom-to) (geometry-coords crs g zoom-to)
                 :else (mapcat (fn [l] (mapcat #(geometry-coords crs % (in-time l clock))
                                               (:geometries l)))
                               layers))]
    (when-let [[w s e n] (geo/bbox (map #(project-pt crs %) coords))]
      [[w s] [e n]])))

(defn auto-view
  [{:keys [zoom-outside-only?]} targets bounds]
  (when (seq targets)
    (when-not (and zoom-outside-only? bounds (every? #(geo/inside-bbox? bounds %) targets))
      (fit targets))))

(defn grid-step
  [m]
  (let [raw (/ m 10)
        p   (Math/pow 10 (Math/floor (/ (Math/log raw) (Math/log 10))))
        f   (/ raw p)]
    (* p (cond (< f 2) 1 (< f 5) 2 :else 5))))

(defn plan-bounds
  [{:keys [w h]}]
  (when (and w h)
    (let [[w0 n0] (geo/plan->lnglat [0 0]) [e0 s0] (geo/plan->lnglat [w h])]
      [[w0 s0] [e0 n0]])))

(defn- sheet-features
  [{:keys [w h y] :or {y 0}}]
  (let [c     (fn [[px py]] (geo/plan->lnglat [px (+ y py)]))
        step  (grid-step (max w h))
        lines (concat (for [x (range step w step)] [[x 0] [x h]])
                      (for [v (range step h step)] [[0 v] [w v]]))]
    {:frame (feature {:type "Polygon" :coordinates [(mapv c [[0 0] [w 0] [w h] [0 h] [0 0]])]}
                     {})
     :grid  (map (fn [[a b]] (feature {:type "LineString" :coordinates [(c a) (c b)]} {}))
                 lines)}))

(defn- plan-part
  [{:keys [w h src frames]}]
  (let [corner geo/plan->lnglat
        sheets (map sheet-features (or (seq frames) [{:w w :h h}]))
        gap    (:y (first frames))]
    {:labels  (for [{:keys [label y]} frames :when label]
                {:at (corner [0 (- y (* 0.5 gap))]) :text label :kind :plan :anchor "left"})
     :sources (cond-> {"sutra-plan" {:type "geojson" :data (fc (map :frame sheets))}
                       "sutra-plan-grid" {:type "geojson" :data (fc (mapcat :grid sheets))}}
                src (assoc "sutra-plan-image"
                           {:type "image" :url src
                            :coordinates [(corner [0 0]) (corner [w 0]) (corner [w h])
                                          (corner [0 h])]}))
     :layers (cond-> [{:id "sutra-plan" :type "fill" :source "sutra-plan"
                       :paint {:fill-color "#ffffff"}}]
               src       (conj {:id "sutra-plan-image" :type "raster" :source "sutra-plan-image"})
               (not src) (conj {:id "sutra-plan-grid" :type "line" :source "sutra-plan-grid"
                                :paint {:line-color "#cbd5e1" :line-width 0.75
                                        :line-opacity 0.55}})
               true      (conj {:id "sutra-plan-edge" :type "line" :source "sutra-plan"
                                :paint {:line-color "#94a3b8" :line-width 1.5}}))}))

(def ^:private accent "#2b7de9")
(def ^:private grip "#f29d38")

(defn- vertex-paths
  [{:keys [type coordinates]}]
  (case type
    "Polygon"    (map-indexed (fn [i c] [[0 i] c]) (geo/open-ring (first coordinates)))
    "LineString" (map-indexed (fn [i c] [[i] c]) coordinates)
    "Point"      [[[] coordinates]]
    []))

(defn- midpoints
  [{:keys [type coordinates]}]
  (let [mid (fn [[ax ay] [bx by]] [(/ (+ ax bx) 2) (/ (+ ay by) 2)])]
    (case type
      "Polygon"    (let [r (geo/open-ring (first coordinates)) n (count r)]
                     (map (fn [i] [(inc i) (mid (nth r i) (nth r (mod (inc i) n)))]) (range n)))
      "LineString" (map (fn [i] [(inc i) (mid (nth coordinates i) (nth coordinates (inc i)))])
                        (range (dec (count coordinates))))
      [])))

(defn targets
  [{:keys [shapes selected-shapes editing mode]}]
  (concat (when (= :modify mode)
            (map (fn [i] [:shape i])
                 (if (seq selected-shapes)
                   (sort selected-shapes)
                   (when (= 1 (count shapes)) [0]))))
          (map (fn [k] (into [:object] k)) (sort-by str (keys editing)))))

(defn target-shape
  [{:keys [shapes editing]} [kind a b]]
  (if (= :shape kind) (get shapes a) (get editing [a b])))

(defn- set-target
  [st [kind a b] g]
  (if (= :shape kind) (assoc-in st [:shapes a] g) (assoc-in st [:editing [a b]] g)))

(defn- edit-part
  [crs {:keys [editing originals] :as st} unit measurements]
  (let [changed (set (keep (fn [[k g]] (when (not= g (get originals k)) k)) editing))
        handles (when (or (= :modify (:mode st)) (seq editing))
                  (for [t (targets st) :let [g (target-shape st t)] :when g]
                    [t g]))
        boxes   (keep (fn [[_ g]]
                        (when-let [[w s e n] (geo/bbox (all-coords g))]
                          (feature {:type "LineString"
                                    :coordinates (mapv #(project-pt crs %)
                                                       [[w s] [e s] [e n] [w n] [w s]])}
                                   {})))
                      handles)
        active  (:active-vertex st)]
    {:sources {"sutra-edit" {:type "geojson"
                             :data (fc (map (fn [[k g]] (feature (project crs g)
                                                                 {:changed (contains? changed k)}))
                                            editing))}
               "sutra-bbox" {:type "geojson" :data (fc boxes)}
               "sutra-handles"
               {:type "geojson"
                :data (fc (concat
                           (for [[t g] handles [path c] (vertex-paths g)]
                             (feature {:type "Point" :coordinates (project-pt crs c)}
                                      {:kind "v" :on (= active {:target t :path path})}))
                           (when (= :points (:modify st))
                             (for [[_ g] handles [_ c] (midpoints g)]
                               (feature {:type "Point" :coordinates (project-pt crs c)}
                                        {:kind "m"})))))}}
     :layers [{:id "sutra-edit" :type "fill" :source "sutra-edit"
               :filter ["==" ["geometry-type"] "Polygon"]
               :paint {:fill-color ["case" ["==" ["get" "changed"] true] "#d97706" accent]
                       :fill-opacity 0.22}}
              {:id "sutra-edit-line" :type "line" :source "sutra-edit"
               :paint {:line-color accent :line-width 3}}
              {:id "sutra-bbox" :type "line" :source "sutra-bbox"
               :paint {:line-color grip :line-width 1.5 :line-dasharray [4 3]}}
              {:id "sutra-handles" :type "circle" :source "sutra-handles"
               :paint {:circle-radius ["case" ["==" ["get" "kind"] "m"] 4 6]
                       :circle-color ["case" ["==" ["get" "on"] true] grip "#ffffff"]
                       :circle-opacity ["case" ["==" ["get" "kind"] "m"] 0.75 1]
                       :circle-stroke-color grip :circle-stroke-width 2}}]
     :labels (when measurements
               (measure-labels-of crs unit (vals editing) measurements))}))

(defn- drawn-part
  [crs {:keys [shapes selected-shapes]} {:keys [draw-color draw-opacity]}]
  {:sources {"sutra-drawn"
             {:type "geojson"
              :data (fc (map-indexed (fn [i s] (feature (project crs s)
                                                        {:i i :sel (contains? selected-shapes i)}))
                                     shapes))}}
   :layers [{:id "sutra-drawn" :type "fill" :source "sutra-drawn"
             :filter ["==" ["geometry-type"] "Polygon"]
             :paint {:fill-color (or draw-color accent) :fill-opacity (or draw-opacity 0.25)}}
            {:id "sutra-drawn-line" :type "line" :source "sutra-drawn"
             :paint {:line-color ["case" ["==" ["get" "sel"] true] grip (or draw-color accent)]
                     :line-width 3}}
            {:id "sutra-drawn-point" :type "circle" :source "sutra-drawn"
             :filter ["==" ["geometry-type"] "Point"]
             :paint {:circle-radius 6 :circle-color (or draw-color accent)
                     :circle-stroke-color ["case" ["==" ["get" "sel"] true] grip "#ffffff"]
                     :circle-stroke-width 2}}]})

(defn draft-edges
  [{:keys [draft hover tool]}]
  (let [pts (cond-> (vec draft) (and hover (seq draft) (#{:line :polygon :wall} tool))
                    (conj hover))]
    (for [[a b] (map vector pts (rest pts)) :when (not= a b)]
      {:mid [(/ (+ (first a) (first b)) 2) (/ (+ (second a) (second b)) 2)]
       :len (Math/hypot (- (first b) (first a)) (- (second b) (second a)))})))

(defn- draft-part
  [crs unit {:keys [draft hover tool] :as state}]
  (let [pts  (mapv #(project-pt crs %) draft)
        path (cond-> pts (and hover (seq pts) (#{:line :polygon :wall} tool))
                     (conj (project-pt crs hover)))]
    {:sources {"sutra-draft"
               {:type "geojson"
                :data (fc (concat
                           (when (next path)
                             [(feature {:type "LineString" :coordinates path} {})])
                           (map-indexed (fn [i p] (feature {:type "Point" :coordinates p}
                                                           {:first (zero? i)}))
                                        pts)))}}
     :labels (when (= :plan crs)
               (for [{:keys [mid len]} (draft-edges state)]
                 {:at (project-pt crs mid) :text (geo/format-plan-length len unit) :kind :edge}))
     :layers [{:id "sutra-draft-line" :type "line" :source "sutra-draft"
               :paint {:line-color accent :line-width 2 :line-dasharray [2 2]}}
              {:id "sutra-draft-point" :type "circle" :source "sutra-draft"
               :filter ["==" ["geometry-type"] "Point"]
               :paint {:circle-radius 5
                       :circle-color ["case" ["==" ["get" "first"] true] accent "#ffffff"]
                       :circle-stroke-color accent :circle-stroke-width 2}}]}))

(defn- overlay-part
  [crs overlays]
  (merge-parts
   (map-indexed
    (fn [i {:keys [geojson color opacity]}]
      (let [id (str "sutra-overlay-" i)]
        {:sources {id {:type "geojson"
                       :data (fc (map #(feature (project crs %) {}) (geo/geometries geojson)))}}
         :layers [{:id id :type "fill" :source id :filter ["==" ["geometry-type"] "Polygon"]
                   :paint {:fill-color (or color accent) :fill-opacity (or opacity 0.4)}}
                  {:id (str id "-line") :type "line" :source id
                   :paint {:line-color (or color accent) :line-width 2}}]}))
    overlays)))

(defn- picked-part
  [crs picked]
  (when picked
    {:sources {"sutra-picked"
               {:type "geojson"
                :data (fc [(feature {:type "Point"
                                     :coordinates (project-pt crs [(:lon picked) (:lat picked)])}
                                    {})])}}
     :layers [{:id "sutra-picked" :type "circle" :source "sutra-picked"
               :paint {:circle-radius 8 :circle-color "#d64541" :circle-stroke-color "#ffffff"
                       :circle-stroke-width 2}}]}))

(defn- camera
  [crs {:keys [center zoom plan]} {:keys [view]} layers clock]
  (cond
    (:zoom view) {:center [(get-in view [:center :lon]) (get-in view [:center :lat])]
                  :zoom (:zoom view)}
    (= :plan crs) (when plan {:bounds (plan-bounds plan)})
    :else (let [coords (points-of-layers layers clock)
                place  (or center (:center (fit coords)) {:lat 0 :lon 0})]
            {:center [(:lon place) (:lat place)]
             :zoom   (or zoom (:zoom (fit coords)) 1)})))

(defn- rings-of
  [{:keys [type coordinates]}]
  (case type
    "Polygon"      [(first coordinates)]
    "MultiPolygon" (map first coordinates)
    "LineString"   [coordinates]
    []))

(defn- role-layer [cfg role] (first (filter #(= role (->kw (:role %))) (layers-of cfg))))

(defn- layer-rings
  [layer]
  (for [g      (:geometries layer)
        :when  (= :shape (->kw (:geometry g)))
        [i r]  (map-indexed vector (:rows layer))
        s      (row-shapes :plan (prop r (:shape g)))
        ring   (rings-of s)]
    [[(::i layer) (row-key r i)] ring]))

(defn plan-areas
  [cfg shapes]
  (when-let [outline (some-> (role-layer cfg :outline) layer-rings first second)]
    (let [rooms (into {} (some-> (role-layer cfg :rooms) layer-rings))
          walls (for [s shapes :when (= "LineString" (:type s))
                      e (map vector (:coordinates s) (rest (:coordinates s)))]
                  e)]
      (pf/faces outline rooms walls))))

(defn room-cuts
  [areas]
  (for [[kind rk] (:problems areas) :when (= :cut kind)
        :let [parts (sort-by #(- (Math/abs (double (:area %))))
                             (filter #(= rk (:room %)) (:faces areas)))]]
    {:room rk :ring (:ring (first parts)) :parts (count parts)}))

(defn free-area-at
  [areas p]
  (some (fn [{:keys [ring room]}] (when (and (nil? room) (pf/point-in-ring? p ring)) ring))
        (:faces areas)))

(defn- areas-part
  [crs unit areas]
  (let [free (filter (comp nil? :room) (:faces areas))]
    {:sources {"sutra-areas"
               {:type "geojson"
                :data (fc (map #(feature (project crs {:type "Polygon" :coordinates [(:ring %)]})
                                         {})
                               free))}}
     :layers [{:id "sutra-areas-fill" :type "fill" :source "sutra-areas"
               :paint {:fill-color "#d9822b" :fill-opacity 0.12}}
              {:id "sutra-areas-line" :type "line" :source "sutra-areas"
               :paint {:line-color "#d9822b" :line-width 1.5 :line-dasharray [3 2]}}]
     :labels (for [{:keys [ring area]} free :let [c (center-of (geo/open-ring ring))] :when c]
               {:at (project-pt crs c) :text (str "frei · " (geo/format-plan-area area unit))
                :kind :area})}))

(defn map->scene
  [{:keys [style picked overlays measurements unit plan] :as cfg}
   {:keys [base shapes] :as state}]
  (let [crs    (crs-of cfg)
        layers (layers-of cfg)
        clock  (:clock state)
        parts  (concat
                (when (and (= :plan crs) plan) [(plan-part (assoc plan :frames (::frames cfg)))])
                [(overlay-part crs overlays)]
                (keep #(layer-parts crs % state) layers)
                (when-let [a (and (= :plan crs) (plan-areas cfg shapes))]
                  [(areas-part crs unit a)])
                [(drawn-part crs state cfg)
                 (edit-part crs state unit measurements)
                 (draft-part crs unit state)]
                (when picked [(picked-part crs picked)]))
        all    (merge-parts parts)]
    (assoc all
           :labels (into (:labels all)
                         (when measurements (measure-labels-of crs unit shapes measurements)))
           :style  (if (= :plan crs) (style-of :plan) (style-of (or base style)))
           :camera (camera crs cfg state layers clock))))

(defn shape-text
  [shape kind]
  (cond
    (not= :points kind)          (geo/write-json shape)
    (= "Polygon" (:type shape))  (poly/->str {:points (geo/open-ring (first (:coordinates shape)))}
                                             :points)
    (= "Point" (:type shape))    (poly/->str {:points [(:coordinates shape)]} :points)
    :else                        (geo/write-json shape)))

(defn shapes-text
  [shapes kind]
  (when (seq shapes)
    (case kind
      :points   (if (next shapes)
                  (poly/rings->str (keep #(when (= "Polygon" (:type %))
                                            (geo/open-ring (first (:coordinates %))))
                                         shapes))
                  (shape-text (first shapes) :points))
      :geometry (if (next shapes)
                  (geo/write-json {:type "GeometryCollection" :geometries (vec shapes)})
                  (geo/write-json (first shapes)))
      (geo/output shapes kind))))

(def tools #{:point :line :polygon :rectangle :circle :wall})

(def modify-tools
  #{:points :translate :rotate})

(defn- finish-shape
  [{:keys [tool draft]} crs]
  (case tool
    :point     (when (seq draft) {:type "Point" :coordinates (first draft)})
    :line      (when (next draft) {:type "LineString" :coordinates (vec draft)})
    :polygon   (when (nnext draft)
                 {:type "Polygon" :coordinates [(conj (vec draft) (first draft))]})
    :rectangle (when (next draft)
                 (let [[[x1 y1] [x2 y2]] draft]
                   (geo/bbox->polygon [(min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2)])))
    :circle    (when (next draft) (geo/circle crs (first draft) (second draft)))
    nil))

(defn- with-shape
  [{:keys [shapes] :as st} shape {:keys [single-draw? antimeridian? crs]}]
  (let [s (cond-> shape (and antimeridian? (not= :plan crs)) geo/split-shape)]
    (assoc st :shapes (if single-draw? [s] (conj (vec shapes) s))
           :selected-shapes #{} :draft [] :tool nil :hover nil :hint nil)))

(defn- done?
  [{:keys [tool draft]}]
  (or (and (= :point tool) (seq draft))
      (and (#{:rectangle :circle} tool) (next draft))))

(defn- moved-vertex
  [{:keys [type coordinates] :as shape} path p]
  (case type
    "Point"      (assoc shape :coordinates p)
    "LineString" (assoc-in shape [:coordinates (first path)] p)
    "Polygon"    (let [i    (second path)
                       last (dec (count (first coordinates)))]
                   (cond-> (assoc-in shape [:coordinates 0 i] p)
                     (zero? i)    (assoc-in [:coordinates 0 last] p)
                     (= i last)   (assoc-in [:coordinates 0 0] p)))
    shape))

(defn- inserted-vertex
  [{:keys [type coordinates] :as shape} at p]
  (let [ins (fn [v] (vec (concat (subvec v 0 at) [p] (subvec v at))))]
    (case type
      "Polygon"    (let [r (ins (geo/open-ring (first coordinates)))]
                     (assoc-in shape [:coordinates 0] (conj r (first r))))
      "LineString" (update shape :coordinates (comp ins vec))
      shape)))

(defn- dropped-vertex
  [{:keys [type coordinates] :as shape} path]
  (case type
    "Polygon"    (let [r (geo/open-ring (first coordinates))
                       i (second path)]
                   (when (< 3 (count r))
                     (let [r (vec (concat (subvec r 0 i) (subvec r (inc i))))]
                       (assoc-in shape [:coordinates 0] (conj r (first r))))))
    "LineString" (when (< 2 (count coordinates))
                   (let [i (first path) v (vec coordinates)]
                     (assoc shape :coordinates (vec (concat (subvec v 0 i) (subvec v (inc i)))))))
    nil))

(defn- grip-at
  [{:keys [modify] :as st} p tol crs]
  (let [ts   (for [t (targets st) :let [g (target-shape st t)] :when g] [t g])
        near (fn [c] (<= (geo/distance crs c p) tol))]
    (or (when (= :points modify)
          (or (first (for [[t g] ts [path c] (vertex-paths g) :when (near c)]
                       {:target t :path path}))
              (first (for [[t g] ts [at c] (midpoints g) :when (near c)]
                       {:target t :insert at :at c}))))
        (first (for [[t g] ts
                     :when (or (geo/point-in-shape? p g)
                               (some (fn [[_ c]] (near c)) (vertex-paths g)))]
                 (case modify
                   :rotate {:target t :angle (angle-at (geo/centroid (all-coords g)) p)}
                   {:target t :from p}))))))

(defn reference-rings
  [cfg shapes]
  (vec (concat (for [l    (layers-of cfg)
                     g    (:geometries l)
                     :when (= :shape (->kw (:geometry g)))
                     r    (:rows l)
                     s    (row-shapes :plan (prop r (:shape g)))
                     ring (rings-of s)]
                 ring)
               (mapcat rings-of shapes))))

(defn- snapped
  [{:keys [tool draft]} p {:keys [crs snap-dirs start-segs tol]}]
  (cond
    (not (and (= :plan crs) snap-dirs (#{:line :polygon} tool))) p
    (empty? draft) (or (when (seq start-segs) (pf/onto-wall start-segs p (or tol 0))) p)
    :else          (snap-point draft p (snap-directions draft snap-dirs))))

(defn- wall-end
  [{:keys [draft]} p {:keys [plan-segs snap-dirs]}]
  (let [a    (first draft)
        ang  (angle-at a p)
        best (when snap-dirs (apply min-key #(angle-gap ang %) (snap-directions [] snap-dirs)))
        ang  (if (and best (<= (angle-gap ang best) (* 2 snap-tolerance))) best ang)]
    (pf/across plan-segs a ang)))

(defn- wall-click
  [{:keys [draft] :as st} p {:keys [plan-segs tol] :as opts}]
  (if (empty? draft)
    (if-let [q (pf/onto-wall plan-segs p (max (or tol 0) 0.25))]
      (assoc st :draft [q] :hint nil)
      (assoc st :hint :keine-wand))
    (if-let [b (when (not= p (first draft)) (wall-end st p opts))]
      (-> st
          (update :shapes (fnil conj []) {:type "LineString" :coordinates [(first draft) b]})
          (assoc :draft [] :hover nil :hint nil :effects [:shapes-changed]))
      (assoc st :hint :keine-wand))))

(defn- crosses-draft?
  [draft p]
  (let [es (map vector draft (rest draft))
        a  (peek (vec draft))]
    (boolean (some (fn [[c e]] (geo/segments-cross? a p c e)) (butlast es)))))

(defn- closes?
  [{:keys [tool draft]} p {:keys [tol crs]}]
  (boolean (and (= :polygon tool) (<= 3 (count draft)) tol
                (<= (geo/distance crs p (first draft)) tol))))

(defn- draw-at
  [{:keys [tool draft] :as st} p {:keys [crs] :as opts}]
  (cond
    (closes? st p opts)
    (let [ring (conj (vec draft) (first draft))]
      (if (geo/simple-ring? ring)
        (-> (with-shape st (finish-shape st crs) opts) (assoc :effects [:drawn]))
        (assoc st :hint :kreuzt)))

    (and (= :polygon tool) (next draft) (crosses-draft? draft p))
    (assoc st :hint :kreuzt)

    :else
    (let [s (-> st (update :draft (fnil conj []) p) (assoc :hint nil))]
      (if (done? s)
        (-> (with-shape s (finish-shape s crs) opts) (assoc :effects [:drawn]))
        s))))

(defn- draw-click
  [st p opts]
  (if (= :wall (:tool st))
    (wall-click st p opts)
    (draw-at st (if (closes? st p opts) p (snapped st p opts)) opts)))

(defn- effect-of [[kind]] (if (= :shape kind) :shapes-changed :edits-changed))

(defn- remove-shapes
  [st gone]
  (assoc st :selected-shapes #{}
         :shapes (vec (keep-indexed (fn [i s] (when-not (contains? gone i) s)) (:shapes st)))))

(def ^:private history-depth 50)

(defn- undoable [st] (select-keys st [:shapes :editing]))

(defn- travel
  [st from to]
  (let [[snap & more] (get st from)]
    (-> (merge st snap)
        (assoc from more :selected-shapes #{} :active-vertex nil :drag nil)
        (update to #(conj (or % ()) (undoable st)))
        (assoc :effects [:shapes-changed :edits-changed]))))

(defn- step*
  [st [ev x y] {:keys [crs] :as opts}]
  (let [st (dissoc st :effects)]
    (case ev
      :tool   (assoc st :tool x :draft [] :hover nil :mode nil :modify nil :hint nil)
      :hover  (cond
                (= :wall (:tool st)) (assoc st :hover (when (seq (:draft st)) (wall-end st x opts)))
                (:tool st)           (assoc st :hover (snapped st x opts))
                :else                st)
      :take-area (-> st
                     (update :shapes (fnil conj []) {:type "Polygon" :coordinates [x]})
                     (as-> s (assoc s :selected-shapes #{(dec (count (:shapes s)))}))
                     (assoc :effects [:shapes-changed :selection-changed]))
      :click  (if (:tool st) (draw-click st x opts) st)
      :edge   (let [ang (edge-angle (:draft st) y)
                    a   (peek (vec (:draft st)))]
                (if (and (#{:line :polygon} (:tool st)) a ang (number? x) (pos? x))
                  (draw-at st (along a ang x) opts)
                  st))
      :finish (if-let [shape (finish-shape st crs)]
                (if (and (= :polygon (:tool st))
                         (not (geo/simple-ring? (first (:coordinates shape)))))
                  (assoc st :hint :kreuzt)
                  (-> (with-shape st shape opts) (assoc :effects [:drawn])))
                (assoc st :draft [] :tool nil :hover nil))
      :undo   (cond (seq (:draft st)) (update st :draft (comp vec butlast))
                    (:tool st)        st
                    (seq (:past st))  (travel st :past :future)
                    :else             st)
      :redo   (if (and (not (:tool st)) (seq (:future st))) (travel st :future :past) st)
      :cancel (assoc st :draft [] :tool nil :hover nil :hint nil)

      :select-shape
      (-> st
          (update :selected-shapes (fn [s] (let [s (or s #{})]
                                             (cond (nil? x)       #{}
                                                   y              (if (s x) (disj s x) (conj s x))
                                                   :else          #{x}))))
          (assoc :effects [:selection-changed]))
      :clear  (assoc st :shapes [] :selected-shapes #{} :draft [] :tool nil :mode nil
                     :modify nil :effects [:shapes-changed])
      :delete-selected (-> (remove-shapes st (:selected-shapes st))
                           (assoc :effects [:shapes-changed]))
      :replace (let [i (first (:selected-shapes st))
                     g (get (:shapes st) i)
                     t (case (:type g) "Point" :point "LineString" :line :polygon)]
                 (if g
                   (-> (remove-shapes st #{i})
                       (assoc :tool t :draft [] :mode nil :modify nil
                              :effects [:shapes-changed]))
                   st))
      :buffer (let [ts (filter #(= :shape (first %)) (targets (assoc st :mode :modify)))]
                (if (and (number? x) (seq ts))
                  (-> (reduce (fn [s t] (set-target s t (geo/buffer crs (target-shape s t) x)))
                              st ts)
                      (assoc :effects [:shapes-changed]))
                  st))
      :set-coords (-> (set-target st x y) (assoc :effects [(effect-of x)]))

      :modify (assoc st :mode (when x :modify) :modify x :tool nil :draft [] :drag nil
                     :active-vertex nil)
      :done   (assoc st :mode nil :modify nil :drag nil :active-vertex nil)
      :press  (if-let [g (and (or (= :modify (:mode st)) (seq (:editing st)))
                              (grip-at (cond-> st (nil? (:modify st)) (assoc :modify :points))
                                       x (or (:tol opts) 0) crs))]
                (cond
                  (:insert g)
                  (let [t (:target g)
                        path (if (= "Polygon" (:type (target-shape st t)))
                               [0 (:insert g)] [(:insert g)])]
                    (-> (set-target st t (inserted-vertex (target-shape st t) (:insert g) (:at g)))
                        (assoc :drag {:target t :path path} :before (undoable st)
                               :active-vertex {:target t :path path})))
                  (:path g) (assoc st :drag g :active-vertex g :before (undoable st))
                  :else     (assoc st :drag g :active-vertex nil :before (undoable st)))
                (assoc st :active-vertex nil))
      :drag   (if-let [{:keys [target path from angle]} (:drag st)]
                (let [g (target-shape st target)]
                  (cond
                    path  (set-target st target (moved-vertex g path x))
                    angle (let [a (angle-at (geo/centroid (all-coords g)) x)]
                            (-> (set-target st target (geo/rotate crs g (- a angle)))
                                (assoc-in [:drag :angle] a)))
                    from  (-> (set-target st target
                                          (geo/translate g [(- (first x) (first from))
                                                            (- (second x) (second from))]))
                              (assoc-in [:drag :from] x))
                    :else st))
                st)
      :release (if-let [{:keys [target]} (:drag st)]
                 (assoc st :drag nil :before nil :effects [(effect-of target)])
                 st)
      :delete-vertex (if-let [{:keys [target path]} (:active-vertex st)]
                       (if-let [g (dropped-vertex (target-shape st target) path)]
                         (-> (set-target st target g)
                             (assoc :active-vertex nil :effects [(effect-of target)]))
                         (assoc st :hint :zu-wenig))
                       st)

      :edit-objects (assoc st :editing (merge (:editing st) x)
                           :originals (merge x (:originals st))
                           :mode :modify :modify :points :tool nil)
      :discard-edits (assoc st :editing {} :originals {} :mode nil :modify nil :drag nil
                            :active-vertex nil :effects [:edits-changed])
      :edits-saved (let [ks (or x (keys (:editing st)))]
                     (cond-> (-> st
                                 (update :editing #(apply dissoc % ks))
                                 (update :originals #(apply dissoc % ks))
                                 (assoc :effects [:edits-changed]))
                       (empty? (apply dissoc (:editing st) ks))
                       (assoc :mode nil :modify nil :active-vertex nil)))
      :import (let [{:keys [matched originals unmatched]} x]
                (cond-> st
                  (seq matched)   (-> (update :originals #(merge originals %))
                                      (update :editing merge matched)
                                      (assoc :mode :modify :modify :points :tool nil))
                  (seq unmatched) (update :shapes (fnil into []) unmatched)
                  true            (assoc :effects [:shapes-changed :edits-changed])))
      st)))

(defn step
  [st [ev :as event] opts]
  (let [after (step* st event opts)
        was   (or (:before st) (undoable st))]
    (cond
      (#{:edits-saved :discard-edits} ev) (assoc after :past () :future ())
      (#{:undo :redo} ev)                 after
      (and (some #{:drawn :shapes-changed :edits-changed} (:effects after))
           (not= was (undoable after)))
      (-> after
          (update :past #(take history-depth (conj (or % ()) was)))
          (assoc :future () :before nil))
      :else after)))

(defn changed-edits
  [{:keys [editing originals]}]
  (into {} (filter (fn [[k g]] (not= g (get originals k)))) editing))

(defn intersecting
  [crs layer shape clock]
  (let [rows (in-time layer clock)]
    (filterv (fn [r]
               (some (fn [g]
                       (if (#{:shape :choropleth} (->kw (:geometry g)))
                         (some #(geo/intersects? % shape) (row-shapes crs (prop r (:shape g))))
                         (when-let [p (point-of g r)] (geo/point-in-shape? p shape))))
                     (:geometries layer)))
             rows)))

(defn shape-hit
  [{:keys [shapes]} p]
  (first (filter #(geo/point-in-shape? p (get shapes %)) (reverse (range (count shapes))))))

(defn tick
  [{:keys [cursor end playing?] :as clock} step-ms auto-pause]
  (if-not (and playing? cursor)
    clock
    (let [next-t (+ cursor step-ms)
          stop   (->> auto-pause (filter #(and (> % cursor) (<= % next-t))) sort first)]
      (cond
        stop                     (assoc clock :cursor stop :playing? false)
        (and end (>= next-t end)) (assoc clock :cursor end :playing? false)
        :else                    (assoc clock :cursor next-t)))))

(defn clock-range
  [layers]
  (let [ts (into [] (mapcat (fn [{:keys [rows time-prop]}]
                              (when time-prop (keep #(some-> (prop % time-prop) ms) rows))))
                 layers)]
    (when (seq ts) [(apply min ts) (apply max ts)])))

(defn show-time
  [t {:keys [time-zone time-format]}]
  #?(:cljs (.toLocaleString
            (js-date t) js/undefined
            (clj->js (cond-> {}
                       (= :utc (->kw time-zone))    (assoc :timeZone "UTC")
                       (= :h12 (->kw time-format)) (assoc :hour12 true)
                       (= :h24 (->kw time-format)) (assoc :hour12 false))))
     :clj  (let [_ [time-zone time-format]] (str (js-date t)))))
