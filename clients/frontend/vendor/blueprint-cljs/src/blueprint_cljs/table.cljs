(ns blueprint-cljs.table
  "Table2 of Blueprint's table package: a virtualised grid of cells with column and row
   headers, frozen rows/columns, selection regions, a focused cell, resizing and
   reordering. Columns are given as children: `[table {:num-rows 1000}
   [column {:name \"A\" :cell-renderer (fn [row col] [cell {} …])}] …]`.

   Layout: one scroll container holds a 3×3 CSS grid — row-header / frozen / main
   columns × column-header / frozen / main rows. Headers and frozen areas are sticky grid
   areas, so they move with the scroll natively; each area renders only the rows and
   columns it can show."
  (:require [reagent.core :as r]
            [blueprint-cljs.core :as bp]
            [blueprint-cljs.context-menu :as context-menu]
            [blueprint-cljs.table.cells :as cell*]
            [blueprint-cljs.table.clipboard :as clipboard]
            [blueprint-cljs.table.formats :as formats]
            [blueprint-cljs.table.headers :as headers]
            [blueprint-cljs.table.layout :as layout]
            [blueprint-cljs.table.regions :as regions]))

(def cell cell*/cell)
(def editable-cell cell*/editable-cell)
(def truncated-format formats/truncated-format)
(def json-format formats/json-format)
(def column-header-cell headers/column-header-cell)
(def row-header-cell headers/row-header-cell)
(def editable-name headers/editable-name)
(def horizontal-cell-divider headers/horizontal-cell-divider)
(def copy-cells-to-clipboard! clipboard/copy-cells!)
(def column-name
  "Blueprint's default column name for an index: A … Z, AA …."
  layout/base26)

(defn column
  "A column definition, read by `table` and never rendered itself. Props: `:name`, `:id`,
   `:cell-renderer (fn [row col] cell)`, `:column-header-cell-renderer (fn [col] header)`,
   `:loading-options` (a set of :cells :column-header)."
  [_]
  nil)

(def ^:private defaults
  {:default-column-width 150 :default-row-height 20 :enable-column-header true
   :enable-focused-cell false :enable-multiple-selection true :enable-row-header true
   :max-column-width 9999 :max-row-height 9999 :min-column-width 50 :min-row-height 20
   :num-frozen-columns 0 :num-frozen-rows 0 :num-rows 0 :selection-modes :all
   :overscan-rows 6 :overscan-columns 2})

(defn- columns-of
  [children]
  (->> children
       (mapcat #(if (seq? %) % [%]))
       (keep (fn [c] (when (vector? c) (if (map? (second c)) (second c) {}))))
       vec))

(defn- inject
  "Blueprint's cloneElement: the table's `extra` props (position, classes, data
   attributes) win; `fn-extra` (indices, loading) goes only to components, and the
   caller's own props win over it."
  [h extra fn-extra]
  (when h
    (let [h            (if (vector? h) h [cell*/cell {} h])
          [tag & more] h
          [p kids]     (if (map? (first more)) [(first more) (rest more)] [{} more])
          p'           (merge p (dissoc extra :style :class)
                              {:style (merge (:style p) (:style extra))
                               :class (bp/cx (:class extra) (:class p))})
          p'           (if (keyword? tag) p' (merge fn-extra p'))]
      (into [tag p'] kids))))

(defn- memo
  "Recomputes `(f)` only when `k` changes."
  [cache k f]
  (let [[k0 v] @cache]
    (if (= k k0)
      v
      (let [v (f)] (reset! cache [k v]) v))))

(defn- digits [n] (count (str (max 1 n))))

(defn- row-header-width
  [p n-rows]
  (if (:enable-row-header p)
    (or (:row-header-width p) (max 30 (+ 10 (* 7 (digits n-rows)))))
    0))

(defn- column-header-height
  [p]
  (if (:enable-column-header p)
    (or (:column-header-height p) (if (:enable-column-interaction-bar p) 51 30))
    0))

(defn- clamp [x lo hi] (max lo (min hi x)))

(defn- in-span? [[a b] i] (and a (<= a i) (<= i b)))

;; --------------------------------------------------------------------- geometry

(defn- area-span
  "All indices of a grid area along one axis: 1 header, 2 frozen, 3 main."
  [area nf n]
  (case area
    1 :header
    2 (when (pos? nf) [0 (dec nf)])
    3 (when (< nf n) [nf (dec n)])))

(defn- area-origin [area frozen-px] (if (= area 3) frozen-px 0))

(defn- pane-style
  [ra ca {:keys [rhw chh]}]
  (let [score {1 3 2 2 3 0}
        top   (case ra 1 0 2 chh nil)
        left  (case ca 1 0 2 rhw nil)]
    ;; every inset is set: Blueprint's quadrant classes carry `right: 0` / `bottom: 0`,
    ;; which would make a sticky pane stick to the far edge
    {:grid-row    ra
     :grid-column ca
     :position    (if (or top left) "sticky" "relative")
     :z-index     (+ (score ra) (score ca))
     :top         (or top "auto")
     :left        (or left "auto")
     :right       "auto"
     :bottom      "auto"}))

(defn- edge-px
  "Viewport offset of boundary `k` (the start of index k) along an axis; the boundary
   after the last frozen index stays at the frozen edge."
  [o k nf header-px scroll]
  (+ header-px (layout/start o k) (if (> k nf) (- scroll) 0)))

(defn- start-px
  "Viewport offset of the start of index `i` along an axis."
  [o i nf header-px scroll]
  (+ header-px (layout/start o i) (if (< i nf) 0 (- scroll))))

(defn- dom-attrs
  [p]
  (into {} (filter (fn [[k _]] (or (= k :id) (re-find #"^(data|aria)-" (name k))))) p))

;; --------------------------------------------------------------------- overlays

(defn- axis-rect
  "[start size] of `span` (inclusive indices) within a pane spanning `pane-span`, in pane
   pixels; a cut edge is pushed out so its border falls outside the pane."
  [o span pane-span origin]
  (let [[a b]   span
        [pa pb] pane-span
        a'      (max a pa)
        b'      (min b pb)]
    (when (<= a' b')
      (let [s (- (layout/start o a') origin (if (> a' a) 3 0))
            e (+ (- (layout/start o (inc b')) origin) (if (< b' b) 3 0))]
        [s (- e s)]))))

(defn- region-rect
  [{:keys [co ro n-rows n-cols]} {:keys [rows cols x0 y0 w h]} region]
  (let [[[r0 r1] [c0 c1]] (regions/bounds region n-rows n-cols)
        y (if (= rows :header)
            (when (nil? (:rows region)) [0 h])
            (axis-rect ro [r0 r1] rows y0))
        x (if (= cols :header)
            (when (nil? (:cols region)) [0 w])
            (axis-rect co [c0 c1] cols x0))]
    (when (and x y)
      {:left (first x) :width (second x) :top (first y) :height (second y)})))

(defn- overlay-layer
  [geo pane selected focused groups]
  (let [body? (and (not= (:rows pane) :header) (not= (:cols pane) :header))
        items (concat
               (for [[i g] (map-indexed vector groups)
                     [j reg] (map-indexed vector (:regions g))
                     :let [rect (region-rect geo pane reg)] :when rect]
                 [(str "g" i "-" j) (bp/cx (bp/cls "table-region") (:class-name g)) rect])
               (for [[i reg] (map-indexed vector selected)
                     :let [rect (region-rect geo pane reg)] :when rect]
                 [(str "s" i) (bp/cx (bp/cls "table-region") (bp/cls "table-selection-region"))
                  rect])
               (when (and body? focused)
                 (when-let [rect (region-rect geo pane (regions/cell (:row focused)
                                                                     (:col focused)))]
                   [["focus" (bp/cls "table-focus-region") rect]])))]
    [:div {:class (bp/cls "table-overlay-layer")}
     (for [[k cls rect] items]
       ^{:key k} [:div {:class (bp/cx (bp/cls "table-overlay") cls) :style rect}])]))

;; --------------------------------------------------------------------- panes

(defn- resize-handle
  [axis i dragging?]
  [:div {:class          (bp/cx (bp/cls "table-resize-handle-target")
                                (bp/cls (if (= axis :col)
                                          "table-resize-vertical"
                                          "table-resize-horizontal"))
                                {(bp/cls "table-dragging") dragging?})
         :data-bp-resize (str (name axis) ":" i)}
   [:div {:class (bp/cx (bp/cls "table-resize-handle") {(bp/cls "table-dragging") dragging?})}]])

(defn- column-headers
  [{:keys [p cols geo selected drag]} {:keys [x0 vis]}]
  (let [{:keys [co chh nf]} geo
        lo   (:loading-options p)
        sel? (fn [c] (regions/column-selected? selected c))]
    (for [c (when vis (range (first vis) (inc (second vis))))
          :let [col  (nth cols c)
                rend (or (:column-header-cell-renderer col)
                         (fn [i] [headers/column-header-cell
                                  {:name (or (:name col) (layout/base26 i))}]))
                h    (inject (rend c)
                             {:style {:position "absolute" :top 0 :height chh
                                      :left (- (layout/start co c) x0)
                                      :width (layout/size co c)}
                              :class (when (= c (dec nf)) (bp/cls "table-last-in-row"))
                              :data-bp-col-header c}
                             {:index c
                              :is-column-selected (sel? c)
                              :loading (or (contains? (set lo) :column-headers)
                                           (contains? (set (:loading-options col))
                                                      :column-header))
                              :enable-column-interaction-bar
                              (:enable-column-interaction-bar p)
                              :enable-column-reordering
                              (and (:enable-column-reordering p) (sel? c))
                              :resize-handle
                              (when (:enable-column-resizing p)
                                (resize-handle :col c (= [:col c] (:resizing drag))))})]
          :when h]
      (with-meta h {:key c}))))

(defn- row-headers
  [{:keys [p selected drag geo]} {:keys [y0 vis]}]
  (let [{:keys [ro rhw nfr]} geo
        rend (or (:row-header-cell-renderer p)
                 (fn [i] [headers/row-header-cell {:name (str (inc i))}]))
        lo   (set (:loading-options p))]
    (for [rw (when vis (range (first vis) (inc (second vis))))
          :let [h (inject (rend rw)
                          {:style {:position "absolute" :left 0 :width rhw
                                   :top (- (layout/start ro rw) y0)
                                   :height (layout/size ro rw)}
                           :class (when (= rw (dec nfr)) (bp/cls "table-last-in-column"))
                           :data-bp-row-header rw}
                          {:index rw
                           :is-row-selected (regions/row-selected? selected rw)
                           :loading (contains? lo :row-headers)
                           :enable-row-reordering
                           (and (:enable-row-reordering p)
                                (regions/row-selected? selected rw))
                           :resize-handle
                           (when (:enable-row-resizing p)
                             (resize-handle :row rw (= [:row rw] (:resizing drag))))})]
          :when h]
      (with-meta h {:key rw}))))

(defn- body-cells
  [{:keys [p cols geo]} {:keys [x0 y0 vis-rows vis-cols]}]
  (let [{:keys [co ro nf nfr]} geo
        table-loading? (contains? (set (:loading-options p)) :cells)]
    (for [rw (when vis-rows (range (first vis-rows) (inc (second vis-rows))))
          c  (when vis-cols (range (first vis-cols) (inc (second vis-cols))))
          :let [col  (nth cols c)
                rend (:cell-renderer col)
                h    (inject (if rend (rend rw c) [cell*/cell {}])
                             {:style {:position "absolute"
                                      :left (- (layout/start co c) x0)
                                      :top (- (layout/start ro rw) y0)
                                      :width (layout/size co c)
                                      :height (layout/size ro rw)}
                              :class (bp/cx (bp/cls (if (odd? rw)
                                                      "table-cell-ledger-odd"
                                                      "table-cell-ledger-even"))
                                            {(bp/cls "table-last-in-row") (= c (dec nf))
                                             (bp/cls "table-last-in-column")
                                             (= rw (dec nfr))})
                              :data-bp-cell (str rw "," c)}
                             {:row-index rw :column-index c
                              :loading (or table-loading?
                                           (contains? (set (:loading-options col)) :cells))})]]
      (with-meta h {:key (str rw "," c)}))))

(defn- panes
  "The grid areas with what each shows."
  [{:keys [geo view]}]
  (let [{:keys [co ro nf nfr n-rows n-cols rhw chh fw fh tw th]} geo
        {:keys [sl stp vw vh orows ocols]} view
        vis-c {2 (area-span 2 nf n-cols)
               3 (layout/visible-range co nf n-cols (+ sl fw) (+ sl (- vw rhw)) ocols)}
        vis-r {2 (area-span 2 nfr n-rows)
               3 (layout/visible-range ro nfr n-rows (+ stp fh) (+ stp (- vh chh)) orows)}
        width  {1 rhw 2 fw 3 (- tw fw)}
        height {1 chh 2 fh 3 (- th fh)}]
    (for [ra [1 2 3] ca [1 2 3]
          :when (and (pos? (width ca)) (pos? (height ra)))]
      {:ra ra :ca ca
       :rows (area-span ra nfr n-rows) :cols (area-span ca nf n-cols)
       :x0 (area-origin ca fw) :y0 (area-origin ra fh)
       :w (width ca) :h (height ra)
       :vis-rows (vis-r ra) :vis-cols (vis-c ca)})))

(defn- quadrant-class
  [ra ca]
  (cond
    (or (and (= ra 1) (= ca 2)) (and (= ra 2) (= ca 1)) (and (= ra 2) (= ca 2)))
    (bp/cls "table-quadrant-top-left")
    (= ra 2) (bp/cls "table-quadrant-top")
    (= ca 2) (bp/cls "table-quadrant-left")))

(defn- render-pane
  [{:keys [geo modes selected focused p] :as ctx} {:keys [ra ca] :as pane}]
  (let [style  (pane-style ra ca geo)
        sel-on (fn [k] {(bp/cls "table-selection-enabled") (contains? modes k)})
        groups (:styled-region-groups p)]
    (cond
      (and (= ra 1) (= ca 1))
      [:div {:key "menu" :style style
             :class (bp/cx (bp/cls "table-menu") (sel-on :full-table))}]

      (= ra 1)
      [:div {:key (str "ch" ca) :style style
             :class (bp/cx (bp/cls "table-column-headers") (sel-on :full-columns)
                           (quadrant-class ra ca))}
       (doall (column-headers ctx {:x0 (:x0 pane) :vis (:vis-cols pane)}))
       [overlay-layer geo pane selected nil nil]]

      (= ca 1)
      [:div {:key (str "rh" ra) :style style
             :class (bp/cx (bp/cls "table-row-headers") (sel-on :full-rows)
                           (quadrant-class ra ca))}
       (doall (row-headers ctx {:y0 (:y0 pane) :vis (:vis-rows pane)}))
       [overlay-layer geo pane selected nil nil]]

      :else
      [:div {:key (str "b" ra ca) :style style
             :class (bp/cx (bp/cls "table-cell-client") (sel-on :cells)
                           (quadrant-class ra ca))}
       (doall (body-cells ctx pane))
       [overlay-layer geo pane selected focused groups]])))

;; --------------------------------------------------------------------- component

(defn table
  "Props as Blueprint's Table2, kebab-cased: `:num-rows`, `:column-widths`,
   `:row-heights`, `:default-column-width` (150), `:default-row-height` (20),
   `:min-/max-column-width`, `:min-/max-row-height`, `:enable-column-resizing`,
   `:enable-row-resizing`, `:enable-column-reordering`, `:enable-row-reordering`,
   `:enable-row-header` (true), `:enable-column-header` (true),
   `:enable-column-interaction-bar`, `:enable-multiple-selection` (true),
   `:enable-focused-cell`, `:selection-modes` (:all :none :columns-and-cells
   :columns-only :rows-and-cells :rows-only, or a set of :cells :full-rows :full-columns
   :full-table), `:selected-regions` (controlled) + `:on-selection (fn [regions])`,
   `:selected-region-transform (fn [region event])`, `:focused-cell` {:row :col}
   (controlled) + `:on-focused-cell`, `:num-frozen-columns`, `:num-frozen-rows`,
   `:loading-options` (set of :cells :column-headers :row-headers),
   `:row-header-cell-renderer (fn [row])`, `:body-context-menu-renderer (fn [ctx] menu)`
   (ctx: :target :target-cell :selected-regions :regions :unique-cells),
   `:get-cell-clipboard-data (fn [row col cell-renderer])`, `:on-copy (fn [ok?])`,
   `:on-column-width-changed (fn [index size])`, `:on-row-height-changed`,
   `:on-columns-reordered (fn [old-index new-index length])`, `:on-rows-reordered`,
   `:on-visible-cells-change (fn [{:keys [row-index-start row-index-end]}
   {:keys [column-index-start column-index-end]}])`, `:styled-region-groups`
   [{:class-name :regions}], `:class`, `:style`. Extensions: `:row-header-width`,
   `:column-header-height`, `:overscan-rows` (6), `:overscan-columns` (2).
   `:render-mode`, `:enable-ghost-cells` and `:focus-mode` are accepted and ignored:
   rendering is always virtualised. Children: `[column {…}]` forms."
  [& _]
  (let [st        (r/atom {:sl 0 :stp 0 :vw 0 :vh 0 :selected [] :focused nil
                           :widths nil :heights nil :drag nil :guide nil})
        cur       (atom nil)
        seen      (atom {:widths ::none :heights ::none})
        container (atom nil)
        scroller  (atom nil)
        observer  (atom nil)
        cw-cache  (atom nil)
        rh-cache  (atom nil)
        visible   (atom nil)
        listeners (atom nil)
        props     (fn [] (:p @cur))
        selection (fn [] (let [p (props)]
                           (if (contains? p :selected-regions)
                             (vec (:selected-regions p))
                             (:selected @st))))
        focus     (fn [] (let [p (props)]
                           (if (contains? p :focused-cell) (:focused-cell p) (:focused @st))))
        set-selection!
        (fn [regs]
          (let [regs (vec regs)]
            (when (not= regs (selection))
              (when-not (contains? (props) :selected-regions)
                (swap! st assoc :selected regs))
              (when-let [f (:on-selection (props))] (f regs)))))
        set-focus!
        (fn [cellc]
          (when (and (:enable-focused-cell (props)) (not= cellc (focus)))
            (when-not (contains? (props) :focused-cell)
              (swap! st assoc :focused cellc))
            (when-let [f (:on-focused-cell (props))] (f cellc))))
        viewport  (fn [e]
                    (let [el   @scroller
                          rect (.getBoundingClientRect el)]
                      {:vx (- (.-clientX e) (.-left rect)) :vy (- (.-clientY e) (.-top rect))
                       :cw (.-clientWidth el) :ch (.-clientHeight el)
                       :sl (.-scrollLeft el) :stp (.-scrollTop el)}))
        hit       (fn [{:keys [vx vy sl stp]} clamp?]
                    (let [{:keys [rhw chh co ro fw fh]} (:geo @cur)]
                      {:col (cond (and (< vx rhw) (not clamp?)) nil
                                  (< vx (+ rhw fw)) (layout/index-at co (max 0 (- vx rhw)))
                                  :else (layout/index-at co (- (+ vx sl) rhw)))
                       :row (cond (and (< vy chh) (not clamp?)) nil
                                  (< vy (+ chh fh)) (layout/index-at ro (max 0 (- vy chh)))
                                  :else (layout/index-at ro (- (+ vy stp) chh)))}))
        content-px (fn [axis {:keys [vx vy sl stp]}]
                     (let [{:keys [rhw chh fw fh]} (:geo @cur)]
                       (if (= axis :col)
                         (if (< vx (+ rhw fw)) (- vx rhw) (- (+ vx sl) rhw))
                         (if (< vy (+ chh fh)) (- vy chh) (- (+ vy stp) chh)))))
        scroll-into-view!
        (fn [[rw c]]
          (when-let [el @scroller]
            (let [{:keys [co ro nf nfr fw fh rhw chh]} (:geo @cur)
                  vw (.-clientWidth el) vh (.-clientHeight el)]
              (when (and c (>= c nf))
                (let [a (- (layout/start co c) fw) b (- (layout/start co (inc c)) (- vw rhw))
                      sl (.-scrollLeft el)]
                  (cond (< (- (layout/start co c) fw) sl) (set! (.-scrollLeft el) a)
                        (> b sl) (set! (.-scrollLeft el) b))))
              (when (and rw (>= rw nfr))
                (let [a (- (layout/start ro rw) fh) b (- (layout/start ro (inc rw)) (- vh chh))
                      s (.-scrollTop el)]
                  (cond (< a s) (set! (.-scrollTop el) a)
                        (> b s) (set! (.-scrollTop el) b)))))))
        stop-drag!
        (fn []
          (when-let [[mm mu] @listeners]
            (.removeEventListener js/window "mousemove" mm)
            (.removeEventListener js/window "mouseup" mu)
            (reset! listeners nil)))
        drag!
        (fn [on-move on-up]
          (stop-drag!)
          (let [mm (fn [e] (on-move e))
                mu (fn [e] (stop-drag!) (on-up e))]
            (reset! listeners [mm mu])
            (.addEventListener js/window "mousemove" mm)
            (.addEventListener js/window "mouseup" mu)))
        autoscroll!
        (fn [{:keys [vx vy cw ch]}]
          (let [el @scroller step 20]
            (cond (> vy (- ch 10)) (set! (.-scrollTop el) (+ (.-scrollTop el) step))
                  (< vy 0)         (set! (.-scrollTop el) (- (.-scrollTop el) step)))
            (cond (> vx (- cw 10)) (set! (.-scrollLeft el) (+ (.-scrollLeft el) step))
                  (< vx 0)         (set! (.-scrollLeft el) (- (.-scrollLeft el) step)))))
        start-select!
        (fn [kind {:keys [row col]} e]
          (let [p        (props)
                xf       (:selected-region-transform p)
                mk       (fn [a b] (let [reg (regions/expand kind a b)]
                                     (if xf (xf reg e) reg)))
                sel      (selection)
                f        (focus)
                here     [(or row 0) (or col 0)]
                shift?   (and (.-shiftKey e) (seq sel))
                mod?     (and (or (.-metaKey e) (.-ctrlKey e)) (:enable-multiple-selection p))
                anchor   (if (and shift? f) [(:row f) (:col f)] here)
                base     (cond shift? (vec (butlast sel)) mod? sel :else [])]
            (set-selection! (conj base (mk anchor here)))
            (when-not shift? (set-focus! {:row (first here) :col (second here)}))
            (swap! st dissoc :corner)
            (drag! (fn [e]
                     (let [v (viewport e)
                           {:keys [row col]} (hit v true)]
                       (autoscroll! v)
                       (when (and row col)
                         (set-selection! (conj base (mk anchor [row col]))))))
                   (fn [_]))))
        start-resize!
        (fn [axis i e]
          (let [{:keys [co ro nf nfr rhw chh]} (:geo @cur)
                p      (props)
                o      (if (= axis :col) co ro)
                size0  (layout/size o i)
                [lo hi] (if (= axis :col)
                          [(:min-column-width p) (:max-column-width p)]
                          [(:min-row-height p) (:max-row-height p)])
                origin (if (= axis :col) (.-clientX e) (.-clientY e))
                size   (atom size0)
                guide  (fn []
                         (let [v  (viewport e)
                               nf (if (= axis :col) nf nfr)
                               hp (if (= axis :col) rhw chh)
                               sc (if (= axis :col) (:sl v) (:stp v))]
                           (+ (start-px o i nf hp sc) @size)))]
            (swap! st assoc :drag {:resizing [axis i]} :guide {:axis axis :px (guide)})
            (drag! (fn [e]
                     (let [d (- (if (= axis :col) (.-clientX e) (.-clientY e)) origin)]
                       (reset! size (clamp (+ size0 d) lo hi))
                       (swap! st assoc :guide {:axis axis :px (guide)})))
                   (fn [_]
                     (swap! st assoc :drag nil :guide nil)
                     (when (not= @size size0)
                       (let [k (if (= axis :col) :widths :heights)
                             n (if (= axis :col) (layout/n-of co) (layout/n-of ro))
                             cur-sizes (mapv #(layout/size o %) (range n))]
                         (swap! st assoc k (assoc cur-sizes i @size))
                         (when-let [f (get p (if (= axis :col)
                                               :on-column-width-changed
                                               :on-row-height-changed))]
                           (f i @size))))))))
        auto-size!
        (fn [axis i]
          (when (= axis :col)
            (let [p     (props)
                  els   (.querySelectorAll @container
                                           (str "[data-bp-cell$=\"," i "\"] > div, "
                                                "[data-bp-col-header=\"" i "\"] "
                                                "." (bp/cls "table-column-name-text") " > *"))
                  width (reduce (fn [m el] (max m (.-scrollWidth el))) 0 (array-seq els))
                  size  (clamp (+ width 16) (:min-column-width p) (:max-column-width p))
                  {:keys [co]} (:geo @cur)]
              (when (pos? width)
                (swap! st assoc :widths
                       (assoc (mapv #(layout/size co %) (range (layout/n-of co))) i size))
                (when-let [f (:on-column-width-changed p)] (f i size))))))
        start-reorder!
        (fn [axis [a b] e]
          (let [len  (inc (- b a))
                o    (if (= axis :col) (:co (:geo @cur)) (:ro (:geo @cur)))
                bnd  (atom nil)
                guide-px (fn [k v]
                           (let [{:keys [nf nfr rhw chh]} (:geo @cur)]
                             (if (= axis :col)
                               (edge-px o k nf rhw (:sl v))
                               (edge-px o k nfr chh (:stp v)))))]
            (swap! st assoc :drag {:reordering axis})
            (drag! (fn [e]
                     (let [v (viewport e)
                           k (layout/boundary-at o (content-px axis v))]
                       (autoscroll! v)
                       (reset! bnd k)
                       (swap! st assoc :guide (when-not (<= a k (inc b))
                                                {:axis axis :px (guide-px k v)}))))
                   (fn [_]
                     (swap! st assoc :drag nil :guide nil)
                     (let [k @bnd]
                       (when (and k (not (<= a k (inc b))))
                         (let [to (if (> k b) (- k len) k)
                               p  (props)]
                           (when-let [f (get p (if (= axis :col)
                                                 :on-columns-reordered
                                                 :on-rows-reordered))]
                             (f a to len))
                           (set-selection! [(if (= axis :col)
                                              (regions/column to (+ to len -1))
                                              (regions/row to (+ to len -1)))]))))))
            (.preventDefault e)))
        on-mouse-down
        (fn [e]
          (let [t (.-target e)]
            (when (and (zero? (.-button e))
                       (not (.closest t (str "input, textarea, select, [contenteditable='true'],"
                                             " ." (bp/cls "table-th-menu-container") ","
                                             " ." (bp/cls "table-cell-interactive") ","
                                             " ." (bp/cls "table-truncated-popover-target")))))
              (let [v     (viewport e)
                    modes (:modes @cur)]
                (when (and (< (:vx v) (:cw v)) (< (:vy v) (:ch v)))
                  (if-let [h (.closest t "[data-bp-resize]")]
                    (let [[axis i] (.split (.getAttribute h "data-bp-resize") ":")]
                      (.preventDefault e)
                      (start-resize! (keyword axis) (js/parseInt i) e))
                    (let [{:keys [row col] :as at} (hit v false)
                          p   (props)
                          sel (selection)
                          block (fn [axis i]
                                  (some (fn [reg]
                                          (let [span (get reg (if (= axis :col) :cols :rows))
                                                other (get reg (if (= axis :col) :rows :cols))]
                                            (when (and (nil? other) (in-span? span i)) span)))
                                        sel))]
                      (.preventDefault e)
                      (some-> @container (.focus #js {:preventScroll true}))
                      (cond
                        (and (nil? row) (nil? col))
                        (when (contains? modes :full-table)
                          (set-selection! [regions/table]))

                        (nil? row)
                        (if-let [span (and (:enable-column-reordering p) (block :col col))]
                          (start-reorder! :col span e)
                          (when (contains? modes :full-columns)
                            (start-select! :full-columns at e)))

                        (nil? col)
                        (if-let [span (and (:enable-row-reordering p) (block :row row))]
                          (start-reorder! :row span e)
                          (when (contains? modes :full-rows)
                            (start-select! :full-rows at e)))

                        (contains? modes :cells)
                        (start-select! :cells at e)

                        :else
                        (set-focus! {:row row :col col})))))))))
        on-double-click
        (fn [e]
          (when-let [h (.closest (.-target e) "[data-bp-resize]")]
            (let [[axis i] (.split (.getAttribute h "data-bp-resize") ":")]
              (auto-size! (keyword axis) (js/parseInt i)))))
        edit-focused!
        (fn [text]
          (when-let [{:keys [row col]} (focus)]
            (when-let [el (.querySelector @container (str "[data-bp-cell=\"" row "," col "\"]"))]
              (not (.dispatchEvent el (js/CustomEvent. cell*/edit-event
                                                       #js {:detail text :cancelable true}))))))
        copy!
        (fn []
          (let [{:keys [cols geo]} @cur
                p   (props)
                sel (selection)
                {:keys [n-rows n-cols]} geo]
            (when (seq sel)
              (let [bs (map #(regions/bounds % n-rows n-cols) sel)
                    r0 (apply min (map ffirst bs)) r1 (apply max (map (comp second first) bs))
                    c0 (apply min (map (comp first second) bs))
                    c1 (apply max (map (comp second second) bs))
                    get-data (or (:get-cell-clipboard-data p)
                                 (fn [rw c rend] (clipboard/hiccup-text (rend rw c))))
                    rend-of  (fn [c] (or (:cell-renderer (nth cols c)) (constantly nil)))
                    on-copy  (or (:on-copy p) (fn [_]))]
                (if (> (* (inc (- r1 r0)) (inc (- c1 c0))) 1000000)
                  (on-copy false)
                  (-> (clipboard/copy-cells!
                       (for [rw (range r0 (inc r1))]
                         (for [c (range c0 (inc c1))]
                           (if (regions/any-contains-cell? sel rw c)
                             (get-data rw c (rend-of c))
                             ""))))
                      (.then on-copy)))))))
        move!
        (fn [dr dc extend?]
          (let [{:keys [n-rows n-cols]} (:geo @cur)
                f      (or (focus) {:row 0 :col 0})
                cells? (contains? (:modes @cur) :cells)]
            (when (and (pos? n-rows) (pos? n-cols))
              (if extend?
                (let [[cr cc] (or (:corner @st) [(:row f) (:col f)])
                      corner  [(clamp (+ cr dr) 0 (dec n-rows)) (clamp (+ cc dc) 0 (dec n-cols))]]
                  (when cells?
                    (swap! st assoc :corner corner)
                    (set-selection! (conj (vec (butlast (selection)))
                                          (regions/cells (:row f) (:col f)
                                                         (first corner) (second corner))))
                    (scroll-into-view! corner)))
                (let [rw (clamp (+ (:row f) dr) 0 (dec n-rows))
                      c  (clamp (+ (:col f) dc) 0 (dec n-cols))]
                  (swap! st dissoc :corner)
                  (set-focus! {:row rw :col c})
                  (when cells? (set-selection! [(regions/cell rw c)]))
                  (scroll-into-view! [rw c]))))))
        on-key-down
        (fn [e]
          (when-not (.closest (.-target e) "input, textarea, select, [contenteditable='true']")
            (let [k      (.-key e)
                  shift? (.-shiftKey e)
                  mod?   (or (.-metaKey e) (.-ctrlKey e))
                  focus? (:enable-focused-cell (props))
                  arrows {"ArrowUp" [-1 0] "ArrowDown" [1 0] "ArrowLeft" [0 -1]
                          "ArrowRight" [0 1]}
                  done   #(.preventDefault e)]
              (cond
                (and mod? (= k "c")) (do (copy!) (done))
                (and mod? (= k "a") (contains? (:modes @cur) :full-table))
                (do (set-selection! [regions/table]) (done))
                (not focus?) nil
                (arrows k) (let [[dr dc] (arrows k)] (move! dr dc shift?) (done))
                (= k "Tab") (do (move! 0 (if shift? -1 1) false) (done))
                (= k "Enter") (do (move! (if shift? -1 1) 0 false) (done))
                (= k "F2") (do (edit-focused! nil) (done))
                (and (= 1 (count k)) (not mod?) (not (.-altKey e)))
                (when (edit-focused! k) (done))))))
        on-context-menu
        (fn [e]
          (let [p (props)]
            (when-let [renderer (and (not (false? (:enable-body-context-menu p)))
                                     (:body-context-menu-renderer p))]
              (let [{:keys [row col]} (hit (viewport e) false)]
                (when (and row col)
                  (.preventDefault e)
                  (let [sel     (selection)
                        target  (regions/cell row col)
                        in-sel? (regions/any-contains-cell? sel row col)
                        _       (when (and (not in-sel?) (contains? (:modes @cur) :cells))
                                  (set-selection! [target])
                                  (set-focus! {:row row :col col}))
                        regs    (if in-sel? sel [target])
                        {:keys [n-rows n-cols]} (:geo @cur)]
                    (context-menu/show-context-menu!
                     {:content       (renderer {:target           target
                                                :target-cell      [row col]
                                                :selected-regions (selection)
                                                :regions          regs
                                                :unique-cells
                                                #(regions/enumerate-cells regs n-rows n-cols)})
                      :target-offset {:left (.-clientX e) :top (.-clientY e)}
                      :is-dark-theme (some? (.closest @container
                                                      (str "." bp/dark-class)))})))))))
        on-scroll
        (fn [_]
          (when-let [el @scroller]
            (swap! st assoc :sl (.-scrollLeft el) :stp (.-scrollTop el))))
        measure!
        (fn []
          (when-let [el @scroller]
            (let [w (.-clientWidth el) h (.-clientHeight el)]
              (when (or (not= w (:vw @st)) (not= h (:vh @st)))
                (swap! st assoc :vw w :vh h)))))
        report-visible!
        (fn []
          (when-let [f (:on-visible-cells-change (props))]
            (let [v (:visible @cur)]
              (when (not= v @visible)
                (reset! visible v)
                (let [[[r0 r1] [c0 c1]] v]
                  (f {:row-index-start r0 :row-index-end r1}
                     {:column-index-start c0 :column-index-end c1}))))))]
    (r/create-class
     {:display-name "Table2"
      :component-did-mount
      (fn [_]
        (measure!)
        (let [ro (js/ResizeObserver. (fn [_] (measure!)))]
          (.observe ro @scroller)
          (reset! observer ro))
        (report-visible!))
      :component-did-update (fn [_] (report-visible!))
      :component-will-unmount
      (fn [_]
        (stop-drag!)
        (some-> @observer .disconnect))
      :reagent-render
      (fn [& args]
        (let [[p children] (if (map? (first args)) [(first args) (rest args)] [{} args])
              p      (merge defaults p)
              cols   (columns-of children)
              n-cols (count cols)
              n-rows (:num-rows p)
              _      (doseq [[k pk] [[:widths :column-widths] [:heights :row-heights]]]
                       (when (not= (get p pk) (get @seen k))
                         (swap! seen assoc k (get p pk))
                         (swap! st assoc k (get p pk))))
              {:keys [widths heights sl stp vw vh drag guide]} @st
              co     (memo cw-cache [n-cols widths (:default-column-width p)
                                     (:min-column-width p) (:max-column-width p)]
                           #(layout/offsets (layout/sizes n-cols widths
                                                          (:default-column-width p)
                                                          (:min-column-width p)
                                                          (:max-column-width p))))
              ro     (memo rh-cache [n-rows heights (:default-row-height p)
                                     (:min-row-height p) (:max-row-height p)]
                           #(layout/offsets (layout/sizes n-rows heights
                                                          (:default-row-height p)
                                                          (:min-row-height p)
                                                          (:max-row-height p))))
              nf     (clamp (:num-frozen-columns p) 0 n-cols)
              nfr    (clamp (:num-frozen-rows p) 0 n-rows)
              geo    {:co co :ro ro :nf nf :nfr nfr :n-rows n-rows :n-cols n-cols
                      :rhw (row-header-width p n-rows) :chh (column-header-height p)
                      :fw (layout/start co nf) :fh (layout/start ro nfr)
                      :tw (layout/total co) :th (layout/total ro)}
              modes  (regions/modes (:selection-modes p))
              ctx    {:p p :cols cols :geo geo :modes modes :drag drag
                      :selected (if (contains? p :selected-regions)
                                  (vec (:selected-regions p))
                                  (:selected @st))
                      :focused  (when (:enable-focused-cell p)
                                  (if (contains? p :focused-cell)
                                    (:focused-cell p)
                                    (:focused @st)))
                      :view {:sl sl :stp stp :vw vw :vh vh
                             :orows (:overscan-rows p) :ocols (:overscan-columns p)}}
              ps     (panes ctx)
              main   (some #(when (and (= 3 (:ra %)) (= 3 (:ca %))) %) ps)]
          (reset! cur (assoc ctx :visible [(:vis-rows main) (:vis-cols main)]))
          [:div (merge (dom-attrs p)
                       {:class (bp/cx (bp/cls "table-container")
                                      {(bp/cls "table-no-rows")    (zero? n-rows)
                                       (bp/cls "table-reordering") (:reordering drag)}
                                      (:class p))
                        :style (merge {:position "relative" :height "100%" :outline "none"}
                                     (:style p))
                        :tab-index 0
                        :ref (fn [el] (reset! container el) nil)
                        :on-mouse-down on-mouse-down
                        :on-double-click on-double-click
                        :on-key-down on-key-down
                        :on-context-menu on-context-menu})
           [:div {:class (bp/cls "table-quadrant-scroll-container")
                  :style {:flex "1 1 auto" :min-height 0 :position "relative"}
                  :ref (fn [el] (reset! scroller el) nil)
                  :on-scroll on-scroll}
            [:div {:style {:display "grid"
                           :grid-template-columns (str (:rhw geo) "px " (:fw geo) "px "
                                                       (- (:tw geo) (:fw geo)) "px")
                           :grid-template-rows (str (:chh geo) "px " (:fh geo) "px "
                                                    (- (:th geo) (:fh geo)) "px")
                           :width (+ (:rhw geo) (:tw geo))
                           :height (+ (:chh geo) (:th geo))}}
             (doall (for [pane ps] (render-pane ctx pane)))]]
           [:div {:class (bp/cx (bp/cls "table-overlay-layer") (bp/cls "table-resize-guides"))}
            (when guide
              (if (= (:axis guide) :col)
                [:div {:class (bp/cx (bp/cls "table-overlay") (bp/cls "table-vertical-guide"))
                       :style {:left (:px guide)}}]
                [:div {:class (bp/cx (bp/cls "table-overlay")
                                     (bp/cls "table-horizontal-guide"))
                       :style {:top (:px guide)}}]))]
           [:div {:class (bp/cx (bp/cls "table-overlay-layer")
                                (bp/cls "table-reordering-cursor-overlay"))}]]))})))
