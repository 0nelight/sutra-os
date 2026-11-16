(ns sutra-ui.apphost.widgets.polygon
  (:require [sutra-ui.ui.sel :as sel :refer [rd wr!]]
            [sutra.kit.coerce :refer [->kw]]
            [sutra-ui.ui.core :as ui]
            [sutra-ui.ui.busy :as busy]
            [sutra-ui.ui.hotkeys :as hotkeys]
            [reagent.core :as r]
            [clojure.string :as str]
            [sutra.capps.polygon :as poly]
            [sutra.capps.table :as tbl]
            [sutra-ui.download :as dl]
            [sutra.i18n.core :as i18n]))

(defn- grob?
  []
  (boolean (when (exists? js/window)
             (some-> js/window (.matchMedia "(pointer: coarse)") .-matches))))

(def ^:private hit-px-fein 12)
(def ^:private hit-px-grob 22)
(defn- hit-px [] (if (grob?) hit-px-grob hit-px-fein))
(defn- griff-r
  [ps closed? u]
  (if (and closed? (seq ps) (pos? u))
    (let [span (fn [f] (let [vs (map f ps)] (- (apply max vs) (apply min vs))))]
      (-> (/ (min (span first) (span second)) u 6) (min 5) (max 2.5)))
    5))
(defn- tap-slop-px [] (if (grob?) 12 6))
(def ^:private zoom-step 1.18)
(def ^:private label-px 11)
(def ^:private glyph-em 0.58)

(defn- px [h] (if (number? h) (str h "px") (or h "420px")))

(defn- extent-dims
  [extent]
  (let [n (fn [v] (let [x (if (string? v) (js/parseFloat v) v)]
                    (when (and (number? x) (not (js/isNaN x)) (pos? x)) (double x))))
        [w h] (if (sequential? extent)
                [(n (first extent)) (n (second extent))]
                [(n (:w extent)) (n (:h extent))])]
    (when (and w h) {:w w :h h})))

(defn- grid-step [m]
  (let [raw (/ m 10)
        p   (Math/pow 10 (Math/floor (Math/log10 raw)))
        f   (/ raw p)]
    (* p (cond (< f 2) 1 (< f 5) 2 :else 5))))

(defn- plan-grid [{:keys [w h]}]
  (let [step (grid-step (max w h))]
    (into [:g.capp-polygon__grid]
          (concat
           (for [x (range step w step)]
             ^{:key (str "x" x)}
             [:line {:x1 x :y1 0 :x2 x :y2 h :vector-effect "non-scaling-stroke"}])
           (for [y (range step h step)]
             ^{:key (str "y" y)}
             [:line {:x1 0 :y1 y :x2 w :y2 y :vector-effect "non-scaling-stroke"}])))))

(defn- dist [[ax ay] [bx by]]
  (let [dx (- ax bx) dy (- ay by)]
    (Math/sqrt (+ (* dx dx) (* dy dy)))))

(defn- units-per-px [{:keys [w h]} {:keys [bw bh]}]
  (if (and (pos? bw) (pos? bh) (pos? w) (pos? h))
    (max (/ w bw) (/ h bh))
    1))

(defn- view-box [{:keys [x y w h]}] (str x " " y " " w " " h))

(defn- fit-view [{:keys [w h]}] {:x 0 :y 0 :w w :h h})

(defn- centre-of [{:keys [x y w h]}] [(+ x (/ w 2)) (+ y (/ h 2))])

(defn- zoom-at [{:keys [x y w h] :as v} factor [ax ay] {iw :w ih :h}]
  (let [nw (* w factor)
        nh (* h factor)
        lo (/ (min iw ih) 40)
        hi (* 4 (max iw ih))]
    (if (or (< nw lo) (< nh lo) (> nw hi) (> nh hi))
      v
      {:x (- ax (* (- ax x) factor))
       :y (- ay (* (- ay y) factor))
       :w nw :h nh})))

(defn- pan [v dx dy] (-> v (update :x + dx) (update :y + dy)))

(defn- svg-point [svg cx cy]
  (when-let [m (some-> svg .getScreenCTM)]
    (let [p (.matrixTransform (js/DOMPoint. cx cy) (.inverse m))]
      [(.-x p) (.-y p)])))

(defn- pinch-mid [ps]
  (let [n (count ps)]
    [(/ (reduce + (map :x ps)) n) (/ (reduce + (map :y ps)) n)]))

(defn- pinch-spread [ps]
  (let [[a b] (vec ps)]
    (dist [(:x a) (:y a)] [(:x b) (:y b)])))

(defn- angle-to [[cx cy] [x y]] (Math/atan2 (- y cy) (- x cx)))

(defn- knob-of [{:keys [points] :as poly} u]
  (when (poly/ring? poly)
    (let [c (poly/centroid points)
          r (reduce max 0 (map #(dist c %) points))]
      [(first c) (- (second c) r (* 26 u))])))

(defn- points-attr [points]
  (str/join " " (map (fn [[x y]] (str x "," y)) points)))

(def ^:private hints
  {:kreuzt      "Die Kante würde sich selbst kreuzen — der Tipp zählt nicht."
   :zu-nah      "Zu nah am letzten Punkt."
   :geschlossen "Der Umriss steht. „Neu zeichnen“ beginnt von vorn."
   :leer        "Es gibt keinen Punkt mehr."})

(defn- ring-shape [poly cls]
  (let [ps (:points poly)]
    (cond
      (poly/ring? poly)  [:polygon {:class cls :points (points-attr ps)
                                    :vector-effect "non-scaling-stroke"}]
      (< 1 (count ps)) [:polyline {:class cls :points (points-attr ps) :fill "none"
                                   :vector-effect "non-scaling-stroke"}]
      :else            nil)))

(defn- an?
  [v] (not (false? v)))

(defn- bbox [points]
  (let [xs (map first points) ys (map second points)]
    [(- (apply max xs) (apply min xs))
     (- (apply max ys) (apply min ys))]))

(defn- label-passt?
  [points text u]
  (let [[w h] (bbox points)
        fs    (* label-px u)]
    (and (<= fs h)
         (<= (* glyph-em fs (count text)) w))))

(defonce ^:private !gemeldet
  (atom #{}))

(defn- melde-einmal!
  [k msg]
  (when-not (contains? @!gemeldet k)
    (swap! !gemeldet conj k)
    (js/console.error msg)))

(defn- unzeichenbar!
  [prop raw gelesen]
  (let [punkte (some->> (seq gelesen) (map (comp count distinct :points)) (apply max))]
    (melde-einmal!
     (str prop "|" raw)
     (str "capp-polygon: " (pr-str prop) " ergab keinen zeichenbaren Ring — "
          (if punkte
            (str "der Umriss hat nur " punkte " verschiedene Punkte, eine Fläche braucht "
                 poly/min-points ": ")
            "der Wert ist da, aber weder GeoJSON-Geometrie noch \"x,y x,y …\": ")
          (subs (str raw) 0 (min 160 (count (str raw))))))))

(defn- parse-others
  [rows points-prop label-prop opts]
  (let [pf    (tbl/field-fn points-prop)
        lf    (tbl/field-fn label-prop)
        kf    (:key-fn opts)
        shift (:shift-fn opts)
        rules (:conditional-format opts)
        opts  (dissoc opts :key-fn :shift-fn :conditional-format)]
    (into []
          (mapcat (fn [row]
                    (let [dy      (if shift (shift row) 0)
                          lbl     (when lf (lf row))
                          raw     (when pf (pf row))
                          gelesen (poly/parse-rings raw)
                          ringe   (filter poly/ring? gelesen)
                          look    (when (seq rules)
                                    (tbl/cond-format rules (or (:values row) row)))
                          color   (or (:background look) (:color look))]
                      (when (and (not (str/blank? (str raw))) (empty? ringe))
                        (unzeichenbar! points-prop raw gelesen))
                      (when dy
                        (map-indexed (fn [i poly]
                                       (cond-> (merge opts {:row row
                                                            :poly (poly/translate poly 0 dy)
                                                            :label (when (zero? i) lbl)})
                                         kf    (assoc :key [(kf row) i])
                                         color (assoc :color color)))
                                     ringe)))))
          rows)))

(defn- num [v]
  (let [x (if (string? v) (js/parseFloat v) v)]
    (when (and (number? x) (not (js/isNaN x))) (double x))))

(defn- stack-plan
  [{:keys [stack-key stack-order stack-label stack-w stack-h]} row]
  (let [at (fn [prop] (when-let [f (tbl/field-fn prop)] (f row)))
        w  (num (at stack-w))
        h  (num (at stack-h))]
    (when (and w h (pos? w) (pos? h))
      {:id (str (at stack-key)) :label (some-> (at stack-label) str)
       :order (num (at stack-order)) :w w :h h})))

(defn- stacked
  [{:keys [stack stack-key] :as cfg}]
  (if-let [{:keys [frames extent]}
           (when stack-key (poly/stack-layout (keep #(stack-plan cfg %) stack)))]
    (let [dy (into {} (map (juxt :id :y)) frames)
          kf (tbl/field-fn stack-key)]
      (assoc cfg :extent extent :frames frames :readonly? true
             :shift-fn #(get dy (str (kf %)))))
    cfg))

(defn- nicht-gezeichnet
  [rows points-prop]
  (let [pf (tbl/field-fn points-prop)]
    (count (filter (fn [row]
                     (let [raw (when pf (pf row))]
                       (and (not (str/blank? (str raw)))
                            (empty? (filter poly/ring? (poly/parse-rings raw))))))
                   rows))))

(defn- other-shapes [others pickable? u]
  (into [:g {:class (cond-> "capp-polygon__others"
                      pickable? (str " capp-polygon__others--waehlbar"))}]
        (map-indexed
         (fn [i {:keys [poly label class color labels? tooltips? gewaehlt? geaendert?
                        einzeln? aktiv layer?]}]
           (let [ps  (:points poly)
                 r   (griff-r ps true u)
                 txt (when (some? label) (str label))
                 hat (not (str/blank? txt))]
             [:g {:key i}
              [:polygon (cond-> {:class (cond-> "capp-polygon__other"
                                          class      (str " " class " capp-polygon__other--" class)
                                          layer?     (str " capp-polygon__other--ebene")
                                          geaendert? (str " capp-polygon__other--geaendert")
                                          gewaehlt?  (str " capp-polygon__other--gewaehlt"))
                                 :points (points-attr ps) :vector-effect "non-scaling-stroke"}
                          (and color (not geaendert?) (not gewaehlt?))
                          (assoc :style {:fill color :fill-opacity 0.85 :stroke color}))
               (when (and hat tooltips?) [:title txt])]
              (when gewaehlt?
                (into [:g.capp-polygon__handles]
                      (map-indexed (fn [j [x y]]
                                     [:circle {:key j :cx x :cy y :r (* r u)
                                               :class (cond-> "capp-polygon__vertex"
                                                        (= j aktiv)
                                                        (str " capp-polygon__vertex--aktiv"))
                                               :vector-effect "non-scaling-stroke"}])
                                   ps)))
              (when einzeln?
                (into [:g.capp-polygon__mitten]
                      (map (fn [[idx [x y]]]
                             [:circle {:key (str "m" idx) :cx x :cy y
                                       :r (* (max 2 (- r 1)) u)
                                       :class "capp-polygon__mitte"
                                       :vector-effect "non-scaling-stroke"}])
                           (poly/edge-midpoints poly))))
              (when (and hat labels? (label-passt? ps txt u))
                (let [[cx cy] (poly/centroid ps)]
                  [:text.capp-polygon__other-label
                   {:x cx :y cy :text-anchor "middle" :dominant-baseline "middle"
                    :font-size (* label-px u)}
                   txt]))]))
         others)))

(defn- upp [{:keys [!view !box]}]
  (units-per-px (or @!view {:w 1 :h 1}) @!box))

(defn- tol [s] (* (hit-px) (upp s)))

(defn- at [{:keys [!svg]} e] (svg-point @!svg (.-clientX e) (.-clientY e)))

(defn- commit! [{:keys [!cfg !poly]} next]
  (let [{:keys [sel pick on-close]} @!cfg
        prev @!poly]
    (reset! !poly next)
    (wr! sel (when (poly/ring? next)
               (poly/->str next (or (->kw pick) :points))))
    (when (and on-close (poly/ring? next) (not (poly/ring? prev)))
      (on-close nil))))

(defn- measure! [{:keys [!svg !box]}]
  (when-let [el @!svg]
    (reset! !box {:bw (.-clientWidth el) :bh (.-clientHeight el)})))

(defn- show-image! [{:keys [!img !view]} d]
  (reset! !img d)
  (reset! !view (fit-view d)))

(defn- load! [{:keys [!src !img] :as s} {:keys [src extent]}]
  (reset! !src [src extent])
  (reset! !img nil)
  (if (str/blank? (str src))
    (some->> (extent-dims extent) (show-image! s))
    (let [img (js/Image.)]
      (set! (.-onload img)
            #(show-image! s {:w (.-naturalWidth img) :h (.-naturalHeight img)}))
      (set! (.-src img) (str src)))))

(defn- wheel! [{:keys [!view !img] :as s} e]
  (.preventDefault e)
  (when-let [a (at s e)]
    (swap! !view zoom-at (if (pos? (.-deltaY e)) zoom-step (/ 1 zoom-step)) a @!img)))

(defn- attach! [{:keys [!svg !ro on-wheel] :as s} el]
  (cond
    (and el (not= el @!svg))
    (do (reset! !svg el)
        (.addEventListener el "wheel" on-wheel #js {:passive false})
        (when (and (exists? js/ResizeObserver) (nil? @!ro))
          (reset! !ro (js/ResizeObserver. #(measure! s))))
        (some-> @!ro (.observe el))
        (measure! s))

    (and (nil? el) @!svg)
    (do (.removeEventListener @!svg "wheel" on-wheel)
        (some-> @!ro .disconnect)
        (reset! !svg nil))))

(declare key-actions)

(defn- make-state
  [initial]
  (let [s {:!poly    (r/atom (poly/parse (rd (:sel initial))))
           :!view    (r/atom nil)
           :!img     (r/atom nil)
           :!box     (r/atom {:bw 0 :bh 0})
           :!svg     (atom nil)
           :!ptrs    (atom {})
           :!pinch   (atom nil)
           :!frozen  (r/atom false)
           :!drag    (atom nil)
           :!hover   (r/atom nil)
           :!cfg     (atom initial)
           :!src     (atom nil)
           :!ro      (atom nil)
           :!file    (atom nil)
           :!queue   (r/atom nil)
           :!edits   (r/atom {})
           :!picked  (r/atom #{})
           :!saving  (r/atom nil)
           :!gelesen (r/atom nil)
           :!vertex  (r/atom nil)
           :!history (r/atom ())
           :!future  (r/atom ())
           :!before  (atom nil)
           :!ctx     (atom nil)}
        s (assoc s :on-wheel #(wheel! s %))]
    (assoc s
           :attach!  #(attach! s %)
           :key-cfgs (vec (hotkeys/canvas #(deref (:!svg s)) (key-actions s))))))

(defn- snapshot [{:keys [!edits !poly]}] [@!edits @!poly])

(defn- remember! [{:keys [!history !future]} snap]
  (swap! !history conj snap)
  (reset! !future ()))

(defn- restore! [{:keys [!edits !poly !vertex] :as s} [edits p]]
  (reset! !edits edits)
  (reset! !vertex nil)
  (when (not= p @!poly) (commit! s p)))

(defn- step-back! [s from to]
  (when-let [snap (first @from)]
    (swap! to conj (snapshot s))
    (swap! from rest)
    (restore! s snap)
    true))

(defn- shape-opts [class labels? tooltips? rules]
  {:class class :labels? (an? labels?) :tooltips? (an? tooltips?) :conditional-format rules})

(defn- layer-rows
  [{:keys [rows except except-key]}]
  (if (str/blank? (str except))
    rows
    (let [kf (tbl/field-fn (or except-key :id))]
      (remove #(= (str except) (str (kf %))) rows))))

(defn- parsed-shapes
  [{:keys [shapes shape-points shape-label shape-labels? shape-tooltips? layers shift-fn
           conditional-format]}
   key-fn]
  (into (into []
              (mapcat (fn [{:keys [points label class labels? tooltips?] :as layer}]
                        (parse-others (layer-rows layer) points label
                                      (cond-> (assoc (shape-opts class labels? tooltips?
                                                                 (:conditional-format layer))
                                                     :layer? true)
                                        shift-fn (assoc :shift-fn shift-fn)))))
              layers)
        (parse-others shapes shape-points shape-label
                      (cond-> (shape-opts nil shape-labels? shape-tooltips? conditional-format)
                        key-fn   (assoc :key-fn key-fn)
                        shift-fn (assoc :shift-fn shift-fn)))))

(defn- with-edits [others edits]
  (if (seq edits)
    (mapv (fn [o]
            (if-let [ps (get edits (:key o))]
              (assoc o :poly (poly/state ps true) :geaendert? true)
              o))
          others)
    others))

(defn- with-selection [others picked aktiv]
  (if (seq picked)
    (let [allein? (= 1 (count picked))]
      (mapv (fn [o]
              (let [k (:key o)]
                (cond-> o
                  (contains? picked k)                 (assoc :gewaehlt? true)
                  (and allein? (contains? picked k))   (assoc :einzeln? true)
                  (= k (:key aktiv))                   (assoc :aktiv (:index aktiv)))))
            others))
    others))

(defn- lost-count [{:keys [shapes shape-points layers]}]
  (+ (nicht-gezeichnet shapes shape-points)
     (reduce + 0 (map #(nicht-gezeichnet (:rows %) (:points %)) layers))))

(defn- diagnosis [{:keys [shapes shape-points layers]} img]
  (str "shapes " (count shapes) " Zeile(n) über " (pr-str shape-points)
       (apply str (map-indexed (fn [i l]
                                 (str " · Ebene " (inc i) " " (count (:rows l))
                                      " Zeile(n) über " (pr-str (:points l))))
                               layers))
       " · Rahmen " (if img (str (:w img) "×" (:h img)) "FEHLT")))

(defn- plan-of [img others poly]
  {:extent (or img {:w 1000 :h 1000})
   :title  "Grundriss"
   :rings  (cond-> (mapv (fn [o] {:label (:label o) :points (:points (:poly o))}) others)
             (poly/ring? poly) (conj {:label nil :points (:points poly)}))})

(defn- cur-pts
  [{:keys [!edits]} {:keys [others]} k]
  (or (get @!edits k)
      (some (fn [o] (when (= k (:key o)) (:points (:poly o)))) others)))

(defn- export! [{:keys [img others poly]}]
  (dl/text! (poly/plan->svg (plan-of img others poly))
            "image/svg+xml;charset=utf-8" "grundriss.svg"))

(defn- seed! [{:keys [!queue] :as s} q]
  (reset! !queue (seq q))
  (when-let [r (first q)]
    (commit! s (poly/state (:points r) true))))

(defn- uebernehmen!
  [{:keys [!edits !picked !gelesen] :as s} {:keys [others]} rings]
  (let [{:keys [matched ambiguous unmatched]} (poly/match-rings rings others)]
    (when (seq matched) (remember! s (snapshot s)))
    (swap! !edits into matched)
    (swap! !picked into (map first) matched)
    (reset! !gelesen {:zugeordnet (count matched) :mehrdeutig ambiguous})
    unmatched))

(defn- import-svg! [{:keys [!cfg] :as s} {:keys [save?] :as ctx} text]
  (let [{:keys [import on-import]} @!cfg
        plan  (poly/svg->plan text)
        rs    (vec (:rings plan))
        rest* (if save? (uebernehmen! s ctx rs) rs)]
    (wr! import (poly/plan->json plan))
    (when (seq rest*) (seed! s rest*))
    (when on-import (on-import nil))))

(defn- import! [s ctx e]
  (let [input (.-target e)]
    (when-let [f (aget (.-files input) 0)]
      (let [rdr (js/FileReader.)]
        (set! (.-onload rdr) #(import-svg! s ctx (.-result rdr)))
        (.readAsText rdr f)))
    (set! (.-value input) "")))

(defn- verwerfen! [{:keys [!edits !picked !saving !gelesen !vertex !history !future]}]
  (reset! !history ())
  (reset! !future ())
  (reset! !edits {})
  (reset! !picked #{})
  (reset! !saving nil)
  (reset! !gelesen nil)
  (reset! !vertex nil))

(defn- punkt-weg! [{:keys [!vertex !edits] :as s} ctx]
  (when-let [{:keys [key index]} @!vertex]
    (when-let [cur (cur-pts s ctx key)]
      (let [neu (poly/drop-point (poly/state cur true) index)]
        (when-not (= :zu-wenig (:ignored neu))
          (remember! s (snapshot s))
          (swap! !edits assoc key (:points neu))
          (reset! !vertex nil)
          true)))))

(defn- changed-rows [others edits pick]
  (vec (keep (fn [o]
               (when-let [ps (get edits (:key o))]
                 {:row    (:row o)
                  :label  (:label o)
                  :points (poly/->str {:points ps} (or (->kw pick) :points))}))
             others)))

(defn- speichern! [{:keys [!cfg !edits !saving] :as s} {:keys [others]}]
  (let [{:keys [pick on-save]} @!cfg
        aendern (changed-rows others @!edits pick)]
    (when (and on-save (seq aendern))
      (reset! !saving {:offen (count aendern)})
      (on-save aendern
               (fn [fehler]
                 (if (seq fehler)
                   (reset! !saving {:fehler (vec fehler)})
                   (verwerfen! s)))))))

(defn- entfernen! [{:keys [!cfg !picked !saving] :as s} {:keys [others]}]
  (let [{:keys [on-remove]} @!cfg
        k (first @!picked)
        o (some #(when (= k (:key %)) %) others)]
    (when (and on-remove o)
      (reset! !saving {:entfernt (:label o)})
      (on-remove (:row o)
                 (fn [fehler]
                   (if fehler
                     (reset! !saving {:entfernen-fehler fehler})
                     (verwerfen! s)))))))

(defn- grip
  [poly p t knob]
  (let [v      (poly/vertex-at poly p t)
        in?    (poly/inside? poly p)
        klein? (some #(> (* 4 t) %) (bbox (:points poly)))]
    (cond
      (and knob (>= t (dist p knob)))
      {:mode :rotate :angle (angle-to (poly/centroid (:points poly)) p)}

      (and in? klein?)    {:mode :move :from p}
      v                   {:mode :vertex :index v}
      in?                 {:mode :move :from p}
      :else               nil)))

(defn- midpoint-at [poly p t]
  (some (fn [[idx m]] (when (>= t (dist p m)) idx))
        (poly/edge-midpoints poly)))

(defn- grab-vertex! [{:keys [!vertex !drag]} k idx]
  (reset! !vertex {:key k :index idx})
  (reset! !drag {:mode :vertex-picked :key k :index idx}))

(defn- grab-picked!
  [{:keys [!picked !edits !drag] :as s} {:keys [others] :as ctx} p]
  (let [sel   (filterv #(contains? @!picked (:key %)) others)
        one   (when (= 1 (count sel)) (first sel))
        t     (tol s)
        v     (when one (poly/vertex-at (:poly one) p t))
        mitte (when (and one (nil? v)) (midpoint-at (:poly one) p t))]
    (cond
      v
      (grab-vertex! s (:key one) v)

      mitte
      (let [k (:key one)]
        (swap! !edits assoc k
               (:points (poly/insert-point (poly/state (cur-pts s ctx k) true) mitte p)))
        (grab-vertex! s k mitte))

      (some #(poly/inside? (:poly %) p) sel)
      (reset! !drag {:mode :move-picked :from p})

      :else
      (reset! !drag nil))))

(defn- down! [{:keys [!svg !ptrs !frozen !drag !pinch !poly !picked !before key-cfgs] :as s}
              {:keys [readonly? save? knob] :as ctx} e]
  (hotkeys/claim! s key-cfgs)
  (when-let [p (at s e)]
    (let [id (.-pointerId e)
          poly @!poly]
      (when (empty? @!ptrs) (reset! !before (snapshot s)))
      (try (some-> @!svg (.setPointerCapture id)) (catch :default _ nil))
      (swap! !ptrs assoc id {:x (.-clientX e) :y (.-clientY e) :moved 0})
      (if (<= 2 (count @!ptrs))
        (do (reset! !frozen true) (reset! !drag nil) (reset! !pinch nil))
        (do (when (and (not readonly?) (poly/ring? poly))
              (reset! !drag (grip poly p (tol s) knob)))
            (when (and save? (nil? @!drag) (seq @!picked))
              (grab-picked! s ctx p)))))))

(defn- track-pointer! [!ptrs e]
  (let [id (.-pointerId e)
        cx (.-clientX e)
        cy (.-clientY e)]
    (when-let [was (get @!ptrs id)]
      (swap! !ptrs assoc id
             (assoc was :x cx :y cy
                    :moved (+ (:moved was)
                              (Math/abs (- cx (:x was)))
                              (Math/abs (- cy (:y was)))))))))

(defn- pinch! [{:keys [!svg !pinch !view !img]} two]
  (let [[mx my] (pinch-mid two)
        d       (pinch-spread two)
        prev    @!pinch]
    (when-let [now (svg-point @!svg mx my)]
      (when prev
        (when (and (pos? (:d prev)) (pos? d))
          (swap! !view zoom-at (/ (:d prev) d) now @!img))
        (when-let [[ox oy] (svg-point @!svg (:x prev) (:y prev))]
          (swap! !view pan (- ox (first now)) (- oy (second now)))))
      (reset! !pinch {:d d :x mx :y my}))))

(defn- translate-picked
  [edits others sel dx dy]
  (reduce (fn [m {k :key :as o}]
            (if (contains? sel k)
              (assoc m k (mapv (fn [[x y]] [(+ x dx) (+ y dy)])
                               (or (get m k) (:points (:poly o)))))
              m))
          edits others))

(defn- drag! [{:keys [!drag !edits !picked] :as s} {:keys [others] :as ctx} poly p]
  (let [{:keys [mode key index from angle]} @!drag
        [dx dy] (when from [(- (first p) (first from)) (- (second p) (second from))])]
    (case mode
      :vertex
      (commit! s (poly/move-point poly index p))

      :move
      (do (commit! s (poly/translate poly dx dy))
          (swap! !drag assoc :from p))

      :rotate
      (let [a (angle-to (poly/centroid (:points poly)) p)]
        (commit! s (poly/rotate poly (- a angle)))
        (swap! !drag assoc :angle a))

      :vertex-picked
      (when-let [cur (cur-pts s ctx key)]
        (swap! !edits assoc key (:points (poly/move-point (poly/state cur true) index p))))

      :move-picked
      (do (swap! !edits translate-picked others @!picked dx dy)
          (swap! !drag assoc :from p))

      nil)))

(defn- move! [{:keys [!hover !frozen !ptrs !drag !poly] :as s} ctx e]
  (let [p  (at s e)
        poly @!poly]
    (reset! !hover (when (and p (not @!frozen)) p))
    (track-pointer! !ptrs e)
    (let [ps (vals @!ptrs)]
      (cond
        (<= 2 (count ps))                (pinch! s (take 2 ps))
        (and p (= 1 (count ps)) @!drag)  (drag! s ctx poly p)))))

(defn- toggle [sel k] (if (contains? sel k) (disj sel k) (conj sel k)))

(defn- tap!
  [{:keys [!poly !picked !vertex] :as s} {:keys [others save? readonly? on-shape-click]} p]
  (let [drawing? (and (seq (:points @!poly)) (not (poly/ring? @!poly)))
        shapes   (remove :layer? (rseq others))
        near     (fn []
                   (let [t (tol s)]
                     (some->> shapes
                              (keep #(let [d (poly/distance (:poly %) p)] (when (<= d t) [d %])))
                              seq (apply min-key first) second)))
        hit      (when-not drawing?
                   (or (some #(when (poly/inside? (:poly %) p) %) shapes)
                       (when (or readonly? on-shape-click) (near))))
        pick     (when on-shape-click hit)]
    (cond
      (and save? hit (:key hit))
      (do (swap! !picked toggle (:key hit))
          (reset! !vertex nil)
          (when pick (on-shape-click (:row pick))))

      (and save? (nil? hit) (seq @!picked))
      (do (reset! !picked #{}) (reset! !vertex nil))

      pick      (on-shape-click (:row pick))
      readonly? nil
      :else     (commit! s (poly/tap @!poly p {:tolerance (tol s)})))))

(defn- up! [{:keys [!svg !ptrs !pinch !drag !frozen !before] :as s} ctx e]
  (let [id      (.-pointerId e)
        was     (get @!ptrs id)
        drag?   (some? @!drag)
        frozen? @!frozen]
    (try (some-> @!svg (.releasePointerCapture id)) (catch :default _ nil))
    (swap! !ptrs dissoc id)
    (when (> 2 (count @!ptrs)) (reset! !pinch nil))
    (when (empty? @!ptrs)
      (reset! !drag nil)
      (reset! !frozen false))
    (when (and was (not frozen?) (not drag?) (> (tap-slop-px) (:moved was)))
      (some->> (at s e) (tap! s ctx)))
    (when (empty? @!ptrs)
      (when-let [b @!before]
        (when (not= b (snapshot s)) (remember! s b)))
      (reset! !before nil))))

(defn- drawing? [poly] (and (seq (:points poly)) (not (poly/ring? poly))))

(defn- edit-now! [s f]
  (remember! s (snapshot s))
  (f)
  true)

(defn- cancel!
  [{:keys [!drag !before !vertex !picked !poly] :as s}]
  (cond
    @!drag            (do (reset! !drag nil)
                          (some->> @!before (restore! s))
                          (reset! !before nil)
                          true)
    @!vertex          (do (reset! !vertex nil) true)
    (seq @!picked)    (do (reset! !picked #{}) true)
    (drawing? @!poly) (edit-now! s #(commit! s (poly/clear @!poly)))))

(defn- confirm! [{:keys [!poly] :as s}]
  (let [poly @!poly]
    (when (and (drawing? poly) (<= poly/min-points (count (:points poly))))
      (edit-now! s #(commit! s (poly/tap poly (first (:points poly)) {}))))))

(defn- delete! [{:keys [!poly !vertex !picked !edits !cfg] :as s}
                {:keys [readonly? save?] :as ctx}]
  (cond
    readonly?         nil
    (drawing? @!poly) (edit-now! s #(commit! s (poly/undo @!poly)))
    @!vertex          (punkt-weg! s ctx)
    (and save? (:on-remove @!cfg) (= 1 (count @!picked)) (empty? @!edits))
    (do (entfernen! s ctx) true)))

(defn- nudge! [{:keys [!picked !edits !poly] :as s} {:keys [others readonly? save?]} e]
  (let [[dx dy] (map #(* % (upp s)) (hotkeys/nudge-of e))]
    (cond
      (or readonly? (nil? dx)) nil
      (and save? (seq @!picked))
      (edit-now! s #(swap! !edits translate-picked others @!picked dx dy))
      (poly/ring? @!poly)
      (edit-now! s #(commit! s (poly/translate @!poly dx dy))))))

(defn- zoom! [{:keys [!view !img]} f]
  (swap! !view #(zoom-at % f (centre-of %) @!img))
  true)

(defn- key-actions
  [{:keys [!ctx !history !future !picked !edits !view !img] :as s}]
  (let [ctx #(deref !ctx)]
    {:cancel     (fn [_] (cancel! s))
     :confirm    (fn [_] (confirm! s))
     :delete     (fn [_] (delete! s (ctx)))
     :undo       (fn [_] (step-back! s !history !future))
     :redo       (fn [_] (step-back! s !future !history))
     :select-all (fn [_] (let [{:keys [save? others]} (ctx)]
                           (when (and save? (seq others))
                             (reset! !picked (into #{} (keep :key) others))
                             true)))
     :save       (fn [_] (when (and (:save? (ctx)) (seq @!edits))
                           (speichern! s (ctx))
                           true))
     :zoom-in    (fn [_] (zoom! s (/ 1 zoom-step)))
     :zoom-out   (fn [_] (zoom! s zoom-step))
     :fit        (fn [_] (when @!img (reset! !view (fit-view @!img)) true))
     :nudge      (fn [e] (nudge! s (ctx) e))}))

(defn- non-scaling [attrs] (assoc attrs :vector-effect "non-scaling-stroke"))

(defn- plan-backdrop [img]
  [:g
   [:rect.capp-polygon__plan (non-scaling {:x 0 :y 0 :width (:w img) :height (:h img)})]
   (plan-grid img)])

(defn- stack-backdrop [frames u]
  (into [:g]
        (map (fn [{:keys [id label y] :as f}]
               [:g {:key id :transform (str "translate(0," y ")")}
                (plan-backdrop f)
                (when label
                  [:text.capp-polygon__stack-label
                   {:x 0 :y (* -0.4 label-px u) :font-size (* label-px u)}
                   label])]))
        frames))

(defn- rubber-band [poly hover]
  (let [[lx ly] (peek (:points poly))]
    [:line.capp-polygon__rubber
     (non-scaling {:x1 lx :y1 ly :x2 (first hover) :y2 (second hover)})]))

(defn- own-handles [poly u]
  (let [r (griff-r (:points poly) (poly/ring? poly) u)]
    (into [:g.capp-polygon__handles]
          (map-indexed
           (fn [i [x y]]
             [:circle (non-scaling
                       {:key i :cx x :cy y :r (* r u)
                        :class (cond-> "capp-polygon__vertex"
                                 (and (zero? i) (not (poly/ring? poly)))
                                 (str " capp-polygon__vertex--erst"))})])
           (:points poly)))))

(defn- rotate-knob [poly knob u]
  (let [[cx cy] (poly/centroid (:points poly))
        [kx ky] knob]
    [:g.capp-polygon__knob
     [:line (non-scaling {:x1 cx :y1 cy :x2 kx :y2 ky})]
     [:circle (non-scaling {:cx kx :cy ky :r (* (+ 2 (griff-r (:points poly) true u)) u)})]]))

(defn- canvas [{:keys [!frozen !view !hover attach!] :as s}
               {:keys [cfg img plan? poly u knob others readonly? on-shape-click] :as ctx}]
  (let [hover @!hover]
    [:svg
     {:class               (cond-> "capp-polygon__canvas"
                             readonly? (str " capp-polygon__canvas--readonly")
                             @!frozen (str " capp-polygon__canvas--frozen"))
      :ref                 attach!
      :viewBox             (view-box (or @!view (fit-view img)))
      :preserveAspectRatio "xMidYMid meet"
      :on-pointer-down     #(down! s ctx %)
      :on-pointer-move     #(move! s ctx %)
      :on-pointer-up       #(up! s ctx %)
      :on-pointer-cancel   #(up! s ctx %)
      :on-pointer-leave    #(reset! !hover nil)}
     (cond
       (and plan? (seq (:frames cfg))) (stack-backdrop (:frames cfg) u)
       plan? (plan-backdrop img)
       :else
       [:image {:href (str (:src cfg)) :x 0 :y 0 :width (:w img) :height (:h img)}])
     (when (seq others) (other-shapes others (boolean on-shape-click) u))
     (ring-shape poly (cond-> "capp-polygon__ring"
                        (not (poly/simple? poly)) (str " capp-polygon__ring--kreuzt")))
     (when (and hover (not readonly?) (not (poly/ring? poly)) (seq (:points poly)))
       (rubber-band poly hover))
     (when-not readonly? (own-handles poly u))
     (when knob (rotate-knob poly knob u))]))

(defn- stage [s {:keys [cfg img plan?] :as ctx}]
  [:div.capp-polygon__stage
   {:style (if img
             {:aspect-ratio (str (:w img) " / " (:h img)) :max-height (px (:height cfg))}
             {:height (px (:height cfg))})}
   (cond
     (and plan? (nil? (extent-dims (:extent cfg))))
     [:div.capp-polygon__empty "kein Grundriss hinterlegt"]

     (nil? img)
     [busy/block {:class "capp-polygon__empty"} "Grundriss wird geladen…"]

     :else
     (canvas s ctx))])

(defn- button
  ([label on-click] (button label on-click {}))
  ([label on-click attrs]
   [ui/button (merge {:on-click on-click} attrs) label]))

(defn- draw-buttons [{:keys [!poly] :as s} poly]
  [:<>
   (button (if (poly/ring? poly) "Umriss öffnen" "Punkt zurück") #(commit! s (poly/undo @!poly)))
   (button (if (poly/ring? poly) "Neu zeichnen" "Verwerfen") #(commit! s (poly/clear @!poly)))])

(defn- view-buttons [{:keys [!view !img]} ctx]
  (let [zoom! (fn [f] (swap! !view #(zoom-at % f (centre-of %) @!img)))]
    [:<>
     (button "+" #(zoom! (/ 1 zoom-step)))
     (button "−" #(zoom! zoom-step))
     (button "Ganzer Plan" #(reset! !view (fit-view @!img)))
     (button "SVG" #(export! ctx))]))

(defn- import-buttons [{:keys [!file] :as s} ctx]
  [:<>
   (button "SVG laden" #(some-> @!file .click))
   [:input {:type "file" :accept ".svg,image/svg+xml"
            :ref #(reset! !file %)
            :style {:display "none"}
            :on-change #(import! s ctx %)}]])

(defn- drop-point-button [{:keys [!vertex] :as s} ctx]
  (let [n (some-> (cur-pts s ctx (:key @!vertex)) count)]
    (button (if (and n (>= poly/min-points n)) "Dreieck — kein Punkt mehr" "Punkt löschen")
            #(punkt-weg! s ctx)
            {:disabled (not (and n (< poly/min-points n)))})))

(defn- save-label [n [eins viele]]
  (if eins
    (i18n/t :ui/capp-save-shapes {:n n :name (if (= 1 n) eins (or viele eins))})
    (str n (if (= 1 n) " Form speichern" " Formen speichern"))))

(defn- save-buttons [{:keys [!saving !edits] :as s} {:keys [cfg] :as ctx}]
  (let [offen? (boolean (:offen @!saving))]
    [:<>
     [ui/button
      {:intent :success :loading offen? :on-click #(speichern! s ctx)}
      (if offen? "…" (save-label (count @!edits) (:shape-noun cfg)))]
     (button "Änderungen verwerfen" #(verwerfen! s) {:disabled offen?})]))

(defn- remove-button [{:keys [!saving] :as s} ctx]
  (let [laeuft? (contains? @!saving :entfernt)]
    [ui/button
     {:intent :danger :minimal true :loading laeuft? :on-click #(entfernen! s ctx)}
     "Form entfernen"]))

(defn- queue-hint [s q]
  [:<>
   [:span.capp-polygon__hint
    (str (count q) " gelesene Umrisse offen"
         (when-let [l (:label (first q))] (str " · „" l "“")))]
   (button "nächster Umriss" #(seed! s (vec (rest q))))])

(defn- drawing-hint [poly]
  (let [n (count (:points poly))]
    (cond
      (poly/ring? poly)           (str n " Punkte · Punkt ziehen, Umriss schieben, am Griff drehen")
      (get hints (:ignored poly)) (get hints (:ignored poly))
      (zero? n)                 "In den Grundriss tippen setzt den ersten Punkt"
      (<= poly/min-points n)    "Auf den ersten Punkt tippen schließt den Umriss"
      :else                     (str n " Punkte"))))

(defn- error-hint [text] [:span.capp-polygon__hint.capp-polygon__hint--fehler text])

(defn- nothing-drawn-hint [cfg img]
  (let [befund (diagnosis cfg img)]
    (melde-einmal! (str "leer|" befund) (str "capp-polygon: nichts gezeichnet — " befund))
    [ui/section
     {:title (i18n/t :ui/capp-no-shapes) :collapsible true :compact true
      :collapse-props {:default-is-open false} :class "capp-polygon__hint"}
     [ui/section-card
      befund]]))

(defn- lost-hint [n]
  (error-hint (str "⚠ " n " " (if (= 1 n) "Form wurde" "Formen wurden")
                   " nicht gezeichnet — der Wert steht da, ergibt aber keinen Ring"
                   " (Einzelheiten in der Browser-Konsole)")))

(defn- matched-hint [{:keys [zugeordnet mehrdeutig]}]
  [:span {:class (cond-> "capp-polygon__hint"
                   (zero? zugeordnet) (str " capp-polygon__hint--fehler"))}
   (str zugeordnet " Umrisse zugeordnet"
        (when (pos? mehrdeutig)
          (str " · " mehrdeutig " übersprungen: der Name steht auf mehr als einer Form")))])

(defn- picked-hint [n]
  [:span.capp-polygon__hint
   (str n " gewählt · ziehen verschiebt "
        (if (= 1 n)
          "die Form, an einer Ecke ziehen zeichnet sie um"
          "alle gewählten zusammen"))])

(defn- bar [{:keys [!edits !queue !vertex !saving !gelesen !picked] :as s}
            {:keys [cfg poly img others readonly? save?] :as ctx}]
  (let [verloren (lost-count cfg)]
    [:div.capp-polygon__bar
     (when (and (not readonly?) (seq (:points poly))) (draw-buttons s poly))
     (view-buttons s ctx)
     (when (and (:import cfg) (not readonly?)) (import-buttons s ctx))
     (when (and save? @!vertex) (drop-point-button s ctx))
     (when (and save? (seq @!edits)) (save-buttons s ctx))
     (when (and save? (:on-remove cfg) (= 1 (count @!picked)) (empty? @!edits))
       (remove-button s ctx))
     (when-let [q (seq @!queue)] (queue-hint s q))
     (cond
       (not readonly?)       [:span.capp-polygon__hint (drawing-hint poly)]
       (:on-shape-click ctx) [:span.capp-polygon__hint "Eine Form antippen öffnet sie"])
     (when (and (empty? others) (not (seq (:points poly)))) (nothing-drawn-hint cfg img))
     (when (pos? verloren) (lost-hint verloren))
     (when-let [f (seq (:fehler @!saving))]
       (error-hint (str "⚠ " (count f) " von " (count @!edits) " nicht gespeichert — " (first f))))
     (when-let [f (:entfernen-fehler @!saving)]
       (error-hint (str "⚠ Form nicht entfernt — " f)))
     (when-let [g @!gelesen] (matched-hint g))
     (when (and save? (seq @!picked) (empty? @!edits)) (picked-hint (count @!picked)))]))

(defn- render-ctx
  [{:keys [!img !poly !edits !picked !vertex] :as s}
   {:keys [readonly? on-save on-shape-click shape-id] :as cfg}]
  (let [plan? (str/blank? (str (:src cfg)))
        img   (or @!img (when plan? (extent-dims (:extent cfg))))
        poly    (if readonly? (poly/parse (rd (:sel cfg))) @!poly)
        save? (boolean (and on-save (not readonly?)))
        u     (upp s)
        edits @!edits]
    {:cfg            cfg
     :plan?          plan?
     :img            img
     :poly             poly
     :u              u
     :readonly?      readonly?
     :save?          save?
     :on-shape-click on-shape-click
     :knob           (when-not readonly? (knob-of poly u))
     :others         (-> (parsed-shapes cfg (when save? (tbl/field-fn (or shape-id :id))))
                         (with-edits edits)
                         (with-selection @!picked @!vertex))}))

(defn polygon [initial]
  (let [{:keys [!cfg !src attach!] :as s} (make-state initial)]
    (r/create-class
     {:display-name "capp-polygon"
      :component-did-mount
      (fn [_]
        (load! s @!cfg)
        (hotkeys/offer! s (:key-cfgs s)))
      :component-did-update
      (fn [_ _]
        (let [{:keys [src extent]} @!cfg]
          (when (not= @!src [src extent]) (load! s @!cfg))))
      :component-will-unmount
      (fn [_]
        (hotkeys/release! s)
        (attach! nil))
      :reagent-render
      (fn [cfg]
        (let [cfg (stacked cfg)]
          (reset! !cfg cfg)
          (let [ctx (render-ctx s cfg)]
            (reset! (:!ctx s) ctx)
            [:div {:class (cond-> "capp-polygon" (:class cfg) (str " " (:class cfg)))}
             (stage s ctx)
             (bar s ctx)])))})))
