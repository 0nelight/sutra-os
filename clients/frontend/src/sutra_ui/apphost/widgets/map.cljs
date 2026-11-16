(ns sutra-ui.apphost.widgets.map
  (:require [sutra-ui.ui.sel :as sel :refer [rd wr!]]
            [clojure.string :as str]
            [sutra-ui.ui.core :as ui :refer [checkbox html-select menu menu-item
                                             segmented-control numeric-input slider text-area]]
            [goog.object :as gobj]
            [reagent.core :as r]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.capps.map-geo :as geo]
            [sutra.capps.map-spec :as ms]
            [sutra.capps.plan-faces :as pf]
            [sutra.capps.polygon :as poly]
            [sutra-ui.download :as dl]
            [sutra-ui.ui.hotkeys :as hotkeys]
            [sutra-ui.object.card :as oc]
            [sutra-ui.ui.vendor :as vendor]
            [sutra-ui.apphost.frame :as fr]))

(def ^:private hit-px 10)

(defonce ^:private !lib (atom nil))

(defn- ^js make [k & args]
  (let [C (aget @!lib k)] (case (count args) 0 (new C) (new C (first args)))))

(defn- r6 [x] (/ (js/Math.round (* x 1e6)) 1e6))
(defn- r2 [x] (/ (js/Math.round (* x 100)) 100))

(defn- coarse? []
  (boolean (some-> js/window (.matchMedia "(pointer: coarse)") .-matches)))

(defn- crs [cfg] (ms/crs-of cfg))

(defn- extent-dims
  [extent]
  (let [n (fn [v] (let [x (if (string? v) (js/parseFloat v) v)]
                    (when (and (number? x) (not (js/isNaN x)) (pos? x)) (double x))))
        [w h] (if (sequential? extent)
                [(n (first extent)) (n (second extent))]
                [(n (:w extent)) (n (:h extent))])]
    (when (and w h) {:w w :h h})))

(defn- plan-of
  [{:keys [extent src]} st]
  (when-let [{:keys [w h]} (or (extent-dims extent) (:img-size st))]
    {:w w :h h :src (when-not (str/blank? (str src)) (str src))}))

(defn- unit-of
  [{:keys [unit extent src] :as cfg}]
  (or unit (if (and (= :plan (crs cfg)) (not (extent-dims extent)) src) "px" "m")))

(defn- measurements
  [{:keys [measure-perimeter? measure-area? measure-lines? measure-segments?]}]
  (when (or measure-perimeter? measure-area? measure-lines?)
    {:perimeter? measure-perimeter? :area? measure-area?
     :lines? measure-lines? :segments? measure-segments?}))

(defn- shape-kind [{:keys [shape-output]}] (or (->kw shape-output) :feature))

(defn- output-of
  [cfg x]
  (or (->kw (:shape-output x)) (shape-kind cfg)))

(defn- edit-layer
  [cfg]
  (first (filter :edit-action (ms/layers-of cfg))))

(defn- pick-string
  [kind lng lat]
  (if (= :geojson (->kw kind))
    (str "{\"type\":\"Point\",\"coordinates\":[" lng "," lat "]}")
    (str lat "," lng)))

(defn- picked-of
  [{:keys [sel pick]}]
  (when (and sel pick)
    (when-let [[lon lat] (geo/parse-point (rd sel))] {:lat lat :lon lon})))

(defn- write-shapes!
  [{:keys [drawn] :as cfg} st]
  (when drawn
    (let [text (ms/shapes-text (:shapes st) (shape-kind cfg))]
      (wr! drawn text))))

(defn- write-selected-shapes!
  [{:keys [selected-shapes] :as cfg} {:keys [shapes] :as st}]
  (when selected-shapes
    (let [picked (keep-indexed (fn [i s] (when (contains? (:selected-shapes st) i) s)) shapes)]
      (wr! selected-shapes (ms/shapes-text picked (shape-kind cfg))))))

(defn- fire!
  [ev env & [row]]
  (when (seq ev)
    (when-let [f (some-> (:event-fn env) (apply [ev]))] (f row))))

(defn- last-call
  [f]
  (let [memo (atom nil)]
    (fn [& args]
      (let [[k v] @memo]
        (if (= k args)
          v
          (let [v (apply f args)] (reset! memo [args v]) v))))))

(def ^:private reference-rings (last-call ms/reference-rings))

(def ^:private wall-segments (last-call #(pf/ring-edges (ms/reference-rings %1 %2))))

(def ^:private plan-areas (last-call ms/plan-areas))

(def ^:private start-segments
  (last-call (fn [plan cfg shapes]
               (pf/ring-edges (cond-> (ms/reference-rings cfg shapes)
                                plan (conj (let [{:keys [w h]} plan]
                                             [[0 0] [w 0] [w h] [0 h] [0 0]])))))))

(defn- step-opts [cfg st extra]
  (let [plan? (= :plan (crs cfg))
        tool  (:tool st)]
    (merge {:crs (crs cfg) :single-draw? (:single-draw? cfg) :antimeridian? (:antimeridian? cfg)
            :tol (:tol st)}
           (when (and plan? (not (:snap-off? st)) (#{:line :polygon :wall} tool))
             {:snap-dirs (reference-rings cfg (:shapes st))})
           (when (and plan? (not (:snap-off? st)) (#{:line :polygon} tool) (empty? (:draft st)))
             {:start-segs (start-segments (plan-of cfg st) cfg (:shapes st))})
           (when (and plan? (= :wall tool))
             {:plan-segs (wall-segments cfg (:shapes st))})
           extra)))

(defn- step!
  ([!st cfg env event] (step! !st cfg env event nil))
  ([!st cfg env event extra]
   (let [st (swap! !st #(ms/step % event (step-opts cfg % extra)))]
     (doseq [e (:effects st)]
       (case e
         :drawn             (do (write-shapes! cfg st) (fire! (:on-drawn cfg) env)
                                (when (= :plan (crs cfg))
                                  (step! !st cfg env [:select-shape (dec (count (:shapes st)))])))
         :shapes-changed    (do (write-shapes! cfg st) (write-selected-shapes! cfg st))
         :selection-changed (write-selected-shapes! cfg st)
         nil)))))

(defn- selections-of
  [cfg st]
  (into {} (for [l (ms/layers-of cfg)]
             [(::ms/i l) (if (:selected l)
                           (vec (rd (:selected l)))
                           (get-in st [:selections (::ms/i l)] []))])))

(defn- set-selection!
  [!st layer rows]
  (if-let [h (:selected layer)]
    (wr! h rows)
    (swap! !st assoc-in [:selections (::ms/i layer)] rows)))

(defn- clear-selections!
  [!st cfg]
  (doseq [l (ms/layers-of cfg)] (set-selection! !st l [])))

(defn- select-row!
  [!st cfg env layer row add?]
  (let [have (get (selections-of cfg @!st) (::ms/i layer))]
    (set-selection! !st layer (cond
                                (some #{row} have) (vec (remove #{row} have))
                                add?               (conj have row)
                                :else              [row]))
    (fire! (:on-select layer) env row)))

(defn- row-index [layer row] (.indexOf (to-array (:rows layer)) row))

(defn- obj-key [layer row] [(::ms/i layer) (ms/row-key row (row-index layer row))])

(defn- row-of [cfg [li k]]
  (when-let [l (get (ms/layers-of cfg) li)]
    (first (keep-indexed (fn [i r] (when (= k (ms/row-key r i)) r)) (:rows l)))))

(defn- point-geometry? [g] (contains? #{nil :point} (->kw (:geometry g))))

(defn- object-shape
  [cfg layer row]
  (let [gs (:geometries layer)]
    (if-let [g (first (filter #(= :shape (->kw (:geometry %))) gs))]
      (first (ms/row-shapes (crs cfg) (get-in row [(:shape g)]
                                              (get-in row [:values (:shape g)]))))
      (when-let [g (first (filter point-geometry? gs))]
        (when-let [p (ms/point-of g row)] {:type "Point" :coordinates p})))))

(defn- start-edit!
  [!st cfg env rows layer]
  (let [m (into {} (keep (fn [row] (when-let [g (object-shape cfg layer row)]
                                     [(obj-key layer row) g])))
                rows)]
    (when (seq m) (step! !st cfg env [:edit-objects m]))))

(defn- row-id [row id-prop] (or (get row id-prop) (get-in row [:values id-prop]) (:id row)))

(defn- submit-shape!
  [cfg env [li _ :as k] g {:keys [on-ok on-error]}]
  (let [layer (get (ms/layers-of cfg) li)
        {action :edit-action object-param :edit-object-param
         shape-param :edit-shape-param label-param :edit-label-param
         id-prop :edit-id-prop} layer
        row   (row-of cfg k)
        label (some-> (first (keep :label (:geometries layer))) (as-> p (get row p)))]
    ((:submit! env) action
                    (cond-> {(->kw (or object-param :object))
                             (row-id row (->kw (or id-prop :id)))
                             (->kw (or shape-param :shape)) (ms/shape-text g (output-of cfg layer))}
                      label-param (assoc (->kw label-param) label))
                    (fr/kept on-ok on-error))))

(defn- save-edits!
  [!st cfg env]
  (let [changed (ms/changed-edits @!st)]
    (when (and (:submit! env) (seq changed))
      (swap! !st assoc :saving {:open (count changed)})
      (let [todo (vec changed)
            errs (atom [])
            done (atom [])]
        ((fn run [i]
           (if (= i (count todo))
             (do (swap! !st assoc :saving (when (seq @errs) {:errors @errs}))
                 (step! !st cfg env [:edits-saved @done]))
             (let [[k g] (nth todo i)]
               (submit-shape! cfg env k g
                              {:on-ok    (fn [_] (swap! done conj k) (run (inc i)))
                               :on-error (fn [m] (swap! errs conj m) (run (inc i)))}))))
         0)))))

(defn- export-svg!
  [cfg st]
  (let [c     (crs cfg)
        plan  (plan-of cfg st)
        ring  (fn [g] (when (= "Polygon" (:type g)) (geo/open-ring (first (:coordinates g)))))
        rows  (for [l (ms/layers-of cfg)
                    g (:geometries l) :when (= :shape (->kw (:geometry g)))
                    row (:rows l)
                    s (ms/row-shapes c (or (get row (:shape g)) (get-in row [:values (:shape g)])))
                    :let [pts (ring s)] :when pts]
                {:label (some->> (:label g) (get row)) :points pts})
        drawn (keep (fn [s] (when-let [pts (ring s)] {:label (:label (meta s)) :points pts}))
                    (:shapes st))]
    (dl/text! (poly/plan->svg {:extent plan :title (or (:title cfg) "Grundriss")
                               :rings (concat rows drawn)})
              "image/svg+xml;charset=utf-8" "grundriss.svg")))

(defn- polygon-of [points] {:type "Polygon" :coordinates [(conj (vec points) (first points))]})

(defn- import-svg!
  [!st cfg env text]
  (let [plan   (poly/svg->plan text)
        rings  (vec (:rings plan))
        layer  (edit-layer cfg)
        lbl    (some :label (:geometries layer))
        objs   (when layer
                 (keep (fn [row] {:key (obj-key layer row)
                                  :label (or (get row lbl) (get-in row [:values lbl]))
                                  :row row})
                       (:rows layer)))
        {:keys [matched ambiguous unmatched]} (poly/match-rings rings objs)
        by-key (into {} (map (juxt :key :row)) objs)]
    (step! !st cfg env
           [:import {:matched   (into {} (map (fn [[k pts]] [k (polygon-of pts)])) matched)
                     :originals (into {} (keep (fn [[k _]]
                                                 (when-let [g (object-shape cfg layer (by-key k))]
                                                   [k g])))
                                      matched)
                     :unmatched (mapv (fn [{:keys [label points]}]
                                        (with-meta (polygon-of points) {:label label}))
                                      unmatched)}])
    (swap! !st assoc :imported {:matched (count matched) :ambiguous ambiguous
                                :unmatched (count unmatched)})
    (when-let [h (:import cfg)] (wr! h (poly/plan->json plan)))
    (fire! (:on-import cfg) env)))

(defn- read-file!
  [!st cfg env ^js e]
  (let [input (.-target e)]
    (when-let [f (aget (.-files input) 0)]
      (let [rdr (js/FileReader.)]
        (set! (.-onload rdr) #(import-svg! !st cfg env (.-result rdr)))
        (.readAsText rdr f)))
    (set! (.-value input) "")))

(defn- ->crs
  [cfg ^js ll]
  (let [p [(.-lng ll) (.-lat ll)]]
    (if (= :plan (crs cfg))
      (mapv r2 (geo/lnglat->plan p))
      (mapv r6 p))))

(defn- tolerance
  [^js m cfg ^js pt]
  (let [px (if (coarse?) (* 2 hit-px) hit-px)
        a  (.unproject m pt)
        b  (.unproject m #js [(+ (.-x pt) px) (.-y pt)])]
    (geo/distance (crs cfg) (->crs cfg a) (->crs cfg b))))

(defn- ->js [x] (clj->js x))

(defn- our-layers [scene] (mapv #(dissoc % :metadata) (:layers scene)))

(defn- line-scale
  [^js m]
  (let [w (.-clientWidth (.getContainer m))]
    (/ (Math/round (* 10 (-> (/ w 800) (max 0.5) (min 1)))) 10)))

(defn- thinned [k l]
  (if (and (< k 1) (= "line" (:type l)) (some? (get-in l [:paint :line-width])))
    (update-in l [:paint :line-width] #(vector "*" k %))
    l))

(defn- apply-layers!
  [^js m !applied scene]
  (let [k    (line-scale m)
        want (our-layers scene)]
    (when (not= [k want] [(:line-scale @!applied) (:layers @!applied)])
      (doseq [{:keys [id]} (:layers @!applied)]
        (when (.getLayer m id) (.removeLayer m id)))
      (doseq [l (:layers scene)]
        (.addLayer m (->js (thinned k l)))))
    (swap! !applied assoc :layers want :line-scale k)))

(defn- apply-sources!
  [^js m !applied scene]
  (let [have (:sources @!applied)]
    (doseq [[id src] (:sources scene)]
      (let [^js s (.getSource m id)]
        (cond
          (nil? s)                (.addSource m id (->js src))
          (= src (get have id))   nil
          (= "image" (:type src)) (.updateImage s (->js (select-keys src [:url :coordinates])))
          :else                   (.setData s (->js (:data src))))))
    (swap! !applied assoc :sources (:sources scene))))

(defn- drop-stale-sources!
  [^js m !applied scene]
  (doseq [id (keys (:sources @!applied))
          :when (not (contains? (:sources scene) id))]
    (when (.getSource m id) (.removeSource m id)))
  (swap! !applied update :sources select-keys (keys (:sources scene))))

(defn- apply-labels!
  [^js m !applied labels]
  (when (not= labels (:labels @!applied))
    (doseq [^js mk (:markers @!applied)] (.remove mk))
    (swap! !applied assoc
           :labels labels
           :markers (mapv (fn [{:keys [at text kind anchor]}]
                            (let [el (js/document.createElement "div")]
                              (set! (.-className el) (str "capp-map-label capp-map-label--"
                                                          (name kind)))
                              (set! (.-textContent el) text)
                              (-> (make "Marker" #js {:element el :anchor (or anchor "center")})
                                  (.setLngLat (->js at))
                                  (.addTo m))))
                          labels))))

(defn- apply-scene!
  [^js m !applied scene]
  (when (and m (.isStyleLoaded m))
    (apply-sources! m !applied scene)
    (apply-layers! m !applied scene)
    (drop-stale-sources! m !applied scene)
    (apply-labels! m !applied (:labels scene))))

(defn- go-to!
  [^js m {:keys [bounds center zoom]}]
  (cond
    bounds (.fitBounds m (->js bounds) #js {:padding 24 :animate false})
    center (.jumpTo m (->js (cond-> {:center center} zoom (assoc :zoom zoom))))))

(defn- hit-layers [scene] (vec (keep #(when (:metadata %) (:id %)) (:layers scene))))

(defn- clicked-object
  [^js m cfg scene ^js pt]
  (let [ids (filterv #(.getLayer m %) (hit-layers scene))]
    (when (seq ids)
      (when-let [^js f (first (.queryRenderedFeatures m pt #js {:layers (->js ids)}))]
        (let [li  (some-> f .-layer (gobj/getValueByKeys "metadata" "hit"))
              i   (some-> f .-properties .-row)
              lyr (when (some? li) (get (ms/layers-of cfg) li))]
          (when (and lyr (number? i) (not (neg? i)) (not (:locked? lyr)))
            {:layer lyr :row (nth (vec (:rows lyr)) i nil)}))))))

(defn- tip-at
  [^js m scene ^js pt]
  (let [ids (filterv #(.getLayer m %) (hit-layers scene))]
    (when (seq ids)
      (some #(let [t (some-> ^js % .-properties .-tip)] (when (and t (not= "null" t)) t))
            (.queryRenderedFeatures m pt #js {:layers (->js ids)})))))

(defn- on-click!
  [{:keys [!st !cfg !env !scene]} ^js m ^js e]
  (let [cfg  @!cfg env @!env st @!st
        p    (->crs cfg (.-lngLat e))
        add? (some-> e .-originalEvent .-shiftKey)
        hit  (delay (ms/shape-hit st p))
        area (delay (when (= :plan (crs cfg))
                      (ms/free-area-at (plan-areas cfg (:shapes st)) p)))
        obj  (delay (clicked-object m cfg @!scene (.-point e)))]
    (cond
      (:tool st)
      (step! !st cfg env [:click p] {:tol (tolerance m cfg (.-point e))})

      (or (:mode st) (seq (:editing st))) nil

      @hit  (step! !st cfg env [:select-shape @hit add?])

      @area (step! !st cfg env [:take-area @area])

      @obj  (let [{:keys [layer row]} @obj] (select-row! !st cfg env layer row add?))

      :else
      (do (when (and (:sel cfg) (:pick cfg))
            (let [^js ll (.-lngLat e)]
              (wr! (:sel cfg) (pick-string (:pick cfg) (r6 (.-lng ll)) (r6 (.-lat ll))))))
          (when (seq (:selected-shapes st)) (step! !st cfg env [:select-shape nil]))
          (clear-selections! !st cfg)))))

(defn- on-press!
  [{:keys [!st !cfg !env]} ^js m ^js e]
  (let [st @!st]
    (when (and (or (:mode st) (seq (:editing st))) (not (:tool st))
               (or (nil? (.-points e)) (= 1 (.-length (.-points e)))))
      (let [cfg @!cfg
            p   (->crs cfg (.-lngLat e))]
        (step! !st cfg @!env [:press p] {:tol (tolerance m cfg (.-point e))})
        (when (:drag @!st)
          (.preventDefault e)
          (.disable (.-dragPan m)))))))

(defn- on-move!
  [{:keys [!st !cfg !env !scene !popup]} ^js m ^js e]
  (let [st @!st cfg @!cfg]
    (cond
      (:drag st) (step! !st cfg @!env [:drag (->crs cfg (.-lngLat e))])
      (:tool st) (swap! !st assoc :hover (->crs cfg (.-lngLat e)))
      :else
      (let [tip (tip-at m @!scene (.-point e))]
        (if tip
          (-> ^js @!popup (.setLngLat (.-lngLat e)) (.setText tip) (.addTo m))
          (.remove ^js @!popup))))))

(defn- on-release!
  [{:keys [!st !cfg !env]} ^js m]
  (when (:drag @!st)
    (step! !st @!cfg @!env [:release])
    (.enable (.-dragPan m))))

(defn- on-dblclick!
  [{:keys [!st !cfg !env]} ^js e]
  (when (#{:line :polygon} (:tool @!st))
    (.preventDefault e)
    (step! !st @!cfg @!env [:finish])))

(defn- on-moveend!
  [{:keys [!st !cfg]} ^js m]
  (let [^js c (.getCenter m)
        ^js b (.getBounds m)
        bb    [(.getWest b) (.getSouth b) (.getEast b) (.getNorth b)]
        cfg   @!cfg]
    (swap! !st assoc :bounds bb :view {:center {:lat (.-lat c) :lon (.-lng c)} :zoom (.getZoom m)})
    (when-let [h (and (= :geo (crs cfg)) (:bounds cfg))]
      (let [text (geo/write-json (geo/bbox->polygon bb))]
        (swap! !st assoc :written-bounds text)
        (wr! h text)))))

(defn- create-map!
  [{:keys [!map !applied !scene !popup] :as ctx} el]
  (let [scene @!scene
        ^js m (make "Map" (->js (merge {:container el :style (:style scene)
                                        :attributionControl {:compact true}
                                        :doubleClickZoom true}
                                       (let [{:keys [center zoom bounds]} (:camera scene)]
                                         (cond
                                           bounds {:bounds bounds :fitBoundsOptions {:padding 24}}
                                           center {:center center :zoom (or zoom 1)})))))]
    (when-let [b (get-in scene [:camera :bounds])] (reset! (:!fitted ctx) b))
    (reset! !map m)
    (reset! !popup (make "Popup" #js {:closeButton false :closeOnClick false
                                      :className "capp-map-tip"}))
    (.addControl m (make "NavigationControl" #js {:visualizePitch false}) "bottom-left")
    (.addControl m (make "ScaleControl" #js {:unit "metric"}) "bottom-right")
    (when (exists? js/ResizeObserver)
      (.observe (js/ResizeObserver. #(do (.resize m)
                                               (when (.isStyleLoaded m)
                                                 (apply-layers! m !applied @!scene))))
                el))
    (.on m "style.load" (fn []
                          (reset! !applied {})
                          (apply-scene! m !applied @!scene)))
    (.on m "click" #(on-click! ctx m %))
    (.on m "mousedown" #(on-press! ctx m %))
    (.on m "touchstart" #(on-press! ctx m %))
    (.on m "mousemove" #(on-move! ctx m %))
    (.on m "touchmove" #(on-move! ctx m %))
    (.on m "mouseup" #(on-release! ctx m))
    (.on m "touchend" #(on-release! ctx m))
    (.on m "dblclick" #(on-dblclick! ctx %))
    (.on m "moveend" #(on-moveend! ctx m))
    m))

(defn- from-variables!
  [!st {:keys [drawn bounds] :as cfg}]
  (when drawn
    (let [text (rd drawn)]
      (when (not= text (:read-shapes @!st))
        (let [written (ms/shapes-text (:shapes @!st) (shape-kind cfg))]
          (swap! !st assoc :read-shapes text)
          (when-not (= text written)
            (swap! !st assoc :shapes (vec (ms/row-shapes (crs cfg) text))
                   :selected-shapes #{}))))))
  (when (and bounds (= :geo (crs cfg)))
    (let [text (rd bounds)]
      (when (and text (not= text (:written-bounds @!st)) (not= text (:read-bounds @!st)))
        (swap! !st assoc :read-bounds text)
        (when-let [g (first (geo/geometries text))]
          (when-let [{:keys [center zoom]} (ms/auto-view {} (first (:coordinates g)) nil)]
            (swap! !st assoc :goto {:center [(:lon center) (:lat center)] :zoom zoom
                                    :n (random-uuid)})))))))

(defn- auto-zoom!
  [!st cfg]
  (when (and (= :plan (crs cfg)) (or (:follow cfg) (:zoom-to cfg) (:auto-zoom? cfg)))
    (let [b (ms/target-bounds :plan cfg (ms/layers-of cfg) (:clock @!st))]
      (when (not= b (:plan-target @!st)) (swap! !st assoc :plan-target b))))
  (when (and (= :geo (crs cfg)) (or (:follow cfg) (:zoom-to cfg) (:auto-zoom? cfg)))
    (let [targets (ms/target-coords cfg (ms/layers-of cfg) (:clock @!st))]
      (when (not= targets (:targets @!st))
        (let [view (ms/auto-view cfg targets (:bounds @!st))]
          (swap! !st (fn [st]
                       (cond-> (assoc st :targets targets)
                         view (assoc :goto {:center [(get-in view [:center :lon])
                                                     (get-in view [:center :lat])]
                                            :zoom (:zoom view) :n (random-uuid)})))))))))

(defn- ms-of
  [x]
  (cond (number? x) x
        (nil? x)    nil
        :else       (let [t (.getTime (js/Date. x))] (when-not (js/isNaN t) t))))

(defn- iso [t] (when t (.toISOString (js/Date. t))))

(defn- clock-of
  [{:keys [selected-time time-start time-end playing] :as cfg} {:keys [cursor playing?]}]
  (when (:timeline? cfg)
    (let [[lo hi] (ms/clock-range (ms/layers-of cfg))]
      {:cursor   (or (some-> selected-time rd ms-of) cursor hi)
       :start    (or (some-> time-start rd ms-of) lo)
       :end      (or (some-> time-end rd ms-of) hi)
       :playing? (if playing (boolean (rd playing)) (boolean playing?))})))

(defn- set-cursor!
  [!st {:keys [selected-time playback-position]} t]
  (swap! !st assoc :cursor t)
  (when selected-time (wr! selected-time (iso t)))
  (when playback-position (wr! playback-position t)))

(defn- set-playing!
  [!st {:keys [playing]} on?]
  (swap! !st assoc :playing? on?)
  (when playing (wr! playing on?)))

(defn- tick!
  [!st cfg]
  (let [clock (clock-of cfg @!st)]
    (when (:playing? clock)
      (let [step (max 1000 (/ (- (or (:end clock) 0) (or (:start clock) 0)) 200))
            auto (keep ms-of (:auto-pause cfg))
            next (ms/tick clock step auto)]
        (set-cursor! !st cfg (:cursor next))
        (when-not (:playing? next) (set-playing! !st cfg false))))))

(defn- timeline-panel
  [!st {:keys [time-editable? live-toggle?] :as cfg} clock]
  [:div.capp-map-timeline
   [ui/button {:intent   :primary
               :disabled (not time-editable?)
               :on-click #(set-playing! !st cfg (not (:playing? clock)))}
    (if (:playing? clock) "Pause" "Abspielen")]
   (when-let [start (:start clock)]
     [slider {:class             "capp-map-timeline__slider"
              :min               start
              :max               (max (or (:end clock) start) (inc start))
              :initial-value     start
              :value             (or (:cursor clock) start)
              :label-values      []
              :label-renderer    false
              :disabled          (not time-editable?)
              :handle-html-props {:aria-label "Gewählte Zeit"}
              :on-change         #(set-cursor! !st cfg %)}])
   [:span.capp-map-timeline__at (when (:cursor clock) (ms/show-time (:cursor clock) cfg))]
   (when (and time-editable? live-toggle?)
     [ui/button {:intent :primary :on-click #(set-cursor! !st cfg (:end clock))}
      "Neueste anzeigen"])])

(defn- legend-panel
  [_ {:keys [legend-collapsed?]}]
  (let [open (r/atom (not legend-collapsed?))]
    (fn [!st {:keys [legend-size] :as cfg}]
      (let [compact? (= :compact (->kw legend-size))]
        [:div {:class (cond-> "capp-map-legend" compact? (str " capp-map-legend--compact"))}
         [ui/button {:minimal true :small true :on-click #(swap! open not)} "Legende"]
         (when @open
           (for [l (ms/layers-of cfg) :when (not (false? (:legend? l)))
                 :let [i (::ms/i l)]]
             ^{:key i}
             [checkbox {:class     "capp-map-legend__row"
                        :checked   (not (contains? (:hidden @!st) i))
                        :label     [:<>
                                    [:span.capp-map-legend__swatch
                                     {:style {:background (:color l)}}]
                                    (or (:label l) (str "Ebene " (inc i)))]
                        :on-change #(swap! !st update :hidden
                                           (fn [h] (let [h (or h #{})]
                                                     (if (contains? h i)
                                                       (disj h i)
                                                       (conj h i)))))}]))]))))

(defn- button
  ([label on-click] (button label on-click {}))
  ([label on-click attrs]
   [ui/button (merge {:on-click on-click} attrs) label]))

(defn- coords-text
  [c g]
  (let [pts (case (:type g)
              "Polygon"    (geo/open-ring (first (:coordinates g)))
              "LineString" (:coordinates g)
              "Point"      [(:coordinates g)]
              [])]
    (str/join "\n" (map (fn [[x y]] (if (= :plan c) (str (r2 x) ", " (r2 y))
                                        (str (r6 y) ", " (r6 x))))
                        pts))))

(defn- parse-coords
  [c g text]
  (let [pts (vec (keep (fn [line]
                         (when-not (str/blank? line)
                           (let [[a b] (map js/parseFloat (str/split (str/trim line) #"[,;\s]+"))]
                             (if (or (js/isNaN a) (js/isNaN b) (nil? b))
                               ::bad
                               (if (= :plan c) [a b] [b a])))))
                       (str/split-lines text)))]
    (cond
      (some #{::bad} pts) {:error "Jede Zeile braucht zwei Zahlen."}
      :else
      (case (:type g)
        "Polygon"    (cond
                       (< (count pts) 3) {:error "Eine Fläche braucht drei Punkte."}
                       (not (geo/simple-ring? pts)) {:error "Die Kanten kreuzen sich."}
                       :else {:shape {:type "Polygon" :coordinates [(conj pts (first pts))]}})
        "LineString" (if (< (count pts) 2)
                       {:error "Eine Linie braucht zwei Punkte."}
                       {:shape {:type "LineString" :coordinates pts}})
        "Point"      (if (= 1 (count pts))
                       {:shape {:type "Point" :coordinates (first pts)}}
                       {:error "Ein Punkt ist eine Zeile."})
        {:error "Diese Form lässt sich hier nicht bearbeiten."}))))

(defn- coords-editor
  [_ _ _ _]
  (let [local (r/atom nil)]
    (fn [!st cfg env target]
      (let [c    (crs cfg)
            g    (ms/target-shape @!st target)
            shown (coords-text c g)
            {:keys [text for error]} @local
            text (if (= for [target shown]) text shown)]
        [:div.capp-map-panel__coords
         [:div.capp-map-panel__label "Koordinatensystem"]
         [:div.capp-map-panel__value (if (= :plan c)
                                       (str "Planmaß (x, y in " (unit-of cfg) ")")
                                       "Dezimalgrad (Breite, Länge)")]
         [:div.capp-map-panel__label "Koordinaten"]
         [text-area
          {:class "capp-map-panel__list"
           :fill  true
           :value text :rows (min 12 (max 3 (count (str/split-lines text))))
           :spell-check false
           :on-change (fn [e]
                        (let [t (.. e -target -value)
                              {:keys [shape error]} (parse-coords c g t)]
                          (if shape
                            (do (step! !st cfg env [:set-coords target shape])
                                (reset! local {:text t :for [target (coords-text c shape)]}))
                            (reset! local {:text t :for [target shown] :error error}))))}]
         (if (and error (= for [target shown]))
           [:div.capp-map-panel__hint.capp-map-panel__hint--error (str "⚠ " error)]
           [:div.capp-map-panel__hint "✓ Gültige Änderungen werden automatisch übernommen"])
         (when-let [ms* (measurements cfg)]
           (when-let [t (seq (ms/measure-labels c (unit-of cfg) g ms*))]
             [:div.capp-map-panel__hint (str/join " · " t)]))]))))

(defn- action-choice
  [_ _]
  (let [chosen (r/atom nil)]
    (fn [env {:keys [actions defaults-of title on-done on-cancel]}]
      (let [a (or @chosen (when (= 1 (count actions)) (first actions)))]
        [:div.capp-map-panel__promote
         [:div.capp-map-panel__label (str title " (Pflicht)")]
         [html-select
          {:fill  true
           :value (or (some-> a :action name) "")
           :on-change #(reset! chosen (some (fn [x] (when (= (.. % -target -value)
                                                             (name (->kw (:action x))))
                                                      x))
                                            actions))}
          [:option {:value ""} "Bitte wählen …"]
          (for [x actions :let [n (name (->kw (:action x)))]]
            ^{:key n} [:option {:value n} (or (:label x) ((:action-label env) (:action x)))])]
         (when a
           ^{:key (name (->kw (:action a)))}
           [:div.capp-map-panel__form
            ((:action-form env) {:action (:action a) :defaults (defaults-of a)
                                 :on-done (fn [_] (reset! chosen nil) (on-done a))
                                 :on-cancel (fn [_] (reset! chosen nil) (on-cancel))})])]))))

(defn- shape-panel
  [!st cfg env i]
  (let [g      (get (:shapes @!st) i)
        acts   (:shape-actions cfg)
        promo? (:promoting @!st)
        close! #(do (swap! !st dissoc :promoting) (step! !st cfg env [:select-shape nil]))]
    [:div.capp-map-panel
     [:div.capp-map-panel__head
      [:span (case (:type g) "Point" "Punkt bearbeiten" "LineString" "Linie bearbeiten"
                   "Umriss bearbeiten")]
      [ui/button {:class "capp-map-panel__close" :variant :minimal :size :small
                  :icon :cross :aria-label "Schließen" :on-click close!}]]
     [:div.capp-map-panel__body
      (when promo?
        [action-choice env
         {:actions acts :title "Objekttyp"
          :defaults-of (fn [a] (merge (:params a)
                                      {(->kw (or (:shape-param a) :shape))
                                       (ms/shape-text g (output-of cfg a))}))
          :on-done (fn [_]
                     (swap! !st dissoc :promoting)
                     (step! !st cfg env [:select-shape i false])
                     (step! !st cfg env [:delete-selected]))
          :on-cancel #(swap! !st dissoc :promoting)}])
      [coords-editor !st cfg env [:shape i]]]
     (when-not promo?
       [:div.capp-map-panel__foot
        (button "Abbrechen" close!)
        (when (seq acts)
          [ui/button {:intent   :primary
                      :outlined true
                      :on-click #(swap! !st assoc :promoting true)}
           "Als Objekt anlegen"])
        [ui/button {:intent :success :on-click close!} "Fertig"]])]))

(defn- edits-panel
  [!st cfg env]
  (let [st      @!st
        ks      (keys (:editing st))
        changed (ms/changed-edits st)
        n       (count changed)
        {:keys [open errors]} (:saving st)]
    [:div.capp-map-panel
     [:div.capp-map-panel__head
      [:span (if (= 1 (count ks)) "Umriss bearbeiten" (str (count ks) " Umrisse bearbeiten"))]
      [ui/button {:class "capp-map-panel__close" :variant :minimal :size :small
                  :icon :cross :aria-label "Schließen"
                  :on-click #(step! !st cfg env [:discard-edits])}]]
     [:div.capp-map-panel__body
      (when (= 1 (count ks)) [coords-editor !st cfg env (into [:object] (first ks))])
      (when (seq errors)
        [:div.capp-map-panel__hint.capp-map-panel__hint--error
         (str "⚠ " (count errors) " nicht gespeichert — " (first errors))])]
     [:div.capp-map-panel__foot
      (button "Verwerfen" #(step! !st cfg env [:discard-edits]) {:disabled (boolean open)})
      [ui/button {:intent   :success
                  :loading  (boolean open)
                  :disabled (zero? n)
                  :on-click #(save-edits! !st cfg env)}
       (cond open "…" (< 1 n) (str n " speichern") :else "Speichern")]]]))

(defn- row-title [layer row]
  (let [lbl (some :label (:geometries layer))]
    (str (or (when lbl (or (get row lbl) (get-in row [:values lbl]))) (:title row) (:id row)))))

(defn- shown-id
  [env]
  (some-> (get-in env [:scope :data*]) deref :object first :id str))

(defn- selection-panel
  [!st cfg env]
  (let [sels (selections-of cfg @!st)
        ls   (ms/layers-of cfg)
        acts (for [l ls :let [rows (get sels (::ms/i l))] :when (seq rows)] [l rows])
        open (:object-action @!st)]
    (when (seq acts)
      [:div.capp-map-panel.capp-map-panel--selection
       [:div.capp-map-panel__head
        [:span (str (reduce + (map (comp count second) acts)) " ausgewählt")]
        [ui/button {:class "capp-map-panel__close" :variant :minimal :size :small
                    :icon :cross :aria-label "Auswahl leeren"
                    :on-click #(clear-selections! !st cfg)}]]
       [:div.capp-map-panel__body
        (for [[l rows] acts]
          ^{:key (::ms/i l)}
          [:div.capp-map-selection
           (when (:label l) [:div.capp-map-selection__head (:label l)])
           (for [[i row] (map-indexed vector rows)]
             ^{:key i}
             [:div.capp-map-selection__row
              (if-let [t (when (and (some? (:id row)) (not= (str (:id row)) (shown-id env)))
                           (:object-type-ref row))]
                [oc/object-link t (:id row)
                 {:row row :on-open (get-in env [:app :open-object])}]
                (row-title l row))])
           (if (and open (= (::ms/i l) (:layer open)))
             [action-choice env
              {:actions (:actions l) :title "Aktion"
               :defaults-of (fn [a] (merge (:params a)
                                           {(->kw (or (:object-param a) :object))
                                            (row-id (first rows) (->kw (or (:id-prop a) :id)))}))
               :on-done #(swap! !st dissoc :object-action)
               :on-cancel #(swap! !st dissoc :object-action)}]
             [:div.capp-map-panel__actions
              (when (:edit-action l)
                (button (if (some #(= :shape (->kw (:geometry %))) (:geometries l))
                          "Umriss bearbeiten" "Punkt verschieben")
                        #(start-edit! !st cfg env rows l)))
              (when (and (seq (:actions l)) (:action-form env))
                (button "Aktionen" #(swap! !st assoc :object-action {:layer (::ms/i l)})))])])]])))

(def ^:private tool-labels
  {:point "Punkt" :line "Linie" :polygon "Fläche" :rectangle "Rechteck" :circle "Kreis"
   :wall "Wand ziehen"})

(def ^:private modify-labels
  {:points "Punkte bearbeiten" :translate "Verschieben" :rotate "Drehen"})

(def ^:private hints
  {:kreuzt   "Die Kante würde sich selbst kreuzen — der Punkt zählt nicht."
   :zu-wenig "Weniger Punkte gehen nicht."
   :keine-wand "Auf eine Wand tippen — eine neue Wand beginnt und endet an einer."})

(defn- drawing-hint
  [{:keys [tool draft hint mode modify editing]}]
  (let [n (count draft)]
    (cond
      (get hints hint) (get hints hint)
      (= :polygon tool) (cond (zero? n) "Tippen setzt den ersten Punkt"
                              (< n 3)   (str n " Punkte")
                              :else     (str n " Punkte · den ersten Punkt tippen, doppelt "
                                             "tippen oder „Fertig“ schließt die Fläche"))
      (= :line tool) (if (< n 2) "Tippen setzt Punkte" "Doppelt tippen oder „Fertig“ beendet")
      (#{:rectangle :circle} tool) (if (zero? n) "Erste Ecke bzw. Mitte tippen"
                                       "Zweite Ecke bzw. Rand tippen")
      (= :point tool) "Tippen setzt den Punkt"
      (or (= :points modify) (and (seq editing) (nil? mode)))
      "Punkt ziehen · Kantenmitte ziehen fügt einen Punkt ein"
      (= :translate modify) "Form ziehen verschiebt sie"
      (= :rotate modify) "Form ziehen dreht sie um ihre Mitte"
      :else nil)))

(defn- buffer-input
  [_ _ _]
  (let [v (r/atom "")]
    (fn [!st cfg env]
      [:span.capp-map-tools__buffer
       [numeric-input {:small true :button-position :none :value @v
                       :placeholder (str "± " (unit-of cfg)) :aria-label "Puffer"
                       :on-value-change #(reset! v %2)}]
       (button "Puffer" #(let [d (js/parseFloat @v)]
                           (when-not (js/isNaN d) (step! !st cfg env [:buffer d]))))])))

(defn- select-in-shape!
  [!st cfg env]
  (let [{:keys [shapes clock]} @!st]
    (doseq [l (ms/layers-of cfg) :when (not (:locked? l))]
      (set-selection! !st l (vec (distinct (mapcat #(ms/intersecting (crs cfg) l % clock)
                                                   shapes)))))
    (when (:clear-after-op? cfg) (step! !st cfg env [:clear]))))

(defn- search-in-shape!
  [!st cfg env]
  (wr! (:shape-search cfg) (ms/shapes-text (:shapes @!st) (shape-kind cfg)))
  (when (:clear-after-op? cfg) (step! !st cfg env [:clear])))

(defn- locate!
  [!st]
  (when-let [^js g (.-geolocation js/navigator)]
    (.getCurrentPosition
     g (fn [^js p]
         (let [^js c (.-coords p)]
           (swap! !st assoc :goto {:center [(.-longitude c) (.-latitude c)] :zoom 12
                                   :n (random-uuid)}))))))

(defn- tool-button
  [active on-click label]
  [ui/tooltip {:content label}
   [ui/button {:intent       :primary
               :active       (boolean active)
               :aria-pressed (boolean active)
               :on-click     on-click}
    label]])

(def ^:private edge-dirs
  {:first [[:right "→" "nach rechts"] [:up "↑" "nach oben"] [:left "←" "nach links"]
           [:down "↓" "nach unten"]]
   :turn  [[:left-square "⟲" "rechtwinklig links"] [:straight "↑" "geradeaus"]
           [:right-square "⟳" "rechtwinklig rechts"]]})

(defn- edge-row
  [_ _ _]
  (let [v (r/atom "")]
    (fn [!st cfg env]
      (let [len (js/parseFloat (str/replace (str @v) "," "."))
            go  (fn [d] #(when (and (not (js/isNaN len)) (pos? len))
                           (step! !st cfg env [:edge len d])))]
        [:div.capp-map-sheet__row
         [:input.capp-map-sheet__length
          {:type "text" :input-mode "decimal" :enter-key-hint "done" :value @v
           :placeholder "Länge in m" :aria-label "Länge der nächsten Kante in Metern"
           :on-change #(reset! v (.. % -target -value))}]
         (for [[d sym label] (get edge-dirs (if (next (:draft @!st)) :turn :first))]
           ^{:key d} [ui/button {:class "capp-map-sheet__key" :aria-label label
                                 :on-click (go d)}
                      sym])]))))

(defn- plan-state
  [cfg shapes]
  (let [roles (set (keep #(some-> % :role ->kw) (ms/layers-of cfg)))
        areas (plan-areas cfg shapes)]
    {:stage (cond (not (roles :outline)) :free areas :walls :else :outline)
     :areas areas}))

(defn- sheet-tools
  [cfg stage]
  (case stage
    :walls   [:wall]
    :outline (filterv #{:polygon :rectangle} (map ->kw (:draw-tools cfg)))
    (mapv ->kw (:draw-tools cfg))))

(defn- choices
  [cfg stage]
  (let [outline-action (some->> (ms/layers-of cfg)
                                (some #(when (= :outline (some-> % :role ->kw)) (:edit-action %)))
                                ->kw)
        outline? #(= outline-action (->kw (:action %)))]
    (case stage
      :outline (let [as (:shape-actions cfg)] (concat (filter outline? as) (remove outline? as)))
      :walls   (remove outline? (:shape-actions cfg))
      (:shape-actions cfg))))

(defn- choice-sheet
  [!st cfg env i stage]
  (let [st   @!st
        g    (get (:shapes st) i)
        ring (when (= "Polygon" (:type g)) (first (:coordinates g)))
        [w s e n] (when ring (geo/bbox ring))
        a    (:promoting st)
        drop! #(do (swap! !st dissoc :promoting) (step! !st cfg env [:delete-selected]))]
    [:div.capp-map-sheet
     [:div.capp-map-sheet__head
      [:b (if a (or (:label a) "Anlegen") "Was ist das?")]
      (when ring
        [:span.capp-map-sheet__muted
         (str (geo/format-plan-area (geo/area :plan ring) (:unit cfg)) " · "
              (geo/format-plan-length (- e w) nil) " × " (geo/format-plan-length (- n s) nil))])]
     (if a
       [:div.capp-map-sheet__form
        ((:action-form env)
         {:action   (:action a)
          :defaults (merge (:params a)
                           {(->kw (or (:shape-param a) :shape))
                            (ms/shape-text g (output-of cfg a))})
          :on-done  (fn [_] (swap! !st dissoc :promoting)
                      (step! !st cfg env [:select-shape i false])
                      (step! !st cfg env [:delete-selected]))
          :on-cancel (fn [_] (swap! !st dissoc :promoting))})]
       [:<>
        (for [x (choices cfg stage) :let [n (name (->kw (:action x)))]]
          ^{:key n}
          [ui/button {:class "capp-map-sheet__choice" :fill true :align-text :left
                      :on-click #(swap! !st assoc :promoting x)}
           (or (:label x) ((:action-label env) (:action x)))])
        [:div.capp-map-sheet__row
         [ui/button {:class "capp-map-sheet__wide" :on-click drop!} "Verwerfen"]
         [ui/button {:class "capp-map-sheet__wide"
                     :on-click #(step! !st cfg env [:modify :points])}
          "Nachbessern"]]])]))

(defn- draw-sheet
  [!st cfg env tools stage]
  (let [{:keys [tool draft snap-off?] :as st} @!st
        multi? (#{:line :polygon} tool)]
    [:div.capp-map-sheet
     [:div.capp-map-sheet__row
      (if (next tools)
        [segmented-control {:class "capp-map-sheet__tools" :fill true :value tool
                            :options (for [t tools] {:label (get tool-labels t (name t)) :value t})
                            :on-value-change (fn [t _] (step! !st cfg env [:tool t]))}]
        [:b.capp-map-sheet__title (if (= :outline stage)
                                    "Umriss"
                                    (get tool-labels tool (name tool)))])
      (when (#{:line :polygon :wall} tool)
        [ui/button {:icon (if snap-off? :square :tick-circle)
                    :intent (when-not snap-off? :primary) :outlined snap-off?
                    :aria-pressed (not snap-off?)
                    :on-click #(swap! !st update :snap-off? not)}
         "Einrasten"])]
     (when-let [h (drawing-hint st)] [:div.capp-map-sheet__hint h])
     (when (and multi? (seq draft)) [edge-row !st cfg env])
     [:div.capp-map-sheet__row
      (when (and multi? (seq draft))
        [ui/button {:class "capp-map-sheet__wide" :icon :undo
                    :on-click #(step! !st cfg env [:undo])} "Zurück"])
      [ui/button {:class "capp-map-sheet__wide" :on-click #(step! !st cfg env [:cancel])}
       (if (= :wall tool) "Fertig" "Abbrechen")]
      (when multi?
        [ui/button {:class "capp-map-sheet__wide" :intent :primary
                    :on-click #(step! !st cfg env [:finish])} "Fertig"])]]))

(defn- rest-sheet
  [!st cfg env {:keys [stage areas]} tools]
  (let [free (count (filter (comp nil? :room) (:faces areas)))
        {:keys [cutting cut-error]} @!st]
    [:div.capp-map-sheet.capp-map-sheet--rest
     (when (some #(not= :cut (first %)) (:problems areas))
       [:div.capp-map-sheet__hint.capp-map-sheet__hint--warn
        "Die Räume teilen den Umriss nicht sauber (Überlappung oder außerhalb) — "
        "die Flächen sind nicht eindeutig."])
     (for [{:keys [room ring parts]} (ms/room-cuts areas)
           :let [[li :as k] room
                 nm (row-title (get (ms/layers-of cfg) li) (row-of cfg k))]]
       ^{:key (str room)}
       [:div.capp-map-sheet__cut
        [:div.capp-map-sheet__hint
         (str "Die Wand teilt „" nm "“ in " parts " Teile — der größte bleibt " nm
              ", der Rest wird frei.")]
        [ui/button
         {:fill true :intent :primary :loading (= room cutting)
          :disabled (some? cutting)
          :on-click #(do (swap! !st assoc :cutting room :cut-error nil)
                         (submit-shape! cfg env room {:type "Polygon" :coordinates [ring]}
                                        {:on-ok    (fn [_] (swap! !st dissoc :cutting))
                                         :on-error (fn [m] (swap! !st assoc :cutting nil
                                                                  :cut-error m))}))}
         (str nm " verkleinern")]])
     (when cut-error
       [:div.capp-map-sheet__hint.capp-map-sheet__hint--warn (str "⚠ " cut-error)])
     (when (and (= :walls stage) (pos? free))
       [:div.capp-map-sheet__hint
        (str free (if (= 1 free) " freie Fläche" " freie Flächen")
             " — antippen, um zu sagen, welcher Raum sie ist")])
     (when (seq tools)
       (let [go #(step! !st cfg env [:tool (if (some #{:polygon} tools)
                                              :polygon (first tools))])]
         (if (= :walls stage)
           [ui/button {:variant :minimal :size :small :icon :new-grid-item :on-click go}
            "Wand ziehen"]
           [ui/button {:class "capp-map-sheet__go" :intent :primary :large true :fill true
                       :icon :edit :on-click go}
            (if (= :outline stage) "Umriss zeichnen" "Zeichnen")])))]))

(defn- plan-sheet
  [!st cfg env]
  (let [st    @!st
        ps    (plan-state cfg (:shapes st))
        tools (sheet-tools cfg (:stage ps))
        one   (when (= 1 (count (:selected-shapes st))) (first (:selected-shapes st)))]
    (cond
      (:tool st)                         [draw-sheet !st cfg env tools (:stage ps)]
      (= :modify (:mode st))
      [:div.capp-map-sheet
       [:div.capp-map-sheet__hint (drawing-hint st)]
       [ui/button {:intent :primary :fill true :on-click #(step! !st cfg env [:done])}
        "Fertig"]]
      (seq (:editing st))                nil
      (and (some? one) (seq (:shape-actions cfg))
           (= "Polygon" (:type (get (:shapes st) one))))
      [choice-sheet !st cfg env one (:stage ps)]
      :else                              [rest-sheet !st cfg env ps tools])))

(defn- history-buttons
  [!st cfg env]
  (let [{:keys [tool past future]} @!st]
    (when (and (not tool) (or (seq past) (seq future)))
      [:<>
       [ui/button {:icon :undo :aria-label "Rückgängig" :disabled (empty? past)
                   :on-click #(step! !st cfg env [:undo])}]
       [ui/button {:icon :redo :aria-label "Wiederholen" :disabled (empty? future)
                   :on-click #(step! !st cfg env [:redo])}]])))

(defn- plan-overlay
  [!st cfg env !file]
  (let [st    @!st
        open? (:menu-open st)
        close #(swap! !st dissoc :menu-open)
        outl  (first (filter #(= :outline (some-> % :role ->kw)) (ms/layers-of cfg)))]
    [:div.capp-map-overlay
     [ui/button {:icon :fullscreen :aria-label "Ganzer Plan"
                 :on-click #(swap! !st assoc :goto {:bounds (ms/plan-bounds (plan-of cfg st))
                                                    :n (random-uuid)})}]
     [history-buttons !st cfg env]
     [ui/button {:icon :more :aria-label "Mehr" :aria-expanded (boolean open?)
                 :on-click #(swap! !st update :menu-open not)}]
     (when open?
       [menu {:class "capp-map-overlay__menu"}
        (when (and outl (:edit-action outl) (seq (:rows outl)))
          [menu-item {:text "Umriss ändern" :icon :edit
                      :on-click #(do (close)
                                     (start-edit! !st cfg env (vec (take 1 (:rows outl))) outl))}])
        [menu-item {:text "Als SVG speichern" :icon :download
                    :on-click #(do (close) (export-svg! cfg st))}]
        (when (:import cfg)
          [menu-item {:text "SVG laden" :icon :upload
                      :on-click #(do (close) (some-> @!file .click))}])
        (when (seq (:shapes st))
          [menu-item {:text "Entwürfe verwerfen" :icon :trash :intent :danger
                      :on-click #(do (close) (step! !st cfg env [:clear]))}])])
     (when (:import cfg)
       [:input {:type "file" :accept ".svg,image/svg+xml" :style {:display "none"}
                :ref #(reset! !file %) :on-change #(read-file! !st cfg env %)}])]))

(defn- toolbar
  [!st {:keys [draw-tools shape-select? shape-search shape-edit? selection-tools? locate?
               base-picker? style import]
        :as cfg}
   env !file]
  (let [{:keys [tool mode modify shapes base selected-shapes]} @!st
        plan? (= :plan (crs cfg))
        tools (map ->kw draw-tools)
        do!   (fn [ev] #(step! !st cfg env ev))]
    [:div.capp-map-tools
     (for [t tools]
       ^{:key t} [tool-button (= t tool) (do! (if (= t tool) [:cancel] [:tool t]))
                  (get tool-labels t (name t))])
     [history-buttons !st cfg env]
     (when (#{:line :polygon} tool)
       [:<>
        [tool-button false (do! [:finish]) "Fertig"]
        [tool-button false (do! [:undo]) "Punkt zurück"]
        [tool-button false (do! [:cancel]) "Abbrechen"]])
     (when (and (seq shapes) (not tool))
       [:<>
        (when shape-edit?
          [:<>
           (for [m [:points :translate :rotate]]
             ^{:key m} [tool-button (= m modify) (do! (if (= m modify) [:done] [:modify m]))
                        (get modify-labels m)])
           [buffer-input !st cfg env]
           (when (= 1 (count selected-shapes))
             [tool-button false (do! [:replace]) "Ersetzen"])
           (when (and (= :points modify) (:active-vertex @!st))
             [tool-button false (do! [:delete-vertex]) "Punkt löschen"])])
        (when (seq selected-shapes)
          [tool-button false (do! [:delete-selected]) "Löschen"])
        [tool-button false (do! [:clear]) "Alle löschen"]
        (when shape-select?
          [tool-button false #(select-in-shape! !st cfg env) "Objekte im Umriss wählen"])
        (when shape-search
          [tool-button false #(search-in-shape! !st cfg env) "Objekte im Umriss suchen"])])
     (when (and (= :modify mode) (empty? shapes) (empty? (:editing @!st)))
       [tool-button false (do! [:done]) "Fertig"])
     (when selection-tools?
       [:<>
        [tool-button false
         #(doseq [l (ms/layers-of cfg) :when (not (:locked? l))]
            (set-selection! !st l (ms/in-time l (:clock @!st))))
         "Alle wählen"]
        [tool-button false #(clear-selections! !st cfg) "Auswahl leeren"]])
     (when plan?
       [:<>
        [tool-button false #(swap! !st assoc :goto {:bounds (ms/plan-bounds (plan-of cfg @!st))
                                                    :n (random-uuid)})
         "Ganzer Plan"]
        (when import
          [:<>
           [tool-button false #(some-> @!file .click) "SVG laden"]
           [:input {:type "file" :accept ".svg,image/svg+xml" :style {:display "none"}
                    :ref #(reset! !file %) :on-change #(read-file! !st cfg env %)}]])
        [tool-button false #(export-svg! cfg @!st) "SVG"]])
     (when locate?
       [tool-button false #(locate! !st) "Mein Standort"])
     (when (and base-picker? (not plan?))
       [html-select {:value (name (or base (->kw style) :carto-positron))
                     :aria-label "Basiskarte"
                     :on-change #(swap! !st assoc :base (keyword (.. % -target -value)))}
        (for [b ms/base-layers] ^{:key b} [:option {:value (name b)} (name b)])])]))

(defn- status-line
  [!st cfg]
  (let [st @!st
        h  (drawing-hint st)
        {:keys [matched ambiguous unmatched]} (:imported st)]
    (when (or h (:imported st))
      [:div.capp-map-status
       (when h [:span h])
       (when (:imported st)
         [:span (str "SVG: " matched " Umrisse zugeordnet"
                     (when (pos? unmatched) (str ", " unmatched " als neue Formen"))
                     (when (pos? ambiguous)
                       (str ", " ambiguous " übersprungen (Name mehrdeutig)")))])
       (when (and (= :plan (crs cfg)) (nil? (plan-of cfg st)))
         [:span "kein Grundriss hinterlegt"])])))

(defn- scene-of
  [!st cfg]
  (let [st    @!st
        clock (clock-of cfg st)]
    (ms/map->scene (assoc cfg
                          :picked (picked-of cfg)
                          :measurements (measurements cfg)
                          :unit (unit-of cfg)
                          :plan (plan-of cfg st))
                   (assoc st
                          :clock clock
                          :selections (selections-of cfg st)))))

(defn- load-image!
  [!st {:keys [src extent] :as cfg}]
  (when (and (= :plan (crs cfg)) (not (extent-dims extent)) (not (str/blank? (str src)))
             (not= src (:img-src @!st)))
    (swap! !st assoc :img-src src)
    (let [img (js/Image.)]
      (set! (.-onload img)
            #(swap! !st assoc :img-size {:w (.-naturalWidth img) :h (.-naturalHeight img)}))
      (set! (.-src img) (str src)))))

(defn- key-actions
  [!st !cfg !env]
  (let [do! (fn [ev] (step! !st @!cfg @!env ev) true)]
    {:cancel  (fn [_] (let [st @!st]
                        (cond (:tool st)                                    (do! [:cancel])
                              (some st [:mode :modify :active-vertex :drag]) (do! [:done]))))
     :confirm (fn [_] (when (:tool @!st) (do! [:finish])))
     :undo    (fn [_] (let [st @!st] (when (or (:tool st) (seq (:past st))) (do! [:undo]))))
     :redo    (fn [_] (when (seq (:future @!st)) (do! [:redo])))
     :delete  (fn [_] (let [st @!st]
                        (cond (:active-vertex st)          (do! [:delete-vertex])
                              (seq (:selected-shapes st)) (do! [:delete-selected]))))}))

(defn map-view
  [_ _]
  (let [!st     (r/atom {:shapes [] :selected-shapes #{} :editing {} :originals {}
                         :timeline-open false})
        !cfg    (atom nil)
        !env    (atom nil)
        !map    (atom nil)
        !applied (atom {})
        !scene  (atom nil)
        !popup  (atom nil)
        !el     (atom nil)
        !file   (atom nil)
        !style  (atom nil)
        !goto   (atom nil)
        !fitted (atom false)
        timer   (atom nil)
        keys*   (vec (hotkeys/canvas #(deref !el) (key-actions !st !cfg !env)))
        ctx     {:!st !st :!cfg !cfg :!env !env :!map !map :!applied !applied :!scene !scene
                 :!popup !popup :!fitted !fitted}
        sync!   (fn []
                  (when-let [^js m @!map]
                    (let [scene @!scene]
                      (when (not= (:style scene) @!style)
                        (reset! !style (:style scene))
                        (.setStyle m (->js (:style scene))))
                      (apply-scene! m !applied scene)
                      (when-let [b (or (when (= :plan (crs @!cfg))
                                         (or (:plan-target @!st)
                                             (some-> (plan-of @!cfg @!st) ms/plan-bounds)))
                                       (get-in scene [:camera :bounds]))]
                        (when (not= b @!fitted)
                          (reset! !fitted b)
                          (go-to! m {:bounds b})))
                      (let [g (:goto @!st)]
                        (when (and g (not= (:n g) @!goto))
                          (reset! !goto (:n g))
                          (go-to! m g)))
                      (set! (.. m getCanvas -style -cursor)
                            (cond (:drag @!st) "grabbing" (:tool @!st) "crosshair" :else "")))))]
    (r/create-class
     {:display-name "capp-map-frame"
      :component-did-mount
      (fn [_]
        (hotkeys/offer! !st keys*)
        (-> (vendor/load! :maplibre)
            (.then (fn [lib]
                     (reset! !lib lib)
                     (when-let [el @!el]
                       (reset! !style (:style @!scene))
                       (create-map! ctx el))))
            (.catch #(js/console.error "capp-map: MapLibre nicht geladen" %))))
      :component-did-update (fn [_ _] (sync!))
      :component-will-unmount
      (fn [_]
        (hotkeys/release! !st)
        (some-> @timer js/clearInterval)
        (doseq [^js mk (:markers @!applied)] (.remove mk))
        (some-> ^js @!map .remove)
        (reset! !el nil))
      :reagent-render
      (fn [cfg env]
        (let [cfg (ms/stacked cfg)]
          (reset! !cfg cfg)
          (reset! !env env)
          (from-variables! !st cfg)
          (load-image! !st cfg)
          (auto-zoom! !st cfg)
          (when (and (:timeline? cfg) (nil? @timer))
            (reset! timer (js/setInterval #(tick! !st @!cfg) 500)))
          (reset! !scene (scene-of !st cfg))
          (let [st    @!st
                clock (clock-of cfg st)
                open? (or (:timeline-open? cfg) (:timeline-open st))
                one   (when (and (= 1 (count (:selected-shapes st))) (not (:tool st)))
                        (first (:selected-shapes st)))
                plan? (= :plan (crs cfg))]
            [:div {:class (cond-> "capp-map-frame"
                            (= :plan (crs cfg)) (str " capp-map-frame--plan")
                            (:class cfg) (str " " (:class cfg)))
                   :on-pointer-down-capture #(hotkeys/claim! !st keys*)}
             (when-not plan? [toolbar !st cfg env !file])
             [:div.capp-map-stage
              [:div.capp-map {:ref #(reset! !el %)
                              :style (let [h (str (or (:height cfg) 420) "px")]
                                       (if-let [{pw :w ph :h} (when (= :plan (crs cfg))
                                                                (plan-of cfg st))]
                                         {:aspect-ratio (str pw " / " ph) :max-height h
                                          :min-height 160}
                                         {:height h}))}]
              (when plan? [plan-overlay !st cfg env !file])
              (cond
                (seq (:editing st))         [edits-panel !st cfg env]
                (and (some? one) (not plan?)) [shape-panel !st cfg env one]
                :else                       [selection-panel !st cfg env])
              (when (:legend? cfg) [legend-panel !st cfg])]
             (if plan? [plan-sheet !st cfg env] [status-line !st cfg])
             (when (:timeline? cfg)
               [:div.capp-map-timeline-wrap
                (when-not open?
                  [ui/button {:intent   :primary
                              :on-click #(swap! !st assoc :timeline-open true)}
                   "Zeitachse"])
                (when open? [timeline-panel !st cfg clock])])])))})))
