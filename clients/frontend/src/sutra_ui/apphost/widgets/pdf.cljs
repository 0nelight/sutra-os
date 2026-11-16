(ns sutra-ui.apphost.widgets.pdf
  (:require [sutra-ui.ui.vendor :as vendor]
            [sutra-ui.ui.sel :as sel]
            [sutra-ui.ui.overlay :as overlay]
            [sutra-ui.download :as dl]
            [sutra-ui.storage :as storage]
            [clojure.string :as str]
            [sutra-ui.ui.busy :as busy]
            [reagent.core :as r]
            [sutra-ui.ui.core :as ui :refer [input-group]]
            [sutra.capps.pdf-export :as pdf-export]
            [sutra.capps.pdf-layers :as pdf-layers]
            [sutra.capps.variables :refer [deref*]]
            [sutra-ui.apphost.interactions :as interactions]
            [sutra-ui.state.auth :as auth]))

(defn- pdfjs [] (vendor/loaded :pdfjs))

(def ^:private segment-weight {:name 400 :sep 400 :value 700})

(defn- caption-tag
  [segments x y w h color]
  (let [fs 11 pad 4
        tw (+ (* 2 pad) (* 0.6 fs (count (pdf-layers/segments-text segments))))
        th (+ fs (* 2 pad 0.75))
        inside? (and (<= tw (* 0.5 w)) (<= th (* 0.5 h)))
        tx (if inside? (- (+ x w) tw) (+ x w 2))
        ty (if inside? y (+ y (/ (- h th) 2)))]
    [:g.capp-pdf__caption
     [:rect {:x tx :y ty :width tw :height th :rx 2 :fill color}]
     (into [:text {:x (- (+ tx tw) pad) :y (+ ty fs 1) :text-anchor "end" :font-size fs
                   :fill "#fff" :font-family "Arial, Helvetica, sans-serif"
                   :xml-space "preserve"}]
           (map (fn [[k t]] [:tspan {:font-weight (segment-weight k)} t]))
           segments)]))

(declare box-editor)

(defn band-box
  [box page-w]
  (assoc box :x1 0 :x2 (/ (js/Math.round (* 10 page-w)) 10)))

(defn rect-mark
  [{:keys [id geom color label caption tag selected on-change] :as a} scale on-select]
  (let [x (* (:x geom) scale) y (* (:y geom) scale)
        w (* (:w geom) scale) h (* (:h geom) scale)
        edit? (and selected on-change)
        band? (= :band (keyword (:kind a)))
        attrs {:fill (or color "#ffeb3b")
               :fill-opacity (cond selected 0.34 caption 0.18 :else 0.30)
               :stroke (or color "#fbc02d") :stroke-width (if selected 3.5 1.5)
               :class (str "capp-pdf__mark" (when selected " capp-pdf__mark--selected")
                           (when on-select " capp-pdf__mark--clickable")
                           (when edit? " capp-pdf__mark--edit")
                           (when (and edit? band?) " capp-pdf__mark--band"))
               :data-layer (:layer-name a)
               :on-click (when on-select #(on-select (or (:row a) id)))}]
    [:g {:key (str id)}
     (if edit?
       [box-editor {:x1 (:x geom) :y1 (:y geom)
                    :x2 (+ (:x geom) (:w geom)) :y2 (+ (:y geom) (:h geom))}
        scale attrs on-change {:defer? true :band? band?
                               :on-done (when on-select #(on-select nil))}]
       [:rect (assoc attrs :x x :y y :width w :height h)
        (when label [:title label])])
     (when tag
       (caption-tag tag x y w h (or color "#fbc02d")))]))

(defn stamp-mark
  [{:keys [id geom color title text label] :as a} scale on-select]
  (let [x (* (:x geom) scale) y (* (:y geom) scale)
        w (* (:w geom) scale) h (* (:h geom) scale)
        ink (or color "#c62828")
        cx (+ x (/ w 2)) cy (+ y (/ h 2))]
    [:g {:key (str id)
         :class (str "capp-pdf__stamp" (when on-select " capp-pdf__mark--clickable"))
         :transform (str "rotate(-8 " cx " " cy ")")
         :style {:mix-blend-mode "multiply"}
         :opacity 0.85
         :on-click (when on-select #(on-select (or (:row a) id)))}
     [:rect {:x x :y y :width w :height h :rx (* 0.08 h)
             :fill ink :fill-opacity 0.05 :stroke ink :stroke-width (max 2 (* 0.05 h))}]
     [:rect {:x (+ x (* 0.06 h)) :y (+ y (* 0.06 h))
             :width (- w (* 0.12 h)) :height (- h (* 0.12 h)) :rx (* 0.05 h)
             :fill "none" :stroke ink :stroke-width (max 1 (* 0.02 h))}]
     (when title
       [:text {:x cx :y (+ y (* (if text 0.5 0.66) h)) :text-anchor "middle"
               :fill ink :font-weight 800 :font-family "Arial, Helvetica, sans-serif"
               :font-size (* (if text 0.34 0.44) h) :letter-spacing (* 0.03 h)}
        title])
     (when text
       [:text {:x cx :y (+ y (* 0.8 h)) :text-anchor "middle"
               :fill ink :font-weight 600 :font-family "Arial, Helvetica, sans-serif"
               :font-size (* 0.17 h)}
        text])
     (when label [:title label])]))

(def ^:private kinds
  {:rect rect-mark
   :band rect-mark
   :stamp stamp-mark})

(defn marks-for
  ([page-num annotations w h scale on-select]
   (marks-for page-num annotations w h scale on-select nil))
  ([page-num annotations w h scale on-select attrs]
   (into [:svg.capp-pdf__overlay
          (merge {:width w :height h :viewBox (str "0 0 " w " " h)} attrs)]
         (keep (fn [a]
                 (when (= page-num (:page a))
                   (when-let [f (kinds (keyword (:kind a)))]
                     (f (if (= :band (keyword (:kind a)))
                          (let [{:keys [x1 x2]} (band-box {} (/ w scale))]
                            (update a :geom assoc :x x1 :w (- x2 x1)))
                          a)
                        scale
                        (if (contains? a :on-select) (:on-select a) on-select))))))
         annotations)))

(def ^:private area-min-px 6)

(defn area-box
  [[x0 y0 x1 y1] scale]
  (when (and (number? scale) (pos? scale)
             (>= (abs (- x1 x0)) area-min-px) (>= (abs (- y1 y0)) area-min-px))
    (let [pt #(/ (js/Math.round (* 10 (/ % scale))) 10)]
      {:x1 (pt (min x0 x1)) :y1 (pt (min y0 y1)) :x2 (pt (max x0 x1)) :y2 (pt (max y0 y1))})))

(defn- svg-xy
  [^js e w h]
  (let [^js r (.getBoundingClientRect (.-currentTarget e))]
    [(* (- (.-clientX e) (.-left r)) (/ w (max 1 (.-width r))))
     (* (- (.-clientY e) (.-top r)) (/ h (max 1 (.-height r))))]))

(defn- area-attrs
  [n w h scale draft on-area collect? band?]
  {:class "capp-pdf__overlay--draw"
   :on-pointer-down (fn [^js e]
                      (when (zero? (.-button e))
                        (.setPointerCapture (.-currentTarget e) (.-pointerId e))
                        (let [[x y] (svg-xy e w h)]
                          (reset! draft {:page n :drawing? true
                                         :px (if band? [0 y w y] [x y x y])}))))
   :on-pointer-move (fn [^js e]
                      (when (and (:drawing? @draft) (= n (:page @draft)))
                        (let [[x y] (svg-xy e w h)]
                          (swap! draft update :px (fn [[x0 y0 x1]] [x0 y0 (if band? x1 x) y])))))
   :on-pointer-up   (fn [_]
                      (when (and (:drawing? @draft) (= n (:page @draft)))
                        (if-let [box (area-box (:px @draft) scale)]
                          (do (if collect?
                                (reset! draft nil)
                                (swap! draft assoc :drawing? false))
                              (on-area {:page n :box box}))
                          (reset! draft nil))))})

(defn- draft-mark
  [{:keys [px]}]
  (let [[x0 y0 x1 y1] px]
    [:rect.capp-pdf__draft {:x (min x0 x1) :y (min y0 y1)
                            :width (abs (- x1 x0)) :height (abs (- y1 y0))}]))

(def ^:private box-min-pt 4)

(defn adjust-box
  [{:keys [x1 y1 x2 y2] :as box} grip dx dy]
  (let [r1 #(/ (js/Math.round (* 10 %)) 10)
        g  (if (= grip :move) #{} (set (name grip)))
        m  (if (= grip :move) 1 0)]
    (assoc box
           :x1 (r1 (if (g "w") (min (+ x1 dx) (- x2 box-min-pt)) (+ x1 (* m dx))))
           :x2 (r1 (if (g "e") (max (+ x2 dx) (+ x1 box-min-pt)) (+ x2 (* m dx))))
           :y1 (r1 (if (g "n") (min (+ y1 dy) (- y2 box-min-pt)) (+ y1 (* m dy))))
           :y2 (r1 (if (g "s") (max (+ y2 dy) (+ y1 box-min-pt)) (+ y2 (* m dy)))))))

(def ^:private grips
  [[:nw 0 0] [:n 0.5 0] [:ne 1 0] [:e 1 0.5] [:se 1 1] [:s 0.5 1] [:sw 0 1] [:w 0 0.5]])

(defn- box-editor
  [_ _ _ _ _]
  (let [drag  (r/atom nil)
        node  (atom nil)
        props (atom nil)
        commit! (fn []
                  (let [{:keys [box base sent grip]} @drag]
                    (when (and box (not grip) (not sent) (not= box base))
                      (swap! drag assoc :sent true)
                      ((:on-change @props) box))))
        after! (fn [p f] (if (instance? js/Promise p) (.then p (fn [_] (f))) (f)))
        outside (fn [^js e]
                  (when-let [^js n @node]
                    (let [t (.-target e)]
                      (when-not (.contains n t)
                        (let [p    (commit!)
                              done (:on-done @props)]
                          (when (and done (some-> (.closest n ".capp-pdf") (.contains t)))
                            (after! p done)))))))]
    (r/create-class
     {:display-name "capp-pdf-box-editor"
      :component-did-mount
      (fn [] (when (:defer? @props)
               (.addEventListener js/document "pointerdown" outside true)))
      :component-will-unmount
      (fn [] (.removeEventListener js/document "pointerdown" outside true)
        (when (:defer? @props) (commit!)))
      :reagent-render
      (fn [box scale attrs on-change {:keys [defer? on-done band?]}]
        (reset! props {:on-change on-change :on-done on-done :defer? defer?})
        (let [d @drag
              shown (if (and d (or (:grip d) (= box (:from d)))) (:box d) box)
              {:keys [x1 y1 x2 y2]} shown
              x (* x1 scale) y (* y1 scale) w (* (- x2 x1) scale) h (* (- y2 y1) scale)
              start (fn [grip ^js e]
                      (when (zero? (.-button e))
                        (.stopPropagation e)
                        (.setPointerCapture (.-currentTarget e) (.-pointerId e))
                        (let [^js svg (.-ownerSVGElement (.-currentTarget e))
                              k (/ (.. svg -viewBox -baseVal -width)
                                   (max 1 (.-width (.getBoundingClientRect svg)))
                                   scale)]
                          (reset! drag {:grip grip :at [(.-clientX e) (.-clientY e)] :k k
                                        :from shown :box shown :base box}))))
              move (fn [^js e]
                     (when-let [{:keys [grip at k from]} @drag]
                       (when grip
                         (.stopPropagation e)
                         (let [[cx cy] at]
                           (swap! drag assoc :box
                                  (adjust-box from grip
                                              (if band? 0 (* k (- (.-clientX e) cx)))
                                              (* k (- (.-clientY e) cy))))))))
              end (fn [^js e]
                    (when-let [{nb :box from :from base :base grip :grip} @drag]
                      (when grip
                        (.stopPropagation e)
                        (if (= nb base)
                          (reset! drag nil)
                          (do (swap! drag assoc :grip nil :from base)
                              (when (and (not defer?) (not= nb from))
                                (on-change nb)))))))
              handlers (fn [grip] {:on-pointer-down #(start grip %) :on-pointer-move move
                                   :on-pointer-up end :on-pointer-cancel end})]
          (into [:g {:ref #(reset! node %)}
                 [:rect (merge attrs {:x x :y y :width w :height h} (handlers :move))]]
                (if band?
                  (for [[g fy] [[:n 0] [:s 1]]]
                    ^{:key g}
                    [:rect.capp-pdf__grip.capp-pdf__grip--line
                     (merge {:x x :y (- (+ y (* fy h)) 5) :width w :height 10} (handlers g))])
                  (for [[g fx fy] grips]
                    ^{:key g}
                    [:rect.capp-pdf__grip
                     (merge {:x (- (+ x (* fx w)) 4) :y (- (+ y (* fy h)) 4)
                             :width 8 :height 8
                             :style {:cursor (str (name g) "-resize")}}
                            (handlers g))])))))})))

(defn- entwurf-mark
  [{:keys [i page box]} scale on-remove on-change band-w]
  (let [{:keys [x1 y1 x2 y2] :as box} (if band-w (band-box box band-w) box)
        x (* x1 scale) y (* y1 scale) w (* (- x2 x1) scale) h (* (- y2 y1) scale)
        kreuz-x (if band-w (- (+ x w) 12) (+ x w))]
    [:g
     (if on-change
       [box-editor box scale
        {:class (str "capp-pdf__draft capp-pdf__draft--edit"
                     (when band-w " capp-pdf__mark--band"))}
        #(on-change i page %) {:band? (some? band-w)}]
       [:rect.capp-pdf__draft {:x x :y y :width w :height h}])
     (when on-remove
       [:g.capp-pdf__draft-x {:on-pointer-down (fn [^js e] (.stopPropagation e))
                              :on-click (fn [^js e] (.stopPropagation e) (on-remove i))}
        [:title "Rahmen wegnehmen"]
        [:circle {:cx kreuz-x :cy y :r 9}]
        [:text {:x kreuz-x :y (+ y 4) :text-anchor "middle"} "×"]])]))

(defn- paint-text!
  [^js tl ^js page ^js vp]
  (when-let [TextLayer (some-> (pdfjs) (aget "TextLayer"))]
    (set! (.-textContent tl) "")
    (.setProperty (.-style (.-parentNode tl)) "--scale-factor" (str (.-scale vp)))
    (-> (TextLayer. #js {:textContentSource (.streamTextContent page)
                         :container tl :viewport vp})
        .render
        (.catch (fn [_])))))

(def ^:private max-canvas-px 6e6)

(defn backing-ratio
  [w h dpr]
  (let [dpr (if (and (number? dpr) (pos? dpr)) dpr 1)
        px  (* w h)]
    (if (pos? px) (min dpr (js/Math.sqrt (/ max-canvas-px px))) dpr)))

(defn- unscale! [^js pg]
  (set! (.. pg -style -transform) "")
  (set! (.. pg -style -transformOrigin) ""))

(defn- page-view
  [_doc n _scale _annotations _on-select _area]
  (let [dims    (r/atom nil)
        cv      (atom nil)
        tl      (atom nil)
        task    (atom nil)
        painted (atom nil)]
    (letfn [(paint! [^js doc scale]
              (when (and @cv (number? scale) (pos? scale) (not= scale @painted))
                (reset! painted scale)
                (some-> ^js @task .cancel)
                (-> (.getPage doc n)
                    (.then (fn [^js page]
                             (let [^js vp (.getViewport page #js {:scale scale})
                                   k      (backing-ratio (.-width vp) (.-height vp)
                                                         (.-devicePixelRatio js/window))
                                   cw     (js/Math.floor (* k (.-width vp)))
                                   ch     (js/Math.floor (* k (.-height vp)))
                                   ^js off (.createElement js/document "canvas")]
                               (set! (.-width off) cw)
                               (set! (.-height off) ch)
                               (reset! task (.render page #js {:canvasContext (.getContext off "2d")
                                                               :viewport vp
                                                               :transform #js [k 0 0 k 0 0]}))
                               (-> (.-promise ^js @task)
                                   (.then
                                    (fn []
                                      (when-let [^js c @cv]
                                        (set! (.-width c) cw)
                                        (set! (.-height c) ch)
                                        (.drawImage (.getContext c "2d") off 0 0)
                                        (when-let [^js pg (.-parentNode c)]
                                          (set! (.. pg -style -width) (str (.-width vp) "px"))
                                          (set! (.. pg -style -height) (str (.-height vp) "px"))
                                          (unscale! pg))
                                        (reset! dims {:w (.-width vp) :h (.-height vp)
                                                      :scale (.-scale vp)})
                                        (some-> @tl (paint-text! page vp)))
                                      (set! (.-width off) 0)
                                      (set! (.-height off) 0)))
                                   (.catch (fn [_]
                                             (set! (.-width off) 0)
                                             (when (= scale @painted)
                                               (some-> ^js @cv .-parentNode unscale!))))))))
                    (.catch (fn [_])))))
            (repaint! [this]
              (let [[_ doc _n scale] (r/argv this)]
                (paint! doc scale)))]
      (r/create-class
       {:display-name         "capp-pdf-page"
        :component-did-mount  repaint!
        :component-did-update repaint!
        :reagent-render
        (fn [_doc _n _scale annotations on-select
             {:keys [draft on-area collect? area-band? drafts on-remove-draft on-change-draft]}]
          (let [{:keys [w h scale]} @dims
                d (when (and draft (= n (:page @draft))) @draft)]
            [:div.capp-pdf__page {:style {:width w :height h}}
             [:canvas {:ref #(reset! cv %)}]
             [:div.textLayer {:ref #(reset! tl %)}]
             (when scale
               (cond-> (marks-for n annotations w h scale on-select
                                  (when on-area
                                    (area-attrs n w h scale draft on-area collect? area-band?)))
                 true (into (comp (filter #(= n (:page %)))
                                  (map (fn [d]
                                         ^{:key (str "entwurf-" (:i d))}
                                         [entwurf-mark d scale on-remove-draft
                                          on-change-draft
                                          (when area-band? (/ w scale))])))
                            drafts)
                 d (conj (draft-mark d))))]))}))))

(defn- scroll-to-mark!
  [^js el {:keys [page geom]} scale]
  (when-let [^js pg (some-> el (.querySelectorAll ".capp-pdf__page") (aget (dec page)))]
    (if (> (.-scrollHeight el) (+ 4 (.-clientHeight el)))
      (let [top (+ (- (.-top (.getBoundingClientRect pg)) (.-top (.getBoundingClientRect el)))
                   (.-scrollTop el) (* (or (:y geom) 0) (or scale 1)) -32)]
        (.scrollTo el #js {:top (max 0 top) :behavior "smooth"}))
      (.scrollIntoView pg #js {:block "start" :behavior "smooth"}))))

(def ^:private such-farben ["#2e7d32" "#1565c0" "#ef6c00" "#6a1b9a" "#c62828"])

(defn- datum-formen
  [v]
  (when-let [[_ y m d] (re-find #"^(\d{4})-(\d{2})-(\d{2})" (str v))]
    (let [m' (str (js/parseInt m 10)) d' (str (js/parseInt d 10))]
      (distinct [(str d "." m "." y) (str d' "." m' "." y) (str d "." m "." (subs y 2))]))))

(defn- betrag-formen
  [v]
  (let [n (if (number? v) v (js/parseFloat (str v)))]
    (when-not (js/isNaN n)
      (let [f #(.toLocaleString n "de-AT" #js {:minimumFractionDigits 2
                                               :maximumFractionDigits 2
                                               :useGrouping %})]
        (distinct [(f true) (f false)])))))

(defn- gedruckt
  [v kind]
  (case kind
    :datum  (datum-formen v)
    :betrag (betrag-formen v)
    :text   [(str/trim (str v))]
    (or (datum-formen v) (when (number? v) (betrag-formen v)) [(str/trim (str v))])))

(defn- muster
  [n]
  (js/RegExp. (str/join "\\s+" (map #(str/replace % #"[.*+?^${}()|\[\]\\]" "\\$&")
                                    (str/split (str/trim n) #"\s+")))
              "i"))

(defn- suchbegriffe
  [search]
  (->> (if (sequential? search) search [search])
       (keep (fn [x]
               (let [{:keys [text label kind]} (if (map? x) x {:text x})]
                 (when-not (str/blank? (str text))
                   {:label (or label (str text))
                    :muster (mapv muster (remove str/blank? (gedruckt text kind)))}))))
       (map-indexed (fn [i b] (assoc b :color (such-farben (mod i (count such-farben))))))
       vec))

(defn- fundstelle
  [s muster]
  (some #(when-let [^js m (.exec % s)] [(.-index m) (count (aget m 0))]) muster))

(defonce ^:private mess-ctx (delay (.getContext (.createElement js/document "canvas") "2d")))

(defn- anteil
  [family s at len]
  (let [^js ctx @mess-ctx
        _ (set! (.-font ctx) (str "100px " (or family "sans-serif")))
        b #(.-width (.measureText ctx %))
        ganz (b s)]
    (if (pos? ganz)
      [(/ (b (subs s 0 at)) ganz) (/ (b (subs s at (+ at len))) ganz)]
      (let [k (max 1 (count s))] [(/ at k) (/ len k)]))))

(defn- page-hits
  [^js page p begriffe]
  (let [^js vp (.getViewport page #js {:scale 1})]
    (-> (.getTextContent page)
        (.then
         (fn [^js tc]
           (into []
                 (comp
                  (map-indexed vector)
                  (mapcat
                   (fn [[i ^js it]]
                     (let [s (or (.-str it) "")]
                       (keep (fn [{:keys [label muster color]}]
                               (when-let [[at len] (fundstelle s muster)]
                                 (let [t  (.-transform it)
                                       h0 (.-height it)
                                       h  (if (pos? h0) h0 (js/Math.hypot (aget t 2) (aget t 3)))
                                       pt (.convertToViewportPoint vp (aget t 4) (aget t 5))
                                       w  (.-width it)
                                       family (some-> (.-styles tc)
                                                      (aget (.-fontName it)) .-fontFamily)
                                       [von breit] (anteil family s at len)]
                                   {:id (str "suche-" p "-" i "-" label) :kind :rect :page p
                                    :geom {:x (+ (aget pt 0) (* w von)) :y (- (aget pt 1) h)
                                           :w (* w breit) :h h}
                                    :color color :label label})))
                             begriffe)))))
                 (array-seq (.-items tc))))))))

(defn- sichtbar
  [^js page hits]
  (if (empty? hits)
    (js/Promise.resolve hits)
    (let [s 2 ^js vp (.getViewport page #js {:scale s})
          c (.createElement js/document "canvas")
          ctx (.getContext c "2d" #js {:willReadFrequently true})]
      (set! (.-width c) (.-width vp))
      (set! (.-height c) (.-height vp))
      (-> (.-promise (.render page #js {:canvasContext ctx :viewport vp}))
          (.then
           (fn []
             (filterv
              (fn [{{:keys [x y w h]} :geom}]
                (let [bx (max 0 (js/Math.floor (* s x))) by (max 0 (js/Math.floor (* s y)))
                      bw (max 1 (js/Math.ceil (* s w))) bh (max 1 (js/Math.ceil (* s h)))
                      ^js d (.-data (.getImageData ctx bx by bw bh))]
                  (loop [i 0 lo 255 hi 0]
                    (if (< i (.-length d))
                      (let [l (if (zero? (aget d (+ i 3)))
                                255
                                (/ (+ (aget d i) (aget d (+ i 1)) (aget d (+ i 2))) 3))]
                        (recur (+ i 4) (min lo l) (max hi l)))
                      (> (- hi lo) 60)))))
              hits)))
          (.catch (fn [_] hits))))))

(defn search-marks
  [^js doc search on-done]
  (let [begriffe (suchbegriffe search)]
    (if (empty? begriffe)
      (on-done [])
      (-> (js/Promise.all
           (clj->js (for [p (range 1 (inc (.-numPages doc)))]
                      (-> (.getPage doc p)
                          (.then (fn [^js pg]
                                   (-> (page-hits pg p begriffe) (.then #(sichtbar pg %)))))))))
          (.then (fn [pages] (on-done (into [] cat (array-seq pages)))))
          (.catch (fn [_] (on-done [])))))))

(defn- such-zeile
  [search marks]
  (let [n (frequencies (map :label marks))]
    (into [:div.capp-pdf__search]
          (interpose " · ")
          (for [{:keys [label color]} (suchbegriffe search)]
            (if (pos? (n label 0))
              [:span {:style {:color color}} (str label " ✓")]
              [ui/tooltip {:content "nicht im Beleg gefunden"}
               [:span {:style {:text-decoration "line-through"}} label]])))))

(defonce ^:private print-frame (atom nil))

(defn- print-pdf!
  [src]
  (let [^js f (or @print-frame
                  (let [f (.createElement js/document "iframe")]
                    (set! (.. f -style -cssText)
                          "position:fixed;right:0;bottom:0;width:0;height:0;border:0")
                    (.appendChild (.-body js/document) f)
                    (reset! print-frame f)))]
    (set! (.-onload f)
          (fn []
            (try (.. f -contentWindow focus)
                 (.. f -contentWindow print)
                 (catch :default _ (js/window.open src "_blank")))))
    (set! (.-src f) src)))

(defn fit-scale
  [box natural]
  (when (and (number? box) (number? natural) (pos? box) (pos? natural))
    (/ box natural)))

(def ^:private zoom-min 0.5)
(def ^:private zoom-max 4)
(def ^:private zoom-step 1.2)

(defn- zoom-key [] (str "sutra.pdf-zoom." (or (:username @auth/auth) "-")))

(defn- stored-zoom []
  (let [z (js/parseFloat (storage/get-item (zoom-key)))]
    (if (and (number? z) (js/isFinite z)) (min zoom-max (max zoom-min z)) 1)))

(defn- store-zoom! [z]
  (storage/set-item! (zoom-key) z))

(def ^:private tag-settings
  {:value-mode {:key "sutra.pdf-captions."
                :read {"1" :all "none" :none}
                :default :selected
                :write {:all "1" :none "none" :selected "0"}}
   :names?     {:key "sutra.pdf-caption-names."
                :read {"0" false}
                :default true
                :write {false "0" true "1"}}})

(defn- setting-key [k]
  (str (get-in tag-settings [k :key]) (or (:username @auth/auth) "-")))

(defn- stored-tag-settings []
  (into {}
        (map (fn [[k {:keys [read default]}]]
               [k (get read (storage/get-item (setting-key k)) default)]))
        tag-settings))

(defn- store-tag-setting! [k v]
  (storage/set-item! (setting-key k) (get-in tag-settings [k :write v])))

(def ^:private value-mode-ui
  {:selected {:label "🔤 Wert: Auswahl"
              :title "Wert an der gewählten Markierung — klicken: an allen"}
   :all      {:label "🔤 Wert: alle" :title "Wert an allen Markierungen — klicken: ausblenden"}
   :none     {:label "🔤 Wert: aus" :title "Wert ausgeblendet — klicken: an der gewählten"}})

(defn- clamp-zoom [z]
  (/ (js/Math.round (* 1000 (min zoom-max (max zoom-min z)))) 1000))

(defn- scroller-y
  [^js el]
  (loop [^js n el]
    (let [^js cs (some-> n js/getComputedStyle)]
      (cond (nil? n) (.-scrollingElement js/document)
            (and (#{"auto" "scroll"} (.-overflowY cs))
                 (or (> (.-scrollHeight n) (inc (.-clientHeight n)))
                     (and (identical? n el) (not= "none" (.-maxHeight cs))))) n
            :else (recur (.-parentElement n))))))

(defn- zoom-anchor
  [^js el x y]
  (let [pages (array-seq (.querySelectorAll el ".capp-pdf__page"))
        dist  (fn [^js pg]
                (let [r (.getBoundingClientRect pg)]
                  (max 0 (- (.-top r) y) (- y (.-bottom r)))))]
    (when-let [^js pg (when (seq pages) (apply min-key dist (reverse pages)))]
      (let [r (.getBoundingClientRect pg)]
        (when (and (pos? (.-width r)) (pos? (.-height r)))
          {:page pg :x x :y y
           :fx (/ (- x (.-left r)) (.-width r))
           :fy (/ (- y (.-top r)) (.-height r))})))))

(defn- hold-anchor!
  [^js el gen* {:keys [^js page x y fx fy]}]
  (let [gen  (swap! gen* inc)
        ro*  (atom nil)
        fix! (fn []
               (if (or (not= gen @gen*) (not (.-isConnected page)))
                 (some-> ^js @ro* .disconnect)
                 (let [r  (.getBoundingClientRect page)
                       dx (- (+ (.-left r) (* fx (.-width r))) x)
                       dy (- (+ (.-top r) (* fy (.-height r))) y)
                       ^js s (scroller-y el)]
                   (when (>= (abs dx) 0.5)
                     (set! (.-scrollLeft el) (+ (.-scrollLeft el) dx)))
                   (when (and s (>= (abs dy) 0.5))
                     (set! (.-scrollTop s) (+ (.-scrollTop s) dy))))))]
    (when (exists? js/ResizeObserver)
      (let [^js ro (js/ResizeObserver. (fn [_] (fix!)))]
        (reset! ro* ro)
        (doseq [pg (array-seq (.querySelectorAll el ".capp-pdf__page"))]
          (.observe ro pg))
        (js/setTimeout #(.disconnect ro) 1500)))
    fix!))

(def ^:private double-tap-ms 300)
(def ^:private double-tap-px 30)
(def ^:private tap-slop-px 10)
(def ^:private not-a-tap
  "a, button, input, .capp-pdf__grip, .capp-pdf__draft, .capp-pdf__overlay--draw")

(defn ink-span
  [^js data w h]
  (let [need   (max 1 (* 0.002 h))
        inked? (fn [col]
                 (loop [row 0 n 0]
                   (cond (>= n need) true
                         (>= row h)  false
                         :else
                         (let [i (* 4 (+ col (* row w)))]
                           (recur (inc row)
                                  (if (< (min (aget data i) (aget data (+ i 1)) (aget data (+ i 2)))
                                         235)
                                    (inc n)
                                    n))))))
        l      (first (filter inked? (range w)))]
    (when l
      [(/ l w) (/ (inc (first (filter inked? (range (dec w) -1 -1)))) w)])))

(defn- canvas-ink
  [^js c]
  (when (and (pos? (.-width c)) (pos? (.-height c)))
    (let [w    (min 400 (.-width c))
          h    (js/Math.max 1 (js/Math.round (* (.-height c) (/ w (.-width c)))))
          ^js o (.createElement js/document "canvas")
          _    (set! (.-width o) w)
          _    (set! (.-height o) h)
          ^js g (.getContext o "2d")]
      (.drawImage g c 0 0 w h)
      (when-let [[l r] (ink-span (.-data (.getImageData g 0 0 w h)) w h)]
        [(max 0 (- l 0.015)) (min 1 (+ r 0.015))]))))

(defn- jpeg-bytes
  [^js canvas]
  (let [b64 (subs (.toDataURL canvas "image/jpeg" 0.9) (count "data:image/jpeg;base64,"))]
    (js/Uint8Array.from (js/atob b64) #(.charCodeAt % 0))))

(defn- burn-marks!
  [^js ctx annotations n k]
  (doseq [{:keys [page geom color]} annotations
          :when (and (= n page) geom)]
    (set! (.-globalAlpha ctx) 0.3)
    (set! (.-fillStyle ctx) (or color "#FBD065"))
    (.fillRect ctx (* k (:x geom)) (* k (:y geom)) (* k (:w geom)) (* k (:h geom)))
    (set! (.-globalAlpha ctx) 1)))

(defn- page-jpeg!
  [^js doc n annotations]
  (-> (.getPage doc n)
      (.then (fn [^js page]
               (let [k  2
                     ^js vp (.getViewport page #js {:scale k})
                     ^js c  (.createElement js/document "canvas")]
                 (set! (.-width c) (.-width vp))
                 (set! (.-height c) (.-height vp))
                 (let [ctx (.getContext c "2d")]
                   (-> (.-promise (.render page #js {:canvasContext ctx :viewport vp}))
                       (.then (fn []
                                (burn-marks! ctx annotations n k)
                                #js [{:w (/ (.-width vp) k) :h (/ (.-height vp) k)
                                      :px-w (.-width c) :px-h (.-height c)}
                                     (jpeg-bytes c)])))))))))

(defn- export-annotated!
  [^js doc annotations filename]
  (-> (js/Promise.all
       (clj->js (for [n (range 1 (inc (.-numPages doc)))] (page-jpeg! doc n annotations))))
      (.then (fn [pages]
               (let [ps     (array-seq pages)
                     infos  (mapv #(aget % 0) ps)
                     images (mapv #(aget % 1) ps)
                     parts  (pdf-export/parts infos (mapv #(.-length %) images))]
                 (dl/blob!
                  (js/Blob. (into-array (map (fn [p] (if (string? p) p (nth images (:image p))))
                                             parts))
                            #js {:type "application/pdf"})
                  filename))))))

(defn- client-rects
  [^js range]
  (mapv (fn [^js r] {:left (.-left r) :top (.-top r) :width (.-width r) :height (.-height r)})
        (array-seq (.getClientRects range))))

(defn- text-selection
  [^js root scale]
  (let [^js sel (js/window.getSelection)]
    (when (and root sel (not (.-isCollapsed sel)) (pos? (.-rangeCount sel)))
      (let [^js node (.-anchorNode sel)
            ^js el   (if (= 3 (.-nodeType node)) (.-parentElement node) node)
            ^js pg   (some-> el (.closest ".capp-pdf__page"))]
        (when (and pg (.contains root pg))
          (let [^js r    (.getBoundingClientRect pg)
                pages    (vec (array-seq (.querySelectorAll root ".capp-pdf__page")))
                n        (inc (.indexOf pages pg))
                boxes    (pdf-layers/rects->boxes (client-rects (.getRangeAt sel 0))
                                                  {:left (.-left r) :top (.-top r)}
                                                  scale)]
            (when (seq boxes)
              (pdf-layers/selection-row n boxes (.toString sel)))))))))

(defn- current-page
  [^js root]
  (let [mid   (/ (.-innerHeight js/window) 2)
        spans (mapv (fn [^js pg]
                      (let [r (.getBoundingClientRect pg)] [(.-top r) (.-bottom r)]))
                    (array-seq (.querySelectorAll root ".capp-pdf__page")))]
    (pdf-layers/page-at spans mid)))

(defn- floating
  [{:keys [x y on-close]} & body]
  [overlay/popover {:at [x y] :on-close on-close}
   (into [:div.capp-pdf__popover {:on-mouse-up #(.stopPropagation %)
                                  :on-pointer-up #(.stopPropagation %)}
          [ui/button {:class "capp-pdf__popover-close" :variant :minimal :size :small
                      :icon :cross :aria-label "Schließen" :on-click on-close}]]
         body)])

(defn- search-field
  [{:keys [value count cursor on-input on-step]}]
  [:span.capp-pdf__searchbox
   [input-group {:small true :type "search" :value (or value "") :placeholder "Suchen…"
                 :on-change #(on-input (.. % -target -value))
                 :on-key-down #(when (= "Enter" (.-key %))
                                 (on-step (if (.-shiftKey %) -1 1)))}]
   (when (seq value)
     [:span.capp-pdf__hits (if (pos? count) (str (inc cursor) " / " count) "0")])
   (when (pos? count)
     [:<>
      [ui/button {:minimal true :small true :icon :chevron-up
                  :aria-label "Vorheriger Treffer" :on-click #(on-step -1)}]
      [ui/button {:minimal true :small true :icon :chevron-down
                  :aria-label "Nächster Treffer" :on-click #(on-step 1)}]])])

(defn- finger-spread
  [^js ts]
  (let [a (aget ts 0) b (aget ts 1)]
    {:d (js/Math.hypot (- (.-clientX a) (.-clientX b)) (- (.-clientY a) (.-clientY b)))
     :x (/ (+ (.-clientX a) (.-clientX b)) 2)
     :y (/ (+ (.-clientY a) (.-clientY b)) 2)}))

(defn- pinch-ratio
  [f0 d0 d]
  (/ (clamp-zoom (* f0 (/ d d0))) f0))

(defn- wheel-step
  [delta-y delta-mode]
  (let [px (* delta-y (case delta-mode 1 33 2 800 1))]
    (js/Math.pow zoom-step (/ (max -100 (min 100 (- px))) 100))))

(defn- tap-kind
  [{:keys [x y t]} start last tap-target?]
  (let [{sx :x sy :y} start
        {px :x py :y pt :t} last]
    (cond
      (not (and tap-target? sx (< (js/Math.hypot (- x sx) (- y sy)) tap-slop-px))) :none
      (and px (< (- t pt) double-tap-ms) (< (js/Math.hypot (- x px) (- y py)) double-tap-px))
      :double
      :else :single)))

(defn- download-name [filename]
  (let [f (str/trim (str filename))]
    (cond (str/blank? f)                         "dokument.pdf"
          (str/ends-with? (str/lower-case f) ".pdf") f
          :else                                  (str f ".pdf"))))

(defn- fields-of [a] (pdf-layers/tag-fields (:caption a)))

(defn- topmost
  [marks]
  (apply min-key (fn [{:keys [page geom]}] (+ (* 1e5 page) (:y geom))) marks))

(defn- measure! [{:keys [box]} el]
  (let [^js cs (js/getComputedStyle el)
        w (- (.-clientWidth ^js el)
             (js/parseFloat (.-paddingLeft cs))
             (js/parseFloat (.-paddingRight cs)))]
    (when (or (nil? @box) (>= (abs (- w @box)) 1)) (reset! box w))))

(defn- zoom-held!
  [{:keys [factor holding el* anchor-gen]} z anchor-of]
  (let [z (clamp-zoom z)]
    (when (not= z @factor)
      (some-> @holding (apply []))
      (let [a (anchor-of)]
        (reset! factor z)
        (store-zoom! z)
        (reset! holding (when a (hold-anchor! @el* anchor-gen a)))))))

(defn- zoom!
  ([{:keys [el*] :as c} z]
   (let [^js el @el*
         r  (some-> el .getBoundingClientRect)
         y0 (some-> r .-top (max 0))
         y1 (some-> r .-bottom (min (.-innerHeight js/window)))]
     (if r
       (zoom! c z (+ (.-left r) (/ (.-width r) 2)) (/ (+ y0 y1) 2))
       (zoom! c z nil nil))))
  ([{:keys [el*] :as c} z x y]
   (zoom-held! c z #(when (and @el* x y) (zoom-anchor @el* x y)))))

(defn- fit-ink! [{:keys [el* box factor] :as c} ^js pg y]
  (when-let [[l r] (some-> (.querySelector pg "canvas") canvas-ink)]
    (let [^js el @el*
          pr (.getBoundingClientRect pg)
          x0 (+ (.-left (.getBoundingClientRect el)) (/ (- (.-clientWidth el) @box) 2))]
      (when (and @box (pos? (.-width pr)))
        (zoom-held! c
                    (* @factor (/ @box (* (- r l) (.-width pr))))
                    (constantly {:page pg :x x0 :y y :fx l
                                 :fy (/ (- y (.-top pr)) (.-height pr))}))))))

(defn- double-tap! [{:keys [factor] :as c} ^js pg x y]
  (if (> @factor 1.02) (zoom! c 1 x y) (fit-ink! c pg y)))

(defn- on-wheel [{:keys [factor anchor-gen] :as c} ^js e]
  (if (or (.-ctrlKey e) (.-metaKey e))
    (do (.preventDefault e)
        (zoom! c (* @factor (wheel-step (.-deltaY e) (.-deltaMode e))) (.-clientX e) (.-clientY e)))
    (swap! anchor-gen inc)))

(defn- pages [{:keys [el*]}] (array-seq (.querySelectorAll ^js @el* ".capp-pdf__page")))

(defn- on-touch-start [{:keys [anchor-gen holding tap-start pinch factor] :as c} ^js e]
  (when (= 1 (.. e -touches -length))
    (swap! anchor-gen inc)
    (reset! holding nil)
    (let [^js t (aget (.-touches e) 0)]
      (reset! tap-start {:x (.-clientX t) :y (.-clientY t)})))
  (when (= 2 (.. e -touches -length))
    (reset! tap-start nil)
    (let [{:keys [d x y]} (finger-spread (.-touches e))]
      (when (pos? d)
        (doseq [^js pg (pages c)]
          (let [r (.getBoundingClientRect pg)]
            (set! (.. pg -style -transformOrigin)
                  (str (- x (.-left r)) "px " (- y (.-top r)) "px"))))
        (reset! pinch {:d0 d :f0 @factor :x x :y y :r 1})))))

(defn- on-touch-move [{:keys [pinch] :as c} ^js e]
  (when-let [{:keys [d0 f0]} (and (= 2 (.. e -touches -length)) @pinch)]
    (.preventDefault e)
    (let [r (pinch-ratio f0 d0 (:d (finger-spread (.-touches e))))]
      (swap! pinch assoc :r r)
      (doseq [^js pg (pages c)]
        (set! (.. pg -style -transform) (str "scale(" r ")"))))))

(defn- on-tap-end [{:keys [last-tap tap-start] :as c} ^js e]
  (let [^js t  (aget (.-changedTouches e) 0)
        ^js pg (some-> (.-target e) (.closest ".capp-pdf__page"))
        now    {:x (.-clientX t) :y (.-clientY t) :t (.now js/Date)}]
    (case (tap-kind now @tap-start @last-tap
                    (and pg (not (.closest (.-target e) not-a-tap))))
      :none   (reset! last-tap nil)
      :double (do (.preventDefault e)
                  (reset! last-tap nil)
                  (double-tap! c pg (:x now) (:y now)))
      :single (reset! last-tap now))))

(defn- on-touch-end [{:keys [pinch factor] :as c} ^js e]
  (when (and (nil? @pinch) (zero? (.. e -touches -length)) (= "touchend" (.-type e)))
    (on-tap-end c e))
  (when-let [{:keys [f0 x y r]} (and (< (.. e -touches -length) 2) @pinch)]
    (reset! pinch nil)
    (when (= (clamp-zoom (* f0 r)) @factor)
      (run! unscale! (pages c)))
    (zoom! c (* f0 r) x y)))

(defn- on-scroll [{:keys [el* props* page-out page-now]} _]
  (when-let [out (and @el* (:page-var @props*))]
    (when-let [n (current-page @el*)]
      (when (not= n @page-out)
        (reset! page-out n)
        (reset! page-now n)
        (sel/wr! out n)))))

(defn- listen!
  [{:keys [h]} ^js el on?]
  (let [f (if on? #(.addEventListener el %1 %2 #js {:passive false})
              #(.removeEventListener el %1 %2))]
    (f "wheel" (:wheel h))
    (f "touchstart" (:touch-start h))
    (f "touchmove" (:touch-move h))
    (f "touchend" (:touch-end h))
    (f "touchcancel" (:touch-end h))
    (f "gesturestart" (:gesture h))
    (f "gesturechange" (:gesture h))
    (if on?
      (.addEventListener js/window "scroll" (:scroll h) #js {:capture true :passive true})
      (.removeEventListener js/window "scroll" (:scroll h) #js {:capture true}))))

(defn- open-create! [{:keys [props* pop-at xy]} row]
  (when (seq (:create @props*))
    (reset! pop-at (assoc @xy :kind (if (seq (:text row)) :text :area) :row row))))

(defn- on-text-selection [{:keys [props* el*] :as c} _]
  (let [{:keys [selected-text area-boxes area-page scale]} @props*]
    (when-let [row (text-selection @el* scale)]
      (sel/wr! selected-text (:text row))
      (sel/wr! area-page (:page row))
      (sel/wr! area-boxes (:boxes row))
      (open-create! c row))))

(defn- mount! [{:keys [el* ro] :as c} el]
  (when (and @el* (not= el @el*))
    (listen! c @el* false))
  (reset! el* el)
  (if el
    (do (when (and (nil? @ro) (exists? js/ResizeObserver))
          (reset! ro (js/ResizeObserver. (fn [] (measure! c el))))
          (.observe ^js @ro el))
        (listen! c el true)
        (measure! c el))
    (do (some-> ^js @ro .disconnect) (reset! ro nil))))

(defn- context
  []
  (let [c {:doc (r/atom nil) :term (r/atom nil) :cursor (r/atom 0) :pop-at (r/atom nil)
           :xy (atom nil) :page-now (atom nil) :page-out (atom nil) :props* (atom nil)
           :factor (r/atom (stored-zoom)) :error (r/atom nil) :for* (atom nil)
           :box (r/atom nil) :natural (r/atom nil) :hits (r/atom nil) :el* (atom nil)
           :anchor-gen (atom 0) :holding (atom nil) :ro (atom nil) :draft (r/atom nil)
           :seen (atom nil) :hidden? (r/atom false) :tags (r/atom (stored-tag-settings))
           :draw? (r/atom false) :sel-for (atom nil) :pinch (atom nil) :last-tap (atom nil)
           :tap-start (atom nil)}]
    (assoc c
           :h {:wheel       #(on-wheel c %)
               :touch-start #(on-touch-start c %)
               :touch-move  #(on-touch-move c %)
               :touch-end   #(on-touch-end c %)
               :gesture     #(.preventDefault ^js %)
               :scroll      #(on-scroll c %)}
           :mount!          #(mount! c %)
           :pointer-up      #(reset! (:xy c) {:x (.-clientX ^js %) :y (.-clientY ^js %)})
           :text-selection  #(on-text-selection c %))))

(defn- keep-draft!
  [{:keys [seen draft]} annotations]
  (when (not= @seen (count annotations))
    (when (and (some? @seen) (not (:drawing? @draft))) (reset! draft nil))
    (reset! seen (count annotations))))

(defn- load-doc! [{:keys [for* doc natural error]} src]
  (when (and src (not= src @for*))
    (reset! for* src)
    (reset! doc nil)
    (reset! natural nil)
    (reset! error nil)
    (-> (vendor/load! :pdfjs)
        (.then (fn [^js lib] (.-promise ^js (.getDocument lib (clj->js {:url src})))))
        (.then (fn [^js d]
                 (-> (.getPage d 1)
                     (.then (fn [^js p]
                              (reset! natural (.-width (.getViewport p #js {:scale 1})))
                              (reset! doc d))))))
        (.catch #(reset! error (str %))))))

(defn- search! [{:keys [doc hits cursor el*]} search]
  (when (and @doc (not= [@doc search] (:for @hits)))
    (let [k [@doc search]]
      (reset! hits {:for k :marks []})
      (search-marks @doc search
                    (fn [ms]
                      (when (= k (:for @hits))
                        (reset! hits {:for k :marks ms})
                        (reset! cursor 0)
                        (when-let [p (some-> (first ms) :page)]
                          (when (> p 1)
                            (some-> @el* (.querySelectorAll ".capp-pdf__page")
                                    (aget (dec p))
                                    (.scrollIntoView #js {:block "start"}))))))))))

(defn- follow-page! [{:keys [doc page-now page-out el*]} wanted scale]
  (when (and (number? wanted) @doc (not= wanted @page-now))
    (reset! page-now wanted)
    (reset! page-out wanted)
    (js/setTimeout #(scroll-to-mark! @el* {:page wanted :geom {:y 0}} scale) 0)))

(defn- follow-selection! [{:keys [sel-for doc el*]} annotations scale]
  (let [chosen (filter :selected annotations)
        k      (sort (map :id chosen))]
    (when (not= k @sel-for)
      (reset! sel-for k)
      (when (and (seq chosen) @doc)
        (let [m (topmost chosen)]
          (js/setTimeout #(scroll-to-mark! @el* m scale) 0))))))

(defn- step! [{:keys [hits cursor el*]} scale dir]
  (let [ms (:marks @hits)
        i  (swap! cursor pdf-layers/step-cursor (count ms) dir)]
    (when-let [m (get (vec ms) i)] (scroll-to-mark! @el* m scale))))

(defn- with-pop
  [{:keys [pop-at xy]} on-select {:keys [popover] :as a}]
  (if-not popover
    a
    (let [own (if (contains? a :on-select) (:on-select a) on-select)]
      (assoc a :on-select (fn [row]
                            (when own (own row))
                            (reset! pop-at (assoc @xy :popover popover :row row)))))))

(defn- shown-annotations [{:keys [hidden? tags] :as c} annotations on-select]
  (if @hidden?
    []
    (mapv (fn [a]
            (let [a' (with-pop c on-select a)]
              (if-let [t (pdf-layers/tag-segments (fields-of a) @tags (:selected a))]
                (assoc a' :tag t)
                a')))
          annotations)))

(defn- bar-link
  [{:keys [tip class on]} label]
  (let [a [:a.capp-pdf__download {:href "#" :class class
                                  :on-click (fn [e] (.preventDefault e) (on))}
           label]]
    (if tip [ui/tooltip {:content tip} a] a)))

(defn- zoom-links [{:keys [factor] :as c}]
  [:span.capp-pdf__zoom
   [bar-link {:tip "Verkleinern (Strg + Mausrad)" :on #(zoom! c (/ @factor zoom-step))} "−"]
   [bar-link {:tip "Auf Breite zurücksetzen" :on #(zoom! c 1)}
    (str (js/Math.round (* 100 @factor)) " %")]
   [bar-link {:tip "Vergrößern (Strg + Mausrad)" :on #(zoom! c (* @factor zoom-step))} "+"]])

(defn- mark-toggles
  [{:keys [hidden? tags]} annotations]
  (let [captioned? (some :caption annotations)
        named?     (some #(some :name (fields-of %)) annotations)]
    [:<>
     (when (seq annotations)
       [bar-link {:tip   (if @hidden? "Markierungen einblenden" "Markierungen ausblenden")
                  :class (when @hidden? "capp-pdf__toggle--off")
                  :on    #(swap! hidden? not)}
        (if @hidden? "▭ Markierungen einblenden" "▭ Markierungen ausblenden")])
     (when (and named? (not @hidden?))
       (let [on? (:names? @tags)]
         [bar-link {:tip   (if on? "Caption der Markierungen ausblenden"
                               "Caption der Markierungen einblenden")
                    :class (when-not on? "capp-pdf__toggle--off")
                    :on    #(store-tag-setting! :names? (:names? (swap! tags update :names? not)))}
          (if on? "🏷 Caption ausblenden" "🏷 Caption einblenden")]))
     (when (and captioned? (not @hidden?))
       (let [m (:value-mode @tags)
             {:keys [label title]} (value-mode-ui m)]
         [bar-link {:tip   title
                    :class (when (= :none m) "capp-pdf__toggle--off")
                    :on    #(->> (swap! tags update :value-mode pdf-layers/next-value-mode)
                                 :value-mode
                                 (store-tag-setting! :value-mode))}
          label]))]))

(defn- download-links [{:keys [doc]} {:keys [src filename]} annotations shown]
  (let [nm (download-name filename)]
    [:<>
     [bar-link {:on #(print-pdf! src)} "🖨 Drucken"]
     [:a.capp-pdf__download {:href src :download nm} "⬇ PDF herunterladen"]
     (when (seq annotations)
       [bar-link {:on #(export-annotated! @doc shown nm)} "⬇ mit Markierungen"])]))

(defn- pdf-bar
  [{:keys [term cursor draw?] :as c}
   {:keys [search marks scale on-area steered? shown]}
   {:keys [annotations area-label downloads?] :as props}]
  [:div.capp-pdf__bar
   [search-field {:value    (if (some? @term)
                              @term
                              (when (or (string? search) (number? search)) (str search)))
                  :count    (count marks)
                  :cursor   @cursor
                  :on-input #(reset! term %)
                  :on-step  #(step! c scale %)}]
   [zoom-links c]
   (when (and on-area (not steered?))
     [bar-link {:tip   (if @draw? "Rahmen ziehen beenden" "Rahmen auf den Beleg ziehen")
                :class (when @draw? "capp-pdf__toggle--on")
                :on    #(swap! draw? not)}
      (str "✎ " (if (str/blank? (str area-label)) "Rahmen ziehen" area-label))])
   [mark-toggles c annotations]
   (when-not (false? downloads?)
     [download-links c props annotations shown])])

(defn- action-popover [{:keys [pop-at]} {:keys [env create]}]
  (when-let [{:keys [x y popover row kind]} @pop-at]
    [floating {:x x :y y :on-close #(reset! pop-at nil)}
     (when (seq (:fields popover))
       (into [:div.capp-pdf__popover-fields]
             (map (fn [{:keys [name value]}]
                    [:div {:key (str name value)} (when name [:b (str name ": ")]) value]))
             (:fields popover)))
     [interactions/bar {:env          env
                        :items        (if popover (:actions popover) create)
                        :kind         kind
                        :row          row
                        :default-icon (if popover "✎" "✚")}]]))

(defn pdf
  [_]
  (let [{:keys [doc term cursor hits factor error box natural draft draw? props*] :as c}
        (context)]
    (fn [{:keys [src annotations search on-select on-area zoom height class draw collect?
                 area-kind drafts on-remove-draft on-change-draft page-var create]
          :as props}]
      (let [search   (if (some? @term) (when-not (str/blank? @term) [{:text @term}]) search)
            on-area  (when (or on-area (seq create))
                       (fn [{:keys [page box] :as m}]
                         (when on-area (on-area m))
                         (open-create! c (pdf-layers/selection-row page [box] ""))))]
        (keep-draft! c annotations)
        (load-doc! c src)
        (search! c search)
        (let [scale    (some-> (or zoom (fit-scale @box @natural)) (* @factor))
              marks    (cond-> (:marks @hits)
                         (= 1 (count (suchbegriffe search))) (pdf-layers/match-marks @cursor))
              _        (reset! props* (assoc props :scale scale))
              _        (follow-page! c (deref* page-var) scale)
              steered? (contains? props :draw)
              drawing? (and on-area (if steered? (boolean draw) @draw?))
              shown    (cond-> (shown-annotations c annotations on-select)
                         (seq marks) (into marks))]
          (follow-selection! c shown scale)
          [:div {:class (cond-> "capp-pdf" class (str " " class))
                 :ref   (:mount! c)
                 :on-pointer-up-capture (:pointer-up c)
                 :on-mouse-up (:text-selection c)
                 :style (when height {:max-height height :overflow-y "auto"})}
           (cond
             @error [ui/problem (str "PDF: " @error) nil]
             (nil? src) [:div.capp-pdf__empty "kein Beleg"]
             (nil? @doc) [busy/block {:class "capp-pdf__empty"} "lädt …"]
             :else
             (into [:<>
                    [pdf-bar c {:search search :marks marks :scale scale :on-area on-area
                                :steered? steered? :shown shown}
                     props]
                    [action-popover c props]
                    (when (seq (suchbegriffe search))
                      [such-zeile search marks])]
                   (for [n (let [^js d @doc] (range 1 (inc (.-numPages d))))]
                     ^{:key n} [page-view @doc n scale shown
                                (when-not drawing? on-select)
                                {:draft draft :on-area (when drawing? on-area)
                                 :collect? collect? :drafts drafts
                                 :area-band? (= :band (some-> area-kind keyword))
                                 :on-remove-draft on-remove-draft
                                 :on-change-draft on-change-draft}])))])))))
