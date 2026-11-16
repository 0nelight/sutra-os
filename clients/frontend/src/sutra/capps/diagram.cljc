(ns sutra.capps.diagram
  (:require [clojure.string :as str]
            #?(:clj [clojure.edn :as edn] :cljs [cljs.reader :as edn])))

(def collections [:shapes :edges :settings])

(def empty-model {:shapes {} :edges {} :settings {}})

(def grid 2.5)

(def ink "#1c2127")

(def page-mm {:a4 [297 210] :a3 [420 297] :a2 [594 420]})

(defn- read-edn [s]
  (try (edn/read-string (str s)) (catch #?(:clj Exception :cljs :default) _ nil)))

(defn read-body
  [s]
  (let [m (when-not (str/blank? s) (read-edn s))]
    (if (map? m) (merge empty-model (select-keys m collections)) empty-model)))

(defn body
  [model]
  (pr-str (into (sorted-map)
                (map (fn [k] [k (into (sorted-map) (get model k))]))
                collections)))

(defn entries [model]
  (into {} (map (fn [k] [(name k) (update-vals (get model k {}) pr-str)])) collections))

(defn from-entries [es]
  (into {}
        (map (fn [k]
               [k (into {} (keep (fn [[id v]] (when-let [x (read-edn v)]
                                                (when (map? x) [id x]))))
                        (get es (name k)))]))
        collections))

(defn blank? [model] (every? empty? (map model [:shapes :edges])))

(defn page-size
  [model]
  (let [{:keys [size portrait?]} (get-in model [:settings "page"])
        [w h] (get page-mm (keyword (or size :a4)) (:a4 page-mm))]
    (if portrait? [h w] [w h])))

(defn snap
  ([x] (snap x grid))
  ([x g] (* g (Math/round (double (/ x g))))))

(defn snap-pt [[x y]] [(snap x) (snap y)])

(defn- n [x] (let [r (/ (Math/round (* 100.0 x)) 100.0)] (if (== r (long r)) (long r) r)))

(defn- sign [x] (cond (pos? x) 1 (neg? x) -1 :else 0))

(defn- dist [[ax ay] [bx by]] (Math/hypot (- bx ax) (- by ay)))

(defn seg-dist [p a b]
  (let [[px py] p [ax ay] a [bx by] b
        dx (- bx ax) dy (- by ay)
        l2 (+ (* dx dx) (* dy dy))
        t  (if (zero? l2) 0 (max 0 (min 1 (/ (+ (* (- px ax) dx) (* (- py ay) dy)) l2))))]
    (dist p [(+ ax (* t dx)) (+ ay (* t dy))])))

(defn project-on-seg
  [p a b]
  (let [[px py] p [ax ay] a [bx by] b
        dx (- bx ax) dy (- by ay)
        l2 (+ (* dx dx) (* dy dy))
        t  (if (zero? l2) 0 (max 0 (min 1 (/ (+ (* (- px ax) dx) (* (- py ay) dy)) l2))))]
    [(+ ax (* t dx)) (+ ay (* t dy))]))

(def ^:private sw 0.35)

(defn- line [x1 y1 x2 y2 & [attrs]]
  [:line (merge {:x1 (n x1) :y1 (n y1) :x2 (n x2) :y2 (n y2)} attrs)])

(defn- poly [pts & [attrs]]
  [:polyline (merge {:points (str/join " " (map (fn [[x y]] (str (n x) "," (n y))) pts))
                     :fill "none"}
                    attrs)])

(defn- polygon [pts & [attrs]]
  [:polygon (merge {:points (str/join " " (map (fn [[x y]] (str (n x) "," (n y))) pts))}
                   attrs)])

(defn- rect [x y w h & [attrs]]
  [:rect (merge {:x (n x) :y (n y) :width (n w) :height (n h) :fill "#fff"} attrs)])

(defn- circle [cx cy r & [attrs]]
  [:circle (merge {:cx (n cx) :cy (n cy) :r (n r) :fill "#fff"} attrs)])

(defn- circle-x [w h]
  (let [r (/ (min w h) 2) cx (/ w 2) cy (/ h 2) k (* r 0.7071)]
    [:g (circle cx cy r)
     (line (- cx k) (- cy k) (+ cx k) (+ cy k))
     (line (- cx k) (+ cy k) (+ cx k) (- cy k))]))

(defn- bowtie [w h]
  (polygon [[0 0] [w h] [w 0] [0 h]] {:fill "#fff" :stroke-linejoin "round"}))

(defn- zigzag [w h]
  (let [teeth (max 2 (* 2 (Math/round (double (/ h (* 2.2 w))))))
        step  (/ h teeth)]
    (poly (for [i (range (inc teeth))] [(if (odd? i) w 0) (* i step)]))))

(def ^:private dashed {:stroke-dasharray "1 1.2" :fill "none"})

(defn- draw-shape
  [stencil w h]
  (case stencil
    (:pumpe :heizstab) (circle-x w h)
    :ventil-2 (bowtie w h)
    :ventil-3 [:g (bowtie w (* 0.5 h))
               (polygon [[(* 0.5 w) (* 0.25 h)] [0 h] [w h]] {:fill "#fff"})]
    :fuehler  (circle (/ w 2) (/ h 2) (/ (min w h) 2) {:fill "#2b9a2b"})
    :knoten   (circle (/ w 2) (/ h 2) (/ (min w h) 2) {:fill ink})
    :spirale  (zigzag w h)
    :speicher (rect 0 0 w h (assoc dashed :rx 1))
    :bereich  (rect 0 0 w h dashed)
    :rahmen   (rect 0 0 w h {:fill "none"})
    :waermetauscher [:g (rect 0 0 w h dashed)
                     (rect (* 0.1 w) (* 0.18 h) (* 0.8 w) (* 0.24 h))
                     (rect (* 0.1 w) (* 0.58 h) (* 0.8 w) (* 0.24 h))]
    :kollektor [:g (rect 0 0 w h)
                (for [i (range 1 (int (/ w 4)))]
                  (with-meta (line (* i 4) h (+ (* i 4) 2) 0 {:stroke-width 0.15}) {:key i}))]
    :luefter [:g (rect 0 0 w h)
                  (for [i (range 1 (int (/ w 2.5)))]
                    (with-meta (line (* i 2.5) 0 (* i 2.5) h {:stroke-width 0.2}) {:key i}))]
    :kessel   [:g (rect 0 0 w h)
               (poly [[(* 0.3 w) (* 0.8 h)] [(* 0.4 w) (* 0.55 h)] [(* 0.5 w) (* 0.75 h)]
                      [(* 0.6 w) (* 0.5 h)] [(* 0.7 w) (* 0.8 h)]] {:stroke-width 0.25})]
    :regler   [:g (rect 0 0 w h {:rx 1})
               (rect (* 0.12 w) (* 0.15 h) (* 0.76 w) (* 0.3 h) {:fill "#e8eef5"})]
    :schriftfeld [:g (rect 0 0 w h)]
    :text     [:g]
    (rect 0 0 w h)))

(def stencils
  (array-map
   :pumpe       {:label "Pumpe" :w 7.5 :h 7.5 :ports :cross :text [5 1.8] :color "#e01e1e"
                 :words ["pumpe" "pump"]}
   :mischer     {:label "Mischer / Stellantrieb" :w 12.5 :h 5 :ports :cross :text [7.5 1.8]
                 :color "#e01e1e" :words ["mischer" "mixer" "stellantrieb" "anforderung"]}
   :ventil-2    {:label "Ventil (2-Wege)" :w 7.5 :h 5
                 :ports {:w [0 2.5 :w] :e [7.5 2.5 :e]} :text [0 -4.5] :color "#e01e1e"
                 :words ["absperr" "zweiwege" "2-wege"]}
   :ventil-3    {:label "Umschaltventil (3-Wege)" :w 7.5 :h 7.5
                 :ports {:w [0 1.875 :w] :e [7.5 1.875 :e] :s [3.75 7.5 :s]} :text [5 -3]
                 :color "#e01e1e" :words ["umschalt" "ventil" "valve" "dreiwege"]}
   :heizstab    {:label "E-Patrone / Heizstab" :w 7.5 :h 7.5 :ports :cross :text [5.5 1.8]
                 :color "#e01e1e" :words ["heizstab" "patrone" "elektro"]}
   :fuehler     {:label "Fühler / Messstelle" :w 1.6 :h 1.6 :ports {:c [0.8 0.8 nil]}
                 :text [-6 1.8] :lead? true :color "#1f8f1f"
                 :words ["fuehler" "fühler" "sensor" "messstelle" "temperatur" "durchfluss"
                         "zaehler" "zähler"]}
   :knoten      {:label "Abzweig (Punkt)" :w 1.4 :h 1.4 :ports {:c [0.7 0.7 nil]}
                 :text nil :words []}
   :speicher    {:label "Speicher / Boiler" :w 20 :h 130 :ports :rand :text [0 -68]
                 :color "#e01e1e" :words ["speicher" "boiler" "puffer" "tank"]}
   :spirale     {:label "Heizregister (Wendel)" :w 6 :h 40
                 :ports {:a [0 0 nil] :b [0 40 nil]} :text nil :words ["register" "wendel"]}
   :waermetauscher {:label "Wärmetauscher" :w 50 :h 20 :ports :plates :text [0 -12.5]
                    :words ["tauscher" "exchanger" "plattenw"]}
   :verteiler   {:label "Verteiler" :w 110 :h 6.25 :ports :rand :text [0 -5.5]
                 :words ["verteiler" "sammler" "manifold"]}
   :kessel      {:label "Kessel / Erzeuger" :w 25 :h 17.5 :ports :rand :text [0 -6]
                 :color "#e01e1e" :words ["kessel" "therme" "brenner" "boiler-heiz"]}
   :kollektor   {:label "Solarkollektor" :w 82.5 :h 5 :ports :rand :text [0 -5]
                 :words ["solar" "kollektor"]}
   :luefter {:label "Lüfter / Heizkreis" :w 20 :h 7.5 :ports :rand :text [0 -6.5]
                 :words ["luefter" "lüfter" "radiator" "heizkreis"]}
   :verbraucher {:label "Verbraucher" :w 30 :h 7.5 :ports :rand :text [0 -6.5]
                 :words ["verbraucher" "halle" "werkstatt"]}
   :regler      {:label "Regler" :w 20 :h 15 :ports :rand :text [0 11]
                 :words ["regler" "uvr" "steuerung" "controller"]}
   :box         {:label "Rechteck" :w 15 :h 7.5 :ports :rand :text [0 0] :words []}
   :bereich     {:label "Bereich (gestrichelt)" :w 40 :h 30 :ports :rand :text nil :words []}
   :rahmen      {:label "Rahmen" :w 60 :h 40 :ports nil :text nil :words []}
   :schriftfeld {:label "Schriftfeld" :w 85 :h 12.5 :ports nil :text [-40 -1.5]
                 :anchor "start" :words []}
   :text        {:label "Text" :w 10 :h 5 :ports nil :text [0 1.8] :words []}))

(def stencil-groups
  [["Hydraulik" [:pumpe :mischer :ventil-2 :ventil-3 :fuehler :knoten :heizstab]]
   ["Anlage" [:speicher :spirale :waermetauscher :verteiler :kessel :kollektor :luefter
              :verbraucher :regler]]
   ["Allgemein" [:box :bereich :rahmen :schriftfeld :text]]])

(defn stencil-of [shape] (get stencils (keyword (:stencil shape)) (:box stencils)))

(defn point-shape?
  [shape]
  (contains? #{:fuehler :knoten} (keyword (:stencil shape))))

(defn- cross-ports [w h]
  {:n [(/ w 2) 0 :n] :e [w (/ h 2) :e] :s [(/ w 2) h :s] :w [0 (/ h 2) :w]})

(defn- rand-ports
  [w h]
  (into {}
        (for [[side len f] [[:n w (fn [t] [t 0])] [:s w (fn [t] [t h])]
                            [:w h (fn [t] [0 t])] [:e h (fn [t] [w t])]]
              i (range 1 (max 2 (Math/round (double (/ len grid)))))
              :let [t (* i grid)] :when (< t (- len 0.01))
              :let [[u v] (f t)]]
          [(keyword (str (name side) i)) [u v side]])))

(defn local-ports
  [shape]
  (let [{:keys [ports]} (stencil-of shape) {:keys [w h]} shape]
    (case ports
      :cross (cross-ports w h)
      :rand  (rand-ports w h)
      :plates (merge (rand-ports w h)
                     {:p1-in [(* 0.1 w) (* 0.3 h) nil] :p1-out [(* 0.9 w) (* 0.3 h) nil]
                      :p2-in [(* 0.1 w) (* 0.7 h) nil] :p2-out [(* 0.9 w) (* 0.7 h) nil]})
      nil    {}
      ports)))

(def ^:private turn {:n :e :e :s :s :w :w :n})

(defn center [{:keys [x y w h]}] [(+ x (/ w 2)) (+ y (/ h 2))])

(defn- rot-of [s] (mod (long (or (:rot s) 0)) 360))

(defn to-page
  [{:keys [w h flip?] :as s} [u v]]
  (let [[cx cy] (center s)
        dx (- (if flip? (- w u) u) (/ w 2))
        dy (- v (/ h 2))
        [rx ry] (case (rot-of s) 90 [(- dy) dx] 180 [(- dx) (- dy)] 270 [dy (- dx)] [dx dy])]
    [(+ cx rx) (+ cy ry)]))

(defn- dir-on-page [{:keys [flip?] :as s} d]
  (when d
    (let [d (if flip? (get {:e :w :w :e} d d) d)]
      (nth (iterate turn d) (quot (rot-of s) 90)))))

(defn ports
  [shape]
  (vec (for [[id [u v d]] (local-ports shape)]
         {:id id :p (to-page shape [u v]) :dir (dir-on-page shape d)})))

(defn bbox
  [{:keys [w h] :as s}]
  (let [[cx cy] (center s)
        [w h] (if (#{90 270} (rot-of s)) [h w] [w h])]
    {:x1 (- cx (/ w 2)) :y1 (- cy (/ h 2)) :x2 (+ cx (/ w 2)) :y2 (+ cy (/ h 2))}))

(defn transform [{:keys [w h flip?] :as s}]
  (let [[cx cy] (center s)]
    (str "translate(" (n cx) " " (n cy) ")"
         (when (pos? (rot-of s)) (str " rotate(" (rot-of s) ")"))
         (when flip? " scale(-1 1)")
         " translate(" (n (- (/ w 2))) " " (n (- (/ h 2))) ")")))

(defn shapes
  [model]
  (->> (:shapes model)
       (map (fn [[id s]] (assoc s :id id)))
       (sort-by (juxt #(if (#{:rahmen :bereich :speicher :waermetauscher}
                            (keyword (:stencil %))) 0 1)
                      #(or (:z %) 0) :id))
       vec))

(defn new-shape
  [st [px py]]
  (let [{:keys [w h]} (get stencils st)]
    {:stencil st :w w :h h :x (snap (- px (/ w 2))) :y (snap (- py (/ h 2))) :rot 0}))

(defn- words-hit [text]
  (let [t (str/lower-case (str text))]
    (when-not (str/blank? t)
      (some (fn [[k {:keys [words]}]] (when (some #(str/includes? t %) words) k)) stencils))))

(def kind-fields
  ["art" "typ" "kind" "type" "groesse" "kategorie" "category"])

(def label-fields
  ["kennung" "kuerzel" "code" "bmk" "tag" "nummer" "nr"])

(defn field-val
  [row f]
  (let [vs (:values row)] (or (get vs (keyword f)) (get vs (name f)))))

(defn stencil-for
  [t row title]
  (or (some #(words-hit (field-val row %)) kind-fields)
      (words-hit title)
      (words-hit (name t))
      :box))

(defn label-field
  [fields]
  (let [have (set (map name fields))] (some have label-fields)))

(defn placed
  [model]
  (into {} (keep (fn [[id s]] (when-let [{:keys [type oid]} (:obj s)] [[type oid] id])))
        (:shapes model)))

(def edge-kinds
  (array-map :rohr {:label "Rohr" :attrs {:stroke-width sw}}
             :vorlauf {:label "Vorlauf" :attrs {:stroke-width sw :stroke "#d33"}}
             :ruecklauf {:label "Rücklauf" :attrs {:stroke-width sw :stroke "#2563c9"}}
             :signal {:label "Signal" :attrs {:stroke-width 0.2 :stroke-dasharray "1.2 0.8"}}
             :gestrichelt {:label "Gestrichelt" :attrs {:stroke-width 0.25
                                                        :stroke-dasharray "0.6 1"}}))

(def arrows (array-map :none "keiner" :mid "Mitte" :end "Ende" :both "Mitte und Ende"))

(defn end-at
  [model {:keys [shape port at]}]
  (if shape
    (when-let [s (get-in model [:shapes shape])]
      (some #(when (= (keyword port) (:id %)) (select-keys % [:p :dir]))
            (ports s)))
    (when at {:p at :dir nil})))

(def ^:private stub 2.5)

(defn- out [{:keys [p dir]}]
  (let [[x y] p]
    (case dir :n [x (- y stub)] :s [x (+ y stub)] :e [(+ x stub) y] :w [(- x stub) y] p)))

(defn- horizontal? [[ax ay] [bx by]] (and (== ay by) (not (== ax bx))))

(defn- aligned? [[ax ay] [bx by]] (or (== ax bx) (== ay by)))

(defn- simplify
  [pts]
  (let [pts (reduce (fn [acc p] (if (= p (peek acc)) acc (conj acc p))) [] pts)]
    (if (< (count pts) 3)
      pts
      (conj (reduce (fn [acc [a b c]]
                      (if (or (== (first a) (first b) (first c))
                              (== (second a) (second b) (second c)))
                        acc
                        (conj acc b)))
                    [(first pts)]
                    (partition 3 1 pts))
            (peek pts)))))

(defn- orthogonal
  [pts h0?]
  (loop [[p & more] pts acc [] prev-h? h0?]
    (if-not p
      acc
      (let [q (peek acc)]
        (cond
          (nil? q) (recur more [p] prev-h?)
          (aligned? q p) (recur more (conj acc p) (horizontal? q p))
          :else (let [c (if prev-h? [(first p) (second q)] [(first q) (second p)])]
                  (recur more (conj acc c p) (not prev-h?))))))))

(defn route
  [a b via]
  (let [h0? (cond (:dir a) (contains? #{:e :w} (:dir a))
                  (:dir b) (contains? #{:n :s} (:dir b))
                  :else (let [[ax ay] (:p a) [bx by] (:p b)]
                          (>= (Math/abs (- bx ax)) (Math/abs (- by ay)))))
        pts (if (seq via)
              (concat [(:p a)] (when (:dir a) [(out a)]) via (when (:dir b) [(out b)]) [(:p b)])
              [(:p a) (out a) (out b) (:p b)])]
    (simplify (orthogonal pts h0?))))

(defn edge-route
  [model e]
  (let [a (end-at model (:from e)) b (end-at model (:to e))]
    (when (and a b)
      (if (:straight? e)
        (simplify (concat [(:p a)] (:via e) [(:p b)]))
        (route a b (:via e))))))

(defn edges [model]
  (->> (:edges model) (map (fn [[id e]] (assoc e :id id))) (sort-by :id) vec))

(defn drag-segment
  [pts i d]
  (let [pts (vec pts) last-i (dec (count pts))
        h?  (horizontal? (nth pts i) (nth pts (inc i)))
        mv  (fn [[x y]] (if h? [x (snap (+ y d))] [(snap (+ x d)) y]))
        pts (cond-> pts (= (inc i) last-i) (conj (peek pts)))
        [pts i] (if (zero? i) [(into [(first pts)] pts) 1] [pts i])
        pts (-> pts (update i mv) (update (inc i) mv))]
    (subvec pts 1 (dec (count pts)))))

(defn edges-at
  [model sid]
  (keep (fn [[id e]] (when (or (= sid (get-in e [:from :shape])) (= sid (get-in e [:to :shape])))
                       id))
        (:edges model)))

(defn hit-shape
  [shs p tol]
  (let [[x y] p]
    (some (fn [s]
            (let [{:keys [x1 y1 x2 y2]} (bbox s)
                  inside? (and (<= (- x1 tol) x (+ x2 tol)) (<= (- y1 tol) y (+ y2 tol)))
                  hollow? (#{:rahmen :bereich :speicher :waermetauscher}
                           (keyword (:stencil s)))]
              (when (and inside?
                         (or (not hollow?)
                             (<= (min (- x x1) (- x2 x) (- y y1) (- y2 y)) tol)))
                s)))
          (reverse shs))))

(defn hit-port
  [shs p tol skip]
  (some->> (for [s shs :when (not (contains? skip (:id s))) pt (ports s)]
             [(dist p (:p pt)) {:shape (:id s) :port (:id pt) :p (:p pt)}])
           (filter #(<= (first %) tol))
           seq
           (apply min-key first)
           second))

(defn hit-edge
  [routes p tol]
  (some->> (for [[id pts] routes [i [a b]] (map-indexed vector (partition 2 1 pts))]
             [(seg-dist p a b) {:id id :seg i :p (project-on-seg p a b)}])
           (filter #(<= (first %) tol))
           seq
           (apply min-key first)
           second))

(defn in-box
  [shs [ax ay] [bx by]]
  (let [x1 (min ax bx) x2 (max ax bx) y1 (min ay by) y2 (max ay by)]
    (filterv (fn [s] (let [b (bbox s)]
                       (and (<= x1 (:x1 b)) (<= (:x2 b) x2) (<= y1 (:y1 b)) (<= (:y2 b) y2))))
             shs)))

(defn align
  [shs how]
  (let [bs (map bbox shs)
        tgt (case how
              :left (apply min (map :x1 bs)) :right (apply max (map :x2 bs))
              :top (apply min (map :y1 bs)) :bottom (apply max (map :y2 bs))
              :center (/ (+ (apply min (map :x1 bs)) (apply max (map :x2 bs))) 2)
              :middle (/ (+ (apply min (map :y1 bs)) (apply max (map :y2 bs))) 2))]
    (into {}
          (for [s shs :let [b (bbox s)
                            dx (case how :left (- tgt (:x1 b)) :right (- tgt (:x2 b))
                                     :center (- tgt (/ (+ (:x1 b) (:x2 b)) 2)) 0)
                            dy (case how :top (- tgt (:y1 b)) :bottom (- tgt (:y2 b))
                                     :middle (- tgt (/ (+ (:y1 b) (:y2 b)) 2)) 0)]]
            [(:id s) {:x (+ (:x s) dx) :y (+ (:y s) dy)}]))))

(defn- chevron [[ax ay] [bx by] at size]
  (let [ang (Math/atan2 (- by ay) (- bx ax))
        [px py] at
        back (fn [da] [(- px (* size (Math/cos (+ ang da))))
                       (- py (* size (Math/sin (+ ang da))))])]
    (poly [(back 0.5) [px py] (back -0.5)] {:stroke-linejoin "miter"})))

(defn- arrow-marks [pts arrow]
  (let [segs (partition 2 1 pts)
        size 1.6]
    (when (seq segs)
      (concat
       (when (#{:mid :both} arrow)
         (let [[a b] (apply max-key (fn [[a b]] (dist a b)) segs)
               mid [(/ (+ (first a) (first b)) 2) (/ (+ (second a) (second b)) 2)]]
           [(chevron a b mid size)]))
       (when (#{:end :both} arrow)
         (let [[a b] (last segs)] [(chevron a b b size)]))))))

(defn edge-hiccup
  [e pts]
  (let [{:keys [attrs]} (get edge-kinds (keyword (:kind e)) (:rohr edge-kinds))
        attrs (cond-> (merge {:stroke ink :fill "none"} attrs)
                (:color e) (assoc :stroke (:color e)))]
    (into [:g (select-keys attrs [:stroke :stroke-width])
           (poly pts (dissoc attrs :stroke :stroke-width))]
          (arrow-marks pts (keyword (or (:arrow e) :none))))))

(defn label-of
  [s lbl]
  (or (not-empty (:label s)) (when lbl (lbl s))))

(defn- text-lines [x y anchor font color txt]
  (let [lines (str/split-lines (str txt))]
    (into [:text {:x (n x) :y (n y) :font-size (n font) :fill color :text-anchor anchor
                  :stroke "none"}]
          (map-indexed (fn [i l] [:tspan {:x (n x) :dy (if (zero? i) 0 (n (* 1.2 font)))} l])
                       lines))))

(defn label-hiccup
  [s txt]
  (let [st (stencil-of s)]
    (when (and (not (str/blank? (str txt))) (or (:text st) (:label-at s)))
      (let [[cx cy] (center s)
            [dx dy] (or (:label-at s) (:text st))
            font    (or (:font s) (if (= :schriftfeld (keyword (:stencil s))) 3.5 4.5))
            anchor  (or (:anchor st)
                        (cond (> dx 0.5) "start" (< dx -0.5) "end" :else "middle"))
            color   (or (:label-color s) (:color st) ink)
            [lx ly] [(+ cx dx) (+ cy dy)]]
        [:g {:class "dg-label"}
         (when (:lead? st)
           (let [[ex ey] (if (> (Math/abs (double dy)) (Math/abs (double dx)))
                           [lx (if (pos? dy) (- ly (* 0.85 font)) (+ ly (* 0.3 font)))]
                           [(- lx (* (sign dx) 0.8)) (- ly (* 0.35 font))])]
             (line cx cy ex ey {:stroke ink :stroke-width 0.25})))
         (text-lines lx ly anchor font color txt)]))))

(defn shape-hiccup [s]
  [:g {:transform (transform s) :stroke (or (:color s) ink) :stroke-width sw}
   (draw-shape (keyword (:stencil s)) (:w s) (:h s))])

(defn icon-hiccup
  [st size]
  (let [{:keys [w h]} (get stencils st)
        k (/ (* 0.8 size) (max w h 1))
        k (if (#{:fuehler :knoten} st) (* 0.25 k) k)]
    [:svg {:width size :height size :viewBox (str "0 0 " size " " size)}
     [:g {:transform (str "translate(" (n (/ (- size (* k w)) 2)) " " (n (/ (- size (* k h)) 2))
                          ") scale(" (n k) ")")
          :stroke ink :stroke-width (/ 1.2 k) :fill "none"}
      (if (= :text st)
        [:text {:x 0 :y h :font-size h :stroke "none" :fill ink} "Aa"]
        (draw-shape st w h))]]))

(defn- esc [s]
  (-> (str s) (str/replace "&" "&amp;") (str/replace "<" "&lt;") (str/replace ">" "&gt;")
      (str/replace "\"" "&quot;")))

(def ^:private svg-attr-names {:viewBox "viewBox"})

(defn svg-str
  [h]
  (cond
    (nil? h) ""
    (string? h) (esc h)
    (number? h) (str h)
    (and (vector? h) (keyword? (first h)))
    (let [[tag & more] h
          attrs (when (map? (first more)) (first more))
          kids  (if attrs (rest more) more)]
      (str "<" (name tag)
           (apply str (for [[k v] attrs :when (some? v)]
                        (str " " (get svg-attr-names k (name k)) "=\"" (esc v) "\"")))
           ">" (apply str (map svg-str kids)) "</" (name tag) ">"))
    (sequential? h) (apply str (map svg-str h))
    :else ""))

(defn drawing-hiccup
  [model lbl]
  (let [[w h] (page-size model)
        shs (shapes model)]
    [:svg {:xmlns "http://www.w3.org/2000/svg" :viewBox (str "0 0 " w " " h)
           :width (str w "mm") :height (str h "mm") :font-family "DejaVu Sans, sans-serif"
           :stroke-linecap "round"}
     [:rect {:x 0 :y 0 :width w :height h :fill "#fff"}]
     (for [s shs :when (#{:rahmen :bereich :speicher :waermetauscher} (keyword (:stencil s)))]
       (shape-hiccup s))
     (for [e (edges model) :let [pts (edge-route model e)] :when pts]
       (edge-hiccup e pts))
     (for [s shs :when (not (#{:rahmen :bereich :speicher :waermetauscher}
                             (keyword (:stencil s))))]
       (shape-hiccup s))
     (for [s shs] (label-hiccup s (label-of s lbl)))]))

(defn svg-file [model lbl]
  (str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" (svg-str (drawing-hiccup model lbl))))
