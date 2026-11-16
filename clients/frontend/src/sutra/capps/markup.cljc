(ns sutra.capps.markup
  (:require [clojure.string :as str]
            #?(:clj [clojure.edn :as edn] :cljs [cljs.reader :as edn])
            [sutra.capps.image-annotation :as ia]
            [sutra.capps.snap :as snap]))

(def collections
  [:photos :marks :comments])

(def empty-model {:photos {} :marks {} :comments {}})

(def kinds #{:dim :note :area})

(def labels
  ["Breite" "Höhe" "Tiefe" "Lichte Breite" "Lichte Höhe" "Diagonale" "Abstand"])

(defn- read-edn [s]
  (try (edn/read-string (str s)) (catch #?(:clj Exception :cljs :default) _ nil)))

(defn read-body
  [s]
  (let [m (when-not (str/blank? s) (read-edn s))]
    (if (map? m)
      (merge empty-model (select-keys m collections))
      empty-model)))

(defn body
  [model]
  (pr-str (into (sorted-map)
                (map (fn [k] [k (into (sorted-map) (get model k))]))
                collections)))

(defn entries
  [model]
  (into {} (map (fn [k] [(name k) (update-vals (get model k {}) pr-str)])) collections))

(defn from-entries
  [es]
  (into {}
        (map (fn [k]
               [k (into {} (keep (fn [[id v]] (when-let [x (read-edn v)]
                                                (when (map? x) [id x]))))
                        (get es (name k)))]))
        collections))

(defn blank? [model] (every? empty? (map model collections)))

(defn photos
  [model]
  (->> (:photos model)
       (map (fn [[id p]] (assoc p :id id)))
       (sort-by (juxt #(or (:pos %) 0) :id))
       vec))

(defn next-pos [model] (inc (reduce max -1 (keep :pos (vals (:photos model))))))

(defn marks-on
  [model pid]
  (->> (:marks model)
       (keep (fn [[id m]] (when (= pid (:photo m)) (assoc m :id id))))
       (sort-by (juxt #(str (:at %)) :id))
       vec))

(defn thread
  [model mid]
  (->> (:comments model)
       (keep (fn [[id c]] (when (= mid (:mark c)) (assoc c :id id))))
       (sort-by (juxt #(str (:at %)) :id))
       vec))

(defn open-count
  [model pid]
  (count (remove :done? (marks-on model pid))))

(defn parse-length
  [s]
  (when-let [[_ n u] (re-matches #"\s*(\d+(?:[.,]\d+)?)\s*(mm|cm|m)?\s*"
                                 (str/lower-case (str s)))]
    (let [x (parse-double (str/replace n "," "."))]
      (Math/round (double (* x (case u "m" 1000 "cm" 10 1)))))))

(defn- group3 [n]
  (let [s (str n)]
    (str/join "." (map str/join (reverse (map reverse (partition-all 3 (reverse s))))))))

(defn mm-text
  [mm]
  (when (number? mm) (str (when (neg? mm) "-") (group3 (Math/abs (long mm))))))

(defn caption
  [{:keys [kind mm label note]}]
  (let [short-note (some-> note str/trim not-empty (#(if (> (count %) 28)
                                                       (str (subs % 0 27) "…") %)))]
    (case kind
      :dim (str/join " " (remove str/blank? [label (or (mm-text mm) "? mm")]))
      (or (not-empty label) short-note (if (= :note kind) "Notiz" "Bereich")))))

(defn- dist [[ax ay] [bx by]] (Math/hypot (- bx ax) (- by ay)))

(defn- seg-dist
  [p a b]
  (let [[px py] p [ax ay] a [bx by] b
        dx (- bx ax) dy (- by ay)
        l2 (+ (* dx dx) (* dy dy))
        t  (if (zero? l2) 0 (max 0 (min 1 (/ (+ (* (- px ax) dx) (* (- py ay) dy)) l2))))]
    (dist p [(+ ax (* t dx)) (+ ay (* t dy))])))

(defn dim-geometry
  [a b tick]
  (let [ang (snap/angle-at a b)
        [nx ny] [(- (Math/sin ang)) (Math/cos ang)]
        h   (/ tick 2)
        end (fn [[x y]] [[(+ x (* h nx)) (+ y (* h ny))] [(- x (* h nx)) (- y (* h ny))]])
        deg (* ang (/ 180 Math/PI))
        deg (cond (> deg 90) (- deg 180) (< deg -90) (+ deg 180) :else deg)
        up  (* deg (/ Math/PI 180))
        off (* 0.6 tick)
        [mx my] [(/ (+ (first a) (first b)) 2) (/ (+ (second a) (second b)) 2)]]
    {:line  [a b]
     :ticks [(end a) (end b)]
     :label [(+ mx (* off (Math/sin up))) (- my (* off (Math/cos up)))]
     :angle deg}))

(defn hit
  [marks p tol]
  (let [d (fn [{:keys [kind a b box]}]
            (case kind
              :dim  (seg-dist p a b)
              :note (dist p a)
              :area (let [{:keys [x1 y1 x2 y2]} box [x y] p]
                      (if (and (<= x1 x x2) (<= y1 y y2)) tol ##Inf))
              ##Inf))]
    (some->> marks
             (map (fn [m] [(d m) m]))
             (filter #(<= (first %) tol))
             seq
             (apply min-key first)
             second)))

(defn handles
  [{:keys [kind a b]}]
  (case kind :dim {:a a :b b} :note {:a a} {}))

(defn- ends
  [marks]
  (mapcat (comp vals handles) marks))

(defn snap-end
  [marks p tol]
  (some->> (ends marks)
           (map (fn [e] [(dist e p) e]))
           (filter #(<= (first %) tol))
           seq
           (apply min-key first)
           second))

(defn- snap-from
  [marks a p tol]
  (or (snap-end marks p tol)
      (snap/snap-point [a] p (snap/snap-directions
                              [] (keep (fn [m] (when (= :dim (:kind m)) [(:a m) (:b m)]))
                                       marks)))))

(def tools [:select :dim :note :area])

(defn- clamp [p {:keys [w h]}] (ia/clamp-point p {:w w :h h}))

(defn step
  [st [kind x] {:keys [marks size tol]}]
  (let [p    (when (vector? x) (clamp x size))
        tool (or (:tool st) :select)]
    (case kind
      :tool   [{:tool x :sel (:sel st)} []]
      :cancel [(dissoc st :draft :hover :grab :area :tap) []]
      :down
      (case tool
        :select (let [sel  (some #(when (= (:sel st) (:id %)) %) marks)
                      grip (some (fn [[k h]] (when (<= (dist h p) tol) k)) (handles sel))]
                  (if grip
                    [(assoc st :grab {:id (:id sel) :end grip :p p}) []]
                    (if-let [m (hit marks p tol)]
                      [(assoc st :sel (:id m)) [[:select (:id m)]]]
                      [(assoc st :tap p) []])))
        :dim    (if (:draft st)
                  [st []]
                  [(assoc st :draft [(or (snap-end marks p tol) p)] :hover nil) []])
        :note   [st [[:create {:kind :note :a p}]]]
        :area   [(assoc st :area (ia/draw nil [:start p])) []])
      :move
      (cond
        (:grab st) (let [{:keys [id end]} (:grab st)
                         m     (some #(when (= id (:id %)) %) marks)
                         other (get (handles m) (if (= :a end) :b :a))
                         q     (if (and other (= :dim (:kind m)))
                                 (snap-from (remove #(= id (:id %)) marks) other p tol)
                                 p)]
                     [(assoc-in st [:grab :p] q) []])
        (:draft st) [(assoc st :hover (snap-from marks (first (:draft st)) p tol)) []]
        (:area st)  [(update st :area ia/draw [:move p]) []]
        :else       [st []])
      :up
      (cond
        (:tap st) [(dissoc st :tap :sel) [[:select nil]]]

        (:grab st)
        (let [{:keys [id end] q :p} (:grab st)]
          [(dissoc st :grab) [[:update id {end q}]]])

        (:draft st)
        (let [a (first (:draft st))
              b (snap-from marks a p tol)]
          (if (> (dist a b) tol)
            [(dissoc st :draft :hover) [[:create {:kind :dim :a a :b b}]]]
            [st []]))

        (:area st)
        (let [{:keys [done]} (ia/draw (:area st) [:finish p])]
          [(dissoc st :area)
           (if-let [box (ia/parse-box done)] [[:create {:kind :area :box box}]] [])])

        :else [st []])
      [st []])))

(defn draft-line
  [st marks]
  (cond
    (and (:draft st) (:hover st)) [(first (:draft st)) (:hover st)]
    (:grab st) (let [{:keys [id end p]} (:grab st)
                     m (some #(when (= id (:id %)) %) marks)]
                 (when (= :dim (:kind m)) (if (= :a end) [p (:b m)] [(:a m) p])))
    :else nil))

(defn moved
  [mark st]
  (if-let [{:keys [end p]} (when (= (:id mark) (get-in st [:grab :id])) (:grab st))]
    (assoc mark end p)
    mark))

(defn fit
  [{:keys [w h]} aspect]
  (let [aspect (if (pos? aspect) aspect 1)]
    (if (> (/ w h) aspect)
      (let [vh (/ w aspect)] {:x 0 :y (/ (- h vh) 2) :w w :h vh})
      (let [vw (* h aspect)] {:x (/ (- w vw) 2) :y 0 :w vw :h h}))))

(defn zoom
  [{:keys [x y w h] :as v} [cx cy] factor size]
  (let [whole (max (:w size) (:h size))
        f     (-> factor (min (/ w 40)) (max (/ w (* 1.5 whole))))]
    (if (or (zero? f) (= 1 f))
      v
      {:x (- cx (/ (- cx x) f)) :y (- cy (/ (- cy y) f)) :w (/ w f) :h (/ h f)})))

(defn pan [v dx dy] (-> v (update :x - dx) (update :y - dy)))

(defn others-on
  [states self pid]
  (vec (for [[client {:keys [user markup]}] states
             :when (and (not= client self) (= pid (:photo markup)) (:name user))]
         (merge {:name (:name user) :color (:color user)}
                (select-keys markup [:cursor :line :sel])))))

(def colors {:dim "#d9480f" :note "#2d72d2" :area "#238551" :done "#5f6b7c"})

(defn color-of [{:keys [kind done?]}] (if done? (:done colors) (get colors kind "#d9480f")))

(defn- esc [s]
  (-> (str s) (str/replace "&" "&amp;") (str/replace "<" "&lt;") (str/replace ">" "&gt;")
      (str/replace "\"" "&quot;")))

(defn- n [x] (let [r (/ (Math/round (* 10.0 x)) 10.0)] (if (== r (long r)) (long r) r)))

(defn scale-of
  [{:keys [w h]}]
  (let [s (/ (max w h) 100.0)] {:stroke (* 0.35 s) :font (* 2.4 s) :tick (* 2.2 s)}))

(defn- svg-mark
  [{:keys [kind a b box] :as m} {:keys [stroke font tick]}]
  (let [c (color-of m) t (esc (caption m))]
    (case kind
      :dim  (let [{:keys [line ticks label angle]} (dim-geometry a b tick)
                  seg (fn [[[x1 y1] [x2 y2]]]
                        (str "<line x1=\"" (n x1) "\" y1=\"" (n y1) "\" x2=\"" (n x2)
                             "\" y2=\"" (n y2) "\"/>"))]
              (str "<g stroke=\"" c "\" stroke-width=\"" (n stroke) "\">"
                   (seg line) (apply str (map seg ticks)) "</g>"
                   "<text x=\"" (n (first label)) "\" y=\"" (n (second label))
                   "\" transform=\"rotate(" (n angle) " " (n (first label)) " "
                   (n (second label)) ")\" fill=\"" c "\" font-size=\"" (n font)
                   "\" text-anchor=\"middle\">"
                   t "</text>"))
      :note (let [[x y] a]
              (str "<circle cx=\"" (n x) "\" cy=\"" (n y) "\" r=\"" (n tick)
                   "\" fill=\"" c "\"/><text x=\"" (n (+ x (* 1.5 tick))) "\" y=\"" (n y)
                   "\" fill=\"" c "\" font-size=\"" (n font) "\">" t "</text>"))
      :area (let [{:keys [x1 y1 x2 y2]} box]
              (str "<rect x=\"" (n x1) "\" y=\"" (n y1) "\" width=\"" (n (- x2 x1))
                   "\" height=\"" (n (- y2 y1)) "\" fill=\"" c "\" fill-opacity=\"0.12\" stroke=\""
                   c "\" stroke-width=\"" (n stroke) "\"/><text x=\"" (n x1) "\" y=\""
                   (n (- y1 (* 0.5 font))) "\" fill=\"" c "\" font-size=\"" (n font) "\">"
                   t "</text>"))
      "")))

(defn photo-svg
  [model pid href]
  (let [{:keys [w h title]} (get-in model [:photos pid])
        sc (scale-of {:w w :h h})]
    (str "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 " w " " h
         "\" width=\"" w "\" height=\"" h "\" font-family=\"sans-serif\" "
         "paint-order=\"stroke\" stroke-linecap=\"round\">"
         "<title>" (esc title) "</title>"
         "<image href=\"" (esc href) "\" width=\"" w "\" height=\"" h "\"/>"
         "<style>text{paint-order:stroke;stroke:#fff;stroke-linejoin:round;stroke-width:"
         (n (/ (:font sc) 6)) "px}</style>"
         "<g>"
         (apply str (map #(svg-mark % sc) (marks-on model pid)))
         "</g></svg>")))
