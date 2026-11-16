(ns sutra.capps.pdf-layers
  (:require [clojure.string :as str]
            [sutra.capps.table :as tbl]))

(def ^:private selected-color "#1976d2")

(def ^:private palette
  ["#1f77b4" "#e8710a" "#2ca02c" "#d62728" "#9467bd" "#8c564b" "#e377c2" "#17becf"
   "#bcbd22" "#7f7f7f"])

(defn value-color
  [v]
  (let [s (str v)]
    (when-not (str/blank? s)
      (nth palette (mod (reduce (fn [h c] (mod (+ (* 31 h) (int c)) 1000003)) 7 s)
                        (count palette))))))

(defn- num* [x]
  (cond (number? x) (double x)
        (string? x) (let [d #?(:clj (try (Double/parseDouble x) (catch Exception _ nil))
                               :cljs (let [v (js/parseFloat x)] (when-not (js/isNaN v) v)))]
                      d)
        :else nil))

(defn parse-box
  [b]
  (let [get* (if (map? b)
               (fn [k] (num* (or (get b k) (get b (name k)))))
               (fn [k]
                 (some->> (str b)
                          (re-find (re-pattern (str "\"" (name k) "\"\\s*:\\s*\"?(-?[0-9.]+)")))
                          second num*)))
        m    (into {} (map (fn [k] [k (get* k)])) [:x1 :y1 :x2 :y2])
        pg   (get* :page)]
    (when (and (:y1 m) (:y2 m) (= (some? (:x1 m)) (some? (:x2 m))))
      (cond-> (into {} (filter val) m) pg (assoc :page (long pg))))))

(defn box-text
  [page {:keys [x1 y1 x2 y2]}]
  (if (and x1 x2)
    (str "{\"x1\": " x1 ", \"y1\": " y1 ", \"x2\": " x2 ", \"y2\": " y2
         ", \"page\": " page "}")
    (str "{\"y1\": " y1 ", \"y2\": " y2 ", \"page\": " page "}")))

(defn band-text
  [page box]
  (box-text page (dissoc box :x1 :x2)))

(defn boxes-of
  [v]
  (cond (nil? v) []
        (string? v) (if (str/starts-with? (str/trim v) "[")
                      (map second (re-seq #"(\{[^}]*\})" v))
                      [v])
        (sequential? v) v
        :else [v]))

(defn caption-fields
  [caption display row]
  (into []
        (keep (fn [p]
                (let [k (if (string? p) (keyword p) p)]
                  (when-let [v (some-> ((tbl/field-fn k) row) str str/trim not-empty)]
                    {:name  (:label (tbl/prop-display (get display k {}) k))
                     :value v}))))
        (if (sequential? caption) caption (some-> caption vector))))

(defn tag-fields
  [caption]
  (if (string? caption) [{:value caption}] caption))

(def value-modes
  [:selected :all :none])

(defn next-value-mode [m]
  (nth value-modes (mod (inc (.indexOf value-modes m)) (count value-modes))))

(defn tag-segments
  [fields {:keys [value-mode names?]} selected?]
  (let [shown?  (or selected? (= :all value-mode))
        values? (and shown? (not= :none value-mode))
        names?  (and shown? names?)]
    (->> fields
         (map (fn [{:keys [name value]}]
                (let [n (when (and names? (not (str/blank? name))) [:name name])
                      v (when values? [:value value])]
                  (if (and n v) [n [:sep " "] v] (filterv some? [n v])))))
         (remove empty?)
         (interpose [[:sep " · "]])
         (apply concat)
         vec
         not-empty)))

(defn segments-text [segments]
  (apply str (map second segments)))

(defn- show-date
  [v]
  (if-let [[_ y m d] (re-matches #"(\d{4})-(\d{2})-(\d{2}).*" (str v))]
    (str d "." m "." y)
    (str v)))

(def ^:private gold5 "#FBD065")

(defn- rule-color
  [rules row]
  (let [look (tbl/cond-format rules (or (:values row) row))]
    (or (:background look) (:color look) gold5)))

(defn layer-marks
  [{:keys [rows page boxes color color-prop color-by caption display label key active kind
           title body on-select on-boxes conditional-format tooltip actions name]
    when-prop :when :as layer}
   layer-idx]
  (let [pf (if (number? page) (constantly page) (tbl/field-fn page))
        act (cond (nil? active) nil
                  (coll? active) (into #{} (map str) active)
                  :else #{(str active)})
        bf (if (or (map? boxes) (and (sequential? boxes) (map? (first boxes))))
             (constantly boxes)
             (tbl/field-fn boxes))
        cf (tbl/field-fn color-prop)
        byf (tbl/field-fn color-by)
        lf (tbl/field-fn label)
        wf (tbl/field-fn when-prop)
        bs (keep tbl/field-fn (if (sequential? body) body (when body [body])))
        kf (or (tbl/field-fn key) (tbl/field-fn :id))
        kind (or (some-> kind keyword) :rect)]
    (into []
          (mapcat
           (fn [row]
             (let [k   (kf row)
                   pg  (some-> (pf row) num* long)
                   sel (and act (contains? act (str k)))
                   c   (cond (and sel (not (or cf byf conditional-format))) selected-color
                             (and cf (re-matches #"#[0-9A-Fa-f]{6}" (str (cf row)))) (cf row)
                             byf (or (value-color (byf row)) color)
                             (seq conditional-format) (rule-color conditional-format row)
                             :else color)
                   tip (not-empty (caption-fields tooltip display row))
                   cap (not-empty (caption-fields caption display row))
                   txt (->> bs (map #(% row)) (remove #(str/blank? (str %)))
                            (map show-date) (str/join " · ") not-empty)]
               (when (or (nil? wf) (not (str/blank? (str (wf row)))))
                 (let [raws (vec (boxes-of (bf row)))]
                   (keep-indexed
                    (fn [i raw]
                      (when-let [{:keys [x1 y1 x2 y2] bp :page} (parse-box raw)]
                        (when-let [p (or bp pg)]
                          (cond-> {:id    (str layer-idx "/" k "/" i)
                                   :kind  (if x1 kind :band)
                                   :page  p
                                   :geom  {:x (or x1 0) :y y1 :w (- (or x2 0) (or x1 0))
                                           :h (- y2 y1)}
                                   :row   row}
                            sel   (assoc :selected true)
                            (contains? layer :on-select) (assoc :on-select on-select)
                            c     (assoc :color c)
                            name  (assoc :layer-name name)
                            (or tip (seq actions)) (assoc :popover {:fields tip :actions actions})
                            title (assoc :title title)
                            txt   (assoc :text txt)
                            lf    (assoc :label (some-> (lf row) str))
                            (and cap (zero? i)) (assoc :caption cap)
                            on-boxes (assoc :on-change
                                            (fn [b]
                                              (on-boxes row (assoc raws i
                                                                   (if (or (not x1) (= :band kind))
                                                                     (band-text p b)
                                                                     (box-text p b))))))))))
                    raws))))))
          rows)))

(defn marks
  [layers]
  (vec (mapcat layer-marks (or layers []) (range))))

(def ^:private current-match "#2e7d32")
(def ^:private other-match "#7b1fa2")

(defn match-marks
  [marks cursor]
  (vec (map-indexed (fn [i m] (assoc m :color (if (= i cursor) current-match other-match))) marks)))

(defn step-cursor
  [cursor n dir]
  (if (pos? n) (mod (+ (or cursor 0) dir) n) 0))

(defn rects->boxes
  [rects {:keys [left top]} scale]
  (let [pt #(/ (long (* 10 (/ % scale))) 10.0)]
    (into []
          (comp (filter (fn [{:keys [width height]}] (and (pos? width) (pos? height))))
                (map (fn [{r-left :left r-top :top :keys [width height]}]
                       {:x1 (pt (- r-left left))
                        :y1 (pt (- r-top top))
                        :x2 (pt (+ (- r-left left) width))
                        :y2 (pt (+ (- r-top top) height))}))
                (distinct))
          rects)))

(defn selection-row
  [page boxes text]
  (let [vs {:page page :boxes (mapv #(box-text page %) boxes) :text (or text "")}]
    (assoc vs :values vs)))

(defn page-at
  [spans y]
  (some (fn [[i [top bottom]]] (when (and (<= top y) (< y bottom)) (inc i)))
        (map-indexed vector spans)))
