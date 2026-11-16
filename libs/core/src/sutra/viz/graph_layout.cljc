(ns sutra.viz.graph-layout
  (:require [sutra.kit.coll :as coll]
            [clojure.string :as str]))

(def col-gap 140)
(def row-gap 40)
(def node-width 180)
(def node-height 48)

(defn ->num [v]
  (cond
    (number? v) v
    (string? v) #?(:clj  (try (Double/parseDouble (str/replace v #"[^0-9.eE+-]" ""))
                              (catch NumberFormatException _ nil))
                   :cljs (let [n (js/parseFloat v)] (when-not (js/isNaN n) n)))
    :else nil))

(defn round
  ([v] (let [n (or (->num v) 0)]
         #?(:clj (Math/round (double n)) :cljs (Math/round n))))
  ([v places]
   (let [f (Math/pow 10 places)]
     (/ (round (* (or (->num v) 0) f)) f))))

(defn fmt [v]
  (let [n (or (->num v) 0)
        r (round n)]
    (if (== r n) (str r) (str n))))

(defn box
  [n]
  [(or (->num (:w n)) node-width) (or (->num (:h n)) node-height)])

(def label-char-px 7)
(def label-icon-px 30)
(def label-lead-px 14)
(def label-pad-px 12)
(def label-min-width 56)
(def label-line-px 16)
(def label-pad-y 10)

(def label-wrap-px
  170)

(def label-max-lines 3)

(defn label-lines
  ([text] (label-lines text label-wrap-px))
  ([text wide]
   (let [fits (max 3 (int (/ (max 1 wide) label-char-px)))
         out  (reduce (fn [acc w]
                        (let [cur (peek acc)]
                          (if (and cur (<= (+ (count cur) 1 (count w)) fits))
                            (conj (pop acc) (str cur " " w))
                            (conj acc w))))
                      []
                      (remove empty? (str/split (str text) #"\s+")))
         out  (if (seq out) out [(str text)])]
     (if (<= (count out) label-max-lines)
       (vec out)
       (conj (vec (take (dec label-max-lines) out))
             (let [rest' (str/join " " (drop (dec label-max-lines) out))]
               (if (> (count rest') fits)
                 (str (subs rest' 0 (max 1 (dec fits))) "…")
                 rest')))))))

(defn label-box
  ([n] (label-box n label-wrap-px))
  ([n wide]
   (let [lead  (if (seq (str (:icon n))) label-icon-px label-lead-px)
         lines (label-lines (str (or (:label n) (:id n))) wide)
         cols  (apply max (map count lines))]
     [(max label-min-width (+ lead (* label-char-px cols) label-pad-px))
      (+ label-pad-y (* label-line-px (count lines)))])))

(def label-wraps
  [120 170 240 320 420])

(defn edges-within
  [ids edges]
  (let [present (set ids)]
    (filterv (fn [{:keys [source target]}]
               (and (present source) (present target) (not= source target)))
             edges)))

(defn- depths
  [ids es sources]
  (let [step  (fn [d]
                (reduce (fn [d {:keys [source target]}]
                          (let [cand (inc (get d source 0))]
                            (cond-> d (> cand (get d target 0)) (assoc target cand))))
                        d es))
        fix   (fn [d0]
                (loop [d d0 i 0]
                  (let [d' (step d)]
                    (if (or (= d d') (>= i (count ids))) d' (recur d' (inc i))))))
        start (if (seq sources) 1 0)
        d     (fix (into {} (for [id ids] [id (if (sources id) 0 start)])))
        fed   (set (map :target es))
        outs  (group-by :source es)]
    (fix (reduce (fn [d id]
                   (if-let [ts (seq (map #(get d (:target %)) (get outs id)))]
                     (assoc d id (max (get d id) (dec (apply min ts))))
                     d))
                 d (remove #(or (fed %) (sources %)) ids)))))

(defn- place-at
  [n main cross richtung]
  (if (= :oben-unten richtung)
    (assoc n :x (* cross (+ node-width row-gap))
           :y (* main (+ node-height col-gap)))
    (assoc n :x (* main (+ node-width col-gap))
           :y (* cross (+ node-height row-gap)))))

(defn sorted-nodes
  [nodes]
  (vec (sort-by (juxt #(str (:label %)) #(str (:id %))) nodes)))

(defn- in-order
  [nodes]
  (mapv :id (sorted-nodes nodes)))

(def ^:private sweeps 4)

(defn- barycentre
  [layers es]
  (let [ins   (reduce (fn [m {:keys [source target]}] (update m target (fnil conj []) source)) {}
                      es)
        outs  (reduce (fn [m {:keys [source target]}] (update m source (fnil conj []) target)) {}
                      es)
        ranks (fn [ls] (into {} (for [l ls, [i id] (map-indexed vector l)] [id i])))
        sweep (fn [ls nbr]
                (let [r (ranks ls)]
                  (mapv (fn [l]
                          (vec (sort-by (fn [id]
                                          (let [ps (keep r (get nbr id))]
                                            [(if (seq ps)
                                               (double (/ (reduce + ps) (count ps)))
                                               (double (get r id 0)))
                                             (str id)]))
                                        l)))
                        ls)))]
    (loop [ls layers i 0]
      (if (>= i sweeps) ls (recur (-> ls (sweep ins) (sweep outs)) (inc i))))))

(defn hierarchie
  ([nodes edges] (hierarchie nodes edges nil))
  ([nodes edges opts]
   (let [ids     (in-order nodes)
         es      (edges-within ids edges)
         linked  (into #{} (mapcat (juxt :source :target)) es)
         alone   (filterv (complement linked) ids)
         sources (into #{} (keep #(when (= :datasource (:kind %)) (:id %))) nodes)
         depth   (depths (filterv linked ids) es (into #{} (filter linked) sources))
         layers  (->> (filter linked ids)
                      (group-by #(get depth % 0))
                      (into (sorted-map))
                      vals
                      (mapv vec))
         ordered (barycentre layers es)
         below   (inc (apply max -1 (map count ordered)))
         wide    (max 1 (count ordered))
         by-id   (coll/index-by :id nodes)]
     (vec (concat
           (for [[main layer] (map-indexed vector ordered)
                 [cross id]   (map-indexed vector layer)]
             (place-at (by-id id) main cross (:richtung opts)))
           (for [[i id] (map-indexed vector alone)]
             (place-at (by-id id) (mod i wide) (+ below (quot i wide)) (:richtung opts))))))))

(defn layered
  [nodes edges]
  (hierarchie nodes edges nil))

(defn- spanning
  [ids es]
  (let [rank   (into {} (map-indexed (fn [i id] [id i])) ids)
        out    (update-vals (group-by :source es)
                            (fn [g] (vec (distinct (sort-by rank (map :target g))))))
        has-in (into #{} (map :target) es)
        walk   (fn walk [acc id]
                 (reduce (fn [a c]
                           (if ((:seen a) c)
                             a
                             (-> a
                                 (update :seen conj c)
                                 (update-in [:children id] (fnil conj []) c)
                                 (walk c))))
                         acc
                         (get out id [])))]
    (reduce (fn [acc id]
              (if ((:seen acc) id)
                acc
                (-> acc (update :seen conj id) (update :roots conj id) (walk id))))
            {:seen #{} :roots [] :children {}}
            (concat (remove has-in ids) ids))))

(defn- lay-subtree
  [children id main acc]
  (let [kids (get children id [])]
    (if (empty? kids)
      (-> acc (assoc-in [:pos id] [main (:next acc)]) (update :next inc))
      (let [acc' (reduce (fn [a c] (lay-subtree children c (inc main) a)) acc kids)
            cs   (mapv #(second (get-in acc' [:pos %])) kids)]
        (assoc-in acc' [:pos id] [main (/ (+ (apply min cs) (apply max cs)) 2.0)])))))

(defn baum
  ([nodes edges] (baum nodes edges nil))
  ([nodes edges opts]
   (let [ids   (in-order nodes)
         es    (edges-within ids edges)
         f     (spanning ids es)
         {:keys [pos]} (reduce (fn [a r] (lay-subtree (:children f) r 0 a))
                               {:next 0 :pos {}}
                               (:roots f))]
     (mapv (fn [n]
             (let [[main cross] (get pos (:id n) [0 0])]
               (place-at n main cross (:richtung opts))))
           nodes))))

(defn- bounds [nodes]
  (when (seq nodes)
    {:min-x (apply min (map :x nodes))
     :min-y (apply min (map :y nodes))
     :max-x (apply max (map #(+ (:x %) (or (:w %) node-width)) nodes))
     :max-y (apply max (map #(+ (:y %) (or (:h %) node-height)) nodes))}))

(defn place
  [nodes edges positions]
  (let [placed?  (fn [n] (contains? positions (:id n)))
        placed   (mapv (fn [n] (merge n (get positions (:id n)))) (filter placed? nodes))
        unplaced (filterv (complement placed?) nodes)]
    (cond
      (empty? unplaced) placed
      (empty? placed)   (layered unplaced edges)
      :else
      (let [{:keys [max-x min-y]} (bounds placed)
            laid (layered unplaced edges)]
        (into placed
              (mapv (fn [n] (-> n
                                (update :x + max-x col-gap)
                                (update :y + min-y)))
                    laid))))))
