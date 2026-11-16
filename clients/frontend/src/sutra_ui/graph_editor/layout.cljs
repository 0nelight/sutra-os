(ns sutra-ui.graph-editor.layout
  (:require [sutra.types.decimal :as d]
            ["dagre" :as dagre]
            ["elkjs/lib/elk.bundled.js" :default ELK]))

(def ^:private default-w 220)
(def ^:private default-h 100)

(defn- num-or
  [v default]
  (or (d/lossy-number v) default))

(defn- nodes-info [rf-nodes]
  (vec
   (for [^js n rf-nodes]
     {:id (.-id n)
      :kind (.-type n)
      :w  (num-or (.-width n)  default-w)
      :h  (num-or (.-height n) default-h)
      :x  (num-or (some-> (.-position n) .-x) 0)
      :y  (num-or (some-> (.-position n) .-y) 0)})))

(defn- edges-info [rf-edges]
  (vec
   (for [^js e rf-edges]
     {:source (.-source e)
      :target (.-target e)})))

(defn- dagre-place
  [ns es direction]
  (let [g (dagre/graphlib.Graph.)]
    (.setGraph g #js {:rankdir direction
                      :nodesep 60 :ranksep 110
                      :marginx 40 :marginy 40})
    (.setDefaultEdgeLabel g (fn [] #js {}))
    (doseq [{:keys [id w h]} ns]
      (.setNode g id #js {:width w :height h}))
    (doseq [{:keys [source target minlen weight]} es]
      (.setEdge g source target (clj->js (cond-> {}
                                           minlen (assoc :minlen minlen)
                                           weight (assoc :weight weight)))))
    (dagre/layout g)
    (reduce (fn [m {:keys [id w h]}]
              (if-let [^js nd (.node g id)]
                (assoc m id {:x (- (.-x nd) (/ w 2))
                             :y (- (.-y nd) (/ h 2))})
                m))
            {} ns)))

(defn dagre-layout
  ([rf-nodes rf-edges] (dagre-layout rf-nodes rf-edges nil))
  ([rf-nodes rf-edges {:keys [direction] :or {direction "LR"}}]
   (js/Promise.resolve (dagre-place (nodes-info rf-nodes) (edges-info rf-edges) direction))))

(def ^:private reversed {"LR" "RL" "TB" "BT"})

(defn- away-from
  [es root]
  (let [nbrs (reduce (fn [m {:keys [source target]}]
                       (-> m (update source (fnil conj #{}) target)
                           (update target (fnil conj #{}) source)))
                     {} es)
        dist (loop [d {root 0} q #queue [root]]
               (if-let [x (peek q)]
                 (let [fresh (remove d (get nbrs x))]
                   (recur (into d (map (fn [y] [y (inc (d x))])) fresh)
                          (into (pop q) fresh)))
                 d))]
    (mapv (fn [{:keys [source target] :as e}]
            (let [a (dist source) b (dist target)]
              (if (and a b (> a b)) {:source target :target source} e)))
          es)))

(def ^:private anchor-id "layout/sources-anchor")

(defn- sources-first
  [ns es]
  (let [srcs (into #{} (keep #(when (= "datasource" (:kind %)) (:id %))) ns)
        fed  (set (map :target es))]
    (if (empty? srcs)
      [ns es]
      [(conj (vec ns) {:id anchor-id :w 1 :h 1})
       (into (vec es)
             (for [{:keys [id]} ns
                   :when (not (fed id))]
               (if (srcs id)
                 {:source anchor-id :target id :minlen 1 :weight 10}
                 {:source anchor-id :target id :minlen 2 :weight 0})))])))

(defn- aside
  [alone placed ns]
  (when (seq alone)
    (let [dims (into {} (map (juxt :id identity)) ns)
          y0   (if (seq placed)
                 (+ 120 (apply max (for [[id {:keys [y]}] placed] (+ y (:h (dims id))))))
                 40)
          x0   (if (seq placed) (apply min (map :x (vals placed))) 40)
          cols (max 1 (Math/ceil (Math/sqrt (count alone))))
          cw   (+ 60 (apply max (map :w alone)))
          ch   (+ 60 (apply max (map :h alone)))]
      (into {} (map-indexed (fn [i {:keys [id]}]
                              [id {:x (+ x0 (* cw (mod i cols)))
                                   :y (+ y0 (* ch (quot i cols)))}])
                            (sort-by :id alone))))))

(defn- hierarchy-place
  [ns es {:keys [direction reverse? root] :or {direction "LR"}}]
  (let [rooted? (and root (some #(= root (:id %)) ns))
        es      (if rooted? (away-from es root) es)
        linked  (into #{} (mapcat (juxt :source :target)) es)
        alone   (remove (comp linked :id) ns)
        [ns' es'] (if rooted?
                    [(filter (comp linked :id) ns) es]
                    (sources-first (filter (comp linked :id) ns) es))
        placed  (if (seq es')
                  (dissoc (dagre-place ns' es' (cond-> direction reverse? reversed)) anchor-id)
                  {})]
    (merge placed (aside alone placed ns))))

(defn hierarchy-layout
  [rf-nodes rf-edges opts]
  (js/Promise.resolve (hierarchy-place (nodes-info rf-nodes) (edges-info rf-edges) opts)))

(defn- components
  [ns es]
  (let [ids  (mapv :id ns)
        nbrs (reduce (fn [m {:keys [source target]}]
                       (-> m (update source (fnil conj #{}) target)
                           (update target (fnil conj #{}) source)))
                     {} es)]
    (loop [left ids seen #{} out []]
      (if-let [id (first left)]
        (if (seen id)
          (recur (rest left) seen out)
          (let [comp (loop [c #{id} q [id]]
                       (if-let [x (peek q)]
                         (let [fresh (remove c (get nbrs x))]
                           (recur (into c fresh) (into (pop q) fresh)))
                         c))]
            (recur (rest left) (into seen comp) (conj out comp))))
        out))))

(defn- bounds [m ns]
  (let [dims (into {} (map (juxt :id identity)) ns)
        xs   (for [[id {:keys [x]}] m] [x (+ x (:w (dims id)))])
        ys   (for [[id {:keys [y]}] m] [y (+ y (:h (dims id)))])]
    {:x0 (apply min (map first xs)) :x1 (apply max (map second xs))
     :y0 (apply min (map first ys)) :y1 (apply max (map second ys))}))

(defn clustered-layout
  [rf-nodes rf-edges opts]
  (let [ns (nodes-info rf-nodes)
        es (edges-info rf-edges)
        by (into {} (map (juxt :id identity)) ns)
        cs (sort-by (comp - count) (components ns es))]
    (js/Promise.resolve
     (first
      (reduce (fn [[acc top] c]
                (let [cn (mapv by (sort c))
                      ce (filterv #(and (c (:source %)) (c (:target %))) es)
                      m  (hierarchy-place cn ce opts)
                      {:keys [x0 y0 y1]} (bounds m cn)]
                  [(into acc (for [[id {:keys [x y]}] m]
                               [id {:x (+ 40 (- x x0)) :y (+ top (- y y0))}]))
                   (+ top (- y1 y0) 120)]))
              [{} 40] cs)))))

(defn- reading-order
  [ns axis]
  (sort-by (juxt axis (if (= axis :x) :y :x) :id) ns))

(defn grid-layout
  [rf-nodes _rf-edges {:keys [columns staggered?]}]
  (let [ns   (reading-order (nodes-info rf-nodes) :y)
        n    (count ns)
        cols (max 1 (or columns (Math/ceil (Math/sqrt n))))
        cw   (+ 60 (apply max 0 (map :w ns)))
        ch   (+ 60 (apply max 0 (map :h ns)))]
    (js/Promise.resolve
     (into {} (map-indexed
               (fn [i {:keys [id]}]
                 (let [r (quot i cols) c (mod i cols)]
                   [id {:x (+ 40 (* c cw) (if (and staggered? (odd? r)) (/ cw 2) 0))
                        :y (+ 40 (* r ch))}]))
               ns)))))

(defn line-layout
  [rf-nodes _rf-edges {:keys [axis]}]
  (let [ns (reading-order (nodes-info rf-nodes) axis)]
    (js/Promise.resolve
     (first (reduce (fn [[m at] {:keys [id w h]}]
                      [(assoc m id (if (= axis :x) {:x at :y 40} {:x 40 :y at}))
                       (+ at 60 (if (= axis :x) w h))])
                    [{} 40] ns)))))

(defn circular-layout
  [rf-nodes _rf-edges _opts]
  (let [ns (reading-order (nodes-info rf-nodes) :y)
        n  (count ns)
        r  (max 200 (/ (* n (+ 40 (apply max 0 (map :w ns)))) (* 2 Math/PI)))]
    (js/Promise.resolve
     (into {} (map-indexed
               (fn [i {:keys [id w h]}]
                 (let [a (- (* 2 Math/PI (/ i (max 1 n))) (/ Math/PI 2))]
                   [id {:x (+ r 40 (* r (Math/cos a)) (- (/ w 2)))
                        :y (+ r 40 (* r (Math/sin a)) (- (/ h 2)))}]))
               ns)))))

(def ^:private elk (delay (ELK.)))

(defn elk-layout
  ([rf-nodes rf-edges] (elk-layout rf-nodes rf-edges nil))
  ([rf-nodes rf-edges {:keys [algorithm direction]
                       :or   {algorithm "layered" direction "RIGHT"}}]
   (let [ns    (nodes-info rf-nodes)
         es    (edges-info rf-edges)
         input #js {:id "root"
                    :layoutOptions
                    #js {"elk.algorithm"        algorithm
                         "elk.direction"        direction
                         "elk.spacing.nodeNode" "60"
                         "elk.layered.spacing.nodeNodeBetweenLayers" "90"}
                    :children
                    (clj->js
                     (mapv (fn [{:keys [id w h]}]
                             {:id id :width w :height h})
                           ns))
                    :edges
                    (clj->js
                     (vec (for [[i e] (map-indexed vector es)]
                            {:id      (str "e" i)
                             :sources [(:source e)]
                             :targets [(:target e)]})))}]
     (-> (.layout @elk input)
         (.then (fn [^js g]
                  (reduce
                   (fn [m ^js c]
                     (assoc m (.-id c)
                            {:x (num-or (.-x c) 0)
                             :y (num-or (.-y c) 0)}))
                   {}
                   (.-children g))))))))
