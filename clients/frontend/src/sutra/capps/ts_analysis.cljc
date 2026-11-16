(ns sutra.capps.ts-analysis
  (:require [sutra.kit.coerce :refer [->kw]]
            [sutra.capps.chart-spec :as cs]
            [sutra.capps.ts-ops :as ops]))

(def default-display
  {:dash :solid :width 2 :gradient? false :shape :none :size 6 :fill nil :outline 0
   :interpolation :linear :external :none :axis 0})

(def default-axis
  {:unit nil :auto? true :min nil :max nil :side :left :log? false :invert? false})

(defn empty-analysis
  [{:keys [canvases]}]
  {:plots [] :event-sets [] :axes {0 default-axis} :canvases (or canvases 1) :next-id 1})

(defn- fresh
  [a kv]
  (let [id (:next-id a)]
    [(assoc a :next-id (inc id)) (assoc kv :id id)]))

(defn add-series
  [a {:keys [label canvas points controlled? source]}]
  (let [[a p] (fresh a {:source source :label label :canvas (or canvas 0)
                        :points (vec points) :controlled? (boolean controlled?)
                        :display default-display})]
    (update a :plots conj p)))

(defn add-plot
  [a {:keys [plot-type inputs params canvas label]}]
  (let [[a p] (fresh a {:plot-type (->kw plot-type) :inputs (vec inputs) :params (or params {})
                        :canvas (or canvas 0)
                        :label (or label (name (->kw plot-type)))
                        :display default-display})]
    (update a :plots conj p)))

(defn add-event-set
  [a {:keys [label events controlled? kind source]}]
  (let [[a e] (fresh a {:label label :events (vec events) :controlled? (boolean controlled?)
                        :event-kind kind :source source :highlight? true})]
    (update a :event-sets conj e)))

(defn- edit-by-id
  [coll id f]
  (mapv #(if (= id (:id %)) (f %) %) coll))

(defn set-display
  [a id k v]
  (update a :plots edit-by-id id #(assoc-in % [:display k] v)))

(defn set-params
  [a id k v]
  (update a :plots edit-by-id id #(assoc-in % [:params k] v)))

(defn set-canvas
  [a id canvas]
  (update a :plots edit-by-id id #(assoc % :canvas canvas)))

(defn set-axis
  [a axis k v]
  (update a :axes update axis #(assoc (or % default-axis) k v)))

(defn new-axis
  [a]
  (let [n (inc (apply max 0 (keys (:axes a))))]
    (assoc-in a [:axes n] default-axis)))

(defn add-canvas [a] (update a :canvases inc))

(defn remove-plot
  [a id]
  (let [p (some #(when (= id (:id %)) %) (:plots a))
        used (some #(some #{id} (:inputs %)) (:plots a))]
    (if (or (nil? p) (:controlled? p) used)
      a
      (update a :plots (fn [ps] (filterv #(not= id (:id %)) ps))))))

(defn remove-event-set
  [a id]
  (update a :event-sets (fn [es] (filterv #(or (:controlled? %) (not= id (:id %))) es))))

(defn toggle-highlight
  [a id]
  (update a :event-sets edit-by-id id #(update % :highlight? not)))

(defn resolved-points
  [{:keys [plots event-sets]}]
  (let [by-id  (into {} (map (juxt :id identity)) plots)
        events (vec (mapcat :events event-sets))]
    (letfn [(points [memo id]
              (if (contains? memo id)
                memo
                (let [p (by-id id)]
                  (if-let [ins (:inputs p)]
                    (let [memo (reduce points memo ins)
                          got  (try (ops/derive-plot p (mapv memo ins) events)
                                    (catch #?(:cljs :default :clj Exception) e
                                      {:error (ex-message e)}))]
                      (assoc memo id got))
                    (assoc memo id (:points p))))))]
      (reduce points {} (map :id plots)))))

(def ^:private interpolation->shape
  {:linear "linear" :nearest "hvh" :step-before "vh" :step-after "hv" :none nil})

(defn- external-points
  [points external [lo hi]]
  (if-not (and (= :nearest (->kw external)) (seq points))
    points
    (cond-> (vec points)
      (and lo (< lo (first (first points)))) (->> (cons [lo (second (first points))]) vec)
      (and hi (> hi (first (last points))))  (conj [hi (second (last points))]))))

(defn- cs-time
  [t utc?]
  #?(:cljs (let [d (js/Date. t)]
             (if utc?
               (.toISOString d)
               (let [o (* 60000 (.getTimezoneOffset d))]
                 (subs (.toISOString (js/Date. (- t o))) 0 23))))
     :clj  (let [_ utc?] (str (java.time.Instant/ofEpochMilli t)))))

(defn- line-trace
  [{:keys [label display]} points color axis-ref view utc?]
  (let [{:keys [dash width gradient? shape size fill outline interpolation external]} display
        pts   (external-points points external view)
        interp (->kw interpolation)
        lines? (not= :none interp)
        marks? (not= :none (->kw shape))]
    (cond-> {:type "scatter"
             :x (mapv #(cs-time (first %) utc?) pts)
             :y (mapv second pts)
             :name label
             :yaxis axis-ref
             :mode (cond (and lines? marks?) "lines+markers" lines? "lines" :else "markers")
             :line {:color color :width width
                    :dash (if (= :dashed (->kw dash)) "dash" "solid")
                    :shape (get interpolation->shape interp "linear")}}
      gradient? (assoc :fill "tozeroy")
      marks?    (assoc :marker {:symbol (name (->kw shape)) :size size
                                :color (or fill color)
                                :line {:width (or outline 0) :color color}}))))

(defn- event-shapes
  [event-sets x-axis utc?]
  (for [{:keys [events highlight?]} event-sets :when highlight?
        {:keys [start end]} events]
    {:type "rect" :xref x-axis :yref "paper" :y0 0 :y1 1
     :x0 (cs-time start utc?) :x1 (cs-time end utc?)
     :fillcolor "rgba(214,69,65,0.12)" :line {:width 0}}))

(defn- axis-layout
  [{:keys [auto? min max side log? invert? unit]} {:keys [overlay-y? collapsed? boundaries?]} i]
  (cond-> {:side (name (or (->kw side) :left))
           :automargin (not overlay-y?)
           :showgrid (zero? i)
           :tickfont {:size 11}}
    (pos? i)    (assoc :overlaying "y" :anchor "x")
    unit        (assoc :title {:text unit})
    (and (not auto?) (some? min) (some? max)) (assoc :range (if invert? [max min] [min max]))
    log?        (assoc :type "log")
    (and invert? auto?) (assoc :autorange "reversed")
    collapsed?  (assoc :showticklabels (boolean boundaries?) :nticks (if boundaries? 2 0))))

(defn- bollinger-traces
  [plot got color axis-ref view utc?]
  (mapv (fn [[k opacity]]
          (-> (line-trace (assoc plot :label (str (:label plot) " " (name k)))
                          (get got k) color axis-ref view utc?)
              (assoc :opacity opacity)))
        [[:mid 1] [:upper 0.6] [:lower 0.6]]))

(defn canvas-spec
  [{:keys [plots axes event-sets]} resolved canvas view opts]
  (let [{:keys [utc? tooltip? digits]} opts
        here (filterv #(= canvas (:canvas %)) plots)
        ax-ids (vec (sort (distinct (map #(get-in % [:display :axis] 0) here))))
        ax-ref (fn [a] (let [i (max 0 (.indexOf ax-ids a))] (if (zero? i) "y" (str "y" (inc i)))))
        traces (vec (mapcat
                     (fn [i plot]
                       (let [got   (get resolved (:id plot))
                             color (nth cs/palette (mod i (count cs/palette)))
                             ref   (ax-ref (get-in plot [:display :axis] 0))]
                         (cond
                           (:error got) nil
                           (map? got)   (bollinger-traces plot got color ref view utc?)
                           :else        [(line-trace plot got color ref view utc?)])))
                     (range) here))
        layout (reduce (fn [l [i a]]
                         (assoc l (keyword (if (zero? i) "yaxis" (str "yaxis" (inc i))))
                                (axis-layout (get axes a default-axis) opts i)))
                       {:margin {:l 56 :r 32 :t 12 :b 36}
                        :paper_bgcolor "transparent" :plot_bgcolor "transparent"
                        :showlegend true :legend {:orientation "h" :y -0.2}
                        :font {:size 11}
                        :hovermode (if (false? tooltip?) false "x unified")
                        :hoverlabel {:namelength -1}
                        :xaxis (cond-> {:type "date"}
                                 (first view) (assoc :range [(cs-time (first view) utc?)
                                                             (cs-time (second view) utc?)]))
                        :shapes (vec (event-shapes event-sets "x" utc?))}
                       (map-indexed vector (or (seq ax-ids) [0])))]
    {:traces (mapv #(cond-> %
                      digits (assoc :hovertemplate (str "%{y:." digits "r}"
                                                        "<extra>%{fullData.name}</extra>")))
                   traces)
     :layout layout}))

(defn full-range
  [resolved]
  (let [ts (mapcat (fn [got]
                     (cond (map? got) (mapcat #(map first (val %)) (dissoc got :error))
                           :else      (map first got)))
                   (vals resolved))]
    (when (seq ts) [(apply min ts) (apply max ts)])))

(defn default-view
  [{:keys [view-range view-start view-end view-relative-ms]} resolved now]
  (case (->kw view-range)
    :fixed    [view-start view-end]
    :relative [(- now (or view-relative-ms 0)) now]
    (full-range resolved)))

(defn stat-row
  [plot resolved view]
  (let [got (get resolved (:id plot))]
    (cond
      (:error got) {:error (:error got)}
      (map? got)   (ops/stats (:mid got) view)
      :else        (ops/stats got view))))

(defn saveable
  [a]
  (-> a
      (update :plots (fn [ps] (mapv #(dissoc % :points) ps)))
      (update :event-sets (fn [es] (mapv #(if (:source %) (dissoc % :events) %) es)))))
