(ns blueprint-cljs.multi-slider
  "MultiSlider: any number of handles on one track.
   `[multi-slider {:min 0 :max 100 :on-change (fn [values]) :on-release f
                   :handles [{:value 20 :type :start :intent-after :primary}
                             {:value 70 :type :end :interaction-kind :push}]}]`
   Handles may also come as children `[handle {:value …}]`. Handle keys: `:value`,
   `:type` (:full :start :end), `:interaction-kind` (:lock default, :push, :none),
   `:intent-before`, `:intent-after`, `:track-style-before`, `:track-style-after`,
   `:on-change`, `:on-release`, `:class`, `:html-props`.
   Slider props: `:min` 0, `:max` 10, `:step-size` 1, `:label-step-size`,
   `:label-values`, `:label-precision`, `:label-renderer` (fn [v {:is-handle-tooltip b}] /
   false), `:show-track-fill` true, `:default-track-intent`, `:vertical`, `:disabled`,
   `:intent`."
  (:require [reagent.core :as r]
            [blueprint-cljs.core :as bp]
            [blueprint-cljs.common :as c]))

(def handle-size 16)

(defn handle
  "A declaration only: multi-slider reads its props."
  [_props]
  nil)

(defn- count-decimals
  [n]
  (if-not (js/isFinite n)
    0
    (loop [e 1 p 0]
      (if (= (/ (js/Math.round (* n e)) e) n) p (recur (* e 10) (inc p))))))

(defn- pct [ratio] (str (* 100 ratio) "%"))

(defn- clamp [v lo hi] (-> v (max lo) (min hi)))

(defn- approx= [a b] (< (js/Math.abs (- a b)) 0.00001))

(defn- handles-of
  [{:keys [handles]} children]
  (->> (concat handles
               (keep (fn [ch] (when (and (vector? ch) (= handle (first ch))) (second ch)))
                     children))
       (sort-by :value)
       vec))

(defn- interactive [hs] (vec (remove #(= :none (some-> (:interaction-kind %) keyword)) hs)))

(defn- label-values
  [{:keys [label-step-size label-values min max]}]
  (if (some? label-values)
    (vec label-values)
    (loop [i min acc []]
      (if (or (< i max) (approx= i max))
        (recur (+ i (or label-step-size 1)) (conj acc i))
        acc))))

(defn- fill-values
  [values from to v]
  (let [inc' (if (< from to) 1 -1)]
    (loop [vs values i (+ from inc')]
      (if (= i (+ to inc')) vs (recur (assoc vs i v) (+ i inc'))))))

(defn- new-values
  "Blueprint `getNewHandleValues`: moving handle `old-index` to `v`, honouring locks."
  [hs old-index v]
  (let [old  (mapv :value hs)
        nv   (vec (sort (assoc old old-index v)))
        ni   (.indexOf (clj->js nv) v)
        step (if (< old-index ni) 1 -1)
        lock (when (not= old-index ni)
               (some (fn [i]
                       (when (not= :push (some-> (:interaction-kind (nth hs i)) keyword)) i))
                     (range (+ old-index step) (+ ni step) step)))]
    (if (nil? lock)
      (fill-values nv old-index ni v)
      (fill-values old old-index lock (nth old lock)))))

(def ^:private defaults
  {:min 0 :max 10 :step-size 1 :show-track-fill true :default-track-intent :none})

(defn multi-slider
  [& _]
  (let [track  (atom nil)
        size   (r/atom nil)
        moving (r/atom nil)
        props* (atom nil)
        hs*    (atom nil)
        measure! (fn []
                   (when-let [t @track]
                     (let [{:keys [vertical]} @props*
                           s (if vertical (.-clientHeight t) (.-clientWidth t))]
                       (when (not= s @size) (reset! size s)))))]
    (letfn [(client->value [e]
              (let [{:keys [min max step-size vertical]} @props*
                    t    @track
                    rect (.getBoundingClientRect t)
                    pt   (if-let [touch (some-> (.-changedTouches e) (aget 0))] touch e)
                    ratio (if vertical
                            (/ (- (.-bottom rect) (.-clientY pt)) (.-height rect))
                            (/ (- (.-clientX pt) (.-left rect)) (.-width rect)))
                    raw  (+ min (* ratio (- max min)))]
                (clamp (+ min (* step-size (js/Math.round (/ (- raw min) step-size)))) min max)))
            (emit! [k index v]
              (let [hs    (interactive @hs*)
                    old   (mapv :value hs)
                    vals  (new-values hs index v)
                    p     @props*]
                (if (= k :change)
                  (when (not= vals old)
                    (when-let [f (:on-change p)] (f vals))
                    (doseq [[i h] (map-indexed vector hs)]
                      (when (and (not= (nth old i) (nth vals i)) (:on-change h))
                        ((:on-change h) (nth vals i)))))
                  (do (when-let [f (:on-release p)] (f vals))
                      (doseq [[i h] (map-indexed vector hs)]
                        (when (:on-release h) ((:on-release h) (nth vals i))))))))
            (move! [e]
              (when-let [i @moving]
                (when-not (:disabled @props*)
                  (emit! :change i (client->value e)))))
            (end! [e]
              (when-let [i @moving]
                (reset! moving nil)
                (doseq [[t f] [["mousemove" move!] ["mouseup" end!] ["touchmove" move!]
                               ["touchend" end!] ["touchcancel" end!]]]
                  (js/document.removeEventListener t f))
                (let [v (client->value e)]
                  (emit! :change i v)
                  (emit! :release i v))))
            (begin! [i e]
              (reset! moving i)
              (doseq [[t f] [["mousemove" move!] ["mouseup" end!] ["touchmove" move!]
                             ["touchend" end!] ["touchcancel" end!]]]
                (js/document.addEventListener t f))
              (emit! :change i (client->value e)))
            (nearest [e]
              (let [v (client->value e)
                    hs (interactive @hs*)]
                (when (seq hs)
                  (apply min-key #(js/Math.abs (- v (:value (nth hs %)))) (range (count hs))))))
            (track-down! [e]
              (when (and (not (:disabled @props*))
                         (nil? (.closest (.-target e) (str "." (bp/cls "slider-handle")))))
                (when-let [i (nearest e)] (begin! i e))))
            (format-label [v tooltip?]
              (let [{:keys [label-renderer label-precision step-size]} @props*]
                (cond
                  (false? label-renderer) nil
                  (fn? label-renderer)    (label-renderer v {:is-handle-tooltip tooltip?})
                  :else (.toFixed v (or label-precision (count-decimals step-size))))))
            (track-intent [start end]
              (let [{:keys [show-track-fill default-track-intent]} @props*]
                (cond (not show-track-fill)               :none
                      (some? (:intent-after start))       (:intent-after start)
                      (some? (:intent-before end))        (:intent-before end)
                      :else                               default-track-intent)))]
      (r/create-class
       {:display-name "MultiSlider"
        :component-did-mount measure!
        :component-did-update measure!
        :component-will-unmount (fn [] (when @moving (end! #js {:clientX 0 :clientY 0})))
        :reagent-render
        (fn [& args]
          (let [[props children] (c/split-args args)
                {:keys [class disabled label-renderer max min step-size vertical] :as p}
                (merge defaults props)
                _      (reset! props* p)
                hs     (handles-of p children)
                _      (reset! hs* hs)
                ratio  (/ 1 (- max min))
                offset (fn [v] (clamp (* (- v min) ratio) 0 1))
                stops  (conj hs {:value max})]
            @size
            [:div {:class (bp/cx (bp/cls "slider")
                                 {(bp/cls "disabled")       disabled
                                  (bp/cls "slider-unlabeled") (false? label-renderer)
                                  (bp/cls "vertical")       vertical}
                                 class)
                   :on-mouse-down  track-down!
                   :on-touch-start track-down!}
             (into [:div {:class (bp/cls "slider-track") :ref #(reset! track %)}]
                   (map-indexed
                    (fn [i [start end]]
                      (let [[a b] (sort [(offset (:value start)) (offset (:value end))])
                            style (merge (if vertical
                                           {:bottom (pct a) :left 0 :top (pct (- 1 b))}
                                           {:left (pct a) :right (pct (- 1 b)) :top 0})
                                         (or (:track-style-after start)
                                             (:track-style-before end)))]
                        ^{:key (str "track-" i)}
                        [:div {:class (bp/cx (bp/cls "slider-progress")
                                             (bp/intent-class (track-intent start end)))
                               :style style}]))
                    (map vector (cons {:value min} hs) stops)))
             (into [:div {:class (bp/cls "slider-axis")}]
                   (when-not (false? label-renderer)
                     (map-indexed
                      (fn [i v]
                        ^{:key i}
                        [:div {:class (bp/cls "slider-label")
                               :style (if vertical
                                        {:bottom (pct (/ (- v min) (- max min)))}
                                        {:left (pct (/ (- v min) (- max min)))})}
                         (format-label v false)])
                      (label-values p))))
             (for [[i {:keys [value type class html-props]}]
                   (map-indexed vector (interactive hs))
                   :let [label (format-label value true)
                         off   (str "calc(" (pct (* (- value min) ratio)) " - "
                                    (/ handle-size 2) "px)")]]
               ^{:key (str i "-" (count hs))}
               [:span (merge
                       {:role "slider" :tab-index 0}
                       html-props
                       {:class (bp/cx (bp/cls "slider-handle")
                                      {(bp/cls "active") (= i @moving)
                                       (bp/cls "start")  (= :start (some-> type keyword))
                                       (bp/cls "end")    (= :end (some-> type keyword))}
                                      class)
                        :style (if vertical {:bottom off} {:left off})
                        :aria-valuemin min :aria-valuemax max :aria-valuenow value
                        :aria-disabled disabled
                        :aria-orientation (if vertical "vertical" "horizontal")}
                       (when-not disabled
                         {:on-mouse-down  (fn [e] (.stopPropagation e) (begin! i e))
                          :on-touch-start (fn [e] (.stopPropagation e) (begin! i e))
                          :on-key-down
                          (fn [e]
                            (when-let [d (case (.-key e)
                                           ("ArrowLeft" "ArrowDown") -1
                                           ("ArrowRight" "ArrowUp") 1
                                           nil)]
                              (.preventDefault e)
                              (let [v (clamp (+ value (* d step-size)) min max)]
                                (when (not= v value) (emit! :change i v)))))
                          :on-key-up
                          (fn [e]
                            (when (#{"ArrowLeft" "ArrowDown" "ArrowRight" "ArrowUp"} (.-key e))
                              (emit! :release i value)))}))
                (when (some? label) [:span {:class (bp/cls "slider-label")} label])])]))}))))
