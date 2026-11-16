(ns sutra-ui.ui.table
  (:require [sutra.kit.coerce :refer [->kw]]
            [reagent.core :as r]
            [clojure.string :as str]
            [sutra-ui.ui.size :as groesse]
            [sutra-ui.ui.core :as ui]
            [sutra-ui.ui.sel :refer [rd wr! as-set]]
            [sutra.capps.pivot :as pivot]
            [sutra-ui.render-ctx :as rctx]
            [sutra.capps.spreadsheet :as ss]
            [sutra.capps.table :as tbl]
            [sutra.i18n.core :as i18n]))

(defn committing
  ([display-name render] (committing display-name render nil))
  ([display-name render {:keys [mount unmount]}]
   (let [pending  (atom [])
         propose! (fn [still? apply!] (swap! pending conj [still? apply!]))
         commit!  (fn [& _]
                    (let [ps @pending]
                      (reset! pending [])
                      (doseq [[still? apply!] ps] (when (still?) (apply!)))))]
     (r/create-class
      {:display-name           display-name
       :component-did-mount    (fn [this] (when mount (mount this)) (commit! this))
       :component-did-update   commit!
       :component-will-unmount (fn [this] (when unmount (unmount this)))
       :reagent-render         (fn [& args] (reset! pending []) (apply render propose! args))}))))

(defn- align-class [align]
  (case (->kw align) :right "capp-cell--right" :center "capp-cell--center" nil))

(defn- freeze-offsets! [^js table n]
  (when table
    (let [ths (.querySelectorAll table "thead th")]
      (loop [i 0 left 0]
        (when (and (< i n) (< i (.-length ths)))
          (.setProperty (.-style table) (str "--capp-fz-" i) (str left "px"))
          (recur (inc i) (+ left (.-offsetWidth (aget ths i)))))))))

(defn- wide-screen? []
  (not (groesse/kompakt?)))

(defn- start-col-resize!
  [widths k ^js e]
  (.preventDefault e)
  (.stopPropagation e)
  (let [x0   (.-clientX e)
        w0   (.. e -target -parentElement -offsetWidth)
        move (fn [^js m] (swap! widths assoc k (max 40 (+ w0 (- (.-clientX m) x0)))))
        up   (atom nil)]
    (reset! up (fn []
                 (js/window.removeEventListener "mousemove" move)
                 (js/window.removeEventListener "mouseup" @up)))
    (js/window.addEventListener "mousemove" move)
    (js/window.addEventListener "mouseup" @up)))

(defn col-grip
  [widths k]
  [:span.capp-col-grip {:on-mouse-down #(start-col-resize! widths k %)
                        :on-click      #(.stopPropagation %)}])

(defn- truncated-title!
  [^js e]
  (let [t    (.-currentTarget e)
        cut? (fn [^js n] (> (.-scrollWidth n) (.-clientWidth n)))]
    (set! (.-title t) (if (some cut? (cons t (array-seq (.querySelectorAll t "*"))))
                        (.-textContent t)
                        ""))))

(defn- more-sentinel
  [_]
  (let [obs (atom nil)]
    (r/create-class
     {:display-name           "capp-table-more"
      :component-will-unmount (fn [_] (some-> ^js @obs .disconnect))
      :reagent-render
      (fn [{:keys [loaded total load!]}]
        [:div.capp-table__more
         {:ref (fn [el]
                 (some-> ^js @obs .disconnect)
                 (reset! obs (when el
                               (doto (js/IntersectionObserver.
                                      (fn [es] (when (.-isIntersecting (aget es 0)) (load!)))
                                      #js {:rootMargin "200px"})
                                 (.observe el)))))}
         (i18n/t :ui/capp-truncated {:shown loaded :total total})])})))

(def ^:private window-from 120)
(def ^:private overscan 20)

(defn- in-view
  [^js box ^js tbody h n]
  (let [b  (.getBoundingClientRect box)
        t  (.-top (.getBoundingClientRect tbody))
        lo (max (.-top b) 0)
        hi (min (.-bottom b) js/window.innerHeight)
        from (max 0 (- (js/Math.floor (/ (- lo t) h)) overscan))
        to   (min n (+ (js/Math.ceil (/ (- hi t) h)) overscan))]
    [from (max from to)]))

(defn- spacer [k n h ncols]
  (when (pos? n)
    [:tr {:key k :class "capp-table__spacer" :aria-hidden true}
     [:td {:col-span ncols :style {:height (* n h)}}]]))

(defn- copy-text
  [c row]
  (let [v (cond
            (:copy c)  ((:copy c) row)
            (:render c) (let [x ((:render c) row)]
                          (if (or (string? x) (number? x))
                            x
                            (some-> (tbl/field-fn (:field c)) (apply [row]))))
            :else      (some-> (tbl/field-fn (:field c)) (apply [row])))]
    (if (nil? v) "" (str v))))

(defn- range-tsv
  [cols rows {:keys [all?] :as rng}]
  (let [{:keys [start-col end-col] :as rg} (ss/selection-range rng)
        cs   (->> (range start-col (inc end-col))
                  (keep #(nth cols % nil))
                  (remove :no-range?))
        rg   (assoc rg :start-col 0 :end-col (dec (count cs)))
        body (ss/range-tsv rg (fn [[r c]]
                                (if-let [row (nth rows r nil)] (copy-text (nth cs c) row) "")))]
    (cond->> body
      all? (str (str/join "\t" (map #(if (string? (:label %)) (:label %) "") cs)) "\n"))))

(defn- spans? [{:keys [a b all?]}] (or all? (not= a b)))

(defn- table-keys!
  [rng cols rows ^js e]
  (when (identical? (.-target e) (.-currentTarget e))
    (let [mod? (or (.-ctrlKey e) (.-metaKey e))
          k    (str/lower-case (str (.-key e)))]
      (cond
        (and mod? (= k "a") (seq rows))
        (do (.preventDefault e)
            (reset! rng {:a [0 0] :b [(dec (count rows)) (dec (count cols))] :all? true}))
        (and mod? (= k "c") @rng)
        (do (.preventDefault e)
            (some-> js/navigator .-clipboard (.writeText (range-tsv cols rows @rng))))
        (and (= k "escape") @rng)
        (do (.stopPropagation e) (reset! rng nil))))))

(defn- range-down!
  [{:keys [rng drag dragged box]} r c ^js e]
  (when (and (zero? (.-button e))
             (not (.closest (.-target e) "input,button,select,textarea,label")))
    (if (and (.-shiftKey e) @rng)
      (do (swap! rng assoc :b [r c] :all? false) (reset! dragged true))
      (do (reset! rng {:a [r c] :b [r c]}) (reset! dragged false)))
    (.preventDefault e)
    (some-> ^js @box (.focus #js {:preventScroll true}))
    (reset! drag true)
    (let [up (fn up [] (reset! drag false) (js/window.removeEventListener "mouseup" up))]
      (js/window.addEventListener "mouseup" up))))

(defn- range-over! [{:keys [rng drag dragged]} r c]
  (when (and @drag @rng (not= [r c] (:b @rng)))
    (swap! rng assoc :b [r c])
    (reset! dragged true)))

(defn- object-table-view
  [propose! {:keys [el box widths win row-h rng dragged] :as st}
   {:keys [cols rows row-key row-class row-style on-row-click foot empty more
           selected active lines frozen-cols on-row-context head-cell preview-rows]
    multi? :multi-select? auto? :auto-active? wrap? :wrap? fit? :fit-columns?
    narrow? :narrow? select-all? :select-all? results? :results?}]
  (let [keyf (or (tbl/field-fn row-key) :id)
        held      (rd active)
        sync      (when (satisfies? IReset active)
                    (tbl/active-sync held rows (and (not (false? auto?)) (wide-screen?))))
        act-row   (or (first sync) (tbl/active-row held))
        act-key   (some-> act-row keyf str not-empty)
        activate! (fn [row] (when (satisfies? IReset active) (reset! active [row])))
        picked (when multi? (as-set (rd selected)))
        pick!  (fn [row checked?]
                 (when (satisfies? ISwap selected)
                   (swap! selected
                          (fn [cur]
                            (let [cur (as-set cur)]
                              (if checked? (conj cur row) (disj cur row)))))))
        all?   (and (seq rows) (every? #(contains? picked %) rows))
        cols   (cond->> cols
                 multi?
                 (into [{:label (if select-all?
                                  [ui/checkbox {:checked (boolean all?) :class "u-m-0"
                                                :on-change #(when (satisfies? IReset selected)
                                                              (reset! selected
                                                                      (if all? #{} (set rows))))}]
                                  "")
                         :no-range?  true
                         :cell-class "capp-table__td--check"
                         :render (fn [row]
                                   [:span {:on-click #(.stopPropagation %)}
                                    [ui/checkbox
                                     {:checked   (contains? picked row)
                                      :on-change #(pick! row (.. % -target -checked))}]])}]))
        n      (count rows)
        ncols  (count cols)
        virt?  (and (> n window-from) (not wrap?) (not lines) (wide-screen?))
        rest-n (if (and preview-rows (not @(:open-all st)) (not (wide-screen?))
                        (> n (inc preview-rows)))
                 (- n preview-rows)
                 0)
        h      @row-h
        [from to] (cond virt?         (let [[f t] @win] [(min f n) (min (max t f) n)])
                        (pos? rest-n) [0 preview-rows]
                        :else         [0 n])
        shown? (spans? @rng)
        frozen (let [n (or frozen-cols 0)] (if multi? (if (pos? n) (inc n) 0) n))
        fz     (fn [i cls style]
                 (if (< i frozen)
                   [(str cls " capp-table__cell--frozen")
                    (assoc style :left (str "var(--capp-fz-" i ")"))]
                   [cls style]))
        col-k  (fn [i c] [i (:label c)])
        col-w  (fn [i c] (or (get @widths (col-k i c)) (:width c)))
        cell (fn [r i c row]
               (let [w           (col-w i c)
                     [cls style] (fz i (cond-> "capp-table__td"
                                         (align-class (:align c))
                                         (str " " (align-class (:align c)))
                                         (:cell-class c) (str " " (:cell-class c))
                                         (and shown? (not (:no-range? c))
                                              (ss/selected? @rng [r i]))
                                         (str " capp-cell--range"))
                                     (merge (:cell-style c)
                                            (when-let [f (:cell-style-of c)] (f row))))]
                 [:td (cond-> {:class cls :style style}
                        (and (string? (:label c)) (not (str/blank? (:label c))))
                        (assoc :data-label (:label c))
                        (not (:no-range? c))
                        (assoc :on-mouse-down #(range-down! st r i %)
                               :on-mouse-enter #(range-over! st r i)))
                  (let [v (when-let [f (:render c)] (f row))]
                    (cond
                      (or (nil? v) (= "" v)) nil
                      lines [:div {:class "capp-cell__clamp"} v]
                      wrap? v
                      :else [:div {:class          "capp-cell__text"
                                   :style          (when w {:max-width w})
                                   :on-mouse-enter truncated-title!}
                             v]))]))
        klick?  (or on-row-click (satisfies? IReset active))
        line (fn [r row]
               (into [:tr {:key      (or (not-empty (str (keyf row))) (str "row-" r))
                           :class    (cond-> (or (when row-class (row-class row)) "")
                                       klick?                            (str " capp-row--click")
                                       (and act-key (= act-key (str (keyf row))))
                                       (str " capp-row--sel"))
                           :style    (when row-style (row-style row))
                           :on-context-menu (when on-row-context #(on-row-context row %))
                           :on-click (when klick?
                                       #(if @dragged
                                          (reset! dragged false)
                                          (do (activate! row)
                                              (when on-row-click (on-row-click row)))))}]
                     (map-indexed #(cell r %1 %2 row) cols)))]
    (when sync (propose! #(= held (rd active)) #(reset! active sync)))
    (when (pos? frozen) (propose! (constantly true) #(freeze-offsets! @el frozen)))
    (when virt? (propose! (constantly true) #((:measure! st) n)))
    [:div {:class     (cond-> "capp-table"
                        wrap? (str " capp-table--wrap")
                        lines (str " capp-table--lines")
                        fit?  (str " capp-table--fit")
                        narrow? (str " capp-table--narrow")
                        results? (str " capp-table--results"))
           :style     (when lines {"--capp-lines" lines})
           :tab-index 0
           :ref       #(reset! box %)
           :on-key-down #(table-keys! rng cols rows %)}
     [ui/html-table {:compact     true
                     :interactive (some? on-row-click)
                     :class       "capp-table__table"
                     :ref         #(reset! el %)}
      [:thead
       (into [:tr]
             (map-indexed
              (fn [i c]
                (let [w           (col-w i c)
                      [cls style] (fz i (cond-> "capp-table__th"
                                          (align-class (:align c))
                                          (str " " (align-class (:align c))))
                                      (when w {:width w :min-width w :max-width w}))]
                  [:th {:class cls :style style}
                   (if head-cell (head-cell c) (:label c))
                   [col-grip widths (col-k i c)]]))
              cols))]
      (-> [:tbody (spacer "top" from h ncols)]
          (into (map line (range from to) (subvec (vec rows) from to)))
          (conj (if (pos? rest-n)
                  [:tr {:key "rest" :class "capp-table__rest"
                        :on-click #(reset! (:open-all st) true)}
                   [:td {:col-span ncols} (i18n/t :ui/capp-rows-more {:n rest-n})]]
                  (spacer "bottom" (- n to) h ncols))))
      (when foot [:tfoot {:class "capp-table__foot"} foot])]
     (when more ^{:key (:loaded more)} [more-sentinel more])
     (when (and (empty? rows) (nil? foot))
       [ui/empty-state
        (if (map? empty)
          (merge {:icon :th :title (i18n/t :ui/capp-no-objects)} empty)
          {:icon :th :title (or (not-empty (str empty)) (i18n/t :ui/capp-no-objects))})])]))

(defn- object-table*
  [_cfg]
  (let [el      (atom nil)
        box     (atom nil)
        n-now   (atom 0)
        frame   (atom nil)
        win     (r/atom [0 (* 3 overscan)])
        row-h   (r/atom 28)
        place!  (fn []
                  (reset! frame nil)
                  (when-let [tbody (some-> ^js @el (.querySelector "tbody"))]
                    (let [w (in-view @box tbody @row-h @n-now)]
                      (when (not= w @win) (reset! win w)))))
        later!  (fn [] (when-not @frame (reset! frame (js/requestAnimationFrame place!))))
        st      {:el el :box box :win win :row-h row-h
                 :widths  (r/atom {})
                 :rng     (r/atom nil)
                 :open-all (r/atom false)
                 :drag    (atom false)
                 :dragged (atom false)
                 :measure! (fn [n]
                             (reset! n-now n)
                             (when-let [^js tr (some-> ^js @el
                                                       (.querySelector
                                                        "tbody > tr:not(.capp-table__spacer)"))]
                               (let [h (.-offsetHeight tr)]
                                 (when (and (pos? h) (not= h @row-h)) (reset! row-h h))))
                             (later!))}]
    (committing "capp-object-table"
                (fn [propose! cfg] (object-table-view propose! st cfg))
                {:mount   (fn [_]
                            (js/window.addEventListener "scroll" later! true)
                            (js/window.addEventListener "resize" later!))
                 :unmount (fn [_]
                            (some-> @frame js/cancelAnimationFrame)
                            (js/window.removeEventListener "scroll" later! true)
                            (js/window.removeEventListener "resize" later!))})))

(defn object-table
  [cfg]
  [object-table* cfg])

(defn table-skeleton
  [cols]
  (let [cols (or (seq (filterv map? (when (sequential? cols) cols))) (repeat 3 nil))]
    [:div {:class "capp-table capp-table--skeleton" :role "status" :aria-busy "true"}
     [ui/html-table {:compact true :class "capp-table__table"}
      [:thead (into [:tr] (map (fn [c] [:th {:class "capp-table__th"} (:label c)])) cols)]
      (into [:tbody]
            (for [i (range 3)]
              (into [:tr {:key i}]
                    (map (fn [_] [:td {:class "capp-table__td"}
                                  [:span {:class (ui/skeleton true "capp-skeleton")}]]))
                    cols)))]]))

(defn- shown
  [{:keys [value fmt]} cell-fn]
  (cond
    cell-fn (cell-fn value)
    fmt     ((tbl/fmt-by fmt) value (rctx/current))
    :else   (tbl/react-safe value)))

(defn- header-color
  [{:keys [color color-mode]}]
  (when color
    (if (= :prominent (->kw color-mode))
      {:background color :color "#fff"}
      {:background (str "color-mix(in srgb, " color " 18%, transparent)")})))

(defn- head-row
  [{:keys [cfg widths on-pick]} i cells]
  (into [:tr {:key i}]
        (map-indexed
         (fn [j {:keys [label kind span rowspan sel width]}]
           (let [k [i j]
                 w (or (get @widths k) width)]
             [:th {:key      j
                   :class    (cond-> "capp-table__th capp-pivot__th"
                               (not= :group kind) (str " capp-cell--right")
                               sel                (str " capp-pivot__pick"))
                   :col-span (or span 1)
                   :row-span (or rowspan 1)
                   :style    (cond-> (header-color cfg) w (assoc :width w :min-width w))
                   :on-click (when sel #(on-pick sel (.-ctrlKey %) (.-metaKey %)))}
              label
              [col-grip widths k]])))
        cells))

(defn- head-cell
  [{:keys [on-pick selected on-toggle row-color]} {:keys [expandable? expanded? path depth]} i
   {:keys [label sel struct total?]}]
  (let [pick (or struct sel)]
    [:td {:key      (str "h" i)
          :class    (cond-> "capp-table__td capp-pivot__head"
                      total?                     (str " capp-pivot__tot")
                      (some #(= pick %) selected) (str " capp-pivot__on")
                      pick                        (str " capp-pivot__pick"))
          :style    (cond-> row-color
                      (and depth (pos? depth)) (assoc :padding-left (+ 8 (* 16 depth))))
          :on-click (when pick #(on-pick pick (.-ctrlKey %) (.-metaKey %)))}
     (when expandable?
       [ui/button
        {:minimal  true :small true :icon (if expanded? :chevron-down :chevron-right)
         :on-click (fn [e] (.stopPropagation e) (on-toggle path))}])
     label]))

(defn- body-row
  [{:keys [on-pick selected cell-fn] :as o} {:keys [key heads cells measure total? top?] :as row}]
  (into [:tr {:key key :class (cond-> nil
                                top?   (str " capp-pivot__top")
                                total? (str " capp-pivot__tot"))}]
        (concat
         (map-indexed #(head-cell o row %1 %2) heads)
         (when measure [[:td.capp-table__td.capp-pivot__measure {:key "m"} measure]])
         (map-indexed
          (fn [i {:keys [sel struct total?] :as c}]
            (let [pick (or struct sel)]
              [:td {:key      i
                    :class    (cond-> "capp-table__td capp-cell--right"
                                total?                      (str " capp-pivot__tot")
                                (some #(= pick %) selected) (str " capp-pivot__on")
                                pick                        (str " capp-pivot__pick"))
                    :on-click (when pick #(on-pick pick (.-ctrlKey %) (.-metaKey %)))}
               (shown c cell-fn)]))
          cells))))

(defn pivot-table
  [_opts]
  (let [picks    (r/atom [])
        widths   (r/atom {})
        expanded (r/atom #{})]
    (fn [{:keys [title selected selected-rows on-select cell color color-mode table-style]
          :as cfg}]
      (let [{:keys [head body foot]} (pivot/table (assoc cfg :expanded @expanded))
            on-pick  (fn [pick ctrl? meta?]
                       (let [next (swap! picks pivot/toggle-selection pick (or ctrl? meta?))]
                         (wr! selected next)
                         (when selected-rows
                           (wr! selected-rows (pivot/selected-rows cfg next)))
                         (when on-select (on-select {:values pick :selection next}))))
            basic?   (not (#{:outlined :banded} (->kw table-style)))
            row-tint (when (and basic? color)
                       {:background (str "color-mix(in srgb, " color " 8%, transparent)")})
            o        {:cfg cfg :widths widths :on-pick on-pick :selected @picks
                      :row-color row-tint
                      :cell-fn (cond (fn? cell) cell
                                     (keyword? cell) #((tbl/fmt-by cell) % (rctx/current)))
                      :on-toggle (fn [path]
                                   (let [p (subvec path 0 (count (take-while some? path)))]
                                     (swap! expanded
                                            #(if (contains? % p) (disj % p) (conj % p)))))}]
        [:div.capp-table
         (when title [:div.capp-pivot__title title])
         [:table {:class (pivot/style-class cfg)
                  :data-color-mode (some-> color-mode ->kw name)}
          (into [:thead] (map-indexed #(head-row o %1 %2)) head)
          (into [:tbody] (map #(body-row o %)) body)
          (when (seq foot)
            (into [:tfoot.capp-table__foot] (map #(body-row o %)) foot))]]))))

(defn data-table
  [{:keys [cols rows row-key on-row-click empty class scroll? head-class row-class expand]}]
  (let [head (fn [i {:keys [key label align width head-attrs]}]
               [:th (merge {:key   (or key i)
                            :class (align-class align)
                            :style (when width {:width width})}
                           head-attrs)
                label])
        td   (fn [row i {:keys [key render align mono? cell-class]}]
               [:td {:key   (or key i)
                     :class [(align-class align) (when mono? "bp-monospace-text")
                             (if (fn? cell-class) (cell-class row) cell-class)]}
                (when render (render row))])
        tbl  [ui/html-table {:compact true :interactive (some? on-row-click)
                             :class ["sutra-table" class]}
              [:thead (into [:tr {:class head-class}] (map-indexed head cols))]
              (into [:tbody]
                    (map-indexed
                     (fn [i row]
                       (let [k    (if row-key (str (row-key row)) (str i))
                             line (into [:tr {:key      k
                                              :class    (when row-class (row-class row))
                                              :on-click (when on-row-click
                                                          #(on-row-click row))}]
                                        (map-indexed #(td row %1 %2) cols))]
                         (if-let [more (when expand (expand row))]
                           ^{:key k} [:<> line
                             [:tr [:td {:col-span (count cols)} more]]]
                           line))))
                    rows)]]
    (cond
      (and (empty? rows) (some? empty)) empty
      scroll? [:div {:class "ui-table-scroll"} tbl]
      :else   tbl)))
