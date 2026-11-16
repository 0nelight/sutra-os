(ns sutra-ui.apphost.widgets.spreadsheet
  (:require [sutra.i18n.core :as i18n]
            [sutra-ui.ui.sel :as sel]
            [sutra-ui.ui.busy :as busy]
            [reagent.core :as r]
            [sutra.capps.spreadsheet :as sheet]
            [sutra.capps.variables :refer [deref*]]
            [sutra-ui.ui.core :as ui]
            [sutra-ui.ui.sheet :as ui.sheet]
            [sutra-ui.apphost.interactions :as interactions]
            [sutra-ui.api :as api]))

(defn- end-of-directory
  [^js view]
  (loop [i (- (.-byteLength view) 22)]
    (cond (neg? i) nil
          (= 0x06054b50 (.getUint32 view i true)) i
          :else (recur (dec i)))))

(defn- zip-entries
  [^js buf]
  (let [view (js/DataView. buf)
        eod  (end-of-directory view)
        dec8 (js/TextDecoder.)]
    (when eod
      (loop [p (.getUint32 view (+ eod 16) true)
             n (.getUint16 view (+ eod 10) true)
             acc {}]
        (if (zero? n)
          acc
          (let [nlen (.getUint16 view (+ p 28) true)
                xlen (.getUint16 view (+ p 30) true)
                clen (.getUint16 view (+ p 32) true)
                nm   (.decode dec8 (js/Uint8Array. buf (+ p 46) nlen))]
            (recur (+ p 46 nlen xlen clen)
                   (dec n)
                   (assoc acc nm {:method (.getUint16 view (+ p 10) true)
                                  :offset (.getUint32 view (+ p 42) true)
                                  :size   (.getUint32 view (+ p 20) true)}))))))))

(defn- read-entry!
  [^js buf {:keys [method offset size]}]
  (let [view  (js/DataView. buf)
        start (+ offset 30 (.getUint16 view (+ offset 26) true)
                 (.getUint16 view (+ offset 28) true))
        raw   (js/Uint8Array. buf start size)]
    (if (zero? method)
      (js/Promise.resolve (.decode (js/TextDecoder.) raw))
      (-> (js/Response. (.pipeThrough (.stream (js/Blob. #js [raw]))
                                      (js/DecompressionStream. "deflate-raw")))
          .text))))

(defn- xml [s] (.parseFromString (js/DOMParser.) s "application/xml"))

(defn- tags
  [^js node tag]
  (array-seq (.getElementsByTagName node tag)))

(defn- shared-strings
  [text]
  (when text
    (mapv (fn [^js si] (apply str (map #(.-textContent %) (tags si "t"))))
          (tags (xml text) "si"))))

(defn- sheet-cells
  [text strings]
  (mapv (fn [^js c]
          (let [t  (.getAttribute c "t")
                v  (some-> (first (tags c "v")) .-textContent)
                is (first (tags c "is"))]
            {:ref   (.getAttribute c "r")
             :value (cond
                      (= "s" t)         (get strings (js/parseInt v))
                      (= "inlineStr" t) (some-> is .-textContent)
                      :else             v)}))
        (tags (xml text) "c")))

(defn- workbook-sheets
  [workbook rels]
  (let [targets (into {}
                      (map (fn [^js r] [(.getAttribute r "Id") (.getAttribute r "Target")]))
                      (tags (xml rels) "Relationship"))]
    (mapv (fn [^js s]
            [(.getAttribute s "name")
             (get targets (.getAttributeNS
                           s "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
                           "id"))])
          (tags (xml workbook) "sheet"))))

(defn- load-workbook!
  [src]
  (-> (api/fetch-url src #js {:credentials "same-origin"})
      (.then #(.arrayBuffer %))
      (.then
       (fn [^js buf]
         (let [entries (zip-entries buf)
               text!   (fn [nm] (if-let [e (get entries nm)]
                                  (read-entry! buf e)
                                  (js/Promise.resolve nil)))]
           (if-not (get entries "xl/workbook.xml")
             (js/Promise.reject (js/Error. "Nur .xlsx-Dateien lassen sich anzeigen."))
             (-> (js/Promise.all #js [(text! "xl/workbook.xml")
                                      (text! "xl/_rels/workbook.xml.rels")
                                      (text! "xl/sharedStrings.xml")])
                 (.then
                  (fn [[wb rels strs]]
                    (let [strings (shared-strings strs)
                          sheets  (workbook-sheets wb rels)]
                      (-> (js/Promise.all
                           (into-array
                            (map (fn [[_ target]]
                                   (text! (if (.startsWith target "/")
                                            (subs target 1)
                                            (str "xl/" target))))
                                 sheets)))
                          (.then
                           (fn [texts]
                             {:names (mapv first sheets)
                              :grids (zipmap (map first sheets)
                                             (map #(sheet/grid (sheet-cells % strings))
                                                  (array-seq texts)))})))))))))))))

(defn- copy!
  [cells sel ^js e]
  (when (:a sel)
    (.setData (.-clipboardData e) "text/plain"
              (sheet/range-tsv (sheet/selection-range sel) #(get cells % "")))
    (.preventDefault e)))

(defn- grid-view
  [{:keys [grid annos current selected on-down on-enter on-up]}]
  (let [{:keys [cells rows cols]} grid]
    [ui.sheet/grid
     {:rows rows :cols cols :sel selected :on-mouse-up on-up
      :on-copy #(copy! cells selected %)
      :cell (fn [r c in?]
              (let [at [r c]
                    a  (sheet/annotation-at annos current at)]
                [:td {:id    (str "capp-sheet-" r "-" c)
                      :class (cond-> ""
                               a   (str " capp-sheet__anno")
                               in? (str " is-in"))
                      :on-mouse-down (fn [^js e]
                                       (.preventDefault e)
                                       (some-> (.closest (.-target e) ".ui-sheet") .focus)
                                       (on-down at a (.-shiftKey e)))
                      :on-mouse-enter #(on-enter at)}
                 (get cells at)]))}]))

(defn- reveal!
  [^js root {:keys [start-row start-col]}]
  (some-> root
          (.querySelector (str "#capp-sheet-" start-row "-" start-col))
          (.scrollIntoView #js {:block "center" :inline "center"})))

(defn spreadsheet
  [_opts]
  (let [book     (r/atom nil)
        error    (r/atom nil)
        current  (r/atom nil)
        drag     (r/atom nil)
        anno     (r/atom nil)
        root     (atom nil)
        for*     (atom nil)
        seek-for (atom nil)
        load!    (fn [src]
                   (reset! book nil)
                   (reset! error nil)
                   (-> (load-workbook! src)
                       (.then (fn [b] (reset! book b) (reset! current (first (:names b)))))
                       (.catch #(reset! error (.-message %)))))]
    (fn [{:keys [src layers selected actions env class height]}]
      (when (and src (not= src @for*))
        (reset! for* src)
        (load! src))
      (let [annos (sheet/annotations layers)
            sel-v (deref* selected)
            sel-1 (if (map? sel-v) sel-v (first sel-v))
            hit   (some #(when (= (:id (:row %)) (:id sel-1)) %) annos)
            {:keys [names grids]} @book]
        (when (and hit @book (not= (:id sel-1) @seek-for))
          (reset! seek-for (:id sel-1))
          (when-let [{to :sheet r :range} (sheet/seek-target hit names grids)]
            (reset! current to)
            (when r
              (reset! drag {:a [(:start-row r) (:start-col r)]
                            :b [(:end-row r) (:end-col r)]})
              (js/setTimeout #(reveal! @root r) 0))))
        (let [range-now (when (:a @drag) (sheet/selection-range @drag))]
          [:div {:class (cond-> "capp-sheet" class (str " " class))
                 :ref   #(reset! root %)
                 :style (when height {:max-height height})}
           (cond
             @error       [ui/problem (str @error) nil]
             (nil? src)   [:div.capp__hint "Keine Datei."]
             (nil? @book) [busy/block {:class "capp-objectview__loading"} (i18n/t :ui/capp-loading)]
             :else
             [:<>
              [grid-view {:grid     (get grids @current)
                          :annos    annos
                          :current  @current
                          :selected @drag
                          :on-down  (fn [at a extend?]
                                      (reset! anno a)
                                      (when (and a selected) (sel/wr! selected [(:row a)]))
                                      (swap! drag #(assoc (sheet/select % at extend?) :held? true)))
                          :on-enter (fn [at]
                                      (when (:held? @drag) (swap! drag sheet/select at true)))
                          :on-up    #(swap! drag dissoc :held?)}]
              [:div.capp-sheet__foot
               (into [ui/tabs {:id :capp-sheet :class "capp-sheet__tabs" :animate false
                               :selected-tab-id @current
                               :on-change (fn [nm] (reset! current nm) (reset! drag nil))}]
                     (map (fn [nm]
                            [ui/tab {:id    nm
                                     :title nm
                                     :class (cond-> "capp-sheet__tab"
                                              (= nm @current) (str " capp-sheet__tab--on"))}]))
                     names)
               (when (and (seq actions) range-now)
                 [interactions/bar {:env env :items actions :default-icon "✎"
                                    :row (sheet/selection-row @current range-now @anno)}])]])])))))
