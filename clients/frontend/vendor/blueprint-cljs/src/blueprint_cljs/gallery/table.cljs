(ns blueprint-cljs.gallery.table
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [blueprint-cljs.menu :as menu]
            [blueprint-cljs.table :as t]
            [blueprint-cljs.table.regions :as regions]))

(defn- frame
  [h & children]
  (into [:div {:style {:height h :width "100%" :max-width 900}}] children))

(defn- note [s] [:div {:style {:font-size 12 :opacity 0.7}} s])

(def ^:private cities
  ["Vienna" "Graz" "Linz" "Salzburg" "Innsbruck" "Klagenfurt" "Villach" "Wels" "St. Pölten"
   "Dornbirn" "Steyr" "Feldkirch" "Bregenz" "Leonding" "Klosterneuburg" "Baden" "Wolfsberg"
   "Leoben" "Krems" "Traun"])

(def ^:private people
  (vec (for [i (range 20)]
         {:name    (str (nth ["Anna" "Ben" "Clara" "David" "Eva" "Felix" "Greta"] (mod i 7))
                        " " (nth ["Huber" "Gruber" "Wagner" "Bauer" "Müller"] (mod i 5)))
          :city    (nth cities i)
          :age     (+ 21 (mod (* i 7) 43))
          :balance (- (* (mod (* i 37) 101) 123.45) 2000)
          :active  (even? i)})))

(defn- col-menu [i]
  [menu/menu
   [menu/menu-item {:icon :sort-asc :text (str "Sort column " i " ascending")}]
   [menu/menu-item {:icon :sort-desc :text "Sort descending"}]
   [menu/menu-divider]
   [menu/menu-item {:icon :eye-off :text "Hide column"}]])

(defn- body-menu [{:keys [regions target-cell]}]
  [menu/menu
   [menu/menu-item {:icon :duplicate :text (str "Copy " (count regions) " region(s)")}]
   [menu/menu-item {:icon :search :text (str "Inspect cell " (str/join "," target-cell))}]])

(defn- basic-demo []
  (let [sel (r/atom [])]
    (fn []
      [:<>
       (note (str "Click, drag, shift-click and mod-click to select; right-click a cell; the "
                  "chevron in a header opens its menu; drag a header edge to resize; arrow "
                  "keys move the focused cell. Selection: "
                  (pr-str @sel)))
       [frame 300
        [t/table {:num-rows 20 :enable-column-resizing true :enable-focused-cell true
                  :column-widths [160 110 70 110 80]
                  :on-selection #(reset! sel %)
                  :body-context-menu-renderer body-menu}
         [t/column {:name "Name"
                    :column-header-cell-renderer
                    (fn [i] [t/column-header-cell {:name "Name" :menu-renderer col-menu
                                                   :index i}])
                    :cell-renderer (fn [r _] [t/cell {} (:name (people r))])}]
         [t/column {:name "City" :cell-renderer (fn [r _] [t/cell {} (:city (people r))])}]
         [t/column {:name "Age"
                    :cell-renderer (fn [r _] [t/cell {:style {:text-align "right"}}
                                              (:age (people r))])}]
         [t/column {:name "Balance"
                    :cell-renderer
                    (fn [r _]
                      (let [b (:balance (people r))]
                        [t/cell {:intent (if (neg? b) :danger :none)
                                 :style {:text-align "right"}}
                         (.toFixed b 2)]))}]
         [t/column {:name "Active"
                    :cell-renderer (fn [r _] [t/cell {:intent (when (:active (people r))
                                                                :success)}
                                              (if (:active (people r)) "yes" "no")])}]]]])))

(defn- big-cell [r c]
  [t/cell {} (str (t/column-name c) (inc r))])

(defn- big-demo []
  (let [vis (r/atom nil)]
    (fn []
      [:<>
       (note (str "100 000 rows × 50 columns, virtualised. Visible: " @vis))
       [frame 420
        (into [t/table {:num-rows 100000 :enable-column-resizing true
                        :enable-row-resizing true :enable-focused-cell true
                        :on-visible-cells-change
                        (fn [rows cols]
                          (reset! vis (str "rows " (:row-index-start rows) "–"
                                           (:row-index-end rows) ", columns "
                                           (:column-index-start cols) "–"
                                           (:column-index-end cols))))}]
              (for [_ (range 50)]
                [t/column {:cell-renderer big-cell}]))]])))

(defn- frozen-demo []
  [:<>
   (note "2 frozen rows and 1 frozen column; scroll in both directions.")
   [frame 340
    (into [t/table {:num-rows 1000 :num-frozen-rows 2 :num-frozen-columns 1
                    :enable-focused-cell true :enable-column-resizing true}]
          (for [c (range 20)]
            [t/column {:name (if (zero? c) "Key" (str "Metric " c))
                       :cell-renderer
                       (fn [r c]
                         (if (zero? c)
                           [t/cell {} (str "row-" r)]
                           [t/cell {:style {:text-align "right"}}
                            (mod (* (inc r) (+ 13 c) 7919) 10000)]))}]))]])

(defn- editable-demo []
  (let [data  (r/atom (vec (for [r (range 12)] (vec (for [c (range 4)] (str "v" r "-" c))))))
        names (r/atom ["First" "Second" "Third" "Fourth"])]
    (fn []
      [:<>
       (note (str "Double-click a cell (or focus it and press F2 / start typing) to edit; "
                  "Enter confirms, Escape cancels. Double-click a column name to rename it."))
       [frame 300
        (into [t/table {:num-rows 12 :enable-focused-cell true}]
              (for [_ (range 4)]
                [t/column
                 {:column-header-cell-renderer
                  (fn [i]
                    [t/column-header-cell
                     {:name (@names i)
                      :name-renderer
                      (fn [nm i]
                        [t/editable-name {:name nm :index i
                                          :on-confirm (fn [v i] (swap! names assoc i v))}])}])
                  :cell-renderer
                  (fn [r c]
                    (let [v (get-in @data [r c])]
                      [t/editable-cell
                       {:value      v
                        :intent     (when (str/blank? v) :danger)
                        :on-confirm (fn [v r c] (swap! data assoc-in [r c] v))}]))}]))]])))

(defn- formats-demo []
  (let [long-text (str/join " " (repeat 12 "The quick brown fox jumps over the lazy dog."))
        values    [{:id 1 :tags ["a" "b"]} nil "plain string" 42 [1 2 3]
                   {:nested {:deep {:deeper true}}}]]
    [:<>
     (note (str "TruncatedFormat with detect-truncation (the … button opens the full text) "
                "and JSONFormat (nil shows as null)."))
     [frame 260
      [t/table {:num-rows 6 :column-widths [260 260]
                :row-heights (vec (repeat 6 40))}
       [t/column {:name "TruncatedFormat"
                  :cell-renderer
                  (fn [r _] [t/cell {:wrap-text true}
                             [t/truncated-format {:detect-truncation true}
                              (if (even? r) long-text (str "short " r))]])}]
       [t/column {:name "JSONFormat"
                  :cell-renderer
                  (fn [r _] [t/cell {} [t/json-format {:detect-truncation true}
                                        (values r)]])}]]]]))

(defn- reorder-demo []
  (let [order (r/atom (vec (range 6)))]
    (fn []
      [:<>
       (note (str "Select a column by its header, then drag the selected header to move it. "
                  "Order: " (pr-str @order)))
       [frame 260
        (into [t/table {:num-rows 10 :enable-column-reordering true
                        :enable-row-reordering true
                        :selection-modes :all
                        :on-columns-reordered
                        (fn [from to len] (swap! order regions/reorder from to len))}]
              (for [c @order]
                [t/column {:name (str "Column " (t/column-name c))
                           :cell-renderer (fn [r _] [t/cell {} (str (t/column-name c) r)])}]))]])))

(defn- loading-demo []
  [:<>
   (note "Loading options: cells and both headers show skeletons.")
   [frame 200
    (into [t/table {:num-rows 8 :loading-options #{:cells :column-headers :row-headers}}]
          (for [c (range 5)] [t/column {:name (str "Col " c)}]))]])

(defn- regions-demo []
  [:<>
   (note "Controlled selection, styled region groups and selection modes :rows-and-cells.")
   [frame 240
    (into [t/table {:num-rows 12 :selection-modes :rows-and-cells
                    :selected-regions [(regions/cells 1 1 3 2) (regions/row 6 6)]
                    :styled-region-groups [{:class-name "bp-intent-warning"
                                            :regions [(regions/cells 8 0 9 3)]}]}]
          (for [_ (range 6)]
            [t/column {:cell-renderer (fn [r c] [t/cell {} (* (inc r) (inc c))])}]))]])

(def sections
  "Each: {:id kw :title str :demo (fn [] hiccup)}."
  [{:id :table-basic :title "Table" :demo basic-demo}
   {:id :table-big :title "Table: 100 000 rows" :demo big-demo}
   {:id :table-frozen :title "Table: frozen" :demo frozen-demo}
   {:id :table-editable :title "Table: editable" :demo editable-demo}
   {:id :table-formats :title "Table: formats" :demo formats-demo}
   {:id :table-reorder :title "Table: reordering" :demo reorder-demo}
   {:id :table-regions :title "Table: regions" :demo regions-demo}
   {:id :table-loading :title "Table: loading" :demo loading-demo}])
