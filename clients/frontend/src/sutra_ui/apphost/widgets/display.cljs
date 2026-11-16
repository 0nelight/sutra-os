(ns sutra-ui.apphost.widgets.display
  (:require [sutra-ui.ui.display :as display]
            [sutra-ui.ui.busy :as busy]
            [sutra-ui.ui.table :as table]
            [sutra-ui.ui.sel :as sel :refer [rd wr!]]
            [sutra-ui.ui.overlay :as overlay]
            [sutra-ui.download :as dl]
            [clojure.string :as str]
            [reagent.core :as r]
            [sutra-ui.ui.core :as ui :refer [checkbox context-menu-popover divider
                                             segmented-control html-select input-group]]
            [sutra.i18n.core :as i18n]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.capps.format :as fmt]
            [sutra.capps.leaf-props :as lp]
            [sutra.capps.object-display :as od]
            [sutra.capps.standard-view :as sv]
            [sutra.capps.table :as tbl]
            [sutra-ui.ui.rule-icon :as rule-icon]
            [sutra.capps.variables :as vars]
            [sutra-ui.render-ctx :as rctx]
            [sutra-ui.object.card :as oc]
            [sutra-ui.apphost.frame :as fr]
            [sutra.kit.paging :as pg]))

(defn- as-rows
  [x]
  (lp/as-rows (rd x)))

(def ^:private crc-table
  (delay
    (into-array
     (for [n (range 256)]
       (reduce (fn [c _] (if (odd? c)
                           (bit-xor 0xEDB88320 (unsigned-bit-shift-right c 1))
                           (unsigned-bit-shift-right c 1)))
               n (range 8))))))

(defn- crc32
  [^js bytes]
  (let [t @crc-table]
    (unsigned-bit-shift-right
     (bit-not
      (areduce bytes i c -1
               (bit-xor (aget t (bit-and (bit-xor c (aget bytes i)) 0xFF))
                        (unsigned-bit-shift-right c 8))))
     0)))

(defn- le
  [n size]
  (mapv #(bit-and (unsigned-bit-shift-right n (* 8 %)) 0xFF) (range size)))

(defn- stored-zip
  [parts]
  (let [enc     (js/TextEncoder.)
        entries (mapv (fn [[nm text]]
                        (let [nb (.encode enc nm) b (.encode enc text)]
                          {:name nb :data b :crc (crc32 b)}))
                      parts)
        common  (fn [{:keys [name data crc]}]
                  (concat [20 0 0 8 0 0 0 0 33 0] (le crc 4) (le (.-length data) 4)
                          (le (.-length data) 4) (le (.-length name) 2) [0 0]))
        locals  (mapv (fn [e] (concat [0x50 0x4B 3 4] (common e))) entries)
        offsets (reductions + 0 (map (fn [l e] (+ (count l) (.-length (:name e))
                                                  (.-length (:data e))))
                                     locals entries))
        centrals (mapv (fn [e off]
                         (concat [0x50 0x4B 1 2 20 0] (common e)
                                 [0 0 0 0 0 0 0 0 0 0] (le off 4)))
                       entries offsets)
        chunks  (concat (mapcat (fn [l e] [(clj->js l) (:name e) (:data e)]) locals entries)
                        (mapcat (fn [c e] [(clj->js c) (:name e)]) centrals entries))
        cd-size (reduce + (map (fn [c e] (+ (count c) (.-length (:name e)))) centrals entries))
        end     (concat [0x50 0x4B 5 6 0 0 0 0] (le (count parts) 2) (le (count parts) 2)
                        (le cd-size 4) (le (last offsets) 4) [0 0])]
    (js/Blob. (clj->js (mapv #(js/Uint8Array.from %) (concat chunks [(clj->js end)])))
              #js {:type (str "application/vnd.openxmlformats-officedocument."
                              "spreadsheetml.sheet")})))

(defn export!
  [kind sheet filename]
  (case kind
    :csv  (dl/blob! (js/Blob. #js [(od/csv-text sheet)] #js {:type "text/csv;charset=utf-8"})
                     (str filename ".csv"))
    :xlsx (dl/blob! (stored-zip (od/xlsx-parts sheet)) (str filename ".xlsx"))))

(defn- series-key
  [{:keys [series]} t]
  [t (select-keys series [:property :from :until :bucket :rollup])])

(defn- ask-series!
  [store {:keys [series] :as col} t {:keys [series!]}]
  (let [k (series-key col t)]
    (when (and t series! (not (contains? @store k)))
      (swap! store assoc k ::asked)
      (series! (assoc (select-keys series [:property :from :until :bucket :rollup])
                      :find (name t))
               {:on-ok    (fn [answer]
                            (swap! store assoc k
                                   (into {}
                                         (for [s (:series answer)
                                               o (:objects s)]
                                           [(str (:id o)) (od/points (:points s))]))))
                :on-error (fn [m] (swap! store assoc k {::error m}))}))))

(defn- series-points
  [store col row]
  (let [held (get @store (series-key col (:object-type-ref row)))]
    (when (map? held)
      (od/apply-transforms (get held (str (:id row)) [])
                           (:transforms (:series col))
                           (js/Date.now)))))

(defn- sparkline
  [pts {:keys [range baseline look]}]
  (let [pts  (if range (od/in-range pts range (js/Date.now)) pts)
        last (some-> (peek pts) second)
        tone (when last (tbl/cond-format look {:value last}))]
    [display/sparkline {:points pts :baseline baseline :color (:color tone)}]))

(defn- series-col
  [store {:keys [series fmt] :as col}]
  (let [value (fn [row]
                (some-> (series-points store col row)
                        (od/summarize (:summarizer series) (js/Date.now))))]
    (assoc col
           :value-fn value
           :render (fn [row]
                     (let [x (value row)]
                       [:div.capp-cell--series
                        [:span (when (some? x) ((tbl/fmt-by fmt) x (rctx/current)))]
                        (when (:sparkline series)
                          [sparkline (series-points store col row) (:sparkline series)])])))))

(defn- expand-col
  [store {:keys [url? fmt url-label] :as c}]
  (let [reader (od/col-reader c)
        c      (cond-> c (or (:id c) (:prop c)) (assoc :id (od/col-id c)))]
    (cond
      (:series c)
      (series-col store c)

      url?
      (assoc c :render
             (fn [row]
               (let [raw (reader row)]
                 (if-let [href (od/safe-url raw)]
                   [:a {:href href :target "_blank" :rel "noopener noreferrer"
                        :on-click #(.stopPropagation %)}
                    (or url-label (str raw))]
                   (some-> raw str)))))

      (or (contains? c :from) (:expr c))
      (assoc c :value-fn reader
             :render (fn [row] ((tbl/fmt-by fmt) (reader row) (rctx/current))))

      :else c)))

(defn- look-style
  [look whole-cell?]
  (when look
    (cond-> {}
      (:color look)                         (assoc :color (:color look))
      (and whole-cell? (:background look))  (assoc :background (:background look)))))

(defn- with-looks
  [cols display whole-cell?]
  (mapv (fn [{:keys [prop conditional-format] :as c}]
          (let [entry (when prop (or (get display (->kw prop)) (get display (name prop))))]
            (if (or (seq conditional-format) (seq (:conditional-format entry)))
              (assoc c
                     :cell-style-of
                     (fn [row] (look-style (od/cell-look c entry row) whole-cell?))
                     :look-of
                     (fn [row] (od/cell-look c entry row)))
              c)))
        cols))

(defn- with-icons
  [cols]
  (mapv (fn [{f :render look-of :look-of :as c}]
          (if (and f look-of)
            (assoc c :render
                   (fn [row]
                     (let [x                  (f row)
                           {:keys [icon title]} (look-of row)
                           tip                (or title (when (string? x) x))]
                       (if icon
                         [:span {:title tip :aria-label tip} (rule-icon/icon icon)]
                         x))))
            c))
        cols))

(defn- with-no-value
  [cols text]
  (if (str/blank? (str text))
    cols
    (mapv (fn [{f :render :as c}]
            (if f
              (assoc c :render
                     (fn [row] (let [x (f row)] (if (or (nil? x) (= "" x)) text x))))
              c))
          cols)))

(defn- head-cell
  [sort-state labels on-sort]
  (fn [c]
    (let [id    (od/col-id c)
          label (or (not-empty (:label c)) (get labels id))]
      (if (and id on-sort)
        [ui/button {:minimal true :small true :on-click #(on-sort id)
                    :end-icon (when (= id (:id sort-state))
                                (if (= :desc (:dir sort-state)) :chevron-down :chevron-up))}
         label]
        label))))

(defn- row-menu
  [{:keys [x y row]} items on-close]
  [context-menu-popover
   {:is-open           true
    :target-offset     {:left x :top y}
    :on-close          on-close
    :portal-class-name overlay/portal-class
    :content           (into [ui/menu {:class "capp-menu"}]
                             (map-indexed (fn [i {:keys [label run]}]
                                            ^{:key i}
                                            [ui/menu-item {:class    "capp-menu__item"
                                                           :text     label
                                                           :on-click #(run row)}]))
                             items)}])

(defn- column-menu
  [all-ids labels visible on-toggle]
  (let [shown (if (seq visible) (set (map str visible)) (set all-ids))]
    [ui/section {:title          (i18n/t :ui/capp-table-columns)
                 :collapsible    true
                 :compact        true
                 :collapse-props {:default-is-open false}
                 :class          "capp-table__config"}
     (into [ui/section-card]
           (map (fn [id]
                  [checkbox {:class     "capp-table__config-item"
                             :checked   (contains? shown id)
                             :label     (or (get labels id) id)
                             :on-change #(on-toggle id)}]))
           all-ids)]))

(defn- type-tabs
  [groups active on-pick env]
  (into [ui/tabs {:id :capp-type-tabs :animate false
                  :selected-tab-id active :on-change (fn [type] (on-pick type))}]
        (map (fn [{:keys [type rows]}]
               [ui/tab {:id    type
                        :title (oc/type-label ((:meta-of env) type))
                        :tag-content (count rows)}]))
        groups))

(defn- icon-col
  [prop env]
  {:label "" :cell-class "capp-table__td--icon"
   :render (fn [row]
             (when-let [src ((:media-src env) (get (or (:values row) row) (->kw prop)))]
               [:img.capp-table__icon {:src src :alt ""}]))})

(defn- inline-frame
  [_cfg _env]
  (let [editing (r/atom false)]
    (fn [{:keys [edit-mode edit-label confirm-submit?] :as cfg} {:keys [action-table] :as env}]
      (if (or @editing (= :always (->kw edit-mode)))
        [action-table
         (cond-> (assoc (dissoc cfg :edit-mode :edit-label :confirm-submit?)
                        :on-ok-local #(reset! editing false))
           confirm-submit?
           (assoc :before-fire
                  (fn [fire!]
                    (-> (overlay/confirm! {:title (i18n/t :ui/capp-save-changes)})
                        (.then #(when % (fire!)))))))
         env]
        [:div.capp-action-table
         [table/object-table
          ((:rows-shown env) (-> cfg
                                 (update :cols (fn [cs] (mapv #(dissoc % :edit :param) cs)))
                                 (update :on-row-click (:event-fn env)))
                             env)]
         [:div.capp-action-table__bar
          [ui/button {:intent :primary :on-click #(reset! editing true)}
           (or edit-label (i18n/t :ui/capp-table-edit))]]]))))

(defn- more-of
  [rows env]
  (let [nm    (first (vars/cut-of rows))
        load! (get-in env [:app :more])]
    (when (and nm load!)
      (pg/sentinel {:rows rows :offset 0 :total (vars/total-of rows)} #(load! nm %)))))

(defn- table-body
  [{:keys [title row-key narrow-headers? user-sort? no-value whole-cell? visible-cols
           column-config? hide-column-config? context-menu right-clicked export-csv?
           export-excel? custom-export-names? icon-prop export-name]
    :as cfg}
   {:keys [sort-state menu local-visible pages]} store env]
  (let [rows    (as-rows (:rows cfg))
        limit   (when (and (number? (:limit cfg)) (pos? (:limit cfg))) (:limit cfg))
        page-n  (let [{:keys [of n]} @pages] (if (identical? of (:rows cfg)) n limit))
        local-more (when (and limit (> (count rows) page-n))
                     {:loaded page-n :total (count rows)
                      :load!  #(reset! pages {:of (:rows cfg) :n (+ page-n limit)})})
        display ((:display-of env) rows)
        expanded (mapv #(expand-col store %) (as-rows (:cols cfg)))
        held    (when visible-cols (rd visible-cols))
        visible (if visible-cols held @local-visible)
        write!  (if (and visible-cols (satisfies? IReset visible-cols))
                  #(reset! visible-cols %)
                  #(reset! local-visible %))
        by-id   (into {} (keep (fn [c] (when-let [i (od/col-id c)] [i c]))) expanded)
        all-ids (into [] (keep od/col-id) expanded)
        labels  (into {} (map (fn [[i c]]
                                [i (or (not-empty (:label c))
                                       (:label (tbl/prop-display (get display (->kw (:prop c)))
                                                                 (:prop c)))
                                       (tbl/humanize (or (:prop c) i)))]))
                      by-id)
        shown   (od/visible-cols expanded visible)
        {:keys [id dir]} (or @sort-state
                             (when-let [{:keys [id dir]} (:sort-column cfg)]
                               {:id (od/col-id {:id id}) :dir (->kw dir)}))
        rows    (if-let [c (get by-id id)]
                  (od/sorted-by rows (od/col-reader c) dir)
                  rows)
        _       (run! (fn [c] (when (:series c)
                                (ask-series! store c (:object-type-ref (first rows)) env)))
                      expanded)
        cfg*    (-> (dissoc cfg :title :visible-cols :column-config? :hide-column-config?
                            :no-value :whole-cell? :context-menu :right-clicked :export-csv?
                            :export-excel? :custom-export-names? :user-sort? :narrow-headers?
                            :icon-prop :export-name :separate-types? :sort-column)
                    (assoc :rows rows :cols (with-looks shown display whole-cell?))
                    (cond-> limit (assoc :limit page-n))
                    (update :on-row-click (:event-fn env)))
        shown*  ((:rows-shown env) (cond-> cfg* id (dissoc :sort)) env)
        cols    (cond-> (with-icons (with-no-value (:cols shown*) no-value))
                  icon-prop (->> (into [(icon-col icon-prop env)])))
        name*   (or export-name "export")
        sheet   (fn [] (od/export-sheet rows (tbl/with-prop-display shown display)
                                        custom-export-names?))
        items   (concat
                 (when export-csv?
                   [{:label (i18n/t :ui/capp-table-export-csv)
                     :run (fn [_] (export! :csv (sheet) name*))}])
                 (when export-excel?
                   [{:label (i18n/t :ui/capp-table-export-excel)
                     :run (fn [_] (export! :xlsx (sheet) name*))}])
                 (map (fn [{:keys [label on-click]}]
                        {:label label :run ((:event-fn env) on-click)})
                      (as-rows context-menu)))
        on-ctx  (when (seq items)
                  (fn [row e]
                    (.preventDefault e)
                    (when right-clicked (wr! right-clicked [row]))
                    (reset! menu {:x (.-clientX e) :y (.-clientY e) :row row})))]
    [:div.capp-table-frame
     (when (or title (and (not hide-column-config?) (or column-config? visible-cols)))
       [:div.capp-table__bar
        (when title [:div.capp-table__title title])
        (when (and (not hide-column-config?) (or column-config? visible-cols))
          [column-menu all-ids labels visible
           #(write! (od/toggle-visible all-ids visible %))])])
     [table/object-table
      (assoc shown* :cols cols
             :more (or local-more (more-of (:rows cfg) env))
             :narrow? narrow-headers?
             :preview-rows (:preview-rows (:descriptor (:app env)))
             :on-row-context on-ctx
             :row-key row-key
             :head-cell (head-cell @sort-state labels
                                   (when-not (false? user-sort?)
                                     #(swap! sort-state od/next-sort %))))]
     (when-let [m @menu]
       [row-menu m (map (fn [it] (update it :run (fn [run] #(do (reset! menu nil) (run %)))))
                        items)
        #(reset! menu nil)])]))

(defn object-table
  [_cfg _env]
  (let [state {:sort-state (r/atom nil) :menu (r/atom nil) :local-visible (r/atom nil)
               :pages (r/atom nil)}
        store (r/atom {})
        tab   (r/atom nil)]
    (fn [{:keys [separate-types? compare] :as cfg} env]
      (cond
        compare ((:compare-table env) cfg env)

        (and (:action cfg) (some :edit (as-rows (:cols cfg))))
        [inline-frame cfg env]

        :else
        (let [rows   (as-rows (:rows cfg))
              groups (when separate-types? (od/type-groups rows))]
          (if (next groups)
            (let [active (or @tab (:type (first groups)))]
              [:div
               [type-tabs groups active #(reset! tab %) env]
               [table-body (assoc cfg :rows (:rows (first (filter #(= active (:type %)) groups))))
                state store env]])
            [table-body cfg state store env]))))))

(defn- media-cell
  [{:keys [prop display expand? hover-props position size fit]} row env expand!]
  (when-let [src ((:media-src env) (get (or (:values row) row) (->kw prop)))]
    (let [icon? (= :icon (->kw display))
          entries (when (seq hover-props)
                    (sv/property-entries row ((:display-of env) [row])
                                         {:props hover-props} (rctx/current)))]
      [:div {:class (str "capp-olist__media capp-olist__media--"
                         (if icon? "icon" (name (or (->kw position) :top))))
             :style (when (and size (not icon?))
                      (if (#{:left :right} (->kw position))
                        {:width size} {:height size}))}
       [:img {:src src :alt ""
              :style {:object-fit (if (= :fit (->kw fit)) "contain" "cover")}
              :on-click (when expand? (fn [e] (.stopPropagation e) (expand! src)))}]
       (when (seq entries)
         (into [:dl.capp-olist__hover]
               (map (fn [{:keys [label text]}] [:div [:dt label] [:dd text]]))
               entries))])))

(defn- card
  [{:keys [props hide-null? property-style media multi-select? reorder?]
    :as _cfg}
   row {:keys [active? picked? on-pick on-activate expand! drag]} env]
  (let [t       (:object-type-ref row)
        meta*   ((:meta-of env) t)
        entries (sv/property-entries row ((:display-of env) [row])
                                     {:props props :hide-null? hide-null?} (rctx/current))
        icon?   (= :icon (->kw (:display media)))
        media*  (when (:prop media) [media-cell media row env expand!])]
    [:div (merge
           {:class (cond-> "capp-olist__card" active? (str " is-active"))
            :on-click on-activate}
           drag)
     (when-not icon? media*)
     [:div.capp-olist__body
      [:div.capp-olist__head
       (when multi-select?
         [:span {:on-click #(.stopPropagation %)}
          [checkbox {:checked   (boolean picked?)
                     :on-change #(on-pick (.. % -target -checked))}]])
       (when icon? media*)
       [oc/object-link t (:id row) {:row row :meta meta* :open? false :icon? (not icon?)}]
       (when reorder? [:span.capp-olist__handle "⠿"])]
      (when (seq entries)
        (into [:dl {:class (str "capp-olist__props capp-olist__props--"
                                (name (or (->kw property-style) :inline)))}]
              (map (fn [{:keys [prop label text look]}]
                     ^{:key (str prop)}
                     [:div.capp-olist__prop
                      [:dt label]
                      [:dd {:style (select-keys look [:color :background])}
                       (if (str/blank? (str text)) "—" text)]]))
              entries))]]))

(defn- grid-style
  [{:keys [columns card-size gap]}]
  (cond-> {}
    columns   (assoc :grid-template-columns (str "repeat(" columns ", minmax(0, 1fr))"))
    (and card-size (not columns))
    (assoc :grid-template-columns (str "repeat(auto-fill, minmax(" card-size "px, 1fr))"))
    gap       (assoc :gap gap)))

(defn- submit-reorder!
  [{:keys [action param params key-param]} keys* row env]
  (when action
    ((:submit! env)
     action
     (cond-> (assoc (or params {}) (->kw (or param :reordered-array)) keys*)
       (and key-param row) (assoc (->kw key-param) (:id row))))))

(defn object-list
  [_cfg _env]
  (let [sort-state (r/atom nil)
        expanded   (r/atom nil)
        dragging   (atom nil)]
    (fn [{:keys [rows sort sort-props props title show-count? empty display grid active selected
                 auto-active? multi-select? on-select row-key user-sort? reorder]
          :as cfg}
         env]
      (let [all     (as-rows rows)
            keyf    (or (tbl/field-fn row-key) :id)
            enabled (let [e (:enabled reorder)] (and reorder (or (nil? e) (true? (rd e)))))
            grid?   (= :grid (->kw display))
            reorder? (and enabled (not grid?))
            base    (tbl/sort-limit-rows (if reorder? (vec (take 500 all)) all)
                                         (if @sort-state nil sort) nil)
            sorted  (if-let [{:keys [id dir]} @sort-state]
                      (od/sorted-by base (tbl/field-fn id) dir)
                      base)
            key-arr (when reorder? (mapv str (or (rd (:keys reorder)) [])))
            {sorted-part :sorted unsorted :unsorted}
            (when reorder? (od/split-by-order sorted key-arr keyf))
            held    (when active (rd active))
            sync    (when (and active (satisfies? IReset active))
                      (tbl/active-sync held sorted (not (false? auto-active?))))
            _       (when sync (js/queueMicrotask #(when (= held (rd active))
                                                     (reset! active sync))))
            act-key (some-> (or (first sync) (tbl/active-row held)) keyf str)
            picked  (when multi-select? (into #{} (map (comp str keyf)) (as-rows selected)))
            fire    ((:event-fn env) on-select)
            write-keys! (fn [ks row]
                          (when (satisfies? IReset (:keys reorder)) (reset! (:keys reorder) ks))
                          (submit-reorder! reorder ks row env))
            drag    (fn [row section i]
                      (when reorder?
                        {:draggable true
                         :on-drag-start (fn [e]
                                          (reset! dragging row)
                                          (.setData (.-dataTransfer e) "text/plain"
                                                    (str (keyf row))))
                         :on-drag-over #(.preventDefault %)
                         :on-drop
                         (fn [e]
                           (.preventDefault e)
                           (when-let [moved @dragging]
                             (reset! dragging nil)
                             (let [mk (str (keyf moved))]
                               (write-keys!
                                (if (= :unsorted section)
                                  (od/drop-key key-arr mk)
                                  (od/reorder-keys key-arr (mapv (comp str keyf) sorted-part)
                                                   mk i))
                                moved))))}))
            line    (fn [section i row]
                      ^{:key (str (keyf row))}
                      [card (assoc cfg :reorder? reorder? :props props) row
                       {:active?     (= act-key (str (keyf row)))
                        :picked?     (contains? picked (str (keyf row)))
                        :on-pick     (fn [on?]
                                       (when (satisfies? ISwap selected)
                                         (swap! selected
                                                (fn [cur]
                                                  (let [cur (vec (as-rows cur))]
                                                    (if on?
                                                      (conj cur row)
                                                      (filterv #(not= (keyf %) (keyf row))
                                                               cur)))))))
                        :on-activate (fn []
                                       (when (and active (satisfies? IReset active))
                                         (reset! active [row]))
                                       (when fire (fire row)))
                        :expand!     #(reset! expanded %)
                        :drag        (drag row section i)}
                       env])
            section (fn [nm rs]
                      (into [:div {:class (str "capp-olist__cards"
                                               (when grid? " capp-olist__cards--grid"))
                                   :style (when grid? (grid-style grid))}]
                            (map-indexed #(line nm %1 %2))
                            rs))
            options (od/sort-options props sort-props)]
        [:div.capp-olist
         (when (or title show-count? user-sort?)
           [:div.capp-olist__bar
            (when title [:div.capp-olist__title title])
            (when show-count?
              [:span.capp-olist__count (or (vars/total-of (rd rows)) (count all))])
            (when user-sort?
              (into [html-select
                     {:value (or (some-> @sort-state :id name) "")
                      :on-change (fn [e]
                                   (let [v (.. e -target -value)]
                                     (reset! sort-state
                                             (when (seq v) {:id (keyword v) :dir :asc}))))}
                     [:option {:value ""} (i18n/t :ui/capp-list-default-sort)]]
                    (map (fn [p] [:option {:value (name p)}
                                  (:label (tbl/prop-display
                                           (get ((:display-of env) all) p) p))]))
                    options))])
         (cond
           (empty? all)
           [ui/empty-state {:icon (or (:icon empty) "▤")
                             :title (or (:title empty) (when (string? empty) empty)
                                        (i18n/t :ui/capp-no-objects))}]

           reorder?
           [:<>
            (when (empty? sorted-part)
              [:div.capp-olist__callout (i18n/t :ui/capp-list-drag-hint)])
            [section :sorted sorted-part]
            (when (seq unsorted)
              [:<>
               [:div.capp-olist__section (i18n/t :ui/capp-list-unsorted)]
               [section :unsorted unsorted]])]

           :else [section :all sorted])
         (when-let [src @expanded]
           [overlay/window {:on-close #(reset! expanded nil) :backdrop "capp-backdrop--clear"}
            [:div.capp-media-modal {:on-click #(reset! expanded nil)}
             [:img {:src src :alt ""}]]])]))))

(defn- preview
  [row spec env]
  (let [entries (sv/property-entries row ((:display-of env) [row])
                                     (if (seq (:preview spec))
                                       {:props (:preview spec)}
                                       {:only-prominent? true})
                                     (rctx/current))]
    (when (seq entries)
      (into [:dl.capp-links__preview]
            (map (fn [{:keys [prop label text]}]
                   ^{:key (str prop)} [:div [:dt label] [:dd text]]))
            entries))))

(declare object-links)

(defn- peer-line
  [_row _spec _cfg _env]
  (let [open? (r/atom false)]
    (fn [row spec {:keys [open-view? preview?] :as cfg} {:keys [open-object] :as env}]
      (let [t (:object-type-ref row)]
        [:li.capp-links__peer
         [:div.capp-links__peer-line
          [ui/button {:minimal true :small true :on-click #(swap! open? not)
                      :icon    (if @open? :chevron-down :chevron-right)}]
          [:span.capp-links__peer-title
           [oc/object-link t (:id row) {:row row :meta ((:meta-of env) t) :open? false}]
           (when preview? [preview row spec env])]
          (when (and open-view? open-object)
            [ui/button
             {:on-click #(open-object t (:id row))}
             (i18n/t :ui/capp-links-open-view)])]
         (when @open? [object-links row (assoc cfg :links nil :expand 0) env])]))))

(defn- object-links
  [_row _cfg _env]
  (let [answer (r/atom nil)
        err    (r/atom nil)
        opened (r/atom nil)
        asked  (atom nil)]
    (fn [row {:keys [links expand explore? on-explore] :as cfg}
         {:keys [around! type-links] :as env}]
      (let [t     (:object-type-ref row)
            k     [t (:id row)]
            specs (od/link-specs (type-links t) links)]
        (when (and t (seq specs) (not= k @asked))
          (reset! asked k)
          (reset! answer nil)
          (reset! err nil)
          (around! {:type (name t) :id (str (:id row)) :standard true}
                   (fr/kept #(reset! answer %) #(reset! err %))
                   {:scenario (some-> (:scenario cfg) rd)}))
        (cond
          (empty? specs) [:div.capp-links__none (i18n/t :ui/capp-no-links)]
          @err           [ui/problem (i18n/t :ui/capp-links-failed) @err]
          (nil? @answer) [busy/block {:class "capp-objectview__loading"} (i18n/t :ui/capp-loading)]
          :else
          (let [open   (or @opened (od/initially-open specs expand))
                fire   ((:event-fn env) on-explore)
                by-api (:links @answer)]
            (into [:div.capp-links]
                  (map (fn [{:keys [api label] :as spec}]
                         (let [rows (od/link-peers spec (or (get by-api api)
                                                            (get by-api (name api))))
                               on?  (contains? open api)]
                           ^{:key (name api)}
                           [:section.capp-links__type
                            [:div.capp-links__head
                             [ui/button
                              {:minimal  true :small true
                               :icon     (if on? :chevron-down :chevron-right)
                               :on-click (fn [] (reset! opened ((if on? disj conj) open api)))}]
                             [:span.capp-links__label label]
                             [:span.capp-links__count (count rows)]
                             (when (and explore? fire)
                               [ui/button
                                {:on-click #(fire rows)}
                                (i18n/t :ui/capp-links-explore)])]
                            (when on?
                              (into [:ul.capp-links__peers]
                                    (map (fn [peer]
                                           ^{:key (str (:id peer))}
                                           [peer-line peer spec cfg env]))
                                    rows))])))
                  specs)))))))

(defn links
  [{:keys [rows empty] :as cfg} env]
  (let [objects (as-rows rows)]
    (if (empty? objects)
      [ui/empty-state {:icon :link :title (or empty (i18n/t :ui/capp-no-objects))}]
      (into [:div.capp-links-set]
            (map (fn [row]
                   ^{:key (str (:object-type-ref row) (:id row))}
                   [:div.capp-links-set__object
                    (when (next objects)
                      [oc/object-link (:object-type-ref row) (:id row)
                       {:row row :meta ((:meta-of env) (:object-type-ref row)) :open? false}])
                    [object-links row cfg env]]))
            objects))))

(defn- drag-attrs
  [rows]
  (when (< (count rows) 500)
    {:draggable true
     :on-drag-start
     (fn [e]
       (.setData (.-dataTransfer e) "application/x-sutra-objects"
                 (js/JSON.stringify
                  (clj->js (mapv (fn [r] {:type (some-> (:object-type-ref r) name)
                                          :id (str (:id r))})
                                 rows)))))}))

(defn object-set-title
  [{:keys [rows single? show-icon? drag? title render-empty? empty-type]} {:keys [meta-of]}]
  (let [objects (as-rows rows)
        t       (or (:object-type-ref (first objects)) (some-> empty-type ->kw))
        meta*   (when t (meta-of t))
        n       (or (vars/total-of (rd rows)) (count objects))
        shown   (od/set-title {:title title :single? single? :count* n
                               :render-empty? render-empty?
                               :type-label (when meta* (oc/type-label meta*))
                               :row-title (when meta* (oc/title meta* (first objects) nil))})]
    (when shown
      [:div (merge {:class "capp-settitle"} (when (and drag? (seq objects)) (drag-attrs objects)))
       (when (and show-icon? (:symbol meta*))
         [:span.capp-settitle__icon (:symbol meta*)])
       [:span.capp-settitle__text (:text shown)]
       (when (:count shown)
         [:span.capp-settitle__count (fmt/auto (:count shown) (rctx/current))])])))

(defn header-text
  [{:keys [title subtitle]} _env]
  [:div.capp-headertext
   [:div.capp-headertext__title title]
   (when-not (str/blank? (str subtitle))
     [:div.capp-headertext__subtitle subtitle])])

(def ^:private divider-gap {:s "var(--sp-1)" :m "var(--sp-3)" :l "var(--sp-5)"})

(defn divider-view
  [{:keys [label spacing]} _env]
  (let [gap (get divider-gap (->kw spacing) (:m divider-gap))]
    (if (str/blank? (str label))
      [divider {:style {:margin (str gap " 0")}}]
      [:div {:role "separator" :aria-label (str label)
             :style {:display "flex" :align-items "center" :gap "var(--sp-2)"
                     :margin (str gap " 0")}}
       [:div {:class "u-flex-1"} [divider]]
       [:span.capp__hint (str label)]
       [:div {:class "u-flex-1"} [divider]]])))

(defn object-view-frame
  [_cfg _env]
  (let [picked (r/atom nil)]
    (fn [{:keys [mode mode-toggle?] :as cfg} {:keys [object-view] :as env}]
      (let [mode* (or @picked (->kw mode) :auto)]
        [:div.capp-objectview-frame
         (when mode-toggle?
           [segmented-control
            {:class           "capp-segmented"
             :inline          true
             :small           true
             :value           mode*
             :options         [{:label (i18n/t :ui/capp-objectview-standard) :value :standard}
                               {:label (i18n/t :ui/capp-objectview-configured)
                                :value :configured}]
             :on-value-change (fn [m _] (reset! picked m))}])
         [object-view (assoc (dissoc cfg :mode-toggle?) :mode mode*) env]]))))

(defn- property-editor
  [_row _entry _spec _env]
  (let [draft (r/atom ::idle)
        err   (r/atom nil)]
    (fn [row {:keys [prop text]} {:keys [action param key-param]} {:keys [submit!]}]
      (if (= ::idle @draft)
        [ui/button
         {:minimal true :small true :on-click #(reset! draft (str (get (:values row) prop "")))}
         (if (str/blank? (str text)) "—" text)]
        [:span.capp-proplist__editing
         [input-group {:value @draft
                       :on-change #(reset! draft (.. % -target -value))}]
         [ui/button
          {:intent   :primary
           :on-click (fn []
                       (reset! err nil)
                       (submit! action
                                {(->kw (or key-param :id)) (:id row)
                                 (->kw (or param prop)) @draft}
                                (fr/kept #(reset! draft ::idle) #(reset! err %))))}
          (i18n/t :ui/capp-save-changes)]
         (when @err [:span.capp-action__error @err])]))))

(defn property-list
  [{:keys [object columns label-position inline-actions] :as cfg} env]
  (let [rows    (as-rows object)
        row     (first rows)
        entries (sv/property-entries row ((:display-of env) rows) cfg (rctx/current))]
    (cond
      (nil? row)       [:div.capp-objectview__empty (i18n/t :ui/property-list-no-object)]
      (empty? entries) [:div.capp-objectview__empty (i18n/t :ui/property-list-empty)]
      :else
      [display/property-list
       (mapv (fn [{:keys [prop] :as e}]
               (if-let [spec (get inline-actions prop)]
                 (assoc e :render [property-editor row e spec env])
                 e))
             entries)
       {:columns columns :label-position (->kw label-position)}])))

(defn display-frame
  [{:keys [mount]} _child]
  (let [seen  (r/atom (not= :on-screen (->kw mount)))
        in?   (r/atom false)
        obs   (atom nil)
        ref   (fn [el]
                (some-> @obs .disconnect)
                (reset! obs nil)
                (when (and el (exists? js/IntersectionObserver))
                  (let [o (js/IntersectionObserver.
                           (fn [es]
                             (let [v (.-isIntersecting (aget es 0))]
                               (reset! in? v)
                               (when v (reset! seen true)))))]
                    (.observe o el)
                    (reset! obs o))))]
    (r/create-class
     {:display-name "capp-display-frame"
      :component-will-unmount #(some-> @obs .disconnect)
      :reagent-render
      (fn [{:keys [unmount]} child]
        (let [off? (= :off-screen (->kw unmount))
              show? (if off? @in? @seen)]
          [:div.capp-display-frame {:ref ref}
           (if show? child [:div.capp-display-frame__wait])]))})))

(defn keep-alive-tabs
  [_panes _active _render]
  (let [seen (r/atom #{})]
    (fn [panes active render]
      (swap! seen conj active)
      (into [:<>]
            (keep (fn [{:keys [id body eager? never?]}]
                    (when (or (= id active) eager? (and never? (contains? @seen id)))
                      ^{:key (str id)}
                      [:div {:style (when (not= id active) {:display "none"})}
                       (render body)])))
            panes))))

(def widgets
  {:object-table     (fn [cfg env] [object-table cfg env])
   :object-list      (fn [cfg env] [object-list cfg env])
   :links            (fn [cfg env] [links cfg env])
   :object-set-title (fn [cfg env] [object-set-title cfg env])
   :header-text      (fn [cfg env] [header-text cfg env])
   :divider          (fn [cfg env] [divider-view cfg env])
   :property-list    (fn [cfg env] [property-list cfg env])
   :object-view      (fn [cfg env]
                       (if (:mode-toggle? cfg)
                         [object-view-frame cfg env]
                         ((:object-view env) cfg env)))})
