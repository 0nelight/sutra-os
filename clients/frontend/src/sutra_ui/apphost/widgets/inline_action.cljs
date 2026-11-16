(ns sutra-ui.apphost.widgets.inline-action
  (:require [reagent.core :as r]
            [sutra-ui.ui.core :as ui :refer [html-select input-group numeric-input]]
            [sutra.capps.action-grid :as grid]
            [sutra.capps.variables :as vars]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.types.submission :as csub]
            [sutra-ui.apphost.frame :as frame]
            [sutra-ui.ui.inputs :as inputs]
            [sutra-ui.apphost.action-form :as af]
            [sutra-ui.apphost.toast :refer [toast!]]
            [sutra-ui.apphost.widgets.rtf :refer [rtf-text]]))

(defn- action-label
  [descriptor nm]
  (or (get-in descriptor [:action-labels nm]) (name nm)))

(defn- deref-vals [m] (into {} (map (fn [[k v]] [k (vars/deref* v)])) (or m {})))

(defn- done!
  [{:keys [refresh] :as env} {:keys [on-done output]} data]
  (when (and refresh data) (refresh data))
  (when (satisfies? IReset output) (reset! output (vec (:created (meta data)))))
  (when-let [f (and (seq on-done) ((:event-fn env) on-done))] (f nil)))

(defn- send-rows!
  [{:keys [descriptor] :as env} {:keys [scenario] :as cfg} nm specs maps on-end]
  (letfn [(step [i data]
            (if (>= i (count maps))
              (do (done! env cfg data) (on-end i nil))
              (frame/request!
               (frame/frame-for env)
               (frame/submit-request descriptor (csub/decode-params-text specs (nth maps i))
                                     {:action   nm
                                      :scenario (vars/deref* scenario)
                                      :on-ok    (fn [d] (step (inc i) (or d data)))
                                      :on-error (fn [m] (on-end i m))}))))]
    (step 0 nil)))

(defn- cell-type
  [spec]
  (case (some-> (:type spec) ->kw)
    (:integer :long :double :decimal :number) "number"
    :date                                     "date"
    :timestamp                                "datetime-local"
    "text"))

(defn- grid-cell
  [t data-cell value put!]
  (case t
    "number" [numeric-input {:button-position :none :data-cell data-cell
                             :value value :on-value-change #(put! %2)}]
    ("date" "datetime-local")
    [inputs/iso-date-field {:minute? (= "datetime-local" t) :value value
                         :input-props {:data-cell data-cell} :on-change put!}]
    [input-group {:data-cell data-cell :value value
                  :on-change #(put! (.. % -target -value))}]))

(defn- focus-cell!
  [^js table [r c]]
  (when-let [^js el (.querySelector table (str "[data-cell='" r ":" c "']"))]
    (.focus el)))

(defn- action-table
  [_nm _specs _cfg _env]
  (let [g    (r/atom nil)
        busy (r/atom false)
        err  (r/atom nil)
        seed (atom ::none)
        file (fn [^js e specs]
               (when-let [^js f (aget (.. e -target -files) 0)]
                 (-> (.text f)
                     (.then (fn [text]
                              (let [{:keys [rows ignored]} (grid/csv-values text specs)]
                                (reset! g (grid/with-rows rows))
                                (when (seq ignored)
                                  (toast! (str "Spalten ohne Parameter: "
                                               (apply str (interpose ", " ignored)))))))))))]
    (fn [nm specs
         {:keys [row-management? file-upload? rows key-param defaults submit-label] :as cfg}
         env]
      (let [picked (vars/deref* rows)
            params (vec (keys specs))
            dflts  (deref-vals defaults)]
        (when (not= @seed picked)
          (reset! seed picked)
          (reset! g (if (and (seq picked) key-param)
                      (grid/prefilled picked key-param)
                      (grid/blank))))
        (let [{all :rows} @g
              maps (grid/to-send @g dflts)]
          [:div.capp-action-grid
           [:table.capp-table__table
            {:on-key-down
             (fn [^js e]
               (let [k (.-key e)
                     t (.. e -target)
                     [r c] (some-> (.getAttribute t "data-cell") (.split ":") array-seq)]
                 (when (and r (#{"ArrowUp" "ArrowDown"} k))
                   (.preventDefault e)
                   (focus-cell! (.-currentTarget e)
                                (grid/next-cell [(js/parseInt r) (js/parseInt c)] k
                                                (count all) (count params))))))}
            [:thead
             (into [:tr]
                   (concat (map (fn [p]
                                  ^{:key p}
                                  [:th.capp-table__th (or (:label (get specs p)) (name p))])
                                params)
                           (when row-management? [[:th.capp-table__th {:key "__rm"}]])))]
            (into [:tbody]
                  (map-indexed
                   (fn [ri {:keys [row values]}]
                     ^{:key row}
                     (into [:tr]
                           (concat
                            (map-indexed
                             (fn [ci p]
                               ^{:key p}
                               [:td.capp-table__td
                                [grid-cell (cell-type (get specs p)) (str ri ":" ci)
                                 (str (get values p ""))
                                 #(swap! g grid/set-cell row p %)]])
                             params)
                            (when row-management?
                              [^{:key "__rm"}
                               [:td.capp-table__td
                                [ui/button
                                 {:minimal    true
                                  :icon       :cross
                                  :aria-label "Zeile entfernen"
                                  :on-click   #(swap! g grid/remove-row row)}]]]))))
                   all))]
           [:div.capp-action-table__bar
            (when row-management?
              [ui/button
               {:icon :add :on-click #(swap! g grid/add-row {})}
               "Zeile hinzufügen"])
            (when file-upload?
              [:label.bp-button
               "CSV hochladen"
               [:input {:type      "file"
                        :accept    ".csv,text/csv"
                        :class     "capp-uploader__file"
                        :on-change #(file % specs)}]])
            [:span.capp-action-table__count (str (count maps) " Zeilen")]
            [ui/button
             {:intent   :primary
              :loading  @busy
              :disabled (empty? maps)
              :on-click (fn []
                          (reset! busy true)
                          (reset! err nil)
                          (send-rows! env cfg nm specs maps
                                      (fn [n m]
                                        (swap! g grid/after-send n)
                                        (reset! busy false)
                                        (when m (reset! err (str m))))))}
             (if @busy "…" (or submit-label "Absenden"))]]
           (when-let [e @err] [:div.capp-action-table__error e])])))))

(defn- blocked?
  [specs defaults]
  (:blocked? (csub/submit-gate specs (deref-vals defaults))))

(defn- action-form
  [nm specs {:keys [defaults options scenario label] :as cfg} {:keys [descriptor] :as env}]
  [af/action-form descriptor
   {:action      nm
    :specs       specs
    :sections    (get (:action-forms descriptor) nm)
    :controls    {:rtf rtf-text}
    :scenario    (vars/deref* scenario)
    :defaults    (deref-vals defaults)
    :options     (into {} (map (fn [[k v]] [k (vec (vars/deref* v))])) (or options {}))
    :object      (:object descriptor)
    :object-type (:object-of-type descriptor)
    :label       label
    :on-ok       #(done! env cfg %)
    :on-error    (fn [m] (toast! (str m) {:error? true}))}])

(defn inline-action
  [_cfg _env]
  (let [picked (r/atom 0)
        layout (r/atom nil)]
    (fn [{:keys [title hide-header? layout-switch? invalid-state] :as cfg}
         {:keys [descriptor] :as env}]
      (let [entries (grid/action-entries cfg)
            i       (min @picked (max 0 (dec (count entries))))
            entry   (merge (dissoc cfg :actions) (get entries i))
            nm      (->kw (:action entry))
            specs   (get (:action-params descriptor) nm)
            shown   (or @layout (->kw (:layout entry)) :form)
            hidden? (and (= :hidden (->kw invalid-state))
                         (= :form shown)
                         (blocked? specs (:defaults entry)))]
        (when-not hidden?
          [:div.capp-inline-action
           (when-not hide-header?
             [:div.capp-inline-action__head
              [:div.capp-inline-action__title
               (or (:title entry) title (when nm (action-label descriptor nm)))]
              (when (> (count entries) 1)
                (into [html-select
                       {:value     i
                        :on-change #(reset! picked (js/parseInt (.. % -target -value)))}]
                      (map-indexed
                       (fn [j {:keys [action title]}]
                         ^{:key j}
                         [:option {:value j}
                          (or title (action-label descriptor (->kw action)))]))
                      entries))
              (when layout-switch?
                [ui/button
                 {:on-click #(reset! layout (if (= :table shown) :form :table))}
                 (if (= :table shown) "Als Formular" "Als Tabelle")])])
           (when nm
             (if (= :table shown)
               ^{:key nm} [action-table nm specs entry env]
               ^{:key nm} [action-form nm specs entry env]))])))))

(def widgets
  {:inline-action (fn [cfg env] [inline-action cfg env])})
