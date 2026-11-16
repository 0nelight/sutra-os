(ns sutra-ui.apphost.widgets.analysis
  (:require [sutra-ui.ui.sel :as sel :refer [wr!]]
            [sutra-ui.storage :as storage]
            [clojure.string :as str]
            [sutra-ui.ui.core :as ui :refer [checkbox html-table html-select input-group text-area]]
            [goog.object :as gobj]
            [reagent.core :as r]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.capps.analysis-path :as ap]
            [sutra.capps.gantt :as gantt]
            [sutra.capps.map-geo :as geo]
            [sutra.capps.table :as tbl]
            [sutra.capps.ts-analysis :as ta]
            [sutra.capps.ts-ops :as ops]
            [sutra-ui.ui.chart :refer [plotly-node]]
            [sutra-ui.ui.inputs :as inputs]
            [sutra-ui.apphost.frame :as fr]))

(defn- saved-key [widget-key] (str "sutra.analysis." widget-key))

(defn- saved-all
  [widget-key]
  (or (geo/parse-json (storage/get-item (saved-key widget-key))) {}))

(defn- save!
  [widget-key nm value]
  (storage/set-item! (saved-key widget-key)
                     (geo/write-json (assoc (saved-all widget-key) (keyword nm) value))))

(defn- saved-bar
  [_ _ _]
  (let [nm (r/atom "")]
    (fn [widget-key on-save on-load]
      [:div.capp-analysis-saved
       [input-group {:small true :value @nm :placeholder "Name der Analyse"
                     :aria-label "Name der Analyse"
                     :on-change #(reset! nm (.. % -target -value))}]
       [ui/button {:intent :primary :disabled (str/blank? @nm)
                   :on-click #(on-save @nm)}
        "Speichern"]
       (into [html-select {:value "" :aria-label "Gespeicherte Analyse"
                           :on-change #(when-let [k (not-empty (.. % -target -value))]
                                         (on-load k))}
              [:option {:value ""} "Laden…"]]
             (for [k (sort (map name (keys (saved-all widget-key))))]
               ^{:key k} [:option {:value k} k]))])))

(def ^:private card-labels
  {:metric "Kennzahl" :text "Text" :filter "Filter" :linked "Verknüpfte Objekte"
   :selector "Objekt wählen" :table "Tabelle" :pivot "Pivot" :bar "Balken" :heat-grid "Heatmap"
   :line "Linie" :pie "Kreis" :waterfall "Wasserfall"})

(defn- prop-select
  [props value on-change placeholder]
  (into [html-select {:value (or (some-> value name) "") :aria-label placeholder
                      :on-change #(on-change (not-empty (.. % -target -value)))}
         [:option {:value ""} placeholder]]
        (for [p props] ^{:key p} [:option {:value (name p)} (name p)])))

(def ^:private agg-fns [:count :sum :avg :min :max :cardinality])

(defn- agg-select
  [value on-change]
  (into [html-select {:value (or (some-> value name) "count") :aria-label "Aggregation"
                      :on-change #(on-change (.. % -target -value))}]
        (for [a agg-fns] ^{:key a} [:option {:value (name a)} (name a)])))

(defn- chart-plot
  [_]
  (plotly-node "analysis-plot" identity {:traces [] :layout {}}))

(defn- conditions-editor
  [props {:keys [conditions]} put!]
  [:div.capp-analysis-conditions
   (doall
    (for [[i {:keys [prop value]}] (map-indexed vector conditions)]
      ^{:key i}
      [:div.capp-analysis-condition
       [prop-select props prop #(put! :conditions (assoc-in (vec conditions) [i :prop] %))
        "Eigenschaft"]
       [input-group {:small true :value (or value "") :aria-label "Wert"
                     :on-change #(put! :conditions
                                       (assoc-in (vec conditions) [i :value]
                                                 (.. % -target -value)))}]]))
   [ui/button
    {:minimal true :small true :on-click #(put! :conditions (conj (vec conditions) {:op :is}))}
    "+ Bedingung"]])

(defn- linked-card
  [{:keys [id opts input]} links !linked around! put!]
  [:div
   (into [html-select {:value (or (:link opts) "") :aria-label "Verknüpfung"
                       :on-change (fn [e]
                                    (let [l (not-empty (.. e -target -value))]
                                      (put! :link l)
                                      (when l
                                        (around! (keyword l) input
                                                 #(swap! !linked assoc id %)))))}
          [:option {:value ""} "Verknüpfung wählen"]]
         (for [l links] ^{:key l} [:option {:value (name l)} (name l)]))
   [:div.capp-analysis-count (str (count (get @!linked id)) " verknüpfte Objekte")]])

(defn- card-view
  [{:keys [card-type opts input output] :as card} {:keys [links !linked around! title-of]} put!]
  (let [props (ap/properties input)
        sel   (fn [k placeholder] [prop-select props (get opts k) #(put! k %) placeholder])]
    (case card-type
      :text     [text-area {:value (or (:text opts) "") :rows 3 :aria-label "Text"
                            :on-change #(put! :text (.. % -target -value))}]
      :metric   [:div
                 [agg-select (:agg-fn opts) #(put! :agg-fn %)]
                 (sel :of "Eigenschaft")
                 [:div.capp-analysis-metric (str (ap/metric input opts))]]
      :filter   [:div
                 [conditions-editor props opts put!]
                 [:div.capp-analysis-count (str (count output) " von " (count input))]]
      :linked   [linked-card card links !linked around! put!]
      :selector (into [html-select {:value (or (:object opts) "") :aria-label "Objekt"
                                    :on-change #(put! :object (not-empty (.. % -target -value)))}
                       [:option {:value ""} "Objekt wählen"]]
                      (for [r input]
                        ^{:key (:id r)} [:option {:value (str (:id r))} (title-of r)]))
      :table    (let [cols (mapv ->kw (or (seq (:cols opts)) (take 6 props)))]
                  [html-table {:class "capp-analysis-table" :compact true}
                   [:thead (into [:tr] (for [c cols] ^{:key c} [:th (name c)]))]
                   [:tbody
                    (for [r (take 100 input)]
                      ^{:key (:id r)}
                      (into [:tr] (for [c cols]
                                    ^{:key c} [:td (str ((tbl/field-fn c) r))])))]])
      :pivot    [:div
                 (sel :row "Zeile") (sel :col "Spalte") (sel :of "Wert")
                 [agg-select (:agg-fn opts) #(put! :agg-fn %)]
                 (when-let [{:keys [row-labels col-labels cells]} (ap/pivot input opts)]
                   [html-table {:class "capp-analysis-table" :compact true}
                    [:thead (into [:tr [:th ""]]
                                  (for [c col-labels] ^{:key (str c)} [:th (str c)]))]
                    [:tbody
                     (for [[i rh] (map-indexed vector row-labels)]
                       ^{:key i}
                       (into [:tr [:td (str rh)]]
                             (for [[j v] (map-indexed vector (get cells i))]
                               ^{:key j} [:td (str v)])))]])]
      [:div
       (sel :x (if (= :heat-grid card-type) "Spalte" "Kategorie"))
       (when (#{:heat-grid :bar} card-type) (sel :group (if (= :bar card-type) "Segment" "Zeile")))
       (sel :of "Wert")
       [agg-select (:agg-fn opts) #(put! :agg-fn %)]
       (when-let [spec (ap/chart-of card-type input opts)]
         [chart-plot spec])])))

(defn free-form-analysis
  [_ _]
  (let [!path   (r/atom (ap/empty-path))
        !linked (r/atom {})
        !sent   (atom ::none)]
    (fn [{:keys [rows empty-header empty-description path-saving? copy-notebook? output links
                 widget-key]}
         {:keys [title-of linked! links-of]}]
      (let [base  (vec rows)
            {:keys [out] ended :rows} (ap/run-path @!path base #(get @!linked %))
            tkey  (or widget-key "free-form")
            ctx   {:links    (or (seq links) (when links-of (links-of base)))
                   :!linked  !linked
                   :around!  linked!
                   :title-of (if title-of (title-of base) str)}]
        (when (and output (not= ended @!sent))
          (reset! !sent ended)
          (js/queueMicrotask #(wr! output ended)))
        [:div.capp-analysis
         (when path-saving?
           [saved-bar tkey
            #(save! tkey % (ap/saveable @!path))
            (fn [k] (when-let [v (get (saved-all tkey) (keyword k))]
                      (reset! !path (update v :cards
                                            (fn [cs] (mapv #(update % :card-type ->kw) cs))))))])
         (if (empty? out)
           [ui/empty-state {:icon :search :title (or empty-header "Noch keine Karten")
                             :hint empty-description}]
           (for [{:keys [id card-type] :as card} out]
             ^{:key id}
             [:div.capp-analysis-card
              [:div.capp-analysis-card__head
               (get card-labels card-type (name card-type))
               (when copy-notebook?
                 [ui/button
                  {:minimal true :small true
                   :on-click #(.writeText (.-clipboard js/navigator) (ap/card-markdown card))}
                  "Für Notebook kopieren"])
               [ui/button {:minimal true :small true :on-click #(swap! !path ap/remove-card id)}
                "Entfernen"]]
              [card-view card ctx #(swap! !path ap/set-opt id %1 %2)]]))
         (into [html-select
                {:class "capp-analysis-add" :value "" :aria-label "Karte hinzufügen"
                 :on-change #(when-let [t (not-empty (.. % -target -value))]
                               (swap! !path ap/add-card t))}
                [:option {:value ""} "+ Karte hinzufügen"]]
               (for [t ap/card-types]
                 ^{:key t} [:option {:value (name t)} (get card-labels t (name t))]))]))))

(defn- answer->series
  [{:keys [series]} fallback]
  (mapv (fn [{:keys [series-id points objects]}]
          {:label  (or (some :label objects) fallback (str series-id))
           :points (mapv (fn [[t x]] [(gantt/ms t) (when (number? x) x)]) points)})
        series))

(defn- load-series!
  [!a env spec controlled?]
  ((:series! env) (dissoc spec :label :canvas)
                  (assoc (fr/kept (fn [answer]
                                    (doseq [s (answer->series answer (:label spec))]
                                      (swap! !a ta/add-series
                                             (assoc s :controlled? controlled?
                                                    :canvas (:canvas spec)
                                                    :source (dissoc spec :label :canvas))))))
                         :report true)))

(defn- initial-events
  [{:keys [rows start end label]}]
  {:label label :controlled? true :event-kind :linked
   :events (vec (keep (fn [r]
                        (let [s (gantt/ms ((tbl/field-fn start) r))
                              e (gantt/ms ((tbl/field-fn end) r))]
                          (when s {:start s :end (or e s)})))
                      rows))})

(defn- num-input
  [value on-change label]
  [inputs/num-field {:small true :aria-label label :placeholder label
                  :value value :on-change on-change}])

(defn- option-name [x] (if (keyword? x) (name x) (str x)))

(defn- select-of
  [value options on-change label]
  (into [html-select {:value (if (some? value) (option-name value) "") :aria-label label
                      :on-change #(on-change (keyword (.. % -target -value)))}]
        (for [o options] ^{:key o} [:option {:value (option-name o)} (option-name o)])))

(defn- plot-row
  [{:keys [id label display controlled? source] :as plot} a !a resolved view]
  (let [set-d (fn [k] #(swap! !a ta/set-display id k %))
        axis  (:axis display)
        ax    (get-in a [:axes axis] ta/default-axis)
        set-a (fn [k] #(swap! !a ta/set-axis axis k %))
        st    (ta/stat-row plot resolved view)]
    [:tr
     [:td label (when (:error st) [ui/problem (:error st) nil])]
     [:td [select-of (:canvas plot) (range (:canvases a))
           #(swap! !a ta/set-canvas id (js/parseInt (name %))) "Diagramm"]]
     [:td [select-of (:dash display) [:solid :dashed] (set-d :dash) "Linie"]]
     [:td [num-input (:width display) (set-d :width) "Breite"]]
     [:td [checkbox {:checked (boolean (:gradient? display)) :aria-label "Verlauf"
                     :on-change #(swap! !a ta/set-display id :gradient?
                                        (not (:gradient? display)))}]]
     [:td [select-of (:shape display) [:none :circle :triangle-up :square :diamond]
           (set-d :shape) "Punktform"]]
     [:td [num-input (:size display) (set-d :size) "Größe"]]
     [:td [:input {:type "color" :value (or (:fill display) "#1f6e63") :aria-label "Füllung"
                   :on-change #(swap! !a ta/set-display id :fill (.. % -target -value))}]]
     [:td [num-input (:outline display) (set-d :outline) "Rand"]]
     [:td [select-of (:interpolation display) [:linear :nearest :step-before :step-after :none]
           (set-d :interpolation) "Interpolation"]]
     [:td [select-of (:external display) [:none :nearest] (set-d :external) "Außen"]]
     [:td [num-input axis #(swap! !a ta/set-display id :axis (int %)) "Achse"]
      [ui/button {:icon :plus :minimal true :small true :on-click #(swap! !a ta/new-axis)}]]
     [:td [input-group {:small true :value (or (:unit ax) "") :aria-label "Einheit"
                        :on-change #((set-a :unit) (.. % -target -value))}]]
     [:td [checkbox {:checked (boolean (:auto? ax)) :aria-label "Automatisch"
                     :on-change #((set-a :auto?) (not (:auto? ax)))}]]
     [:td [num-input (:min ax) (set-a :min) "Min"]]
     [:td [num-input (:max ax) (set-a :max) "Max"]]
     [:td [select-of (:side ax) [:left :right] (set-a :side) "Seite"]]
     [:td [checkbox {:checked (boolean (:log? ax)) :aria-label "Logarithmisch"
                     :on-change #((set-a :log?) (not (:log? ax)))}]]
     [:td [checkbox {:checked (boolean (:invert? ax)) :aria-label "Umkehren"
                     :on-change #((set-a :invert?) (not (:invert? ax)))}]]
     [:td (str (or (:find source) "")) " " (some-> (:property source) str)]
     [:td (some-> (:min st) str)]
     [:td (some-> (:max st) str)]
     [:td (some-> (:mean st) str)]
     [:td (when-not controlled?
            [ui/button {:icon :cross :minimal true :small true
                        :on-click #(swap! !a ta/remove-plot id)}])]]))

(defn- plots-panel
  [a !a resolved view]
  [html-table {:class "capp-ts-plots" :compact true}
   [:thead
    (into [:tr]
          (for [h ["Plot" "Diagramm" "Linie" "Breite" "Verlauf" "Punkt" "Größe" "Füllung" "Rand"
                   "Innen" "Außen" "Achse" "Einheit" "Auto" "Min" "Max" "Seite" "Log" "Umkehren"
                   "Quelle" "Min" "Max" "Mittel" ""]]
            ^{:key h} [:th h]))]
   [:tbody
    (for [p (:plots a)]
      ^{:key (:id p)} [plot-row p a !a resolved view])
    (for [{:keys [id label events highlight? controlled?]} (:event-sets a)]
      ^{:key (str "e" id)}
      [:tr
       [:td label]
       [:td {:col-span 19} (str (ops/count-in-view events view) " Ereignisse")]
       [:td [checkbox {:checked (boolean highlight?) :label "Markieren"
                       :on-change #(swap! !a ta/toggle-highlight id)}]]
       [:td {:col-span 2}]
       [:td (when-not controlled?
              [ui/button {:icon :cross :minimal true :small true
                          :on-click #(swap! !a ta/remove-event-set id)}])]])]])

(defn- new-plot-menu
  [_ _ _]
  (let [st (r/atom {:params {}})]
    (fn [a !a {:keys [plot-types new-plot-canvas]}]
      (let [{:keys [plot-type inputs params]} @st
            set-p (fn [k] #(swap! st assoc-in [:params k] %))]
        [:div.capp-ts-new
         (into [html-select {:value (or (some-> plot-type name) "") :aria-label "Neuer Plot"
                             :on-change #(swap! st assoc :plot-type
                                                (not-empty (.. % -target -value)))}
                [:option {:value ""} "Neuer Plot…"]]
               (for [t (or (seq (map ->kw plot-types)) ops/plot-types)]
                 ^{:key t} [:option {:value (name t)} (name t)]))
         (into [:select {:multiple true :aria-label "Eingaben"
                         :on-change (fn [e]
                                      (swap! st assoc :inputs
                                             (mapv #(js/parseInt (.-value %))
                                                   (array-seq (.. e -target -selectedOptions)))))}]
               (for [{:keys [id label]} (:plots a)]
                 ^{:key id} [:option {:value id} label]))
         [num-input (:window params) (set-p :window) "Fenster (ms)"]
         [num-input (:period params) (set-p :period) "Periode (ms)"]
         [num-input (:every params) (set-p :every) "Takt (ms)"]
         [num-input (:by params) (set-p :by) "Versatz (ms)"]
         [num-input (:k params) (set-p :k) "Standardabweichungen"]
         [num-input (:cutoff params) (set-p :cutoff) "Grenzfrequenz (Hz)"]
         [num-input (:value params) (set-p :value) "Schwelle"]
         [select-of (or (:how params) :mean) [:mean :min :max :sum :count :first :last]
          (set-p :how) "Methode"]
         [select-of (or (:family params) :butterworth)
          [:butterworth :chebyshev :inverse-chebyshev] (set-p :family) "Filter"]
         [select-of (or (:op params) :gt) [:gt :gte :lt :lte :eq :ne] (set-p :op) "Bedingung"]
         [input-group {:small true :value (or (:expr params) "") :placeholder "Formel: a + 2*b"
                       :aria-label "Formel"
                       :on-change #(swap! st assoc-in [:params :expr] (.. % -target -value))}]
         [ui/button {:intent :primary :disabled (or (nil? plot-type) (empty? inputs))
                     :on-click #(do (swap! !a ta/add-plot
                                           {:plot-type plot-type :inputs inputs
                                            :params params
                                            :canvas (or new-plot-canvas 0)})
                                    (swap! st assoc :plot-type nil))}
          "Hinzufügen"]]))))

(defn- event-set-menu
  [_ _ _]
  (let [st (r/atom {:op :gt})]
    (fn [a !a {:keys [event-set-types]}]
      (let [types (or (seq (map ->kw event-set-types)) ops/event-set-types)]
        (when (some #{:search} types)
          (let [{:keys [source op value]} @st
                pts (some #(when (= source (:id %)) (:points %)) (:plots a))]
            [:div.capp-ts-new
             "Ereignisse suchen: "
             (into [html-select {:aria-label "Zeitreihe"
                                 :on-change #(swap! st assoc :source
                                                    (js/parseInt (.. % -target -value)))}
                    [:option {:value ""} "Zeitreihe"]]
                   (for [{:keys [id label]} (:plots a)] ^{:key id} [:option {:value id} label]))
             [select-of op [:gt :gte :lt :lte] #(swap! st assoc :op %) "Bedingung"]
             [num-input value #(swap! st assoc :value %) "Schwelle"]
             [ui/button
              {:intent :primary :disabled (or (nil? pts) (nil? value))
               :on-click #(swap! !a ta/add-event-set
                                 {:label (str "Suche " (name op) " " value)
                                  :event-kind :search
                                  :events (ops/search-events pts {:op op :value value})})}
              "Anlegen"]]))))))

(defn- add-data
  [_ _ _]
  (let [choice (r/atom nil)]
    (fn [!a env selectable]
      [:div.capp-ts-new
       (into [html-select {:value (or @choice "") :aria-label "Zeitreihe hinzufügen"
                           :on-change #(reset! choice (not-empty (.. % -target -value)))}
              [:option {:value ""} "+ Daten…"]]
             (map-indexed (fn [i {:keys [label find property]}]
                            ^{:key i} [:option {:value i}
                                       (or label (str (name find) " · " (name property)))])
                          selectable))
       [ui/button
        {:intent :primary :disabled (nil? @choice)
         :on-click #(do (load-series! !a env (nth selectable (js/parseInt @choice)) false)
                        (reset! choice nil))}
        "Hinzufügen"]])))

(defn- canvas-plot
  [_]
  (plotly-node "ts-canvas" :spec {:spec {:traces [] :layout {}}}
               (fn [^js node {:keys [on-range]}]
                 (when (and node (.-on node))
                   (set! (.-sutraOnRange node) on-range)
                   (when-not (.-sutraRange node)
                     (set! (.-sutraRange node) true)
                     (.on node "plotly_relayout"
                          (fn [^js e]
                            (let [a (gobj/get e "xaxis.range[0]")
                                  b (gobj/get e "xaxis.range[1]")]
                              (when-let [f (and a b (.-sutraOnRange node))]
                                (f [(gantt/ms a) (gantt/ms b)]))))))))))

(defn- load-saved!
  [!a env tkey k keep? output-rid]
  (when-let [v (get (saved-all tkey) (keyword k))]
    (when-not keep? (reset! !a (assoc v :plots [] :next-id (:next-id v 1))))
    (when output-rid (wr! output-rid (str "analysis:" k)))
    (doseq [spec (distinct (keep :source (:plots v)))] (load-series! !a env spec false))))

(defn time-series-analysis
  [_ _]
  (let [!a      (r/atom nil)
        !view   (r/atom nil)
        loaded? (atom false)]
    (fn [{:keys [series event-sets selectable overlay-y? collapse-y? boundaries? tooltip? digits
                 utc? sync-x? saving? output-rid autoload keep-on-load? widget-key canvases]
          :as cfg}
         env]
      (when-not @loaded?
        (reset! loaded? true)
        (reset! !a (ta/empty-analysis {:canvases canvases}))
        (doseq [spec series] (load-series! !a env spec true))
        (doseq [e event-sets] (swap! !a ta/add-event-set (initial-events e)))
        (doseq [k autoload]
          (load-saved! !a env (or widget-key "time-series") k keep-on-load? output-rid)))
      (let [a        @!a
            resolved (ta/resolved-points a)
            now      (.getTime (js/Date.))
            view     (or @!view
                         (ta/default-view (-> cfg
                                              (update :view-start gantt/ms)
                                              (update :view-end gantt/ms))
                                          resolved now))
            tkey     (or widget-key "time-series")
            opts     {:utc? utc? :tooltip? tooltip? :digits digits
                      :overlay-y? overlay-y? :collapsed? collapse-y? :boundaries? boundaries?}]
        [:div.capp-ts
         [add-data !a env selectable]
         [new-plot-menu a !a cfg]
         [event-set-menu a !a cfg]
         [ui/button {:minimal true :small true :on-click #(swap! !a ta/add-canvas)}
          "+ Diagramm"]
         (when saving?
           [saved-bar tkey
            (fn [nm]
              (save! tkey nm (ta/saveable a))
              (when output-rid (wr! output-rid (str "analysis:" nm))))
            #(load-saved! !a env tkey % keep-on-load? output-rid)])
         (for [c (range (:canvases a))]
           ^{:key c}
           [canvas-plot {:spec     (ta/canvas-spec a resolved c view opts)
                         :on-range (when sync-x? #(reset! !view %))}])
         [plots-panel a !a resolved view]]))))

(def widgets
  {:free-form-analysis   (fn [cfg env] [free-form-analysis cfg env])
   :time-series-analysis (fn [cfg env] [time-series-analysis cfg env])})
