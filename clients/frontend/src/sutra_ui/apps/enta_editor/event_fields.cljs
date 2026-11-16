(ns sutra-ui.apps.enta-editor.event-fields
  (:require [sutra-ui.ui.core :as ui]
            [sutra.kit.tree :as tree]
            [clojure.string :as str]
            [reagent.core :as r]
            [sutra.capps.events :as ce]
            [sutra.i18n.core :as i18n]
            [sutra-ui.apps.enta-editor.state :as es]
            [sutra-ui.apps.enta-editor.fields :as f]
            [sutra-ui.graph-editor.shared :as gs]
            [sutra-ui.ui.form :as uf]
            [sutra-ui.ui.inputs :as inputs]))

(defn- select-in
  [v options on-change placeholder fill?]
  [f/select v options #(on-change (f/kw-or-nil %)) placeholder {:fill? fill?}])

(defn- kw-in
  [v on-change placeholder]
  [inputs/text-input (f/at (f/value-str v) #(on-change (f/kw-or-nil %)))
   {:placeholder placeholder :small true :class "u-flex-1 ab-field--min"}])

(defn- literal
  [s]
  (cond (str/blank? s)                  nil
        (str/starts-with? s ":")        (keyword (subs s 1))
        (= s "true")                    true
        (= s "false")                   false
        (re-matches #"-?\d+(\.\d+)?" s) (js/parseFloat s)
        :else                           s))

(defn- literal-in
  [v on-change]
  [inputs/text-input
   (f/at (cond (nil? v) "" (string? v) v (keyword? v) (str v) :else (pr-str v))
         #(on-change (literal %)))
   {:placeholder "Wert (leer = nil)" :small true :class "u-flex-1 ab-field--min"}])

(defn- literal-area
  [v on-change]
  [ui/text-area {:value (if (string? v) v (if (nil? v) "" (pr-str v)))
                 :rows 5
                 :on-change #(let [s (.. % -target -value)]
                               (on-change (when-not (str/blank? s) s)))
                 :fill true :style {:flex "1 1 100%"}}])

(defn- hint-line [t]
  (when t [:div {:style {:color gs/grey :font-size "var(--fs-xs)" :margin-top "var(--sp-0)"}} t]))

(defn- raw-field
  [_v _on-change]
  (let [txt (r/atom nil) err (r/atom nil)]
    (fn [v on-change]
      [:div
       [ui/text-area {:value (or @txt (if (nil? v) "" (pr-str v)))
                      :rows 3
                      :on-change (fn [e]
                                   (let [s (.. e -target -value)]
                                     (reset! txt s)
                                     (if (str/blank? s)
                                       (do (reset! err nil) (on-change nil))
                                       (let [parsed (es/read-edn s)]
                                         (reset! err (:error parsed))
                                         (when-not (:error parsed)
                                           (on-change (:value parsed)))))))
                      :on-blur #(reset! txt nil)
                      :fill true :class "u-mono-12"}]
       (when @err [:div {:style {:color "var(--c-danger-strong)"
                                 :font-size "var(--fs-xs)"}} @err])])))

(defn- value-field
  [v var-names otype multiline? on-change]
  (let [mode  (ce/value-mode v)
        inner (ce/value-of v)
        key-of (fn [m] (:key (first (filter #(= m (:id %)) ce/value-refs))))]
    [:div {:style {:display "flex" :gap "var(--sp-1)" :align-items "center"
                   :flex-wrap (if multiline? "wrap" "nowrap")}}
     [f/select mode (for [{:keys [id label]} ce/value-refs] [(name id) label])
      #(let [k (key-of (keyword %))] (on-change (if k {k nil} nil)))
      nil {:clearable? false}]
     (case mode
       :literal    (if multiline?
                     [literal-area inner on-change]
                     [literal-in inner on-change])
       :row        [:div {:class "u-flex-1"}
                    [f/prop-field inner otype {:small true} #(on-change {:row %})]]
       (:item :arg) [kw-in inner #(on-change {(key-of mode) %}) "Eigenschaft"]
       (:val :var) [select-in inner (f/var-options var-names) #(on-change {(key-of mode) %})
                    "Variable…" nil]
       nil)]))

(defn- pane-options
  [handle]
  (let [want (some-> (:var handle) name)]
    (vec (for [m (tree/nodes (es/tree))
               :when (#{:pages :tabs} (some-> (:kind m) keyword))
               :when (or (nil? want) (= want (some-> m :select :var name)))
               pane (concat (:pages m) (:tabs m))
               :when (:id pane)]
           [(name (:id pane)) (str (or (:label pane) (name (:id pane))))]))))

(defn- overlay-options []
  (vec (for [m (tree/nodes (es/tree))
             :when (= :overlay (some-> (:kind m) keyword))
             :when (:overlay m)]
         [(name (:overlay m)) (str (or (:title m) (name (:overlay m))))])))

(defn- field-editor
  [{:keys [key kind label hint multiline]} e var-names otype put!]
  [uf/field {:label label :help hint :inline? true :class "ab-row-field ab-row-field--event"}
   (case kind
     :handle      [select-in (:var (get e key)) (f/var-options var-names)
                   #(put! key (when % {:var %})) "Variable…" true]
     :value       [value-field (get e key) var-names otype multiline #(put! key %)]
     :page        [f/pick-or-free (get e key) (pane-options (:var e)) #(put! key %)
                   (i18n/t :ui/app-builder-page-pick)]
     :type        [f/pick-or-free (get e key) (f/object-type-options) #(put! key %)
                   (i18n/t :ui/app-builder-type-of-row)]
     :overlay-key [f/pick-or-free (get e key) (overlay-options) #(put! key %)
                   (i18n/t :ui/app-builder-overlay-pick)]
     [kw-in (get e key) #(put! key %) (str key)])])

(defn- retype
  [e d]
  (merge (ce/blank d) (select-keys e (disj (ce/effect-keys d) :do)) {:do d}))

(defn- effect-row
  [i e var-names otype problems on-change* rows]
  (let [d     (:do e)
        known (some? (ce/spec d))
        put!  (fn [k v] (on-change* (if (nil? v) (assoc rows i (dissoc e k))
                                        (assoc-in rows [i k] v))))
        drop! #(on-change* (vec (concat (subvec rows 0 i) (subvec rows (inc i)))))
        move! (fn [j] (on-change* (assoc rows i (nth rows j) j e)))]
    [:div {:style {:border "1px solid var(--c-border-soft)" :border-radius "var(--radius)"
                   :padding "var(--sp-1) var(--sp-3)"
                   :background (if (seq problems) "var(--c-danger-bg)" "var(--c-surface)")}}
     [:div {:style {:display "flex" :gap "var(--sp-1)" :align-items "center"}}
      [:span {:style {:color gs/grey :font-size "var(--fs-sm)"}} (str (inc i) ".")]
      [f/select (when known d) (for [{k :do lbl :label} ce/effects] [(name k) lbl])
       #(on-change* (assoc rows i (retype e (f/kw-or-nil %))))
       (when-not known (str "? " (pr-str d))) {:class "u-flex-1" :clearable? false}]
      [f/row-button :arrow-up "nach oben" #(move! (dec i)) (zero? i)]
      [f/row-button :arrow-down "nach unten" #(move! (inc i)) (= i (dec (count rows)))]
      [f/row-button :cross "entfernen" drop! false]]
     (if known
       [:div
        (for [fd (ce/fields d)]
          ^{:key (:key fd)} [field-editor fd e var-names otype put!])]
       [:div {:style {:margin-top "var(--sp-1)"}}
        [raw-field e #(on-change* (assoc rows i %))]])
     (for [{:keys [error]} problems]
       ^{:key error} [:div {:style {:color "var(--c-danger-strong)" :font-size "var(--fs-xs)"
                                    :margin-top "var(--sp-1)"}}
                      error])]))

(defn event-field
  [_value _on-change _var-names _otype]
  (let [raw? (r/atom false)]
    (fn [value on-change var-names otype]
      (let [rows (ce/as-vector value)
            by-i (group-by :index (ce/explain value))
            emit (fn [v] (on-change (ce/as-event v)))]
        [:div {:style {:display "flex" :flex-direction "column" :gap "var(--sp-1)"}}
         (if @raw?
           [raw-field value on-change]
           [:div {:style {:display "flex" :flex-direction "column" :gap "var(--sp-1)"}}
            (for [[i e] (map-indexed vector rows)]
              ^{:key i} [effect-row i e var-names otype (get by-i i) emit rows])
            (when (empty? rows)
              (hint-line (i18n/t :ui/app-builder-event-empty)))
            [:div {:style {:display "flex" :gap "var(--sp-2)" :flex-wrap "wrap"}}
             (for [{k :do lbl :label} ce/effects]
               ^{:key k} [f/dashed-button (str "+ " lbl) #(emit (conj rows (ce/blank k)))])]])
         [:div
          [ui/button {:minimal true :small true :on-click #(swap! raw? not)}
           (if @raw? (i18n/t :ui/app-builder-event-builder)
             (i18n/t :ui/app-builder-edit-as-edn))]]]))))
