(ns sutra-ui.ui.inputs
  (:require [sutra-ui.ui.table :as table :refer [committing]]
            [sutra-ui.ui.overlay :as overlay :refer [portal-class combo combo-option]]
            [sutra-ui.ui.sel :as sel :refer [rd wr!]]
            [sutra.kit.coerce :refer [->kw]]
            [reagent.core :as r]
            [sutra-ui.ui.core :as ui :refer [html-select input-group numeric-input radio
                                             segmented-control]]
            [clojure.string :as str]
            [sutra.capps.format :as fmt :refer []]
            [sutra-ui.render-ctx :as rctx]
            [sutra.capps.table :as tbl]
            [sutra.ref.tokens :as rt]
            [sutra.types.submission :as csub]
            [sutra.i18n.core :as i18n]))

(defn text-input
  [sel {:keys [placeholder class type] :as opts}]
  [input-group (merge (select-keys opts [:fill :disabled :small :auto-focus :left-icon
                                         :right-element :inputMode :list :maxLength :title
                                         :on-key-down :on-focus :on-blur])
                      {:class       class
                       :type        (or type "text")
                       :value       (str (or (rd sel) ""))
                       :placeholder placeholder
                       :on-change   #(wr! sel (.. % -target -value))})])

(defn- js-num
  [s]
  (let [n (when-not (str/blank? s) (js/parseFloat s))]
    (when-not (js/isNaN n) n)))

(defn num-field
  [_]
  (let [draft (r/atom "")]
    (fn [{:keys [value on-change] :as props}]
      [numeric-input (merge {:button-position :none}
                            (dissoc props :value :on-change)
                            {:value (if (= (js-num @draft) value) @draft (str (or value "")))
                             :on-value-change (fn [_ s]
                                                (reset! draft s)
                                                (on-change (js-num s)))})])))

(def ^:private iso-day "yyyy-MM-dd")
(def ^:private iso-minute "yyyy-MM-dd'T'HH:mm")

(defn- iso->date
  [s pattern]
  (let [s (str s)
        n (if (= pattern iso-day) 10 16)
        d (when (>= (count s) n) (ui/parse-date (subs s 0 n) pattern))]
    (when (and d (not (js/isNaN (.getTime d)))) d)))

(defn iso-date-field
  [{:keys [value on-change minute?] lo :min hi :max :as props}]
  (let [pat (if minute? iso-minute iso-day)]
    [ui/date-input
     (merge (dissoc props :value :on-change :minute? :min :max)
            {:value                (iso->date value pat)
             :time-precision       (when minute? :minute)
             :date-fns-format      pat
             :min-date             (or (iso->date lo iso-day) (js/Date. 1900 0 1))
             :max-date             (or (iso->date hi iso-day) (js/Date. 2100 11 31))
             :invalid-date-message (i18n/t :ui/capp-date-invalid)
             :out-of-range-message (i18n/t :ui/capp-date-out-of-range)
             :on-change            (fn [d _] (on-change (if d (ui/format-date d pat) "")))})]))

(defn- joined-field-fn
  [spec sep]
  (when-let [fs (some->> (if (sequential? spec) spec [spec])
                         (keep tbl/field-fn) seq vec)]
    (fn [item]
      (->> fs (map #(some-> (% item) str)) (remove str/blank?) (str/join sep)))))

(defn option-list
  [items {sort-prop :sort-by :keys [item-value item-label item-search item-match sort]}]
  (let [ctx  (rctx/current)
        vf   (or (tbl/field-fn item-value) :value)
        mf   (tbl/field-fn item-match)
        lf   (or (joined-field-fn item-label " · ") :label)
        sf   (joined-field-fn item-search " ")
        dir  (->kw sort)
        key* (or (when-let [p (tbl/field-fn sort-prop)]
                   (let [by (into {} (map (fn [it] [(if (map? it) (vf it) it) (p it)])) items)]
                     (fn [o] (get by (:value o)))))
                 :value)
        ord  (fn [os] (cond
                        (= :desc dir)               (vec (sort-by key* #(tbl/cmp-vals %2 %1) os))
                        (or (= :asc dir) sort-prop) (vec (sort-by key* tbl/cmp-vals os))
                        :else                       os))]
    (->> items
         (map (fn [it]
                (cond
                  (not (map? it))                       {:value it :label (fmt/auto it ctx)}
                  (or item-value item-label)            (let [val (vf it)]
                                                          {:value  val
                                                           :label
                                                           (or (some-> (lf it) str not-empty)
                                                               (fmt/auto val ctx))
                                                           :search (some-> sf (as-> f (f it)))
                                                           :match  (when mf (mf it))})
                  :else                                 (merge
                                                         (select-keys it [:disabled? :on-select
                                                                          :group :title])
                                                         {:value (:value it)
                                                          :label
                                                          (or (:label it)
                                                              (fmt/auto (:value it) ctx))}))))
         (remove (fn [{:keys [value]}] (nil? value)))
         (reduce (fn [[seen acc] {:keys [value] :as o}]
                   (if (contains? seen value) [seen acc] [(conj seen value) (conj acc o)]))
                 [#{} []])
         second
         ord)))

(defn- dec->hms
  [x]
  (when-let [n (fmt/calc x)]
    (let [secs (Math/round (* (double n) 3600.0))
          two  (fn [v] (str (when (< v 10) "0") v))]
      (str (two (quot secs 3600)) ":" (two (quot (mod secs 3600) 60)) ":"
           (two (mod secs 60))))))

(defn- hms->dec
  [s]
  (let [ps (map #(js/parseInt % 10) (str/split (str s) #":"))
        [h m sec] (concat ps (repeat 0))]
    (when (and (number? h) (not (js/isNaN h)))
      (/ (Math/round (* 1e6 (/ (+ (* h 3600) (* (or m 0) 60) (or sec 0)) 3600.0))) 1e6))))

(defn time-input
  [sel {:keys [class]}]
  [input-group
   {:type "time" :step 1 :class class
    :value (or (dec->hms (rd sel)) "")
    :on-change #(let [t (.. % -target -value)]
                  (wr! sel (when-not (str/blank? t) (hms->dec t))))}])

(defn number-input
  [_ _]
  (let [draft (r/atom nil)]
    (fn [sel {:keys [placeholder class step format] lo :min hi :max}]
      (let [ctx      (rctx/current)
            cur      (rd sel)
            [dv dt]  @draft
            typed    (when (and (some? @draft) (= dv cur)) dt)
            unlesbar (and (some? typed) (not (str/blank? typed))
                          (nil? (fmt/input-number typed ctx)))
            {:keys [prefix suffix]} (fmt/affixes format)
            step!    (fn [dir]
                       (let [s (or (fmt/calc step) 1)
                             v (+ (or (fmt/calc (rd sel)) 0) (* dir s))
                             v (cond->> v
                                 (some? lo) (max (fmt/calc lo))
                                 (some? hi) (min (fmt/calc hi)))]
                         (reset! draft nil)
                         (wr! sel v)))]
        (cond->
         [input-group {:class       class
                       :intent      (when unlesbar :danger)
                       :type        "text" :input-mode "decimal"
                       :aria-invalid (when unlesbar "true")
                       :value       (or typed (if (nil? cur)
                                                ""
                                                (or (fmt/input-text cur format ctx) (str cur))))
                       :placeholder placeholder
                       :on-change   #(let [s (.. % -target -value)
                                           v (when-not (str/blank? s) (fmt/input-number s ctx))]
                                       (reset! draft [v s])
                                       (wr! sel v))
                       :on-blur     #(reset! draft nil)
                       :on-key-down #(case (.-key %)
                                       "ArrowUp"   (do (.preventDefault %) (step! 1))
                                       "ArrowDown" (do (.preventDefault %) (step! -1))
                                       nil)}]
          (or prefix suffix)
          (as-> in [:span.capp-numfield
                    (when prefix [:span.capp-numfield__affix prefix])
                    in
                    (when suffix [:span.capp-numfield__affix suffix])]))))))

(defn date-input
  [sel {:keys [class min max]}]
  [iso-date-field {:class     class
                   :min       min
                   :max       max
                   :value     (or (fmt/iso-date (rd sel) (rctx/current)) "")
                   :on-change #(wr! sel (when-not (str/blank? %) %))}])

(defn date-range-input
  [sel sel-to {:keys [class] lo :min hi :max}]
  (let [ctx  (rctx/current)
        day  #(iso->date (fmt/iso-date (rd %) ctx) iso-day)
        iso  #(when % (ui/format-date % iso-day))]
    [ui/date-range-input
     {:class                  class
      :value                  [(day sel) (day sel-to)]
      :date-fns-format        iso-day
      :allow-single-day-range true
      :min-date               (or (iso->date lo iso-day) (js/Date. 1900 0 1))
      :max-date               (or (iso->date hi iso-day) (js/Date. 2100 11 31))
      :invalid-date-message   (i18n/t :ui/capp-date-invalid)
      :out-of-range-message   (i18n/t :ui/capp-date-out-of-range)
      :popover-props          {:portal-class-name portal-class}
      :on-change              (fn [[a b]] (wr! sel (iso a)) (wr! sel-to (iso b)))}]))

(defn- option-tags
  [opts*]
  (for [[g run] (map (juxt (comp :group first) identity) (partition-by :group opts*))
        :let [os (for [{:keys [value label disabled? title]} run]
                   [:option {:key (str value) :value (str value) :disabled (boolean disabled?)
                             :title title}
                    label])]
        x    (if g [(into [:optgroup {:key (str "g:" g) :label g}] os)] os)]
    x))

(defn select-input
  [sel {:keys [items placeholder class strict? disabled? clearable? on-change] :as opts}]
  (let [opts*  (option-list items opts)
        by-str (into {} (map (fn [{:keys [value]}] [(str value) value])) opts*)
        cur    (if (contains? opts :value) (:value opts) (rd sel))]
    (if (and (empty? opts*) (not strict?))
      [text-input sel {:placeholder placeholder :class class}]
      (into [html-select
             (merge (select-keys opts [:fill :minimal :auto-focus :title :on-focus :on-blur])
                    {:class     class
                     :disabled  (boolean disabled?)
                     :value     (str (if (nil? cur) "" cur))
                     :on-change #(let [v (get by-str (.. % -target -value))]
                                   (wr! sel v)
                                   (when on-change (on-change v {})))})
             [:option {:key "__none" :value "" :disabled (false? clearable?)}
              (or placeholder (i18n/t :ui/select-an-option))]]
            (option-tags opts*)))))

(defn- same-choice? [a b] (and (some? a) (= (str a) (str b))))

(defn- next-choice
  [cur v segmented? required?]
  (if (and segmented? (not required?) (same-choice? cur v)) nil v))

(defn- choice-layout
  [display layout]
  (->kw (or layout (if (= :segmented display) :horizontal :vertical))))

(defn choice-input
  [_sel _opts]
  (let [group (str (gensym "capp-choice-"))]
    (fn [sel {:keys [items display layout columns selection-required? disabled? class
                     on-change] :as opts}]
      (let [display (->kw display)
            opts*   (option-list items opts)
            cur     (if (contains? opts :value) (:value opts) (rd sel))
            seg?    (= :segmented display)
            lay     (choice-layout display layout)
            pick!   (fn [o]
                      (let [v (next-choice cur (:value o) seg? selection-required?)]
                        (when-not (same-choice? cur v)
                          (wr! sel v)
                          (when on-change (on-change v o)))))
            off?    (fn [o] (boolean (or disabled? (:disabled? o))))]
        (if seg?
          [segmented-control
           {:class           (cond-> "capp-segmented" class (str " " class))
            :inline          true
            :small           true
            :value           (or (some #(when (same-choice? cur (:value %)) (str (:value %)))
                                       opts*)
                                 "")
            :options         (mapv (fn [o] {:label    (:label o)
                                            :value    (str (:value o))
                                            :disabled (off? o)})
                                   opts*)
            :on-value-change (fn [v _]
                               (some->> opts* (filter #(= v (str (:value %)))) first pick!))}]
          (into [:div {:class (cond-> (str "capp-choice capp-choice--" (name lay))
                                class (str " " class))
                       :role  "radiogroup"
                       :style (when (= :grid lay)
                                {:grid-template-columns
                                 (str "repeat(" (or columns 2) ", minmax(0, 1fr))")})}]
                (map (fn [o]
                       ^{:key (str (:value o))}
                       [radio {:class     (cond-> "capp-choice__opt"
                                            (off? o) (str " capp-choice__opt--off"))
                               :name      group
                               :checked   (same-choice? cur (:value o))
                               :disabled  (off? o)
                               :label     [:span (:label o)]
                               :on-change #(pick! o)}]))
                opts*))))))

(defn combo-input
  [_sel _opts]
  (let [typed  (r/atom nil)
        edited (r/atom false)]
    (committing
     "capp-combo"
     (fn [propose! sel {:keys [items placeholder class limit strict? on-search] :as opts}]
       (let [opts*  (option-list items opts)
             exact  (into {} (map (fn [{:keys [value label]}] [label value])) opts*)
             folded (->> opts* (group-by #(str/lower-case (str (:label %))))
                         (keep (fn [[k os]] (when (= 1 (count (distinct (map :value os))))
                                              [k (:value (first os))])))
                         (into {}))
             pick   (fn [k]
                      (if (contains? exact k) (get exact k) (get folded (str/lower-case (str k)))))
             by-val (into (into {} (keep (fn [{:keys [match label]}]
                                           (when (some? match) [match label])))
                                opts*)
                          (map (fn [{:keys [value label]}] [value label])) opts*)
             raw    (rd sel)
             [tv tt] @typed
             cur    (if (and strict? (some? @typed) (= tv raw))
                      tt
                      (str (or (get by-val raw) raw "")))
             terms  (when @edited (rt/tokens cur))
             shown  (->> opts*
                         (filter (fn [{:keys [label search]}]
                                   (let [doc (rt/tokens (str label " " search))]
                                     (every? (fn [t] (some #(str/starts-with? % t) doc))
                                             terms))))
                         (take (or limit csub/option-visible-max)))]
         (when (and strict? (some? @typed) (nil? tv))
           (when-some [v (pick tt)]
             (propose! #(= @typed [nil tt]) #(do (reset! typed [v tt]) (wr! sel v)))))
         (let [choose! (fn [{:keys [value label]}]
                         (reset! typed [value label])
                         (reset! edited false)
                         (wr! sel value))]
           [ui/suggest
            (merge combo
                   {:input-props        {:input-class-name class
                                         :placeholder      placeholder
                                         :on-blur          #(reset! edited false)}
                    :query              cur
                    :on-query-change    (fn [s _]
                                          (reset! edited true)
                                          (when on-search (on-search s))
                                          (if strict?
                                            (let [v (pick s)]
                                              (reset! typed [v s])
                                              (wr! sel v))
                                            (wr! sel (if (contains? exact s) (get exact s) s))))
                    :items              (vec shown)
                    :items-equal        :value
                    :item-renderer      (combo-option :label)
                    :on-item-select     (fn [o _] (choose! o))})]))))))

(defn checkbox
  [sel {:keys [label class]}]
  [ui/checkbox {:class     (cond-> "capp-check" class (str " " class))
                :checked   (boolean (rd sel))
                :label     (when label [:span.capp-check__label label])
                :on-change #(wr! sel (.. % -target -checked))}])
