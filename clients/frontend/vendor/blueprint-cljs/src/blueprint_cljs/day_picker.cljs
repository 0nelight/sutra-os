(ns blueprint-cljs.day-picker
  "The calendar grid behind DatePicker and DateRangePicker. Blueprint renders it with
   react-day-picker 8; this builds the same DOM (the `rdp-*` classes Blueprint's datetime
   CSS styles) from ClojureScript: months with captions, weekday header, day buttons with
   modifier classes, keyboard navigation between days.

   Props: `:months` (first-of-month dates shown side by side), `:caption`
   (:dropdown-buttons, rdp's own month/year selects with nav buttons; :blueprint,
   Blueprint's DatePickerCaption), `:from-date` / `:to-date` (bounds for navigation,
   dropdowns and disabled days), `:on-month-change (fn [first-month])`, `:day-mods
   (fn [day] #{:selected …})`, `:class-names {mod class}` for custom modifiers,
   `:disabled (fn [day])`, `:on-day-click` / `:on-day-mouse-enter` / `:on-day-mouse-leave`
   `(fn [day mods event])`, `:locale`, `:first-day-of-week`, `:show-outside-days`,
   `:show-week-number`, `:reverse-month-and-year-menus`, `:class`."
  (:require [reagent.core :as r]
            [blueprint-cljs.core :as bp]
            [blueprint-cljs.date-utils :as du]
            [blueprint-cljs.html-select :refer [html-select]]
            [blueprint-cljs.icon :as icon]))

(def ^:private internal-mods
  #{:outside :disabled :today :selected :range_start :range_end :range_middle :hidden})

(defn- mod-class
  [class-names m]
  (or (get class-names m)
      (when (internal-mods m) (str "rdp-day_" (name m)))))

(defn- weeks
  [month first-dow]
  (let [start (du/month-start month)
        end   (du/month-end month)
        first (du/add-days start (- (mod (- (.getDay start) first-dow) 7)))
        last  (du/add-days end (mod (- (+ first-dow 6) (.getDay end)) 7))
        lk    (du/day-key last)]
    (->> (iterate #(du/add-days % 1) first)
         (take-while #(<= (du/day-key %) lk))
         (partition 7))))

(defn- iso-week
  [^js d]
  (let [t   (js/Date. (.getFullYear d) (.getMonth d) (.getDate d))
        dow (let [x (.getDay t)] (if (zero? x) 7 x))
        _   (.setDate t (+ (.getDate t) (- 4 dow)))
        y0  (js/Date. (.getFullYear t) 0 1)]
    (inc (js/Math.floor (/ (inc (/ (- (du/day-key t) (du/day-key y0)) 86400000)) 7)))))

(defn day-modifiers
  "The active modifiers of `day` shown in `month`."
  [day month {:keys [from-date to-date disabled day-mods]}]
  (let [k (du/day-key day)]
    (cond-> (set (when day-mods (day-mods day)))
      (not (du/same-month? day month))                 (conj :outside)
      (or (and from-date (< k (du/day-key from-date)))
          (and to-date (> k (du/day-key to-date)))
          (and disabled (disabled day)))               (conj :disabled)
      (du/today? day)                                  (conj :today))))

(defn- previous-month
  [first-month from-date]
  (when-not (and from-date (>= (du/month-diff first-month from-date) 0))
    (du/add-months first-month -1)))

(defn- next-month
  [first-month to-date n]
  (when-not (and to-date (< (du/month-diff first-month to-date) n))
    (du/add-months first-month 1)))

;; --------------------------------------------------------------------- captions

(defn- measure-text-width
  [text class container]
  (let [span (js/document.createElement "span")]
    (.add (.-classList span) class)
    (set! (.-textContent span) text)
    (.appendChild container span)
    (let [w (.-offsetWidth span)]
      (.remove span)
      w)))

(defn- month-select
  "Blueprint's month HTMLSelect, its caret moved next to the shown month name
   (`useMonthSelectRightOffset`). `:wrap?` adds rdp's dropdown container div."
  [_]
  (let [offset (r/atom 0)
        sel    (atom nil)
        text   (atom nil)
        measure!
        (fn []
          (when-let [s @sel]
            (when-let [container (some-> s .-parentNode .-parentNode)]
              (let [tw (measure-text-width @text (bp/cls "datepicker-caption-measure")
                                           container)
                    o  (max 2 (- (.-clientWidth s) tw 16 2))]
                (when (not= o @offset) (reset! offset o))))))]
    (r/create-class
     {:display-name         "DatePickerMonthSelect"
      :component-did-mount  measure!
      :component-did-update measure!
      :reagent-render
      (fn [{:keys [wrap? caption-text options] :as props}]
        (reset! text caption-text)
        (let [select [html-select (merge (dissoc props :wrap? :caption-text :options)
                                         {:minimal    true
                                          :ref        #(do (reset! sel %) nil)
                                          :icon-props {:style {:right @offset}}})
                      (for [{:keys [value label disabled]} options]
                        ^{:key value} [:option {:value value :disabled disabled} label])]]
          (if wrap? [:div select] select)))})))

(defn- nav-button
  [dir {:keys [disabled on-click aria-label]}]
  [:button {:type       "button"
            :name       (str dir "-month")
            :aria-label aria-label
            :class      (bp/cx (bp/cls "button") (bp/cls "minimal")
                               (bp/cls "datepicker-nav-button")
                               (bp/cls (str "datepicker-nav-button-" dir))
                               {(bp/cls "disabled") disabled})
            :disabled   disabled
            :on-click   on-click}
   [icon/icon {:icon (if (= dir "previous") :chevron-left :chevron-right)
               :class "rdp-nav_icon"}]])

(defn- year-options
  [from-year to-year locale]
  (for [y (range from-year (inc to-year))]
    {:value y :label (.format (js/Intl.DateTimeFormat. locale #js {:year "numeric"})
                              (js/Date. y 0 1))}))

(defn- caption-label
  [month locale id]
  [:div {:class "rdp-vhidden"}
   [:div {:class "rdp-caption_label" :aria-live "polite" :role "presentation" :id id}
    (du/caption month locale)]])

(defn- rdp-caption
  "react-day-picker's `dropdown-buttons` caption, with Blueprint's dropdown component."
  [{:keys [month index n first-month from-date to-date locale go-to cap-id]}]
  (let [year      (.getFullYear month)
        months    (if (= (.getFullYear from-date) (.getFullYear to-date))
                    (range (.getMonth from-date) (inc (.getMonth to-date)))
                    (range 12))
        go        (fn [d] (go-to (du/add-months d (- index))))
        prev      (previous-month first-month from-date)
        nxt       (next-month first-month to-date n)
        hide-next (and (> n 1) (< index (dec n)))
        hide-prev (and (> n 1) (pos? index))]
    [:div {:class "rdp-caption"}
     [:div {:class "rdp-caption_dropdowns"}
      [caption-label month locale cap-id]
      [month-select
       {:wrap?        true
        :name         "months"
        :aria-label   "Month: "
        :class        (bp/cls "datepicker-month-select")
        :value        (.getMonth month)
        :caption-text (du/month-name month locale)
        :options      (for [m months]
                        {:value m :label (du/month-name (js/Date. year m 1) locale)})
        :on-change    #(go (js/Date. year (js/parseInt (.. % -target -value) 10) 1))}]
      [:div
       [html-select
        {:minimal    true
         :name       "years"
         :aria-label "Year: "
         :class      (bp/cls "datepicker-year-select")
         :value      year
         :options    (year-options (.getFullYear from-date) (.getFullYear to-date) locale)
         :on-change  #(go (js/Date. (js/parseInt (.. % -target -value) 10)
                                    (.getMonth month) 1))}]]]
     [:div {:class "rdp-nav"}
      (when-not hide-prev
        [nav-button "previous" {:disabled   (nil? prev)
                                :aria-label "Go to previous month"
                                :on-click   #(when prev (go-to prev))}])
      (when-not hide-next
        [nav-button "next" {:disabled   (nil? nxt)
                            :aria-label "Go to next month"
                            :on-click   #(when nxt (go-to nxt))}])]]))

(defn- blueprint-caption
  "Blueprint's DatePickerCaption: nav buttons around month and year selects."
  [{:keys [month first-month from-date to-date locale go-to cap-id reverse?]}]
  (let [year      (.getFullYear month)
        min-year  (.getFullYear from-date)
        max-year  (.getFullYear to-date)
        start-m   (if (= year min-year) (.getMonth from-date) 0)
        end-m     (if (= year max-year) (inc (.getMonth to-date)) 12)
        prev      (previous-month first-month from-date)
        nxt       (next-month first-month to-date 1)
        month-sel [month-select
                   {:key          "month"
                    :aria-label   "Month: "
                    :class        (bp/cx (bp/cls "datepicker-month-select") "rdp-dropdown_month")
                    :value        (.getMonth month)
                    :caption-text (du/month-name month locale)
                    :options      (for [m (range start-m end-m)]
                                    {:value m :label (du/month-name (js/Date. year m 1) locale)})
                    :on-change    #(let [v (js/parseInt (.. % -target -value) 10)]
                                     (when-not (js/isNaN v) (go-to (js/Date. year v 1))))}]
        year-sel  [html-select
                   {:key        "year"
                    :minimal    true
                    :aria-label "Year: "
                    :class      (bp/cx (bp/cls "datepicker-year-select") "rdp-dropdown_year")
                    :value      year
                    :options    (cond-> (vec (year-options min-year max-year locale))
                                  (> year max-year)
                                  (conj {:value year :label (str year) :disabled true}))
                    :on-change  #(let [v (js/parseInt (.. % -target -value) 10)]
                                   (when-not (js/isNaN v)
                                     (go-to (js/Date. v (.getMonth month) 1))))}]]
    [:div {:class (bp/cx (bp/cls "datepicker-caption") "rdp-caption")}
     [caption-label month locale cap-id]
     [nav-button "previous" {:disabled   (nil? prev)
                             :aria-label "Go to previous month"
                             :on-click   #(when prev (go-to prev))}]
     (if reverse? year-sel month-sel)
     (if reverse? month-sel year-sel)
     [nav-button "next" {:disabled   (nil? nxt)
                         :aria-label "Go to next month"
                         :on-click   #(when nxt (go-to nxt))}]]))

;; ------------------------------------------------------------------------- grid

(defn- move-day
  [^js d key shift? first-dow]
  (case key
    "ArrowLeft"  (du/add-days d -1)
    "ArrowRight" (du/add-days d 1)
    "ArrowUp"    (du/add-days d -7)
    "ArrowDown"  (du/add-days d 7)
    ("PageUp" "PageDown")
    (let [n  (* (if (= key "PageUp") -1 1) (if shift? 12 1))
          m1 (du/add-months d n)]
      (js/Date. (.getFullYear m1) (.getMonth m1)
                (min (.getDate d) (.getDate (du/month-end m1)))))
    "Home" (du/add-days d (- (mod (- (.getDay d) first-dow) 7)))
    "End"  (du/add-days d (- 6 (mod (- (.getDay d) first-dow) 7)))
    nil))

(defn- focus-target
  "rdp's initial focus: a selected day, else today, else the first enabled day shown."
  [months opts]
  (let [days (for [m months
                   w (weeks m (:first-dow opts))
                   d w
                   :let [mods (day-modifiers d m opts)]
                   :when (not (or (mods :outside) (mods :disabled)))]
               [d mods])]
    (or (some (fn [[d mods]] (when (mods :selected) d)) days)
        (some (fn [[d mods]] (when (mods :today) d)) days)
        (ffirst days))))

(defn day-picker
  [_]
  (let [focused  (r/atom nil)
        pending  (atom false)
        root     (atom nil)
        cur      (atom nil)
        uid      (gensym "rdp-")
        focus-pending!
        (fn []
          (when (and @pending @root @focused)
            (when-let [b (.querySelector @root (str "button[data-day=\"" (du/day-key @focused)
                                                    "\"]:not([data-outside])"))]
              (reset! pending false)
              (.focus b))))
        on-key-down
        (fn [day e]
          (let [{:keys [months from-date to-date on-month-change] :as p} @cur
                key (.-key e)]
            (when-let [first-try (move-day day key (.-shiftKey e) (:first-dow p))]
              (.preventDefault e)
              (let [step (fn [d] (move-day d key (.-shiftKey e) (:first-dow p)))
                    ok?  (fn [d] (not ((day-modifiers d (du/month-start d) p) :disabled)))
                    nd   (loop [d first-try i 0]
                           (cond (ok? d) d
                                 (or (> i 365) (contains? #{"Home" "End"} key)) d
                                 :else (recur (step d) (inc i))))
                    nd   (du/clamp-date nd from-date to-date)
                    fm   (first months)
                    lm   (last months)]
                (cond
                  (< (du/month-diff fm nd) 0)
                  (on-month-change (du/month-start nd))
                  (> (du/month-diff lm nd) 0)
                  (on-month-change (du/add-months nd (- (dec (count months))))))
                (reset! focused nd)
                (reset! pending true)
                (focus-pending!)))))]
    (r/create-class
     {:display-name         "DayPicker"
      :component-did-update focus-pending!
      :reagent-render
      (fn [{:keys [months caption locale first-day-of-week show-outside-days show-week-number
                   reverse-month-and-year-menus class class-names on-day-click
                   on-day-mouse-enter on-day-mouse-leave on-month-change from-date to-date]
            :or   {caption :dropdown-buttons show-outside-days true}
            :as   props}]
        (let [locale (or locale du/default-locale)
              fdow   (or first-day-of-week (du/first-day-of-week locale))
              p      (assoc props :first-dow fdow)
              _      (reset! cur p)
              n      (count months)
              target (let [f @focused]
                       (if (and f (some #(du/same-month? f %) months))
                         f
                         (focus-target months p)))
              header (take 7 (first (weeks (first months) fdow)))]
          [:div {:class (bp/cx "rdp" {"rdp-multiple_months" (> n 1)
                                      "rdp-with_weeknumber" show-week-number}
                               class)
                 :ref   #(do (reset! root %) nil)}
           [:div {:class "rdp-months"}
            (for [[i month] (map-indexed vector months)
                  :let [cap-id (str uid "-" i)
                        ctx    {:month month :index i :n n :first-month (first months)
                                :from-date from-date :to-date to-date :locale locale
                                :go-to on-month-change :cap-id cap-id
                                :reverse? reverse-month-and-year-menus}]]
              ^{:key i}
              [:div {:class (bp/cx "rdp-month" {"rdp-caption_start"   (zero? i)
                                                "rdp-caption_end"     (= i (dec n))
                                                "rdp-caption_between" (< 0 i (dec n))})}
               (if (= caption :blueprint) [blueprint-caption ctx] [rdp-caption ctx])
               [:table {:class "rdp-table" :role "grid" :aria-labelledby cap-id}
                [:thead {:class "rdp-head"}
                 [:tr {:class "rdp-head_row"}
                  (when show-week-number [:td {:class "rdp-head_cell"}])
                  (for [d header]
                    ^{:key (.getDay d)}
                    [:th {:scope "col" :class "rdp-head_cell"
                          :aria-label (du/weekday-long d locale)}
                     (du/weekday-short d locale)])]]
                [:tbody {:class "rdp-tbody"}
                 (for [week (weeks month fdow)]
                   ^{:key (du/day-key (first week))}
                   [:tr {:class "rdp-row"}
                    (when show-week-number
                      [:td {:class "rdp-cell"}
                       [:span {:class "rdp-weeknumber"} (iso-week (first week))]])
                    (for [day week
                          :let [mods (day-modifiers day month p)]]
                      ^{:key (du/day-key day)}
                      [:td {:class "rdp-cell" :role "presentation"}
                       (if (and (mods :outside) (not show-outside-days))
                         [:div {:role "gridcell"}]
                         [:button
                          {:type          "button"
                           :name          "day"
                           :role          "gridcell"
                           :data-day      (du/day-key day)
                           :data-outside  (when (mods :outside) true)
                           :aria-label    (du/day-label day locale)
                           :aria-selected (when (mods :selected) true)
                           :tab-index     (if (and target (not (mods :outside))
                                                   (du/same-day? day target))
                                            0 -1)
                           :disabled      (boolean (mods :disabled))
                           :class         (bp/cx (bp/cls "button") (bp/cls "minimal")
                                                 "rdp-day"
                                                 (keep #(mod-class class-names %) mods))
                           :on-click      (fn [e]
                                            (reset! focused day)
                                            (when on-day-click (on-day-click day mods e)))
                           :on-focus      #(reset! focused day)
                           :on-key-down   #(on-key-down day %)
                           :on-mouse-enter (when on-day-mouse-enter
                                             #(on-day-mouse-enter day mods %))
                           :on-mouse-leave (when on-day-mouse-leave
                                             #(on-day-mouse-leave day mods %))}
                          (.getDate day)])])])]]])]]))})))
