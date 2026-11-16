(ns blueprint-cljs.date-shortcuts
  "The shortcut menu beside DatePicker and DateRangePicker (Blueprint
   `DatePickerShortcutMenu`). A shortcut is `{:label s :date-range [start end]
   :include-time bool}`."
  (:require [blueprint-cljs.core :as bp]
            [blueprint-cljs.date-utils :as du]
            [blueprint-cljs.menu :as menu]))

(defn default-shortcuts
  "Blueprint `createDefaultShortcuts`."
  [allow-single-day? has-time-precision? single-date?]
  (let [today     (js/Date.)
        make      (fn [f] (let [d (du/clone today)] (f d) (.setDate d (inc (.getDate d))) d))
        tomorrow  (make identity)
        yesterday (make #(.setDate % (- (.getDate %) 2)))
        week      (make #(.setDate % (- (.getDate %) 7)))
        month     (make #(.setMonth % (- (.getMonth %) 1)))
        months3   (make #(.setMonth % (- (.getMonth %) 3)))
        months6   (make #(.setMonth % (- (.getMonth %) 6)))
        year      (make #(.setFullYear % (- (.getFullYear %) 1)))
        years2    (make #(.setFullYear % (- (.getFullYear %) 2)))
        sc        (fn [label r] {:label label :date-range r})]
    (vec
     (concat
      (when (or allow-single-day? single-date?)
        [(sc "Today" [today (if has-time-precision? tomorrow today)])
         (sc "Yesterday" [yesterday (if has-time-precision? today yesterday)])])
      [(sc (if single-date? "1 week ago" "Past week") [week today])
       (sc (if single-date? "1 month ago" "Past month") [month today])
       (sc (if single-date? "3 months ago" "Past 3 months") [months3 today])]
      (when-not single-date? [(sc "Past 6 months" [months6 today])])
      [(sc (if single-date? "1 year ago" "Past year") [year today])]
      (when-not single-date? [(sc "Past 2 years" [years2 today])])))))

(defn shortcut-menu
  "Props: `:shortcuts` (true for the defaults, or a seq of shortcuts), `:min-date`,
   `:max-date`, `:allow-single-day-range`, `:time-precision`, `:selected-shortcut-index`,
   `:use-single-date-shortcuts`, `:on-shortcut-click (fn [shortcut index])`."
  [{:keys [shortcuts min-date max-date allow-single-day-range time-precision
           selected-shortcut-index use-single-date-shortcuts on-shortcut-click]
    :or   {selected-shortcut-index -1}}]
  (let [items (if (true? shortcuts)
                (default-shortcuts allow-single-day-range (some? time-precision)
                                   use-single-date-shortcuts)
                shortcuts)]
    (into [menu/menu {:aria-label "Date picker shortcuts"
                      :class      (bp/cls "daterangepicker-shortcuts")
                      :tab-index  0}]
          (map-indexed
           (fn [i {:keys [label date-range] :as sc}]
             ^{:key i}
             [menu/menu-item {:active                 (= i selected-shortcut-index)
                              :disabled               (not (du/day-range-in-range?
                                                            date-range [min-date max-date]))
                              :on-click               #(on-shortcut-click sc i)
                              :should-dismiss-popover false
                              :text                   label}])
           items))))
