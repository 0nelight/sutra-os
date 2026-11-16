(ns blueprint-cljs.timezone-select
  "TimezoneSelect: a Select over the IANA timezones the browser knows
   (`Intl.supportedValuesOf(\"timeZone\")`), each with its UTC offset, abbreviation and
   long name at `:date`, computed with Intl. Without a query it lists Blueprint's short
   list (plus the local timezone with `:show-local-timezone`).
   `[timezone-select {:value \"Europe/Vienna\" :on-change #(reset! tz %)}]`"
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [blueprint-cljs.button :as button]
            [blueprint-cljs.core :as bp]
            [blueprint-cljs.menu :as menu]
            [blueprint-cljs.select :refer [select]]))

(def utc {:label "UTC" :iana-code "Etc/UTC"})

(def minimal-timezones
  "Blueprint `MINIMAL_TIMEZONE_ITEMS`."
  [utc
   {:label "Pago Pago" :iana-code "Pacific/Pago_Pago"}
   {:label "Hawaii Time" :iana-code "Pacific/Honolulu"}
   {:label "Marquesas" :iana-code "Pacific/Marquesas"}
   {:label "Alaska" :iana-code "America/Anchorage"}
   {:label "Los Angeles" :iana-code "America/Los_Angeles"}
   {:label "Denver" :iana-code "America/Denver"}
   {:label "Mexico City" :iana-code "America/Mexico_City"}
   {:label "New York" :iana-code "America/New_York"}
   {:label "Puerto Rico" :iana-code "America/Puerto_Rico"}
   {:label "St. Johns" :iana-code "America/St_Johns"}
   {:label "Buenos Aires" :iana-code "America/Argentina/Buenos_Aires"}
   {:label "Sao Paulo" :iana-code "America/Sao_Paulo"}
   {:label "Cape Verde" :iana-code "Atlantic/Cape_Verde"}
   {:label "Paris" :iana-code "Europe/Paris"}
   {:label "Cairo" :iana-code "Africa/Cairo"}
   {:label "Moscow" :iana-code "Europe/Moscow"}
   {:label "Tehran" :iana-code "Asia/Tehran"}
   {:label "Dubai" :iana-code "Asia/Dubai"}
   {:label "Karachi" :iana-code "Asia/Karachi"}
   {:label "India - Kolkata" :iana-code "Asia/Kolkata"}
   {:label "Kathmandu" :iana-code "Asia/Kathmandu"}
   {:label "Dhaka" :iana-code "Asia/Dhaka"}
   {:label "Rangoon" :iana-code "Asia/Yangon"}
   {:label "Jakarta" :iana-code "Asia/Jakarta"}
   {:label "Manila" :iana-code "Asia/Manila"}
   {:label "Tokyo" :iana-code "Asia/Tokyo"}
   {:label "Brisbane" :iana-code "Australia/Brisbane"}
   {:label "Adelaide" :iana-code "Australia/Adelaide"}
   {:label "Melbourne, Sydney" :iana-code "Australia/Sydney"}
   {:label "Nauru" :iana-code "Pacific/Nauru"}
   {:label "Auckland" :iana-code "Pacific/Auckland"}
   {:label "Kiritimati" :iana-code "Pacific/Kiritimati"}])

(def ^:private known-labels
  (into {} (map (juxt :iana-code :label)) minimal-timezones))

(defn current-timezone
  []
  (.. (js/Intl.DateTimeFormat.) resolvedOptions -timeZone))

(defn valid-timezone?
  [tz]
  (try (js/Intl.DateTimeFormat. "en-US" #js {:timeZone tz}) true
       (catch :default _ false)))

(defn- tz-name
  [tz style date]
  (let [parts (.formatToParts (js/Intl.DateTimeFormat. "en-US" #js {:timeZone tz
                                                                    :timeZoneName style})
                              date)]
    (some #(when (= "timeZoneName" (.-type %)) (.-value %)) (array-seq parts))))

(defn- offset-minutes
  [tz date]
  (let [[_ sign h m] (re-find #"GMT([+-])(\d{2}):(\d{2})" (or (tz-name tz "longOffset" date) ""))]
    (if sign
      (* (if (= sign "-") -1 1) (+ (* 60 (js/parseInt h 10)) (js/parseInt m 10)))
      0)))

(defn- format-offset
  [mins]
  (str (if (neg? mins) "-" "+")
       (.padStart (str (quot (js/Math.abs mins) 60)) 2 "0") ":"
       (.padEnd (str (mod (js/Math.abs mins) 60)) 2 "0")))

(defn- label-of
  [code]
  (or (known-labels code)
      (str/replace (last (str/split code #"/")) "_" " ")))

(defn with-names
  "`tz` ({:iana-code :label}) with `:offset`, `:offset-minutes`, `:short-name` (date-fns
   `zzz`) and `:long-name` (`zzzz`) at `date`."
  [{:keys [iana-code] :as tz} date]
  (let [mins (offset-minutes iana-code date)]
    (assoc tz
           :offset         (format-offset mins)
           :offset-minutes mins
           :short-name     (tz-name iana-code "short" date)
           :long-name      (tz-name iana-code "long" date))))

(defn all-timezones
  "Every timezone of `Intl.supportedValuesOf`, UTC first, then by offset and label."
  [date]
  (let [codes (if (exists? js/Intl.supportedValuesOf)
                (js->clj (js/Intl.supportedValuesOf "timeZone"))
                (map :iana-code minimal-timezones))
        items (for [c codes :when (not= c "UTC")]
                (with-names {:label (label-of c) :iana-code c} date))]
    (into [(with-names utc date)]
          (sort-by (juxt :offset-minutes :label) items))))

(defn initial-timezones
  "Blueprint `getInitialTimezoneItems`."
  [date show-local?]
  (let [local (when show-local?
                (let [c (current-timezone)]
                  (when (valid-timezone? c)
                    (assoc (with-names {:label (label-of c) :iana-code c} date)
                           :long-name "Current timezone"))))]
    (into (if local [local] [])
          (comp (remove #(= (:iana-code %) (:iana-code local)))
                (map #(with-names % date)))
          minimal-timezones)))

(defn format-timezone
  "Blueprint `formatTimezone`: `fmt` is :abbreviation, :code, :composite, :long-name (or
   :name) or :offset."
  [{:keys [short-name label offset iana-code long-name] :as tz} fmt]
  (when tz
    (case (keyword fmt)
      :abbreviation       short-name
      :offset             offset
      :code               iana-code
      (:long-name :name)  long-name
      :composite          (if (or (re-find #"[-+]" (or short-name "")) (= short-name label))
                            (str label " " offset)
                            (str label " (" short-name ") " offset))
      nil)))

(defn- query-regex
  [query]
  (js/RegExp. (-> query
                  (.replace (js/RegExp. "([[()+*?])" "g") "\\$1")
                  (.replace " " "[ _/\\(\\)]+"))
              "i"))

(defn filter-timezones
  "Blueprint's predicate (code, label, long or short name), plus the offset."
  [query items]
  (let [re (try (query-regex query) (catch :default _ nil))]
    (if-not re
      []
      (filterv (fn [{:keys [iana-code label long-name short-name offset]}]
                 (some #(and % (.test re %)) [iana-code label long-name short-name offset]))
               items))))

(defn- render-item
  [tz {:keys [handle-click handle-focus modifiers ref id]}]
  (when (:matches-predicate modifiers)
    ^{:key (:iana-code tz)}
    [menu/menu-item {:selected (:active modifiers)
                     :text     (str (:label tz) ", " (:long-name tz))
                     :label    (:short-name tz)
                     :on-click handle-click
                     :on-focus handle-focus
                     :ref      ref
                     :id       id}]))

(defn timezone-select
  "Props (Blueprint TimezoneSelect): `:value` (IANA code), `:on-change (fn [code])`,
   `:date` (for offsets and names; default now), `:value-display-format` (:composite
   default, :abbreviation, :offset, :code, :long-name/:name), `:show-local-timezone`,
   `:placeholder` (\"Select timezone...\"), `:disabled`, `:fill`, `:button-props`,
   `:input-props`, `:popover-props`, `:class`. Children replace the button."
  [props & _]
  (let [query (r/atom (or (get-in props [:input-props :value]) ""))
        cache (atom {})
        now   (js/Date.)
        items (fn [k date f]
                (let [key [k (.getTime date)]]
                  (or (get @cache key)
                      (let [v (f)] (swap! cache assoc key v) v))))]
    (fn [{:keys [value on-change date disabled fill placeholder button-props input-props
                 popover-props value-display-format show-local-timezone class]
          :or   {placeholder "Select timezone..."}}
         & children]
      (let [date     (or date now)
            all      (items :all date #(all-timezones date))
            initial  (items [:initial (boolean show-local-timezone)] date
                            #(initial-timezones date show-local-timezone))
            selected (some #(when (= (:iana-code %) value) %) all)
            content  (if selected
                       (format-timezone selected (or value-display-format :composite))
                       [:span {:class (bp/cls "text-muted")} placeholder])]
        [select
         {:class                (bp/cx (bp/cls "timezone-select") class)
          :disabled             disabled
          :fill                 fill
          :input-props          (merge {:placeholder "Search for timezones..."} input-props)
          :item-list-predicate  filter-timezones
          :item-renderer        render-item
          :items                (if (seq @query) all initial)
          :items-equal          :iana-code
          :no-results           [menu/menu-item {:disabled       true
                                                 :role-structure :listoption
                                                 :text           "No matching timezones."}]
          :on-item-select       #(when on-change (on-change (:iana-code %)))
          :on-query-change      #(reset! query %)
          :popover-props        (assoc popover-props :popover-class-name
                                       (bp/cx (bp/cls "timezone-select-popover")
                                              (:popover-class-name popover-props)))
          :popover-target-props {:aria-label "timezone"}
          :reset-on-close       true
          :reset-on-select      true}
         (if (seq (remove nil? children))
           (first (remove nil? children))
           [button/button (merge {:end-icon :caret-down :disabled disabled :fill fill
                                  :text content}
                                 button-props)])]))))
