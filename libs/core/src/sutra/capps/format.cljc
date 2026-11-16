(ns sutra.capps.format
  (:refer-clojure :exclude [num])
  (:require [clojure.string :as str]
            [sutra.capps.ctx :as ctx]
            [sutra.types.decimal :as d]))

(defn v
  [o]
  (if (contains? o :metrics)
    (merge (:group o) (:metrics o))
    (:values o)))

(defn links
  [data link-type]
  (vec (get data link-type)))

(defn link-index
  [data link-type]
  (persistent! (reduce (fn [m {:keys [from to]}] (if from (assoc! m from to) m))
                       (transient {}) (links data link-type))))

(defn num
  [x]
  (cond
    (d/exact? x) x
    (number? x)  x
    (and (string? x) (not (str/blank? x)))
    (some-> (re-find #"^\s*([+-]?(?:\d+(?:[.]\d*)?|[.]\d+)(?:[eE][+-]?\d+)?)\s*$"
                     (str/replace x "," "."))
            second
            d/carry)
    :else nil))

(defn num-text
  [x]
  (let [t (-> (str x) (str/replace #"[\s\u00a0\u202f'\u2019]" ""))
        t (if (< 1 (count (re-seq #"," t))) (str/replace t "," "") t)
        t (cond
            (str/includes? t ",")                      (-> t (str/replace "." "")
                                                           (str/replace "," "."))
            (re-matches #"[+-]?\d{1,3}(?:\.\d{3})+" t) (str/replace t "." "")
            :else                                      t)]
    (when (re-matches #"[+-]?(?:\d+(?:\.\d*)?|\.\d+)" t)
      (d/carry t))))

(defn calc
  [x]
  (d/host-number (num x)))

(defn temporal
  [x]
  (cond
    (nil? x)     nil
    (number? x)  nil
    (d/exact? x) nil
    #?@(:cljs [(instance? js/Date x) (.getTime ^js x)]
        :clj  [(instance? java.util.Date x) (.getTime ^java.util.Date x)
               (instance? java.time.LocalDate x)
               (-> ^java.time.LocalDate x (.atStartOfDay java.time.ZoneOffset/UTC)
                   .toInstant .toEpochMilli)
               (instance? java.time.Instant x) (.toEpochMilli ^java.time.Instant x)])
    (string? x)
    (let [s (str/trim x)]
      (when (and (>= (count s) 6)
                 (re-find #"[-/:A-Za-z]" s)
                 (not (re-matches #"[+-]?\d+([.,]\d+)?" s)))
        (let [s #?(:cljs (str/replace s #"\s+[A-Z]{2,5}\s+(\d{4})$" " $1")
                   :clj  (str/replace s #"\s+[A-Z]{2,5}\s+(\d{4})$" " $1"))]
          #?(:cljs (if-let [[_ y m d] (re-matches #"(\d{4})-(\d{2})-(\d{2})" s)]
                     (js/Date.UTC (js/parseInt y 10) (dec (js/parseInt m 10)) (js/parseInt d 10))
                     (let [t (.parse js/Date s)] (when-not (js/isNaN t) t)))
             :clj  (letfn [(try-fmt [p]
                             (try (-> (java.time.format.DateTimeFormatter/ofPattern
                                       p java.util.Locale/ENGLISH)
                                      (.parse s)
                                      (java.time.LocalDate/from)
                                      (.atStartOfDay java.time.ZoneOffset/UTC)
                                      .toInstant .toEpochMilli)
                                  (catch java.time.DateTimeException _ nil)))]
                     (or (try (-> (java.time.LocalDate/parse s)
                                  (.atStartOfDay java.time.ZoneOffset/UTC)
                                  .toInstant .toEpochMilli)
                              (catch java.time.DateTimeException _ nil))
                         (try (.toEpochMilli (java.time.Instant/parse s))
                              (catch java.time.DateTimeException _ nil))
                         (try-fmt "EEE MMM dd HH:mm:ss yyyy")
                         (try-fmt "yyyy-MM-dd HH:mm:ss")
                         (try-fmt "d.M.yyyy")))))))
    :else nil))

(defn compare-operands
  [a b]
  (let [ta (temporal a) tb (temporal b)]
    (if (and ta tb)
      [ta tb]
      (let [na (calc a) nb (calc b)] (when (and na nb) [na nb])))))

(defn abs*
  [n]
  (if (d/exact? n)
    (d/exact (d/magnitude n))
    #?(:cljs (js/Math.abs n) :clj (Math/abs (double n)))))

(defn- log10
  [n]
  #?(:cljs (js/Math.log10 n) :clj (Math/log10 (double n))))

(defn- pow10
  [e]
  #?(:cljs (js/Math.pow 10 e) :clj (Math/pow 10.0 (double e))))

(def float-noise
  1e-9)

(defn snap
  [n]
  (cond (nil? n)                              nil
        (nil? (d/plain n))                    n
        (neg? (d/cmp-magnitude n float-noise)) 0
        :else                                 n))

(def ^:private l10n
  {:de {:group "." :decimal "," :hour12? false :ja "Ja" :nein "Nein"
        :months-long  ["Jänner" "Februar" "März" "April" "Mai" "Juni"
                       "Juli" "August" "September" "Oktober" "November" "Dezember"]
        :months-short ["Jän." "Feb." "März" "Apr." "Mai" "Juni"
                       "Juli" "Aug." "Sep." "Okt." "Nov." "Dez."]
        :days-short   ["So." "Mo." "Di." "Mi." "Do." "Fr." "Sa."]
        :compact      ["" " Tsd." " Mio." " Mrd." " Bio."]
        :now "gerade eben" :ago "vor %n %u" :in "in %n %u"
        :units {:minute ["Minute" "Minuten"] :hour ["Stunde" "Stunden"]}
        :brief-units {:minute ["Min." "Min."]}
        :shape "Form" :corners ["Ecke" "Ecken"]}
   :en {:group "," :decimal "." :hour12? true :ja "Yes" :nein "No"
        :months-long  ["January" "February" "March" "April" "May" "June"
                       "July" "August" "September" "October" "November" "December"]
        :months-short ["Jan" "Feb" "Mar" "Apr" "May" "Jun"
                       "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"]
        :days-short   ["Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"]
        :compact      ["" "K" "M" "B" "T"]
        :now "just now" :ago "%n %u ago" :in "in %n %u"
        :units {:minute ["minute" "minutes"] :hour ["hour" "hours"]}
        :brief-units {:minute ["min" "min"]}
        :shape "Shape" :corners ["corner" "corners"]}})

(defn- L
  [ctx]
  (l10n (:locale ctx)))

(defn- to-fixed
  [n dec]
  (d/round-half-up (d/magnitude n) dec))

(defn fmt
  [x dec ctx]
  (let [n (snap (num x))
        p (d/plain n)]
    (if (nil? p) "–"
        (let [{:keys [group decimal]} (L ctx)
              neg   (str/starts-with? p "-")
              s     (to-fixed n dec)
              [i f] (str/split s #"\.")
              gi    (str/replace i #"\B(?=(\d{3})+(?!\d))" group)]
          (str (when neg "-") gi (when (and f (pos? (count f))) (str decimal f)))))))

(def sep
  \u00a0)

(defn eur [x ctx] (str (fmt x 2 ctx) sep "€"))
(defn eur0 [x ctx] (str (fmt x 0 ctx) sep "€"))
(defn eur1 [x ctx] (str (fmt x 1 ctx) sep "€"))

(defn prozent
  ([x ctx] (prozent x 1 ctx))
  ([x dec ctx] (str (fmt x dec ctx) sep "%")))

(defn menge
  [x ctx]
  (if-let [n (snap (num x))]
    (let [dp (:decimal (L ctx))
          s  (fmt n 3 ctx)]
      (if (str/includes? s dp)
        (let [t (str/replace s #"0+$" "")]
          (cond-> t (str/ends-with? t dp) (subs 0 (dec (count t)))))
        s))
    (if (nil? (num x)) (some-> x str) "0")))

(defn geoshape
  [x ctx]
  (when (some? x)
    (let [s   #?(:cljs (if (object? x) (js/JSON.stringify x) (str x)) :clj (str x))
          pts (re-seq #"\[\s*(-?[\d.eE+-]+)[\s,]+(-?[\d.eE+-]+)\s*\]" s)
          n   (cond-> (count pts)
                (and (< 1 (count pts)) (= (first pts) (last pts))) dec)
          {:keys [shape corners]} (or (L ctx) (l10n :de))]
      (if (pos? n)
        (str shape " · " n " " (corners (if (= 1 n) 0 1)))
        shape))))

(defn auto
  [x ctx]
  (cond
    (nil? x)      nil
    (vector? x)   (if (or (keyword? (first x)) (some coll? x))
                    x
                    (str/join ", " (keep #(auto % ctx) x)))
    (map? x)      x
    (fn? x)       x
    (d/exact? x)  (let [n (snap x)]
                    (if (d/integral? n) (d/plain-integral n) (menge n ctx)))
    (number? x)   (let [n (snap x)
                        t #?(:cljs (js/Math.trunc n) :clj (double (long n)))]
                    (if (and (== n t) (< (abs* n) 1e15))
                      (str #?(:cljs (js/Math.trunc n) :clj (long n)))
                      (menge n ctx)))
    (boolean? x)  (if x (:ja (L ctx)) (:nein (L ctx)))
    :else         (str x)))

(defn- ->int
  [s]
  #?(:cljs (js/parseInt s 10) :clj (Integer/parseInt (str s))))

(defn- pad
  [width n]
  (let [s (str n)]
    (str (apply str (repeat (max 0 (- width (count s))) "0")) s)))

(def ^:private plain-date-re
  #"^\d{4}-\d{2}-\d{2}$")

(def ^:private dotted-date-re
  #"^(\d{1,2})\.(\d{1,2})\.(\d{4})$")

(defn- plain-date-parts
  [x]
  (cond
    (and (string? x) (re-matches plain-date-re (str/trim x)))
    (let [[y m d] (mapv ->int (str/split (str/trim x) #"-"))]
      {:y y :m m :d d :hh 0 :mi 0 :ss 0})

    (string? x)
    (when-let [[_ d m y] (re-matches dotted-date-re (str/trim x))]
      {:y (->int y) :m (->int m) :d (->int d) :hh 0 :mi 0 :ss 0})
    #?@(:clj [(instance? java.time.LocalDate x)
              {:y (.getYear ^java.time.LocalDate x)
               :m (.getMonthValue ^java.time.LocalDate x)
               :d (.getDayOfMonth ^java.time.LocalDate x)
               :hh 0 :mi 0 :ss 0}])
    :else nil))

(defn- zoned-parts
  [ms zone]
  #?(:clj
     (let [zdt (.atZone (java.time.Instant/ofEpochMilli (long ms))
                        (java.time.ZoneId/of zone))]
       {:y (.getYear zdt) :m (.getMonthValue zdt) :d (.getDayOfMonth zdt)
        :hh (.getHour zdt) :mi (.getMinute zdt) :ss (.getSecond zdt)})
     :cljs
     (let [o (js-obj "year" "numeric" "month" "2-digit" "day" "2-digit"
                     "hour" "2-digit" "minute" "2-digit" "second" "2-digit"
                     "hourCycle" "h23" "timeZone" zone)
           g (reduce (fn [m p] (assoc m (keyword (.-type p)) (.-value p)))
                     {}
                     (array-seq (.formatToParts (js/Intl.DateTimeFormat. "en-US" o)
                                                (js/Date. ms))))]
       {:y (->int (:year g)) :m (->int (:month g)) :d (->int (:day g))
        :hh (mod (->int (:hour g)) 24) :mi (->int (:minute g)) :ss (->int (:second g))})))

(def ^:private month-offsets
  [0 3 2 5 0 3 5 1 4 6 2 4])

(defn- weekday
  [{:keys [y m d]}]
  (let [y (if (< m 3) (dec y) y)]
    (mod (+ y (quot y 4) (- (quot y 100)) (quot y 400) (nth month-offsets (dec m)) d) 7)))

(defn- clock
  [ctx {:keys [hh mi ss]} seconds?]
  (let [h12? (:hour12? (L ctx))
        h    (if h12? (let [h (mod hh 12)] (if (zero? h) 12 h)) hh)]
    (str (if h12? h (pad 2 h)) ":" (pad 2 mi)
         (when seconds? (str ":" (pad 2 ss)))
         (when h12? (if (< hh 12) " AM" " PM")))))

(defn- assemble
  [ctx style {:keys [y m d] :as p}]
  (let [{:keys [months-long months-short days-short]} (L ctx)
        de? (= (:months-long (l10n :de)) months-long)
        wd  (nth days-short (weekday p))
        ml  (nth months-long (dec m))
        mn  (nth months-short (dec m))]
    (case style
      :numeric        (if de? (str (pad 2 d) "." (pad 2 m) "." y) (str m "/" d "/" y))
      :date           (if de? (str wd ", " d ". " ml " " y)
                          (str wd ", " mn " " d ", " y))
      :datetime-long  (if de? (str wd ", " d ". " ml " " y ", " (clock ctx p true))
                          (str wd ", " ml " " d ", " y ", " (clock ctx p true)))
      :datetime-short (if de? (str (pad 2 d) "." (pad 2 m) "." y ", " (clock ctx p false))
                          (str mn " " d ", " y ", " (clock ctx p false)))
      :weekday-short  (str wd ", " (assemble ctx :datetime-short p))
      :time           (clock ctx p false)
      (:date-only :iso-instant) (str y "-" (pad 2 m) "-" (pad 2 d))
      (assemble ctx :numeric p))))

(defn- relative
  [ctx ms now brief?]
  (let [t     (L ctx)
        diff  (- now ms)
        past? (pos? diff)
        secs  (quot (if past? diff (- diff)) 1000)]
    (when (< secs 86400)
      (if (< secs 60)
        (:now t)
        (let [[unit n]   (if (< secs 3600) [:minute (quot secs 60)] [:hour (quot secs 3600)])
              [one many] (or (when brief? (get (:brief-units t) unit)) (get (:units t) unit))]
          (-> (if past? (:ago t) (:in t))
              (str/replace "%n" (str n))
              (str/replace "%u" (if (= 1 n) one many))))))))

(def date-styles
  #{:date :datetime :datetime-long :datetime-short :iso-instant :relative :time})

(defn- date-style
  [k]
  (case k :datetime :datetime-short k))

(defn temporal-text
  ([x ctx] (temporal-text x nil ctx))
  ([x {:keys [style timezone brief?]} ctx]
   (let [style (or style :numeric)
         z     (or (not-empty (str timezone)) (:zone ctx))]
     (if-let [p (plain-date-parts x)]
       (assemble ctx (if (#{:time :relative :iso-instant} style) :date-only style) p)
       (when-let [ms (temporal x)]
         (case style
           :iso-instant (let [{:keys [y m d hh mi ss]} (zoned-parts ms ctx/utc)]
                          (str y "-" (pad 2 m) "-" (pad 2 d) "T" (pad 2 hh) ":" (pad 2 mi)
                               ":" (pad 2 ss) "." (pad 3 (mod ms 1000)) "Z"))
           :relative    (or (when-let [n (:now ctx)] (relative ctx ms n brief?))
                            (assemble ctx :weekday-short (zoned-parts ms z)))
           (assemble ctx style (zoned-parts ms z))))))))

(defn datum
  [x ctx]
  (when (some? x)
    (or (temporal-text x {:style :numeric} ctx) (str x))))

(defn iso-date
  [x ctx]
  (when (some? x)
    (when-let [{:keys [y m d]} (or (plain-date-parts x)
                                   (some-> (temporal x) (zoned-parts (:zone ctx))))]
      (str y "-" (pad 2 m) "-" (pad 2 d)))))

(def monate
  (:months-long (l10n :de)))

(defn dezimalstunde
  [x]
  (when-let [n (calc x)]
    (let [secs (Math/round (* (double n) 3600.0))
          h    (quot secs 3600)
          m    (quot (mod secs 3600) 60)]
      (str (when (< h 10) "0") h ":" (when (< m 10) "0") m))))

(defn monat-name
  [m ctx]
  (let [n (calc m)]
    (when (and n (<= 1 n 12))
      (nth (:months-long (L ctx)) (dec (int n))))))

(def ^:private currency-symbols {"EUR" "€" "USD" "$" "GBP" "£" "CHF" "CHF"})

(defn- digits
  [n dec grouping ctx]
  (let [s (fmt n dec ctx)]
    (if (false? grouping) (str/replace s (:group (L ctx)) "") s)))

(defn- compact
  [x places grouping ctx]
  (if-let [n (snap (num x))]
    (let [m     (d/host-number (abs* n))
          tiers (:compact (L ctx))
          top   (dec (count tiers))
          tier  (if (< m 1000) 0 (min top (int (/ (log10 m) 3))))
          scaled (/ (d/host-number n) (pow10 (* 3 tier)))]
      (str (digits scaled
                   (or places (if (and (pos? tier) (< (abs* scaled) 10)) 1 0))
                   grouping ctx)
           (nth tiers tier)))
    (digits x (or places 0) grouping ctx)))

(defn- scientific
  [x places ctx]
  (when-let [n (snap (num x))]
    (let [m (d/host-number n)]
      (if (zero? m)
        (str (fmt 0 (or places 2) ctx) "E0")
        (let [e #?(:cljs (js/Math.floor (log10 (js/Math.abs m)))
                   :clj  (long (Math/floor (log10 (Math/abs (double m))))))]
          (str (fmt (/ m (pow10 e)) (or places 2) ctx) "E" e))))))

(defn- notated
  [x notation places grouping ctx]
  (case notation
    :compact               (compact x places grouping ctx)
    (:scientific
     :engineering)         (scientific x places ctx)
    (digits x (or places 0) grouping ctx)))

(defn- fraction-digits
  [n]
  (let [[_ f] (some-> (d/plain n) (str/split #"\."))]
    (count (str/replace (or f "") #"0+$" ""))))

(defn input-text
  [x {:keys [kind decimals grouping] :as spec} ctx]
  (when-let [n (snap (num x))]
    (let [k    (some-> kind name keyword)
          want (or (some-> (num decimals) calc int)
                   (case k :currency 2 nil)
                   0)
          dec* (max want (fraction-digits n))]
      (digits n dec* (if (seq spec) grouping false) ctx))))

(defn input-number
  [s ctx]
  (if (= "," (:decimal (L ctx)))
    (num-text s)
    (num-text (str/replace (str s) #"[.,]" #(if (= % ".") "," ".")))))

(defn affixes
  [{:keys [kind prefix suffix unit currency]}]
  (let [k (some-> kind name keyword)]
    (not-empty
     (into {} (remove (comp str/blank? val))
           {:prefix (str prefix)
            :suffix (str (case k
                           :currency   (or suffix (get currency-symbols currency currency) "€")
                           :unit       (or unit suffix)
                           :percentage (or suffix "%")
                           suffix))}))))

(defn by-spec
  [{:keys [kind decimals grouping prefix suffix unit currency notation timezone]}]
  (let [k    (if (keyword? kind) kind (when (seq (str kind)) (keyword (str kind))))
        dec* (some-> (num decimals) calc int)
        wrap (fn [s] (str prefix s suffix))
        n    (fn [x d ctx] (if notation
                           (notated x (if (keyword? notation) notation (keyword (str notation)))
                                    dec* grouping ctx)
                           (digits x d grouping ctx)))]
    (if (date-styles k)
      (fn [x ctx] (some-> (temporal-text x {:style (date-style k) :timezone timezone} ctx) wrap))
      (case k
        :currency   (let [d (or dec* 2)
                          sym (or suffix (get currency-symbols currency currency) "€")]
                      (fn [x ctx] (str prefix (n x d ctx) sep sym)))
        :unit       (let [d (or dec* 0)]
                      (fn [x ctx] (str prefix (n x d ctx) (when unit (str sep unit)) suffix)))
        :percentage (let [d (or dec* 1)]
                      (fn [x ctx] (str prefix (n x d ctx) sep (or suffix "%"))))
        (when (or dec* prefix suffix notation)
          (fn [x ctx] (wrap (n x (or dec* 0) ctx))))))))

(defn i18n
  ([dict ctx] (i18n dict ctx ctx/fallback-locale))
  ([dict ctx fallback]
   (let [order (distinct (concat [(:locale ctx) fallback] (keys dict)))]
     (fn [k]
       (or (some #(get-in dict [% k]) order) (name k))))))
