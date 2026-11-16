(ns sutra.types.aggregate
  (:require [sutra.kit.anomaly :as anomaly]
            #?(:clj [sutra.kit.time :as time])
            [clojure.string :as str]
            [sutra.types.table :as tbl]))

(def agg-fns
  {:count       {:sql "COUNT" :attr :optional :exact? true
                 :doc "How many objects in the bucket. Without `:attr`,
                       COUNT(*) — the cardinality of the set. With one, how many
                       carry a value for that attribute."}
   :sum         {:sql "SUM" :attr :required :exact? true
                 :doc "Total of a numeric attribute."}
   :avg         {:sql "AVG" :attr :required :exact? true
                 :doc "Mean of a numeric/date attribute."}
   :min         {:sql "MIN" :attr :required :exact? true}
   :max         {:sql "MAX" :attr :required :exact? true}
   :cardinality {:sql "COUNT" :distinct? true :attr :required :exact? true
                 :doc "How many DISTINCT values the attribute takes. this one is exact."}})

(def bucket-units
  {:year    {:sql {:postgres "to_char({}, 'YYYY')"}
             :example "2026"}
   :quarter {:sql {:postgres "to_char({}, 'YYYY-\"Q\"Q')"}
             :example "2026-Q3"}
   :month   {:sql {:postgres "to_char({}, 'YYYY-MM')"}
             :example "2026-08"}
   :week    {:sql {:postgres "to_char({}, 'IYYY-\"W\"IW')"}
             :example "2026-W32"}
   :day     {:sql {:postgres "to_char({}, 'YYYY-MM-DD')"}
             :example "2026-08-05"}
   :hour    {:sql {:postgres "to_char({}, 'YYYY-MM-DD HH24')"}
             :example "2026-08-05 14"}
   :minute  {:sql {:postgres "to_char({}, 'YYYY-MM-DD HH24:MI')"}
             :example "2026-08-05 14:23"}
   :second  {:sql {:postgres "to_char({}, 'YYYY-MM-DD HH24:MI:SS')"}
             :example "2026-08-05 14:23:07"}})

(def max-buckets
  10000)

(defn- term-map
  [t]
  (cond
    (keyword? t) {:kind :attr :attr t}
    (map? t)     (assoc t :kind (cond (:unit t) :bucket (:width t) :width
                                      (:link t) :link :else :attr))
    (vector? t)  (let [[k a x] t]
                   (case k
                     :bucket {:kind :bucket :attr a :unit x}
                     :width  {:kind :width  :attr a :width x}
                     :origin {:kind :origin}
                     :link   (cond-> {:kind :link :link a} (some? x) (assoc :title x))
                     {:kind :unknown :term t}))
    :else        {:kind :unknown :term t}))

(defn- term-name [{:keys [kind attr unit width name link]}]
  (or name
      (case kind
        :attr   attr
        :bucket (keyword (str (clojure.core/name attr) "-" (clojure.core/name unit)))
        :width  (keyword (str (clojure.core/name attr) "-w" width))
        :origin :origin
        :link   (keyword (clojure.core/name link))
        nil)))

(defn- agg-name [{f :fn :keys [attr name]}]
  (or name
      (if attr
        (keyword (str (clojure.core/name attr) "-" (clojure.core/name f)))
        (keyword (clojure.core/name f)))))

(defn- legacy-count? [aggs] (= :count aggs))

(def ^:private alias-letters "abcdefghijklmnopqrstuvwxyz")

(defn- alias-suffix
  [i]
  (loop [n i acc ""]
    (let [acc (str (nth alias-letters (mod n 26)) acc)
          n'  (dec (quot n 26))]
      (if (neg? n') acc (recur n' acc)))))

(defn explain-aggregate
  [{:keys [aggs group-by]}]
  (let [terms (mapv term-map (or group-by []))
        specs (cond (legacy-count? aggs) [{:fn :count}]
                    (sequential? aggs)   (vec aggs)
                    :else                nil)]
    (or
     (when (and (nil? specs) (some? aggs))
       {:problem :bad-aggs :aggs aggs
        :message (str "`:aggs` is `:count` (the bare set-size question) or a "
                      "vector of {:fn … :attr … :name …} — got " (pr-str aggs))})
     (when (and (nil? aggs) (seq terms))
       {:problem :group-by-without-aggs
        :message ":group-by needs :aggs — grouping alone computes nothing"})
     (when (and specs (empty? specs))
       {:problem :empty-aggs :message ":aggs must name at least one metric"})
     (some (fn [spec]
             (let [{f :fn :keys [attr]} (when (map? spec) spec)
                   row (agg-fns f)]
               (cond
                 (not (map? spec))
                 {:problem :bad-agg :agg spec
                  :message "an aggregate is a map {:fn … :attr … :name …}"}

                 (nil? row)
                 (assoc (tbl/unknown :unknown-agg-fn "aggregate function" agg-fns f)
                        :fn f :agg spec)

                 (and (= :required (:attr row)) (nil? attr))
                 {:problem :agg-needs-attr :fn f :agg spec
                  :message (str (pr-str f) " needs an attribute: "
                                "{:fn " (pr-str f) " :attr <attribute>}")}

                 (and (some? attr) (not (keyword? attr)))
                 {:problem :bad-agg-attr :fn f :agg spec
                  :message ":attr is an attribute NAME (a keyword), never a column"})))
           specs)
     (some (fn [{:keys [kind attr unit width link] :as t}]
             (cond
               (= :unknown kind)
               {:problem :bad-group-term :term (:term t)
                :message (str "a :group-by term is an attribute keyword, "
                              "[:bucket <attr> " (pr-str (vec (sort (keys bucket-units))))
                              "], [:width <attr> <n>], [:origin], or "
                              "[:link <link-type>]")}

               (= :origin kind) nil

               (= :link kind)
               (when-not (keyword? link)
                 {:problem :bad-group-term :term t
                  :message "[:link <link-type>] names a link type (a keyword)"})

               (not (keyword? attr))
               {:problem :bad-group-term :term t
                :message ":group-by names an attribute NAME (a keyword)"}

               (and (= :bucket kind) (not (bucket-units unit)))
               (assoc (tbl/unknown :unknown-bucket-unit "bucket unit" bucket-units unit)
                      :unit unit)

               (and (= :width kind) (not (and (number? width) (pos? width))))
               {:problem :bad-bucket-width :width width
                :message "[:width <attr> <n>] needs a positive number"}))
           terms)
     (let [names (into [] (concat (map term-name terms) (map agg-name specs)))
           dupes (into (sorted-set)
                       (keep (fn [[k n]] (when (> n 1) k)))
                       (frequencies names))]
       (when (seq dupes)
         {:problem :duplicate-result-keys :keys dupes
          :message (str "two columns of this aggregate would answer under the "
                        "same key " (pr-str (vec dupes))
                        " — give one an explicit :name")})))))

(defn normalize
  [{:keys [aggs group-by] :as spec}]
  (when (some? aggs)
    (when-let [problem (explain-aggregate spec)]
      (anomaly/coded! :refusal.dialect/malformed-aggregate :incorrect
        "malformed aggregate: {detail}"
        problem {:detail (:message problem)}))
    (let [specs (if (legacy-count? aggs) [{:fn :count}] (vec aggs))]
      {:group-by (into [] (map-indexed (fn [i t]
                                         (let [t (term-map t)]
                                           (cond-> (assoc t :name (term-name t)
                                                          :alias (str "g" (alias-suffix i)))
                                             (= :link (:kind t))
                                             (assoc :title-alias (str "gt" (alias-suffix i)))))))
                       (or group-by []))
       :aggs     (into [] (map-indexed (fn [i a]
                                         (assoc a :name (agg-name a)
                                                :alias (str "a" (alias-suffix i)))))
                       specs)})))

(defn bucket-row [group metrics] {:group group :metrics metrics})

(defn result-keys
  [{:keys [group-by aggs]}]
  (into (mapv :name group-by) (map :name) aggs))

(defn result-key-groups
  [{:keys [group-by aggs]}]
  {:group (mapv :name group-by) :metrics (mapv :name aggs)})

(defn- render [tmpl col] (str/replace tmpl "{}" col))

(defn group-sql
  [dialect {:keys [kind unit width]} col]
  (case kind
    (:attr :origin :link) col
    :bucket (render (or (get-in bucket-units [unit :sql dialect])
                        (anomaly/coded! :refusal.dialect/bucket-unit-spelling :incorrect
                          "no {dialect-name} spelling for bucket unit {unit:pr}"
                          {:unit unit :dialect dialect
                           :known (set (keys (get-in bucket-units [unit :sql])))}
                          {:dialect-name (name dialect)}))
                    col)
    :width  (str "(FLOOR(CAST(" col " AS NUMERIC) / " width ") * " width ")")))

(defn agg-sql
  [{f :fn} col]
  (let [{:keys [sql distinct?]} (agg-fns f)
        e (str sql "(" (if col (str (when distinct? "DISTINCT ") col) "*") ")")]
    (if (= :sum f) (str "COALESCE(" e ", 0)") e)))

(defn- pad [n w]
  (let [s (str n)]
    (str (apply str (repeat (- w (count s)) "0")) s)))

#?(:cljs
   (defn- date-parts
     [v]
     (let [s (if (instance? js/Date v) (.toISOString v) (str/trim (str v)))]
       (when-let [[_ y mo d h mi sec]
                  (re-find #"^(\d{4})-(\d{2})-(\d{2})(?:[T ](\d{2}):(\d{2})(?::(\d{2}))?)?" s)]
         (mapv #(js/parseInt (or % "0") 10) [y mo d h mi sec])))))

#?(:cljs
   (defn- iso-week
     [y mo d]
     (let [t   (js/Date. (js/Date.UTC y (dec mo) d))
           dow (let [x (.getUTCDay t)] (if (zero? x) 7 x))
           _   (.setUTCDate t (+ (.getUTCDate t) (- 4 dow)))
           wy  (.getUTCFullYear t)
           wk  (js/Math.ceil (/ (inc (/ (- (.getTime t) (js/Date.UTC wy 0 1)) 86400000)) 7))]
       [wy wk])))

(defn bucket-label
  [unit v]
  #?(:clj
     (when-let [^java.time.LocalDateTime t (time/->utc-local v)]
       (let [f #(.format t (java.time.format.DateTimeFormatter/ofPattern %))]
         (case unit
           :year    (f "yyyy")
           :quarter (str (f "yyyy") "-Q" (inc (quot (dec (.getMonthValue t)) 3)))
           :month   (f "yyyy-MM")
           :week    (str (pad (.get t java.time.temporal.IsoFields/WEEK_BASED_YEAR) 4) "-W"
                         (pad (.get t java.time.temporal.IsoFields/WEEK_OF_WEEK_BASED_YEAR) 2))
           :day     (f "yyyy-MM-dd")
           :hour    (f "yyyy-MM-dd HH")
           :minute  (f "yyyy-MM-dd HH:mm")
           :second  (f "yyyy-MM-dd HH:mm:ss"))))
     :cljs
     (when-let [[y mo d h mi s] (date-parts v)]
       (let [ymd (str (pad y 4) "-" (pad mo 2) "-" (pad d 2))]
         (case unit
           :year    (pad y 4)
           :quarter (str (pad y 4) "-Q" (inc (quot (dec mo) 3)))
           :month   (str (pad y 4) "-" (pad mo 2))
           :week    (let [[wy wk] (iso-week y mo d)] (str (pad wy 4) "-W" (pad wk 2)))
           :day     ymd
           :hour    (str ymd " " (pad h 2))
           :minute  (str ymd " " (pad h 2) ":" (pad mi 2))
           :second  (str ymd " " (pad h 2) ":" (pad mi 2) ":" (pad s 2)))))))

(defn- fdiv [a b] (long (Math/floor (/ a b))))

(defn- ->int [s] #?(:clj (Long/parseLong s) :cljs (js/parseInt s 10)))

(defn- civil->days [y m d]
  (let [y   (if (<= m 2) (dec y) y)
        era (fdiv y 400)
        yoe (- y (* era 400))
        doy (+ (quot (+ (* 153 (+ m (if (> m 2) -3 9))) 2) 5) (dec d))
        doe (+ (* yoe 365) (quot yoe 4) (- (quot yoe 100)) doy)]
    (+ (* era 146097) doe -719468)))

(defn- days->civil [z]
  (let [z   (+ z 719468)
        era (fdiv z 146097)
        doe (- z (* era 146097))
        yoe (quot (- (+ (- doe (quot doe 1460)) (quot doe 36524)) (quot doe 146096)) 365)
        doy (- doe (+ (* 365 yoe) (quot yoe 4) (- (quot yoe 100))))
        mp  (quot (+ (* 5 doy) 2) 153)
        d   (inc (- doy (quot (+ (* 153 mp) 2) 5)))
        m   (if (< mp 10) (+ mp 3) (- mp 9))]
    [(+ yoe (* era 400) (if (<= m 2) 1 0)) m d]))

(defn- iso-dow [z] (inc (mod (+ z 3) 7)))

(defn- iso-monday [wy wk]
  (let [jan4 (civil->days wy 1 4)]
    (+ (- jan4 (dec (iso-dow jan4))) (* 7 (dec wk)))))

(def ^:private label-re
  {:year    #"^(\d{4})$"
   :quarter #"^(\d{4})-Q([1-4])$"
   :month   #"^(\d{4})-(\d{2})$"
   :week    #"^(\d{4})-W(\d{2})$"
   :day     #"^(\d{4})-(\d{2})-(\d{2})$"})

(defn- label->ordinal [unit s]
  (when-let [[_ a b c] (and (string? s) (re-find (label-re unit) s))]
    (let [y (->int a)]
      (case unit
        :year    y
        :quarter (+ (* y 4) (dec (->int b)))
        :month   (+ (* y 12) (dec (->int b)))
        :week    (fdiv (- (iso-monday y (->int b)) 4) 7)
        :day     (civil->days y (->int b) (->int c))))))

(defn- ordinal->label [unit n]
  (case unit
    :year    (pad n 4)
    :quarter (str (pad (fdiv n 4) 4) "-Q" (inc (mod n 4)))
    :month   (str (pad (fdiv n 12) 4) "-" (pad (inc (mod n 12)) 2))
    :week    (let [thu     (+ (* n 7) 4 3)
                   [wy _ _] (days->civil thu)]
               (str (pad wy 4) "-W" (pad (inc (quot (- thu (civil->days wy 1 1)) 7)) 2)))
    :day     (let [[y m d] (days->civil n)]
               (str (pad y 4) "-" (pad m 2) "-" (pad d 2)))))

(defn label-unit
  [labels]
  (let [ls (remove nil? labels)]
    (when (seq ls)
      (some (fn [u] (when (every? #(label->ordinal u %) ls) u))
            [:month :quarter :week]))))

(defn bucket-range
  [unit labels]
  (let [ns (map #(label->ordinal unit %) (remove nil? labels))]
    (when (and (seq ns) (every? some? ns))
      (let [lo (apply min ns) hi (apply max ns)]
        (when (< (- hi lo) max-buckets)
          (mapv #(ordinal->label unit %) (range lo (inc hi))))))))

(defn fold-values
  [f xs]
  (case f
    :count       (count xs)
    :cardinality (count (distinct xs))
    :sum         (reduce + 0 xs)
    :avg         (when (seq xs) (/ (double (reduce + xs)) (count xs)))
    :min         (when (seq xs) (reduce #(if (neg? (compare %2 %1)) %2 %1) xs))
    :max         (when (seq xs) (reduce #(if (pos? (compare %2 %1)) %2 %1) xs))))
