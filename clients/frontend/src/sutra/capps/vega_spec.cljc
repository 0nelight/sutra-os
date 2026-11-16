(ns sutra.capps.vega-spec
  (:require [clojure.string :as str]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.capps.chart-spec :refer [prop]]
            [sutra.capps.format :refer [num]]
            [sutra.capps.map-geo :as geo]
            [sutra.capps.object-filter :as of]
            [sutra.types.aggregate :as agg]
            [sutra.types.decimal :as d]))

(def blueprint-theme
  {:background "transparent"
   :font "inherit"
   :axis {:gridColor "rgba(128,128,128,0.18)" :domainColor "rgba(128,128,128,0.35)"
          :tickColor "rgba(128,128,128,0.35)" :labelFontSize 11 :titleFontSize 11}
   :legend {:labelFontSize 11 :titleFontSize 11}
   :range {:category ["#1f6e63" "#b87914" "#4f6bd8" "#b3446c" "#3f7f3f" "#6b4a34" "#7a5ea8"
                      "#8a8f98"]}})

(defn- plain-value
  [x]
  (cond (keyword? x) (name x)
        :else        x))

(defn object-data
  [rows props]
  (mapv (fn [r]
          (if (seq props)
            (into {} (map (fn [p] [(name (->kw p)) (plain-value (prop r (->kw p)))])) props)
            (into {} (map (fn [[k x]] [(name k) (plain-value x)])) (or (:values r) r))))
        rows))

(defn- bucket
  [x {:keys [unit width]}]
  (cond
    unit  (agg/bucket-label (->kw unit) x)
    width (let [n (d/lossy-number (num x))] (when n (* width (Math/floor (/ n width)))))
    :else x))

(defn aggregate-data
  [rows {:keys [group-by agg-fn of value-name]}]
  (let [gs   (mapv (fn [g] (if (map? g) g {:prop g})) group-by)
        f    (or (->kw agg-fn) :count)
        vnm  (or value-name (name f))
        keyf (fn [r] (mapv #(bucket (prop r (->kw (:prop %))) %) gs))]
    (mapv (fn [[k rs]]
            (assoc (zipmap (map #(name (->kw (:prop %))) gs) (map plain-value k))
                   vnm
                   (agg/fold-values
                    f
                    (cond
                      (nil? of)                (mapv (constantly 1) rs)
                      (#{:count :cardinality} f) (into [] (keep #(prop % (->kw of))) rs)
                      :else (into []
                                  (keep #(some-> (num (prop % (->kw of))) d/lossy-number))
                                  rs)))))
          (clojure.core/group-by keyf rows))))

(defn input-data
  [{:keys [rows properties aggregate values]}]
  (cond
    (some? values) (vec values)
    aggregate      (aggregate-data rows aggregate)
    :else          (object-data rows properties)))

(defn- read-spec
  [spec]
  (cond
    (map? spec)    spec
    (string? spec) (geo/parse-json spec)
    :else          nil))

(defn- themed
  [spec theme]
  (let [custom (read-spec theme)
        conf   (if (false? theme) nil (or custom blueprint-theme))]
    (cond-> spec
      conf (update :config #(merge conf %)))))

(defn- with-data
  [spec grammar named]
  (if (= :vega (->kw grammar))
    (let [others (remove #(contains? named (:name %)) (:data spec))]
      (assoc spec :data (into (vec (map (fn [[n vs]] {:name n :values vs}) named)) others)))
    (update spec :datasets merge named)))

(defn complete
  [{:keys [spec grammar theme inputs]}]
  (if-let [s (read-spec spec)]
    (let [named (into {} (map (fn [i] [(:name i) (input-data i)])) inputs)]
      {:spec (-> s (with-data grammar named) (themed theme))})
    {:error "Die Spezifikation ist kein gültiges JSON."}))

(defn truncated
  [spec n]
  (let [cut (fn [vs] (if (> (count vs) n) (vec (take n vs)) vs))]
    (cond-> spec
      (:datasets spec) (update :datasets update-vals cut)
      (sequential? (:data spec)) (update :data (fn [ds] (mapv #(update % :values cut) ds))))))

(defn selection->filter
  [value]
  (let [terms (keep (fn [[k x]]
                      (let [p (->kw (str/replace (name k) #"^vlPoint$" ""))]
                        (cond
                          (and (sequential? x) (= 2 (count x)) (every? number? x))
                          {:prop p :op :range :min (first x) :max (second x)}
                          (sequential? x) {:prop p :op :is :values (vec x)}
                          (some? x)       {:prop p :op :is :values [x]})))
                    (dissoc value :vlPoint :_vgsid_))]
    (when (seq terms)
      (of/normalize {:operator :and :terms (vec terms)}))))
