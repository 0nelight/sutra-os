(ns sutra.capps.leaf-props
  (:require [clojure.string :as str]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.capps.layout :as lay]
            [sutra.capps.standard-view :as sv]
            [sutra.capps.table :as tbl]
            [sutra.capps.variables :as vars]
            [sutra.types.rid :as rid]))

(defn as-rows
  [x]
  (cond (map? x) [x] (coll? x) (vec x) :else []))

(defn by-type
  [table t]
  (when (and table (some? t))
    (or (get table t)
        (get table (->kw t))
        (get table (name t)))))

(defn rid-of [descriptor t]
  (when (some? t)
    (if (rid/rid? (name t))
      t
      (or (some (fn [[r n]] (when (= (name n) (name t)) r)) (:object-names descriptor)) t))))

(defn type-entry [descriptor k t]
  (by-type (get descriptor k) (rid-of descriptor t)))

(defn viewed-type
  [descriptor row]
  (let [t (or (:object-type-ref row) (:object-type row))]
    (->kw (or (get (:object-names descriptor) t) t))))

(defn display-of
  [descriptor t]
  (type-entry descriptor :property-display t))

(defn display-entry
  [descriptor t prop]
  (let [m (display-of descriptor t)]
    (or (get m prop) (get m (name prop)))))

(defn- auto-tone? [t] (or (nil? t) (= :auto (->kw t))))

(defn- source-look
  [descriptor {:keys [prop values type]}]
  (when prop
    (let [t (viewed-type descriptor (if type {:object-type-ref type} (:object descriptor)))]
      (tbl/prop-look (display-entry descriptor t prop) prop values))))

(defn sourced
  [{:keys [descriptor ctx]} x tone-key value-key]
  (let [tone  (get x tone-key)
        auto? (auto-tone? tone)
        look  (when (or auto? (nil? (:fmt x))) (source-look descriptor (::lay/source x)))
        f     (if (:fmt x) (tbl/fmt-by (:fmt x)) (or (:fmt look) (tbl/fmt-by nil)))]
    (cond-> (-> x
                (dissoc ::lay/source)
                (assoc tone-key (when-not auto? tone))
                (update value-key f ctx))
      (and auto? (:look look)) (assoc :look (:look look)))))

(defn- metric-secondary
  [ctx x]
  (if (map? x)
    {:value ((tbl/fmt-by (:fmt x)) (:value x) ctx)
     :look  (tbl/cond-format (:conditional-format x) {:value (:value x)})}
    {:value x}))

(defn metric-item
  [page {:keys [value conditional-format secondary] :as it}]
  (cond-> (dissoc (sourced page it :accent :value) :conditional-format)
    (seq conditional-format) (update :look merge
                                     (tbl/cond-format conditional-format {:value value}))
    (some? secondary)        (assoc :secondary (metric-secondary (:ctx page) secondary))))

(def ^:private agg-words
  {:count "Anzahl" :distinct-count "Verschiedene" :sum "Summe" :avg "Durchschnitt"
   :min "Minimum" :max "Maximum"})

(defn- card-spec
  [c]
  (if (map? c)
    (cond-> c
      (:prop c) (update :prop ->kw)
      (:of c)   (update :of ->kw)
      (:link c) (update :link ->kw)
      (:agg c)  (update :agg ->kw))
    {:prop (->kw c)}))

(defn- agg-card
  [{:keys [descriptor ctx]} data {:keys [link agg of label fmt icon] :as c}]
  (let [linked (as-rows (get data (vars/link-key link)))
        n      ((get vars/agg-fns agg (get vars/agg-fns :count)) of linked)
        e      (when (and of (not (#{:count :distinct-count} agg)))
                 (some->> (first linked) (viewed-type descriptor)
                          (#(display-entry descriptor % of))))
        f      (cond fmt  (tbl/fmt-by fmt)
                     e    (:fmt (tbl/prop-look e of nil))
                     :else (tbl/fmt-by nil))
        cf     (:conditional-format c)]
    {:label (or label (str (tbl/humanize link) " · " (get agg-words agg (name agg))))
     :value (f n ctx)
     :look  (cond-> (when (seq cf) (tbl/cond-format cf {:value n}))
              icon (assoc :icon icon))}))

(defn- prop-card
  [{:keys [descriptor ctx]} row t data {:keys [prop link label fmt icon] :as c}]
  (let [row   (if link (first (as-rows (get data (vars/link-key link)))) row)
        t     (if link (some->> row (viewed-type descriptor)) t)
        vals  (:values row)
        value (get vals prop)
        d     (tbl/prop-look (when t (display-entry descriptor t prop)) prop vals)
        cf    (:conditional-format c)]
    {:label (or label (:label d))
     :value ((if fmt (tbl/fmt-by fmt) (:fmt d)) value ctx)
     :look  (cond-> (if (seq cf) (tbl/cond-format cf (assoc vals :value value)) (:look d))
              icon (assoc :icon icon))}))

(defn property-cards
  [{:keys [descriptor] :as page} {:keys [cards object display]}]
  (let [data  (or (:data page) {})
        given (first (as-rows object))
        row0  (or given (:object descriptor))
        t     (when row0 (viewed-type descriptor row0))
        row   (or given (some-> (get data t) first) row0)
        specs (if (sequential? cards)
                (mapv card-spec cards)
                (mapv (fn [p] {:prop p}) (sv/prominent-props (display-of descriptor t))))]
    (if (nil? row)
      {:no-object? true}
      {:items   (mapv #(if (:agg %) (agg-card page data %) (prop-card page row t data %))
                      specs)
       :display display})))

(defn seg-list
  [x]
  (let [clean (fn [s] (let [s (str s)] (when-not (str/blank? s) s)))]
    (cond
      (nil? x)        []
      (sequential? x) (into [] (comp (keep clean) (distinct)) x)
      :else           (vec (keep clean [x])))))

(defn scenario-name
  [items seg]
  (or (some (fn [{:keys [scenario name]}] (when (= (str scenario) (str seg)) name)) items)
      (str seg)))

(defn compare-chart
  [{cmp :compare :as cfg}]
  (let [{:keys [rows scenarios]} (if (map? cmp) cmp {})
        label (into {} (map (juxt :scenario :name)) scenarios)
        segs  (mapv :scenario scenarios)
        base  (into [] (keep :base) rows)
        by    (into {} (map (fn [s] [s (into [] (keep #(get (:by %) s)) rows)])) segs)]
    (-> cfg
        (dissoc :compare)
        (assoc :rows (tbl/compare-tag-rows base by label :szenario "Basis")
               :group :szenario))))

(defn compare-table
  [{:keys [cols sort limit] cmp :compare :as cfg} ctx]
  (let [{:keys [rows scenarios loading? error]} (if (map? cmp) cmp {})
        segs  (mapv :scenario scenarios)
        label (into {} (map (juxt :scenario :name)) scenarios)
        rows  (cond->> rows
                (seq sort)                               (#(tbl/compare-sort % sort))
                (and limit (number? limit) (pos? limit)) (into [] (take limit)))]
    (cond-> (-> cfg
                (dissoc :compare :sort :limit)
                (assoc :rows rows
                       :cols (into []
                                   (mapcat
                                    (fn [c]
                                      (into [{:col c :seg nil}]
                                            (keep (fn [seg]
                                                    (when (tbl/compare-changed? c rows seg ctx)
                                                      {:col   c :seg seg
                                                       :label (str (:label c) " · "
                                                                   (get label seg seg))})))
                                            segs)))
                                   cols)))
      error    (assoc :rows [] :foot [:error (str error)])
      loading? (assoc :foot [:loading]))))

(def ^:private by-widget
  {:property-cards property-cards
   :kpi            metric-item
   :kpi-row        (fn [page cfg] (update cfg :items #(mapv (partial metric-item page)
                                                            (as-rows %))))
   :badge          (fn [page cfg] (sourced page cfg :tone :text))})

(defn leaf->props
  [page {:keys [widget] :as leaf}]
  (if-let [f (by-widget (->kw widget))]
    (f page (dissoc leaf :widget))
    (dissoc leaf :widget)))
