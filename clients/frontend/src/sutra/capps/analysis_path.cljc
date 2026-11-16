(ns sutra.capps.analysis-path
  (:require [clojure.string :as str]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.capps.chart-spec :as cs :refer [prop]]
            [sutra.capps.format :refer [num]]
            [sutra.capps.object-filter :as of]
            [sutra.capps.table :as tbl]
            [sutra.types.aggregate :as agg]
            [sutra.types.decimal :as d]))

(def card-types
  [:metric :text :filter :linked :selector :table :pivot :bar :heat-grid :line :pie :waterfall])

(def ^:private set-cards #{:filter :linked :selector})

(defn empty-path [] {:cards [] :next-id 1})

(defn add-card
  [{:keys [next-id] :as path} card-type]
  (-> path
      (update :cards conj {:id next-id :card-type (->kw card-type) :opts {}})
      (assoc :next-id (inc next-id))))

(defn set-opt
  [path id k v]
  (update path :cards (fn [cs] (mapv #(if (= id (:id %)) (assoc-in % [:opts k] v) %) cs))))

(defn remove-card
  [path id]
  (update path :cards (fn [cs] (filterv #(not= id (:id %)) cs))))

(defn- filtered
  [rows {:keys [conditions operator]}]
  (let [f {:operator (or (->kw operator) :and)
           :terms (mapv (fn [{:keys [prop op value]}]
                          {:prop (->kw prop) :op (->kw (or op :is)) :values [value]})
                        (remove #(str/blank? (str (:value %))) conditions))}]
    (if (seq (:terms f)) (filterv #(of/matches? f %) rows) (vec rows))))

(defn output-of
  [{:keys [card-type opts]} rows linked]
  (case card-type
    :filter   (filtered rows opts)
    :linked   (vec linked)
    :selector (if-let [id (:object opts)]
                (filterv #(= (str id) (str (:id %))) rows)
                (vec rows))
    (vec rows)))

(defn run-path
  [{:keys [cards]} base linked-of]
  (reduce (fn [{:keys [out rows]} card]
            (let [next-rows (output-of card rows (linked-of (:id card)))]
              {:out  (conj out (assoc card :input rows :output next-rows))
               :rows (if (set-cards (:card-type card)) next-rows rows)}))
          {:out [] :rows (vec base)}
          cards))

(defn properties
  [rows]
  (vec (sort (into #{} (mapcat #(keys (or (:values %) %))) (take 50 rows)))))

(defn metric
  [rows {:keys [agg-fn of]}]
  (let [f (or (->kw agg-fn) :count)]
    (if (or (= :count f) (nil? of))
      (count rows)
      (agg/fold-values f (if (= :cardinality f)
                           (into [] (keep #(prop % (->kw of))) rows)
                           (into []
                                 (keep #(some-> (num (prop % (->kw of))) d/lossy-number))
                                 rows))))))

(defn pivot
  [rows {:keys [row col of agg-fn]}]
  (when (and row col)
    (tbl/pivot {:rows rows :row (->kw row) :col (->kw col) :agg (or (->kw agg-fn) :count)
                :value (if of (->kw of) (constantly 1))})))

(defn- running-total
  [rows {:keys [x of agg-fn]}]
  (->> (group-by #(prop % (->kw x)) rows)
       (sort-by (comp str key))
       (mapv (fn [[g rs]] {:x g :v (metric rs {:agg-fn agg-fn :of of})}))))

(defn chart-of
  [card-type rows {:keys [x of agg-fn group orientation] :as opts}]
  (let [f (or (->kw agg-fn) :count)
        x (->kw x)]
    (when x
      (case card-type
        :bar   (cs/chart->spec {:rows rows :x x :chart-kind :bar :agg f :y (some-> of ->kw)
                                :orientation orientation :group (some-> group ->kw)})
        :line  (cs/chart->spec {:rows rows :x x :chart-kind :line :agg f :y (some-> of ->kw)})
        :pie   (cs/chart->spec {:rows rows :x x :chart-kind :pie :agg f :y (some-> of ->kw)})
        :waterfall
        (let [steps (running-total rows opts)]
          {:traces [{:type "waterfall" :x (mapv (comp str :x) steps) :y (mapv :v steps)
                     :measure (mapv (constantly "relative") steps)}]
           :layout {:margin {:l 48 :r 16 :t 16 :b 40} :showlegend false
                    :paper_bgcolor "transparent" :plot_bgcolor "transparent"}})
        :heat-grid
        (when-let [y (some-> group ->kw)]
          (let [xs (vec (sort (distinct (map #(str (prop % x)) rows))))
                ys (vec (sort (distinct (map #(str (prop % y)) rows))))
                by (group-by (fn [r] [(str (prop r x)) (str (prop r y))]) rows)]
            {:traces [{:type "heatmap" :x xs :y ys :colorscale "YlOrRd"
                       :z (mapv (fn [yv]
                                  (mapv (fn [xv]
                                          (when-let [rs (get by [xv yv])]
                                            (metric rs {:agg-fn f :of of})))
                                        xs))
                                ys)}]
             :layout {:margin {:l 72 :r 16 :t 16 :b 40}
                      :paper_bgcolor "transparent" :plot_bgcolor "transparent"}}))
        nil))))

(defn card-markdown
  [{:keys [card-type opts input]}]
  (case card-type
    :metric (str "**" (or (:title opts) "Kennzahl") ":** " (metric input opts))
    :text   (str (:text opts))
    (str "**" (name card-type) "** über " (count input) " Objekte")))

(defn saveable
  [path]
  (select-keys path [:cards :next-id]))
