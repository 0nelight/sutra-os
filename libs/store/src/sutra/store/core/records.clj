(ns sutra.store.core.records
  (:require [clojure.edn :as edn]
            [malli.core :as m]
            [sutra.store.api :as api]
            [sutra.store.scan :as scan]
            [sutra.store.spi :as spi]
            [sutra.store.core :as-alias core]
            [sutra.store.core.engine :refer [bare ecx scan]]
            [sutra.store.core.kv :refer [enc kv-fx kv-get kv-scan record-part]]))

(set! *warn-on-reflection* true)

(defn collection! [{:keys [collections]} coll]
  (or (get collections coll)
      (api/refuse! :malformed (str "no records collection " coll " is declared") {:coll coll})))

(defn record-loc [db coll b]
  [:record (record-part coll (if (= :global (:scope (collection! db coll))) "" b))])

(defn indexed!
  [db coll wanted]
  (let [wanted (dissoc wanted (:auto (collection! db coll)))
        decl  (:index (collection! db coll))
        eq    (into #{} (map #(if (vector? %) (first %) %)) decl)
        range (into #{} (keep #(when (vector? %) (first %))) decl)]
    (doseq [[a k] wanted
            :when (not (contains? (if (= :range k) range eq) a))]
      (api/refuse! :malformed (str coll " declares no " (when (= :range k) "range ") "index on " a)
                   {:coll coll :attribute a :index decl}))))

(defn in-engine?
  [{:keys [engine]} coll]
  (contains? (spi/-rec-colls engine) coll))

(defn rec-ns [db coll b] (if (= :global (:scope (collection! db coll))) "" b))

(defn record-key
  [db b coll k]
  (when-not (in-engine? db coll) (conj (record-loc db coll b) (enc k))))

(defn record-value
  ([db h b coll k] (record-value db h b coll k nil))
  ([{:keys [engine] :as db} h b coll k kv]
   (let [rk (record-key db b coll k)
         v  (cond (nil? rk) (spi/-rec-get engine h {:ns (rec-ns db coll b) :sig {} :as-of nil} coll
                                          k)
                  kv        (get kv rk)
                  :else     (kv-get db h rk))]
     (if (nil? v) api/absent v))))

(def ^:private validator (memoize m/validator))

(def ^:private auto-span
  1048576)

(defn- auto-key [rev i]
  (when (>= i auto-span)
    (api/refuse! :malformed (str "one commit assigns at most " auto-span " keys") {:entries i}))
  (+ (* (long rev) auto-span) i))

(declare rec-list)

(defn unique-holders
  [db h b coll cols vals]
  (into #{} (map first) (rec-list db h b coll {:where (zipmap cols vals)} #{} true)))

(defn unique-claims
  [db records]
  (for [[i {:keys [coll key value]}] (map-indexed vector records)
        :when (some? value)
        cols (:unique (collection! db coll))
        :let [vs (mapv #(get value %) cols)]
        :when (every? some? vs)]
    {:i i :coll coll :key key :cols cols :vals vs}))

(defn unique-refusal
  [db records taken]
  (let [claims  (unique-claims db records)
        freed   (into #{}
                      (keep (fn [{:keys [coll key value]}]
                              (when (and (nil? value) (not= api/auto key)) [coll key])))
                      records)
        refused (fn [{:keys [coll cols vals]}]
                  (api/refused :conflict (str coll " " (pr-str (zipmap cols vals)) " is taken")
                               {:coll coll :unique (zipmap cols vals)}))]
    (some-> (or (first (for [{:keys [i coll key cols] :as c} claims
                             :when (some #(not (or (= % key) (contains? freed [coll %])))
                                         (get taken [i cols]))]
                         c))
                (first (for [[_ cs] (group-by (juxt :coll :cols :vals) claims)
                             :when (< 1 (count (distinct
                                                (map #(if (= api/auto (:key %))
                                                        [::core/auto (:i %)]
                                                        (:key %))
                                                     cs))))]
                         (first cs))))
            refused)))

(defn record-effect
  [db b rev i {:keys [coll] :as w} current]
  (let [decl  (collection! db coll)
        strip #(bare {coll (:auto decl)} coll %)
        {:keys [key value] :as w} (cond-> (update w :value strip)
                                    (contains? w :expect) (update :expect strip))
        auto? (= api/auto key)]
    (cond
      (not (or auto? ((validator (:key decl)) key)))
      (api/refused :malformed (str "bad key for " coll) {:coll coll :key key})

      (and (some? value) (not ((validator (:value decl)) value)))
      (api/refused :malformed (str "bad value for " coll)
                   {:coll coll :key key :explain (m/explain (:value decl) value)})

      (and (not auto?) (contains? w :expect) (not= (:expect w) current))
      (api/refused :conflict (str coll " " (pr-str key) " changed since it was read")
                   {:store/fence [:record coll key (:expect w)]})

      (in-engine? db coll)
      [[:rec-put {:ns (rec-ns db coll b) :sig {} :as-of nil} coll key value i] ::core/engine]

      :else
      (let [k (if auto? (auto-key rev i) key)]
        [(kv-fx (conj (record-loc db coll b) (enc k)) value) k]))))

(defn wanted-index
  [{:keys [where index range order]}]
  (cond-> (zipmap (keys where) (repeat :eq))
    index (assoc (first index) :eq)
    range (assoc (first range) :range)
    order (assoc (first order) :range)))

(defn- records-scan
  [coll auto {:keys [where index range prefix after order limit count? pick] gb :group-by}]
  (let [attr       #(if (and auto (= % auto)) :db/key %)
        eq         (fn [a v]
                     (cond (nil? v) [:nil? (attr a) true]
                       (= :records/present v) [:nil? (attr a) false]
                       (set? v) [:in (attr a) v]
                       :else [:= (attr a) v]))
        [ra lo hi] range]
    (cond-> {:source [:records coll]
             :where  (cond-> (mapv (fn [[a v]] (eq a v)) where)
                       index         (conj (eq (first index) (second index)))
                       (some? lo)    (conj [:>= (attr ra) lo])
                       (some? hi)    (conj [:<= (attr ra) hi])
                       prefix        (conj [:starts-with :db/key prefix]))
             :order  (if order [[(attr (first order)) (second order)]] [[:db/key :asc]])}
      (some? after) (assoc :after after)
      limit   (assoc :limit limit)
      gb      (assoc :agg {:group-by gb})
      pick    (assoc :agg {:pick (update-vals pick (partial mapv attr))})
      count?  (assoc :agg {:count? true}))))

(defn rec-list
  ([db h b coll opts] (rec-list db h b coll opts nil false))
  ([{:keys [engine] :as db} h b coll opts project commit?]
   (let [s  (cond-> (records-scan coll (:auto (collection! db coll)) (or opts {}))
              project (assoc :project project))
         cx (cond-> {:ns (rec-ns db coll b) :sig {} :as-of nil} commit? (assoc :commit? true))]
     (if (in-engine? db coll)
       (scan db h cx s #(spi/-rec-scan engine h (ecx cx) coll))
       (scan/reference (map (fn [[k v]] [(edn/read-string k) v])
                            (kv-scan db h (record-loc db coll b) nil)) s)))))
