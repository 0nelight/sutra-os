(ns sutra.evals.cases
  (:require [sutra.objects.api :as objects]
            [sutra.kit.refusal :as refusal]
            [sutra.store.access.substrate :as sub]
            [sutra.catalog.lookup :as lookup]
            [sutra.types.link-side :as ls]))

(set! *warn-on-reflection* true)

(def page-size 1000)

(defn- query-all
  [store schema spec limit]
  (loop [offset 0 acc []]
    (let [want (if limit (min page-size (- limit (count acc))) page-size)
          rows (:rows (objects/run-object-query store schema
                                              (assoc spec :limit want :offset offset)))
          acc  (into acc rows)]
      (if (or (< (count rows) want) (and limit (>= (count acc) limit)))
        acc
        (recur (+ offset (count rows)) acc)))))

(defn link-source? [src] (and (map? src) (contains? src :link)))

(defn- linked
  [store schema ot rows {:keys [link property]}]
  (let [rid   (lookup/rid-of schema :object-types (keyword (name ot)))
        side  (ls/resolve-side schema rid link)
        conn  (objects/connected (sub/reader store) schema rid link (map :id rows))
        far   (vec (distinct (mapcat val conn)))
        by-id (if (and property (seq far))
                (into {} (map (juxt (comp str :id) :values))
                      (query-all store schema
                                 {:find (or (lookup/name-of schema :object-types (:far-type side))
                                            (:far-type side))
                                  :ids far}
                                 nil))
                {})
        one   (fn [id] (if property (get-in by-id [(str id) (keyword (name property))]) id))]
    (update-vals conn (fn [ids] (let [vs (mapv one ids)]
                                  (case (count vs) 0 nil 1 (first vs) vs))))))

(defn- column-value [links {:keys [id values]} src]
  (cond
    (and (map? src) (contains? src :val)) (:val src)
    (link-source? src)                    (get-in links [src (str id)])
    (= :id (keyword (name src)))          id
    :else                                 (get values (keyword (name src)))))

(defn object-set-cases
  [store schema {:keys [object-type where limit id input expected] :as from}]
  (when-not (and object-type (or (seq input) (seq expected)))
    (refusal/refuse! :refusal.evals/cases-from-incomplete {:cases-from from}))
  (let [rows  (query-all store schema
                         (cond-> {:find (keyword (name object-type))} where (assoc :where where))
                         limit)
        srcs  (distinct (filter link-source? (concat (vals input) (vals expected) [id])))
        links (into {} (for [s srcs] [s (linked store schema object-type rows s)]))
        cases (mapv (fn [row]
                      {:id       (str (column-value links row (or id :id)))
                       :input    (update-vals (or input {}) #(column-value links row %))
                       :expected (update-vals (or expected {}) #(column-value links row %))})
                    rows)
        kept  (filterv #(or (empty? (:input %)) (some some? (vals (:input %)))) cases)]
    {:cases kept :skipped (- (count cases) (count kept)) :found (count rows)}))

(defn mapped-input
  [input-map input]
  (if (and (seq input-map) (map? input))
    (let [renamed (into {} (for [[to from] input-map
                                 :when (contains? input (keyword (name from)))]
                             [(keyword (name to)) (get input (keyword (name from)))]))
          moved   (set (map (comp keyword name) (vals input-map)))]
      (merge (apply dissoc input moved) renamed))
    input))
