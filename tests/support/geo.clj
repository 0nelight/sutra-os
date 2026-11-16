(ns support.geo
  (:require [sutra.store.api :as api]))

(def flughafen "ri.sutra.main.object-type.00000000-0000-0000-0000-0000000000f1")

(def world
  {:object-types
   {flughafen {:object-type-name :flughafen
               :attribute-list [{:attribute-name :code :attribute-type :string}
                                {:attribute-name :lage :attribute-type :geopoint}
                                {:attribute-name :lat :attribute-type :double}
                                {:attribute-name :lon :attribute-type [:decimal 9 6]}]
               :plural "Flughäfen"}}})

(def ^:private airports
  [["VIE" 48.1103 16.5697] ["KLU" 46.6425 14.3377] ["LJU" 46.2237 14.4576]
   ["TSF" 45.6484 12.1944] ["ZRH" 47.4647 8.5492] ["NAN" -17.7554 177.4434]
   ["HNL" 21.3187 -157.9225] ["AKL" -37.0082 174.785]])

(defn id [i] (format "00000000-0000-0000-0000-%012d" (inc i)))

(def rows
  (into (mapv (fn [[code lat lon]]
                {:code code :lage (str lat "," lon) :lat lat :lon (bigdec (str lon))})
              airports)
        [{:code "NIL"}
         {:code "BAD" :lage "n/a"}]))

(def villach [46.6167 13.85])

(def cases
  (for [field [:lage [:lat :lon]]
        [where codes]
        [[[:within-distance field {:center villach :distance 300}]
          #{"KLU" "LJU" "TSF" "VIE"}]
         [[:within-distance field {:center "46.6167,13.85" :distance 50 :unit :km}] #{"KLU"}]
         [[:within-distance field {:center {:lat 46.6167 :lon 13.85} :distance 300 :unit :mi}]
          #{"KLU" "LJU" "TSF" "VIE" "ZRH"}]
         [[:within-distance field {:center villach :distance 0}]                   #{}]
         [[:within-bounding-box field {:top-left [48.5 12.0] :bottom-right [46.0 17.0]}]
          #{"KLU" "LJU" "VIE"}]
         [[:within-bounding-box field {:top-left [-10 170] :bottom-right [-40 -170]}]
          #{"NAN" "AKL"}]
         [[:or [:within-distance field {:center villach :distance 50}] [:= :code "NIL"]]
          #{"KLU" "NIL"}]
         [[:and [:within-distance field {:center villach :distance 300}] [:starts-with :code "L"]]
          #{"LJU"}]]]
    [where codes]))

(defn seed! [db]
  (let [basis (with-open [s (api/snapshot db {:branch "main"})] (api/basis s))]
    (api/transact! db
                   {:basis basis :actor {:principal "t"} :schema {:authored world :migration []}
                    :ops (map-indexed (fn [i v]
                                        {:op :create-object :type flughafen
                                         :payload
                                           {:id (id i)
                                            :values (into {} (remove (comp nil? val)) v)}})
                                      rows)})))

(defn codes [db where]
  (with-open [s (api/snapshot db {:branch "main"})]
    (into #{} (map (comp :code :values))
          (api/read s {:system? true}
                    [:select {:op :filter :where where :in {:op :scan :root flughafen}}]))))
