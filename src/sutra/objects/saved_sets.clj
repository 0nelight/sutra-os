(ns sutra.objects.saved-sets
  (:require [sutra.store.control :as ctl]))

(set! *warn-on-reflection* true)

(defn- live? [store row]
  (.after ^java.util.Date (:expires-at row) (ctl/clock store)))

(defn- row->set
  [row]
  (when row
    {:rid        (:rid row)
     :ir         (:definition row)
     :kind       (:kind row)
     :lifecycle  :temporary
     :created-by (:created-by row)
     :branch     (:branch row)
     :expires-at (:expires-at row)}))

(defn create!
  [store {:keys [rid ir kind created-by branch ttl-seconds]}]
  (ctl/insert! store :object-sets-temp
               {:rid rid :created-by (str created-by) :branch branch :kind kind
                :definition ir :created-at ctl/now
                :expires-at
                  (java.util.Date. (+ (ctl/millis store) (* 1000 (long ttl-seconds))))})
  {:rid rid :ir ir :kind kind :lifecycle :temporary
   :created-by created-by :branch branch})

(defn fetch
  [store rid principal]
  (when (and rid principal)
    (let [row (ctl/get store :object-sets-temp {:rid rid})]
      (when (and row (= (str principal) (:created-by row)) (live? store row))
        (row->set row)))))

(defn list-for
  [store principal]
  (when principal
    (into [] (comp (filter #(live? store %)) (map row->set))
          (ctl/list store :object-sets-temp {:created-by (str principal)}))))

(defn revoke!
  [store rid principal]
  (boolean
   (when (and rid principal)
     (pos? (long (ctl/delete! store :object-sets-temp {:rid rid :created-by (str principal)}))))))

(defn sweep-expired!
  [store]
  (ctl/commit! store
               [(fn [store _]
               (let [dead (remove #(live? store %) (ctl/list store :object-sets-temp {}))]
                 {:records
                    (vec (mapcat #(:records (ctl/deleting store :object-sets-temp {:rid (:rid %)}))
                                 dead))
                  :result  (count dead)}))]))
