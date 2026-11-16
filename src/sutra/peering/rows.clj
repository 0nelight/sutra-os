(ns sutra.peering.rows
  (:require [sutra.kit.refusal :as refusal]
            [sutra.store.control :as ctl]
            [sutra.types.peering :as peering]))

(set! *warn-on-reflection* true)

(defn- upsert! [store t rows] (ctl/commit! store [[:upsert t (vec rows)]]))

(defn- insert! [store t rows] (ctl/commit! store [[:insert t (vec rows)]]))

(defn- delete! [store t wheres]
  (when (seq wheres)
    (ctl/commit! store (mapv (fn [w] [:delete t w]) wheres))))

(defn connections [store] (vec (ctl/list store :peer-connections {})))

(defn connection [store id] (ctl/get store :peer-connections {:connection-id (str id)}))

(defn connection!
  [store id]
  (or (connection store id)
      (refusal/refuse! :refusal.peering/no-connection {:connection-id id})))

(defn by-name-or-id
  [store x]
  (or (connection store x)
      (first (filter #(= (str x) (:name %)) (connections store)))
      (refusal/refuse! :refusal.peering/no-connection-named {:connection x})))

(defn put-connection! [store row] (upsert! store :peer-connections [row]) row)

(defn drop-connection! [store id]
  (delete! store :peer-connections [{:connection-id (str id)}]))

(defn mappings
  ([store] (vec (ctl/list store :peer-mappings {})))
  ([store id] (vec (ctl/list store :peer-mappings {:connection-id (str id)}))))

(defn mapping [store id local-type]
  (ctl/get store :peer-mappings {:connection-id (str id) :local-type (name local-type)}))

(defn mapping-by-remote [store id remote-type]
  (first (filter #(= (str remote-type) (:remote-type %)) (mappings store id))))

(defn put-mapping! [store row] (upsert! store :peer-mappings [row]) row)

(defn drop-mapping! [store id local-type]
  (delete! store :peer-mappings [{:connection-id (str id) :local-type (name local-type)}]))

(defn shares [store id] (vec (ctl/list store :peer-shares {:connection-id (str id)})))

(defn shared? [store id ds]
  (some? (ctl/get store :peer-shares {:connection-id (str id) :dataset (name ds)})))

(defn share! [store row] (upsert! store :peer-shares [row]) row)

(defn unshare! [store id ds]
  (delete! store :peer-shares [{:connection-id (str id) :dataset (name ds)}]))

(defn jobs
  ([store] (vec (ctl/list store :peer-jobs {})))
  ([store id] (vec (ctl/list store :peer-jobs {:connection-id (str id)}))))

(defn job!
  [store row]
  (let [row (merge {:job-id (str (random-uuid)) :items 0 :held 0 :superseded 0} row)]
    (insert! store :peer-jobs [row])
    row))

(defn outbox
  ([store] (vec (sort-by :seq (ctl/list store :peer-outbox {}))))
  ([store id] (vec (sort-by :seq (ctl/list store :peer-outbox {:connection-id (str id)})))))

(defn enqueue!
  [store {:keys [connection-id local-type seq payload]}]
  (insert! store :peer-outbox [{:item-id (str (random-uuid)) :connection-id connection-id
                                :seq seq :local-type (name local-type) :payload payload
                                :attempts 0 :next-at 0}]))

(defn acknowledged! [store item-ids]
  (delete! store :peer-outbox (mapv (fn [i] {:item-id (str i)}) item-ids)))

(defn failed! [store item next-at why]
  (upsert! store :peer-outbox [(assoc item :attempts (inc (long (or (:attempts item) 0)))
                                      :next-at next-at :last-error (str why))]))

(defn crosswalk
  ([store id] (crosswalk store id nil))
  ([store id local-type]
   (into {} (for [r (ctl/list store :peer-crosswalk
                              (cond-> {:connection-id (str id)}
                                local-type (assoc :local-type (name local-type))))]
              [(:local-id r) (:remote-id r)]))))

(defn crosswalk-of-type
  [store local-type]
  (vec (ctl/list store :peer-crosswalk {:local-type (name local-type)})))

(defn crosswalk-row [store id local-id]
  (ctl/get store :peer-crosswalk {:connection-id (str id) :local-id (str local-id)}))

(defn walked! [store rows] (upsert! store :peer-crosswalk rows))

(defn pending
  ([store] (vec (ctl/list store :peer-pending-links {})))
  ([store id] (vec (ctl/list store :peer-pending-links {:connection-id (str id)}))))

(defn- edge-key [id {:keys [link-type from-id to-id]}]
  {:connection-id (str id) :link-type (str link-type) :from-id (str from-id)
   :to-id (str to-id)})

(defn hold!
  [store id named edges]
  (let [now (set (map #(edge-key id %) edges))
        old (for [r (pending store id)
                  :when (contains? named [(:from-id r) (:link-type r)])
                  :let [k (edge-key id r)]
                  :when (not (contains? now k))]
              k)]
    (delete! store :peer-pending-links (vec old))
    (when (seq now) (upsert! store :peer-pending-links (vec now)))))

(defn released! [store id edges]
  (delete! store :peer-pending-links (mapv #(edge-key id %) edges)))

(defn health
  [store id]
  (peering/health (jobs store id)
                  (peering/pending-links (pending store id) (System/currentTimeMillis))))

(defn profiles [store] (vec (ctl/list store :peer-profiles {})))

(defn profile [store id] (ctl/get store :peer-profiles {:profile-id (str id)}))

(defn profile!
  [store x]
  (or (profile store x)
      (first (filter #(= (str x) (:name %)) (profiles store)))
      (refusal/refuse! :refusal.peering/no-profile {:profile x})))

(defn put-profile! [store row] (upsert! store :peer-profiles [row]) row)

(defn drop-profile! [store id] (delete! store :peer-profiles [{:profile-id (str id)}]))

(defn mesh
  [store]
  (into {} (for [r (ctl/list store :peer-mesh-nodes {})]
             [(:trust r) (set (map str (:labels r)))])))

(defn put-mesh-node! [store trust labels]
  (upsert! store :peer-mesh-nodes [{:trust (str trust) :labels (vec (sort (map str labels)))}]))

(defn drop-mesh-node! [store trust]
  (delete! store :peer-mesh-nodes [{:trust (str trust)}]))

(defn offers [store] (vec (ctl/list store :peer-offers {})))

(defn put-offer! [store row] (upsert! store :peer-offers [row]))

(defn drop-offer! [store id] (delete! store :peer-offers [{:connection-id (str id)}]))

(defn media-mappings
  ([store] (vec (ctl/list store :peer-media-mappings {})))
  ([store id] (vec (ctl/list store :peer-media-mappings {:connection-id (str id)}))))

(defn media-mapping-by-remote [store id remote-set]
  (first (filter #(= (str remote-set) (:remote-set %)) (media-mappings store id))))

(defn put-media-mapping! [store row] (upsert! store :peer-media-mappings [row]) row)

(defn drop-media-mapping! [store id local-set]
  (delete! store :peer-media-mappings [{:connection-id (str id) :local-set (str local-set)}]))
