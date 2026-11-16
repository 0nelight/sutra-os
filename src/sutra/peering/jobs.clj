(ns sutra.peering.jobs
  (:require [sutra.access.api :as access]
            [sutra.catalog.lookup :as lookup]
            [sutra.kit.refusal :as refusal]
            [sutra.peering.markings :as markings]
            [sutra.peering.media :as media]
            [sutra.peering.objects :as objects]
            [sutra.peering.rows :as rows]
            [sutra.peering.wire :as wire]
            [sutra.types.authority :as policy]
            [sutra.types.peering :as peering]))

(set! *warn-on-reflection* true)

(def batch-max 200)

(def peer-path "/api/peering/peer")

(defn- own-fingerprint [store] (:fingerprint (access/instance-identity store)))

(defn- link-rids
  [schema m]
  (into {} (for [[l r] (:links m)
                 :let [rid (lookup/rid-of schema :link-types (keyword (name l)))]
                 :when rid]
             [rid (name r)])))

(defn- wire-markings [schema c rid obj-markings]
  (peering/wire-markings (into (objects/type-markings schema rid) (map keyword) obj-markings)
                         (policy/classification-of schema) (:marking-map c)))

(defn- outbound-item
  [store schema c m rid cw links id obj]
  (let [ref (peering/outbound-ref cw id)]
    (if (nil? obj)
      {:ref ref :op :delete-object :markings (wire-markings schema c rid [])}
      (cond-> {:ref      ref
               :op       :create-object
               :values   (peering/outbound-values m (:values obj))
               :markings (wire-markings schema c rid (:markings obj))}
        (seq links)
        (assoc :links (into {} (for [[l to-ids] (objects/links-from store (keys links) id)]
                                 [(get links l) (mapv #(peering/outbound-ref cw %) to-ids)])))))))

(defn- split-held [ceiling items]
  (let [{ok true held false} (group-by #(peering/admitted? ceiling (:markings %)) items)]
    [(vec ok) (vec held)]))

(defn send-source!
  [store c m]
  (let [schema (objects/schema store)
        rid    (objects/type-rid schema (:local-type m))
        feeds  (mapv #(objects/changes store rid % (:source-rev m))
                     (objects/source-origins store rid))
        rev    (reduce peering/later-rev (map :rev feeds))
        entries (into [] (mapcat :entries) feeds)
        ids    (vec (distinct (map (comp str :id) entries)))]
    (when (seq ids)
      (let [since (peering/oldest-change (map :at entries))
            objs  (objects/objects store rid ids)
            cw    (rows/crosswalk store (:connection-id c))
            links (link-rids schema m)
            items (mapv #(outbound-item store schema c m rid cw links % (get objs %)) ids)
            [ok held] (split-held (:ceiling c) items)
            job   {:connection-id (:connection-id c) :data-type :source-data :way :out
                   :local-type (:local-type m) :items (count ok) :held (count held)}]
        (try
          (doseq [batch (partition-all batch-max ok)]
            (wire/deliver! store (:trust c) peer-path
                           {:op :batch :connection-id (:connection-id c)
                            :data-type :source-data :type (:local-type m)
                            :from (own-fingerprint store) :items (vec batch)}))
          (rows/put-mapping! store (assoc m :source-rev rev))
          (rows/job! store (assoc job :state :ok
                                  :latency-ms (peering/lag since (System/currentTimeMillis))))
          (catch clojure.lang.ExceptionInfo e
            (rows/job! store (assoc job :state :failed :error (ex-message e)))))))))

(defn collect-edits!
  [store c m]
  (let [schema (objects/schema store)
        rid    (objects/type-rid schema (:local-type m))]
    (if (nil? (:action-rev m))
      (rows/put-mapping! store (assoc m :action-rev (objects/clock store rid)))
      (let [{:keys [rev entries]} (objects/changes store rid :edit (:action-rev m))
            mine   (objects/peer-action (:connection-id c))
            es     (remove #(= mine (get-in % [:actor :action])) entries)
            cw     (rows/crosswalk store (:connection-id c))
            links  (link-rids schema m)
            objs   (objects/objects store rid (distinct (map (comp str :id) es)))
            fp     (own-fingerprint store)
            items  (vec (for [{:keys [id op values at]} es
                              :let [id   (str id)
                                    obj  (get objs id)
                                    item (outbound-item store schema c m rid cw links id
                                                        (when-not (= :delete-object op)
                                                          (or obj {:values values})))
                                    item (if (= :update-object op)
                                           (update item :values
                                                   #(select-keys % (map name (keys values))))
                                           item)]
                              :when (or (not= :update-object op) (seq (:values item)))]
                          (assoc item :at at :from fp)))]
        (doseq [[i item] (map-indexed vector items)]
          (rows/enqueue! store {:connection-id (:connection-id c) :local-type (:local-type m)
                                :seq (+ (* 1000 (System/currentTimeMillis)) i)
                                :payload item}))
        (rows/put-mapping! store (assoc m :action-rev rev))))))

(defn send-edits!
  [store c now]
  (let [due (filter #(<= (long (or (:next-at %) 0)) (long now))
                    (rows/outbox store (:connection-id c)))]
    (vec (for [[t items] (group-by :local-type due)
               batch (partition-all batch-max items)
               :let [[ok held] (split-held (:ceiling c) (map :payload batch))
                     held-ids (set (map :ref held))
                     job {:connection-id (:connection-id c) :data-type :action-data :way :out
                          :local-type t :items (count ok) :held (count held)}]]
           (try
             (wire/deliver! store (:trust c) peer-path
                            {:op :batch :connection-id (:connection-id c)
                             :data-type :action-data :type t
                             :from (own-fingerprint store) :items ok})
             (rows/acknowledged! store (map :item-id batch))
             (rows/job! store (assoc job :state :ok :held (count held-ids)
                                     :latency-ms (peering/lag
                                                  (peering/oldest-change
                                                   (map (comp :at :payload) batch))
                                                  (System/currentTimeMillis))))
             (catch clojure.lang.ExceptionInfo e
               (doseq [i batch]
                 (rows/failed! store i (+ (long now) (peering/backoff-ms (:attempts i)))
                               (ex-message e)))
               (rows/job! store (assoc job :state :failed :error (ex-message e)))))))))

(defn- local-last
  [store c rid local-id]
  (when-let [{:keys [at via]} (objects/last-edit store rid local-id)]
    (cond
      (= via (:connection-id c))
      (let [w (rows/crosswalk-row store (:connection-id c) local-id)]
        {:at (or (:edit-at w) at) :from (or (:edit-from w) (:remote-fingerprint c))})
      via {:at at :from (:remote-fingerprint (rows/connection store via))}
      :else {:at at :from (own-fingerprint store)})))

(defn- create-markings!
  [store schema c names]
  (if (objects/create-markings! store schema (:connection-id c) names)
    nil
    (try (markings/declare! store (:connection-id c) names) nil
         (catch clojure.lang.ExceptionInfo e (or (ex-message e) "refused")))))

(defn- markings!
  [store schema c items]
  (let [plans   (mapv #(peering/marking-plan (:connection-id c) (:markings %) (:marking-map c))
                      items)
        created (apply merge (map :created plans))
        why     (when (seq created) (create-markings! store schema c (vals created)))
        made?   (nil? why)]
    (when (and made? (seq created))
      (rows/put-connection! store (update c :marking-map merge created)))
    {:plans plans :made? made? :why why}))

(defn settle-links!
  [store schema c]
  (when-let [es (seq (rows/pending store (:connection-id c)))]
    (let [done (objects/create-links! store schema (:connection-id c) es)]
      (rows/released! store (:connection-id c) done)
      (count done))))

(declare receive-objects!)

(defn receive!
  [store c {:keys [data-type] :as msg}]
  (if (= :media (keyword (name data-type)))
    (media/receive! store c msg)
    (receive-objects! store c msg)))

(defn- receive-objects!
  [store c {:keys [data-type type items from]}]
  (let [dt     (keyword (name data-type))
        m      (rows/mapping-by-remote store (:connection-id c) type)]
    (when-not (and m (peering/may-flow? c m dt :in))
      (refusal/refuse! :refusal.peering/data-type-not-taken
                       {:connection-id (:connection-id c) :type type :data-type dt}
                       {:data-type-name (name dt)}))
    (let [schema (objects/schema store)
          rid    (objects/type-rid schema (:local-type m))
          _      (when-let [p (first (peering/datasource-problems
                                       {dt :import} (objects/datasource-kind schema rid)))]
                   (refusal/refuse! :refusal.peering/stream-backed
                                    (merge (dissoc p :why :data-type) {:type type :data-type dt})
                                    {:peered (name (:data-type p))}))
          [ok held] (split-held (:ceiling c) items)
          {:keys [plans made? why]} (markings! store schema c ok)
          rev-cw (into {} (map (fn [[l r]] [r l])) (rows/crosswalk store (:connection-id c)))
          named  (for [[item {:keys [local created]}] (map vector ok plans)
                       :when (or made? (empty? created))]
                   (assoc item
                          :id (peering/inbound-id (:connection-id c) rev-cw (:ref item))
                          :values (peering/inbound-values m (:values item))
                          :markings local))
          unmade (- (count ok) (count named))
          [fresh old] (if (= :action-data dt)
                        (let [g (group-by #(peering/edit-stands?
                                            {:at (:at %) :from (or (:from %) from)}
                                            (local-last store c rid (:id %)))
                                          named)]
                          [(vec (get g true)) (vec (get g false))])
                        [(vec named) []])]
      (if (= :action-data dt)
        (objects/write-edits! store (:connection-id c) rid fresh)
        (objects/write-source! store (:connection-id c) rid fresh))
      (rows/walked! store (for [{:keys [id ref at] :as item} fresh]
                            (cond-> {:connection-id (:connection-id c) :local-id id
                                     :remote-id (:id ref) :local-type (:local-type m)}
                              (= :action-data dt) (assoc :edit-at at
                                                         :edit-from (or (:from item) from)))))
      (let [back (into {} (for [[l r] (:links m)
                                :let [rid (lookup/rid-of schema :link-types (keyword (name l)))]
                                :when rid]
                            [(name r) rid]))
            want (into {} (for [{:keys [id links]} fresh :when (seq links)]
                            [id (into {} (for [[wl refs] links
                                               :let [l (get back (name wl))]
                                               :when l]
                                           [l (mapv #(peering/inbound-id (:connection-id c)
                                                                         rev-cw %)
                                                    refs)]))]))]
        (let [{:keys [held]} (objects/write-links! store schema (:connection-id c) want)]
          (rows/hold! store (:connection-id c)
                      (set (for [[from by-link] want l (keys by-link)] [(str from) (str l)]))
                      held))
        (settle-links! store schema c))
      (let [r {:applied (count fresh) :held (+ (count held) unmade) :superseded (count old)}]
        (rows/job! store (merge {:connection-id (:connection-id c) :data-type dt :way :in
                                 :local-type (:local-type m) :state :ok
                                 :items (:applied r)}
                                (select-keys r [:held :superseded])
                                (when (and why (pos? unmade))
                                  {:error (str unmade " held: their marking could not be"
                                               " declared here — " why)})))
        r))))

(defn peered-ids
  [store obj-type]
  (let [schema (objects/schema store)
        nm     (or (objects/type-name schema (str obj-type)) (name obj-type))]
    (into #{} (map :local-id) (rows/crosswalk-of-type store nm))))

(defn run-connection!
  [store c now]
  (when (peering/flowing? c)
    (doseq [m (rows/mappings store (:connection-id c))]
      (when (peering/may-flow? c m :source-data :out)
        (send-source! store c m))
      (when (peering/may-flow? c m :action-data :out)
        (collect-edits! store c m)))
    (send-edits! store c now)
    (media/round! store c)
    (settle-links! store (objects/schema store) c)))

(defn- guarded!
  [store c f]
  (try (f)
       (catch InterruptedException e (throw e))
       (catch Exception e
         (rows/job! store {:connection-id (:connection-id c) :data-type :source-data
                           :way :out :state :failed :error (ex-message e)}))))

(defn round!
  [store]
  (doseq [c (rows/connections store)]
    (guarded! store c #(run-connection! store c (System/currentTimeMillis)))))

(defonce ^:private written (atom #{}))
(defonce ^:private poked? (atom false))
(defonce ^:private last-round (atom 0))

(defn noticed!
  [branch t poke!]
  (when (= objects/branch (str branch))
    (swap! written conj t)
    (when (compare-and-set! poked? false true) (poke!))))

(defn- taken! []
  (reset! poked? false)
  (first (reset-vals! written #{})))

(defn sent-now!
  [store types]
  (when (seq types)
    (let [schema (objects/schema store)
          names  (into #{} (map #(let [rid (or (objects/type-rid schema %)
                                                (objects/type-rid schema (name %))
                                                (objects/type-rid schema (keyword (name %))))]
                                   (or (some->> rid (objects/type-name schema)) (name %))))
                       types)]
      (doseq [c (rows/connections store) :when (peering/flowing? c)
              :let [ms (filterv #(names (:local-type %)) (rows/mappings store (:connection-id c)))]
              :when (seq ms)]
        (guarded! store c
                  #(do (doseq [m ms]
                         (when (peering/may-flow? c m :source-data :out) (send-source! store c m))
                         (when (peering/may-flow? c m :action-data :out)
                           (collect-edits! store c m)))
                       (send-edits! store c (System/currentTimeMillis))))))))

(defn tick!
  ([store] (tick! store 0 nil))
  ([store every-ms also]
   (sent-now! store (taken!))
   (let [now (System/currentTimeMillis)]
     (when (>= (- now (long @last-round)) (long every-ms))
       (reset! last-round now)
       (when also (also))
       (round! store)))))
