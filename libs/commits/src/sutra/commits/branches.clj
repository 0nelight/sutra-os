(ns sutra.commits.branches
  (:require [sutra.world.db :as kdb]
            [sutra.world.delta :as delta]
            [sutra.world.index :as ix]
            [sutra.world.instance :as inst]
            [sutra.commits.loader :as lader]
            [sutra.commits.log :as log]
            [sutra.store.core.reads :as reads]
            [sutra.store.core.resources :as res]))

(set! *warn-on-reflection* true)

(defn- kept-under-its-id?
  [upgrade id]
  (= id (first (log/upgraded upgrade id {}))))

(defn head
  [db h b rows home]
  (cond-> (lader/head-db db h b rows) home (inst/overlaid (lader/head-db db h home []))))

(defn- unmet!
  [why data]
  (throw (ex-info (str "no branch states: " (name why)) (assoc data :precondition why))))

(def ^:private instance-sections (set inst/sections))

(defn- some-nodes [ids] (vec (take 5 ids)))

(defn- frame-base
  [db h theirs rev m t mdb tdb]
  (let [frame (fn [x] (into {} (map (fn [s] [s (delta/frame-value (kdb/world x) s)]))
                            (keys (kdb/world x))))]
    (cond
      (not (contains? t res/frame-id)) (frame tdb)
      (not (contains? m res/frame-id)) (frame mdb)
      :else (if-let [v (first (reads/resource-history db h theirs rev res/frame-id))]
              (:value v)
              ::unknown))))

(defn- frame-nodes
  [mdb tdb fb]
  (into #{}
        (comp (map delta/frame-node)
              (remove #(= (kdb/value mdb %) (kdb/value tdb %)
                          (get fb (second %) delta/absent))))
        (distinct (concat (keys (kdb/world mdb)) (keys (kdb/world tdb)) (keys fb)))))

(defn- at-rev
  [hs rev]
  (first (filter #(<= (:rev %) rev) hs)))

(defn- at-fork
  [db h theirs rev upgrade [sec :as id]]
  (let [hs    (reads/resource-history db h theirs nil id)
        since (last (filter #(> (:rev %) rev) hs))
        nm    (some-> (get (:value since) (ix/name-field sec)) name keyword)
        named (when nm (at-rev (reads/resource-history db h theirs rev [sec nm]) rev))]
    (if-let [at (at-rev hs rev)]
      (second (log/upgraded upgrade id (:value at)))
      (cond
        (or (some? (:from since))
            (nil? (:to since)))  ::unknown
        (:value named)          (let [[node e] (log/upgraded upgrade [sec nm] (:value named))]
                                  (if (= node id) e ::unknown))
        :else                   nil))))

(defn- raw-states
  [db h mine theirs rev rows]
  (let [m       (reads/changed-since db h mine nil nil)
        t       (reads/changed-since db h theirs rev nil)
        upgrade (or (:upgrade db) identity)]
    (when-not m (unmet! :log-short {:branch mine}))
    (when-not t (unmet! :log-short {:branch theirs :rev rev}))
    (let [ids (disj (into (set m) t) res/frame-id)]
      (when-let [rekeyed (seq (remove #(kept-under-its-id? upgrade %) ids))]
        (unmet! :rekeyed {:nodes (some-nodes rekeyed)}))
      (let [mdb  (lader/head-db db h mine rows)
            tdb  (lader/head-db db h theirs rows)
            fb   (when (or (contains? m res/frame-id) (contains? t res/frame-id))
                   (frame-base db h theirs rev m t mdb tdb))
            _    (when (= ::unknown fb)
                   (unmet! :no-history {:branch theirs :rev rev :nodes [res/frame-id]}))
            fns  (if fb (frame-nodes mdb tdb fb) #{})
            base (fn [id]
                   (cond
                     (delta/frame-node? id)  (get fb (second id) delta/absent)
                     (not (contains? t id))  (kdb/entry tdb id)
                     (not (contains? m id))  (kdb/entry mdb id)
                     :else (at-fork db h theirs rev upgrade id)))
            nodes (into ids fns)
            es    (into {} (map (juxt identity base)) nodes)]
        (when-let [unknown (seq (keep (fn [[id e]] (when (= ::unknown e) id)) es))]
          (unmet! :no-history {:branch theirs :rev rev :nodes (some-nodes unknown)}))
        {:mine mdb :theirs tdb :nodes nodes :base es}))))

(defn- at-nodes
  [es]
  (let [e (kdb/db {})] (kdb/db (kdb/world (kdb/db-with e (kdb/changes-to e es))))))

(defn- homed
  [{:keys [mine theirs nodes base]} F]
  (let [m     (inst/overlaid mine F)
        t     (inst/overlaid theirs F)
        own   (into {} (remove (fn [[n]] (and (delta/frame-node? n)
                                              (contains? instance-sections (second n)))))
                    base)
        braw  (kdb/db-with theirs (kdb/changes-to theirs own))
        b     (-> braw (inst/overlaid F) (inst/overlaid t))
        moved (filter #(not= (kdb/entry b %) (kdb/entry m %) (kdb/entry t %))
                      (reduce into #{} (map inst/held [mine theirs braw F])))
        apart (into nodes moved)]
    {:mine m :theirs t :nodes nodes :apart apart
     :base (at-nodes (into {} (map (juxt identity #(kdb/value b %))) apart))}))

(defn states
  ([db h mine theirs rev] (states db h mine theirs rev []))
  ([db h mine theirs rev rows] (states db h mine theirs rev rows nil))
  ([db h mine theirs rev rows home]
   (let [{:keys [base] :as raw} (raw-states db h mine theirs rev rows)]
     (if home
       (homed raw (lader/head-db db h home []))
       (assoc raw :base (at-nodes base) :apart (:nodes raw))))))
