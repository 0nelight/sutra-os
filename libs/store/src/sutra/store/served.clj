(ns sutra.store.served
  (:require [sutra.store.api :as api]))

(set! *warn-on-reflection* true)

(defn- lane-of [lane m]
  (if (and lane (nil? (:lane m))) (assoc m :lane lane) m))

(defrecord Served [db lane fences]
  api/Db
  (-snapshot [_ basis] (api/-snapshot db (lane-of lane basis)))
  (-prepare [_ tx]
    (api/-prepare db (cond-> (lane-of lane tx)
                       (seq fences) (update :fences (fnil into []) fences))))
  (-branch! [_ op] (api/-branch! db op))
  (-read-at [_ basis authority q] (api/-read-at db (lane-of lane basis) authority q)))

(defn- unwrapped [db]
  (if (instance? Served db) db (->Served db nil [])))

(defn on-lane
  [db lane]
  (assoc (unwrapped db) :lane lane))

(defn fenced
  [db fences]
  (if (seq fences) (update (unwrapped db) :fences into fences) db))

(defn lane
  [db]
  (when (instance? Served db) (:lane db)))

(deftype Answering [basis answer snap]
  api/Snapshot
  (-basis [_] basis)
  (-answer [_ authority q] (answer authority q))
  (-overlay [_ staged]
    (if snap
      (api/-overlay snap staged)
      (throw (ex-info "a read door has no snapshot to overlay" {:basis basis}))))
  (-close [_] nil)
  java.io.Closeable
  (close [_] nil))

(defrecord Reading [db basis answer snap]
  api/Db
  (-snapshot [_ b]
    (if (and (nil? (:rev b)) (= (str (:branch b)) (:branch basis)))
      (->Answering basis answer snap)
      (api/-snapshot db b)))
  (-prepare [_ tx] (api/-prepare db tx))
  (-branch! [_ op] (api/-branch! db op))
  (-read-at [_ b authority q] (api/-read-at db b authority q)))

(defn reading
  [db branch answer]
  (->Reading db {:branch (str branch)} answer nil))

(defn holding
  [db snap]
  (->Reading db (api/-basis snap) #(api/-answer snap %1 %2) snap))

(defn reads-held?
  [db]
  (instance? Reading db))
