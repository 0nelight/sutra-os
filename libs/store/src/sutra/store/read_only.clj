(ns sutra.store.read-only
  (:require [sutra.datasets.api :as datasets]
            [sutra.store.api :as api]
            [sutra.streams.api :as streams]))

(set! *warn-on-reflection* true)

(defn- refusal [what data]
  (api/refusal :read-only (str "this store only reads; " what " was refused") data))

(defrecord ReadOnlyDb [db]
  api/Db
  (-snapshot [_ basis] (api/-snapshot db basis))
  (-prepare [_ tx]
    (api/refused :read-only "this store only reads; a commit was refused"
                 {:branch (:branch tx)}))
  (-branch! [_ op] (throw (refusal "a branch op" {:op (:op op)})))
  (-read-at [_ basis authority q] (api/-read-at db basis authority q)))

(defrecord ReadOnlyDatasets [d]
  datasets/Datasets
  (-profile [_] (datasets/-profile d))
  (-layout! [_ branch _] (throw (refusal "a dataset layout" {:branch branch})))
  (-list [_ branch opts] (datasets/-list d branch opts))
  (-inspect [_ branch decls] (datasets/-inspect d branch decls))
  (-write! [_ addr tx]
    (if (:scratch? addr)
      (datasets/-write! d addr tx)
      (throw (refusal "a dataset write" {:addr addr}))))
  (-read [_ addr opts] (datasets/-read d addr opts))
  (-columns [_ addr] (datasets/-columns d addr))
  (-count [_ addr opts] (datasets/-count d addr opts))
  (-version [_ addr] (datasets/-version d addr))
  (-delta [_ addr lo hi] (datasets/-delta d addr lo hi))
  (-horizon [_ addr] (datasets/-horizon d addr))
  (-retire-below! [_ addr _] (throw (refusal "a retention mark" {:addr addr})))
  (-sweep! [_ addr _] (throw (refusal "a retention sweep" {:addr addr})))
  (-expire! [_ addr _] (throw (refusal "a retention expiry" {:addr addr})))
  (-history [_ addr opts] (datasets/-history d addr opts))
  (-query [_ plan] (datasets/-query d plan))
  (-restore! [_ addr _] (throw (refusal "a dataset restore" {:addr addr})))
  (-fork! [_ from to] (throw (refusal "a dataset fork" {:from from :to to})))
  (-drop! [_ branch] (throw (refusal "a dataset drop" {:branch branch})))
  (-drop-dataset! [_ addr]
    (if (:scratch? addr)
      (datasets/-drop-dataset! d addr)
      (throw (refusal "a dataset drop" {:addr addr})))))

(defrecord ReadOnlyStreams [s]
  streams/Streams
  (-append! [_ ref _] (throw (refusal "a stream append" {:stream ref})))
  (-read [_ ref opts] (streams/-read s ref opts))
  (-ends [_ ref] (streams/-ends s ref))
  (-stats [_ ref] (streams/-stats s ref))
  (-trim! [_ ref _] (throw (refusal "a stream trim" {:stream ref})))
  (-reset! [_ ref] (throw (refusal "a stream reset" {:stream ref})))
  (-drop! [_ ref] (throw (refusal "a stream drop" {:stream ref}))))

(defrecord ReadStore [db branch clock datasets streams instance-keys instance-home view])

(defn read-only?
  [store]
  (instance? ReadStore store))

(defn of
  [store]
  (if (read-only? store)
    store
    (map->ReadStore (-> (into {} store)
                        (update :db ->ReadOnlyDb)
                        (update :datasets #(some-> % ->ReadOnlyDatasets))
                        (update :streams #(some-> % ->ReadOnlyStreams))))))
