(ns sutra.builds.ledger.marks
  (:refer-clojure :exclude [get])
  (:require [sutra.kit.digest :as digest]
            [sutra.kit.tree :as tree]
            [sutra.store.control.collections :as collections]
            [sutra.store.api :as api]
            [sutra.store.machine :as machine]))

(set! *warn-on-reflection* true)

(def ^:private coll (:coll collections/consumer-marks))
(def ^:private actor {:principal "system/marks"})

(defn fingerprint
  [observed]
  (digest/sha256-hex (pr-str (tree/canonical {:leaf #(if (or (uuid? %) (inst? %)) (str %) %)}
                                             observed))))

(defn- k [branch owner] [(str branch) (pr-str owner)])

(defn- current
  [s branch owner]
  (let [stored (api/read s nil [:records/get coll (k branch owner)])]
    [(if (some? stored) stored api/absent)
     (or stored ::none)]))

(defn- write [branch owner expect fp]
  {:actor actor :records [{:coll coll :key (k branch owner) :expect expect :value fp}]})

(defn- command! [store decide]
  (machine/command! (:db store) {:branch "main"} decide))

(defn get
  [store branch owner]
  (with-open [s ^java.io.Closeable (api/snapshot (:db store) {:branch "main"})]
    (second (current s branch owner))))

(defn put!
  [store branch owner fp]
  (command! store (fn [s] {:tx (write branch owner (first (current s branch owner)) fp)}))
  fp)

(defn record!
  [store branch owner observed]
  (put! store branch owner (fingerprint observed)))

(defn advance!
  [store branch owner from observed]
  (:result
   (command! store (fn [s]
                  (let [[stored mark] (current s branch owner)]
                    (if (= from mark)
                      {:result true :tx (write branch owner stored (fingerprint observed))}
                      {:result false}))))))

(defn moved?
  [mark observed]
  (and (not= ::none mark) (not= mark (fingerprint observed))))

(defn observe!
  [store branch owner observed]
  (let [m (get store branch owner)]
    (when (= ::none m) (record! store branch owner observed))
    m))

(defn consume!
  [store branch owner observed act]
  (let [m (get store branch owner)]
    (cond
      (= ::none m)                 (do (record! store branch owner observed) ::first)
      (= m (fingerprint observed)) ::same
      :else                        (let [r (act)] (advance! store branch owner m observed) r))))
