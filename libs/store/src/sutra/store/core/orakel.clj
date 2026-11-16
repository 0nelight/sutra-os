(ns sutra.store.core.orakel
  (:require [sutra.types.schema :as sch]))

(set! *warn-on-reflection* true)

(def on?
  (some? (System/getenv "SUTRA_STORE_ORACLE")))

(def ^:private runs (atom {}))

(defn ran!
  [k]
  (let [n (get (swap! runs update k (fnil inc 0)) k)]
    (when (or (= 1 n) (zero? (mod n 100)))
      (binding [*out* *err*] (println "sutra.store oracle" (pr-str {k n}))))))

(defn derived!
  [db A W sig]
  (let [whole (sch/indexed ((:derive db) A))]
    (ran! :stepped)
    (when (not= whole W)
      (throw (ex-info "the stepped world is not the derived one" {:oracle :derived})))
    (when (not= ((:signature db) whole) sig)
      (throw (ex-info "the stepped signature is not the whole one" {:oracle :signature})))))

(defn decls!
  [view W st]
  (when (not= (:decls st) (:decls ((:init view) W)))
    (throw (ex-info "the stepped declarations are not the whole ones" {:oracle :decls}))))

(defn touched!
  [sig0 sig touched]
  (let [outside #(reduce-kv (fn [s sec ks] (update s sec (partial apply dissoc) ks)) % touched)]
    (when (not= (outside sig0) (outside sig))
      (throw (ex-info "the signature differs outside what its step touched"
                      {:oracle :sig-touched})))))

(defn held!
  ([db old W] (held! db old W nil))
  ([db old W st]
   (ran! :held)
   (let [whole (sch/indexed ((:derive db) old))]
     (when (not= whole W)
       (throw (ex-info "the held head is not the derived one" {:oracle :held})))
     (when (and st (not= st ((get-in db [:sig-view :init]) whole)))
       (throw (ex-info "the held signature state is not the whole one"
                       {:oracle :held-sig}))))))
