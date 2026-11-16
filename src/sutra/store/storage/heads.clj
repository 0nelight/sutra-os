(ns sutra.store.storage.heads
  (:require [sutra.catalog.world-columns :as kc]
            [sutra.catalog.world-derived :as kd]
            [sutra.commits.loader :as lader]
            [sutra.types.schema :as sch]))

(set! *warn-on-reflection* true)

(defn- head
  [db h b _read]
  (kc/world-of (lader/head-db db h b kc/rows)))

(defn- held-head
  [db h b old _whole]
  (let [d (lader/head-db db h b [kd/fixed kd/signature])]
    (when-let [ns (seq (kd/unfixed d))]
      (throw (ex-info (str "the head of " b " is not its own derivation")
                      {:precondition :head-not-derived :branch b :nodes (vec (take 5 ns))})))
    {:W (sch/indexed old) :st (kd/sig-state d)}))

(def store-opts
  {:head head :held-head held-head})
