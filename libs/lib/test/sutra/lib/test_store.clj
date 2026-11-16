(ns sutra.lib.test-store
  (:require [sutra.types.world-rules :as rules]
            [sutra.store.mem :as smem]))

(defn fixed-clock
  []
  (let [n (atom 0)]
    {:fresh-uuid #(str "u-" (swap! n inc))
     :now        (constantly (java.time.Instant/parse "2026-01-01T00:00:00Z"))
     :zone       (constantly java.time.ZoneOffset/UTC)}))

(def pg? (= "pg" (System/getProperty "sutra.lib.store")))

(def ^:private server
  (delay
    (let [builder (requiring-resolve 'sutra.lib.test-store-pg/start!)]
      (builder))))

(defn fresh
  [clock]
  (if pg?
    ((requiring-resolve 'sutra.lib.test-store-pg/fresh) @server (rules/store-opts clock))
    (smem/store smem/empty-seed (rules/store-opts clock))))
