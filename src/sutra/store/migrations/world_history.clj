(ns sutra.store.migrations.world-history
  (:require [sutra.store.core.replay :as replay]))

(set! *warn-on-reflection* true)

(defonce ^:private job (atom {:state :idle}))

(defn status
  []
  @job)

(defn start!
  [db {:keys [apply? verify? repair? drop-texts? dedupe? threads]}]
  (let [begun (swap-vals! job (fn [j] (if (= :running (:state j))
                                        j
                                        {:state :running :apply? (boolean apply?)
                                         :verify? (boolean verify?)
                                         :repair? (boolean repair?)
                                         :drop-texts? (boolean drop-texts?)
                                         :dedupe? (boolean dedupe?)
                                         :started (str (java.time.Instant/now))
                                         :branches []})))]
    (when (not= :running (:state (first begun)))
      (future
        (try
          (let [o (cond-> {:on-branch #(swap! job update :branches conj %)}
                    threads (assoc :threads threads))
                r (cond
                    repair? (replay/repair! db o)
                    drop-texts? (replay/drop-texts! db (dissoc o :on-branch))
                    dedupe? (replay/dedupe-versions! db)
                    verify? (replay/verify-all! db o)
                    :else   (replay/replay! db (assoc o :dry-run? (not apply?))))]
            (swap! job assoc :state :done :result (dissoc r :branches)))
          (catch Throwable t
            (swap! job assoc :state :failed :error (or (ex-message t) (str t)))))))
    (status)))
