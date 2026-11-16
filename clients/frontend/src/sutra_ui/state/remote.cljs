(ns sutra-ui.state.remote
  (:require [sutra.kit.remote :as rd]
            [sutra-ui.api :as api]))

(defn fail
  ([prev e] (fail prev e (.-message e)))
  ([prev e reason]
   (if (api/aborted-error? e) prev (rd/after-failure prev reason))))

(defonce ^:private in-flight (atom {}))

(defn load!
  [holder ask pick {:keys [reason report then dedup current?]}]
  (if-let [p (and dedup (get @in-flight dedup))]
    p
    (let [current? (or current? (constantly true))
          p (-> (ask)
                (.then (fn [a]
                         (when (current?)
                           (reset! holder (pick a))
                           (when then (then a)))
                         a))
                (.catch (fn [e]
                          (when (current?)
                            (swap! holder fail e (if reason (reason e) (.-message e))))
                          (when (and report (not (api/aborted-error? e))) (report e))
                          nil)))]
      (when dedup
        (swap! in-flight assoc dedup p)
        (.finally p #(swap! in-flight (fn [m] (cond-> m (identical? p (get m dedup))
                                                 (dissoc dedup))))))
      p)))
