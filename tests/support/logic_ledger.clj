(ns support.logic-ledger
  (:require [sutra.automation.logic-events :as logic-events]))

(defn fake
  []
  (let [rows (atom {})]
    [rows
     {#'logic-events/claim!
      (fn [_st _bh auto oid fp {:keys [manual?]}]
        (let [k [auto (str oid) fp] r (get @rows k)]
          (when (or (nil? r)
                    (and (= "failed" (:status r)) (or manual? (:due? r))))
            (let [claim {:event-id (or (:event-id r) (str "ev-" (count @rows)))
                         :attempts (or (:attempts r) 0)}]
              (swap! rows assoc k (merge r claim {:status "running" :due? false}))
              claim))))
      #'logic-events/done!
      (fn [_st _bh auto oid fp outcome & _]
        (swap! rows update [auto (str oid) fp] assoc :status "done" :outcome outcome :error nil))
      #'logic-events/failed!
      (fn [_st _bh auto oid fp {:keys [attempts error transient?]}]
        (let [n (inc (or attempts 0))]
          (swap! rows update [auto (str oid) fp] assoc :status "failed" :attempts n :error error
                 :transient transient?)
          {:attempts n :retry-in-ms (when transient? (logic-events/cool-down-ms n))}))
      #'logic-events/fallback!
      (fn [_st _bh auto oid fp r & _] (swap! rows update [auto (str oid) fp] assoc :fallback r))
      #'logic-events/state-of
      (fn [_st _bh auto oids]
        (let [ids (set (map str oids))]
          (group-by :object-id (for [[[a o fp] r] @rows :when (and (= a auto) (ids o))]
                                 (assoc r :object-id o :fingerprint fp)))))}]))
