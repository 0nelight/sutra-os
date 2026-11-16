(ns sutra.capps.device
  (:require [sutra.kit.coerce :refer [->kw]]))

(defn multiple? [{:keys [mode]}] (= :multiple (->kw mode)))

(defn scan-step
  [{:keys [seen] :as st} {:keys [prompt?] :as cfg} code]
  (cond
    (nil? code)         {:state (assoc st :pending nil)}
    (some #{code} seen) {:state st}
    prompt?             {:state (assoc st :pending code)}
    :else               {:state  (update st :seen (fnil conj []) code)
                         :take   code
                         :close? (not (multiple? cfg))}))

(defn confirm-step
  [{:keys [pending] :as st} cfg]
  (if pending
    {:state  (-> st (assoc :pending nil) (update :seen (fnil conj []) pending))
     :take   pending
     :close? (not (multiple? cfg))}
    {:state st}))

(defn output-value
  [cfg seen]
  (if (multiple? cfg) (vec seen) (peek (vec seen))))

(defn location-string
  [lat lon]
  (str lat "," lon))

(defn location-preview
  [lat lon]
  (let [r #(/ (Math/round (* (double %) 10000)) 10000.0)]
    (str (r lat) ", " (r lon))))
