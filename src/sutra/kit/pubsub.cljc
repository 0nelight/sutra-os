(ns sutra.kit.pubsub
  (:refer-clojure :exclude [count]))

(defn registry [] (atom {}))

(defn subscribe!
  [reg topic sink]
  (let [id (str (random-uuid))]
    (swap! reg assoc-in [topic id] sink)
    (fn [] (swap! reg update topic dissoc id))))

(defn count [reg topic]
  (clojure.core/count (get @reg topic)))

(defn publish!
  [reg topic msg]
  (doseq [[id sink] (get @reg topic)
          :when (contains? (get @reg topic) id)]
    (try (sink msg)
         (catch #?(:clj Throwable :cljs :default) _
           (swap! reg update topic dissoc id))))
  nil)

(defn clear! [reg]
  (reset! reg {})
  nil)
