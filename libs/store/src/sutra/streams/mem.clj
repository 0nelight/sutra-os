(ns sutra.streams.mem
  (:require [sutra.streams.api :as s]))

(set! *warn-on-reflection* true)

(defn- k [{:keys [branch stream]}] [branch stream])

(defn- place
  [next entries]
  (reduce (fn [[nx out] {:keys [partition] :as e}]
            (let [o (get nx partition 0)]
              [(assoc nx partition (inc o)) (conj out (assoc e :offset o))]))
          [(or next {}) []] entries))

(defn- trimmed
  [records below before]
  (into (sorted-map)
        (remove (fn [[[p o] {:keys [at]}]]
                  (and (< o (get below p 0)) (or (nil? before) (neg? (compare at before))))))
        records))

(defrecord MemStreams [state]
  s/Streams
  (-append! [_ stream-ref entries]
    (let [[old] (swap-vals! state update (k stream-ref)
                            (fn [{:keys [next records]}]
                              (let [[next' placed] (place next entries)]
                                {:next    next'
                                 :records (into (or records (sorted-map))
                                                (map (fn [{:keys [partition offset] :as e}]
                                                       [[partition offset] e]))
                                                placed)})))]
      (mapv #(select-keys % [:partition :offset])
            (second (place (get-in old [(k stream-ref) :next]) entries)))))
  (-read [_ stream-ref {:keys [from partition limit] :or {limit 1000}}]
    (into []
          (comp (map val)
                (filter #(and (or (nil? partition) (= partition (:partition %)))
                              (>= (:offset %) (get from (:partition %) 0))))
                (take limit))
          (get-in @state [(k stream-ref) :records])))
  (-ends [_ stream-ref] (or (get-in @state [(k stream-ref) :next]) {}))
  (-stats [_ stream-ref]
    (let [rs (vals (get-in @state [(k stream-ref) :records]))]
      {:depth   (into (sorted-map) (frequencies (map :partition rs)))
       :last-at (when (seq rs) (reduce #(if (pos? (compare %1 %2)) %1 %2) (map :at rs)))}))
  (-trim! [_ stream-ref {:keys [below before]}]
    (let [[old new] (swap-vals! state
                                #(if (contains? % (k stream-ref))
                                   (update-in % [(k stream-ref) :records] trimmed below before)
                                   %))]
      (- (count (get-in old [(k stream-ref) :records]))
         (count (get-in new [(k stream-ref) :records])))))
  (-reset! [_ stream-ref] (swap! state assoc-in [(k stream-ref) :records] (sorted-map)) nil)
  (-drop! [_ stream-ref] (swap! state dissoc (k stream-ref)) nil))

(defn streams
  []
  (->MemStreams (atom {})))
