(ns sutra.streams.api
  (:refer-clojure :exclude [read reset!])
  (:require [sutra.port :as port]))

(set! *warn-on-reflection* true)

(def stream-schema
  [:map {:closed true} [:branch :string] [:stream [:re #"^[A-Za-z0-9_]{1,63}$"]]])

(def cursor-schema [:map-of nat-int? nat-int?])

(def append-schema
  [:sequential [:map {:closed true}
                [:partition nat-int?] [:key {:optional true} [:maybe :string]]
                [:at inst?] [:record map?]]])

(def read-schema
  [:map {:closed true}
   [:from {:optional true} cursor-schema]
   [:partition {:optional true} nat-int?]
   [:limit {:optional true} pos-int?]])

(def trim-schema
  [:map {:closed true} [:below cursor-schema] [:before {:optional true} inst?]])

(defprotocol Streams
  (-append! [s stream-ref entries])
  (-read [s stream-ref opts])
  (-ends [s stream-ref])
  (-stats [s stream-ref])
  (-trim! [s stream-ref opts])
  (-reset! [s stream-ref])
  (-drop! [s stream-ref]))

(defn- check! [schema x what] (port/asked! :streams schema x what))

(def ^:private offset-answer [:sequential [:map [:partition nat-int?] [:offset nat-int?]]])

(def ^:private record-answer
  [:sequential [:map [:partition nat-int?] [:offset nat-int?] [:at inst?] [:record map?]]])

(defn- one-offset-each
  [entries offsets]
  (and (= (count entries) (count offsets))
       (every? true? (map #(= (:partition %1) (:partition %2)) entries offsets))
       (every? (fn [[_ os]] (apply < (map :offset os))) (group-by :partition offsets))))

(defn- in-cursor-order [records]
  (= records (sort-by (juxt :partition :offset) records)))

(defn append! [s stream-ref entries]
  (let [entries (vec (check! append-schema entries :entries))]
    (port/answered! :streams offset-answer
                    (-append! s (check! stream-schema stream-ref :stream) entries)
                    :append
                    (partial one-offset-each entries))))
(defn read ([s stream-ref] (read s stream-ref {}))
  ([s stream-ref opts]
   (port/answered! :streams record-answer
                   (-read s (check! stream-schema stream-ref :stream)
                          (check! read-schema opts :read))
                   :read in-cursor-order)))
(defn ends [s stream-ref]
  (port/answered! :streams cursor-schema (-ends s (check! stream-schema stream-ref :stream)) :ends))
(def ^:private stats-answer [:map [:depth [:map-of nat-int? nat-int?]] [:last-at [:maybe inst?]]])
(defn stats [s stream-ref]
  (port/answered! :streams stats-answer (-stats s (check! stream-schema stream-ref :stream))
                  :stats))
(defn trim! [s stream-ref opts]
  (port/answered! :streams nat-int?
                  (-trim! s (check! stream-schema stream-ref :stream)
                          (check! trim-schema opts :trim))
                  :trim))
(defn reset! [s stream-ref] (-reset! s (check! stream-schema stream-ref :stream)))
(defn drop! [s stream-ref] (-drop! s (check! stream-schema stream-ref :stream)))

(defn advance
  [cursor records]
  (reduce (fn [c {:keys [partition offset]}] (update c partition (fnil max 0) (inc offset)))
          (or cursor {}) records))

(defn floor
  [cursors]
  (let [ps (into #{} (mapcat keys) cursors)]
    (into {} (for [p ps] [p (reduce min (map #(get % p 0) cursors))]))))
