(ns sutra.types.stream
  (:require [clojure.string :as str]))

(def stream-types
  {:low-latency     {:label "Low latency"     :batch-ms 0}
   :high-throughput {:label "High throughput" :batch-ms 50}})

(def semantics
  #{:at-least-once :exactly-once})

(def max-record-bytes
  1048576)

(def max-partitions
  16)

(def default-spec
  {:partitions       1
   :stream-type      :low-latency
   :semantics        :at-least-once
   :compressed?      false
   :retention-ms     3600000
   :archive-every-ms 60000})

(defn- kws [xs] (into [] (comp (remove nil?) (map keyword)) xs))

(defn normalize
  [spec]
  (when spec
    (let [pk       (:primary-key spec)
          dedup    (kws (:columns pk))
          part     (kws (:partition-keys spec))
          together (into part (remove (set part)) dedup)]
      (cond-> (merge default-spec
                     (dissoc spec :partition-keys :primary-key)
                     {:partitions (max 1 (min max-partitions (long (or (:partitions spec) 1))))})
        (seq together) (assoc :partition-keys together)
        (seq dedup)    (assoc :primary-key
                              (cond-> {:columns dedup}
                                (:deletion-column pk)
                                (assoc :deletion-column (keyword (:deletion-column pk)))))))))

(defn cdc?
  [spec]
  (boolean (seq (get-in spec [:primary-key :columns]))))

(defn dedup-columns
  [spec]
  (get-in spec [:primary-key :columns]))

(defn deletion-column
  [spec]
  (get-in spec [:primary-key :deletion-column]))

(defn- deleted-value?
  [v]
  (cond
    (nil? v)     false
    (boolean? v) v
    (number? v)  (not (zero? v))
    (string? v)  (not (contains? #{"" "0" "false" "f" "no" "n"} (str/lower-case v)))
    :else        true))

(defn deleted?
  [spec record]
  (boolean (when-let [col (deletion-column spec)]
             (deleted-value? (get record col)))))

(defn key-of
  [spec record]
  (when-let [cols (seq (:partition-keys spec))]
    (str/join "" (map #(str (get record %)) cols))))

(defn dedup-key-of
  [spec record]
  (when-let [cols (seq (dedup-columns spec))]
    (str/join "" (map #(str (get record %)) cols))))

(defn- fnv1a
  [s]
  (reduce (fn [h c]
            (-> (bit-xor h (bit-and (int c) 0xff))
                (* 16777619)
                (bit-and 0xffffffff)))
          2166136261
          (seq (str s))))

(defn partition-for
  [spec record n]
  (let [p (long (:partitions spec 1))]
    (if (<= p 1)
      0
      (if-let [k (key-of spec record)]
        (int (mod (fnv1a k) p))
        (int (mod (long n) p))))))

(defn record-problems
  [spec columns record]
  (let [declared (set columns)]
    (if-not (map? record)
      ["a record is a map of column to value"]
      (-> []
          (into (for [k (keys record)
                      :when (and (seq declared) (not (contains? declared k)))]
                  (str "no column " k " on this stream — declared: "
                       (str/join ", " (sort (map name declared))))))
          (into (for [k (dedup-columns spec)
                      :when (nil? (get record k))]
                  (str "the primary-key column " k " is null — a changelog record"
                       " without its key cannot be resolved into a current view")))))))

(defn current-view
  [spec records]
  (if-not (cdc? spec)
    (vec records)
    (->> records
         (reduce (fn [acc r] (assoc acc (dedup-key-of spec r) r)) (array-map))
         vals
         (into [] (remove #(deleted? spec %))))))
