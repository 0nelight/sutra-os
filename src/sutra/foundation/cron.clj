(ns sutra.foundation.cron
  (:require [sutra.kit.time :as time]
            [clojure.string :as str])
  (:import (java.time LocalDateTime ZoneId Instant)
           (java.time.temporal ChronoUnit)))

(set! *warn-on-reflection* true)

(defn- parse-field
  [s lo hi]
  (reduce
   (fn [acc part]
     (let [[rng step] (str/split part #"/")
           step (if step (Long/parseLong step) 1)
           [a b] (cond
                   (= rng "*")             [lo hi]
                   (str/includes? rng "-") (let [[x y] (str/split rng #"-")]
                                             [(Long/parseLong x) (Long/parseLong y)])
                   :else                   (let [v (Long/parseLong rng)] [v v]))]
       (into acc (map int) (range a (inc b) step))))
   #{} (str/split (str/trim s) #",")))

(defn matcher
  [cron]
  (try
    (let [parts (str/split (str/trim cron) #"\s+")]
      (if (not= 5 (count parts))
        (constantly false)
        (let [[mi h dom mo dow] parts
              mi-s  (parse-field mi 0 59)
              h-s   (parse-field h 0 23)
              mo-s  (parse-field mo 1 12)
              dom-s (parse-field dom 1 31)
              dow-s (let [s (parse-field dow 0 7)] (if (s 7) (conj s (int 0)) s))
              dom?  (not= (str/trim dom) "*")
              dow?  (not= (str/trim dow) "*")]
          (fn [^LocalDateTime ldt]
            (and (boolean (mi-s (.getMinute ldt)))
                 (boolean (h-s (.getHour ldt)))
                 (boolean (mo-s (.getMonthValue ldt)))
                 (let [dom-ok (boolean (dom-s (.getDayOfMonth ldt)))
                       dow-ok (boolean (dow-s (mod (.getValue (.getDayOfWeek ldt)) 7)))]
                   (if (and dom? dow?) (or dom-ok dow-ok) (and dom-ok dow-ok))))))))
    (catch Exception _ (constantly false))))

(defn ->ldt
  [ts ^ZoneId zone]
  (if (instance? Instant ts)
    (LocalDateTime/ofInstant ts zone)
    (time/->utc-local ts)))

(def ^:private horizon-minutes
  (* 60 24 31))

(defn due?
  ([cron last-fired ^LocalDateTime now zone]
   (let [hit? (matcher cron)
         now  (.truncatedTo now ChronoUnit/MINUTES)]
     (if-let [^LocalDateTime lf (->ldt last-fired zone)]
       (let [floor (.minusMinutes now horizon-minutes)
             from  (.plusMinutes (.truncatedTo lf ChronoUnit/MINUTES) 1)]
         (loop [^LocalDateTime t (if (.isBefore from floor) floor from)]
           (cond
             (.isAfter t now) false
             (hit? t)         true
             :else            (recur (.plusMinutes t 1)))))
       (hit? now)))))

(defn next-fire
  ([cron ^LocalDateTime now]
   (when (and (string? cron) (not (str/blank? cron)))
     (let [start (.plusMinutes (.truncatedTo now ChronoUnit/MINUTES) 1)
           hit?  (matcher cron)]
       (loop [t start, n 0]
         (cond
           (>= n horizon-minutes) nil
           (hit? t)               t
           :else                  (recur (.plusMinutes t 1) (inc n))))))))

(defn next-fire-of
  ([{:keys [cron every-ms paused last-fired-at]} ^LocalDateTime now zone]
   (when-not paused
     (cond
       (and (string? cron) (not (str/blank? cron))) (next-fire cron now)
       (and every-ms (pos? (long every-ms)))
       (let [^LocalDateTime base (or (->ldt last-fired-at zone) now)
             nxt  (.plusNanos base (* (long every-ms) 1000000))]
         (if (.isAfter nxt now) nxt now))
       :else nil))))
