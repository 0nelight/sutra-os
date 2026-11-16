(ns sutra.foundation.live-trace
  (:require [sutra.kit.attempt :as attempt]
            [clojure.string :as str]
            [sutra.foundation.encoding :as enc]))

(set! *warn-on-reflection* true)

(def ^:const capacity
  1024)

(def ^:const preview-chars
  400)

(defonce ^:private ring
  (atom {:seq 0 :events []}))

(defn clip
  [x]
  (when (some? x)
    (enc/elide preview-chars (-> (str x) (str/replace #"\s+" " ") str/trim))))

(defn- appended
  [{:keys [seq events]} m at]
  (let [n (inc seq)]
    {:seq    n
     :events (enc/conj-capped events
                              (assoc m :seq n :at at :level (or (:level m) :info))
                              capacity)}))

(defonce ^:private tap (atom nil))

(defn tap!
  [f]
  (reset! tap f))

(def ^:dynamic *listener*
  nil)

(defn emit!
  [m]
  (let [n (:seq (swap! ring appended m (System/currentTimeMillis)))]
    (when *listener* (*listener* m))
    (when-let [f @tap]
      (try (f m)
           (catch Throwable t
             (attempt/rethrow-fatal! t)
             (binding [*out* *err*] (println (str "[live-trace] tap failed: " (ex-message t)))))))
    n))

(defn ingest!
  [events]
  (doseq [{:keys [at] :as m} events]
    (swap! ring appended (dissoc m :seq :at) (or at (System/currentTimeMillis)))))

(defn packets
  [events max-bytes]
  (let [size #(alength (.getBytes (pr-str %) "UTF-8"))]
    (->> events
         (reduce (fn [{:keys [cur n out]} e]
                   (let [k (inc (long (size e)))]
                     (cond (> k max-bytes)            {:cur cur :n n :out out}
                           (> (+ n k) max-bytes)      {:cur [e] :n k :out (conj out cur)}
                           :else                      {:cur (conj cur e) :n (+ n k) :out out})))
                 {:cur [] :n 0 :out []})
         ((fn [{:keys [cur out]}] (cond-> out (seq cur) (conj cur)))))))

(defn since
  [after]
  (let [{:keys [seq events]} @ring
        after   (long (or after 0))
        fresh   (filterv #(> (:seq %) after) events)
        oldest  (:seq (first events))
        dropped (if (and oldest (pos? after) (> (dec oldest) after))
                  (- (dec oldest) after)
                  0)]
    {:seq seq :events fresh :dropped dropped}))

(defn reset-ring!
  []
  (reset! ring {:seq 0 :events []}))
