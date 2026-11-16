(ns sutra.builds.ledger.facts
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

(def channels
  [{:key :rows-in          :init {}  :by :consumer  :fold :sum   :job-fact? true}
   {:key :code-sha         :init {}  :by :transform :fold :last  :job-fact? true}
   {:key :pending-observed :init {}  :by :consumer  :fold :last  :job-fact? true}
   {:key :job-log          :init {}  :by :consumer  :fold :line  :job-fact? true
    :prep #(not-empty (str/trim (str %)))}
   {:key :job-spans        :init {}  :by :consumer  :fold :cat   :job-fact? true}
   {:key :dropped-cols     :init {}  :by :transform :fold :union :prep not-empty}
   {:key :input-sigs       :init {}  :by :transform :fold :first}
   {:key :consumed-cursors :init []  :fold :conj}
   {:key :bootstrapped     :init #{} :fold :conj}
   {:key :snapshot-run     :init #{} :fold :conj}
   {:key :incomplete       :init #{} :fold :conj}
   {:key :trace            :init []  :fold :conj :opt-in? true}])

(def deltas
  [{:key :obj-decls            :init [] :fold :into}
   {:key :link-decls           :init [] :fold :into}
   {:key :restriction-failures :init [] :fold :into}
   {:key :pk-index             :init {} :fold :merge}
   {:key :writeback-index      :init {} :fold :merge}
   {:key :rows                 :init {} :fold :merge}
   {:key :conn-pairs           :init {} :fold :merge}
   {:key :staged               :init {} :fold :merge}
   {:key :landed               :init {} :fold :merge}])

(def ^:private folds
  {:conj  conj
   :into  into
   :cat   (fn [prev v] (into (or prev []) v))
   :merge merge
   :sum   (fnil + 0)
   :last  (fn [_ v] v)
   :first (fn [prev v] (if (some? prev) prev v))
   :union (fn [prev v] (into (or prev #{}) v))
   :line  (fn [prev v] (if (seq prev) (str prev "\n" v) v))})

(defn- channel-of
  [table k]
  (or (some #(when (= k (:key %)) %) table)
      (throw (ex-info (str "no channel " (pr-str k) " to fold into")
                      {:key k :known (mapv :key table)}))))

(defn start
  [table opt-in?]
  (into {} (for [{:keys [key init] :as ch} table
                 :when (or opt-in? (not (:opt-in? ch)))]
             [key init])))

(defn fold-in
  [table state k at v]
  (let [{:keys [by fold prep]} (channel-of table k)
        v (if prep (prep v) v)]
    (cond
      (not (contains? state k)) state
      (nil? v)                  state
      (nil? by)                 (update state k (folds fold) v)
      (nil? at)                 state
      :else                     (update-in state [k at] (folds fold) v))))

(def job-fact-keys
  (into [] (comp (filter :job-fact?) (map :key)) channels))

(defn empty-facts
  [trace?]
  (start channels trace?))

(defn fold-note
  ([facts k v] (fold-note facts k nil v))
  ([facts k at v] (fold-in channels facts k at v)))

(defn fold-notes
  [facts notes]
  (reduce (fn [f [k at v]] (fold-note f k at v)) facts notes))

(defn fold-delta
  [acc delta]
  (reduce-kv (fn [a k v] (fold-in deltas a k nil v)) acc delta))

(defn job-facts
  [facts t-kw at]
  (into {:at at} (map (fn [k] [k (get-in facts [k t-kw])])) job-fact-keys))
