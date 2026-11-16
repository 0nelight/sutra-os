(ns sutra.types.llm-forecast
  (:require [sutra.kit.coerce :refer [->num ->num0]]
            [sutra.types.llm-cost :as llm-cost]))

(def ^:const assumed-calls-per-row
  1.0)

(def ^:const default-window-days
  30)

(defn history-for
  [history rid t-name]
  (let [ks (llm-cost/caller-keys rid t-name)]
    (filterv #(contains? ks (llm-cost/caller-key (:caller %))) history)))

(defn- sum-of [k rows] (reduce + 0.0 (map #(->num0 (get % k)) rows)))

(defn sample
  [rows model]
  (when (seq rows)
    (let [want (filterv #(= (str model) (str (:model %))) rows)
          use  (if (seq want) want rows)]
      {:calls        (long (sum-of :calls use))
       :in-tokens    (long (sum-of :in-tokens use))
       :out-tokens   (long (sum-of :out-tokens use))
       :models       (vec (distinct (map #(str (:model %)) use)))
       :model-match? (boolean (seq want))})))

(defn per-row
  ([s rows-in-window] (per-row s rows-in-window nil))
  ([{:keys [calls in-tokens out-tokens] :as s} rows-in-window override]
   (let [cpr (->num override)]
     (cond
       (nil? s) {:source :uncalibrated}

       (and (nil? cpr) rows-in-window (pos? rows-in-window) (pos? calls))
       {:source     :measured
        :calls      (/ (double calls) rows-in-window)
        :in-tokens  (/ (double in-tokens) rows-in-window)
        :out-tokens (/ (double out-tokens) rows-in-window)
        :sample     {:calls calls :rows rows-in-window}}

       (pos? calls)
       {:source     (if cpr :override :per-call)
        :calls      (or cpr assumed-calls-per-row)
        :in-tokens  (* (or cpr assumed-calls-per-row) (/ (double in-tokens) calls))
        :out-tokens (* (or cpr assumed-calls-per-row) (/ (double out-tokens) calls))
        :sample     {:calls calls :rows 0}}

       :else {:source :uncalibrated}))))

(defn per-call
  [{:keys [calls in-tokens out-tokens] :as s}]
  (if (and s (pos? calls))
    {:source     :measured-call
     :in-tokens  (/ (double in-tokens) calls)
     :out-tokens (/ (double out-tokens) calls)
     :sample     {:calls calls}}
    {:source :uncalibrated}))

(defn project-drain
  ([ceiling cap pc rate] (project-drain ceiling cap pc rate nil))
  ([ceiling cap pc rate observed]
   (let [ceiling (max 0 (long (or ceiling 0)))
         obs     (some-> observed long (max 0) (min ceiling))
         pending (or obs ceiling)
         cap     (when (and cap (pos? (long cap))) (long cap))
         next    (if cap (min pending cap) pending)
         runs-of (fn [n] (if (and cap (pos? cap))
                           (long (Math/ceil (/ (double n) cap)))
                           (if (pos? n) 1 0)))
         {:keys [in-tokens out-tokens]} pc
         money   (fn [n]
                   (when (and in-tokens out-tokens)
                     (llm-cost/call-cost rate (* n (double in-tokens)) (* n (double out-tokens)))))]
     (cond-> {:pending        pending
              :cap            cap
              :runs           (runs-of pending)
              :calls          pending
              :calls-next-run next
              :in-tokens      (some-> in-tokens (* pending) Math/round)
              :out-tokens     (some-> out-tokens (* pending) Math/round)
              :cost           (money pending)
              :cost-next-run  (money next)
              :currency       (:currency rate llm-cost/default-currency)}
       (some? obs) (assoc :measured?       true
                          :ceiling         ceiling
                          :ceiling-calls   ceiling
                          :ceiling-runs    (runs-of ceiling)
                          :ceiling-cost    (money ceiling))
       (= :uncalibrated (:source pc)) (assoc :unknown :uncalibrated)
       (and (not= :uncalibrated (:source pc))
            (not (and (:in rate) (:out rate)))) (assoc :unknown :unregistered-model)))))

(defn project
  [rows pr rate]
  (let [rows (max 0 (long (or rows 0)))
        {:keys [in-tokens out-tokens]} pr
        in   (when in-tokens  (* rows (double in-tokens)))
        out  (when out-tokens (* rows (double out-tokens)))]
    (cond-> {:rows       rows
             :calls      (when (:calls pr) (Math/round (* rows (double (:calls pr)))))
             :in-tokens  (some-> in Math/round)
             :out-tokens (some-> out Math/round)
             :cost       (when (and in out) (llm-cost/call-cost rate in out))
             :currency   (:currency rate llm-cost/default-currency)}
      (= :uncalibrated (:source pr)) (assoc :unknown :uncalibrated)
      (and (not= :uncalibrated (:source pr))
           (not (and (:in rate) (:out rate)))) (assoc :unknown :unregistered-model))))

(defn roll-up
  [entries]
  (let [priced (filterv :cost entries)
        n      #(long (reduce + 0 (keep % entries)))]
    {:cost       (when (seq priced) (reduce + 0.0 (map :cost priced)))
     :cost-next-run (when (seq priced)
                      (reduce + 0.0 (keep #(or (:cost-next-run %) (:cost %)) priced)))
     :runs       (reduce max 0 (map #(long (or (:runs %) (if (:cost %) 1 0))) entries))
     :currency   (or (some :currency priced) llm-cost/default-currency)
     :calls      (n :calls)
     :calls-next-run (long (reduce + 0 (map #(or (:calls-next-run %) (:calls %) 0) entries)))
     :in-tokens  (n :in-tokens)
     :out-tokens (n :out-tokens)
     :tokens     (+ (n :in-tokens) (n :out-tokens))
     :priced     (count priced)
     :unknown    (vec (for [e entries :when (:unknown e)]
                        {:transform (:transform e) :reason (:unknown e)}))
     :delta-based (vec (for [e entries :when (= :delta (:pending-basis e))]
                         (:transform e)))
     :measured   (vec (for [e entries :when (:measured? e)] (:transform e)))
     :ceiling-only (vec (for [e entries
                              :when (and (not= :delta (:pending-basis e))
                                         (not (:measured? e)))]
                          (:transform e)))
     :ceiling-cost (let [c (filterv #(or (:ceiling-cost %) (:cost %)) entries)]
                     (when (seq c)
                       (reduce + 0.0 (map #(or (:ceiling-cost %) (:cost %)) c))))
     :lower-bound? (boolean (some :unknown entries))}))

(defn budget-verdict
  [{:keys [budget spent]} estimate entries]
  (let [budget    (or (->num budget) 0.0)
        spent     (or (->num spent) 0.0)
        unlimited (not (pos? budget))
        remaining (max 0.0 (- budget spent))]
    (cond-> {:budget budget :spent spent :remaining remaining
             :unlimited? unlimited
             :estimate estimate
             :fits? (cond unlimited        true
                          (nil? estimate)  nil
                          :else            (<= estimate remaining))}
      (and (not unlimited) estimate (> estimate remaining))
      (assoc :shortfall (- estimate remaining)
             :cut (loop [[e & more] (filterv :cost entries), left remaining]
                    (cond
                      (nil? e) nil
                      (<= (:cost e) left) (recur more (- left (:cost e)))
                      :else
                      (let [per (when (pos? (:rows e 0)) (/ (:cost e) (:rows e)))]
                        {:transform (:transform e)
                         :rows      (:rows e)
                         :covered   (if (and per (pos? per))
                                      (long (Math/floor (/ left per)))
                                      0)})))))))
