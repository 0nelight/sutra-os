(ns sutra.kit.effects
  (:require [sutra.kit.refusal :as refusal]
            [sutra.kit.attempt :as attempt]))

(set! *warn-on-reflection* true)

(defn work-token
  ([] (work-token nil))
  ([deadline-ms] {:cancelled (atom false) :deadline deadline-ms}))

(defn cancel!
  ([token] (cancel! token :cancelled))
  ([token reason] (some-> token :cancelled (swap! #(or % reason))) nil))

(defn reason
  [{:keys [cancelled deadline] :as token}]
  (when token
    (or (and cancelled @cancelled)
        (when (and deadline (> (System/currentTimeMillis) (long deadline))) :deadline))))

(defn cancelled? [token] (boolean (reason token)))

(defn check!
  [token]
  (when-let [r (reason token)]
    (refusal/refuse! :refusal.kit/work-cancelled {:refusal :work-cancelled :reason r})))

(defn cancellation
  [^Throwable t]
  (some (fn [^Throwable c]
          (let [{:keys [refusal cancelled reason]} (ex-data c)]
            (when (or cancelled (= :work-cancelled refusal))
              (or reason :cancelled))))
        (take-while some? (iterate #(.getCause ^Throwable %) t))))

(def ^:dynamic *work*
  nil)

(def ^:dynamic *attempt*
  nil)

(defonce ^:private observer
  (atom nil))

(defn observe-calls!
  [begin]
  (reset! observer begin))

(defn observed
  [target f]
  (if-let [begin (and target @observer)]
    (let [finish (begin target)]
      (try (finish (f) nil)
           (catch Throwable t (finish nil t) (throw t))))
    (f)))

(defn- open!
  [token]
  (check! token)
  (some-> *attempt* (reset! true)))

(defn call!
  ([f] (call! *work* f))
  ([token f] (call! token nil f))
  ([token target f]
   (open! token)
   (observed target f)))

(defn call-async!
  [token target start]
  (open! token)
  (if-let [begin (and target @observer)]
    (let [finish (begin target)]
      (try (start #(finish % (:error %)))
           (catch Throwable t (finish nil t) (throw t))))
    (start identity)))

(defn attempt
  [f]
  (let [reached (atom false)]
    (binding [*attempt* reached]
      (try {:ok (f)}
           (catch Throwable t
             (attempt/rethrow-fatal! t)
             {:error t :reached? @reached})))))
