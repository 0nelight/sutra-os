(ns sutra.kit.anomaly
  (:require [sutra.kit.err :as err]
            [sutra.kit.placeholder :as placeholder]))

(def statuses
  {:incorrect     400
   :forbidden     403
   :not-found     404
   :conflict      409
   :too-large     413
   :unprocessable 422
   :fault         500
   :upstream      502
   :unavailable   503})

(defn category
  [d]
  (when (map? d) (::category d)))

(defn status
  [d]
  (some-> (category d) statuses))

(defn client-error?
  [d]
  (boolean (some-> (status d) (< 500))))

(defn acl-denied?
  [d]
  (boolean (:sutra/acl-denied d)))

(defn refused
  [category msg d]
  {:pre [(contains? statuses category)]}
  (assoc d ::category category ::message msg))

(defn refusal?
  [x]
  (and (map? x) (contains? x ::message) (not= :fault (::category x))))

(defn message
  [f]
  (if (map? f) (::message f) (ex-message f)))

(defn data
  [f]
  (dissoc (if (map? f) f (ex-data f)) ::category ::message ::cause))

(defn cause
  [r]
  (::cause r))

(defn then
  [x f]
  (if (refusal? x) x (f x)))

(defn ex
  ([r] (ex r (cause r)))
  ([r cause]
   (ex-info (message r) (assoc (data r) ::category (category r)) cause)))

(defn raise!
  ([category msg d] (throw (ex (refused category msg d) nil)))
  ([category msg d cause] (throw (ex (refused category msg d) cause))))

(defn coded
  ([code category template d] (coded code category template d nil))
  ([code category template d params]
   (let [d (cond-> (assoc d :refusal/code code) (seq params) (assoc :refusal/params params))]
     (refused category (placeholder/fill template (merge d params)) d))))

(defn coded!
  ([code category template d] (coded! code category template d nil nil))
  ([code category template d params] (coded! code category template d params nil))
  ([code category template d params cause]
   (throw (ex (coded code category template d params) cause))))

(defn unwrap!
  [x]
  (if (refusal? x) (throw (ex x)) x))

(defn acl-denial
  [e]
  (err/cause (comp acl-denied? ex-data) e))

(defn run-limit
  [e]
  (some (comp :sutra/run-limit ex-data) (err/causes e)))

(defn of
  [e]
  (when-let [t (or (acl-denial e)
                   (let [c (category (ex-data e))] (when (and c (not= :fault c)) e)))]
    (assoc (ex-data t) ::category (category (ex-data t)) ::message (ex-message t) ::cause e)))

(defn catching
  [f]
  (try (f)
       (catch #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) e
         (or (of e) (throw e)))))

(def ^:private parameter-refusals
  #{:missing-required :invalid-type :constraint-refusal :unresolved-param :collect-blocked
    :missing-argument :unknown-argument :type-mismatch})

(defn- failure-data
  [e]
  (if (map? e)
    (cons (data e) (some->> (cause e) err/causes (keep ex-data)))
    (keep ex-data (err/causes e))))

(defn metric
  [e]
  (let [ds   (failure-data e)
        has? (fn [f] (some f ds))]
    (cond
      (has? :sutra/run-limit)                                        :scale-limit
      (has? (comp parameter-refusals :ref/kind))                     :invalid-parameter
      (has? #(or (#{:criterion-failed :sys-denied} (:ref/kind %))
                 (acl-denied? %) (:failed-criterion %)))             :authorization
      (has? #(= :writeback-failed (:ref/kind %)))                    :side-effect
      (has? #(= :provenance (:ref/kind %)))                          :function
      (has? #(#{:conflict :stale-object} (:store/refusal %)))        :conflict
      :else                                                          :unclassified)))
