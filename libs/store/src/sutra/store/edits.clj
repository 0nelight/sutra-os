(ns sutra.store.edits)

(set! *warn-on-reflection* true)

(defn epoch-ms
  [t]
  (cond (instance? java.util.Date t)             (.getTime ^java.util.Date t)
        (instance? java.time.Instant t)          (.toEpochMilli ^java.time.Instant t)
        (instance? java.time.OffsetDateTime
                   t)   (.toEpochMilli (.toInstant ^java.time.OffsetDateTime t))
        (instance? java.time.ZonedDateTime
                   t)    (.toEpochMilli (.toInstant ^java.time.ZonedDateTime t))
        (instance? java.time.LocalDateTime
                   t)
        (.toEpochMilli (.toInstant ^java.time.LocalDateTime t java.time.ZoneOffset/UTC))
        (integer? t)                             (long t)
        (string? t)
        (or (try (.toEpochMilli (java.time.Instant/parse t))
                 (catch java.time.DateTimeException _ nil))
            (try (.toEpochMilli (.toInstant (java.time.OffsetDateTime/parse t)))
                 (catch java.time.DateTimeException _ nil))
            (try (.toEpochMilli (.toInstant (java.time.LocalDateTime/parse t)
                                            java.time.ZoneOffset/UTC))
                 (catch java.time.DateTimeException _ nil)))
        :else                                    nil))

(defn timestamp-attr
  [{:keys [strategy timestamp-attr]}]
  (when (= :apply-most-recent (some-> strategy keyword)) (some-> timestamp-attr name keyword)))

(defn source-times
  [ts-attr {:keys [values stamps] :as payload}]
  (if (contains? payload :stamps)
    (reduce (fn [m {:keys [at attrs]}]
              (let [ms (epoch-ms at)]
                (reduce #(if ms (assoc %1 %2 ms) (dissoc %1 %2)) m attrs)))
            {} stamps)
    (when-let [ms (some->> ts-attr (get values) epoch-ms)]
      (zipmap (keys values) (repeat ms)))))

(defn- newer? [t at] (boolean (and t at (> t at))))

(defn resolve-object
  [e source times]
  (let [attrs   (set (:attrs e))
        edit-at (fn [a] (get (:attrs-at e) a (:at e)))
        sup     (into #{} (filter #(newer? (get times %) (edit-at %))) attrs)
        held    (into #{} (remove sup) attrs)
        follow  (fn [why]
                  {:visible? true :follows-source? true :held held :superseded sup :why why
                   :values (merge source (select-keys (:values e) held))})]
    (cond
      (:deleted? e)
      {:visible? false :follows-source? false :held #{} :superseded #{} :why :deleted}

      (and (:created? e)
           (not (and source
                     (some #(newer? % (:created-at e (:at e))) (vals times)))))
      {:visible? true :follows-source? false :held attrs :superseded #{} :why :created
       :values (or (:values e) {})}

      (:created? e)  (follow :create-superseded)

      (nil? source)
      {:visible? false :follows-source? true :held #{} :superseded #{}
       :why :absent-from-source}

      (empty? attrs) (follow :source-only)
      (seq held)     (follow :edits-applied)
      :else          (follow :edits-superseded))))
