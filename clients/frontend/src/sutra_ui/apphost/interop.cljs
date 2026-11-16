(ns sutra-ui.apphost.interop
  (:require [sutra.types.decimal :as d]
            [sutra-ui.debug :as dbg]))

(defn ->js
  [x]
  (cond
    (map? x)                  (let [o (js/Object.)] (doseq [[k val] x] (aset o (name k) (->js val)))
                                   o)
    (or (vector? x) (seq? x)) (let [a (js/Array.)] (doseq [e x] (.push a (->js e))) a)
    (keyword? x)              (name x)
    (d/exact? x)              (d/plain x)
    :else                     x))

(defn ->clj
  [x]
  (cond
    (nil? x)                            nil
    (instance? js/Array x)              (mapv ->clj x)
    (identical? (.-constructor x) js/Object)
    (persistent! (reduce (fn [m k] (assoc! m (keyword k) (->clj (aget x k))))
                         (transient {}) (js/Object.keys x)))
    :else                               x))

(defn- obj? [x]
  (and (some? x) (identical? (.-constructor x) js/Object)))

(defn- ->name-map
  [x f]
  (cond
    (map? x) (persistent! (reduce-kv (fn [m k v] (assoc! m (name k) (f v))) (transient {}) x))
    (obj? x) (persistent! (reduce (fn [m k] (assoc! m k (f (aget x k)))) (transient {})
                                  (js/Object.keys x)))
    :else    nil))

(defn- type-table
  [x]
  (when-let [t (->name-map x #(->name-map % identity))]
    (not-empty t)))

(defn- lift-number
  [v]
  (if (string? v) (or (d/carry v) v) v))

(defn- lift
  [v t]
  (if (d/numeric-value-type? t) (lift-number v) v))

(defn- lift-props!
  [o props]
  (when (and (obj? o) props)
    (doseq [k (js/Object.keys o)]
      (when-let [t (get props k)]
        (aset o k (lift (aget o k) t))))))

(defn- lift-numbers!
  [o]
  (when (obj? o)
    (doseq [k (js/Object.keys o)] (aset o k (lift-number (aget o k))))))

(defn- lift-rows!
  [rows types dflt]
  (when (instance? js/Array rows)
    (doseq [row rows]
      (when (obj? row)
        (let [tn (or (some-> (aget row "object-type-ref") name) dflt)]
          (lift-props! (aget row "values") (get types tn)))))))

(defn- lift-row-groups!
  [groups types]
  (when (obj? groups)
    (doseq [k (js/Object.keys groups)] (lift-rows! (aget groups k) types k))))

(defn- lift-aggregations!
  [aggs types dflt]
  (when (instance? js/Array aggs)
    (let [props (get types dflt)]
      (doseq [b aggs]
        (when (obj? b)
          (lift-props!   (aget b "group") props)
          (lift-numbers! (aget b "metrics")))))))

(defn- lift-series!
  [series]
  (when (instance? js/Array series)
    (doseq [s series]
      (when (obj? s)
        (let [pts (aget s "points")]
          (when (instance? js/Array pts)
            (doseq [p pts]
              (when (instance? js/Array p)
                (aset p 1 (lift-number (aget p 1)))))))))))

(defn- decode-body!
  [x types]
  (when (and (obj? x) types)
    (when-let [data (aget x "data")]
      (when (obj? data)
        (doseq [tn (js/Object.keys data)] (lift-rows! (aget data tn) types tn))))
    (let [objs (aget x "objects")]
      (if (instance? js/Array objs) (lift-rows! objs types nil) (lift-row-groups! objs types)))
    (lift-row-groups! (aget x "links") types)
    (lift-row-groups! (aget x "answers") types)
    (let [tn (some-> (aget x "object-type") name)]
      (when-let [o (aget x "object")] (lift-rows! #js [o] types tn))
      (lift-rows!         (aget x "rows") types tn)
      (lift-aggregations! (aget x "aggregations") types tn)
      (lift-series!       (aget x "series"))))
  x)

(defn rows-note
  [d]
  (when (map? d)
    (str (reduce-kv (fn [n _ v] (+ n (if (coll? v) (count v) 0))) 0 d) " rows")))

(defn- payload-note
  [x]
  (when (obj? x)
    (let [data (aget x "data")
          n    (if (obj? data)
                 (reduce (fn [n k]
                           (let [v (aget data k)]
                             (+ n (if (instance? js/Array v) (.-length v) 0))))
                         0 (js/Object.keys data))
                 0)
          objs (let [o (aget x "objects")] (if (instance? js/Array o) (.-length o) 0))]
      (str (+ n objs) " rows"))))

(defn decode-json
  ([txt] (decode-json txt nil))
  ([txt types]
   (when (seq txt)
     (let [x   (dbg/timed :app/json-parse #(js/JSON.parse txt) payload-note)
           tbl (or (some-> (when (obj? x) (aget x "application"))
                           (aget "property-types")
                           type-table)
                   (type-table types))]
       (dbg/timed :app/lift-numbers #(decode-body! x tbl))))))

(defn read-response
  ([resp] (read-response resp nil))
  ([resp types]
   (let [t0 (dbg/now)]
     (.then (.text resp)
            (fn [t]
              (dbg/measure! :app/response-text t0 (str (.-length t) " chars"))
              {:ok (.-ok resp) :status (.-status resp) :body (decode-json t types)})))))

(defn event-value
  [^js e]
  (some-> e .-target .-value))

(defn event-checked [^js e] (boolean (some-> e .-target .-checked)))

(defn event-key [^js e] (some-> e .-key))

(defn prevent-default! [^js e] (some-> e .preventDefault) nil)

(defn stop-propagation! [^js e] (some-> e .stopPropagation) nil)
