(ns sutra.actions.batch
  (:require [sutra.kit.anomaly :as anomaly]
            [sutra.catalog.shape :as shape]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.lookup :as lookup]
            [sutra.catalog.constraints :as cstr]))

(set! *warn-on-reflection* true)

(def group-ops
  {[:objects :create]
   {:op :create :tx-op :create-object
    :row-fn (fn [schema type]
              (let [validate (cstr/row-validator schema type)]
                (fn [{:keys [id values markings stamps]}]
                  (validate values true)
                  (cond-> {:id (or id (java.util.UUID/randomUUID))
                           :values values
                           :markings (when markings (set markings))}
                    stamps (assoc :stamps stamps)))))}

   [:objects :update]
   {:op :update :tx-op :update-object
    :row-fn (fn [schema type]
              (let [validate (cstr/row-validator schema type)]
                (fn [{:keys [id values stamps]}]
                  (validate values false)
                  (cond-> {:id id :values values} stamps (assoc :stamps stamps)))))}

   [:objects :delete]
   {:op :delete-object :tx-op :delete-object
    :row-fn (fn [schema type]
              (when-not (lookup/get-type schema :object type)
                (throw (cstr/unknown-type type)))
              (fn [{:keys [id]}] {:id id}))}

   [:links :create]
   {:op :create-link :tx-op :create-link
    :row-fn (fn [schema type]
              (let [link-id (shape/link-id-fn schema type)]
                (fn [{:keys [id from-id to-id markings]}]
                  {:id (or id (link-id from-id to-id))
                   :from-id from-id :to-id to-id
                   :markings (when markings (set markings))})))}})

(defn- authorizer-for
  [authorize authority op target-type]
  (when authorize
    (authorize op {:type target-type :id nil} {})
    (when (seq (grants/denied-markings authority))
      (fn [row] (authorize op {:type target-type :id (:id row)} {})))))

(defn compile-plan
  [schema {:keys [kind verb type]} authorize authority]
  (let [{:keys [op tx-op row-fn]}
        (or (group-ops [kind verb])
            (anomaly/raise! :fault (str "Not a publish group: " (pr-str [kind verb]))
                            {:kind kind :verb verb :type type}))
        authorize-row (authorizer-for authorize authority op type)]
    {:kind kind :verb verb :type type :tx-op tx-op
     :row-fn (row-fn schema type)
     :authorize-row authorize-row}))

(def ^:private min-parallel-rows
  2000)

(defn partition-count [n-rows cpus]
  (if (< n-rows min-parallel-rows)
    1
    (max 1 (min 8 cpus))))

(defn partition-rows
  [decls p]
  (if (= 1 p)
    [decls]
    (let [buckets (object-array p)]
      (dotimes [i p] (aset buckets i (transient [])))
      (doseq [{:keys [row] :as decl} decls]
        (let [k (or (:id row) [(:from-id row) (:to-id row)])
              i (Math/floorMod (long (hash k)) (long p))]
          (aset buckets i (conj! (aget buckets i) decl))))
      (into [] (comp (map persistent!) (remove empty?)) buckets))))

(def ^:private eval-chunk-rows
  5000)

(defonce ^:private eval-pool
  (delay
    (java.util.concurrent.Executors/newFixedThreadPool
     (max 2 (.availableProcessors (Runtime/getRuntime)))
     (reify java.util.concurrent.ThreadFactory
       (newThread [_ r]
         (doto (Thread. ^Runnable r "sutra-publish") (.setDaemon true)))))))

(defn- submit
  [f]
  (.submit ^java.util.concurrent.ExecutorService @eval-pool
           ^java.util.concurrent.Callable (bound-fn* f)))

(defn run-planned!
  ([plan decls p flush-chunk!] (run-planned! plan decls p flush-chunk! nil))
  ([plan decls p flush-chunk! unchanged?]
   (let [parts   (partition-rows decls p)
         q       (java.util.concurrent.ArrayBlockingQueue. (* 2 (count parts)))
         DONE    (Object.)
         err     (atom nil)
         skipped (java.util.concurrent.atomic.AtomicLong. 0)
         row-fn  (:row-fn plan)
         fail!   (fn [e] (compare-and-set! err nil e))
         futs    (mapv
                  (fn [part]
                    (submit
                     (fn []
                       (try
                         (loop [rows (seq part), buf (transient []), n 0]
                           (cond
                             (some? @err) nil
                             (nil? rows)  (when (pos? n) (.put q (persistent! buf)))
                             :else
                             (let [row (row-fn (:row (first rows)))]
                               (if (and unchanged? (unchanged? row))
                                 (do (.incrementAndGet skipped)
                                     (recur (next rows) buf n))
                                 (let [buf (conj! buf row), n (inc n)]
                                   (if (>= n eval-chunk-rows)
                                     (do (.put q (persistent! buf))
                                         (recur (next rows) (transient []) 0))
                                     (recur (next rows) buf n)))))))
                         (catch Throwable e (fail! e))
                         (finally (.put q DONE))))))
                  parts)
         written (loop [live (count futs), written 0]
                   (if (zero? live)
                     written
                     (let [item (.take q)]
                       (if (identical? DONE item)
                         (recur (dec live) written)
                         (do
                           (try (flush-chunk! item)
                                (catch Throwable e (fail! e)))
                           (recur live (+ written (count item))))))))]
     (doseq [^java.util.concurrent.Future f futs] (.get f))
     (when-let [e @err] (throw e))
     {:published written :skipped (.get skipped)})))
