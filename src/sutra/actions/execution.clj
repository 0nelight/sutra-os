(ns sutra.actions.execution
  (:require [sutra.kit.coll :as coll]
            [sutra.kit.anomaly :as anomaly]
            [sutra.kit.refusal :as refusal]
            [sutra.catalog.grants :as grants]
            [sutra.foundation.side-effect :as fx]
            [sutra.store.api :as api]
            [sutra.store.tx-vector :as tv]
            [sutra.store.lib :as lib]
            [sutra.types.link-side :as ls]
            [sutra.types.object-set :as os]
            [sutra.types.run-limits :as limits])
  (:refer-clojure :exclude [deliver])
  (:import (java.util SplittableRandom UUID)))

(set! *warn-on-reflection* true)

(defn- limit-error
  [kind code data params]
  (refusal/refusal code (assoc data :sutra/run-limit kind) params))

(defrecord Io [pass snapshot trace project installer authorize sql-gate derived-reader
                lm-service checkpoint])

(declare checkpoint!)

(defn io
  [m]
  (map->Io (merge {:checkpoint checkpoint!} m)))

(defn run
  [{:keys [timeout-ms deadline now seed] :as policy} perform]
  (let [t (long timeout-ms)]
    (assoc (select-keys policy [:label :trace])
           :timeout-ms t
           :deadline   (or deadline (+ (System/nanoTime) (* t 1000000)))
           :now        (cond (instance? java.time.Instant now) now
                             (string? now) (java.time.Instant/parse now)
                             :else (java.time.Instant/now))
           :seed       (if (some? seed) (long seed) (.nextLong (SplittableRandom.)))
           :perform    perform)))

(defn oracle
  [run]
  (when run {:now (str (:now run)) :seed (:seed run)}))

(defn continued
  [run]
  (assoc run :trace nil :seed (.nextLong (SplittableRandom. (long (:seed run))))))

(defn- pass
  [run {:keys [answers recorded basis]}]
  (assoc run
         :answers  answers
         :recorded recorded
         :basis    basis
         :snap     (volatile! nil)
         :asked    (volatile! 0)
         :loaded   (volatile! 0)
         :reads    (volatile! {})
         :overlays (volatile! {})
         :uuids    (SplittableRandom. (long (:seed run)))))

(defn remaining-ms
  [run]
  (when-let [{:keys [deadline]} run]
    (quot (- deadline (System/nanoTime)) 1000000)))

(defn- check-deadline!
  [run]
  (when run
    (let [left (remaining-ms run)]
      (when-not (pos? left)
        (throw (limit-error :timeout :refusal.actions/run-deadline-passed
                            {:timeout-ms (:timeout-ms run)}
                            {:label (or (:label run) "the run")
                             :max   (:max limits/timeout-ms)}))))))

(defn- edit-count
  [run-state]
  (+ (count (:ops run-state))
     (count (:collected run-state))))

(defn edits-within-limit!
  [store]
  (let [n (edit-count store)]
    (when (> n limits/edits-max)
      (throw (limit-error :edits :refusal.actions/run-too-many-edits
                          {:edits n :limit limits/edits-max} nil)))))

(defn checkpoint!
  [{:keys [pass]} run-state]
  (when pass
    (check-deadline! pass)
    (edits-within-limit! run-state)))

(defn loaded!
  [{:keys [pass]} n]
  (when-let [v (:loaded pass)]
    (let [total (vswap! v + (long n))]
      (when (> total limits/objects-loaded-max)
        (throw (limit-error :objects :refusal.actions/run-too-many-objects
                            {:loaded total :limit limits/objects-loaded-max} nil))))))

(defn objects-left
  [run]
  (when-let [v (some-> run :loaded)]
    (max 0 (- limits/objects-loaded-max @v))))

(defn- epoch
  [p]
  @(:asked p))

(defn fresh-uuid
  [{:keys [pass]}]
  (if-let [^SplittableRandom r (:uuids pass)]
    (let [hi (.nextLong r) lo (.nextLong r)]
      (UUID. (-> hi (bit-and (bit-not 0xF000)) (bit-or 0x4000))
             (-> lo (bit-and 0x3FFFFFFFFFFFFFFF) (bit-or Long/MIN_VALUE))))
    (UUID/randomUUID)))

(defn now
  [{:keys [pass]}]
  (or (:now pass) (java.time.Instant/now)))

(defn- need!
  [request]
  (throw (ex-info "the run needs an answer from outside" {:ref/kind :need ::request request})))

(defn need
  [t]
  (some #(::request (ex-data %)) (take-while some? (iterate ex-cause t))))

(defn answer
  [{p :pass} request]
  (let [p (or p
              (anomaly/raise! :fault "an external call outside a run" {:request (:kind request)}))
        k (epoch p)]
    (if-let [{asked :request a :answer} (get (:answers p) k)]
      (if (= asked request)
        (do (vswap! (:asked p) inc) a)
        (refusal/refuse! :refusal.actions/replay-nondeterministic
                        {:ref/kind :nondeterministic :call k :before asked :now request}
                        {:n (inc k)}))
      (need! request))))

(defn gate
  [ctx [kind detail]]
  (let [fx (fx/side-effect (:effect-gate ctx) kind detail)]
    (cond
      (:dry-run? ctx)       {:outcome :planned}
      (not (fx/admit! fx)) {:outcome :held :side-effect fx})))

(defn deliver
  [ctx io request]
  (if (= :side-effect (:phase ctx))
    {:outcome :queued :request (dissoc request :world)}
    {:outcome :performed :result (answer io request)}))

(defn ask
  [ctx io {:keys [effect] :as request}]
  (if effect
    (or (gate ctx effect) (deliver ctx io (dissoc request :effect)))
    {:outcome :performed :result (answer io request)}))

(defn source
  [{:keys [db]}]
  {:snapshot (fn [branch rev]
               (api/snapshot db (cond-> {:branch branch} rev (assoc :rev rev))))})

(defn- open-snapshot
  [ctx io rev]
  ((or (:snapshot io)
       (anomaly/raise! :fault "the run's io carries no :snapshot (execution/source)" {}))
   (str (or (:branch ctx) "main")) rev))

(defn- pass-snapshot
  [p ctx io]
  (let [s (:snap p)]
    (or @s (vreset! s (open-snapshot ctx io (:rev (:basis p)))))))

(defn can-read?
  [ctx]
  (boolean (:reads? ctx)))

(defn staged-writes
  [ctx]
  (when (:staged? ctx)
    (some->> (:ops ctx) (filterv tv/store-write?) not-empty)))

(defn- under
  [snap ctx]
  (let [ops (not-empty (into (vec (:prior ctx)) (staged-writes ctx)))
        w   (:installed ctx)]
    (cond w     (api/overlay snap {:world w :ops (vec ops)})
          ops   (api/overlay snap ops)
          :else snap)))

(defn- overlay-key
  [ctx]
  [(count (:prior ctx)) (count (staged-writes ctx)) (:installed ctx)])

(defn- pass-view
  [p ctx io]
  (let [k (overlay-key ctx)]
    (or (get @(:overlays p) k)
        (let [s (under (pass-snapshot p ctx io) ctx)]
          (vswap! (:overlays p) assoc k s)
          s))))

(defn- read-now
  [ctx {p :pass :as io} authority query]
  (if p
    (api/read (pass-view p ctx io) authority query)
    (with-open [s ^java.io.Closeable (open-snapshot ctx io nil)]
      (api/read (under s ctx) authority query))))

(defn- read-key
  [ctx authority query]
  (into [authority query] (overlay-key ctx)))

(defn- read-set
  [reads]
  (into #{}
        (for [[[_ [k T] prior staged installed] answer] reads
              :when (and (= :objects k) (zero? prior) (zero? staged) (nil? installed))
              o answer]
          [:object T (str (:id o)) (api/object-version o)])))

(defn write-fences
  [read-set ops]
  (let [written (into #{} (map (juxt :type (comp str :id :payload))) ops)]
    (filterv (fn [[_ T id]] (contains? written [T id])) read-set)))

(defn read-store
  ([ctx io query] (read-store ctx io (:authority ctx) query))
  ([ctx {p :pass :as io} authority query]
   (check-deadline! p)
   (if p
     (let [k (read-key ctx authority query)]
       (cond
         (contains? @(:reads p) k)   (get @(:reads p) k)
         (contains? (:recorded p) k) (get (:recorded p) k)
         :else (let [r (read-now ctx io authority query)]
                 (vswap! (:reads p) assoc k r)
                 r)))
     (read-now ctx io authority query))))

(defn- perform
  [{:keys [perform label timeout-ms] :as run} request]
  (let [left (quot (- (:deadline run) (System/nanoTime)) 1000000)]
    (when-not (pos? left)
      (throw (limit-error :timeout :refusal.actions/run-timed-out
                          {:timeout-ms timeout-ms} {:label (or label "the run")})))
    (perform request left)))

(defn replaying
  [run evaluate]
  (let [trace (:trace run)
        mark  (some-> trace deref count)]
    (loop [answers [] recorded {} basis nil]
      (when (and mark (seq answers))
        (swap! trace #(into (empty %) (take mark %))))
      (let [p (pass run {:answers answers :recorded recorded :basis basis})
            [out basis]
            (try [(try {:done (evaluate p)}
                       (catch Throwable t
                         (if-let [r (need t)] {:need r} (throw t))))
                  (or basis (some-> @(:snap p) api/basis))]
                 (finally (some-> @(:snap p) api/close)))
            recorded (merge recorded @(:reads p))]
        (if-let [request (:need out)]
          (recur (conj answers {:request request :answer (perform run request)}) recorded basis)
          {:result (:done out)
           :facts  {:basis basis :released? (boolean (seq answers)) :answers answers
                    :read-set (read-set recorded)}})))))

(defn commit-basis
  [{:keys [basis read-set]}]
  (if basis
    {:read-set read-set :basis basis :fences [[:schema]]}
    {:read-set read-set :at-head? true}))

(defn reader
  [ctx io]
  (fn
    ([query] (read-store ctx io query))
    ([authority query] (read-store ctx io authority query))))

(defn object
  ([read T id] (first (read [:objects T [(str id)]])))
  ([read authority T id] (first (read authority [:objects T [(str id)]]))))

(defn objects-of
  [read T]
  (read [:objects T]))

(defn lookup
  [read rid {[k v] :key id :id}]
  (read [:select (os/qt->ir (if k
                              {:root rid :where [:= k v] :limit 2}
                              {:root rid :where [:id-in [id]] :limit 1}))]))

(defn- rows-query [kind T ids]
  (if (= :link kind) [:links T {:ids ids}] [:objects T ids]))

(defn blocked-among
  [read authority targets]
  (let [denied (lib/denied-markings authority)]
    (if (empty? denied)
      #{}
      (into #{}
            (for [[[kind T] ts] (group-by (fn [[kind T]] [kind T]) targets)
                  :let [ids (into [] (comp (map #(str (nth % 2))) (distinct)) ts)]
                  r (read grants/system-authority (rows-query kind T ids))
                  :when (not (lib/visible? denied r))]
              [kind T (str (:id r))])))))

(defn blocking
  [read authority]
  (fn [kind T id]
    (contains? (blocked-among read authority [[kind T id]]) [kind T (str id)])))

(defn traverse-fn
  [ctx io schema]
  (when (can-read? ctx)
    (fn [_state object-id conn object-type]
      (let [{:keys [link dir far-type]} (ls/resolve-side schema object-type conn)
            [near far] (if (= :to dir) [:from :to] [:to :from])
            fars (mapv far (read-store ctx io [:links link {near [(str object-id)]}]))
            r    (if (seq fars)
                   (let [by-id (coll/index-by :id (read-store ctx io
                                                 [:select
                                                  (os/qt->ir {:root far-type
                                                              :where [:id-in fars]})]))]
                     (into [] (keep by-id) fars))
                   [])]
        (loaded! io (count r))
        r))))
