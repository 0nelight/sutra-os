(ns sutra.store.access.events
  (:require [sutra.kit.attempt :as attempt]
            [sutra.catalog.grants :as grants]
            [sutra.foundation.ops :as op]
            [sutra.store.access.scope :as ctx]
            [sutra.store.api :as api]
            [sutra.store.control :as ctl]
            [sutra.store.control.collections :as collections]
            [sutra.store.machine :as machine]
            [sutra.store.records :as records]
            [sutra.store.world :as world]))

(set! *warn-on-reflection* true)

(def ^:private cursor-coll (:coll collections/event-cursors))

(def ^:private parked-coll (:coll collections/parked-events))

(def attempts-before-parking 3)

(defn- cursor-key [b reaction] [b (subs (str reaction) 1)])

(defn- next-event
  [snap ev at]
  (let [past? #(or (nil? at) (pos? (compare [(:rev %) (:index %)] at)))]
    (->> (api/read snap nil [:log/range (cond-> {:kinds #{ev} :own? true}
                                          at (assoc :after (dec (first at))))])
         (filter past?)
         first)))

(defn- answer-one!
  [store branch reaction failures]
  (let [ev  (:on (op/entry reaction))
        k   (cursor-key branch reaction)
        sys {:principal "sutra.store.access.events"}]
    (machine/command!
     (:db store) {:branch branch}
     (fn [snap]
       (let [at (api/read snap nil [:records/get cursor-coll k])]
         (if-let [{:keys [rev index payload]} (next-event snap ev at)]
           (let [here [rev index]
                 ack  {:coll cursor-coll :key k :value here
                       :expect (or at api/absent)}
                 tx   (try
                        (let [s (records/deciding store snap)
                              {:keys [tx]} (op/react {:store s :auth grants/system-authority
                                                      :branch branch}
                                                     reaction payload)]
                          (ctl/judge! s grants/system-authority (:records tx))
                          tx)
                        (catch Exception e
                          (attempt/rethrow-fatal! e)
                          (let [n (get (swap! failures update [branch reaction here]
                                              (fnil inc 0))
                                       [branch reaction here])]
                            (if (< n attempts-before-parking)
                              (throw e)
                              {:records [{:coll  parked-coll
                                          :key   [branch (subs (str reaction) 1) rev index]
                                          :value {:event ev :payload payload
                                                  :error (ex-message e) :attempts n}}]}))))]
             {:tx     (op/merge-tx (assoc tx :actor sys) {:records [ack]})
              :result (if (some #(= parked-coll (:coll %)) (:records tx)) :parked :answered)})
           {:result nil}))))))

(defonce ^:private failures (atom {}))

(defn dispatch-branch!
  [store branch]
  (doseq [reaction (sort (for [[o {:keys [kind consistency]}] (op/entries)
                                :when (and (= :reaction kind)
                                           (not= :same-commit consistency))]
                            o))
          :when (op/bound? reaction)]
    (try
      (loop []
        (when (:result (answer-one! store branch reaction failures))
          (recur)))
      (catch Exception e
        (attempt/rethrow-fatal! e)
        (println (str "[events] " branch " " reaction ": " (ex-message e)))))))

(defn dispatch!
  [store]
  (when (some #(= :reaction (:kind %)) (vals (op/entries)))
    (doseq [{b :name} (world/branches store)]
      (dispatch-branch! (ctx/at store b) b))))

(defn parked
  [store branch]
  (with-open [s ^java.io.Closeable (api/snapshot (:db store) {:branch branch})]
    (vec (for [[[b r rev index] v] (api/read s nil [:records/list parked-coll])
               :when (= b branch)]
           (assoc v :reaction (keyword r) :at [rev index])))))

(def module
  {:name     :ops-events
   :provides {:ops/dispatch dispatch-branch!}
   :jobs     [{:name :ops-events :every "SUTRA_EVENTS_MS" :default 2000 :tick dispatch!}]})
