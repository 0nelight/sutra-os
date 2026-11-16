(ns sutra.builds.run.lock
  (:require [sutra.kit.refusal :as refusal]
            [sutra.foundation.periodic :as periodic]
            [sutra.foundation.settings :as settings]
            [sutra.store.control :as ctl]
            [sutra.store.control.lifecycle :as lcs]
            [sutra.kit.lifecycle :as lc]
            [sutra.builds.run.lane :as lane]
            [sutra.builds.plan.outputs :as outputs]
            [clojure.set :as set]
            [sutra.coord.api :as coord]
            [sutra.foundation.live-trace :as live]
            [sutra.kit.decisions :as decisions]
            [sutra.store.storage.node :as node]
            [sutra.builds.run.rows :as run-rows]
            [sutra.store.world :as world]
            [sutra.store.access.scope :as scope]
            [sutra.kit.noted :as noted]))

(set! *warn-on-reflection* true)

(def ^:private poll-ms 1000)

(def ^:private admission-lock "build-admission")

(defn- claim-lease [run-id] (str "build-claim/" run-id))

(defonce ^:private held (atom {}))

(defonce ^:private waiting (atom #{}))

(defonce ^:private handed-back (atom #{}))

(def claim-table
  (lc/table {:name   :build-claim
             :states #{:queued :running}
             :final  #{}
             :edges  {:admit {:from #{:queued} :to :running}}}))

(defn- trunk? [branch] (= "main" branch))

(defn conflicts?
  [a b]
  (let [ra (set (:reads a)) wa (set (:writes a)) ba (str (or (:branch a) "main"))
        rb (set (:reads b)) wb (set (:writes b)) bb (str (or (:branch b) "main"))]
    (boolean
     (or (some outputs/external-key? (set/intersection wa wb))
         (and (or (= ba bb) (trunk? ba)) (seq (set/intersection wa rb)))
         (and (or (= ba bb) (trunk? bb)) (seq (set/intersection wb ra)))
         (and (= ba bb) (seq (set/intersection wa wb)))))))

(defn blockers
  [claims me]
  (let [ahead (set (map :run-id (take-while #(not= (:run-id me) (:run-id %)) claims)))]
    (filterv #(and (not= (:run-id me) (:run-id %))
                   (or (#{"running" "orphaned"} (:status %)) (ahead (:run-id %)))
                   (conflicts? me %))
             claims)))

(defn admission
  [claims run-id cap]
  (let [me (some #(when (= (str run-id) (:run-id %)) %) claims)]
    (cond
      (nil? me)                  {:state :cancelled}
      (= "running" (:status me)) {:state :admitted}
      :else
      (let [bs      (blockers claims me)
            running (count (filter #(= "running" (:status %)) claims))]
        (if (and (empty? bs) (< running (long cap)))
          {:state :admitted :admit? true}
          {:state :queued :blockers (mapv :run-id bs) :running running})))))

(defn lease-renewer
  [thread-name label renew]
  (delay
    (let [ms (max 1000 (quot node/lease-ms 3))]
      (periodic/start! (periodic/slot) thread-name ms ms
                       #(noted/quietly (str label " lease renewal") (renew))))))

(defn- renew-held! []
  (doseq [[run-id {c :coord l :lease}] @held]
    (if-let [l' (coord/renew! c l node/lease-ms)]
      (swap! held assoc-in [run-id :lease] l')
      (swap! held dissoc run-id))))

(defonce ^:private renewer
  (lease-renewer "sutra-build-claims" "build-lock" renew-held!))

(defn- under-admission-lock
  [c f]
  (let [l (coord/acquire! c {:name admission-lock :owner (node/id) :ttl-ms 30000 :wait-ms 30000})]
    (when-not l
      (refusal/refuse! :refusal.builds/admission-busy {:lock admission-lock}))
    (try (f) (finally (coord/release! c l)))))

(def ^:private slots-key {:name "builds"})

(defn- admitting
  [run-id cap gone orphaned]
  (fn [store t]
    (let [claims  (filterv #(not (gone (:run-id %))) (ctl/list store :build-claims {}))
          ids     (set (map :run-id (remove #(orphaned (:run-id %)) claims)))
          running (into #{} (filter ids) (:running (ctl/get store :build-slots slots-key)))
          claims  (mapv #(cond (orphaned (:run-id %)) (assoc % :status "orphaned")
                               (running (:run-id %))  (assoc % :status "running")
                               :else                  %)
                        claims)
          r       (admission claims run-id cap)]
      (if-not (:admit? r)
        {:result r}
        (let [sw (ctl/swapping store :build-slots slots-key
                               (fn [_] {:running (conj running (str run-id))}) t)
              up ((lcs/transitioning :build-claims {:run-id (str run-id)} claim-table :admit
                                     {:id       (str run-id)
                                      :state-of lcs/status-state
                                      :sets     (fn [_ to] {:status (name to)
                                                            :admitted-at ctl/now})})
                  store t)]
          {:records (into (vec (:records sw)) (:records up)) :result r})))))

(defn- free-slot!
  [store run-id]
  (ctl/swap! store :build-slots slots-key
             (fn [old] (when (contains? (set (:running old)) (str run-id))
                         {:running (disj (set (:running old)) (str run-id))}))))

(defn- settled?
  [store {:keys [run-id branch]}]
  (not= "running" (:status (if branch
                             (run-rows/fetch-run store branch run-id)
                             (run-rows/fetch-run-anywhere store run-id)))))

(defn- admit!
  [store c run-id]
  (under-admission-lock
   c
   (fn []
     (let [lapsed   (remove #(coord/holder c (claim-lease (:run-id %)))
                            (ctl/list store :build-claims {}))
           {gone true orphaned false} (group-by #(settled? store %) lapsed)
           gone     (set (map :run-id gone))]
       (last (ctl/commit! store
                          (concat
                           (when (seq gone)
                             [(fn [s _] (ctl/deleting s :build-claims {:run-id gone}))])
                           [(admitting run-id (settings/value :build-concurrency) gone
                                       (set (map :run-id orphaned)))])))))))

(defn waiting?
  [run-id]
  (contains? @waiting (str run-id)))

(defn hand-back!
  [run-id]
  (swap! handed-back conj (str run-id)))

(defn poll-after
  [waiters]
  (+ (long poll-ms) (* 250 (long waiters))))

(defn- forget-wait!
  [run-id]
  (swap! waiting disj (str run-id))
  (swap! handed-back disj (str run-id)))

(defn release!
  [store run-id]
  (forget-wait! run-id)
  (when-let [{c :coord l :lease} (get @held (str run-id))]
    (swap! held dissoc (str run-id))
    (noted/quietly "build-lock release" (coord/release! c l)))
  (noted/quietly "build-lock release" (ctl/delete! store :build-claims {:run-id (str run-id)}))
  (noted/quietly "build-lock release" (free-slot! store run-id)))

(defn revoke!
  [store run-id]
  (swap! held dissoc (str run-id))
  (let [n (claim-lease run-id)
        e (coord/revoke! (scope/coord) n)]
    (world/raise-fence! (assoc store :branch "main") n e))
  (noted/quietly "build-lock revoke" (ctl/delete! store :build-claims {:run-id (str run-id)}))
  (noted/quietly "build-lock revoke" (free-slot! store run-id)))

(defn cancel-queued!
  [store run-id]
  (pos? (long (or (ctl/delete! store :build-claims {:run-id (str run-id) :status "queued"}) 0))))

(defn queued
  [store]
  (noted/or-else "build queue" [] (ctl/list store :build-claims {:status "queued"})))

(defn queue-place
  [store run-id]
  (let [claims (ctl/list store :build-claims {})
        me     (some #(when (= (str run-id) (:run-id %)) %) claims)]
    (when (= "queued" (:status me))
      {:status        "queued"
       :queued-at     (str (:queued-at me))
       :queued-behind (mapv :run-id (blockers claims me))})))

(defn waited-behind
  [run-id]
  (get-in @held [(str run-id) :behind]))

(defn wait-step
  [{:keys [announced?] :as state} {:keys [admission handed-back? waiters]}]
  (let [{:keys [blockers]} admission]
    (case (:state admission)
      :admitted  [state [[:admitted]]]
      :cancelled [state [[:fail :cancelled]]]
      :queued    (if handed-back?
                   [state [[:fail :handed-back]]]
                   [(assoc state :announced? true)
                    (cond-> [[:wait blockers]]
                      (not announced?) (conj [:announce blockers])
                      true             (conj [:sleep (poll-after waiters)]))]))))

(defn- wait-effect!
  [run-id [effect x]]
  (let [id (str run-id)]
    (case effect
      :admitted (forget-wait! run-id)
      :fail     (throw (if (= :cancelled x)
                         (ex-info "build cancelled while queued" {:cancelled true :run-id id})
                         (ex-info "build handed back while queued"
                                  {:handed-back true :run-id id})))
      :wait     (do (swap! waiting conj id)
                    (swap! held update-in [id :behind] (fnil into #{}) x))
      :announce (do (println (str "[build] " run-id " queued behind " (pr-str x)))
                    (live/emit! {:kind :build/queued :level :info :run-id id :blocked-by x}))
      :sleep    (Thread/sleep (long x)))))

(defn acquire!
  [store {:keys [run-id branch reads writes origin targets]}]
  @renewer
  (let [c (scope/coord)
        l (or (coord/acquire! c {:name (claim-lease run-id) :owner (node/id) :ttl-ms node/lease-ms})
              (refusal/refuse! :refusal.builds/claim-held-elsewhere {:run-id (str run-id)}))]
    (swap! held assoc (str run-id) {:coord c :lease l})
    (ctl/insert! store :build-claims
                 {:run-id (str run-id) :branch (some-> branch str) :status "queued"
                  :reads (set reads) :writes (set writes) :origin origin :targets targets
                  :owner (node/id) :queued-at ctl/now})
    (try
      (loop [state {:announced? false}]
        (let [admission       (admit! store c run-id)
              seen            {:admission    admission
                               :handed-back? (contains? @handed-back (str run-id))
                               :waiters      (count (conj @waiting (str run-id)))}
              [state effects] (decisions/decide :build/admission state seen)]
          (run! #(wait-effect! run-id %) effects)
          (if (some #{[:admitted]} effects) l (recur state))))
      (catch Throwable t
        (release! store run-id)
        (throw t)))))

(defn with-claim
  [store {:keys [run-id] :as claim} f]
  (let [l (acquire! store claim)]
    (try (f (lane/holding store l))
         (finally (release! store run-id)))))
