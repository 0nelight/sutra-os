(ns sutra.builds.run.leader
  (:require [sutra.store.access.scope :as ctx]
            [clojure.string :as str]
            [sutra.coord.api :as coord]
            [sutra.foundation.periodic :as periodic]
            [sutra.store.storage.node :as node]
            [sutra.store.world :as world]
            [sutra.kit.noted :as noted]))

(set! *warn-on-reflection* true)

(def ^:private leader-name "leader")

(def ^:private lock-ttl-ms 60000)

(defonce ^:private state
  (atom {:armed? false :lease nil}))

(defonce ^:private ticker (periodic/slot))

(defn- lose! [why]
  (when-let [{:keys [epoch]} (:lease @state)]
    (swap! state assoc :lease nil)
    (println (str "[leader] " (node/id) " lost the lease (epoch " epoch "): " why))))

(defn- tick!
  []
  (let [c (:coord @state)]
    (if-let [l (:lease @state)]
      (if-let [l' (coord/renew! c l node/lease-ms)]
        (swap! state assoc :lease l')
        (lose! "renewal refused"))
      (when-let [l (coord/acquire! c {:name leader-name :owner (node/id) :ttl-ms node/lease-ms})]
        (world/raise-fence! (ctx/store "main") leader-name (:epoch l))
        (swap! state assoc :lease l)
        (println (str "[leader] " (node/id) " holds the lease (epoch " (:epoch l) ")"))))))

(defn leader?
  []
  (let [{:keys [armed? lease]} @state]
    (or (not armed?) (some? lease))))

(defn as-leader
  [f]
  (let [{:keys [armed? lease]} @state]
    (cond
      (not armed?) (f [])
      lease        (f [[:lease leader-name (:epoch lease)]]))))

(defn status []
  (let [{:keys [armed? lease]} @state]
    {:node (node/id) :armed? armed? :leader? (leader?) :epoch (:epoch lease)}))

(defn start!
  []
  (swap! state assoc :armed? true :coord (ctx/coord))
  (if (#{:api :worker} node/role)
    (println (str "[leader] " (node/id) " is an " (name node/role)
                  " node and never takes the lease"))
    (noted/quietly "leader tick" (tick!)))
  (when-not (#{:api :worker} node/role)
    (let [ms (max 1000 (quot node/lease-ms 3))]
      (periodic/start! ticker "sutra-leader" ms ms
                       #(when (:armed? @state)
                          (try (tick!) (catch Exception e (noted/note "leader tick" e)))))))
  (status))

(defn stop!
  []
  (swap! state assoc :armed? false)
  (periodic/stop! ticker)
  (when-let [l (:lease @state)]
    (noted/quietly "leader release" (coord/release! (:coord @state) l)))
  (lose! "stopping"))

(defn with-lock
  [c lock-name f on-busy]
  (if-let [l (coord/acquire! c {:name (str lock-name) :owner (node/id) :ttl-ms lock-ttl-ms})]
    (let [held (atom l)]
      (try (periodic/renewing (str "sutra-lock " lock-name) (quot lock-ttl-ms 3)
                              #(some->> (coord/renew! c @held lock-ttl-ms) (reset! held))
                              f)
           (finally (noted/quietly "lock release" (coord/release! c @held)))))
    (on-busy)))

(defn lock-name [& parts] (str/join ":" parts))

(def module
  {:name :lease
   :jobs [{:name :lease :start start! :stop stop!}]})
