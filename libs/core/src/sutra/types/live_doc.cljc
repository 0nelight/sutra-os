(ns sutra.types.live-doc
  #?(:clj (:require [malli.core :as m])))

(def compact-after
  200)

(def max-update
  (* 3 1024 1024))

(def Id [:string {:min 1 :max 64}])

(def Update
  [:string {:min 1 :max max-update}])

(def Peer
  [:map [:peer Id] [:user [:string {:max 255}]] [:client nat-int?] [:edit? :boolean]])

(def Entry [:map [:at pos-int?] [:update Update]])

(def Room
  [:map
   [:doc Id]
   [:clock nat-int?]
   [:log [:vector Entry]]
   [:peers [:vector Peer]]
   [:awareness [:map-of Id Update]]
   [:seed {:optional true} Id]
   [:compacting {:optional true} pos-int?]])

(defn- msg [t & fields] (into [:map [:type [:= t]]] fields))

(def Inbound
  [:multi {:dispatch :type}
   ["update" (msg "update" [:update Update])]
   ["awareness" (msg "awareness" [:update Update])]
   ["compacted" (msg "compacted" [:through pos-int?] [:state Update])]])

(def Outbound
  [:multi {:dispatch :type}
   ["sync" (msg "sync" [:peer Id] [:edit? :boolean] [:seed? :boolean]
                [:updates [:vector Update]] [:awareness [:vector Update]])]
   ["update" (msg "update" [:update Update])]
   ["awareness" (msg "awareness" [:update Update])]
   ["peers" (msg "peers" [:leader [:maybe Id]] [:peers [:vector Peer]])]
   ["left" (msg "left" [:client nat-int?])]
   ["compact" (msg "compact" [:through pos-int?])]
   ["reset" (msg "reset")]
   ["refused" (msg "refused" [:reason :string])]])

(def Event
  [:multi {:dispatch first}
   [:join [:tuple [:= :join] Peer]]
   [:leave [:tuple [:= :leave] Id]]
   [:receive [:tuple [:= :receive] Id Inbound]]
   [:reset [:tuple [:= :reset]]]])

(def Effect
  [:multi {:dispatch first}
   [:send [:tuple [:= :send] Id Outbound]]
   [:close [:tuple [:= :close] Id]]
   [:persist [:tuple [:= :persist] [:vector Update]]]
   [:retire [:tuple [:= :retire]]]
   [:discard [:tuple [:= :discard]]]])

#?(:clj
   (def ^:private inbound? (m/validator Inbound)))

#?(:clj
   (defn accept
     [msg]
     (when (inbound? msg) msg)))

(defn room
  [doc updates]
  {:doc       doc
   :clock     (count updates)
   :log       (into [] (map-indexed (fn [i u] {:at (inc i) :update u})) updates)
   :peers     []
   :awareness {}})

(defn updates [room] (mapv :update (:log room)))

(defn leader
  [room]
  (some #(when (:edit? %) (:peer %)) (:peers room)))

(defn- peer [room id] (some #(when (= id (:peer %)) %) (:peers room)))

(defn present?
  [room id]
  (boolean (peer room id)))

(defn- everyone [room] (mapv :peer (:peers room)))

(defn- others [room id] (filterv #(not= id %) (everyone room)))

(defn- send-to [ids m] (mapv (fn [id] [:send id m]) ids))

(defn- roster [room] {:type "peers" :leader (leader room) :peers (:peers room)})

(defn- join
  [room p]
  (let [seed? (boolean (and (:edit? p) (empty? (:log room)) (nil? (:seed room))))
        room' (cond-> (update room :peers conj p) seed? (assoc :seed (:peer p)))]
    [room'
     (into [[:send (:peer p) {:type      "sync"
                              :peer      (:peer p)
                              :edit?     (:edit? p)
                              :seed?     seed?
                              :updates   (updates room)
                              :awareness (vec (vals (:awareness room)))}]]
           (send-to (everyone room') (roster room')))]))

(defn- leave
  [room id]
  (if-let [p (peer room id)]
    (let [room' (cond-> (-> room
                            (update :peers (fn [ps] (filterv #(not= id (:peer %)) ps)))
                            (update :awareness dissoc id))
                  (= id (:seed room))   (dissoc :seed)
                  (= id (leader room))  (dissoc :compacting))]
      [room'
       (if (empty? (:peers room'))
         [[:retire]]
         (into (send-to (everyone room') {:type "left" :client (:client p)})
               (send-to (everyone room') (roster room'))))])
    [room []]))

(defn- append
  [room u]
  (let [at (inc (:clock room))]
    (-> room (assoc :clock at) (update :log conj {:at at :update u}))))

(defn- ask-compaction
  [room]
  (let [l (leader room)]
    (if (and l (nil? (:compacting room)) (> (count (:log room)) compact-after))
      [(assoc room :compacting (:clock room))
       [[:send l {:type "compact" :through (:clock room)}]]]
      [room []])))

(defn- on-update
  [room id u]
  (let [[room' fx] (ask-compaction (append room u))]
    [room' (-> (send-to (others room id) {:type "update" :update u})
               (conj [:persist (updates room')])
               (into fx))]))

(defn- on-awareness
  [room id u]
  [(assoc-in room [:awareness id] u)
   (send-to (others room id) {:type "awareness" :update u})])

(defn- on-compacted
  [room id {:keys [through state]}]
  (if (and (= id (leader room)) (= through (:compacting room)))
    (let [room' (-> room
                    (dissoc :compacting)
                    (update :log (fn [log]
                                   (into [{:at through :update state}]
                                         (filter #(> (:at %) through))
                                         log))))]
      [room' [[:persist (updates room')]]])
    [room []]))

(defn- receive
  [room id {:keys [type] :as m}]
  (let [p (peer room id)]
    (cond
      (nil? p)             [room []]
      (= "awareness" type) (on-awareness room id (:update m))
      (not (:edit? p))     [room [[:send id {:type "refused" :reason "read-only"}]]]
      (= "update" type)    (on-update room id (:update m))
      (= "compacted" type) (on-compacted room id m)
      :else                [room []])))

(defn- reset
  [room]
  (let [ids (everyone room)]
    [(assoc room :peers [] :awareness {})
     (-> (send-to ids {:type "reset"})
         (into (map (fn [id] [:close id])) ids)
         (conj [:discard]))]))

(defn step
  [room [kind a b]]
  (case kind
    :join    (join room a)
    :leave   (leave room a)
    :receive (receive room a b)
    :reset   (reset room)))
