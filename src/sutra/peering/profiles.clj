(ns sutra.peering.profiles
  (:require [sutra.types.plan :as cp]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [sutra.access.api :as access]
            [sutra.kit.refusal :as refusal]
            [sutra.peering.connections :as conns]
            [sutra.peering.rows :as rows]
            [sutra.peering.wire :as wire]
            [sutra.types.peering :as peering]))

(set! *warn-on-reflection* true)

(def peer-path "/api/peering/peer")

(defn- kw [x] (when (some? x) (keyword (name x))))

(defn- yes? [x] (contains? #{true "true"} x))

(defn- outbound-trusts
  [store]
  (vec (for [[n t] (access/instance-trusts store) :when (:outbound? t)] (str n))))

(defn- edn-of
  [x]
  (if (string? x)
    (try (edn/read-string x)
         (catch Exception e
           (refusal/refuse! :refusal.peering/profile-not-edn {:text x} {:why (ex-message e)})))
    x))

(defn strategy-of
  [s]
  (cond
    (map? s) (update s :kind kw)
    (str/blank? (str s)) {:kind :all}
    :else (let [[k v] (str/split (str s) #":" 2)
                k (kw (str/trim k))]
            (cond-> {:kind k}
              (= :instance k) (assoc :instance (some-> v str/trim))
              (= :labels k) (assoc :labels (vec (remove str/blank?
                                                        (map str/trim (str/split (str v)
                                                                                 #",")))))))))

(defn- normal
  [p]
  (-> p
      (update :side kw)
      (update :types edn-of)
      (update :strategy strategy-of)
      (update :data-types peering/normal-data-types)
      (update :types (fn [ts] (mapv #(-> %
                                         (update :source-data (fn [d] (or (kw d) :none)))
                                         (update :action-data (fn [d] (or (kw d) :none))))
                                    ts)))
      (update :ceiling #(or % {:classification nil :markings []}))
      (update :auto-connect yes?)))

(defn- keep!
  [store actor p]
  (let [p (normal p)]
    (cp/check! (cp/of-findings (cp/problems :peering/profile (peering/profile-problems p)))
               {:code :refusal.peering/profile-not-kept})
    (rows/put-profile! store (assoc p :profile-id (or (:profile-id p) (str (random-uuid)))
                                    :created-by actor))))

(defn create-pair!
  [store actor p]
  (let [flat [:source-data :action-data :media :datasets :classification :markings]
        p    (cond-> (apply dissoc p flat)
               (nil? (:data-types p)) (assoc :data-types (conns/data-types-of p))
               (and (nil? (:ceiling p)) (or (:classification p) (:markings p)))
               (assoc :ceiling (conns/ceiling-of p)))
        row  (keep! store actor (assoc p :pair (or (:pair p) (str (random-uuid)))
                                       :side (or (:side p) :hub)))]
    {:profile row :counterpart (peering/counterpart row)}))

(defn install!
  [store actor {:keys [profile space strategy auto-connect]}]
  (let [p (update-keys (edn-of profile) keyword)]
    (keep! store actor (cond-> (assoc (select-keys p [:pair :side :name :ceiling :data-types
                                                      :types :auto-connect])
                                      :space space)
                         strategy (assoc :strategy strategy)
                         (some? auto-connect) (assoc :auto-connect (yes? auto-connect))))))

(defn view
  [store p]
  (let [cs (filter #(= (:profile-id p) (:profile-id %)) (rows/connections store))]
    (assoc p
           :targets (peering/targets p (outbound-trusts store) (rows/mesh store))
           :connections (mapv #(select-keys % [:connection-id :name :trust :status]) cs))))

(defn- connection-of [store p trust]
  (first (filter #(and (= (:profile-id p) (:profile-id %)) (= (str trust) (:trust %)))
                 (rows/connections store))))

(defn- offer!
  [store p c]
  (wire/deliver! store (:trust c) peer-path
                 {:op :profile-offer :connection-id (:connection-id c) :pair (:pair p)
                  :invite (:invite c)}))

(defn- connect-targets!
  [store p]
  (when (and (:auto-connect p) (= :hub (kw (:side p))))
    (doseq [t (peering/targets p (outbound-trusts store) (rows/mesh store))
            :let [c (or (connection-of store p t)
                        (conns/connect! store (:created-by p)
                                        {:name (str (:name p) " → " t) :trust t
                                         :space (:space p) :profile-id (:profile-id p)
                                         :data-types (:data-types p)
                                         :classification (get-in p [:ceiling :classification])
                                         :markings (get-in p [:ceiling :markings])}))
                  c (rows/connection store (:connection-id c))]
            :when (= :requested (kw (:status c)))]
      (try (offer! store p c)
           (catch clojure.lang.ExceptionInfo e
             (rows/job! store {:connection-id (:connection-id c) :data-type :source-data
                               :way :out :state :failed
                               :error (str "the profile's invite did not reach " t ": "
                                           (ex-message e))}))))))

(defn take-offer!
  [store signer {:keys [connection-id pair invite]}]
  (when-not (some #(= (str pair) (:pair %)) (rows/profiles store))
    (refusal/refuse! :refusal.peering/no-profile-of-pair {:pair pair}))
  (rows/put-offer! store {:connection-id (str connection-id) :trust (str signer)
                          :pair (str pair) :invite (str invite)})
  {:ok true})

(defn- accept-offers!
  [store]
  (let [trusts (outbound-trusts store) mesh (rows/mesh store)]
    (doseq [o (rows/offers store)
            :let [p (first (filter #(peering/profile-offer-stands? % (:pair o) (:trust o)
                                                                   trusts mesh)
                                   (rows/profiles store)))]
            :when p]
      (try
        (when-not (rows/connection store (:connection-id o))
          (conns/accept! store (:created-by p) {:invite (:invite o) :space (:space p)}))
        (rows/put-connection! store (assoc (rows/connection store (:connection-id o))
                                           :profile-id (:profile-id p)))
        (rows/drop-offer! store (:connection-id o))
        (catch clojure.lang.ExceptionInfo e
          (rows/put-offer! store (assoc o :error (ex-message e))))))))

(defn- activate-accepted!
  [store]
  (doseq [c (rows/connections store)
          :when (and (:profile-id c) (= :originator (kw (:role c)))
                     (= :accepted (kw (:status c))))]
    (try (conns/activate! store (:connection-id c))
         (catch clojure.lang.ExceptionInfo e
           (println (str "[peering] activating connection " (:connection-id c)
                         " failed, next round tries again: " (ex-message e)))))))

(defn- map-types!
  [store]
  (doseq [c (rows/connections store)
          :when (and (:profile-id c) (= :active (kw (:status c))))
          :let [p (rows/profile store (:profile-id c))]
          :when p
          t (:types p)
          :when (nil? (rows/mapping store (:connection-id c) (:local-type t)))]
    (try (conns/map-type! store (:created-by p) (:connection-id c) t)
         (catch clojure.lang.ExceptionInfo e
           (rows/job! store {:connection-id (:connection-id c) :data-type :source-data
                             :way :out :state :failed :local-type (str (:local-type t))
                             :error (str "the profile's type " (:local-type t)
                                         " cannot be mapped yet: " (ex-message e))})))))

(defn round!
  [store]
  (doseq [p (rows/profiles store)]
    (try (connect-targets! store p)
         (catch clojure.lang.ExceptionInfo e
           (println (str "[peering] profile " (:profile-id p)
                         " did not reach its targets, next round tries again: "
                         (ex-message e))))))
  (accept-offers! store)
  (activate-accepted! store)
  (map-types! store))

(defn set-mesh-node!
  [store trust labels remove?]
  (when-not (get (access/instance-trusts store) (str trust))
    (refusal/refuse! :refusal.peering/no-trust {:trust trust}))
  (if remove?
    (rows/drop-mesh-node! store trust)
    (rows/put-mesh-node! store trust (if (string? labels)
                                       (remove str/blank? (map str/trim
                                                               (str/split labels #",")))
                                       labels)))
  {:mesh (rows/mesh store)})
