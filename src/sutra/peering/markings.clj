(ns sutra.peering.markings
  (:require [sutra.actions.api :as actions]
            [sutra.authoring.api :as authoring]
            [sutra.governance.api :as governance]
            [sutra.peering.objects :as objects]
            [sutra.store.access.scope :as scope]
            [sutra.store.protection :as protection]
            [sutra.store.seat :as seat]
            [sutra.store.world :as world]))

(set! *warn-on-reflection* true)

(defn declared
  [schema]
  (into #{} (map (comp name key)) (:markings schema)))

(defn- guard [br seen]
  (fn [store touched rows]
    (-> (governance/write-guard store seen br rows {})
        (protection/protection-guard br touched))))

(defn fragment
  [connection-id names]
  {:markings (into {} (for [n names]
                        [(keyword n)
                         {:description (str "a marking of the instance at the other end of"
                                            " peer connection " connection-id
                                            "; nobody is cleared until someone is")}]))})

(defn declare!
  [store connection-id names]
  (let [br   (objects/home connection-id)
        bs   (scope/as-system (world/at store br))
        live (seat/branch-schema bs)
        new  (vec (distinct (remove (declared live) (map str names))))]
    (when (seq new)
      (authoring/publish-schema! bs (fragment connection-id new) live
                                 {:provenance (str "peer/" connection-id)
                                  :planned    (authoring/on-index bs br)}
                                 (assoc actions/commit-callbacks
                                        :guard (guard br (protection/guard-reads bs)))))
    new))
