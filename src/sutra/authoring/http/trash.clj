(ns sutra.authoring.http.trash
  (:require [clojure.string :as str]
            [sutra.authoring.mutation :as mutation]
            [sutra.http.endpoint :as ep]
            [sutra.http.responses :refer [ok]]
            [sutra.kit.refusal :as refusal]
            [sutra.store.access.scope :as ctx]
            [sutra.store.world :as world]
            [sutra.types.rid :as rid]))

(set! *warn-on-reflection* true)

(def ^:private restorable-sections
  {:object-type :object-types :link-type :link-types
   :transform :transforms :function :functions})

(defn- schema-constructs
  [sm]
  (for [[kind section] restorable-sections
        [rid spec]      (get sm section)
        :let [nm (get spec (rid/name-field-of section))]
        :when nm]
    {:kind kind :name (name nm) :rid rid :spec spec :section section}))

(def ^:private trash-history-depth 100)

(defn- constructs-of
  [deleted]
  (let [section->kind (into {} (map (fn [[k s]] [s k])) restorable-sections)]
    (for [{:keys [kind rid seq spec]} deleted
          :let [k  (section->kind kind)
                nm (when (and k spec) (get spec (rid/name-field-of kind)))]
          :when nm]
      {:kind k :name (name nm) :rid rid :spec spec :section kind :at seq})))

(defn- deleted-constructs
  [store branch]
  (constructs-of (world/deleted (world/at store branch) trash-history-depth)))

(defn- retire-cascade
  [store branch at rid spec]
  (let [rid-s  (str rid)
        spec-s (pr-str spec)]
    (for [{d-rid :rid d-spec :spec seq :seq
           :as d} (world/deleted (world/at store branch) trash-history-depth)
          :when (and (= seq at) (not= d-rid rid)
                     (or (str/includes? spec-s (str d-rid))
                         (str/includes? (pr-str d-spec) rid-s)))]
      d)))

(def list-trash
  (ep/handler :trash
              {:call (fn [{:keys [store branch]}]
                       (let [{:keys [deleted complete? reaches-back-to]}
                             (world/removed (world/at store branch) trash-history-depth)]
                         (cond-> {:trash (vec (for [{:keys [kind name rid at]}
                                                    (constructs-of deleted)]
                                                {:kind kind :name name :rid rid :at at}))
                                  :complete? complete?}
                           (not complete?) (assoc :reaches-back-to reaches-back-to))))}))

(def restore-construct
  (ep/handler :trash-restore
              {:respond
               (fn [{:keys [store branch schema body edge] project :sutra/project}]
                 (let [kind    (keyword (:kind body))
                       nm      (name (:name body ""))
                       section (restorable-sections kind)]
                   (cond
                     (nil? section)
                     (refusal/refuse! :refusal.authoring/unrestorable-kind body
                                      {:kinds (str/join
                                               " / "
                                               (sort (map name (keys restorable-sections))))})

                     (some #(= [kind nm] [(:kind %) (:name %)]) (schema-constructs schema))
                     (refusal/refuse! :refusal.authoring/restore-exists {}
                                      {:kind-name (clojure.core/name kind) :name nm})

                     :else
                     (if-let [hit (first (filter #(= [kind nm] [(:kind %) (:name %)])
                                                 (deleted-constructs store branch)))]
                       (let [{:keys [rid spec at]} hit
                             spec    (-> spec (assoc :status :experimental) (dissoc :deprecation))
                             cascade (retire-cascade store branch at rid spec)
                             result  (mutation/merge-and-install (ctx/at store branch) schema
                                                        (fn [s]
                                                          (reduce (fn [s {:keys [kind rid spec]}]
                                                                    (assoc-in s [kind rid] spec))
                                                                  (assoc-in s [section rid] spec)
                                                                  cascade))
                                                        {:project project :edge edge})]
                         (ok (assoc result :restored {:kind kind :name nm :rid rid :from-seq at
                                                      :with
                                                        (mapv #(select-keys % [:kind :rid])
                                                              cascade)})))
                       (refusal/refuse! :refusal.authoring/no-deleted {:name nm}
                                        {:kind-name (clojure.core/name kind)})))))}))

(def module
  {:name        :trash
   :contributes {:http/routes {:trash         {:get list-trash}
                               :trash-restore {:post restore-construct}}}})
