(ns sutra.agents.threads
  (:refer-clojure :exclude [list])
  (:require [clojure.string :as str]
            [sutra.store.control :as ctl]
            [sutra.catalog.grants :as grants]))

(set! *warn-on-reflection* true)

(def ^:private title-max 80)

(defn owner-of
  [store]
  (str (or (grants/acting-user (grants/actor store))
           (grants/acting-username (grants/actor store))
           "system")))

(defn- title-of [input]
  (let [s (str/trim (str/replace (str input) #"\s+" " "))]
    (if (> (count s) title-max) (str (subs s 0 (dec title-max)) "…") s)))

(defn create!
  [store {:keys [agent branch title]}]
  (ctl/insert! store :agent-threads {:id (str (random-uuid)) :owner (owner-of store)
                                  :agent (name agent) :branch branch
                                  :title (some-> title title-of)}))

(defn list
  [store {:keys [limit]}]
  (ctl/list store :agent-threads {:owner (owner-of store)}
            {:limit (max 1 (min 200 (or limit 50)))}))

(defn one
  [store id]
  (let [t (ctl/get store :agent-threads {:id (str id)})]
    (when (and t (= (:owner t) (owner-of store))) t)))

(defn messages [store thread-id]
  (ctl/list store :agent-messages {:thread-id (str thread-id)} {:limit 1000}))

(defn- next-seq [store thread-id]
  (inc (reduce max 0 (map :seq (messages store thread-id)))))

(defn add!
  [store thread-id {:keys [role content status run-id detail]}]
  (let [m (ctl/insert! store :agent-messages
                       (cond-> {:thread-id (str thread-id) :seq (next-seq store thread-id)
                                :role role :content content :status (or status "done")}
                         run-id (assoc :run-id run-id)
                         detail (assoc :detail detail)))]
    (ctl/commit! store [[:touch :agent-threads {:id (str thread-id)}]])
    m))

(defn update-message!
  [store thread-id seq sets]
  (ctl/update! store :agent-messages {:thread-id (str thread-id) :seq seq} sets))

(defn titled!
  [store thread input]
  (when (str/blank? (:title thread))
    (ctl/update! store :agent-threads {:id (:id thread)} {:title (title-of input)})))

(defn delete!
  [store id]
  (ctl/delete! store :agent-messages {:thread-id (str id)})
  (ctl/delete! store :agent-threads {:id (str id)}))

(defn attachment-hashes
  [store]
  (into #{}
        (comp (filter #(= "user" (:role %)))
              (mapcat #(get-in % [:detail :attachments]))
              (keep :hash))
        (ctl/list store :agent-messages {:role "user"})))
