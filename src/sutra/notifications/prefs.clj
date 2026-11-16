(ns sutra.notifications.prefs
  (:require [sutra.store.control :as ctl]))

(set! *warn-on-reflection* true)

(def kinds
  #{:action :automation :automation-triggered :automation-recovered :automation-errors
    :automation-info :review :branch :mention :health})

(def ways #{:platform :email :both :off})

(def defaults
  {:delivery {:default :platform :health :both :branch :both}})

(defn way
  [settings kind automation-id]
  (if (and automation-id (contains? (set (:muted settings)) (str automation-id)))
    :muted
    (let [d (merge (:delivery defaults) (:delivery settings))]
      (get d kind (:default d)))))

(defn channels
  [w]
  (case w
    :platform #{:inbox :live}
    :email    #{:inbox :email}
    :both     #{:inbox :live :email}
    :off      #{:inbox}
    #{}))

(defn problems
  [{:keys [delivery muted] :as settings}]
  (let [bad-kind (remove (conj kinds :default) (keys delivery))
        bad-way  (remove ways (vals delivery))]
    (cond (not (map? settings)) "settings are a map {:delivery {kind way} :muted [...]}"
          (seq bad-kind) (str "unknown kinds " (vec bad-kind) " — known: " (sort kinds))
          (seq bad-way)  (str "unknown ways " (vec bad-way) " — known: " (sort ways))
          (and muted (not (coll? muted))) ":muted is a list of automation ids")))

(defn of-users
  [store usernames]
  (into {}
        (for [u usernames]
          [u (or (:settings (ctl/get store :notification-preferences {:username (str u)})) {})])))

(defn of-user [store username] (get (of-users store [username]) username))

(defn normalized
  [settings]
  (cond-> (select-keys settings [:delivery :muted])
    (:delivery settings) (update :delivery
                                 #(into {} (map (fn [[k v]] [(keyword (name k)) (keyword v)])) %))
    (:muted settings)    (update :muted #(vec (distinct (map str %))))))

(defn save!
  [store username settings]
  (ctl/commit! store [[:upsert :notification-preferences
                       [{:username (str username) :settings settings}]]])
  settings)
