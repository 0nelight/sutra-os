(ns sutra.store.seat
  (:require [sutra.kit.attempt :as attempt]
            [sutra.store.access.scope :as ctx]
            [sutra.store.kept :as kept]
            [sutra.store.memo :as memo]
            [sutra.store.world :as world]
            [sutra.foundation.env :as env]))

(set! *warn-on-reflection* true)

(defonce ^:private boot-schema-error (atom nil))
(defn schema-load-error  [] @boot-schema-error)
(defn set-schema-load-error! [m] (reset! boot-schema-error m))
(defn clear-schema-load-error! [] (reset! boot-schema-error nil))

(defn seated-branch
  []
  (env/env "SUTRA_BRANCH" "main"))

(defonce ^:private schemas (atom {}))

(def ^:private schemas-spec {::schema {:cap 64}})

(defn- schema-key
  [store]
  (let [rev  (:rev (world/basis store))
        tick (world/schema-tick store)]
    (if (and rev tick (<= tick rev)) [:tick tick] rev)))

(defn branch-schema
  [store]
  (let [{:keys [rev]} (world/basis store)
        b             (str (:branch store))
        home          (when-let [h (and (:instance-keys store) (world/instance-home store))]
                        (schema-key (world/at store h)))
        read          #(kept/request-schema store b rev)]
    (if (nil? rev)
      (read)
      (memo/through schemas schemas-spec ::schema [(:db store) b (schema-key store) home]
                    read))))

(defn rebuild-schema-from-storage
  [store]
  (branch-schema store))

(defn on-branch
  [tag f]
  (fn [store]
    (let [branch (seated-branch)]
      (try (f (ctx/at store branch) branch)
           (catch Throwable t
             (attempt/rethrow-fatal! t)
             (println (str "[" tag "] tick error: " (.getMessage t))))))))
