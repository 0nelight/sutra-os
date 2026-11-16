(ns enta.live
  (:require [sutra.change.server :as srv]
            [sutra.types.rid :as rid]
            [sutra.types.link-side :as ls]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(defn schema->ctx
  [canon]
  (let [idx (rid/rid->name-index canon)
        nm  (fn [x] (rid/resolve-rid->name idx x))]
    {:props (into {} (for [[k v] (:object-types canon)]
                       [(nm k) (set (map :attribute-name (:attribute-list v)))]))
     :links (into {} (for [[k v] (:link-types canon)]
                       [(nm k) (ls/table-entry v nm)]))}))

(defn ctx
  [dir]
  (try
    (when-let [sess (srv/verbinden! dir)]
      (binding [tx/*auth-token* (:token sess)]
        (let [r (api/get! :schema)]
          (when (<= 200 (:status r) 299)
            (schema->ctx (:body r))))))
    (catch Exception _ nil)))

(defn app-file
  [dir nm]
  (try
    (when-let [sess (srv/verbinden! dir)]
      (binding [tx/*auth-token* (:token sess)]
        (let [r (api/get! :application-file {:name (name nm)})]
          (when (<= 200 (:status r) 299) (:file (:body r))))))
    (catch Exception _ nil)))

(defn why-not [dir] (srv/warum-nicht dir))
