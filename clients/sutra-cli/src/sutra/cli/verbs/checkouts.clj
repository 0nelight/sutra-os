(ns sutra.cli.verbs.checkouts
  (:require [sutra.cli.dispatch :refer [step!]]
            [sutra.cli.projection :as projection]
            [sutra.cli.slice :as slice]
            [sutra.wire.transport :as tx]))

(defn- named-branch
  [{:keys [session]}]
  (if-let [s (some-> session deref)]
    (let [b (:branch s)]
      (when (or (not= "main" b) (= "main" (get-in s [:opts :branch]))) b))
    tx/*branch*))

(defn- opts-of
  [ctx]
  {:branch     (named-branch ctx)
   :project    tx/*project*
   :json?      (:json? ctx)
   :creds-path (:creds-path ctx)})

(defn- code->state [code] (when (and code (not (zero? code))) :fail))

(defn- needs!
  [xs what]
  (when (empty? xs)
    (throw (ex-info (str "missing argument " what) {:missing what}))))

(defn- resolve-of [{:keys [mine? theirs?]} mine theirs]
  (cond mine? mine theirs? theirs))

(defmethod step! :projection-list [ctx _]
  (code->state (projection/list! (opts-of ctx))))

(defmethod step! :projection-find [ctx [_ {:keys [needle projection]}]]
  (code->state (projection/find! (opts-of ctx) needle {:projection projection})))

(defmethod step! :projection-where [ctx [_ {:keys [names projection]}]]
  (needs! names "names")
  (code->state (projection/where! (opts-of ctx) names {:projection projection})))

(defmethod step! :projection-pull [ctx [_ {:keys [projection texts project to force? rehome?]}]]
  (code->state (projection/pull! (opts-of ctx) projection
                                 {:texts   (vec texts)
                                  :project project
                                  :to      to
                                  :force?  (boolean force?)
                                  :rehome? (boolean rehome?)})))

(defmethod step! :projection-push
  [ctx [_ {:keys [texts preview? adopt? land? stage?] :as args}]]
  (code->state
   (projection/push! (opts-of ctx) (vec texts)
                     (cond-> {:preview? (boolean preview?)
                              :adopt?   (boolean adopt?)}
                       (resolve-of args :mine :theirs) (assoc :resolve
                                                              (resolve-of args :mine :theirs))
                       land?  (assoc :land? true)
                       stage? (assoc :land? false)))))

(defmethod step! :slice-pull [ctx [_ {:keys [nodes to force?]}]]
  (needs! nodes "nodes")
  (code->state (slice/pull! (opts-of ctx) (vec nodes) {:to to :force? (boolean force?)})))

(defmethod step! :slice-push [ctx [_ {:keys [dir preview? message] :as args}]]
  (code->state
   (slice/push! (opts-of ctx) dir
                (cond-> {:preview? (boolean preview?)}
                  message                             (assoc :message message)
                  (resolve-of args "mine" "theirs") (assoc :resolve
                                                           (resolve-of args "mine" "theirs"))))))
