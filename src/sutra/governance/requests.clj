(ns sutra.governance.requests
  (:require [sutra.store.control :as ctl]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [sutra.types.review :as review]
            [sutra.access.api :as access]
            [sutra.catalog.branch-policy :refer [actor-name]]
            [sutra.governance.proposals :as proposals])
  (:import (java.util UUID)))

(set! *warn-on-reflection* true)

(defn- request-row->value
  [r]
  (when r
    (merge (some-> (:payload r) not-empty edn/read-string)
           {:id (:id r) :kind (keyword (:kind r)) :status (:status r) :author (:author r)
            :justification (:justification r) :created-at (str (:created-at r))
            :resolved-at (some-> (:resolved-at r) str) :resolved-by (:resolved-by r)
            :applied (some-> (:applied r) not-empty edn/read-string)})))

(defn fetch-request
  [store id]
  (when-let [r (request-row->value (ctl/get store :access-requests {:id (str id)}))]
    (assoc r :tasks (access/tasks r) :reviews (proposals/reviews-of store (:id r)))))

(defn list-requests
  [store {:keys [status]}]
  (mapv #(fetch-request store (:id %))
        (ctl/list store :access-requests
                  (cond-> {} (not= "all" status) (assoc :status (or status "open")))
                  {:order [[:created-at :desc]]})))

(defn create-request!
  [store fields justification]
  (if-let [ps (access/problems fields)]
    (proposals/refused :refusal.governance/request-invalid {:problems (vec ps)}
                       {:why (str/join "; " ps)})
    (let [id (str (UUID/randomUUID))]
      (ctl/insert! store :access-requests
                   {:id id :kind (name (:kind fields)) :author (actor-name store)
                    :payload (pr-str (dissoc fields :kind))
                    :justification (some-> justification str not-empty)})
      (fetch-request store id))))

(defn judge-request!
  [store id verdict note owns?]
  (let [r (fetch-request store id)]
    (cond
      (nil? r)                  (proposals/refused :refusal.governance/no-request {} {:id id})
      (not= "open" (:status r)) (proposals/refused :refusal.governance/request-not-open {}
                                                   {:status (:status r)})
      :else
      (let [me  (actor-name store)
            ctx {:author (:author r)}
            ap  (fn [t]
                  {:who me
                   :owner? (boolean (or (nil? owns?) (and (:project t) (owns? (:project t)))))})
            may (filter #(review/eligible? % (ap %) ctx) (:tasks r))]
        (if (empty? may)
          (proposals/refused :refusal.governance/request-not-eligible {}
                             {:needs (str/join "; "
                                               (for [t (:tasks r)]
                                                 (str/join " or "
                                                           (map review/describe
                                                                (:requires t)))))})
          (do (proposals/verdicts-decided! r me verdict may (constantly true)
                                 #(str/join " or " (map review/describe (:requires %))))
              (ctl/insert-many! store :proposal-verdicts
                                (for [t may]
                                  {:proposal-id (str id) :approver me :task (:task t)
                                   :verdict verdict
                                   :fingerprint (:fingerprint t) :note note}))
              (fetch-request store id)))))))

(defn resolve-request!
  [store id to applied]
  (ctl/update! store :access-requests {:id (str id)}
               (fn [_]
                 (cond-> {:status (name to) :resolved-at ctl/now :resolved-by (actor-name store)}
                   applied (assoc :applied (pr-str applied)))))
  (fetch-request store id))
