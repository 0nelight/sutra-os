(ns ^:unit ontology.health-digest-test
  (:require [clojure.test :refer [deftest is]]
            [sutra.health.watch :as watch]
            [sutra.store.control :as ctl]
            [support.mem :as mem]))

(defn- sub [store user]
  (ctl/get store :health-group-subscriptions
           {:group-id "g" :branch "main" :username user}))

(deftest a-digest-stamps-only-its-own-subscription
  (let [store (mem/store)
        g     (watch/save-group! store "main" {:id "g" :name "g" :check-ids []} "admin")
        _     (watch/subscribe! store "main" (:id g) "due" 60000 false)
        _     (watch/subscribe! store "main" (:id g) "later" 3600000 false)
        sent  (java.util.Date. (- (System/currentTimeMillis) 60000))
        _     (ctl/update! store :health-group-subscriptions
                           {:group-id "g" :branch "main" :username "later"}
                           {:last-sent-at sent})
        _     (watch/send-digests! store)]
    (is (some? (:last-sent-at (sub store "due"))) "the due subscription is stamped")
    (is (= (.getTime sent) (.getTime ^java.util.Date (:last-sent-at (sub store "later"))))
        "a subscription that was not due keeps its last-sent-at")))
