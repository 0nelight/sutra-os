(ns support.wire
  (:require [sutra.wire.api :as api]))

(defn ok? [{:keys [status]}] (= 200 status))

(defn uid [] (str (java.util.UUID/randomUUID)))

(defn stage! [verb args] (api/mutate verb args {:stage? true}))

(defn commit! [] (api/post! :staging))

(defn publish!
  [nm]
  (let [r (api/post! :resource-versions-publish
                     {:section "applications" :name (name nm)} {})]
    (if (ok? r) (commit!) r)))

(defn eval-run!
  [body]
  (let [{:keys [status] start :body :as r} (api/post! :eval-run body)]
    (if (not= 200 status)
      r
      (loop [n 0]
        (let [j (:body (api/get! :eval-job {:id (:id start)}))]
          (cond
            (= "done" (:status j))    {:status 200 :body (:result j)}
            (not= "running" (:status j)) {:status 500 :body j}
            (> n 1200)                (throw (ex-info "eval job did not end" j))
            :else                     (do (Thread/sleep 100) (recur (inc n)))))))))
