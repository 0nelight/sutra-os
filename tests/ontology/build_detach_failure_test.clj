(ns ^:api ontology.build-detach-failure-test
  (:require [support.wire :refer [ok?]]
            [clojure.test :refer [deftest is use-fixtures]]
            [sutra.builds.plan :as plan]
            [sutra.builds.run.queue :as queue]
            [sutra.wire.api :as api]
            [support.in-project :refer [in-project]]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-pod)

(defn- settled [id]
  (loop [n 100]
    (let [r (api/get! :run {:id id})]
      (cond (#{"failed" "succeeded"} (get-in r [:body :status])) r
            (zero? n) r
            :else (do (Thread/sleep 100) (recur (dec n)))))))

(deftest a-detached-build-that-never-began-names-its-reason
  (is (ok? (api/mutate :create-project {:name :detached} {:stage? true})))
  (is (ok? (api/mutate :create-dataset
                       {:name :seed_d :project :detached
                        :columns [{:attribute-name :k :attribute-type :string}]}
                       {:stage? true})))
  (is (ok? (api/post! :staging)))
  (with-redefs [plan/build-plan (fn [& _] (throw (ex-info "the plan does not resolve" {})))]
    (do
      (let [started (api/post! :import-run nil {:detach true})
            id      (get-in started [:body :run-id])
            run     (settled id)]
        (is (= "started" (get-in started [:body :status])))
        (is (= 200 (:status run)) "the handle is known")
        (is (= "failed" (get-in run [:body :status])))
        (is (re-find #"does not resolve" (str (get-in run [:body :error-message]))))
        (is (some #(= (str id) (str (:id %))) (:runs (:body (api/get! :import-runs))))
            "and the run stands in the list")))))

(deftest a-request-answers-for-the-run-it-will-be
  (let [now 1000
        run #(queue/request-run (merge {:run-id "r"} %) now)]
    (is (= "queued" (:status (run {:status "queued"}))))
    (is (= "running" (:status (run {:status "running"
                                    :claimed-until (java.util.Date. (+ now 5000))}))))
    (is (= {:status "failed" :error-message "kaputt"}
           (select-keys (run {:status "failed" :error "kaputt"}) [:status :error-message])))
    (is (= "failed" (:status (run {:status "running" :claimed-until (java.util.Date. 1)})))
        "its worker died before the run began")
    (is (= "cancelled" (:status (run {:status "cancelled"}))))
    (is (nil? (run {:status "done" :result {}})) "a finished build has its row")))
