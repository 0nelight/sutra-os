(ns sutra.health.http
  (:require [sutra.kit.refusal :as refusal]
            [sutra.foundation.ops :as op]
            [sutra.builds.api :as builds]
            [sutra.health.checks :as health]
            [sutra.health.watch :as watch]
            [sutra.http.endpoint :as ep]
            [sutra.http.responses :refer [ok]]
            [sutra.store.control :as ctl]
            [sutra.schedules.api :as schedules]
            [sutra.store.access.scope :as ctx]))

(set! *warn-on-reflection* true)

(def list-health-checks
  (ep/handler :health-checks
              {:call #(watch/annotated (:store %) (:branch %) (:user %))
               :wrap :checks}))

(def watch-health-check
  (ep/handler :health-check-watch
              {:needs-user true
               :respond (fn [{:keys [store branch id body] u :user}]
                          (let [level (str (or (:level body) "all"))]
                            (if (or (= "none" level) (watch/levels level))
                              (ok (watch/watch! store branch id u level))
                              (refusal/refused :refusal.health/level-is-all-critical-or {:level level}))))}))

(defn- snooze-until
  [{:keys [hours until]} now]
  (cond
    until (java.util.Date/from (java.time.Instant/parse (str until)))
    hours (java.util.Date. (+ (.getTime ^java.util.Date now)
                              (long (* 3600000 (double (if (string? hours)
                                                         (parse-double hours)
                                                         hours))))))))

(def snooze-health-check
  (ep/handler :health-check-snooze
              {:needs-user true
               :respond (fn [{:keys [store branch id body] u :user}]
                          (if-let [t (snooze-until body (ctl/clock store))]
                            (ok (watch/snooze! store branch id t (:reason body) u))
                            (refusal/refused :refusal.health/snooze-takes-hours-n-or {:got body})))}))

(def unsnooze-health-check
  (ep/handler :health-check-snooze
              {:call #(watch/unsnooze! (:store %) (:branch %) (:id %))}))

(def list-check-groups
  (ep/handler :health-check-groups
              {:call #(watch/groups (:store %) (:branch %)) :wrap :groups}))

(def save-check-group
  (ep/handler :health-check-groups
              {:needs-user true
               :respond (fn [{:keys [store branch body] u :user}]
                          (if (and (seq (str (:name body))) (sequential? (:check-ids body)))
                            (ok (watch/save-group! store branch body u))
                            (refusal/refused :refusal.health/a-check-group-has-a {:got body})))}))

(def delete-check-group
  (ep/handler :health-check-group-of
              {:call #(watch/delete-group! (:store %) (:branch %) (:id %))}))

(def subscribe-check-group
  (ep/handler :health-check-group-subscribe
              {:needs-user true
               :respond (fn [{:keys [store branch id body] u :user}]
                          (if-let [period (watch/every-ms
                                           (or (:every-ms body) (:every body) "1d"))]
                            (ok (watch/subscribe! store branch id u period (:only-failing body)))
                            (refusal/refused :refusal.health/every-is-15m-1h-daily {:got body})))}))

(def unsubscribe-check-group
  (ep/handler :health-check-group-subscribe
              {:needs-user true
               :respond (fn [{:keys [store branch id] u :user}]
                          (ok (watch/unsubscribe! store branch id u)))}))

(def upsert-health-check
  (ep/handler :health-checks
              {:call (fn [{:keys [store branch body]}]
                       (health/upsert! store branch (if (and (not (:node-kind body)) (:target body))
                                                      (health/rider->check body)
                                                      body)))}))

(def delete-health-check
  (ep/handler :health-check-of {:call #(health/delete! (:store %) (:branch %) (:id %))}))

(def run-health-check
  (ep/handler :health-check-run
              {:call #(health/run-check! (:store %) (:schema %) (:branch %) (:id %))}))

(def health-check-results
  (ep/handler :health-check-results
              {:call #(health/results (:store %) (:branch %) (:id %)) :wrap :results}))

(def run-all-health-checks
  (ep/handler :health-run-all
              {:call #(health/run-all! (:store %) (:schema %) (:branch %)) :wrap :results}))

(def health-overview
  (ep/handler :health-overview
              {:call (fn [{:keys [store schema branch]}]
                       (assoc (health/operator-summary
                               {:results       (health/run-all! store schema branch)
                                :checks        (health/list-all store branch)
                                :active-builds (builds/active-builds store)
                                :last-run
                                (first (builds/list-recent (ctx/at store branch) 1 nil))
                                :schedules     (schedules/enriched-schedules store schema branch)})
                              :node (builds/status)))}))

(op/defop :health/by-node [{:keys [store]} {:keys [branch]}]
  (health/summary-by-node store branch))

(op/defop :health/record-expectation [{:keys [store]} in]
  {:tx  {:records (health/expectation-records store in)}
   :out nil})

(def module
  {:name        :health-http
   :provides    {:health/result-recorded watch/result-recorded!}
   :jobs        [{:name :health-digests :every "SUTRA_HEALTH_DIGEST_MS"
                  :tick watch/send-digests! :default 60000}]
   :contributes {:repos/riders
                 {:health-checks
                  {:upsert! (fn [store branch r]
                              (health/upsert! store branch
                                              (if (and (not (:node-kind r)) (:target r))
                                                (health/rider->check r)
                                                r)))
                   :delete! health/delete!}}
                 :http/routes
                 {:health-check-of      {:delete delete-health-check}
                  :health-check-watch   {:post watch-health-check}
                  :health-check-snooze  {:post snooze-health-check :delete unsnooze-health-check}
                  :health-check-groups  {:get list-check-groups :post save-check-group}
                  :health-check-group-of {:delete delete-check-group}
                  :health-check-group-subscribe {:post   subscribe-check-group
                                                 :delete unsubscribe-check-group}
                  :health-check-results {:get health-check-results}
                  :health-check-run     {:post run-health-check}
                  :health-checks        {:get list-health-checks :post upsert-health-check}
                  :health-overview      {:get health-overview}
                  :health-run-all       {:post run-all-health-checks}}}})
