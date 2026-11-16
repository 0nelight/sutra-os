(ns ^:integration ontology.scheduler
  (:require [sutra.store.access.scope :as ctx]
            [sutra.store.seat :as seat]
            [sutra.builds.plan :as plan]
            [sutra.schedules.schedules :as schedules]
            [support.helpers :refer [check section]]))

(def ^:private scope-schema
  {:object-types {:email {} :host {}}
   :transforms
   {:email-rows {:language :python :output {:kind :object-type :object-type :email} :inputs {}}
    :host-rows  {:language :pg-sql :output {:kind :object-type :object-type :host}
                 :inputs {:in {:kind :transform :transform :host-prep}}}
    :host-prep  {:language :pg-sql :output-columns [:a]
                 :inputs {:in {:kind :transform :transform :host-raw}}}
    :host-raw   {:language :pg-sql :output-columns [:b]
                 :inputs {:in {:kind :datasource :datasource :glpi}}}}})

(defn- test-scoping []
  (section "target-scoped build closure (importer)")
  (let [needed (fn [only] (get-in (plan/scoped-import-plan scope-schema {} only)
                               [:node-scope :materializations]))
        scope  #'plan/scope-import]
    (check "external backing transform needs NO intermediates (email → ∅)"
           (= #{} (needed [:email])))
    (check "multi-hop target pulls its full upstream closure (host → prep+raw)"
           (= #{:host-prep :host-raw} (needed [:host])))
    (check "scope-import keeps the target object-type + drops unrelated links"
           (let [s (scope {:objects {:email :E :host :H}
                           :links {:l {:from-type :host :to-type :host}}} #{:email})]
             (and (= [:email] (keys (:objects s))) (= {} (:links s)))))
    (check "scope-import keeps a link TOUCHING the target (email→carrier)"
           (let [s (scope {:objects {:email :E}
                           :links {:email-von {:from-type :email :to-type :carrier}}}
                          #{:carrier})]
             (= [:email-von] (keys (:links s)))))))

(defn run-tests []
  (test-scoping)
  (section "build-schedule time-trigger (sutra.schedules.dispatch)")
  (if-let [branch (seat/seated-branch)]
    (let [made (atom [])
          mk   (fn [m]
                 (let [s (schedules/upsert! (ctx/store) branch m)] (swap! made conj (:id s)) s))]
      (try
        (let [due-short  (mk {:name "test-due-short"  :target "t" :every-ms 1       :paused false})
              not-due    (mk {:name "test-not-due"    :target "t" :every-ms 3600000 :paused false})
              paused     (mk {:name "test-paused"     :target "t" :every-ms 1       :paused true})
              cron-only  (mk {:name "test-cron-only"  :target "t" :cron "0 * * * *" :every-ms nil
                              :paused false})
              later      (assoc (ctx/store)
                                :clock #(java.util.Date. (+ (System/currentTimeMillis) 1000)))
              due-ids    (set (map :id (schedules/list-due later branch)))]
          (check "elapsed cadence → due"            (contains? due-ids (:id due-short)))
          (check "fresh long cadence → NOT due"     (not (contains? due-ids (:id not-due))))
          (check "paused → never due"               (not (contains? due-ids (:id paused))))
          (check "cron-only (no every-ms) → skipped" (not (contains? due-ids (:id cron-only))))
          (let [reset-s (mk {:name "test-reset" :target "t" :every-ms 3600000 :paused false})]
            (schedules/mark-fired! (ctx/store) branch (:id reset-s) nil)
            (check "just-fired long cadence → NOT due"
                   (not (contains? (set (map :id (schedules/list-due (ctx/store) branch)))
                                   (:id reset-s))))))
        (finally
          (doseq [id @made]
            (try (schedules/delete! (ctx/store) branch id) (catch Throwable _ nil))))))
    (check "seated branch handle present (skipped — none)" true))
  (println "\n  scheduler: all checks passed"))
