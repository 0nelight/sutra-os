(ns sutra.linter.rules
  (:require [sutra.kit.coll :as coll]
            [sutra.kit.attempt :as attempt]
            [clojure.set :as set]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)

(def ^:private day-ms (* 24 60 60 1000))
(def ^:private window-ms (* 30 day-ms))

(defn- node-name [id] (second (str/split (str id) #"/" 2)))

(defn- sched-res [s] (str "schedule/" (:name s)))

(defn- live [facts] (remove :paused (:schedules facts)))

(defn- by-name [facts] (coll/index-by :transform (:transforms facts)))

(defn- built [facts s] (keep (by-name facts) (:builds s)))

(defn- project-of [facts s]
  (some #(get-in % [:declared :repository]) (built facts s)))

(defn- finding
  ([facts s detail] {:resource (sched-res s) :project (project-of facts s) :detail detail})
  ([t detail] {:resource (str "transform/" (:transform t))
               :project (get-in t [:declared :repository]) :detail detail}))

(defn- runs-of [facts s]
  (let [prefix (str "schedule " (:name s))]
    (filter #(let [o (str (get-in % [:targets :origin]))]
               (or (= o prefix) (str/starts-with? o (str prefix " "))))
            (:runs facts))))

(defn- since-ms [facts at]
  (when at (- (:now facts) (long at))))

(defn- outputs-of [ts] (set (mapcat :outputs ts)))

(defn- inputs-of [ts] (set (mapcat :inputs ts)))

(def ^:private rules*
  [{:id :schedule-always-fails :mode :pipeline-resilience :counterpart "Schedule always fails"
    :why "Each run tries to rebuild what has not succeeded for 30 days: resources spent, and whatever reads it stays behind."
    :recommendation "Pause the schedule, fix the failing transform, or take it out of the schedule."
    :scan (fn [facts]
            (for [s (live facts)
                  :let [rs (runs-of facts s)]
                  :when (and (seq rs)
                             (not-any? #(= "succeeded" (some-> (:status %) name)) rs))]
              (finding facts s (str (count rs) " run(s) in the last 30 days, none succeeded"))))}

   {:id :schedule-builds-unknown-target :mode :pipeline-resilience
    :counterpart "Schedule builds trashed dataset"
    :why "The schedule names what no longer exists, so it cannot build it."
    :recommendation "Restore the resource or update the schedule's targets."
    :scan (fn [facts]
            (for [s (live facts)
                  :let [gone (remove (:known facts) (:targets s))]
                  :when (seq gone)]
              (finding facts s (str "targets not found: " (str/join ", " gone)))))}

   {:id :schedule-no-built-datasets :mode :pipeline-resilience
    :counterpart "Schedule has no built datasets"
    :why "The schedule is potentially a waste of resources and noise for monitoring."
    :recommendation "Decide whether it is useful; delete it or give it a target that builds."
    :scan (fn [facts]
            (for [s (live facts)
                  :when (and (empty? (:builds s)) (every? (:known facts) (:targets s)))]
              (finding facts s "its build resolves to no transform")))}

   {:id :overlapping-schedules :mode :pipeline-resilience :counterpart "Overlapping schedules"
    :why "Two schedules build the same transform: compute waste, queueing, unreliable latency."
    :recommendation "Change the targets of the schedules to eliminate the overlap."
    :scan (fn [facts]
            (for [s (live facts)
                  :let [others (for [o (live facts)
                                     :when (not= (:name o) (:name s))
                                     :let [both (set/intersection (set (:builds s))
                                                                  (set (:builds o)))]
                                     :when (seq both)]
                                 (str (:name o) " (" (count both) ")"))]
                  :when (seq others)]
              (finding facts s (str "shares transforms with " (str/join ", " (sort others))))))}

   {:id :schedule-does-not-abort-on-failure :mode :pipeline-resilience
    :counterpart "Schedule does not abort on failure"
    :why "Monitoring waits for the whole schedule even when a transform has failed."
    :recommendation "Enable --abort-on-failure, so the failure is reported sooner."
    :scan (fn [facts]
            (for [s (live facts) :when (not (:abort-on-failure s))]
              (finding facts s "abort on failure is off")))}

   {:id :schedule-retries-disabled :mode :pipeline-resilience
    :counterpart "Schedule retries disabled"
    :why "Failures of the environment often do not recur; without a retry the output stays unbuilt."
    :recommendation "Enable retries; three attempts are recommended (--retry-count 3)."
    :scan (fn [facts]
            (for [s (live facts) :when (not (pos? (long (or (:retry-count s) 0))))]
              (finding facts s "no retries")))}

   {:id :schedule-on-branch :mode :resource-savings :counterpart "Schedule on branch"
    :why "A schedule left running on a branch keeps computing an alternate version of the data."
    :recommendation "Pause or delete it if it is no longer needed."
    :scan (fn [facts]
            (for [{:keys [name branch]} (:branch-schedules facts)]
              {:resource (str "schedule/" branch "/" name) :detail (str "runs on " branch)}))}

   {:id :schedule-triggers-itself :mode :pipeline-resilience :counterpart "Schedule triggers itself"
    :why "What it builds starts it again."
    :recommendation "Remove the resource from the triggers or from the build."
    :scan (fn [facts]
            (for [s (live facts)
                  :let [outs (set (map node-name (outputs-of (built facts s))))
                        hit  (filter outs (:watched s))]
                  :when (seq hit)]
              (finding facts s (str "builds and watches " (str/join ", " hit)))))}

   {:id :schedule-trigger-is-not-input :mode :pipeline-resilience
    :counterpart "Schedule trigger is not input"
    :why "It runs when something changes that none of its transforms reads."
    :recommendation "Remove the trigger, or add the resource to the inputs."
    :scan (fn [facts]
            (for [s (live facts)
                  :let [ts   (built facts s)
                        ins  (set (map node-name (inputs-of ts)))
                        outs (set (map node-name (outputs-of ts)))
                        hit  (remove #(or (ins %) (outs %)) (:watched s))]
                  :when (and (seq ts) (seq hit))]
              (finding facts s (str "watches " (str/join ", " hit) ", which it does not read"))))}

   {:id :schedule-inputs-unscheduled :mode :pipeline-resilience
    :counterpart "Schedule inputs unscheduled"
    :why "An input has logic but no schedule, so the schedule reads a stale result."
    :recommendation "Schedule its producer, or retire the producer to make the input raw."
    :scan (fn [facts]
            (let [covered  (set (mapcat :builds (live facts)))
                  producer (into {} (for [t (:transforms facts) o (:outputs t)]
                                      [o (:transform t)]))]
              (for [s (live facts)
                    :let [mine (set (:builds s))
                          hit  (sort (distinct
                                      (for [i (inputs-of (built facts s))
                                            :let [p (producer i)]
                                            :when (and p (not (mine p)) (not (covered p)))]
                                        p)))]
                    :when (seq hit)]
                (finding facts s (str "reads the output of " (str/join ", " hit))))))}

   {:id :schedule-potentially-unused :mode :resource-savings
    :counterpart "Schedule potentially unused"
    :why "Nothing reads what it builds, so it spends resources on results nobody uses."
    :recommendation "Pause the schedule, or remove the unused part of it."
    :scan (fn [facts]
            (for [s (live facts)
                  :let [ts   (built facts s)
                        mine (set (map #(str "transform/" %) (:builds s)))
                        read? (fn [t] (or (seq (:read-via-link-ends t))
                                          (some (complement mine) (:readers t))))]
                  :when (and (seq ts) (not-any? read? ts))]
              (finding facts s "no reader beyond its own transforms")))}

   {:id :schedule-unnecessary-force-builds :mode :resource-savings
    :counterpart "Schedule unnecessary force builds"
    :why "A force build runs transforms whose inputs did not change."
    :recommendation "Remove --full unless a transform has an untracked external dependency."
    :scan (fn [facts]
            (for [s (live facts)
                  :let [ts (built facts s)]
                  :when (and (:full s) (seq ts) (not-any? #(seq (:external-systems %)) ts))]
              (finding facts s "forces a full build with no external source")))}

   {:id :dataset-code-out-of-date :mode :pipeline-resilience :counterpart "Dataset code out of date"
    :why "The output does not reflect the latest logic of its transform."
    :recommendation "Build it, or take it out of the pipeline if it should stay as it is."
    :scan (fn [facts]
            (for [t (:transforms facts) :when (= "logic" (:freshness t))]
              (finding t "built with older logic than its transform now has")))}

   {:id :transform-always-fails :mode :pipeline-resilience
    :why "Its last run failed and it has not succeeded for 30 days. Asked of the transform, not of a schedule: a transform without one would never be reported."
    :recommendation "Fix or retire the transform."
    :scan (fn [facts]
            (for [t (:transforms facts)
                  :let [failed (get-in t [:last-failure :at-ms])
                        ok     (get-in t [:last-success :at-ms])]
                  :when (and failed
                             (or (nil? ok) (< ok failed))
                             (or (nil? ok) (> (since-ms facts ok) window-ms)))]
              (finding t (if ok "last success over 30 days ago" "never succeeded"))))}

   {:id :transform-never-built :mode :resource-savings
    :why "It is declared but has never run, so nothing it writes exists. Asked of the transform, not only through a schedule."
    :recommendation "Build it, or retire it."
    :scan (fn [facts]
            (for [t (:transforms facts)
                  :when (and (= "never" (:status t)) (nil? (:last-failure t))
                             (empty? (:triggers t)))]
              (finding t "never ran, and no schedule covers it")))}])

(def rules
  (mapv #(dissoc % :scan) rules*))

(defn- run-rule
  [facts {:keys [id mode scan]}]
  (try {:found (mapv #(assoc % :rule id :mode mode) (scan facts))}
       (catch Exception e
         (attempt/rethrow-fatal! e)
         {:failed [{:rule id :error (ex-message e)}]})))

(defn run
  [facts]
  (apply merge-with into {:found [] :failed []} (map #(run-rule facts %) rules*)))
