(ns sutra.health.watch
  (:require [sutra.kit.coll :as coll]
            [clojure.string :as str]
            [sutra.health.checks :as checks]
            [sutra.notifications.api :as notifications]
            [sutra.store.control :as ctl]
            [sutra.types.opid :as opid]))

(set! *warn-on-reflection* true)

(def health-url "/operations")

(def levels #{"all" "critical"})

(defn- sev [s] (some-> s (cond-> (keyword? s) name) str str/lower-case))

(defn level-fits?
  [level severity]
  (case (str level)
    "all"      true
    "critical" (contains? #{"critical" "high"} (sev severity))
    false))

(defn- ms [d] (some-> ^java.util.Date d .getTime))

(defn snoozed?
  [snooze now]
  (boolean (some-> (ms (:until snooze)) (> now))))

(defn turned-failed?
  [previous result]
  (and (= "failed" (:status result)) (not= "failed" (:status previous))))

(defn to-tell
  [check previous result watches snooze now]
  (if (and (turned-failed? previous result) (not (snoozed? snooze now)))
    (into [] (comp (filter #(level-fits? (:level %) (get-in check [:expects :severity])))
                   (map :username) (distinct))
          watches)
    []))

(defn alert
  [check result]
  {:source     :health
   :heading    (str "Health Check fehlgeschlagen: " (or (:name check) (:check-id result)))
   :message    (str/join "\n"
                         (remove str/blank?
                                 [(str (:node-kind check) " " (:node-key check)
                                       " · Schwere " (or (sev (get-in check [:expects :severity]))
                                                         "moderate")
                                       (when (seq (str (:value result)))
                                         (str " · Wert " (:value result))))
                                  (str (:detail result))]))
   :link-url   health-url
   :link-label "Data Health öffnen"})

(def ^:private units {"m" 60000 "h" 3600000 "d" 86400000 "w" 604800000})

(defn every-ms
  [every]
  (cond
    (number? every)        (long every)
    (= "daily" every)      (get units "d")
    (= "weekly" every)     (get units "w")
    (= "hourly" every)     (get units "h")
    :else (when-let [[_ n u] (re-matches #"(\d+)\s*([mhdw])" (str/trim (str every)))]
            (* (parse-long n) (get units u)))))

(defn due?
  [{:keys [every-ms last-sent-at]} now]
  (or (nil? last-sent-at) (>= (- now (ms last-sent-at)) every-ms)))

(defn- in-group? [ids c] (or (contains? ids (:id c)) (contains? ids (:name c))))

(defn digest
  [{:keys [name check-ids]} checks snoozes only-failing?]
  (let [mine    (filterv (partial in-group? (set (map str check-ids))) checks)
        failing (filterv #(= "failed" (get-in % [:latest :status])) mine)]
    (when-not (and only-failing? (empty? failing))
      {:source     :health
       :heading    (str "Check-Gruppe " name ": " (count failing) " von " (count mine)
                        " fehlgeschlagen")
       :message    (if (empty? failing)
                     "Alle Checks bestehen."
                     (str/join "\n" (for [c failing]
                                      (str "• " (:name c)
                                           (when (contains? snoozes (:id c)) " (snoozed)")
                                           (when-let [d (not-empty (str (get-in c [:latest
                                                                                   :detail])))]
                                             (str " — " d))))))
       :link-url   health-url
       :link-label "Data Health öffnen"})))

(defn annotate
  [rows watches snoozes username now]
  (let [mine (into {} (for [w watches :when (= username (:username w))]
                        [(:check-id w) (:level w)]))
        snz  (into {} (for [s snoozes :when (snoozed? s now)] [(:check-id s) s]))
        iso  #(some-> % ms java.time.Instant/ofEpochMilli str)]
    (mapv (fn [r]
            (cond-> r
              (mine (:id r)) (assoc :watching (mine (:id r)))
              (snz (:id r))  (assoc :snoozed (-> (snz (:id r))
                                                 (select-keys [:until :reason :snoozed-by])
                                                 (update :until iso)))))
          rows)))

(defn watch!
  [store branch check-id username level]
  (let [k {:check-id (str check-id) :branch branch :username (str username)}]
    (if (= "none" (str level))
      (do (ctl/commit! store [[:delete :health-watches k]]) {:watching nil})
      (do (ctl/commit! store [[:upsert :health-watches [(assoc k :level (str level))]]])
          {:watching (str level)}))))

(defn snooze!
  [store branch check-id until reason by]
  (ctl/commit! store [[:upsert :health-snoozes
                       [{:check-id (str check-id) :branch branch :until until
                         :reason reason :snoozed-by by}]]])
  {:check-id check-id :snoozed-until (str (.toInstant ^java.util.Date until))})

(defn unsnooze!
  [store branch check-id]
  (ctl/commit! store [[:delete :health-snoozes {:check-id (str check-id) :branch branch}]])
  {:check-id check-id :snoozed nil})

(defn annotated
  [store branch username]
  (annotate (checks/list-all store branch)
            (ctl/list store :health-watches {:branch branch})
            (ctl/list store :health-snoozes {:branch branch})
            (str username) (ctl/millis store)))

(defn result-recorded!
  [store branch check-id]
  (try
    (let [[result previous] (sort-by (juxt #(ms (:run-at %)) :seq) #(compare %2 %1)
                                     (checks/results store branch check-id))
          check (ctl/get store :health-checks {:branch branch :id check-id})
          us    (to-tell check previous result
                         (ctl/list store :health-watches {:branch branch :check-id check-id})
                         (ctl/get store :health-snoozes {:branch branch :check-id check-id})
                         (ctl/millis store))]
      (when (seq us)
        (notifications/send! store branch us (alert check result))))
    (catch InterruptedException e (throw e))
    (catch Exception t
      (println "[health] telling the watchers of" check-id "failed:" (ex-message t)))))

(defn groups [store branch]
  (mapv (fn [g] (assoc g :subscribers
                       (mapv #(select-keys % [:username :every-ms :only-failing :last-sent-at])
                             (ctl/list store :health-group-subscriptions
                                       {:branch branch :group-id (:id g)}))))
        (ctl/list store :health-check-groups {:branch branch})))

(defn save-group!
  [store branch {:keys [id name description check-ids]} by]
  (let [id (or id (opid/name-id :health-check-group name))]
    (ctl/commit! store [[:upsert :health-check-groups
                         [{:id id :branch branch :name name :description description
                           :check-ids (vec (map str check-ids)) :created-by by}]]])
    (ctl/get store :health-check-groups {:id id :branch branch})))

(defn delete-group!
  [store branch id]
  (ctl/commit! store [[:delete :health-group-subscriptions {:branch branch :group-id id}]
                      [:delete :health-check-groups {:branch branch :id id}]])
  {:deleted id})

(defn subscribe!
  [store branch group-id username period only-failing?]
  (ctl/commit! store [[:upsert :health-group-subscriptions
                       [{:group-id group-id :branch branch :username (str username)
                         :every-ms period
                         :only-failing (if (nil? only-failing?) true (boolean only-failing?))}]]])
  {:group-id group-id :every-ms period})

(defn unsubscribe!
  [store branch group-id username]
  (ctl/commit! store [[:delete :health-group-subscriptions
                       {:group-id group-id :branch branch :username (str username)}]])
  {:group-id group-id :subscribed false})

(defn- digest-branch!
  [store branch subs now at]
  (let [checks  (checks/list-all store branch)
        snoozes (into {} (for [s (ctl/list store :health-snoozes {:branch branch})
                               :when (snoozed? s now)]
                           [(:check-id s) s]))
        gs      (coll/index-by :id (ctl/list store :health-check-groups {:branch branch}))]
    (doseq [{:keys [group-id username only-failing] :as sub} subs
            :when (due? sub now)
            :let [g (gs group-id)]
            :when g]
      (when-let [n (digest g checks snoozes only-failing)]
        (notifications/send! store branch [username] n))
      (ctl/commit! store [[:update :health-group-subscriptions
                           (select-keys sub [:group-id :branch :username])
                           {:last-sent-at at}]]))))

(defn send-digests!
  [store]
  (let [at  (ctl/clock store)
        now (.getTime at)]
    (doseq [[branch subs] (group-by :branch (ctl/list store :health-group-subscriptions {}))]
      (try (digest-branch! store branch subs now at)
           (catch InterruptedException e (throw e))
           (catch Exception t
             (println "[health] digest on" branch "failed:" (ex-message t)))))))
