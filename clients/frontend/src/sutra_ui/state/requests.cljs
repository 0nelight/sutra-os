(ns sutra-ui.state.requests
  (:require [sutra.kit.remote :as rd]
            [sutra-ui.debug :as debug]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.shell :as shell]))

(defn settle
  [reply v]
  (when reply [:invoke [(:resolve reply) v]]))

(defn refuse
  [reply sentence]
  (when reply
    (if sentence
      [:invoke [(:reject reply) (js/Error. sentence)]]
      (settle reply nil))))

(defn failure
  [prefix sentence]
  (when sentence [:invoke [shell/fail! prefix sentence]]))

(defn- busy-at [busy] (if (vector? (first busy)) busy [busy true]))

(defn request
  ([db req] (request db req nil))
  ([db {:keys [ask busy dedup] :as req} reply]
   (if (and dedup (contains? (::in-flight db) dedup))
     {:db (update-in db [::in-flight dedup] conj
                     {:reply reply :then (:then req) :else (:else req)})}
     {:db (cond-> db
            dedup (assoc-in [::in-flight dedup] [{:reply reply}])
            busy  (assoc-in (first (busy-at busy)) (second (busy-at busy))))
      :fx [[:call {:call ask :ok [:rf/answered req reply] :err [:rf/failed req reply]}]]})))

(defn requests
  [db reqs]
  (reduce (fn [{:keys [db fx]} req]
            (update (request db req) :fx #(into (vec fx) %)))
          {:db db :fx []} reqs))

(defn ask
  ([req] (ask req nil))
  ([req reply] [:dispatch [:rf/request req reply]]))

(defn- unbusy [db busy]
  (let [[p v]  (busy-at busy)
        parent (butlast p)]
    (cond
      (and parent (nil? (get-in db parent))) db
      (not (vector? (first busy)))           (assoc-in db p false)
      (not= v (get-in db p))                 db
      parent                                 (update-in db parent dissoc (last p))
      :else                                  (dissoc db (last p)))))

(defn- landed
  [db {:keys [dedup busy]} reply]
  (let [db (cond-> db busy (unbusy busy))]
    (if dedup
      (let [waiting (get-in db [::in-flight dedup])]
        [(update db ::in-flight dissoc dedup) (map :reply waiting) (rest waiting)])
      [db [reply] nil])))

(defn- current? [db {[p v] :current :as req}]
  (or (not (contains? req :current)) (= v (get-in db p))))

(defn- open? [db {:keys [within]}] (or (nil? within) (some? (get-in db within))))

(defn- answer-at [db {:keys [path pick rd?] :as req} answer]
  (let [v ((or pick identity) answer)]
    (cond-> db (and path (open? db req)) (assoc-in path (if rd? (rd/of-answer v) v)))))

(defn- each [evs & xs]
  (keep (fn [e] (when-let [e (if (fn? e) (apply e xs) (into e xs))] [:dispatch e])) evs))

(defn- always [{evs :always}] (map (fn [e] [:dispatch e]) evs))

(machine/reg-event :rf/request (fn [{:keys [db]} [_ req reply]] (request db req reply)))

(machine/reg-event :rf/answered
                   (fn [{:keys [db]} [_ {:keys [then fx handover?] :as req} reply answer]]
                     (let [[db replies joined] (landed db req reply)
                           settled      (keep #(settle % answer) replies)
                           joined-then  (mapcat #(each (:then %) answer) joined)]
                       (cond
                         (not (current? db req))
                         {:db db :fx (-> (vec (always req)) (into settled))}
                         handover?
                         {:db (answer-at db req answer)
                          :fx (-> (vec (mapcat #(each then answer %) replies))
                                  (into fx) (into (always req)))}
                         :else
                         {:db (answer-at db req answer)
                          :fx (-> (vec (each then answer)) (into joined-then) (into fx)
                                  (into (always req)) (into settled))}))))

(defn- reporter
  [db]
  (or (::reporter db) shell/fail!))

(defn- reported [db {:keys [report error else rd? reject?]} sentence]
  (cond
    (fn? report)                     [:invoke [(reporter db) nil (report sentence)]]
    report                           [:invoke [(reporter db) (when (string? report) report)
                                               sentence]]
    (or error (seq else) rd? reject?) nil
    :else                            [:invoke [debug/record! :error "request" sentence]]))

(machine/reg-event :rf/failed
                   (fn [{:keys [db]} [_ {:keys [path rd? error else reject?] :as req} reply
                                      sentence]]
                     (let [[db replies joined] (landed db req reply)
                           live?        (and sentence (current? db req))
                           write?       (and live? (open? db req))]
                       {:db (cond-> db
                              (and write? rd? path) (update-in path rd/after-failure sentence)
                              (and write? error)    (assoc-in error sentence))
                        :fx (-> (if sentence [(reported db req sentence)] [])
                                (into (when live? (each else sentence)))
                                (into (when sentence (mapcat #(each (:else %) sentence) joined)))
                                (into (always req))
                                (into (map #(if reject? (refuse % sentence) (settle % nil)))
                                      replies)
                                (->> (filterv some?)))})))

(machine/reg-event :rf/set (fn [{:keys [db]} [_ path v]] {:db (assoc-in db path v)}))

(machine/reg-event :rf/invoke (fn [_ [_ & f-args]] {:fx [[:invoke f-args]]}))
