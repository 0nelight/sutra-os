(ns sutra-ui.apphost.model
  (:require [reagent.core :as r]
            [sutra.kit.paging :as pg]
            [sutra-ui.apphost.client :as client]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.requests :as rq]))

(defn with-partial
  [old d]
  (let [asked  (map keyword (:_partial d))
        side   (fn [k] (not-empty (merge (apply dissoc (get old k) asked) (get d k))))
        merged (merge (apply dissoc old :_truncated :_refused asked)
                      (dissoc d :_partial :_truncated :_refused))]
    (cond-> merged
      (side :_truncated) (assoc :_truncated (side :_truncated))
      (side :_refused)   (assoc :_refused (side :_refused)))))

(defn- keep-stop! [{:keys [rt]} k stop]
  (when-let [s (get-in @rt [:stops k])] (s))
  (swap! rt assoc-in [:stops k] stop))

(machine/reg-fx :capp/watch-var
                (fn [{f :frame} {:keys [key handle on now]}]
                  (let [reply (machine/answering f)]
                    (add-watch handle key
                               (fn [_ _ o n]
                                 (when (not= o n) (reply (conj on o n (when now (now)))))))
                    (keep-stop! f key #(remove-watch handle key)))))

(defn track!
  [f k thunk]
  (let [t (r/track! thunk)] (keep-stop! f k #(r/dispose! t))))

(machine/reg-fx :capp/stop-scope
                (fn [{{:keys [rt]} :frame} sid]
                  (doseq [[k s] (:stops @rt) :when (and (vector? k) (= sid (first k)))]
                    (swap! rt update :stops dissoc k)
                    (s))))

(defn- watching [sid handles names now]
  (for [nm (distinct names) :let [h (get handles nm)] :when (satisfies? IWatchable h)]
    [:capp/watch-var {:key [sid :var nm] :handle h :on [:capp/set-var sid nm] :now now}]))

(defn- fetch [db {:keys [teil? still?]} {:keys [vars scenario]}]
  (let [n (inc (:asked db 0))]
    (rq/request (assoc db :asked n)
                (cond-> {:ask     (conj (:door db) {:vars     vars
                                                    :held     (when teil? (:held db))
                                                    :scenario scenario})
                         :current [[:asked] n]
                         :then    [[:capp/answered vars]]}
                  (not still?) (assoc :busy [[:asking] n])))))

(defn- branch [db sid sc]
  (let [{:keys [door into]} (get-in db [:scopes sid :scenario])]
    (rq/ask {:ask (conj door {:scenario sc}) :then [[:capp/branch-data into]]})))

(machine/reg-sub :capp/pending? (fn [db _] (some? (:asking db))))

(machine/reg-event
 :capp/start
 (fn [{:keys [db]} [_ handles now asked]]
   (let [watch (watching :root handles
                         (concat (:inputs db) (:module-vars db)
                                 (some-> (get-in db [:scopes :root :scenario :var]) list))
                         now)]
     (if asked
       (update (fetch db {:teil? true} asked) :fx into watch)
       {:fx (vec watch)}))))

(machine/reg-event :capp/fetch (fn [{:keys [db]} [_ opts now]] (fetch db opts now)))

(machine/reg-event
 :capp/answered
 (fn [{:keys [db]} [_ vs d]]
   (when d
     {:db (-> db
              (assoc :held vs)
              (update :data #(if (:_partial d) (with-partial % d) d)))})))

(machine/reg-event
 :capp/more
 (fn [{:keys [db]} [_ nm offset now]]
   (rq/request db {:ask     (conj (:door db) {:vars     (or (:held db) (:vars now))
                                              :scenario (:scenario now)
                                              :more     {nm offset}})
                   :dedup   [:capp/more nm offset]
                   :current [[:asked] (:asked db)]
                   :then    [[:capp/more-rows nm offset]]})))

(defn set-page
  [data nm]
  (pg/page {:rows (get data nm) :offset 0 :total (get-in data [:_truncated nm])}))

(defn- with-page [data nm {:keys [rows total next-offset]}]
  (cond-> (assoc data nm rows)
    next-offset       (assoc-in [:_truncated nm] total)
    (nil? next-offset) (update :_truncated dissoc nm)))

(machine/reg-event
 :capp/more-rows
 (fn [{:keys [db]} [_ nm offset d]]
   (when (and d (vector? (get-in db [:data nm])))
     (let [held (set-page (:data db) nm)
           rows (get d nm)
           more {:rows rows :offset offset
                 :total (get-in d [:_truncated nm] (+ offset (count rows)))}]
       (when (pg/continues? held more)
         {:db (update db :data with-page nm (pg/append held more))})))))

(machine/reg-event :capp/branch-data
                   (fn [{:keys [db]} [_ into d]]
                     (when d {:db (cond-> (assoc-in db into d)
                                    (= [:data] into) (assoc :held nil))})))

(machine/reg-event
 :capp/set-var
 (fn [{:keys [db]} [_ sid nm _old new now]]
   (let [root?  (= :root sid)
         input? (and root? (some #{nm} (:inputs db)))
         emit   (and root? (contains? (:module-vars db) nm) (:emit db))]
     {:db (cond-> db input? (assoc :asking :input))
      :fx [(when input?
             [:after {:key [:input] :ms 250 :event [:capp/fetch {:teil? true} now]}])
           (when (= nm (get-in db [:scopes sid :scenario :var])) (branch db sid new))
           (when (and emit (not= new (get (:handed-in db) nm))) [:invoke [emit nm new]])]})))

(machine/reg-event
 :capp/scope
 (fn [{:keys [db]} [_ sid scenario handles]]
   {:db (cond-> db scenario (assoc-in [:scopes sid :scenario] scenario))
    :fx (vec (when scenario (watching sid handles [(:var scenario)] nil)))}))

(machine/reg-event :capp/drop-scope
                   (fn [{:keys [db]} [_ sid]]
                     {:db (update db :scopes dissoc sid) :fx [[:capp/stop-scope sid]]}))

(machine/reg-event
 :capp/reload
 (fn [{:keys [db]} [_ now]]
   (let [db (update db :generation inc)]
     (if-let [reload (:reload db)]
       (rq/request db {:ask reload :then [[:capp/branch-data [:data]]]})
       (fetch db {:still? true} now)))))

(machine/reg-event
 :capp/refresh
 (fn [{:keys [db]} [_ d now]]
   (let [reload (:reload db)
         db     (update db :generation inc)]
     (cond
       reload                       (rq/request db {:ask  reload
                                                    :then [[:capp/branch-data [:data]]]})
       (nil? d)                     nil
       (:app/whole-page (meta d))   {:db (assoc db :held (:vars now) :data d)}
       :else                        (fetch (update db :data merge d) {} now)))))

(machine/reg-event :capp/payload
                   (fn [{:keys [db]} [_ d]] {:db (assoc db :given d :data d :held nil)}))

(machine/reg-event
 :capp/module
 (fn [{:keys [db]} [_ herein emit handles]]
   {:db (assoc db :handed-in herein :emit emit)
    :fx (when (not= herein (:handed-in db))
          (vec (for [[nm v] herein
                     :let  [h (get handles nm)]
                     :when (and h (contains? (:module-vars db) nm))]
                 [:invoke [reset! h v]])))}))

(machine/reg-event
 :capp/compute
 (fn [{:keys [db]} [_ sid nm req]]
   (let [p [:scopes sid :turn nm]
         n (inc (get-in db p 0))]
     (rq/request (assoc-in db p n)
                 (assoc req
                        :current [p n]
                        :then [[:capp/computed sid nm false]]
                        :else [[:capp/computed sid nm true]])))))

(machine/reg-event
 :capp/computed
 (fn [{:keys [db]} [_ sid nm error? v]]
   {:db (assoc-in db [:scopes sid :computed nm]
                  (if error? {:loading? false :error v} (assoc v :loading? false)))}))

(machine/reg-event :capp/live-changed
                   (fn [_ _] {:fx [[:invoke [client/refresh-page!]]]}))
