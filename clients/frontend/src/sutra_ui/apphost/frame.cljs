(ns sutra-ui.apphost.frame
  (:require [reagent.core :as r]
            [reagent.ratom :as ratom]
            [sutra.kit.remote :as rd]
            [sutra-ui.apphost.client :as client]
            [sutra-ui.apphost.toast :refer [toast!]]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.requests :as rq]))

(defn report!
  [prefix sentence]
  (toast! (str prefix sentence) {:error? true}))

(defn capp-frame
  ([] (capp-frame {}))
  ([db] (machine/frame (r/atom (assoc db ::rq/reporter report!))
                       {:name :capp :lifetime :view :rt (atom {})})))

(defn frame-for
  [env]
  (:frame env))

(defn send!
  [f event]
  (machine/dispatch event {:frame f}))

(defn- put [db path v] (if (seq path) (assoc-in db path v) v))

(machine/reg-sub :capp/at (fn [db [_ path]] (get-in db path)))

(machine/reg-event :capp/put (fn [{:keys [db]} [_ path v]] {:db (put db path v)}))

(machine/reg-event :capp/update
                   (fn [{:keys [db]} [_ path g args]]
                     {:db (put db path (apply g (get-in db path) args))}))

(deftype DbCursor [f path]
  ratom/IReactiveAtom
  IDeref
  (-deref [_] (if (ratom/reactive?)
                @(machine/subscribe [:capp/at path] {:frame f})
                (get-in @(:db f) path)))
  IReset
  (-reset! [_ v] (send! f [:capp/put path v]) v)
  ISwap
  (-swap! [_ g] (send! f [:capp/update path g []]) (get-in @(:db f) path))
  (-swap! [_ g x] (send! f [:capp/update path g [x]]) (get-in @(:db f) path))
  (-swap! [_ g x y] (send! f [:capp/update path g [x y]]) (get-in @(:db f) path))
  (-swap! [_ g x y more]
    (send! f [:capp/update path g (into [x y] more)])
    (get-in @(:db f) path))
  IWatchable
  (-add-watch [this key g]
    (add-watch (:db f) [::cursor path key]
               (fn [_ _ old new]
                 (let [o (get-in old path) n (get-in new path)]
                   (when-not (identical? o n) (g key this o n)))))
    this)
  (-remove-watch [this key]
    (remove-watch (:db f) [::cursor path key])
    this)
  (-notify-watches [this _ _] this))

(defn cursor
  [f path]
  (DbCursor. f path))

(defn request!
  [f req]
  (send! f [:rf/request req]))

(defn kept
  ([on-answer] (kept on-answer nil))
  ([on-answer on-failure]
   (cond-> {:then [[:rf/invoke on-answer]]}
     on-failure (assoc :else [[:rf/invoke on-failure]]))))

(machine/reg-event :capp/deed
                   (fn [{:keys [db]} [_ k req]]
                     (rq/request (assoc-in db [:deeds k] {:busy? false :error nil})
                                 (assoc req :busy [:deeds k :busy?] :error [:deeds k :error]))))

(machine/reg-event :capp/drop-deed
                   (fn [{:keys [db]} [_ k]] {:db (update db :deeds dissoc k)}))

(defn deed!
  [f k req]
  (send! f [:capp/deed k req]))

(defn deed
  [f k]
  (or @(machine/subscribe [:capp/at [:deeds k]] {:frame f}) {}))

(defn submit-request
  [descriptor params {:keys [action scenario on-ok on-error on-stale]}]
  (let [opts {:action action :scenario scenario}]
    (cond-> {:ask  (if on-stale
                     [client/submit! descriptor params opts]
                     [client/submit-strict! descriptor params opts])
             :then [(if on-stale
                      (fn [{:keys [data stale]}]
                        (if stale
                          [:rf/invoke on-stale stale]
                          [:rf/invoke (or on-ok identity) data]))
                      [:rf/invoke (or on-ok identity)])]}
      on-error (assoc :else [[:rf/invoke on-error]]))))

(defn- stop! [{:keys [rt]} k]
  (when-let [s (get-in @rt [:stops k])]
    (swap! rt update :stops dissoc k)
    (s)))

(defn- keep-stop! [{:keys [rt]} k s]
  (swap! rt assoc-in [:stops k] s))

(machine/reg-fx :capp/watch
                (fn [{f :frame} {:keys [key descriptor spec on]}]
                  (stop! f key)
                  (let [reply (machine/answering f)]
                    (keep-stop! f key
                                (client/watch! descriptor
                                               (assoc spec :on-change
                                                      #(reply (conj on %))))))))

(machine/reg-fx :capp/listen
                (fn [{f :frame} {:keys [key event on target]}]
                  (stop! f key)
                  (when (exists? js/window)
                    (let [t     (if (= :document target) js/document js/window)
                          reply (machine/answering f)
                          h     (fn [_] (reply on))]
                      (.addEventListener t event h)
                      (keep-stop! f key #(.removeEventListener t event h))))))

(machine/reg-fx :capp/unwatch (fn [{f :frame} k] (stop! f k)))

(defn stop-all!
  [{:keys [rt]}]
  (let [stops (vals (:stops @rt))]
    (swap! rt dissoc :stops)
    (doseq [s stops] (s))))

(machine/reg-fx :capp/stop-all (fn [{f :frame} _] (stop-all! f)))

(machine/reg-event :capp/left (fn [_ _] {:fx [[:capp/stop-all nil]]}))

(defn leave!
  [f]
  (send! f [:capp/left])
  (machine/leave! f))

(def leaving
  (r/create-class
   {:display-name           "capp-leaving"
    :component-will-unmount (fn [this] (leave! (second (r/argv this))))
    :reagent-render         (fn [_] nil)}))

(defn- read-request [db slot]
  (let [n                  (inc (::asked db 0))
        {:keys [ask opts]} (get-in db [:reads slot])
        p                  [:reads slot]]
    (rq/request
     (-> db
         (assoc ::asked n)
         (update-in p assoc :n n :error nil))
     {:ask     (cond-> ask opts (conj opts))
      :path    (conj p :rd)
      :rd?     true
      :error   (conj p :error)
      :busy    [(conj p :asking) n]
      :current [(conj p :n) n]
      :within  p})))

(machine/reg-event :capp/read
                   (fn [{:keys [db]} [_ slot key ask opts]]
                     (let [s (get-in db [:reads slot])]
                       (when-not (and s (= key (:key s)))
                         (read-request (assoc-in db [:reads slot]
                                                 {:key key :ask ask :opts opts
                                                  :rd (or (:rd s) rd/loading)})
                                       slot)))))

(machine/reg-event :capp/reread
                   (fn [{:keys [db]} [_ slot]]
                     (when (get-in db [:reads slot]) (read-request db slot))))

(machine/reg-event :capp/drop-read
                   (fn [{:keys [db]} [_ slot]] {:db (update db :reads dissoc slot)}))

(defn slot-view
  [{:keys [rd error asking]}]
  {:data (when (some? rd) (rd/value-or rd nil)) :error error :loading? (some? asking)})

(defn read!
  ([env slot key ask] (read! env slot key ask nil))
  ([env slot key ask opts]
   (let [f   (frame-for env)
         sub (machine/subscribe [:capp/at [:reads slot]] {:frame f})]
     (let [s @sub]
       (when-not (and s (= key (:key s))) (send! f [:capp/read slot key ask opts])))
     (slot-view @sub))))
