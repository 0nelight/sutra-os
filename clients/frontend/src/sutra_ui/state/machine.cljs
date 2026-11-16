(ns sutra-ui.state.machine
  (:require [sutra-ui.ui.overlay :as overlay]
            [reagent.core :as r]
            [sutra-ui.debug :as debug]
            [sutra-ui.state.loading :as loading]))

(defonce ^:private event-handlers (atom {}))

(defonce ^:private fx-handlers (atom {}))

(defonce ^:private sub-handlers (atom {}))

(defn reg-event
  ([id handler] (reg-event id nil handler))
  ([id _meta handler]
   (swap! event-handlers assoc id handler)
   nil))

(defn reg-fx
  [kind f]
  (swap! fx-handlers assoc kind f)
  nil)

(defn reg-sub
  ([id computation] (reg-sub id nil computation))
  ([id meta computation]
   (swap! sub-handlers assoc id {:inputs (:inputs meta) :f computation})
   nil))

(defn frame
  [db opts]
  (merge {:lifetime :app}
         opts
         {:db db :timers (atom {}) :queue (atom nil) :generation (atom 0)}))

(defn effects
  [db [id :as event]]
  (if-let [h (get @event-handlers id)]
    (h {:db db :event event} event)
    (throw (ex-info (str "no event " id) {:id id}))))

(defn fx!
  [ctx [kind arg :as fx]]
  (if-let [f (get @fx-handlers kind)]
    (f ctx arg)
    (throw (ex-info (str "no effect " kind) {:fx fx}))))

(defn- trace!
  [f event fx t0]
  (debug/record! :state (str (or (:name f) "frame"))
                 (str (first event) " " (into [] (comp (remove nil?) (map first)) fx) " "
                      (.toFixed (- (debug/now) t0) 1) "ms")))

(defn- handle!
  [{:keys [db] :as f} event]
  (let [t0  (debug/now)
        s   @db
        eff (effects s event)
        ctx {:frame f :event event}]
    (when (contains? eff :db)
      (let [s' (or (:db eff) {})]
        (when-not (identical? s s') (reset! db s'))))
    (doseq [x (:fx eff) :when x] (fx! ctx x))
    (trace! f event (:fx eff) t0)))

(defn dispatch
  [event {f :frame}]
  (when-not f (throw (ex-info (str "no frame for " (first event)) {:id (first event)})))
  (let [queue (:queue f)]
    (if @queue
      (swap! queue conj event)
      (do (reset! queue (conj #queue [] event))
          (try
            (loop []
              (when-let [e (peek @queue)]
                (handle! f e)
                (swap! queue pop)
                (recur)))
            (finally (reset! queue nil)))))))

(defn answering
  [{:keys [generation] :as f}]
  (let [g @generation]
    (fn [event] (when (and event (= g @generation)) (dispatch event {:frame f})))))

(defn leave!
  [{:keys [lifetime generation timers]}]
  (when (= :view lifetime)
    (swap! generation inc)
    (doseq [t (vals @timers)] (js/clearTimeout t))
    (reset! timers {})))

(defn- cancel! [{f :frame} key]
  (js/clearTimeout (get @(:timers f) key))
  (swap! (:timers f) dissoc key))

(reg-fx :call
        (fn [{f :frame} {:keys [call ok err]}]
          (let [reply (answering f)]
            (.then (apply (first call) (rest call))
                   #(when ok (reply (conj ok %)))
                   (fn [e] (if err (reply (conj err (loading/problem e))) (throw e)))))))

(reg-fx :dispatch (fn [{f :frame} event] (dispatch event {:frame f})))

(reg-fx :invoke (fn [_ [f & args]] (when f (apply f args))))

(reg-fx :after
        (fn [{f :frame :as ctx} {:keys [key ms event]}]
          (cancel! ctx key)
          (let [reply (answering f)]
            (swap! (:timers f) assoc key
                   (js/setTimeout #(do (swap! (:timers f) dissoc key) (reply event)) ms)))))

(reg-fx :cancel cancel!)

(reg-fx :confirm
        (fn [{f :frame} {:keys [ask ok]}]
          (let [reply (answering f)]
            (.then (overlay/confirm! ask) #(when % (reply ok))))))

(declare tracked)

(defn- computed
  [db [id :as query]]
  (if-let [{:keys [inputs f]} (get @sub-handlers id)]
    (if inputs
      (f (mapv #(deref (tracked db %)) inputs) query)
      (f @db query))
    (throw (ex-info (str "no subscription " id) {:id id}))))

(defn- tracked [db query] (r/track computed db query))

(defn subscribe
  [query {f :frame}]
  (tracked (:db f) query))

(def leaving
  (r/create-class
   {:display-name           "machine-leaving"
    :component-will-unmount (fn [this] (leave! (second (r/argv this))))
    :reagent-render         (fn [_] nil)}))

(defn entering
  ([f event view] (entering f event nil view))
  ([f event leave view]
   (let [of (fn [e this] (if (fn? e) (apply e (rest (r/argv this))) e))]
     (r/create-class
      {:component-did-mount    (fn [this] (when event (dispatch (of event this) {:frame f})))
       :component-will-unmount (fn [this]
                                 (when leave (dispatch (of leave this) {:frame f}))
                                 (leave! f))
       :reagent-render         view}))))
