(ns sutra.notifications.http
  (:require [sutra.kit.refusal :as refusal]
            [sutra.kit.attempt :as attempt]
            [clojure.data.json :as json]
            [org.httpkit.server :as http-kit]
            [sutra.notifications.inbox :as notes]
            [sutra.notifications.prefs :as prefs]
            [sutra.foundation.periodic :as periodic]
            [sutra.http.endpoint :as ep]
            [sutra.http.responses :refer [ok]]))

(set! *warn-on-reflection* true)

(defn- wire [n]
  (-> (select-keys n [:id :heading :message :object-type :object-id :automation-id :branch
                      :source :link-url :link-label])
      (assoc :created-at (some-> (:created-at n) .toInstant str)
             :read? (some? (:read-at n)))))

(def list-notifications
  (ep/handler :notifications
              {:needs-user true
               :respond (fn [{:keys [store limit unread] u :user}]
                          (ok {:notifications
                                 (mapv wire
                                       (notes/inbox store u
                                                    {:limit (or limit 50) :unread? unread}))
                               :unread (notes/unread-count store u)}))}))

(def mark-read
  (ep/handler :notifications-read
              {:needs-user true
               :respond (fn [{:keys [store body] u :user}]
                          (if-let [ids (cond (:all body) :all
                                             (sequential? (:ids body)) (:ids body))]
                            (ok {:marked (notes/mark-read! store u ids)
                                 :unread (notes/unread-count store u)})
                            (refusal/refused :refusal.notifications/read-takes-ids-or-all {:got body})))}))

(defn- settings-view [store u]
  {:settings (prefs/of-user store u)
   :defaults prefs/defaults
   :kinds    (sort prefs/kinds)
   :ways     (sort prefs/ways)})

(def preferences
  (ep/handler :notification-preferences
              {:needs-user true
               :call       #(settings-view (:store %) (:user %))}))

(def save-preferences
  (ep/handler :notification-preferences
              {:needs-user true
               :respond (fn [{:keys [store body] u :user}]
                          (let [s (prefs/normalized (or body {}))]
                            (if-let [p (prefs/problems s)]
                              (refusal/refused :refusal.notifications/passed-p {:got body} {:message p})
                              (do (prefs/save! store u s)
                                  (ok (settings-view store u))))))}))

(defn- sse [event data]
  (str "event: " event "\ndata: " (json/write-str data) "\n\n"))

(defonce ^:private open-streams (atom {}))

(def ^:private heartbeat-ms 25000)

(defonce ^:private heartbeat (periodic/slot))

(defn- ping-all! []
  (doseq [[ch _] @open-streams]
    (attempt/silently (http-kit/send! ch ": ping\n\n" false))))

(defn stream
  [req]
  (if-let [u (ep/acting-username :notifications-stream (:sutra/store req))]
    (do
      (periodic/start! heartbeat "sutra-notification-heartbeat"
                       heartbeat-ms heartbeat-ms ping-all!)
      (http-kit/as-channel
       req
       {:on-open  (fn [ch]
                    (http-kit/send! ch {:status 200
                                        :headers {"Content-Type" "text/event-stream"
                                                  "Cache-Control" "no-cache"
                                                  "X-Accel-Buffering" "no"}
                                        :body (sse "open" {:user u})}
                                    false)
                    (swap! open-streams assoc ch
                           (notes/subscribe! u
                                             #(http-kit/send!
                                               ch (sse "notification" (wire %)) false))))
        :on-close (fn [ch _]
                    (when-let [unsub (get @open-streams ch)] (unsub))
                    (swap! open-streams dissoc ch))}))
    (refusal/refused :refusal.notifications/no-user-to-stream-notifications {})))

(defn close-all! []
  (periodic/stop! heartbeat)
  (doseq [[ch unsub] @open-streams]
    (attempt/silently (unsub))
    (attempt/silently (http-kit/close ch)))
  (reset! open-streams {}))

(def module
  {:name        :notifications-rest
   :jobs        [{:name :notification-streams :start (fn [] nil) :stop close-all!}]
   :contributes {:http/routes {:notifications        {:get list-notifications}
                               :notifications-read   {:post mark-read}
                               :notifications-stream {:get stream}
                               :notification-preferences {:get preferences
                                                          :put save-preferences}}}})
