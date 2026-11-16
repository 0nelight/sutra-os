(ns platform-sdk.watch
  (:require ["ws" :refer [WebSocket]]
            [platform-sdk.client :as client]
            [sutra.types.paths :as paths]))

(defn eq
  [field value]
  {:type "eq" :field (name field) :value value})

(defn- object-set
  [{:keys [object-type where]}]
  (let [base {:type "base" :objectType (name object-type)}]
    (if where {:type "filter" :objectSet base :where where} base)))

(defn- request-of
  [{:keys [properties] :as sub}]
  (cond-> {:objectSet (object-set sub)}
    (seq properties) (assoc :propertySet (mapv name properties))))

(defn- ws-url
  [{:keys [base-url branch]}]
  (str (.replace base-url #"^http" "ws") (paths/address :ontology-watch)
       (when branch (str "?branch=" (js/encodeURIComponent branch)))))

(def ^:private retry-ms 5000)

(defn- dispatch!
  [{:keys [on-change on-refresh on-error]} ^js raw]
  (let [m (js->clj (js/JSON.parse (.toString raw)) :keywordize-keys true)]
    (case (:type m)
      "objectSetChanged" (doseq [u (:updates m)] (on-change u))
      "refreshObjectSet" (on-refresh m)
      "error"            (on-error m)
      "subscribeResponses" (doseq [r (:responses m) :when (= "error" (:type r))] (on-error r))
      nil)))

(defn subscribe!
  [c subs {:keys [on-open on-error] :or {on-open (fn []) on-error (fn [_])} :as handlers}]
  (let [handlers (assoc handlers :on-open on-open :on-error on-error
                        :on-change (:on-change handlers (fn [_]))
                        :on-refresh (:on-refresh handlers (fn [_])))
        wanted?  (atom true)
        socket   (atom nil)
        open!    (fn open! []
                   (-> (client/fresh-token! c)
                       (.then
                        (fn [token]
                          (when @wanted?
                            (let [ws (WebSocket. (ws-url c)
                                                 #js {:headers #js {:Authorization
                                                                    (str "Bearer " token)}})
                                  retry! #(when @wanted? (js/setTimeout open! retry-ms))]
                              (reset! socket ws)
                              (.on ws "open"
                                   (fn []
                                     (.send ws (js/JSON.stringify
                                                (clj->js {:id       (str (random-uuid))
                                                          :requests (mapv request-of subs)})))
                                     ((:on-open handlers))))
                              (.on ws "message" #(dispatch! handlers %))
                              (.on ws "unexpected-response"
                                   (fn [_ ^js res]
                                     (when (= 401 (.-statusCode res)) (reset! (:token c) nil))))
                              (.on ws "error" (fn [^js e] (on-error {:message (.-message e)})))
                              (.on ws "close" retry!)))))
                       (.catch (fn [^js e]
                                 (on-error {:message (.-message e)})
                                 (when @wanted? (js/setTimeout open! retry-ms))))))]
    (open!)
    (fn []
      (reset! wanted? false)
      (when-let [^js ws @socket] (.close ws)))))
