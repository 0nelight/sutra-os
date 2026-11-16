(ns sutra.peering.http
  (:require [sutra.http.endpoint :as ep]
            [sutra.kit.refusal :as refusal]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.lookup :as lookup]
            [sutra.http.refusals :as refusals]
            [sutra.http.request :as http-request]
            [sutra.http.responses :refer [ok]]
            [sutra.kit.anomaly :as anomaly]
            [sutra.peering.connections :as conns]
            [sutra.peering.jobs :as jobs]
            [sutra.peering.media :as media]
            [sutra.peering.objects :as objects]
            [sutra.peering.profiles :as profiles]
            [sutra.peering.rows :as rows]
            [sutra.peering.sync :as sync]
            [sutra.peering.wire :as wire]
            [sutra.schedules.api :as schedules]
            [sutra.types.peering :as peering]))

(set! *warn-on-reflection* true)

(def ^:private round-every-ms
  (or (some-> (System/getenv "SUTRA_PEERING_MS") parse-long) 5000))

(defn- refused [req f]
  (try (f)
       (catch clojure.lang.ExceptionInfo e
         (if-let [r (anomaly/of e)] (refusals/response req r) (throw e)))))

(defmacro ^:private refusing [req & body] `(refused ~req (fn [] ~@body)))

(defn- actor [req] (some-> (ep/signed-in-user req) str))

(defn- conn-param [req] (get-in req [:path-params :connection]))

(defn- truthy? [v] (or (true? v) (= "true" v)))

(defn manages?
  [{store :sutra/store auth :sutra/auth :as req} space]
  (or (grants/auth-off? store)
      (= grants/admin-username (actor req))
      (boolean
       (when-let [rid (and space auth
                           (lookup/rid-of (objects/schema store) :spaces (keyword (str space))))]
         ((grants/owner-of auth) rid)))))

(defn- managed!
  [req space]
  (when-not (manages? req space)
    (refusal/refuse! :refusal.peering/only-who-can-manage-manages {:space space} {:value (if space (str "space " space) "this instance")}))
  space)

(defn- connection!
  [{store :sutra/store :as req}]
  (let [c (rows/by-name-or-id store (conn-param req))]
    (managed! req (:space c))
    c))

(defn connections-handler
  [{:keys [request-method body-params] store :sutra/store :as req}]
  (refusing req
    (if (= :post request-method)
      (do (managed! req (:space body-params))
          (ok (conns/connect! store (actor req) body-params)))
      (ok {:connections (into [] (comp (filter #(manages? req (:space %)))
                                       (map #(conns/view store %)))
                              (rows/connections store))}))))

(defn accept-handler [{:keys [body-params] store :sutra/store :as req}]
  (refusing req
    (managed! req (:space body-params))
    (ok (conns/accept! store (actor req) body-params))))

(defn connection-handler
  [{:keys [request-method] store :sutra/store :as req}]
  (refusing req
    (let [c (connection! req)
          id (:connection-id c)]
      (if (= :delete request-method)
        (do (try (conns/set-disabled! store id true)
                 (catch Exception e
                   (println (str "[peering] connection " id
                                 " not disabled before removal: " (ex-message e)))))
            (doseq [m (rows/mappings store id)] (rows/drop-mapping! store id (:local-type m)))
            (rows/acknowledged! store (map :item-id (rows/outbox store id)))
            (rows/drop-connection! store id)
            (ok {:deleted id}))
        (ok (assoc (conns/view store c)
                   :invite (when (= :requested (:status c)) (:invite c))
                   :mappings (rows/mappings store id)
                   :queued (count (rows/outbox store id))
                   :health (rows/health store id)))))))

(defn activate-handler [{store :sutra/store :as req}]
  (refusing req (ok (conns/activate! store (:connection-id (connection! req))))))

(defn status-handler [{:keys [body-params] store :sutra/store :as req}]
  (refusing req (ok (conns/set-disabled! store (:connection-id (connection! req))
                                         (truthy? (:disabled body-params))))))

(defn ceiling-handler [{:keys [body-params] store :sutra/store :as req}]
  (refusing req
    (if (truthy? (:approve body-params))
      (ok (conns/approve-ceiling! store (:connection-id (connection! req))))
      (ok (conns/propose-ceiling! store (:connection-id (connection! req)) body-params)))))

(defn ingress-handler [{:keys [body-params] store :sutra/store :as req}]
  (refusing req (ok (conns/set-ingress! store (:connection-id (connection! req))
                                        (:cidrs body-params)))))

(defn markings-handler [{:keys [body-params] store :sutra/store :as req}]
  (refusing req (ok (conns/map-markings! store (:connection-id (connection! req))
                                         (:remote body-params) (:local body-params)))))

(defn mappings-handler
  [{:keys [request-method body-params] store :sutra/store :as req}]
  (refusing req
    (if (= :post request-method)
      (ok (conns/map-type! store (actor req) (:connection-id (connection! req)) body-params))
      (ok {:mappings (rows/mappings store (:connection-id (connection! req)))}))))

(defn mapping-handler [{store :sutra/store :as req}]
  (refusing req (ok (conns/unmap-type! store (:connection-id (connection! req))
                                       (get-in req [:path-params :type])))))

(defn jobs-handler [{store :sutra/store :as req}]
  (refusing req
    (let [c  (connection! req)
          js (rows/jobs store (:connection-id c))]
      (ok {:connection-id (:connection-id c)
           :health        (rows/health store (:connection-id c))
           :jobs          (vec (take 100 js))
           :queued        (count (rows/outbox store (:connection-id c)))}))))

(defn run-handler [{store :sutra/store :as req}]
  (refusing req
    (profiles/round! store)
    (jobs/tick! store)
    (ok {:connections (mapv #(conns/view store %) (rows/connections store))})))

(defn media-handler
  [{:keys [request-method body-params] store :sutra/store :as req}]
  (refusing req
    (let [c (connection! req)]
      (if (= :post request-method)
        (ok (media/map-set! store (actor req) (:connection-id c) body-params))
        (ok {:media (rows/media-mappings store (:connection-id c))})))))

(defn media-set-handler [{store :sutra/store :as req}]
  (refusing req
    (let [c (connection! req) s (get-in req [:path-params :set])]
      (rows/drop-media-mapping! store (:connection-id c) s)
      (ok {:unmapped s :connection-id (:connection-id c)}))))

(defn shares-handler
  [{:keys [request-method body-params] store :sutra/store :as req}]
  (refusing req
    (let [c (connection! req)]
      (if (= :post request-method)
        (ok (sync/share! store (actor req) (:connection-id c) (:dataset body-params)))
        (ok {:shares (rows/shares store (:connection-id c))})))))

(defn share-handler [{store :sutra/store :as req}]
  (refusing req
    (let [c (connection! req)]
      (ok (sync/unshare! store (:connection-id c) (get-in req [:path-params :dataset]))))))

(defn profiles-handler
  [{:keys [request-method body-params] store :sutra/store :as req}]
  (refusing req
    (if (= :post request-method)
      (do (managed! req (:space body-params))
          (ok (profiles/create-pair! store (actor req) body-params)))
      (ok {:profiles (into [] (comp (filter #(manages? req (:space %)))
                                    (map #(profiles/view store %)))
                           (rows/profiles store))}))))

(defn profile-install-handler [{:keys [body-params] store :sutra/store :as req}]
  (refusing req
    (managed! req (:space body-params))
    (ok (profiles/view store (profiles/install! store (actor req) body-params)))))

(defn profile-handler
  [{:keys [request-method] store :sutra/store :as req}]
  (refusing req
    (let [p (rows/profile! store (get-in req [:path-params :profile]))]
      (managed! req (:space p))
      (if (= :delete request-method)
        (do (rows/drop-profile! store (:profile-id p))
            (ok {:deleted (:profile-id p)}))
        (ok (assoc (profiles/view store p) :counterpart (peering/counterpart p)))))))

(defn mesh-handler
  [{:keys [request-method body-params] store :sutra/store :as req}]
  (refusing req
    (if (= :post request-method)
      (ok (profiles/set-mesh-node! store (:trust body-params) (:labels body-params)
                                   (truthy? (:remove body-params))))
      (ok {:mesh (rows/mesh store)}))))

(defn- answer [x] (ok {:payload (pr-str x)}))

(defn peer-handler
  [{:keys [body-params] store :sutra/store :as req}]
  (refusing req
    (let [opened (wire/opened store body-params)]
      (if (map? opened)
        {:status 401 :body {:error (:refused opened)}}
        (let [[signer {:keys [op connection-id] :as msg}] opened
              c (rows/connection store connection-id)]
          (cond
            (and (nil? c) (= :profile-offer (keyword (name op))))
            (answer (profiles/take-offer! store signer msg))

            (nil? c)
            (refusal/refused :refusal.peering/no-peer-connection {} {:connection-id connection-id})

            (not= (:trust c) signer)
            (refusal/refused :refusal.peering/connection-is-with-not {} {:connection-id connection-id :trust (:trust c) :signer signer})

            (not (wire/ingress-allows? (:ingress c) (http-request/client-ip req)))
            (refusal/refused :refusal.peering/the-connection-s-ingress-does {})

            (#{:shared :rows} (keyword (name op)))
            (answer (sync/served store c msg))

            (= :batch (keyword (name op)))
            (if (peering/flowing? c)
              (answer (jobs/receive! store c msg))
              (refusal/refused :refusal.peering/the-connection-is-not-active {}))

            :else (answer (conns/control! store c msg))))))))

(def module
  {:name        :peering
   :provides    {:objects/foreign-owned jobs/peered-ids}
   :contributes {:http/routes
                 {:peering-connections {:get connections-handler :post connections-handler}
                  :peering-accept      {:post accept-handler}
                  :peering-connection  {:get connection-handler :delete connection-handler}
                  :peering-activate    {:post activate-handler}
                  :peering-status      {:post status-handler}
                  :peering-ceiling     {:post ceiling-handler}
                  :peering-ingress     {:post ingress-handler}
                  :peering-markings    {:post markings-handler}
                  :peering-mappings    {:get mappings-handler :post mappings-handler}
                  :peering-mapping     {:delete mapping-handler}
                  :peering-jobs        {:get jobs-handler}
                  :peering-run         {:post run-handler}
                  :peering-peer        {:post peer-handler}
                  :peering-profiles    {:get profiles-handler :post profiles-handler}
                  :peering-profile-install {:post profile-install-handler}
                  :peering-profile     {:get profile-handler :delete profile-handler}
                  :peering-mesh        {:get mesh-handler :post mesh-handler}
                  :peering-media       {:get media-handler :post media-handler}
                  :peering-media-set   {:delete media-set-handler}
                  :peering-shares      {:get shares-handler :post shares-handler}
                  :peering-share       {:delete share-handler}}
                 :foreign/connectors {:sutra-peer sync/connector}
                 :watch/hooks {:peering (fn [branch t]
                                         (jobs/noticed! branch t
                                                        #(schedules/poke-job! :peering)))}}
   :jobs        [{:name :peering :every "SUTRA_PEERING_MS" :default 5000
                  :tick (fn [store]
                          (jobs/tick! store round-every-ms #(profiles/round! store)))}]})
