(ns sutra-ui.state.auth
  (:require [sutra-ui.state.shell :as shell]
            [sutra-ui.state.db :as db]
            [sutra-ui.api :as api]
            [sutra-ui.state.requests :as rq]
            [sutra.i18n.core :as i18n]
            [sutra-ui.state.instances :as instances]
            [sutra-ui.state.sync :as sync]))

(declare reload-visible-data!)

(defonce auth (db/cursor [:auth :auth] {:status :unknown}))

(def ^:private guest-key "sutra.guest")

(defn- guest-remembered? []
  (try (= "1" (.getItem js/localStorage guest-key)) (catch :default _ false)))

(defonce bootstrap-fn (atom nil))

(defonce consumer-bootstrap-fn (atom nil))

(defonce current-workspace (db/cursor [:auth :current-workspace] nil))

(defn- run-bootstrap!
  []
  (if (= "consumer" (:role @auth))
    (when-let [cboot @consumer-bootstrap-fn] (cboot))
    (if-let [boot @bootstrap-fn] (boot) (reload-visible-data!))))

(defn remember-guest! []
  (try (.setItem js/localStorage guest-key "1") (catch :default _ nil))
  (db/put! auth {:status :guest})
  (run-bootstrap!))

(defn- forget-guest! []
  (try (.removeItem js/localStorage guest-key) (catch :default _ nil)))

(reset! api/on-http-error
        (fn [{:keys [status]}]
          (when (and (= 401 status)
                     (#{:authed :guest} (:status @auth)))
            (forget-guest!)
            (db/put! auth {:status          :anon
                           :allow-anon      (:allow-anon @auth)}))))

(defn- ask-me
  []
  (-> (api/get! :me)
      (.then (fn [me] {:me me}))
      (.catch (fn [e] {:refused (:body (ex-data e))}))))

(db/reg :sutra.auth/check
        (fn [db [_ guest? reply]]
          (rq/request db {:ask [ask-me] :then [[:sutra.auth/checked guest?]]} reply)))

(db/reg :sutra.auth/checked
        (fn [db [_ guest? {:keys [me refused]}]]
          (let [allow-anon (boolean (:allow-anon refused))]
            {:db (assoc-in db [:auth :auth]
                           (if me
                             (merge {:status :authed} me)
                             {:status     (if (and allow-anon guest?) :guest :anon)
                              :allow-anon allow-anon}))
             :fx [(when-not (or me allow-anon) [:invoke [forget-guest!]])]})))

(def ^:private federated-prefix "#federated=")

(defn- federated-login!
  []
  (let [h (str (.-hash js/location))]
    (if-not (.startsWith h federated-prefix)
      (js/Promise.resolve nil)
      (let [a (js/decodeURIComponent (subs h (count federated-prefix)))]
        (.replaceState js/history nil "" (str (.-pathname js/location) (.-search js/location)))
        (-> (api/post! :federation-login {:assertion a})
            (.catch (fn [e]
                      (shell/fail! (i18n/t :ui/federated-login-failed
                                           {:error (or (:error (:body (ex-data e)))
                                                       (ex-message e))})))))))))

(defn check-auth!
  []
  (-> (federated-login!)
      (.then (fn [_] (db/promise! [:sutra.auth/check (guest-remembered?)])))))

(defn- reload-visible-data!
  []
  (sync/refresh-snapshot!)
  (instances/refresh-instance-state!))

(defn finish-login!
  []
  (-> (check-auth!) (.then (fn [_] (run-bootstrap!)))))

(defn sign-in!
  [username password code]
  (-> (api/post! :login (cond-> {:username username :password password}
                          (seq code) (assoc :code code)))
      (.then (fn [res]
               (if (seq (:limited res))
                 res
                 (.then (finish-login!) (fn [_] res)))))))

(defn login!
  [username password]
  (sign-in! username password nil))

(defn change-password!
  [current new]
  (api/post! :password {:current current :new new}))

(defn mfa-enroll!
  [password]
  (api/post! :mfa-enroll {:password password}))

(defn mfa-confirm!
  [code]
  (api/post! :mfa-confirm {:code code}))

(defn try-sso!
  []
  (-> (api/fetch-url (api/negotiate-url)
                       #js {:credentials "include"
                            :headers     #js {"Accept" "application/transit+json"}})
      (.then  (fn [resp] (boolean (.-ok resp))))
      (.catch (fn [_] false))))

(defn sso-login!
  []
  (-> (try-sso!)
      (.then (fn [ok?]
               (if ok?
                 (-> (check-auth!)
                     (.then (fn [_] (run-bootstrap!)))
                     (.then (fn [_] true)))
                 false)))))

(defn logout! []
  (forget-guest!)
  (-> (api/post! :logout)
      (.then (fn [_] (db/put! auth {:status :anon})))
      (.then (fn [_] (reload-visible-data!)))))

(defn install-bootstraps!
  [builder consumer]
  (reset! bootstrap-fn builder)
  (reset! consumer-bootstrap-fn consumer))

(defn set-current-workspace! [ws] (db/put! current-workspace ws))

(defn show-sign-in!
  []
  (db/put! auth {:status :anon}))
