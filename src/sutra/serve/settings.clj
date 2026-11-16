(ns sutra.serve.settings
  (:require [sutra.kit.attempt :as attempt]
            [sutra.foundation.periodic :as periodic]
            [sutra.foundation.settings :as settings]
            [sutra.http.responses :refer [ok]]
            [sutra.kit.system :as sys]
            [sutra.store.control :as ctl]
            [sutra.serve.ports :as ports]
            [sutra.store.access.scope :as ctx]
            [sutra.store.storage.instance-config :as ic]))

(set! *warn-on-reflection* true)

(def ^:private poll-ms 60000)

(defonce ^:private poller (periodic/slot))

(defn- take-rows!
  [store rows]
  (when (seq (settings/install! rows))
    (sys/rearm-jobs! (sys/of store) settings/lookup)))

(defn load!
  []
  (let [store (ctx/system-store)]
    (take-rows! store (settings/read-row (ic/get-cfg store settings/row-key)))))

(defn- load-logged!
  []
  (try (load!)
       (catch Throwable t
         (attempt/rethrow-fatal! t)
         (println (str "[settings] reload failed: " (.getMessage t))))))

(defn start! []
  (load-logged!)
  (periodic/start! poller "sutra-settings" poll-ms poll-ms load-logged!))

(defn stop! [] (periodic/stop! poller))

(defn change!
  [store {:keys [sets resets by]}]
  (let [at   (ctl/stamp store)
        text (ic/swap-cfg! store settings/row-key
                           #(pr-str (settings/changed-rows
                                     (settings/read-row %)
                                     {:sets sets :resets resets :by by :at at})))]
    (take-rows! store (settings/read-row text))
    (when-let [broadcast (ports/cluster-broadcast store)]
      (broadcast :settings/changed {}))
    (settings/view)))

(defn get-settings
  [_]
  (ok (settings/view)))

(defn set-settings
  [{:keys [body-params principal] store :sutra/store}]
  (ok (change! store {:sets   (:set body-params)
                      :resets (:reset body-params)
                      :by     (or (:username principal) (:user-id principal))})))

(def module
  {:name        :settings
   :jobs        [{:name :settings :start start! :stop stop!}]
   :contributes {:cluster/listeners {:settings/changed (fn [_] (load-logged!))}
                 :http/routes       {:instance-settings {:get get-settings
                                                         :post set-settings}}}})
