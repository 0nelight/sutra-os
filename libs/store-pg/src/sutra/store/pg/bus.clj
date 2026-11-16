(ns sutra.store.pg.bus
  (:require [clojure.edn :as edn]
            [next.jdbc :as jdbc]
            [sutra.kit.system :as sys])
  (:import (java.sql Connection)
           (org.postgresql PGConnection PGNotification)))

(set! *warn-on-reflection* true)

(def ^:private channel "sutra_cluster")

(defonce ^:private origin (str (random-uuid)))

(def ^:private max-payload-bytes 7900)

(defn- problem
  [topic message]
  (when-let [check (sys/provided (sys/process) :cluster/check-message)]
    (check topic message)))

(defn broadcast!
  [pools topic message]
  (let [s (pr-str {:topic topic :origin origin :message message})]
    (if-let [p (problem topic message)]
      (do (println (str "[bus] " topic " not sent: " p)) false)
      (if (> (alength (.getBytes s "UTF-8")) max-payload-bytes)
        false
        (do (jdbc/execute! (pools :control) ["SELECT pg_notify(?, ?)" channel s])
            true)))))

(defn- dispatch!
  [^String s]
  (let [{:keys [topic message] from :origin} (edn/read-string s)]
    (when-not (= origin from)
      (when-let [p (problem topic message)]
        (println (str "[bus] " topic " from another process is not what this one reads: " p)))
      (when-let [f (get (sys/table (sys/process) :cluster/listeners) topic)]
        (f message)))))

(defonce ^:private state (atom {:running? false :thread nil}))

(defn- listen!
  [pools]
  (while (:running? @state)
    (try
      (with-open [^Connection conn (.getConnection ^javax.sql.DataSource (pools :control))]
        (jdbc/execute! conn [(str "LISTEN " channel)])
        (let [^PGConnection pgc (.unwrap conn PGConnection)]
          (while (:running? @state)
            (doseq [^PGNotification n (.getNotifications pgc 1000)]
              (try (dispatch! (.getParameter n))
                   (catch InterruptedException e (throw e))
                   (catch Exception t
                     (println (str "[bus] a listener failed: " (.getMessage t)))))))))
      (catch InterruptedException _ nil)
      (catch Throwable t
        (when (:running? @state)
          (println (str "[bus] listening failed, again in 2 s: " (.getMessage t)))
          (try (Thread/sleep 2000) (catch InterruptedException _ nil)))))))

(defn start!
  []
  (swap! state assoc :running? true)
  (when-not (:thread @state)
    (swap! state assoc :thread (doto (Thread. ^Runnable (partial listen! (sys/required (sys/process) :pg/ds))
                                              "sutra-cluster-bus")
                                 (.setDaemon true)
                                 (.start)))))

(defn stop!
  []
  (swap! state assoc :running? false)
  (when-let [^Thread t (:thread @state)]
    (.interrupt t)
    (.join t 3000))
  (swap! state assoc :thread nil))

(def module
  {:name     :cluster-bus
   :requires [:pg/ds]
   :opens    {:cluster/broadcast (fn [_ {pools :pg/ds}] (partial broadcast! pools))}
   :jobs     [{:name :cluster-bus :start start! :stop stop!}]})
