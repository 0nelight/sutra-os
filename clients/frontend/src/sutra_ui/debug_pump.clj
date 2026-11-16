(ns sutra-ui.debug-pump
  (:require [shadow.cljs.devtools.api :as shadow]
            [clojure.edn :as edn]))

(defonce ^:private pump (atom nil))

(defn- drain-lines!
  []
  (try
    (let [res (shadow/cljs-eval :app "(sutra-ui.debug/drain-lines)" {})
          s   (some-> res :results first)]
      (if (and s (not= s "nil")) (edn/read-string s) ""))
    (catch Throwable _ "")))

(defn stop! []
  (when-let [{:keys [running?]} @pump] (reset! running? false))
  (reset! pump nil)
  :stopped)

(defn start!
  ([path] (start! path 600))
  ([path interval-ms]
   (stop!)
   (let [running? (atom true)
         t (Thread.
            (fn []
              (loop []
                (when @running?
                  (let [lines (drain-lines!)]
                    (when (seq lines) (spit path lines :append true)))
                  (Thread/sleep interval-ms)
                  (recur)))))]
     (.setDaemon t true)
     (.setName t "sutra-frontend-log-pump")
     (.start t)
     (reset! pump {:running? running? :path path})
     (spit path (str "=== frontend.log pump started @ " (java.util.Date.) " ===\n")
           :append true)
     {:started path :interval-ms interval-ms})))

(defn- default-path
  []
  (-> (java.io.File. (System/getProperty "user.dir") "../../.run/frontend.log")
      .getCanonicalFile .getPath))

(defn hook
  {:shadow.build/stage :flush}
  [build-state & _]
  (when-not @pump
    (try (start! (default-path)) (catch Throwable _ nil)))
  build-state)
