(ns sutra.serve.nrepl
  (:require [sutra.foundation.env :as env]
            [sutra.serve.hot-patches :as hot]))

(set! *warn-on-reflection* true)

(defonce ^:private nrepl-server (atom nil))

(defn start-nrepl! []
  (when-let [p (and (hot/dev-surfaces?)
                    (some-> (env/getenv "SUTRA_NREPL_PORT") Long/parseLong))]
    (try
      (require 'nrepl.server 'cider.nrepl)
      (let [handler @(resolve 'cider.nrepl/cider-nrepl-handler)
            srv     ((resolve 'nrepl.server/start-server) :port p :handler handler)]
        (reset! nrepl-server srv)
        (spit ".nrepl-port" (str p))
        (println (str "nREPL listening on " p " (.nrepl-port written)")))
      (catch Throwable e
        (println (str "nREPL start failed: " (.getMessage e)))))))
