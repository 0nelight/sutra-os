(ns sutra.repos.custody
  (:require [sutra.store.access.scope :as ctx]
            [sutra.repos.projection :as gp]))

(set! *warn-on-reflection* true)

(defonce ^:private drained? (atom false))

(defn tick! [store]
  (try
    (let [{:keys [rebuilt failed]} (gp/rebuild-missing! store (ctx/repos) (ctx/blobs))]
      (doseq [rid rebuilt] (println "[custody] rebuilt the git projection of" rid "from Postgres"))
      (doseq [[rid msg] failed] (println "[custody] could not rebuild" rid "—" msg)))
    (catch InterruptedException e (throw e))
    (catch Exception t (println "[custody] repository check failed:" (ex-message t))))
  (when-not @drained?
    (try
      (let [{:keys [copied corrupt]} (ctx/drain-blobs! (ctx/blobs))]
        (when (pos? (or copied 0))
          (println "[custody] moved" copied "blob(s) from the legacy directory into Postgres"))
        (doseq [h corrupt]
          (println "[custody] legacy blob" h "does not hash to its name — left behind"))
        (reset! drained? true))
      (catch InterruptedException e (throw e))
      (catch Exception t
        (println "[custody] blob drain failed, retrying next tick:" (ex-message t))))))

(def module
  {:name :custody
   :jobs [{:name :custody :every "SUTRA_CUSTODY_MS" :tick tick!
           :default 600000 :first-tick #(min % 5000)}]})
