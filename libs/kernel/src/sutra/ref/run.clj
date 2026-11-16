(ns sutra.ref.run
  (:require [sutra.ref.action :as action]
            [sutra.ref.err :as err]
            [sutra.ref.kernel :as k]
            [sutra.ref.lower :as lower]))

(set! *warn-on-reflection* true)

(defn- run* [sigma kappa world f]
  (let [cx {:sigma sigma :kappa (merge {:mode :apply :objects-loaded-max 100000} kappa)}]
    (try
      (let [run-state (f {:world world :bindings {} :events [] :ops [] :loaded 0} cx)
            R  (:ops run-state)]
        {:world      (:world run-state)
         :bindings   (:bindings run-state)
         :ops        R
         :events     (:events run-state)
         :tx-vector  (into R (:events run-state))
         :proposals  (:proposals run-state [])
         :collected  (:collected run-state [])
         :ddl        (:ddl run-state [])})
      (catch clojure.lang.ExceptionInfo e
        {:error {:kind (err/kind e) :message (ex-message e) :data (dissoc (ex-data e) :ref/kind)}
         :ops   []})
      (catch InterruptedException e (throw e))
      (catch Exception e
        {:error {:kind :host-error :message (ex-message e)
                 :data {:cause-class (.getName (class e))}}
         :ops   []}))))

(defn run-rules
  ([sigma kappa world rules] (run-rules sigma kappa world {} rules))
  ([sigma kappa world bindings rules]
   (run* sigma kappa world
         (fn [run-state cx]
           (k/ev (lower/lower-seq rules) (assoc run-state :bindings bindings) cx)))))

(defn run-action
  [sigma kappa world action-name args]
  (run* sigma kappa world
        (fn [run-state cx] (action/invoke run-state cx action-name {:arg0 args}))))

