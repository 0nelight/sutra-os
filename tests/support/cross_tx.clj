(ns support.cross-tx
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [sutra.store.core.commit :as commit]))

(defn- cores [] (edn/read-string (slurp "cores.edn")))

(def owners
  (delay (into {} (for [[m ts] (get-in (cores) [:modules :tables]) t ts] [(keyword t) m]))))

(def declared
  (delay (into #{} (map (comp set :modules)) (get-in (cores) [:modules :cross-module-tx]))))

(def ^:private world-keys [:ops :schema :log :retires :parents :reparent :forget-edits
                           :adopt-edits :restore])

(defn modules-of
  [tx]
  (cond-> (into #{}
                (keep (fn [{:keys [coll]}]
                        (case (namespace coll)
                          "ctl" (get @owners (keyword (name coll))
                                     (symbol (str "unowned:" (name coll))))
                          ("consumer" "ops" "outbox") 'store
                          nil)))
                (:records tx))
    (some #(seq (get tx %)) world-keys) (conj 'world)
    (seq (:fences tx))                  (conj 'lease)))

(defn- where []
  (->> (.getStackTrace (Thread/currentThread))
       (map #(.getClassName ^StackTraceElement %))
       (keep #(second (re-find #"^(sutra\.[\w.]+?)\$" (str/replace % "_" "-"))))
       (remove #(re-find #"^sutra\.(store|kit|foundation)\." %))
       distinct
       (take 4)
       vec))

(defn- seen! [ms]
  (let [log (System/getenv "SUTRA_CROSS_TX_LOG")]
    (cond
      log
      (locking owners (spit log (str (pr-str {:modules (vec (sort ms)) :where (where)}) "\n")
                            :append true))

      (not (contains? @declared ms))
      (throw (ex-info (str "a commit for modules " (vec (sort ms)) " that cores.edn "
                           ":cross-module-tx does not declare — a new transaction across "
                           "modules ties them to one database; declare it with why, or "
                           "split it")
                      {:modules ms :where (where)})))))

(defn enable!
  [config]
  (alter-var-root #'commit/commit-in!
                  (fn [f]
                    (if (::wrapped (meta f))
                      f
                      (with-meta (fn [db h tx]
                                   (let [ms (modules-of tx)]
                                     (when (and (< 1 (count ms))
                                                (some (complement '#{world lease store}) ms))
                                       (seen! ms)))
                                   (f db h tx))
                        {::wrapped true}))))
  (println "[support.cross-tx] commits across modules: checked against cores.edn")
  config)
