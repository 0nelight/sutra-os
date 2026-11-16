(ns sutra.linter.http
  (:require [sutra.kit.attempt :as attempt]
            [sutra.http.endpoint :as ep]
            [sutra.lineage.http :as lineage]
            [sutra.linter.rules :as rules]
            [sutra.linter.sweep :as sweep]
            [sutra.store.access.scope :as ctx]
            [sutra.store.seat :as seat]))

(set! *warn-on-reflection* true)

(defn- sweep-now! [store schema branch]
  (sweep/sweep! store schema branch
                (lineage/answers {:store store :schema schema :branch branch})))

(def ^:private first-sweep-ms (* 10 60 1000))

(defn- tick [store]
  (let [at (ctx/at store "main")
        r  (sweep-now! at (seat/rebuild-schema-from-storage at) "main")]
    (println (str "[linter] sweep: " r))))

(def module
  {:name :linter-http
   :jobs [{:name :linter :every "SUTRA_LINTER_SWEEP_MS" :default (* 24 60 60 1000)
           :first-tick (constantly first-sweep-ms)
           :tick (fn [store]
                   (try (tick store)
                        (catch Exception e
                          (attempt/rethrow-fatal! e)
                          (println (str "[linter] sweep failed: " (ex-message e))))))}]
   :contributes
   {:http/routes
    {:linter-rules    {:get (ep/handler :linter-rules {:call (fn [_] {:rules rules/rules})})}
     :linter-sweep    {:post (ep/handler :linter-sweep
                                         {:call #(sweep-now! (:store %) (:schema %) (:branch %))})}
     :linter-list     {:get (ep/handler :linter-list
                                        {:call #(sweep/listed (:store %) (:branch %) %)})}
     :linter-snooze   {:post (ep/handler :linter-snooze
                                         {:call (fn [{:keys [store branch body]}]
                                                  (if (:off body)
                                                    (sweep/unsnooze! store branch body)
                                                    (sweep/snooze! store branch body)))})}
     :linter-assign   {:post (ep/handler :linter-assign
                                         {:call #(sweep/assign! (:store %) (:branch %)
                                                                (:body %))})}}}})
