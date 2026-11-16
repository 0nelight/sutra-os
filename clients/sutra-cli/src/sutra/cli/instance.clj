(ns sutra.cli.instance
  (:require [sutra.cli.out :as out]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.set :as set]
            [sutra.types.paths :as paths]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(def cache-path ".run/system.edn")

(def ^:private ttl-ms (* 10 60 1000))

(defn drift
  [server-routes cli-routes plugin-of]
  (let [s (set server-routes) c (set cli-routes)]
    {:server-only (vec (sort (set/difference s c)))
     :cli-only    (vec (sort (remove plugin-of (set/difference c s))))}))

(defn- read-cache []
  (let [f (io/file cache-path)]
    (or (when (.exists f) (try (edn/read-string (slurp f)) (catch Exception _ nil))) {})))

(defn- warn-drift! [facts]
  (let [{:keys [server-only cli-only]} (drift (:routes facts) (keys paths/paths) paths/plugin-of)]
    (when (or (seq server-only) (seq cli-only))
      (out/warn! (str "  ! this CLI and the server differ in their API"
                      (when (seq server-only)
                        (str " — the server serves " (count server-only)
                             " route(s) this CLI does not know"))
                      (when (seq cli-only)
                        (str " — this CLI knows " (count cli-only) " route(s) the server lacks: "
                             (pr-str (take 5 cli-only))))
                      "; update the one that is older")))))

(defn facts
  []
  (let [base  (str (tx/current-base))
        now   (System/currentTimeMillis)
        cache (read-cache)
        hit   (get cache base)]
    (if (and hit (< (- now (:at hit)) ttl-ms))
      (:facts hit)
      (let [{:keys [status body]} (try (api/get! :system) (catch Exception _ nil))]
        (when (= 200 status)
          (warn-drift! body)
          (try (io/make-parents (io/file cache-path))
               (spit cache-path
                     (with-out-str (pprint/pprint (assoc cache base {:at now :facts body}))))
               (catch Exception _ nil))
          body)))))

(defn provider
  [fs id]
  (some #(when (= (name id) (name (:id %))) %) (:identity-providers fs)))
