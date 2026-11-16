(ns sutra.builds.import.env
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.lookup :as lookup]
            [sutra.foundation.live-trace :as live]
            [sutra.foundation.profile :as prof]
            [sutra.store.access.datasets :as dsets]
            [sutra.store.access.scope :as ctx]
            [sutra.store.control :as ctl]
            [sutra.store.storage.instance-config :as ic]
            [sutra.builds.run.monitor :as bm]))

(set! *warn-on-reflection* true)

(defn ms-since [t0]
  (Math/round (double (/ (- (System/nanoTime) t0) 1e6))))

(defn- log-level
  [line]
  (cond
    (re-find #"(?i)\b(error|exception|traceback|critical|fatal)\b" line) :error
    (re-find #"(?i)\bwarn(ing)?\b" line) :warn
    (re-find #"(?i)\b(debug|trace)\b" line) :debug
    :else :info))

(defn- span-marker!
  [store tname line]
  (when (and (prof/armed?) (str/starts-with? line "{\"sutra_span\": \"close\""))
    (let [{:keys [ms name model failed]} (json/read-str line :key-fn keyword)
          ms (long (or ms 0))]
      (prof/ingest! [{:kind :Python :epoch-ms (- (ctl/millis store) ms) :dur ms
                      :track (str "python " (clojure.core/name tname))
                      :data (cond-> {:name (str name) :transform (clojure.core/name tname)}
                              model  (assoc :model (str model))
                              failed (assoc :failed (str failed)))}]))))

(defn- job-log-sink
  [store schema run-id]
  (let [rid (some-> run-id str)]
    (fn [tname line]
      (live/emit! {:kind :job/log :run-id rid
                   :transform (str (or (lookup/rid-of schema :transforms tname) tname))
                   :level (log-level line) :line (live/clip line)})
      (span-marker! store tname line))))

(defn- observer
  [pa ptimes]
  (fn [{:keys [event phase ms] :as e}]
    (case event
      :phase/start    (swap! pa assoc :phase phase)
      :phase/end      (do (swap! ptimes conj {:phase phase :ms ms})
                          (swap! pa update :phases (fnil conj []) {:phase phase :ms ms}))
      :step           (do (swap! ptimes conj {:phase phase :ms ms})
                          (swap! pa update :steps (fnil conj [])
                                 {:step (subs (str phase) 1) :ms ms}))
      :landing        (swap! pa update :steps (fnil conj [])
                             (assoc (:landing e)
                                    :step (str (some-> (:phase @pa) name) "/landing")
                                    :ms (:total-ms (:landing e))
                                    :dataset (:dataset e)))
      :publish/failed
      (println
       (format "  ⚠ publish job FAILED (isolated — build continued) — %s: %d row(s) skipped: %s"
               (clojure.core/name (:fn e)) (:count e) (:error e))))))

(defn build-env
  [store schema build-branch run-id {:keys [origin] :as build}]
  (let [pa         (bm/register! run-id nil
                                 {:branch build-branch
                                  :origin (or origin
                                              (str "user "
                                                   (grants/current-user-id
                                                    (grants/actor store))))})
        ptimes     (atom [])
        observe    (observer pa ptimes)
        work       (bm/work pa)
        media-sink (atom [])]
    (merge (select-keys build [:plan :datasources :node-scope :scope :options])
           {:store      (assoc store :on-landing
                               (fn [dataset landing]
                                 (observe {:event :landing :dataset dataset :landing landing})))
            :schema     schema
            :build-branch   build-branch
            :run-id     run-id
            :pa         pa
            :work       work
            :observe    observe
            :phase-times (fn [] @ptimes)
            :media-sink media-sink
            :build-low  (dsets/current-version store)
            :row-cache  (atom {})
            :code-env   {:work       work
                         :blobs      (ctx/blobs)
                         :lm-service (delay (ic/lm-service-config store))
                         :media-sink media-sink
                         :log-line   (job-log-sink store schema run-id)
                         :hook       {:before (fn [tname]
                                                (bm/check-cancelled! pa {:transform tname})
                                                (bm/start-transform! pa tname))
                                      :after  (fn [tname ms rows _code-sha]
                                                (bm/finish-transform! pa tname ms rows))}}
            :sub        (fn [k thunk]
                          (let [t0 (System/nanoTime) r (thunk)]
                            (observe {:event :step :phase k :ms (ms-since t0)})
                            r))
            :cancel-at  (fn [phase] #(bm/check-cancelled! pa {:phase phase}))})))
