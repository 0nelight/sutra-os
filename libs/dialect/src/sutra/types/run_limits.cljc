(ns sutra.types.run-limits)

(def timeout-ms
  {:default 60000
   :min     1000
   :max     600000})

(def objects-loaded-max 100000)

(def edits-max 10000)

(def action-batch
  {:calls-max 10000
   :unbatched-function-calls-max 20})

(def transcript-chars-max 800000)

(def query-tool-rows-max 50)

(def media-pages-default 4)
(def media-pages-max 20)
(def media-blob-bytes-max (* 32 1024 1024))
(def media-image-bytes-max (* 12 1024 1024))
(def media-text-min-chars 24)
(def media-render-dpi 110)

(def automate
  {:scheduled-per-object-max 1000
   :manual-object-set-max    5000000
   :batch-max                1000})

(def logic-concurrency-default 20)

(def logic-event-cool-down {:first-ms 60000 :max-ms 86400000})
(def logic-event-attempts-max 8)

(def logic-retry
  {:attempts-max 3 :delay-ms 1000 :max-delay-ms 30000 :jitter 0.2})

(def await-margin-ms 10000)

(defn function-timeout-ms
  [spec]
  (or (:timeout-ms spec) (:default timeout-ms)))
