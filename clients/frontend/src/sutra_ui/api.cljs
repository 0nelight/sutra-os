(ns sutra-ui.api
  (:require [sutra.wire.abort :as abort]
            [sutra.wire.api :as api]
            [sutra.wire.codec :as codec]
            [sutra.wire.protocol :as protocol]
            [sutra.wire.transport :as tx]
            [sutra-ui.debug :as dbg]))

(defn- log-color [ok? status]
  (cond
    (not ok?)                    "color:#c00;font-weight:bold"
    (and status (>= status 400)) "color:#c00;font-weight:bold"
    :else                        "color:#0a0;font-weight:bold"))

(defn- log-event! [{:keys [phase id method uri body status ok?
                           elapsed-ms decode-ms chars error span]}]
  (let [tag    (str "[api#" id "] " (.toUpperCase (name method)) " " uri)
        api-src (str "api#" id)]
    (case phase
      :request
      (do (js/console.groupCollapsed (str "%c→ " tag) "color:#06c;font-weight:bold")
          (when body (js/console.log "request body:" (clj->js body)))
          (js/console.groupEnd)
          (dbg/record! :api api-src (str "→ " (.toUpperCase (name method)) " " uri)))

      :response
      (do (js/console.log (str "%c← " tag " · " status " · " (.toFixed elapsed-ms 1) "ms"
                               (when chars (str " · " chars " chars")))
                          (log-color ok? status))
          (dbg/record! (if ok? :api :api-error) api-src
                       (str "← " (.toUpperCase (name method)) " " uri
                            " · " status " · " (.toFixed elapsed-ms 1) "ms"))
          (dbg/mark-ms! :api/transit elapsed-ms
                        (str (.toUpperCase (name method)) " " uri " · " status
                             (when chars (str " · " chars " chars")))
                        span)
          (when decode-ms
            (dbg/mark-ms! :api/transit-decode decode-ms
                          (when chars (str chars " chars"))
                          span)))

      :error
      (do (js/console.error (str "✗ " tag) error)
          (dbg/record! :api-error api-src (str "✗ " uri " · " error))))))

(defonce ^:private _install-hook (reset! tx/log!-hook log-event!))

(defonce ^:private _install-span (reset! tx/mint-span!-hook dbg/next-span!))

(def get!    api/get!)
(def post!   api/post!)
(def put!    api/put!)
(def delete! api/delete!)

(def negotiate-url         api/negotiate-url)

(def list-objects          api/list-objects)
(def rename                api/rename)

(def mutate                api/mutate)
(def install-package       api/install-package)

(def list-branches         api/list-branches)

(def list-proposals        api/list-proposals)
(def ask-review            api/ask-review)
(def my-workspace-search   api/my-workspace-search)
(def test-datasource       api/test-datasource)
(def build-estimate           api/build-estimate)
(def edit-history             api/edit-history)
(def data-freshness           api/data-freshness)
(def list-runs             api/list-runs)
(def preview-automation    api/preview-automation)
(def automation-proposals  api/automation-proposals)
(def preview-transform api/preview-transform)
(def stream-subscribe-url  api/stream-subscribe-url)

(def documents          api/documents)

(def on-page-branch         tx/on-page-branch)
(def selected-page-branch   tx/selected-page-branch)
(def effective-page-branch  tx/effective-page-branch)
(def set-page-branch!       tx/set-page-branch!)
(def set-branch-scope!      tx/set-branch-scope!)
(def selected-ontology      tx/selected-ontology)
(def set-page-ontology!     tx/set-page-ontology!)
(def profile-armed?         tx/profile-armed?)
(def on-http-error          tx/on-http-error)
(def adopt!                 tx/adopt!)

(def fetch!          abort/fetch!)
(def fetch-url       abort/fetch-url)
(def aborted-error?  abort/aborted-error?)

(def decode          codec/decode)
(def CT              tx/CT)
(def protocol-header  protocol/header)
(def protocol-version protocol/version)
