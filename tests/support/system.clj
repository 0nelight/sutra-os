(ns support.system
  (:require [sutra.kit.system :as sys]
            [sutra.serve.server :as server]
            [sutra.store.access.scope :as ctx]
            [sutra.store.seat :as seat]))

(def ^:private current @#'sys/installed)

(defn ensure! []
  (when (empty? (:provides @current))
    (sys/install! (sys/instrument (server/system))))
  nil)

(defn ds
  (^javax.sql.DataSource [] (ds nil))
  (^javax.sql.DataSource [lane] ((sys/required (sys/process) :pg/ds) lane)))

(defn seat!
  [b]
  (ensure!)
  (swap! current assoc-in [:config "SUTRA_BRANCH"] b)
  nil)

(defn unseat!
  []
  (swap! current update :config dissoc "SUTRA_BRANCH")
  nil)

(defn with-provided* [m f]
  (ensure!)
  (let [prev @current]
    (swap! current update :provides merge m)
    (try (f) (finally (reset! current prev)))))

(defmacro with-provided
  [m & body]
  `(with-provided* ~m (fn [] ~@body)))

(defn provide!
  [k f]
  (ensure!)
  (swap! current #(if f (assoc-in % [:provides k] f) (update % :provides dissoc k)))
  nil)

(defn verbs
  []
  (ensure!)
  (sys/table (sys/process) :action/verbs))

(defn seated-schema
  []
  (seat/branch-schema (ctx/store (seat/seated-branch))))
