(ns sutra-ui.health.badge
  (:require [sutra-ui.ui.core :as ui]
            [sutra.i18n.core :as i18n]
            [sutra-ui.state.lineage-status :as lineage-status]))

(def ^:private status-intent
  {"passed" :success "warning" :warning "failed" :danger})

(def status-color
  {"passed"  "var(--c-success)"
   "warning" "var(--c-warning-accent)"
   "failed"  "var(--c-danger)"
   nil       "var(--c-muted-3)"})

(defn- status-label [status]
  (i18n/t (case status
            "passed"  :ui/health-status-passed
            "warning" :ui/health-status-warning
            "failed"  :ui/health-status-failed
            :ui/health-status-pending)))

(defonce ^:private asked? (atom false))

(defn- ensure-status! []
  (when (and (empty? (:nodes @lineage-status/lineage-status)) (not @asked?))
    (reset! asked? true)
    (lineage-status/refresh-lineage-status!)))

(defn- health-of [kind kw]
  (let [h (:health (lineage-status/node-status kind kw))]
    (when (pos? (or (:checks h) 0)) h)))

(defn health-dot
  [kind kw]
  (ensure-status!)
  (when-let [{:keys [status]} (health-of kind kw)]
    [ui/tooltip {:content (status-label status)}
     [:span {:class "sutra-health-dot"
             :style {:background (status-color status)}}]]))

(defn health-badge
  [kind kw on-click]
  (ensure-status!)
  (when-let [{:keys [status checks failed]} (health-of kind kw)]
    [ui/tooltip {:content (i18n/t :ui/health-badge-hint {:n checks})}
     [ui/tag {:class       "sutra-health-badge"
              :interactive (some? on-click)
              :minimal     true
              :intent      (status-intent status)
              :icon        [:span {:class "sutra-health-dot"
                                   :style {:background (status-color status)}}]
              :on-click    on-click}
      (str (i18n/t :ui/data-health) ": " (status-label status)
           (when (pos? (or failed 0)) (str " (" failed ")")))]]))
