(ns sutra.automation.activity
  (:require [sutra.automation.runs :as autos]
            [sutra.notifications.api :as notifications]
            [sutra.types.opid :as opid]))

(set! *warn-on-reflection* true)

(def auto-mute-window 30)

(def auto-mute-share 0.8)

(defn auto-mute?
  [runs]
  (let [last-n (take auto-mute-window runs)]
    (and (= auto-mute-window (count last-n))
         (>= (count (filter #(= "error" (:status %)) last-n))
             (* auto-mute-share auto-mute-window)))))

(defn run-category
  [previous {:keys [status applied]}]
  (cond
    (= "error" status)                                         :errors
    (and (= "error" (:status previous)) (not= "error" status)) :recovered
    (pos? (long (or applied 0)))                               :triggered))

(defn config-events
  [before after]
  (cond-> []
    (and before (not= (boolean (:paused before)) (boolean (:paused after))))
    (conj (if (:paused after) :paused :resumed))
    (and before (or (not= (:when-spec before) (:when-spec after))
                    (not= (:effect-spec before) (:effect-spec after))))
    (conj :condition-edited)))

(def ^:private words
  {:triggered        "hat ausgelöst"
   :recovered        "läuft wieder fehlerfrei"
   :errors           "ist fehlgeschlagen"
   :muted            "wurde stummgeschaltet"
   :auto-muted       "hat sich stummgeschaltet: zu viele Fehlschläge"
   :unmuted          "ist nicht mehr stumm"
   :paused           "wurde pausiert"
   :resumed          "läuft wieder"
   :condition-edited "wurde geändert"
   :subscribed       "hat einen neuen Abonnenten"
   :unsubscribed     "hat einen Abonnenten weniger"})

(defn kind
  [event]
  (case event
    :triggered :automation-triggered
    :recovered :automation-recovered
    :errors    :automation-errors
    :automation-info))

(defn notice
  [{:keys [id name]} event detail]
  {:source        (kind event)
   :automation-id id
   :heading       (str "Automation " name " " (words event))
   :message       detail
   :link-url      (str "/automations/" id)
   :link-label    "Automation öffnen"})

(defn tell!
  [store branch auto event detail]
  (when-let [subs (seq (autos/subscribers store branch (:id auto)))]
    (try (notifications/send! store branch subs (notice auto event detail))
         (catch InterruptedException e (throw e))
         (catch Exception t
           (println "[automation] activity notification failed:" (ex-message t))))))

(defn after-run!
  [store branch auto result]
  (let [[latest previous] (autos/list-runs store branch (:id auto) 2)
        runs     (when (:auto-mute auto) (autos/list-runs store branch (:id auto)
                                                          auto-mute-window))]
    (when-let [c (run-category previous (or latest result))]
      (tell! store branch auto c (:detail (or latest result))))
    (when (and (not (:muted auto)) (auto-mute? runs))
      (autos/set-muted! store branch (:id auto) true "auto-mute")
      (tell! store branch auto :auto-muted
             (str "Alle Effekte schlugen in mindestens 80 % der letzten "
                  auto-mute-window " Läufe fehl.")))))

(defn mute!
  [store branch auto muted? who]
  (autos/set-muted! store branch (:id auto) muted? who)
  (tell! store branch auto (if muted? :muted :unmuted) (str "von " who)))

(defn subscribe!
  [store branch auto username subscribe?]
  (if subscribe?
    (autos/subscribe! store branch (:id auto) username)
    (autos/unsubscribe! store branch (:id auto) username))
  (tell! store branch auto (if subscribe? :subscribed :unsubscribed) (str username)))

(defn saved!
  [store branch r]
  (let [id     (or (:id r) (opid/name-id :automation (:name r)))
        before (autos/get-one store branch id)
        after  (autos/upsert! store branch (assoc r :id id))]
    (doseq [e (config-events before after)]
      (tell! store branch after e nil))
    after))
