(ns sutra-ui.apphost.widgets.action
  (:require [sutra-ui.apphost.frame :as fr]
            [sutra-ui.ui.core :as ui]
            [reagent.core :as r]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.capps.intent :as ci]
            [sutra.capps.leaf-props :as lp]
            [sutra.capps.table :as tbl]
            [sutra.capps.variables :as vars :refer [deref*]]
            [sutra.i18n.core :as i18n]
            [sutra.types.submission :as csub]
            [sutra-ui.object.card :as oc]
            [sutra-ui.apphost.action-form :refer
             [action-form action-table order-list]]
            [sutra-ui.apphost.widgets.rtf :refer [rtf-text]]
            [sutra-ui.apphost.toast :as toast]
            [sutra-ui.apphost.events :refer [on-click-fn]]
            [sutra-ui.apphost.state :as state]
            [sutra-ui.apphost.page-ctx :refer [ev-ctx]]
            [sutra-ui.apphost.widgets.table :refer [with-rows-shown]]))

(defn action-button
  [descriptor {:keys [frame label busy-label params disabled? class reason intent] :as opts}]
  (r/with-let [own (when-not frame (fr/capp-frame))
               k   (random-uuid)]
    (let [f        (or frame own)
          {b? :busy? e :error} (fr/deed f k)
          btn [ui/button
               (merge
                (ci/btn-props intent)
                {:class    class
                 :loading  b?
                 :disabled (or disabled? (boolean reason))
                 :on-click (fn [_]
                             (when-not (:busy? (fr/deed f k))
                               (fr/deed! f k (fr/submit-request descriptor params opts))))})
               (if b? (or busy-label "…") label)]
          note (or e reason)]
      [:div {:class (cond-> "capp-action" note (str " capp-action--note"))}
       [ui/tooltip {:content reason} btn]
       (when note [:div {:class (if e "capp-action__error" "capp-action__hint")} note])])
    (finally
      (fr/send! (or frame own) [:capp/drop-deed k])
      (when own (fr/leave! own)))))

(defn action-label
  [descriptor action]
  (when-let [an (->kw (or action (:default-action descriptor)))]
    (or (some (fn [info]
                (some #(when (= an (->kw (:action %))) (not-empty (str (:label %))))
                      (:actions info)))
              (vals (:standard-views descriptor)))
        (tbl/humanize an))))

(defn- action-intent
  [descriptor action authored]
  (ci/resolve-intent authored
                     (get (:action-intents descriptor)
                          (->kw (or action (:default-action descriptor))))
                     :primary))

(defn- rows-type-label
  [desc rows]
  (when-let [t (some-> (first rows) :object-type-ref)]
    (:plural (oc/descriptor-meta desc t))))

(defn- write-output!
  [output data]
  (when (vars/handle? output)
    (reset! output (vec (:created (meta data))))))

(defn- error-sink
  [handles error-var label]
  (fn [msg]
    (if-let [h (and error-var (get handles error-var))]
      (reset! h msg)
      (js/console.warn label msg))))

(defn- action-done
  [{:keys [descriptor refresh overlays]} modal action label output done-fn]
  (fn [data]
    (when (and refresh data) (refresh data))
    (toast/toast! (i18n/t :ui/capp-action-done
                          {:name (or (action-label descriptor action) label)}))
    (write-output! output data)
    (when done-fn (done-fn (first (:created (meta data)))))
    (when (and overlays modal) (state/close! overlays modal))))

(defn- save-label
  [label desc rows k]
  (or label (some->> (rows-type-label desc rows) (hash-map :name) (i18n/t k))))

(defn deref-vals [m] (into {} (map (fn [[k v]] [k (deref* v)])) (or m {})))

(defn- action-button-widget
  [{:keys [label busy-label params action scenario disabled? class error-var on-done output
           intent]} {ctx :app}]
  (let [{:keys [descriptor handles modal]} ctx
        done-fn  (on-click-fn on-done (ev-ctx ctx))
        specs    (get (:action-params descriptor)
                      (or (->kw action) (:default-action descriptor)))
        gate     (csub/submit-gate specs (deref-vals params))]
    [action-button descriptor
     {:frame (:frame ctx)
      :label (or label (action-label descriptor action)) :busy-label busy-label
      :params (or params {})
      :action action
      :scenario (if (satisfies? IDeref scenario) @scenario scenario)
      :class class
      :intent (action-intent descriptor action intent)
      :disabled? (or disabled? (:blocked? gate))
      :reason (:reason gate)
      :on-ok    (action-done ctx modal action label output done-fn)
      :on-error (error-sink handles error-var "app/action-button error:")}]))

(defn- own-object-params
  [descriptor defaults]
  (when-let [own (some-> (:object descriptor) :id str)]
    (into #{} (keep (fn [[p v]] (when (= own (some-> v str)) p))) defaults)))

(defn action-form-widget
  [{:keys [action defaults options label busy-label class error-var
           cancel-label on-cancel on-done scenario output intent]} {ctx :app}]
  (let [{:keys [descriptor handles modal]} ctx
        done-fn   (on-click-fn on-done (ev-ctx ctx))
        cancel-fn (on-click-fn on-cancel (ev-ctx ctx))
        nm        (or (->kw action) (:default-action descriptor))
        dv        (deref-vals defaults)]
    [action-form descriptor
     {:action       action
      :specs        (get (:action-params descriptor) nm)
      :sections     (get (:action-forms descriptor) nm)
      :controls     {:rtf rtf-text}
      :scenario     (deref* scenario)
      :defaults     dv
      :hidden       (own-object-params descriptor dv)
      :options      (into {} (map (fn [[k v]] [k (lp/as-rows v)])) (or options {}))
      :object       (:object descriptor)
      :object-type  (:object-of-type descriptor)
      :label        (or label (action-label descriptor nm))
      :busy-label   busy-label
      :cancel-label cancel-label
      :on-cancel    (when cancel-fn #(cancel-fn nil))
      :class        class
      :intent       (action-intent descriptor nm intent)
      :on-ok        (action-done ctx modal nm label output done-fn)
      :on-error     (error-sink handles error-var "app/action-form error:")}]))

(defn action-table-widget [{:keys [error-var scenario on-done] :as cfg} {ctx :app :as env}]
  (let [{:keys [descriptor refresh handles]} ctx
        done-fn (on-click-fn on-done (ev-ctx ctx))
        rows    (lp/as-rows (:rows cfg))]
    [action-table descriptor
     (-> (with-rows-shown cfg env)
         (assoc :scenario (deref* scenario) :frame (:frame ctx))
         (dissoc :error-var :on-done)
         (update :submit-label save-label descriptor rows :ui/capp-save-objects)
         (assoc :on-ok    (fn [data]
                            (when (and refresh data) (refresh data))
                            (when-let [f (:on-ok-local cfg)] (f))
                            (when done-fn (done-fn nil)))
                :on-error (error-sink handles error-var "app/action-table:")))]))

(defn- order-list-widget [{:keys [error-var] :as cfg} {ctx :app}]
  (let [{:keys [descriptor refresh handles]} ctx
        items (lp/as-rows (:items cfg))]
    [order-list descriptor
     (-> cfg
         (update :scenario deref*)
         (assoc :items items :frame (:frame ctx))
         (update :submit-label save-label descriptor items :ui/capp-save-order-of)
         (dissoc :error-var)
         (assoc :on-ok    (fn [data] (when (and refresh data) (refresh data)))
                :on-error (error-sink handles error-var "app/order-list:")))]))

(def widgets
  {:action-button action-button-widget
   :action-form   action-form-widget
   :action-table  action-table-widget
   :order-list    order-list-widget})
