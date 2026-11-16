(ns sutra-ui.apphost.widgets.metric
  (:require [sutra-ui.ui.display :as display]
            [reagent.core :as r]
            [sutra.capps.metric :as metric]
            [sutra.kit.coerce :refer [->kw]]
            [sutra-ui.apphost.client :as client]
            [sutra-ui.apphost.frame :as frame]))

(defn- iso [ms] (.toISOString (js/Date. ms)))

(defn- wire
  [{:keys [from-ms until-ms] :as read}]
  (cond-> (dissoc read :from-ms :until-ms)
    from-ms  (assoc :from (iso from-ms))
    until-ms (assoc :until (iso until-ms))))

(defn sparkline
  [_opts]
  (let [values (r/atom nil)
        asked  (atom nil)
        load!  (fn [f descriptor spec]
                 (frame/request!
                  f (assoc (frame/kept #(reset! values (metric/first-series-values %))
                                       #(reset! values []))
                           :ask [client/series! descriptor
                                 (wire (metric/series-read spec (js/Date.now)))])))]
    (fn [{:keys [descriptor spec] f :frame}]
      (let [{:keys [baseline position]} spec]
        (when (not= @asked spec)
          (reset! asked spec)
          (load! f descriptor spec))
        (let [vs (or @values [])]
          [display/sparkline {:points   (vec (map-indexed vector vs))
                              :baseline (metric/baseline-value baseline vs)
                              :w 120 :h 32 :pad 2 :stretch? true
                              :class    (str "capp-spark--"
                                             (name (or (->kw position) :stacked)))}])))))
