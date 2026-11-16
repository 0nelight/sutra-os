(ns sutra-ui.ui.remote
  (:require [sutra.kit.remote :as rd]
            [sutra.i18n.core :as i18n]
            [sutra-ui.ui.busy :as busy]
            [sutra-ui.ui.core :as ui]))

(defn show
  ([r value] (show r value nil))
  ([r value {:keys [loading empty failed class]}]
   (rd/fold r {:loading (fn [] [busy/block {:class class} (or loading (i18n/t :ui/loading))])
               :absent  (fn [] (if empty
                                 [ui/empty-state (assoc empty :class class)]
                                 [ui/problem (or failed (i18n/t :ui/capp-data-failed)) nil
                                  class]))
               :failed  (fn [why] [ui/problem (or failed (i18n/t :ui/capp-data-failed)) why
                                   class])
               :value   value})))
