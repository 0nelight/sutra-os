(ns sutra-ui.apphost.toast
  (:require [sutra-ui.ui.core :as ui]))

(defn toast!
  ([msg] (toast! msg nil))
  ([msg {:keys [error?]}]
   (ui/show! (ui/default-toaster)
             {:message msg
              :intent  (if error? :danger :success)
              :timeout (if error? 8000 4000)})))
