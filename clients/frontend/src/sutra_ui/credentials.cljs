(ns sutra-ui.credentials
  (:require [sutra.i18n.core :as i18n]
            [sutra-ui.api :as api]))

(defn seal
  ([cred-map] (seal cred-map {}))
  ([cred-map q]
   (if-not (and js/window.isSecureContext (some-> js/crypto .-subtle))
     (js/Promise.reject
      (js/Error. (i18n/t :ui/credentials-need-https {:origin (.-origin js/location)})))
     (-> (api/get! :egress-pubkey q)
         (.then (fn [resp]
                  (let [raw (js/Uint8Array.from (js/atob (:public-key resp))
                                                (fn [c _] (.charCodeAt c 0)))]
                    (js/crypto.subtle.importKey
                     "spki" raw #js {:name "RSA-OAEP" :hash "SHA-256"} false
                     #js ["encrypt"]))))
         (.then (fn [k]
                  (js/crypto.subtle.encrypt #js {:name "RSA-OAEP"} k
                                            (.encode (js/TextEncoder.) (pr-str cred-map)))))
         (.then (fn [ct]
                  (js/btoa (.apply (.-fromCharCode js/String) nil (js/Uint8Array. ct)))))))))
