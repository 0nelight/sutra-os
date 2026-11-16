(ns ^:unit platform.json-wire-test
  (:require [clojure.test :refer [deftest is]]
            [sutra.foundation.contracts :as ct]))

(deftest a-json-action-call-decodes-to-the-transit-shape
  (let [json {:action "set-status" :args {:status "open"}}
        body (ct/decode-json ::ct/action-wire json)]
    (is (= :set-status (:action body)))
    (is (nil? (ct/http-check ::ct/action-wire body)))
    (is (some? (ct/http-check ::ct/action-wire json)) "undecoded, the contract refuses it")))
