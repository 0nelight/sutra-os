(ns ^:unit apps.consumer-error-surface-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.data.json :as json]
            [sutra.serve.errors :as errors]
            [sutra.apps.http.transfer :as transfer]))

(def ^:private json-req  {:request-method :post :uri "/api/x"
                          :headers {"accept" "application/json"}})
(def ^:private transit-req {:request-method :post :uri "/api/x"
                            :headers {"accept" "application/transit+json"}})

(defn- handle [req e]
  ((#'errors/wrap-exceptions (fn [_] (throw e))) req))

(deftest json-caller-can-read-the-reason
  (testing "a JSON caller gets REAL json — a body its .error can be read off"
    (let [r (handle json-req (ex-info "1 job(s) failed: :befund-parsen" {}))]
      (is (= 500 (:status r)))
      (is (= "application/json" (get-in r [:headers "Content-Type"]))
          "explicit content-type — this is what bypasses the transit-only muuntaja")
      (is (string? (:body r)) "an encoded body, not a map muuntaja would re-encode")
      (let [b (json/read-str (:body r))]
        (is (re-matches #"internal error \(incident inc-[0-9a-f]{16}\)" (get b "error"))
            "a fault tells a caller only its incident, the reason stays in the log")
        (is (= (get b "incident") (re-find #"inc-[0-9a-f]{16}" (get b "error")))))))

  (testing "a client-error and a policy refusal negotiate the same way"
    (let [bad (handle json-req (ex-info "kein :limit" {:sutra.kit.anomaly/category :incorrect}))
          pol (handle json-req (ex-info "main ist geschützt"
                                      {:sutra.kit.anomaly/category :forbidden}))]
      (is (= 400 (:status bad)))
      (is (= "kein :limit" (get (json/read-str (:body bad)) "error")))
      (is (= 403 (:status pol)))
      (is (= "main ist geschützt" (get (json/read-str (:body pol)) "error")))))

  (testing "the transit wire is UNCHANGED — a plain map muuntaja encodes"
    (let [r (handle transit-req (ex-info "boom" {}))]
      (is (= 500 (:status r)))
      (is (map? (:body r)) "still a map for the portal/CLI wire client")
      (is (string? (:incident (:body r)))))))

(deftest throwable-msg-folds-in-the-cause
  (let [msg #'transfer/throwable-msg]
    (testing "the CAUSE carries the actionable half and must not be dropped"
      (let [e (ex-info "1 job(s) failed: :befund-parsen" {}
                       (ex-info "Scalar Function with name substring_index does not exist!" {}))]
        (is (= (str "1 job(s) failed: :befund-parsen — "
                    "Scalar Function with name substring_index does not exist!")
               (msg e)))))

    (testing "no cause, or a cause that only repeats the message, adds nothing"
      (is (= "plain" (msg (ex-info "plain" {}))))
      (is (= "same" (msg (ex-info "same" {} (ex-info "same" {})))))
      (is (= "outer: inner" (msg (ex-info "outer: inner" {} (ex-info "inner" {}))))
          "a message that already quotes its cause is not doubled"))))
