(ns ^:unit platform.abort-rule-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.types.paths :as paths]
            [sutra.wire.abort :as abort]))

(deftest a-path-names-its-endpoint
  (is (= :my-application-aggregate
         (paths/endpoint-of "/api/my-application/ri.app.1/aggregate?scenario=s&v=3")))
  (is (= :my-application-aggregate
         (paths/endpoint-of "https://sutra.example/api/my-application/ri.app.1/aggregate")))
  (is (= :my-application-submit (paths/endpoint-of "/api/my-application/x/submit")))
  (is (nil? (paths/endpoint-of "/api/kein/solcher/weg/hier")))
  (testing "every endpoint is found from its own address"
    (doseq [[id tmpl] paths/paths
            :let [p (paths/path-for tmpl (into {} (for [s (re-seq #":([a-z-]+)" tmpl)]
                                                    [(keyword (second s)) "v1"])))
                  hit (paths/endpoint-of p)]]
      (is (= tmpl (paths/paths hit)) (str id " " p " -> " hit)))))

(deftest whether-a-request-may-be-asked-again
  (is (abort/reads? :post "/api/my-application/x/aggregate") "a POST that only reads")
  (is (abort/reads? "POST" "/api/my-application/x/function?v=1"))
  (is (not (abort/reads? :post "/api/my-application/x/submit")) "a submit writes")
  (is (abort/reads? :get "/api/kein/solcher/weg") "unknown: a GET reads")
  (is (not (abort/reads? :post "/api/kein/solcher/weg")) "unknown: a POST might write"))

(deftest what-follows-a-failed-fetch
  (testing "an answer with a status is an error, whatever the page did"
    (is (= :failed (abort/next-step {:status 500 :hidden? true :reads? true}))))
  (testing "a network failure while the page was visible is an error"
    (is (= :failed (abort/next-step {:error-name "TypeError" :hidden? false :reads? true}))))
  (testing "broken off: a read is asked again, a write is not"
    (is (= :retry (abort/next-step {:error-name "TypeError" :hidden? true :reads? true})))
    (is (= :retry (abort/next-step {:error-name "AbortError" :reads? true})))
    (is (= :aborted (abort/next-step {:error-name "TypeError" :hidden? true :reads? false}))))
  (testing "a read is asked again only so often"
    (is (= :aborted (abort/next-step {:error-name "AbortError" :reads? true
                                      :attempt abort/max-retries}))))
  (is (abort/aborted-error? (ex-info "x" {::abort/aborted true})))
  (is (not (abort/aborted-error? (ex-info "x" {:status 500})))))
