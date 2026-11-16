(ns ^:unit platform.json-wire-opt-in-test
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [sutra.serve.server :as server]))

(def ^:private wrap-format #'server/wrap-format)

(defn- answer [headers]
  ((wrap-format (fn [_] {:status 200 :body {:a 1}}))
   {:request-method :get :uri "/api/x" :headers headers}))

(defn- content-type [r] (str (get-in r [:headers "Content-Type"])))

(deftest a-client-that-names-no-version-keeps-transit
  (let [r (answer {"accept" "application/json"})]
    (is (str/starts-with? (content-type r) "application/transit+json"))))

(deftest a-client-that-names-the-version-gets-json
  (let [r (answer {"accept" "application/json" "sutra-api-version" "1.0.0"})]
    (is (str/starts-with? (content-type r) "application/json"))
    (is (contains? (json/read-str (let [b (:body r)] (if (string? b) b (slurp b)))) "a"))))

(deftest a-json-body-stays-unread-without-the-version
  (testing "the handler reads the body itself, as sutra.apps.http.session/app-body does"
    (let [seen (atom nil)
          body "{\"x\": 1.5}"]
      ((wrap-format (fn [req] (reset! seen [(:body-params req) (slurp (:body req))])
                      {:status 200 :body "ok"}))
       {:request-method :post :uri "/api/x"
        :headers {"content-type" "application/json"}
        :body (java.io.ByteArrayInputStream. (.getBytes body "UTF-8"))})
      (is (= [nil body] @seen)))))
