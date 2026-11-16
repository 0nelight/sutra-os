(ns ^:unit platform.egress-webhook-auth-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.sources.egress :as egress]
            [org.httpkit.server :as http-kit]))

(defn- with-recorder [f]
  (let [seen (atom nil)
        srv  (http-kit/run-server (fn [req] (reset! seen req) {:status 201 :body "ok"})
                                  {:port 0 :ip "127.0.0.1" :legacy-return-value? false})]
    (try (f (http-kit/server-port srv) seen)
         (finally (http-kit/server-stop! srv)))))

(defn- spec [port auth wh]
  {:egress   [(str "127.0.0.1:" port)]
   :rest     {:base-url (str "http://127.0.0.1:" port) :auth auth}
   :webhooks {:senden wh}})

(deftest bearer-und-idempotenzschluessel
  (with-recorder
    (fn [port seen]
      (let [res (egress/invoke! (spec port {:mode :bearer :token "geheim"}
                                      {:method :post :path-template "/auftrag/{id}"
                                       :params [:id :betrag] :body-template {:betrag "{betrag}"}
                                       :idempotency-key "ueberweisung-{id}"})
                                {:webhook-id :senden :params {:id "u-7" :betrag 12.5M}})
            h   (:headers @seen)]
        (is (= 201 (:status res)))
        (is (= "/auftrag/u-7" (:uri @seen)))
        (is (= "Bearer geheim" (get h "authorization")))
        (testing "derselbe Auftrag traegt bei jeder Wiederholung denselben Schluessel"
          (is (= "ueberweisung-u-7" (get h "idempotency-key"))))))))

(deftest ohne-angabe-kein-header
  (with-recorder
    (fn [port seen]
      (egress/invoke! (spec port nil {:method :post :path-template "/x" :params [:id]})
                      {:webhook-id :senden :params {:id "1"}})
      (is (not (contains? (:headers @seen) "authorization")))
      (is (not (contains? (:headers @seen) "idempotency-key"))))))
