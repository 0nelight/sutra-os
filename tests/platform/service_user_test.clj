(ns ^:unit platform.service-user-test
  (:require [sutra.serve.errors :as errors]
            [clojure.test :refer [deftest is testing]]
            [sutra.access.http.auth :as auth]
            [sutra.apps.http.custom :as apps]
            [sutra.access.oauth :as oauth]
            [sutra.access.principals :as pr]
            [support.mem :as mem]))

(defn- client! [store grants]
  (oauth/register-client! store {:app (str "ri.app." (random-uuid)) :app-name :waechter
                              :grant-types grants :created-by "admin"}))

(deftest client-credentials-creates-its-own-service-user
  (let [store (mem/store)
        c  (client! store [:client-credentials])
        p  (pr/fetch store (:client-id c))]
    (is (= (:client-id c) (:service-user c)) "the service user is named like the client id")
    (is (some? p) "and exists in the same commit as the client")
    (is (pr/service-user? p))
    (is (nil? (:external-subject p)))
    (is (oauth/own-service-user? (oauth/fetch-client store (:client-id c))))))

(deftest authorization-code-creates-none
  (let [store (mem/store)
        c  (client! store [:authorization-code])]
    (is (nil? (:service-user c)))
    (is (nil? (pr/fetch store (:client-id c))) "a person signs in; there is nobody to create")))

(deftest no-other-sign-in
  (let [store (mem/store)
        {:keys [client-id]} (client! store [:client-credentials])]
    (testing "no password can be set on it"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"service user"
                            (pr/register-principal!
                             store {:username client-id :password "pw" :user-id "x"}))))
    (testing "and nobody registers one by hand"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"created with its OAuth client"
                            (pr/register-principal!
                             store {:username "von-hand" :auth-source "oauth-client"
                                 :user-id "y"}))))
    (testing "a password login opens no session"
      (is (nil? (auth/login! store client-id ""))))
    (testing "nor does an assertion that names it"
      (is (nil? (auth/establish-session! store {:provider :spnego :subject client-id
                                             :principal (pr/fetch store client-id)}))))))

(deftest mint-refuses-a-chosen-principal
  (let [r ((errors/wrap-exceptions apps/mint-client) {:path-params {:name "waechter"}
                             :body-params {:service-user "admin" :grant-types [:client-credentials]}
                             :principal {:username "admin"}
                             :sutra/store (mem/store)
                             :sutra/schema {:custom-applications
                                            {"ri.app.w" {:custom-application-name :waechter}}}
                             :sutra/host {:oauth {:register-client! oauth/register-client!}}})]
    (is (= 400 (:status r)) "there is no principal a client may be told to run as")))
