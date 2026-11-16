(ns ^:api platform.platform-experience-endpoint-test
  (:require [sutra.catalog.grants :as grants]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.access.principals :as pr]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(use-fixtures :once pod/with-mem-pod-auth)

(def ^:private png "data:image/png;base64,iVBORw0KGgo=")

(deftest oeffentlich-lesbar-und-nur-vom-admin-zu-aendern
  (let [admin-tok (-> (api/post! :login {:username grants/admin-username
                                  :password pr/default-admin-password})
                      :body :token)]
    (is (some? admin-tok) "admin bootstrap login")

    (testing "ohne Anmeldung lesbar — sonst kann sich das Anmeldefenster nicht benennen"
      (let [resp (api/get! :platform-experience)]
        (is (ok? resp))
        (is (nil? (get-in resp [:body :title])) "unkonfiguriert trägt keinen eigenen Titel")))

    (testing "der Schreibweg ist NICHT öffentlich, obwohl der Leseweg es ist"
      (is (= 401 (:status (api/post! :platform-experience-set {:title "Fremd"})))
          "ein anonymer Aufruf endet an der Tür; der Adminzwang dahinter hängt am admin-gated-Wrapper"))

    (tx/with-token admin-tok
      (testing "der Admin benennt die Plattform und hinterlegt EIN Logo"
        (let [resp (api/post! :platform-experience-set {:title "Sutra-OS" :logo {:medium png}})]
          (is (ok? resp))
          (is (= "Sutra-OS" (get-in resp [:body :title])))
          (is (= png (get-in resp [:body :logo :small])) "klein borgt sich mittel")
          (is (nil? (get-in resp [:body :logo :favicon])) "das Favicon erbt nichts")
          (is (= [:medium] (get-in resp [:body :uploaded])))))

      (testing "was kein Bild ist, kommt nicht in die Kopfzeile jedes Nutzers"
        (is (= 400
               (:status (api/post! :platform-experience-set
                                   {:logo {:large "https://example.org/logo.png"}})))))

      (testing "ein leerer Titel gibt den ausgelieferten Namen zurück"
        (is (nil? (get-in (api/post! :platform-experience-set {:title ""}) [:body :title])))))

    (testing "was der Admin setzt, sieht auch der nicht angemeldete Leser"
      (tx/with-token admin-tok (api/post! :platform-experience-set {:title "Sutra-OS"}))
      (is (= "Sutra-OS" (get-in (api/get! :platform-experience) [:body :title]))))))
