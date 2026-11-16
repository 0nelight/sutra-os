(ns ^:unit platform.workspace-scope-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.apps.http.workspaces :as workspaces]
            [sutra.access.http.auth :as auth]))

(def ^:private scope #'workspaces/workspace-scope)
(def ^:private blocked? #'auth/consumer-blocked?)

(def ^:private ticket "ri.sutra.main.object-type.ticket")
(def ^:private kunde "ri.sutra.main.object-type.kunde")
(def ^:private lohn "ri.sutra.main.object-type.lohn")
(def ^:private geheim "ri.sutra.main.object-type.geheim")

(def ^:private schema
  {:projects     {"ri.p.board" {:project-name :board}
                  "ri.p.hr"    {:project-name :hr}}
   :object-types {ticket {:object-type-name :ticket :project "ri.p.board" :plural "Tickets"}
                  kunde  {:object-type-name :kunde :project "ri.p.board" :plural "Kunden"}
                  lohn   {:object-type-name :lohn :project "ri.p.hr" :plural "Löhne"}
                  geheim {:object-type-name :geheim :project "ri.p.hr" :plural "Geheime"}}
   :applications {"ri.app.board" {:application-name :board :published "1" :releases {"1" {}}
                                  :project "ri.p.board" :object-type ticket}
                  "ri.app.hr"    {:application-name :hr :published "1" :releases {"1" {}}
                                  :project "ri.p.hr" :object-type lohn}}
   :workspaces   {"ri.ws.w" {:workspace-name :w :project "ri.p.board"
                             :apps ["ri.app.board" "ri.app.hr"]
                             :home [{:items [{:kind :object-type :object-type kunde}
                                             {:kind :object-type :object-type geheim}]}]}}})

(defn- als [ops f]
  (f {:project-operations ops}))

(deftest nur-was-die-module-schon-erreichen
  (let [w (get-in schema [:workspaces "ri.ws.w"])
        s (als {"ri.p.board" #{:read}} #(scope % schema w))]
    (testing "die Typen einer freigegebenen App und ein gezeigter lesbarer Typ"
      (is (contains? (:object-type s) ticket))
      (is (contains? (:object-type s) kunde)))
    (testing "eine App ohne Freigabe traegt nichts bei, auch wenn der Arbeitsplatz sie nennt"
      (is (not (contains? (:object-type s) lohn))))
    (testing "ein gezeigter Typ ohne :read auf seinem Project bleibt draussen"
      (is (not (contains? (:object-type s) geheim))))
    (testing "blosses Entdecken reicht fuer einen gezeigten Typ nicht"
      (is (not (contains? (:object-type (als {"ri.p.board" #{:discover}} #(scope % schema w)))
                          kunde))))))

(deftest der-konsument-erreicht-die-arbeitsplatz-lesungen
  (is (not (blocked? "consumer" "/api/my-workspace/ri.ws.w/search")))
  (is (not (blocked? "consumer" "/api/my-workspace/ri.ws.w/around")))
  (is (blocked? "consumer" "/api/instances/search")
      "die Builder-Suche bleibt dem Konsumenten verschlossen"))
