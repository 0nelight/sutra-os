(ns sutra-ui.portal-tray-smoke-test
  (:require [cljs.test :refer [deftest is testing]]
            [sutra.capps.portal :as portal]
            [sutra-ui.apps.launcher.core :as launcher]))

(def ^:private leitwarte {:id "a1" :name :leitwarte  :workspaces ["w1"]})
(def ^:private faktura   {:id "a2" :name :faktura    :workspaces []})
(def ^:private apps      [leitwarte faktura])

(deftest ohne-regal-bleibt-das-gitter-vollstaendig
  (testing "kein Arbeitsplatz erklärt — alles bleibt erreichbar wie bisher"
    (is (= apps (portal/tray apps nil)))
    (is (= apps (portal/tray apps [])))))

(deftest mit-regal-zeigt-das-gitter-nur-noch-das-unverstaute
  (let [shelves [{:id "w1" :name "Geschäftsführung"}]]
    (testing "was ein Regal trägt, verlässt das Gitter"
      (is (= [faktura] (portal/tray apps shelves))))
    (testing "trägt jedes Regal alles, ist das Gitter leer — und das ist das Ziel"
      (is (= [] (portal/tray [leitwarte] shelves))))))

(def ^:private shelves [{:id "w1" :name "Geschäftsführung"}
                        {:id "w2" :name "Hangar"}])

(deftest die-regale-einer-app-werden-benannt
  (testing "ein Regal"
    (is (= ["Geschäftsführung"] (launcher/shelves-of leitwarte shelves))))
  (testing "mehrere Regale, in der Reihenfolge des Servers"
    (is (= ["Hangar" "Geschäftsführung"]
           (launcher/shelves-of {:workspaces ["w2" "w1"]} shelves))))
  (testing "keins"
    (is (= [] (launcher/shelves-of faktura shelves))))
  (testing "ein Regal, das dieser Leser nicht betreten darf, zählt trotzdem"
    (is (= ["—"] (launcher/shelves-of {:workspaces ["w-fremd"]} shelves)))
    (is (seq (launcher/shelves-of {:workspaces ["w-fremd"]} shelves)))))
