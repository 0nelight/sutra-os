(ns ^:unit platform.workspace-rights-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.catalog.grants :as grants]
            [sutra.apps.http.scope :as app-scope]))

(def ^:private rights #'app-scope/workspace-rights)

(def ^:private schema
  {:projects   {"ri.p.ctrl" {:project-name :controlling}}
   :workspaces {:gf {:workspace-name :gf :project "ri.p.ctrl"}}})

(def ^:private gf (get-in schema [:workspaces :gf]))

(defn- als [ops f]
  (f {:project-operations ops}))

(deftest ein-voller-login-hat-alle-drei
  (testing "grants/all ist der Vollzugriff — das macht hier einen Admin aus"
    (is (= {:view true :edit true :promote true}
           (als grants/all #(rights % schema gf))))))

(deftest die-drei-rechte-kommen-einzeln
  (testing "Betrachter: :read allein sieht, ändert nicht"
    (is (= {:view true :edit false :promote false}
           (als {"ri.p.ctrl" #{:read}} #(rights % schema gf)))))
  (testing "Bearbeiter: :update ändert, bewirbt aber nicht"
    (is (= {:view false :edit true :promote false}
           (als {"ri.p.ctrl" #{:update}} #(rights % schema gf)))))
  (testing "Administrator: :manage-permissions bewirbt"
    (is (= true (:promote (als {"ri.p.ctrl" #{:read :update :manage-permissions}}
                               #(rights % schema gf))))))
  (testing "ein Entdecker sieht das Regal, mehr nicht"
    (is (= {:view true :edit false :promote false}
           (als {"ri.p.ctrl" #{:discover}} #(rights % schema gf)))))
  (testing "eine Freigabe auf einem FREMDEN Project trägt hier nichts"
    (is (= {:view false :edit false :promote false}
           (als {"ri.p.andere" #{:update :manage-permissions}} #(rights % schema gf))))))

(deftest die-standardrollen-tragen-genau-diese-operationen
  (testing "Bearbeiter-Rolle bringt :update, aber nicht :manage-permissions"
    (let [ops (grants/role-operations :ontology-editor)]
      (is (contains? ops :update))
      (is (not (contains? ops :manage-permissions)))))
  (testing "erst der Owner darf bewerben"
    (is (contains? (grants/role-operations :ontology-owner) :manage-permissions))))

(deftest anlegen-fragt-das-project-nicht-das-noch-leere-regal
  (testing "beim Anlegen gibt es keine Ressource, die eine Freigabe tragen könnte
            — gefragt ist das Project, in das der Arbeitsplatz gelegt wird"
    (is (= {:view false :edit true :promote false}
           (als {"ri.p.ctrl" #{:update}}
                #(rights % schema {:project "ri.p.ctrl"})))))
  (testing "ohne genanntes Project trägt niemand ausser dem Vollzugriff ein Recht"
    (is (= {:view false :edit false :promote false}
           (als {"ri.p.ctrl" #{:update}} #(rights % schema {}))))
    (is (= {:view true :edit true :promote true}
           (als grants/all #(rights % schema {}))))))
