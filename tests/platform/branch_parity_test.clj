(ns ^:unit platform.branch-parity-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [sutra.branching.landing :as landing]
            [sutra.notifications.mail :as mail]
            [sutra.catalog.branch-policy :as policy]
            [sutra.types.branch-kinds :as kinds]))

(deftest schutz-je-ressource
  (let [p {:resources #{"a"} :released #{"b"} :projects #{"p"}}]
    (testing "genannt, oder im Projekt das Neues schuetzt"
      (is (policy/protected-row? p {:rid "a"}))
      (is (policy/protected-row? p {:rid "c" :project "p" :status "modified"})))
    (testing "wer freigegeben wurde, bleibt es"
      (is (not (policy/protected-row? p {:rid "b" :project "p" :status "modified"}))))
    (testing "das Anlegen selbst ist frei, erst danach greift der Schutz"
      (is (not (policy/protected-row? p {:rid "c" :project "p" :status "added"}))))
    (testing "ausserhalb bleibt alles offen"
      (is (not (policy/protected-row? p {:rid "c" :project "q" :status "modified"}))))
    (testing "eine Marking-Entfernung ist immer geschuetzt"
      (is (policy/protected-row? p {:rid "c" :expands-access true})))))

(deftest projektregel-nur-fuer-geschuetztes
  (let [rule     {:when [:always] :by [:owner]}
        policies {"p" [rule]}
        schema   {:projects {"p" {:project-name :p}}}]
    (is (= [rule] (policy/applicable-rules [] policies schema "p" true)))
    (is (= [] (policy/applicable-rules [] policies schema "p" false)))))

(deftest aufbewahrung-je-space
  (let [gov {"*"           {:retention-days 40}
             "space/werk"  {:retention-days 10 :protected 1}
             "z"           {:space "werk"}
             "y"           {:space "werk" :retention-days 3}
             "x"           {}}]
    (is (= 40 (:retention-days (policy/effective gov "x"))))
    (is (= 10 (:retention-days (policy/effective gov "z"))) "der Space schlaegt die Vorgabe")
    (is (= 3 (:retention-days (policy/effective gov "y"))) "der Zweig schlaegt den Space")
    (is (false? (:protected (policy/effective gov "z"))) "ein Space setzt nur die Fristen")))

(deftest maschinenzweige
  (is (every? kinds/machine-branch? ["staging/u/main" "scenario/a/b" "run/x/1"]))
  (is (not-any? kinds/machine-branch? ["main" "feature/x" "sketch/a"])))

(deftest was-nach-dem-merge-gebaut-wird
  (let [world {:transforms   {"t1" {:transform-name :t1}
                              "t2" {:transform-name :t2 :inputs ["ds1"]}}
               :datasets     {"ds1" {:dataset-name :ds1}}
               :object-types {}}
        scope (landing/build-scope world [{:rid "t1" :section "transforms"}
                                          {:rid "x"  :section "action-types"}])]
    (testing "nur Baubares, nach Namen"
      (is (= ["t1"] (:modified scope))))
    (testing "Betroffenes enthaelt das Geaenderte"
      (is (some #{"t1"} (:affected scope))))))

(deftest mail
  (testing "ein Zweigname kann keinen Kopf einschleusen"
    (let [m (mail/message {:from "s@x" :to "a@x" :subject "Branch a\r\nBcc: b@x" :body "hi"})]
      (is (not (re-find #"(?m)^Bcc:" m)))
      (is (str/includes? m "Subject: =?UTF-8?B?"))))
  (testing "nur gueltige Adressen"
    (is (= "anna@x.example" (mail/address "anna" nil "x.example")))
    (is (= "a@b.c" (mail/address "a@b.c" nil "x.example")))
    (is (nil? (mail/address "a b" nil "x.example")))
    (is (nil? (mail/address "a>\r\nRCPT" nil "x.example")))
    (is (= "anna@firma.example" (mail/address "anna" "anna@firma.example" "x.example")))))
