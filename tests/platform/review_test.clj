(ns ^:unit platform.review-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.types.review :as review]))

(def ^:private typ
  {:project "lager" :description "Artikel"
   :attributes [{:attribute-name :nr :type :string}
                {:attribute-name :name :type :string :description "kurz"}]
   :interfaces #{:pruefbar}})

(deftest zuwachs-und-bruch
  (testing "anlegen wächst, entfernen bricht"
    (is (= :accretion (review/change-kind "added" nil typ)))
    (is (= :breaking (review/change-kind "removed" typ nil))))
  (testing "ein neues Attribut, ein neues Interface, eine neue Beschreibung: Zuwachs"
    (is (= :accretion (review/change-kind "modified" typ
                                          (update typ :attributes conj
                                                  {:attribute-name :preis :type :decimal}))))
    (is (= :accretion (review/change-kind "modified" typ (update typ :interfaces conj :zaehlbar))))
    (is (= :accretion (review/change-kind "modified" typ (assoc typ :description "Ware"))))
    (is (= :accretion
           (review/change-kind "modified" typ (assoc-in typ [:attributes 1 :description] "lang")))
        "kosmetisch auch in einem benannten Element"))
  (testing "ein Attribut weg, ein Typ verengt, ein Interface weg, ein anderes Projekt: Bruch"
    (is (= :breaking (review/change-kind "modified" typ (update typ :attributes pop))))
    (is (= :breaking
           (review/change-kind "modified" typ (assoc-in typ [:attributes 0 :type] :long))))
    (is (= :breaking (review/change-kind "modified" typ (assoc typ :interfaces #{}))))
    (is (= :breaking (review/change-kind "modified" typ (assoc typ :project "einkauf")))))
  (testing "ohne Basis ist eine Änderung ein Bruch"
    (is (= :breaking (review/change-kind "modified" nil typ)))))

(defn- task [kind project]
  {:task "object-types/artikel" :section "object-types" :project project :kind kind})

(defn- ctx [author owned] {:author author :author-owns? (comp boolean owned)})

(defn- ctx-editor [author edited] {:author author :author-edits? (comp boolean edited)})

(deftest vorgabe-wie-counterpart-ein-bearbeiter-stimmt-zu
  (let [rs review/default-rules]
    (testing "wer die Ressource bearbeiten darf, erfuellt die Vorgabe selbst, auch bei einem Bruch"
      (is (empty? (review/requires rs (task :breaking "lager") (ctx-editor "anna" #{"lager"})))))
    (testing "ohne Bearbeitungsrecht wartet auch ein Zuwachs"
      (is (seq (review/requires rs (task :accretion "lager") (ctx-editor "ben" #{"einkauf"})))))
    (testing "ein anderer Bearbeiter genuegt, ein Eigentuemer ist nicht noetig"
      (let [t (assoc (task :breaking "lager") :requires
                     (review/requires rs (task :breaking "lager") (ctx-editor "ben" #{"einkauf"})))]
        (is (= [{:when [:not-editor] :by [:editor] :count 1}] (:requires t)))
        (is (seq (review/unmet t [] {:author "ben"})))
        (is (seq (review/unmet t [{:who "carl" :editor? false}] {:author "ben"})))
        (is (empty? (review/unmet t [{:who "dora" :editor? true}] {:author "ben"})))
        (is (empty? (review/unmet t [{:who "anna" :owner? true}] {:author "ben"})) "Eigentuemer auch")
        (is (not (review/eligible? t {:who "carl"} {:author "ben"})))))
    (testing "eine Ressource ohne Projekt gehoert allen"
      (is (empty? (review/requires rs (task :breaking nil) (ctx-editor "ben" #{})))))))

(deftest eigene-regel-des-projekts
  (let [rule (review/custom-rule {:reviewers [{:user "dora"} {:group "pruefer"}] :count 2})
        t    (assoc (task :accretion "lager") :requires [rule] :contributors ["carl"])
        c    {:author "anna"}]
    (testing "Bearbeiter sind ausgeschlossen, wenn das Projekt es nicht erlaubt"
      (is (not (review/eligible? t {:who "carl" :groups #{"pruefer"}} c)))
      (is (not (review/eligible? t {:who "anna" :groups #{"pruefer"}} c))))
    (testing "nur Genannte und Gruppenmitglieder, und so viele wie verlangt"
      (is (not (review/eligible? t {:who "emil" :editor? true} c)))
      (is (seq (review/unmet t [{:who "dora"}] c)))
      (is (empty? (review/unmet t [{:who "dora"} {:who "fritz" :groups #{"pruefer"}}] c))))
    (testing "die Regel laesst sich zurueck lesen"
      (is (= {:reviewers [{:user "dora"} {:group "pruefer"}] :count 2 :contributors? false}
             (review/custom-of [rule])))
      (is (nil? (review/custom-of review/default-rules))))
    (testing "ohne Namen genuegt ein Bearbeiter"
      (is (= {:reviewers [] :count 1 :contributors? true}
             (review/custom-of [(review/custom-rule {:contributors? true})]))))
    (testing "die Regel besteht die Pruefung"
      (is (nil? (review/problems [rule]))))))

(deftest vier-augen-ist-ausdruecklich
  (let [rs  [{:when [:always] :by [:other] :count 2}]
        t   (assoc (task :accretion "lager")
                   :requires (review/requires rs (task :accretion "lager") (ctx "anna" #{"lager"})))
        c   {:author "anna"}]
    (is (seq (review/unmet t [{:who "anna" :owner? true} {:who "ben"}] c))
        "der Autor zählt nicht, auch als Eigentümer")
    (is (empty? (review/unmet t [{:who "ben"} {:who "carl"}] c)))
    (is (not (review/eligible? t {:who "anna" :owner? true} c)))))

(deftest regeln-mit-argumenten
  (let [rs [{:when [[:section :actions] :breaking] :by [[:principal "revision"]]}]]
    (is (empty? (review/requires rs (task :breaking "lager") (ctx "a" #{}))))
    (let [t (assoc (task :breaking "lager") :section "actions")
          t (assoc t :requires (review/requires rs t (ctx "a" #{})))]
      (is (= 1 (count (:requires t))))
      (is (empty? (review/unmet t [{:who "revision"}] {:author "a"}))))))

(deftest unbekannte-begriffe-werden-benannt
  (is (nil? (review/problems review/default-rules)))
  (is (seq (review/problems [{:when [:gibtsnicht] :by [:owner]}])))
  (is (seq (review/problems [{:when [:always] :by []}])))
  (is (seq (review/problems [{:when [:always] :by [:owner] :count 0}])))
  (is (thrown? clojure.lang.ExceptionInfo
               (review/requires [{:when [:gibtsnicht] :by [:owner]}] (task :breaking "x")
                                (ctx "a" #{})))))
