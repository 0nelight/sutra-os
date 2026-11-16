(ns ^:unit platform.synonyms-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.parser.synonyms :as syn]))

(def ^:private names
  [:wartungsauftrag :kundenauftrag :auftragsbestaetigung :geschäftspartner :mitarbeiter :zeichnung
   :zeitbuchung])

(defn- best [w & [known]] (:name (first (syn/candidates w names (or known {})))))

(deftest what-a-project-may-mean
  (is (= :geschäftspartner (best :geschaeftspartner)) "spelling")
  (is (= :zeichnung (best :zeichnungen)) "plural")
  (is (= :wartungsauftrag (best :wartungsauftraege)) "umlaut plural")
  (is (= :mitarbeiter (best :mitarbeitern)))
  (is (= :geschäftspartner (best :partner)) "part of a compound")
  (is (= :zeitbuchung (best :zietbuchung)) "typo")
  (is (= :wartungsauftrag (best :auftrag {:auftrag :wartungsauftrag}))
      "an alias another project adopted outranks every guess")
  (is (empty? (syn/candidates :heizkreis names {}))))

(deftest the-endpoint-says-create-or-use
  (require 'sutra.authoring.http.oms)
  (let [suggest @(resolve 'sutra.authoring.http.oms/name-suggestions)
        schema  {:projects     {"p1" {:project-name :controlling}
                                "p2" {:project-name :konstruktion :synonyms {:ba :wartungsauftrag}}}
                 :object-types {"o1" {:object-type-name :wartungsauftrag :project "p1"
                                      :plural "Wartungsaufträge"}
                                "o2" {:object-type-name :zeichnung :project "p2"
                                      :plural "Zeichnungen"}}
                 :actions      {:auftrag-anlegen {:project :controlling}}}]
    (testing "a name close to one in another project: adopt or use it"
      (let [r (suggest schema "wartungsauftraege" nil :konstruktion)]
        (is (= :close (:verdict r)))
        (is (= [{:name :wartungsauftrag :score 0.9 :why :plural :project :controlling}]
               (:candidates r)))))
    (testing "the name itself, in another project: use it — no import — do not create"
      (let [r (suggest schema "wartungsauftrag" nil :konstruktion)]
        (is (= :use (:verdict r)))
        (is (= [[:object-type :wartungsauftrag "o1" :controlling]]
               (mapv (juxt :kind :name :rid :project) (:exact r))))))
    (testing "an alias a project adopted names the type it stands for"
      (is (= :wartungsauftrag (:name (first (:exact (suggest schema "ba" nil :konstruktion)))))))
    (testing "in its own project a name exists; nowhere, it is new"
      (is (= :exists (:verdict (suggest schema "auftrag-anlegen" :action :controlling))))
      (is (= :create (:verdict (suggest schema "pruefbericht" nil :konstruktion)))))))
