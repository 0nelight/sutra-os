(ns ^:api ontology.action-returns-edits-test
  (:require [support.wire :refer [ok?]]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.parser.sugar :as sugar]
            [sutra.wire.api :as api]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-pod)

(def ^:private schema
  {:schema
   {:projects {:retedits {}}
    :objects {:vorgang {:project :retedits :symbol "📁"
                        :primary-key :nr :attributes [[:nr :string] [:titel :string]]
                        :plural "Vorgänge"}}
    :actions
    {:vorgang-anlegen
     {:name   :vorgang-anlegen
      :params {:nr {:name "Nr" :type :string :required true}
               :titel {:name "Titel" :type :string :required true}}
      :rules  [{:op :create :object-type :vorgang :id :nr
                :values {:nr :nr :titel :titel}}]
      :edits  [{:object-type :vorgang}]}
     :vorgang-umbenennen
     {:name   :vorgang-umbenennen
      :params {:vorgang {:name "Vorgang" :type :vorgang :required true}
               :titel   {:name "Titel" :type :string :required true}}
      :rules  [{:op :update :object-type :vorgang :target :vorgang
                :values {:titel :titel}}]
      :edits  [{:object-type :vorgang}]}}}})

(deftest eine-action-nennt-was-sie-angefasst-hat
  (testing "install"
    (is (ok? (api/post! :install (update schema :schema sugar/lower-schema))) "install stages")
    (is (ok? (api/post! :staging)) "commit schema"))

  (testing "ohne Nachfrage bleibt die Antwort, wie sie war"
    (let [b (:body (api/mutate :vorgang-anlegen {:nr "V-1" :titel "Dach"}
                               {:publish? true}))]
      (is (nil? (:edits b))
          "eine Antwort waechst nicht ungefragt")))

  (testing "eine Anlage nennt das angelegte Objekt"
    (let [e (:edits (:body (api/mutate :vorgang-anlegen {:nr "V-2" :titel "Fenster"}
                                       {:publish? true :return-edits? true})))
          edit (first (:edits e))]
      (is (= 1 (:added-objects-count e)))
      (is (= 0 (:modified-objects-count e)))
      (is (= :add-object (:type edit)))
      (is (= :vorgang (:object-type edit)))
      (is (some? (:object-id edit)) "die Id, mit der man das Objekt adressiert")
      (is (contains? edit :primary-key)
          "jeder Typ erklaert seinen Schluessel, auch einer ohne Producer")))

  (testing "eine Aenderung zaehlt als Aenderung, und der Griff bleibt derselbe"
    (let [oid (-> (api/mutate :vorgang-anlegen {:nr "V-3" :titel "alt"}
                              {:publish? true :return-edits? true})
                  :body :edits :edits first :object-id)
          e   (:edits (:body (api/mutate :vorgang-umbenennen {:vorgang oid :titel "neu"}
                                         {:publish? true :return-edits? true})))]
      (is (= 0 (:added-objects-count e)))
      (is (= 1 (:modified-objects-count e)))
      (is (= :modify-object (:type (first (:edits e)))))
      (is (= oid (:object-id (first (:edits e))))
          "der Rueckgabewert der Anlage ist derselbe Griff wie der der Aenderung")))

  (testing "ein Stapel ist ein Aufruf"
    (let [rows (mapv (fn [i] {:nr (str "B-" i) :titel (str "Stapel " i)}) (range 5))
          b    (:body (api/mutate-batch :vorgang-anlegen rows
                                        {:publish? true :return-edits? true}))]
      (is (= 5 (:added-objects-count (:edits b))))
      (is (= 5 (count (set (map :object-id (:edits (:edits b))))))
          "jede Zeile des Stapels steht einzeln und mit eigener Id in der Antwort")))

  (testing "gestaged zaehlt genauso — das ist der Weg, den ein Abspieler geht"
    (let [b (:body (api/mutate :vorgang-anlegen {:nr "S-1" :titel "gestaged"}
                               {:stage? true :return-edits? true}))]
      (is (true? (:staged b)) "setup: wirklich gestaged, nicht dispatched")
      (is (= 1 (:added-objects-count (:edits b))))
      (is (some? (:object-id (first (:edits (:edits b)))))
          "die Id steht schon vor dem Commit fest — sie kommt aus dem tx-vector")))

  (testing "und ein gestagter Stapel ist EINE Transaktion"
    (let [rows (mapv (fn [i] {:nr (str "S-B-" i) :titel "gestapelt"}) (range 4))
          b    (:body (api/mutate-batch :vorgang-anlegen rows
                                        {:stage? true :return-edits? true}))]
      (is (= 4 (count (:seqs b))) "vier Staging-Zeilen aus einem Aufruf")
      (is (= 4 (:added-objects-count (:edits b))))))

  (testing "eine Action ohne Funktion traegt 10000 Aufrufe"
    (let [rows (fn [n] (mapv (fn [i] {:nr (str "Z-" i) :titel "viel"}) (range n)))]
      (is (ok? (api/mutate-batch :vorgang-anlegen (rows 21) {:publish? true})))
      (is (= 400 (:status (api/mutate-batch :vorgang-anlegen (rows 10001) {:publish? true})))
          "die Tuer weist ab, statt teilweise zu schreiben"))))
