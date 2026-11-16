(ns ^:unit grammar.impact-and-findings-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.parser.sugar :as sugar]
            [sutra.parser.impact :as impact]
            [sutra.parser.resolve :as res]))

(def ^:private ot-luftfahrzeug :ri.test.object-type.luftfahrzeug)
(def ^:private ot-mangel  :ri.test.object-type.mangel)

(def ^:private schema
  {:object-types
   {ot-luftfahrzeug {:object-type-name :luftfahrzeug :plural "Luftfahrzeuge"}
    ot-mangel  {:object-type-name :mangel :plural "Mängel"}}
   :link-types
   {:ri.test.link-type.lr {:link-type-name :luftfahrzeug-mangel
                           :from-object-type ot-luftfahrzeug
                           :to-object-type ot-mangel
                           :cardinality :one-to-many}}
   :transforms
   {    :ri.test.transform.a {:transform-name :liest-plural
                          :inputs {:l {:kind :object-type :object-type ot-luftfahrzeug}}
                          :output {:kind :object-type :object-type ot-mangel}}
    :ri.test.transform.b {:transform-name :liest-singular
                          :input {:kind :object-type :object-type ot-luftfahrzeug}
                          :output {:kind :dataset}}
    :ri.test.transform.c {:transform-name :schreibt-multi
                          :outputs [{:kind :object-type :object-type ot-luftfahrzeug}
                                    {:kind :dataset}]}}
   :actions
   {:luftfahrzeug-registrieren {:edits [{:object-type ot-luftfahrzeug}]
                        :rules [{:op :create :object-type ot-luftfahrzeug}]}}
   :applications
   {:ri.test.application.x {:application-name :flottenmodul :object-type ot-luftfahrzeug}}})

(deftest transform-inputs-reads-both-shapes
  (testing "the singular :input is an input — the drift that cost `dependents` its readers"
    (is (= 1 (count (impact/transform-inputs (get-in schema [:transforms :ri.test.transform.b])))))
    (is (= 1 (count (impact/transform-inputs (get-in schema [:transforms :ri.test.transform.a])))))
    (is (empty? (impact/transform-inputs {})))))

(deftest transform-outputs-reads-both-shapes
  (is (= 1 (count (impact/transform-outputs (get-in schema [:transforms :ri.test.transform.a])))))
  (testing "a multi-output producer carries NO :output key — reading only the singular blinds the DAG"
    (is (= 2
           (count (impact/transform-outputs (get-in schema [:transforms :ri.test.transform.c])))))))

(deftest dependents-finds-readers-writers-links-actions-apps
  (let [d (impact/dependents-of schema :luftfahrzeug)]
    (testing "both readers are found, whichever input spelling they use"
      (is (= #{"liest-plural" "liest-singular"}
             (set (map :transform (filter :reads (:transforms d)))))))
    (testing "the multi-output producer is found as a WRITER"
      (is (= ["schreibt-multi"] (map :transform (filter :writes (:transforms d))))))
    (is (= ["luftfahrzeug-mangel"] (map :link (:links d))))
    (is (= ["luftfahrzeug-registrieren"] (map :action (:actions d))))
    (is (= ["create"] (:ops (first (:actions d)))))
    (is (= ["flottenmodul"] (map :app (:apps d))))
    (is (= 6 (:total d)))))

(deftest dependents-distinguishes-unknown-from-unreferenced
  (testing "an unknown type is nil, not an empty result — one of the two is a typo"
    (is (nil? (impact/dependents-of schema :gibt-es-nicht))))
  (testing "a known type nobody references answers zero"
    (let [leer (impact/dependents-of {:object-types {:ri.x {:object-type-name :allein
                                                            :plural "Alleine"}}} :allein)]
      (is (= 0 (:total leer))))))

(deftest action-targets-names-what-a-rule-touches
  (is (= #{[:object-type ot-luftfahrzeug]}
         (impact/action-targets (get-in schema [:actions :luftfahrzeug-registrieren]))))
  (testing "a link op and an action-dispatch op are their own kinds"
    (is (= #{[:link-type :lr] [:action :andere]}
           (impact/action-targets {:rules [{:op :create-link :link-type :lr}
                                           {:op :action :name :andere}]}))))
  (testing "a rule set that names nothing is empty, not nil"
    (is (= #{} (impact/action-targets {})))))

(deftest function-references-reads-only-the-two-client-idioms
  (let [refs (impact/function-references
              (str "(-> (OntologyClient/search) (.auftrags-ereignis) (.filter x))"
                   "(-> OntologyClient/action (.freigeben {}))"))]
    (is (= #{:auftrags-ereignis} (:reads-types refs)))
    (is (= #{:freigeben} (:invokes-actions refs))))
  (testing "a `.foo` on a plain value is NOT a reference — that is what anchoring buys"
    (let [refs (impact/function-references "(-> rows (.filter x) (.deref))")]
      (is (= #{} (:reads-types refs)))
      (is (= #{} (:invokes-actions refs)))))
  (testing "absent source answers empty sets, so a function draws no edges rather than wrong ones"
    (is (= {:invokes-actions #{} :reads-types #{} :edits-types #{}}
           (impact/function-references nil)))
    (is (= {:invokes-actions #{} :reads-types #{} :edits-types #{}}
           (impact/function-references ""))))
  (testing "an edit function names what it edits in its declaration"
    (is (= #{:rechnung :partner}
           (:edits-types (impact/function-references
                          "(defn ^{:sutra/function {:edits [:rechnung :partner]}} f [x] x)"))))))

(deftest dependents-walks-a-nested-rule-tree
  (testing "an action whose rules nest still names the type it edits"
    (let [s (assoc-in schema [:actions :verschachtelt]
                      {:rules [{:op :branch :steps [{:op :update :object-type ot-mangel}]}]})]
      (is (some #(= "verschachtelt" (:action %))
                (:actions (impact/dependents-of s :mangel)))))))

(def ^:private aktionen
  (sugar/lower-schema
   {:objects {:luftfahrzeug {:attributes [[:kennzeichen :string]] :match-keys [:kennzeichen]
                             :plural "Luftfahrzeuge"}}
    :actions {:luftfahrzeug-registrieren {:rules [{:op :create :object-type :luftfahrzeug}]
                                  :edits [{:object-type :luftfahrzeug}
                                          {:object-type :mangel}]}
              :halter-anlegen     {:rules [{:op :create :object-type :halte}]}}}))

(deftest ein-phantom-edit-nennt-seine-action
  (testing "`actions-valid?` has always REFUSED this; what it could not do is say which action"
    (let [f (first (filter #(= :aktion/phantom-edit (:rule %)) (res/findings aktionen)))]
      (is (= [:actions :luftfahrzeug-registrieren :edits] (:where f)))
      (is (= :error (:severity f)))
      (is (re-find #"no rule performs it" (:message f))))))

(deftest eine-regel-auf-einen-unbekannten-typ-blockiert
  (testing "the ordinary edit rules got no resolver pass at all — only the interface ones did"
    (let [f (first (filter #(= :aktion/unbekannter-typ (:rule %)) (res/findings aktionen)))]
      (is (= [:actions :halter-anlegen :rules :create] (:where f)))
      (is (re-find #":halte" (:message f)))))
  (testing "a parameter in the type slot is a RUNTIME fact and must not be reported"
    (let [s (sugar/lower-schema
             {:objects {:luftfahrzeug {:attributes [[:kennzeichen :string]]
                                       :plural "Luftfahrzeuge"}}
              :actions {:a {:rules [{:op :update :object-type [:get :arg0 :typ]}]}}})]
      (is (empty? (filter #(= :aktion/unbekannter-typ (:rule %)) (res/findings s))))))
  (testing "and an action over types this file relies on prod for is not told they are missing"
    (let [s (sugar/lower-schema {:actions
                                   {:a {:rules [{:op :create :object-type :nur-auf-prod}]}}})]
      (is (empty? (res/findings s))))))

(deftest actions-valid-ist-vollstaendig-erklaerbar
  (testing "all three halves of `actions-valid?` now name the action they mean"
    (let [s (sugar/lower-schema
             {:objects {:luftfahrzeug {:attributes [[:kennzeichen :string]]
                                       :plural "Luftfahrzeuge"}}
              :links   [[:luftfahrzeug-triebwerk :luftfahrzeug :triebwerk :one-to-many]]
              :actions {:ziel-daneben  {:target :gibt-es-nicht}
                        :param-daneben {:target :luftfahrzeug
                                        :params {:wer {:type :niemand}}}
                        :ziel-ok       {:target {:target-object-type :luftfahrzeug}}}})
          by-rule (group-by :rule (res/findings s))]
      (is (= [[:actions :ziel-daneben :target]]
             (map :where (:aktion/unbekanntes-ziel by-rule))))
      (is (= [[:actions :param-daneben :params :wer]]
             (map :where (:aktion/unbekannter-parametertyp by-rule))))
      (testing "a scalar param type and a well-formed target stay silent"
        (let [ok (sugar/lower-schema
                  {:objects {:luftfahrzeug {:attributes [[:kennzeichen :string]]
                                            :plural "Luftfahrzeuge"}}
                   :links   []
                   :actions {:a {:target :luftfahrzeug :params {:n {:type :string}}}}})]
          (is (empty? (filter #(= "aktion" (namespace (or (:rule %) :x/y)))
                              (res/findings ok)))))))))

(deftest die-spaltenwarnung-schweigt-wo-sie-blind-ist
  (let [sql {:language :pg-sql
             :query   "SELECT z.lizenz, z.jahr FROM stundenbuchung z"
             :inputs  {:stundenbuchung {:kind :object-type :object-type :stundenbuchung}}}
        spalten (fn [s] (->> (res/findings s)
                             (filter #(re-find #"has no column" (str (:message %))))
                             (map :message) vec))]
    (testing "type not declared in this tree ⇒ nothing is said"
      (is (empty? (spalten {:transforms {:arbeitszeit-rows sql}}))))
    (testing "type declared and the column really missing ⇒ still warned, with its subject"
      (let [fs (spalten {:transforms {:arbeitszeit-rows sql}
                         :objects {:stundenbuchung {:description "z"
                                                 :attributes [[:buchung-id [:string]]
                                                              [:jahr [:integer]]]
                                                    :plural "Stundenbuchungen"}}})]
        (is (= 1 (count fs)))
        (is (re-find #"lizenz" (first fs))
            "the missing column is named — and :jahr, which exists, is not")))))

(deftest reference-rules-judge-only-what-a-row-marks-required
  (testing "an agent without :model names nothing — the grammar marks it optional"
    (let [s {:agents {:a {:agent-name :helfer :tools [{:kind :action}]}}
             :llm-models {"m" {}}}]
      (is (empty? (res/findings s)))))
  (testing "an interface edit rule without :interface is refused even when the file declares no :interfaces"
    (let [s  {:actions {:x {:rules [{:op :update-of-interface}]}}}
          fs (res/findings s)]
      (is (= 1 (count fs)))
      (is (= [:actions :x :rules :update-of-interface] (:where (first fs))))
      (is (re-find #"names no :interface" (:message (first fs))))))
  (testing "an optional slot that IS filled is judged"
    (let [s {:agents {:a {:agent-name :helfer :model "nope"}} :llm-models {"m" {}}}]
      (is (re-find #"\"nope\", which is not a declared llm-model"
                   (:message (first (res/findings s))))))))
