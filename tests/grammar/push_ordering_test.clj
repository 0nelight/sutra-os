(ns ^:unit grammar.push-ordering-test
  (:require [sutra.ref.install :as ri]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [sutra.catalog.kernel :as kernel]))

(defn- tx
  [nm spec]
  (let [rid (str "ri.sutra.main.transform." (java.util.UUID/randomUUID))]
    [rid (merge {:transform-name nm :id rid :language :python :code "x" :inputs []} spec)]))

(deftest a-transform-naming-an-uninstalled-type-says-which-repository-to-push-first
  (let [[rid spec] (tx :parse-invoices {:output {:kind :object-type :object-type :rechnung}})
        why        (kernel/reasons {:transforms {rid spec}})]
    (testing "the boolean gate is unchanged — the itemizer is additive and may
              only ADD a sentence, never soften a refusal"
      (is (some #{:transforms-valid?} (:relational-checks-failed why))))
    (let [errs (:transform-errors why)
          txt  (str/join " " (map :reason errs))]
      (is (= [:parse-invoices] (mapv :transform errs))
          "named by its NAME, not by the RID the map is keyed with — a reader
           handed `ri.sutra.main.transform.a1b7…` has been given the one
           identifier they cannot find in the file they are editing")
      (is (str/includes? txt ":rechnung") "and the unresolved target is named")
      (is (str/includes? txt "ONTOLOGY repository")
          "with the reason it cannot be fixed here")
      (is (str/includes? txt "push the ontology first")
          "and the ORDER, which is the actual fix"))))

(deftest a-missing-SIBLING-transform-gets-no-cross-repository-advice
  (testing "the ordering hint is about the ontology-owned kinds. A transform
            whose :input names another transform has a problem inside its own
            repository, and sending its author to a different one would be worse
            than saying nothing — a hint that fires everywhere teaches nothing."
    (let [[rid spec] (tx :chain {:input {:kind :transform :transform :not-here}})
          txt (->> (:transform-errors (kernel/reasons {:transforms {rid spec}}))
                   (map :reason) (str/join " "))]
      (is (str/includes? txt ":not-here"))
      (is (not (str/includes? txt "ONTOLOGY repository"))))))

(defn- ot [nm]
  (let [rid (str "ri.sutra.main.object-type." (java.util.UUID/randomUUID))]
    [rid {:object-type-name nm :id rid :attributes [[:name :string]]}]))

(deftest an-invalid-action-is-named-with-its-reason
  (let [[o-rid o-spec] (ot :lieferant)
        schema {:object-types {o-rid o-spec}
                :actions {:a1 {:action-name :lieferant-anlegen
                               :target :gibt-es-nicht}
                          :a2 {:action-name :param-daneben
                               :target :lieferant
                               :params {:wer {:type :niemand}}}
                          :a3 {:action-name :phantom
                               :target :lieferant
                               :rules [{:op :create :object-type :lieferant}]
                               :edits [{:object-type :rechnung}]}}}
        why    (kernel/reasons schema)
        errs   (:action-errors why)
        by     (into {} (for [e errs] [(:action e) (:reason e)]))]
    (testing "the boolean gate is unchanged — additive, like its two siblings"
      (is (some #{:actions-valid?} (:relational-checks-failed why))))
    (testing "each failing action is named BY NAME, not by the map key"
      (is (= #{:lieferant-anlegen :param-daneben :phantom} (set (keys by)))))
    (testing "and each carries the condition it violated"
      (is (str/includes? (by :lieferant-anlegen) ":gibt-es-nicht"))
      (is (str/includes? (by :param-daneben) ":niemand"))
      (is (str/includes? (by :phantom) "no rule performs it")))))

(deftest a-valid-action-produces-no-sentence
  (let [[o-rid o-spec] (ot :lieferant)
        schema {:object-types {o-rid o-spec}
                :actions {:a1 {:action-name :ok :target :lieferant
                               :params {:n {:type :string}}
                               :rules [{:op :create :object-type :lieferant}]
                               :edits [{:object-type :lieferant}]}}}]
    (is (nil? (:action-errors (kernel/reasons schema))))))

(deftest re-pointing-an-actions-rules-drops-the-old-edits-provenance
  (testing "an update that moves the rule to another type must not carry the OLD
            type's provenance forward — the merged action would then declare an
            edit its rules never perform, which `actions-valid?` refuses"
    (let [welt {:actions {:lieferant-bestätigen
                          {:rules [{:op :update :object-type :lieferant}]
                           :edits [{:object-type :lieferant}]}}}
          schritt {:op :set :path [:actions :lieferant-bestätigen]
                   :value {:rules [{:op :update :object-type :geschäftspartner}]}}
          nach (ri/merge-existing welt schritt)]
      (is (nil? (:edits (:value nach)))
          "dropped, not rewritten — `ensure-actions` re-derives it from the new rules")
      (is (= [{:op :update :object-type :geschäftspartner}] (:rules (:value nach))))))

  (testing "an author who DECLARES :edits still wins — theirs is in the new value"
    (let [welt {:actions {:a {:rules [{:op :update :object-type :alt}]
                              :edits [{:object-type :alt}]}}}
          nach (ri/merge-existing
                welt {:op :set :path [:actions :a]
                      :value {:rules [{:op :update :object-type :neu}]
                              :edits [{:object-type :neu}]}})]
      (is (= [{:object-type :neu}] (:edits (:value nach))))))

  (testing "and a metadata-only update leaves the provenance alone — no :rules,
            no re-derivation, so omit-preserve is right there"
    (let [welt {:actions {:a {:rules [{:op :update :object-type :alt}]
                              :edits [{:object-type :alt}]}}}
          nach (ri/merge-existing
                welt {:op :set :path [:actions :a] :value {:params {:x {:type :string}}}})]
      (is (= [{:object-type :alt}] (:edits (:value nach)))))))

(def ^:private ot-rid "ri.sutra.main.object-type.11111111-1111-1111-1111-111111111111")

(defn- korrektur-schema
  [supp-out]
  (let [[rid spec] (tx :resolve-korrektur-vorschlag {:output supp-out})]
    {:object-types {ot-rid {:object-type-name :auftrag-korrektur
                            :primary-key :korrektur-id
                            :attribute-list [{:attribute-name :korrektur-id :attribute-type :string}
                                             {:attribute-name :vorschlag :attribute-type :string}]
                            :plural "Auftragskorrekturen"}}
     :transforms {rid spec}}))

(deftest a-supplement-whose-primary-is-gone-names-itself-and-the-order
  (let [why (kernel/reasons
             (korrektur-schema {:kind :object-type :object-type ot-rid
                                :role :supplement :primary-key :korrektur-id
                                :column-map {:vorschlag :vorschlag}}))
        errs (:transform-errors why)
        txt  (str/join " " (map :reason errs))]
    (testing "the boolean gate is untouched — the itemizer may only ADD a sentence"
      (is (some #{:transforms-valid?} (:relational-checks-failed why))))
    (is (= [:resolve-korrektur-vorschlag] (mapv :transform errs))
        "named by its NAME, which is what the author has in hand")
    (is (str/includes? txt ":auftrag-korrektur")
        "and the object-type is named, not its RID")
    (is (str/includes? txt "NO primary producer") "the reason")
    (is (str/includes? txt "delete THIS transform before the primary")
        "and the ORDER, which is the actual fix")))

(deftest a-well-formed-supplement-says-nothing
  (testing "the itemizer is silent while a primary is present — it must not
            editorialize about a schema the predicate accepts"
    (let [[prid pspec] (tx :derive-auftrag-korrektur
                          {:output {:kind :object-type :object-type ot-rid
                                    :primary-key :korrektur-id}})
          schema (update (korrektur-schema {:kind :object-type :object-type ot-rid
                                            :role :supplement :primary-key :korrektur-id
                                            :column-map {:vorschlag :vorschlag}})
                         :transforms assoc prid pspec)
          txt (->> (:transform-errors (kernel/reasons schema))
                   (map :reason) (str/join " "))]
      (is (not (str/includes? txt "NO primary producer"))))))
