(ns ^:unit grammar.strict-sweep-test
  (:require [sutra.parser.authoring.standard :as standard]
            [clojure.test :refer [deftest is testing]]
            [sutra.authoring.install.live :as live]
            [sutra.catalog.kernel :as kernel]
            [sutra.ref.install :as ri]
            [sutra.repos.checks :as repo-checks]
            [sutra.authoring.install :as install]
            [sutra.authoring.install.plan :as install-plan]
            [sutra.authoring.install.findings.retire :as retire]
            [sutra.parser.ent :as ent]
            [sutra.parser.project-export :as pe]
            [sutra.parser.project-emit :as pemit]))

(defn- ix [W] (live/world-index W))

(defn- phase-of [plan phase entity]
  (first (filter #(and (= phase (:phase %)) (= entity (:entity %))) plan)))

(defn- deletes [plan]
  (into #{} (map (juxt :kind :entity)) (filter #(= :deleted (:outcome %)) plan)))

(def ^:private live-widget
  {:object-types {"rid-w" {:object-type-name :widget
                           :attribute-list [{:attribute-name :id   :attribute-type :string}
                                            {:attribute-name :size :attribute-type :integer}]
                           :plural "Widgets"}}})

(deftest an-attribute-left-out-is-a-change
  (let [desired {:objects {:widget {:attributes [[:id :string]] :plural "Widgets"}}}
        e (phase-of (install/plan-schema desired live-widget {}) :object-types :widget)]
    (is (= :updated (:outcome e))
        "the whole documented removal path: drop the property from the tree, push.
         While this was a plain subset match it planned :skipped, so the push was
         green and the attribute stayed live")
    (is (= [[:id :string]] (map #(vec (take 2 %)) (:attributes (:args e))))
        "and what it stages is the author's list, not the union — otherwise the
         update would faithfully re-write the attribute back")))

(deftest reconcile-mode-was-never-the-lever
  (let [desired {:objects {:widget {:attributes [[:id :string]]
                                    :reconcile-mode :replace
                                    :plural "Widgets"}}}]
    (is (= :updated (:outcome (phase-of (install/plan-schema desired live-widget {})
                                        :object-types :widget))))))

(deftest an-interface-contributed-column-is-still-tolerated
  (let [desired {:objects    {:gadget {:attributes [[:id :string]] :implements [:thing]
                                       :plural "Gadgets"}}
                 :interfaces {:thing {:property-list [[:label :string]]}}}
        live    {:object-types {"rid-g" {:object-type-name :gadget
                                         :implements [:thing]
                                         :attribute-list
                                         [{:attribute-name :id    :attribute-type :string}
                                          {:attribute-name :label :attribute-type :string}]
                                         :plural "Gadgets"}}
                 :interfaces   {"rid-t" {:interface-name :thing
                                         :property-list [{:attribute-name :label
                                                          :attribute-type :string}]}}}]
    (is (= :skipped (:outcome (phase-of (install/plan-schema desired live {})
                                        :object-types :gadget))))))

(deftest a-transitively-inherited-column-is-tolerated-too
  (let [desired {:objects    {:gadget {:attributes [[:id :string]] :implements [:thing]
                                       :plural "Gadgets"}}
                 :interfaces {:thing {:extends [:base] :property-list []}
                              :base  {:property-list [[:label :string]]}}}
        live    {:object-types {"rid-g" {:object-type-name :gadget
                                         :implements [:thing]
                                         :attribute-list
                                         [{:attribute-name :id    :attribute-type :string}
                                          {:attribute-name :label :attribute-type :string}]
                                         :plural "Gadgets"}}}]
    (is (= :skipped (:outcome (phase-of (install/plan-schema desired live {})
                                        :object-types :gadget))))))

(def ^:private live-action
  {:actions {:zap {:name :zap}}
   :object-types {"rid-w" {:object-type-name :widget
                           :attribute-list [{:attribute-name :id :attribute-type :string}]
                           :plural "Widgets"}}})

(def ^:private keep-widget
  {:objects {:widget {:attributes [[:id :string]] :plural "Widgets"}}})

(deftest an-action-absent-from-the-tree-that-owns-it-is-retired
  (is (contains? (deletes (install/plan-schema keep-widget live-action {:strict? true}))
                 [:action :zap])))

(deftest a-tree-only-speaks-for-the-sections-it-owns
  (let [live (assoc live-action
                    :transforms {"rid-t" {:transform-name :nightly}})
        onto (install/plan-schema keep-widget live
                                  {:strict? true :strict-sections pe/type-sections})]
    (is (not (contains? (deletes onto) [:transform :nightly]))
        "an ontology push may not retire a transform it has no way to declare")
    (is (contains? (deletes onto) [:action :zap])
        "while the sections it DOES own are still swept — the narrowing is per
         section, not an off switch")
    (is (contains? (deletes (install/plan-schema keep-widget live {:strict? true}))
                   [:transform :nightly])
        "and unscoped (the CLI's --strict over a whole-ontology .ent) it is")))

(deftest a-source-repository-speaks-only-for-its-own-members
  (let [live {:transforms {"rid-a" {:transform-name :mine   :repository "ri.repo.a"}
                           "rid-b" {:transform-name :theirs :repository "ri.repo.b"}}}
        d    (deletes (install/plan-schema {} live
                                           {:strict? true
                                            :strict-sections pe/repo-sections
                                            :strict-repository "ri.repo.a"}))]
    (is (contains? d [:transform :mine]))
    (is (not (contains? d [:transform :theirs])))))

(deftest the-sections-that-cannot-carry-absence-are-not-swept
  (let [live {:datasets       {"rid-d" {:dataset-name :d_widget :kind :object-backing}}
              :value-types    {"rid-v" {:value-type-name :currency}}
              :shared-properties {"rid-s" {:shared-property-name :doc-number}}}
        d    (deletes (install/plan-schema {} live {:strict? true}))]
    (doseq [k [:dataset :value-type :shared-property]]
      (is (empty? (filter (comp #{k} first) d))
          (str k " is not exported, so no tree can restate it")))))

(deftest a-declared-dataset-left-out-is-retired
  (let [live {:datasets {"rid-d" {:dataset-name :d_notes :kind :standalone}
                         "rid-e" {:dataset-name :d_kept  :kind :standalone}}}
        d    (deletes (install/plan-schema {:datasets {:d_kept {:kind :standalone}}} live
                                           {:strict? true
                                            :strict-sections pe/ontology-sections}))]
    (is (contains? d [:dataset :d_notes]))
    (is (not (contains? d [:dataset :d_kept])))))

(def ^:private controlling "ri.sutra.main.repository.9f0c")

(def ^:private code-opts
  {:strict? true :strict-sections pe/repo-sections :strict-repository controlling})

(def ^:private live-controlling
  {:repositories {controlling {:repository-name :controlling}
                  "ri.sutra.main.repository.other" {:repository-name :other}}
   :transforms   {"ri.sutra.main.transform.a" {:transform-name :shape-auftragsbestandteil
                                               :repository controlling}
                  "ri.sutra.main.transform.b" {:transform-name :shape-projektbeteiligung
                                               :repository controlling
                                               :status :active}
                  "ri.sutra.main.transform.c" {:transform-name :bleibt
                                               :repository controlling}
                  "ri.sutra.main.transform.d" {:transform-name :fremd
                                               :repository "ri.sutra.main.repository.other"}}})

(deftest a-source-repository-sweeps-its-members-by-rid
  (let [plan (install/plan-schema {:transforms {:bleibt {}}} live-controlling
                                  {:strict? true
                                   :strict-sections pe/repo-sections
                                   :strict-repository controlling})
        d    (deletes plan)]
    (is (contains? d [:transform :shape-auftragsbestandteil])
        "live :repository reads as the NAME after live-by-name, the push passes the RID —
         comparing the two spellings verbatim swept nothing and normalization wrote the
         omitted transforms back")
    (is (contains? d [:transform :shape-projektbeteiligung]))
    (is (not (contains? d [:transform :bleibt])))
    (is (not (contains? d [:transform :fremd])))
    (testing "an :active member is refused, an unmarked (= :experimental) one is not"
      (let [ps (retire/retire-problems live-controlling plan (ix live-controlling))]
        (is (= #{[:retire-protected :shape-projektbeteiligung]}
               (into #{} (map (juxt :rule :entity)) ps)))))))

(deftest a-retired-resource-still-referenced-is-refused
  (let [live {:transforms {"ri.sutra.main.transform.a" {:transform-name :quelle}
                           "ri.sutra.main.transform.b" {:transform-name :leser
                                                        :input
                                                          {:transform
                                                             "ri.sutra.main.transform.a"}}}}
        only-a [{:kind :transform :entity :quelle :outcome :deleted}]
        both   (conj only-a {:kind :transform :entity :leser :outcome :deleted})]
    (is (= [:retire-referenced] (map :rule (retire/retire-problems live only-a (ix live)))))
    (is (re-find #"transform leser" (:message (first (retire/retire-problems live only-a (ix live))))))
    (is (empty? (retire/retire-problems live both (ix live)))
        "a referrer leaving in the same push strands nothing")))

(deftest without-strict-none-of-this-happens
  (testing "the CLI and the API keep their opt-in"
    (is (empty? (deletes (install/plan-schema keep-widget live-action {}))))))

(def ^:private deprecation
  {:reason "abgeloest" :deadline "2026-10-01" :replacement :shape-neu})

(deftest every-swept-kind-carries-the-lifecycle
  (doseq [[section
           entry] {:transforms {:t {:language :pg-sql :query "select 1"
                                    :output-columns [[:k :string]]}}
                           :datasets   {:d {:kind :standalone}}
                           :sources    {:s {:type :jdbc}}
                           :pipelines  {:p {}}}]
    (let [[k e] (first entry)]
      (is (nil? (ent/explain-schema
                 {section {k (assoc e :status :deprecated :deprecation deprecation)}}))
          (str section " takes :status :deprecated + :deprecation")))))

(deftest a-transform-status-survives-store-and-export
  (let [stored (ri/spec kernel/install-cx :transform :t {:language :pg-sql :query "select 1"
                                          :status :deprecated :deprecation deprecation})
        [_ e]  (:entry (pemit/emit-transform stored {:inline? false}))]
    (is (= :deprecated (:status stored)))
    (is (= [:deprecated deprecation] [(:status e) (:deprecation e)])
        "the export is a guided list — without the keys the next install strips them")))

(deftest the-source-repository-retire-check-reads-the-plan
  (let [plan (install-plan/plan-retirements {:transforms {:bleibt {}}} live-controlling
                                       code-opts)
        v    (repo-checks/member-retire-verdict plan)]
    (is (= :failed (:status v)))
    (is (re-find #"shape-projektbeteiligung is left out of the tree but is :active" (:detail v)))
    (is (= :passed (:status (repo-checks/member-retire-verdict
                             (install-plan/plan-retirements
                              {:transforms {:bleibt {} :shape-projektbeteiligung {}}}
                              live-controlling code-opts)))))))

(deftest only-what-the-projection-writes-is-swept
  (let [live {:repositories {controlling {:repository-name :controlling}}
              :applications {"a1" {:application-name :auftrag-sicht :form-factor :full
                                   :object-type :auftrag}
                             "a2" {:application-name :werkzeug-view-panel :form-factor :panel
                                   :object-type :werkzeug}
                             "a3" {:application-name :kalkulation :repository controlling}}}
        onto (deletes (install/plan-schema
                       {} live {:strict? true :strict-sections (pe/carried-sections true)
                                :strict-projected (delay {:applications #{"auftrag-sicht"}})}))
        code (deletes (install/plan-schema
                       {} live
                       (assoc code-opts :strict-projected {:applications #{"kalkulation"}})))]
    (is (contains? onto [:application :auftrag-sicht]))
    (is (not (contains? onto [:application :werkzeug-view-panel]))
        "an orphan view no .entv carries cannot be restated by any tree — it stays")
    (is (not (contains? onto [:application :kalkulation])))
    (is (= #{[:application :kalkulation]} code))))

(deftest a-generated-producer-goes-with-its-owner-first
  (let [live {:link-types {"l1" {:link-type-name :auftrag-projekt}}
              :datasets   {"d1" {:dataset-name :kalk_raw :kind :standalone}}
              :transforms {"t1" {:transform-name :link-auftrag-projekt
                                 :output {:kind :link-type :link-type "l1"
                                          :fk-attribute :projekt-id}}
                           "t2" {:transform-name :kalk_raw-sync
                                 :output {:kind :dataset :dataset "d1"}}}}
        plan (install/plan-schema {} live
                                  {:strict? true :strict-sections (pe/carried-sections true)})
        dels (mapv (juxt :kind :entity) (filter #(= :deleted (:outcome %)) plan))
        at   #(.indexOf dels %)]
    (is (< -1 (at [:transform :link-auftrag-projekt]) (at [:link-type :auftrag-projekt])))
    (is (< -1 (at [:transform :kalk_raw-sync]) (at [:dataset :kalk_raw])))
    (is (empty? (retire/retire-problems live plan (ix live)))
        "the companion leaves with its owner, so it strands nothing")))

(deftest a-transform-the-projection-does-not-write-is-not-swept
  (let [live (assoc-in live-controlling [:transforms "ri.sutra.main.transform.fk"]
                       {:transform-name :link-auftrag-kunde :repository controlling
                        :output {:kind :link-type :link-type :auftrag-kunde
                                 :fk-attribute :kunde-id}})]
    (is (not (contains? (deletes (install/plan-schema {:transforms {:bleibt {}}} live
                                                      code-opts))
                        [:transform :link-auftrag-kunde])))))

(deftest the-projection-decides-an-orphan-view
  (let [lay  {:kind :rows :items []}
        live {:projections standard/projections
              :object-types {"ri.sutra.main.object-type.a"
                             {:object-type-name :auftrag :id "ri.sutra.main.object-type.a"
                              :attribute-list [{:attribute-name :id :attribute-type :string}]
                              :plural "Aufträge"}}
              :applications {"ri.sutra.main.application.v1"
                             {:application-name :auftrag-view-full :form-factor :full
                              :object-type "ri.sutra.main.object-type.a" :layout lay}
                             "ri.sutra.main.application.v2"
                             {:application-name :werkzeug-view-panel :form-factor :panel
                              :object-type "ri.sutra.main.object-type.gone" :layout lay}}}
        opts (repo-checks/push-plan-opts "ri.sutra.main.repository.ontology" live)
        d    (deletes (install/plan-schema {:objects {:auftrag {:attributes [[:id :string]]
                                                                :plural "Aufträge"}}}
                                           live opts))]
    (is (contains? d [:application :auftrag-view-full]))
    (is (not (contains? d [:application :werkzeug-view-panel]))
        "the live projection does not restate a view of a vanished type under its name,
         so no push may take it by omission")))

(deftest a-lifted-transform-the-tree-still-lifts-is-not-swept
  (let [live    {:datasets     {"rid-d" {:dataset-name :foo :kind :standalone}}
                 :object-types {"rid-o" {:object-type-name :bar
                                         :attribute-list
                                           [{:attribute-name :id :attribute-type :string}]
                                         :plural "Bars"}}
                 :transforms   {"rid-t" {:transform-name :foo-sync
                                         :input {:kind :datasource :datasource :db}
                                         :source {:table "t"}
                                         :output {:kind :dataset :dataset "rid-d"}}
                                "rid-x" {:transform-name :bar-rows-src1
                                         :input {:kind :datasource :datasource :db}
                                         :source {:table "u"}
                                         :output {:kind :object-type :object-type "rid-o"}}}}
        desired {:datasets {:foo {:kind :standalone :external-source {:datasource :db :table "t"}}}
                 :objects  {:bar {:attributes [[:id :string]]
                                  :external-sources
                                    [{:datasource :db :table "u" :primary-key :id}]
                                  :plural "Bars"}}}
        plan    (install/plan-schema desired live {:strict? true})]
    (is (not (contains? (deletes plan) [:transform :foo-sync]))
        "a dataset's sync is staged by :dataset-source-transforms; sweeping it in the same plan
         deletes what the plan just kept")
    (is (not (contains? (deletes plan) [:transform :bar-rows-src1]))
        "and so is every extra :external-sources transform of an object type")))
