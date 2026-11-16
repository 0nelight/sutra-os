(ns ^:unit platform.scoped-import-plan-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.builds.plan :as plan]))

(def ^:private import-block
  {:objects {:widget {:from {:datasource :src}}}
   :links   {}})

(deftest unscoped-is-a-full-reindex
  (doseq [only [nil [] #{}]]
    (testing (str "only-types = " (pr-str only))
      (let [{{only-ds :datasets only-mat :materializations} :node-scope :keys [import]}
            (plan/scoped-import-plan {} import-block only)]
        (is (= import-block import) "import block is unchanged")
        (is (nil? only-mat) "no intermediate scope ⇒ materialize all")
        (is (nil? only-ds)  "no producer scope ⇒ run all producers")))))

(def ^:private schema-with-link-xf
  {:object-types {:buchungssatz {:object-type-name :buchungssatz :plural "Buchungssätze"}
                  :rechnung     {:object-type-name :rechnung :plural "Rechnungen"}
                  :widget       {:object-type-name :widget :plural "Widgets"}}
   :link-types   {:buchung-rechnung {:from-object-type :buchungssatz
                                     :to-object-type   :rechnung}}
   :transforms   {:t-recon  {:transform-name :reconcile-buchung-rechnung
                             :output {:kind :link-type :link-type :buchung-rechnung}}
                  :t-widget {:transform-name :mk-widget
                             :output {:kind :object-type :object-type :widget}}}})

(deftest link-only-scope-is-rejected
  (testing "a transform whose ONLY output is a link-type is flagged, with endpoint hint"
    (let [[v & more] (plan/link-only-scope-violations
                      schema-with-link-xf [:reconcile-buchung-rechnung])]
      (is (nil? more) "exactly one violation")
      (is (= :reconcile-buchung-rechnung (:name v)))
      (is (= :buchungssatz (:from v)) "hint names the link's from-object-type")
      (is (= :rechnung (:to v))       "hint names the link's to-object-type")))
  (testing "an object-type-producing transform is NOT flagged"
    (is (nil? (plan/link-only-scope-violations schema-with-link-xf [:mk-widget]))))
  (testing "a mixed scope flags only the link-only transform"
    (is (= [:reconcile-buchung-rechnung]
           (mapv :name (plan/link-only-scope-violations
                        schema-with-link-xf [:mk-widget :reconcile-buchung-rechnung])))))
  (testing "an unknown / empty scope is clean (nil)"
    (is (nil? (plan/link-only-scope-violations schema-with-link-xf [:no-such])))
    (is (nil? (plan/link-only-scope-violations schema-with-link-xf [])))))

(def ^:private pipeline-schema
  {:object-types {"rid-best" {:object-type-name :bestellung :plural "Bestellungen"}}
   :datasets     {"ds-raw"     {:dataset-name :raw_messages   :kind :transform-output}
                  "ds-extract" {:dataset-name :raw_extract    :kind :transform-output}
                  "ds-shape"   {:dataset-name :shape-positionen :kind :transform-output}}
   :transforms
   {:pull    {:transform-name :pull-mailbox :language :python
              :input   {:kind :datasource :datasource :imap}
              :outputs [{:kind :dataset :dataset "ds-raw" :name :raw_messages}]}
    :extract {:transform-name :extract-positionen :language :python
              :inputs  {:in {:kind :dataset :dataset "ds-raw"}}
              :outputs [{:kind :dataset :dataset "ds-extract" :name :raw_extract}]}
    :shape   {:transform-name :shape-positionen :language :pg-sql
              :dataset "ds-shape" :output-columns [:pos-nr :menge]
              :inputs  {:in {:kind :dataset :dataset "ds-extract"}}}
    :mk-best {:transform-name :mk-bestellung :language :pg-sql
              :inputs {:in {:kind :transform :transform :shape}}
              :output {:kind :object-type :object-type "rid-best"}}}})

(def ^:private pipeline-import {:objects {"rid-best" {:from {:datasource :x}}} :links {}})

(defn- plan
  ([only] (plan only nil))
  ([only opts] (plan/scoped-import-plan pipeline-schema pipeline-import only opts)))

(deftest dataset-terminal-is-buildable-alone
  (testing "naming the checkpoint DATASET selects its producer — and nothing else"
    (doseq [nm [:raw-extract :raw_extract "raw_extract"]]
      (let [{{only-ds :datasets only-mat :materializations} :node-scope :keys [import]} (plan [nm])]
        (is (= #{:extract} only-ds) (str nm " → the extract producer runs"))
        (is (= #{} only-mat)        "no intermediate is dragged in")
        (is (= {} (:objects import)) "a dataset build publishes NO object-type")
        (is (not (contains? only-ds :pull))))))
  (testing "naming the TRANSFORM behind it resolves to the same node"
    (is (= #{:extract} (get-in (plan [:extract-positionen]) [:node-scope :datasets]))))
  (testing "an intermediate's own dataset name is buildable too (was a no-op)"
    (is (= #{:shape} (get-in (plan [:shape-positionen]) [:node-scope :materializations])))))

(deftest default-strategy-does-not-re-pull-upstream
  (let [{{only-ds :datasets only-mat :materializations} :node-scope
         :keys [import]} (plan [:bestellung])]
    (is (= #{} only-ds)      "neither the IMAP pull nor the LLM extract re-runs")
    (is (= #{:shape} only-mat) "the :transform-edge intermediate chain still builds")
    (is (= ["rid-best"] (keys (:objects import))))))

(deftest build-strategies-have-the-documented-shape
  (testing ":ancestors builds the target AND every upstream ancestor"
    (let [{{only-ds :datasets only-mat :materializations} :node-scope}
          (plan [:bestellung] {:strategy :ancestors})]
      (is (= #{:pull :extract} only-ds))
      (is (= #{:shape} only-mat))))
  (testing ":between builds only the path from the boundary down to the target"
    (let [{{only-ds :datasets only-mat :materializations} :node-scope}
          (plan [:bestellung] {:strategy :between
                                                          :from [:raw-extract]})]
      (is (= #{} only-ds) "the extract is the boundary and is NOT re-run")
      (is (= #{:shape} only-mat))))
  (testing ":selected builds exactly the named nodes, nothing upstream"
    (let [{{only-ds :datasets only-mat :materializations} :node-scope}
          (plan [:raw-messages :raw-extract] {:strategy :selected})]
      (is (= #{:pull :extract} only-ds))
      (is (= #{} only-mat)))
    (is (= #{} (get-in (plan [:bestellung] {:strategy :selected}) [:node-scope :datasets]))
        "an object target selects its backing transform only — no intermediates")))

(def ^:private edited-boundary-schema
  {:object-types {"rid-pos"    {:object-type-name :pos}
                  "rid-gruppe" {:object-type-name :gruppe}
                  "rid-ab"     {:object-type-name :abschnitt}}
   :transforms
   {:parse    {:transform-name :parse-pos :language :python
               :outputs [{:kind :object-type :object-type "rid-pos"}]}
    :zuordnen {:transform-name :pos-abschnitt :language :pg-sql
               :inputs  {:g {:kind :object-type :object-type "rid-gruppe"}
                         :p {:kind :object-type :object-type "rid-pos"}}
               :outputs [{:kind :object-type :object-type "rid-pos" :role :supplement}]}
    :shape-ab {:transform-name :shape-ab :language :pg-sql
               :inputs  {:g {:kind :object-type :object-type "rid-gruppe"}
                         :p {:kind :object-type :object-type "rid-pos"}}
               :outputs [{:kind :object-type :object-type "rid-ab"}]}}})

(deftest between-from-an-edited-type-starts-at-its-readers
  (let [build (-> (plan/scoped-import-plan edited-boundary-schema {:objects {} :links {}}
                                           [:pos :abschnitt]
                                           {:strategy :between :from [:gruppe]})
                  :scope :build set)]
    (is (= #{:zuordnen :shape-ab} build)
        "the readers of the edited type are the path; a type with no producer used to
         leave the boundary empty, and the scope built nothing")
    (is (not (build :parse)) "the producer upstream of the path is not re-run")))

(deftest unresolved-targets-are-reported-not-dropped
  (let [{:keys [transforms object-types datasets unresolved]}
        (plan/resolve-build-targets pipeline-schema [:raw-extract :bestellung :nope])]
    (is (= #{:extract :mk-best} transforms))
    (is (= #{"rid-best"} object-types))
    (is (= #{"ds-extract"} datasets))
    (is (= #{:nope} unresolved))))

(deftest scope-preview-answers-for-the-build-that-would-run
  (testing "unscoped previews the WHOLE plan — an unscoped run really does build all"
    (let [{:keys [strategy jobs levels]} (plan/scope-preview pipeline-schema nil)]
      (is (= :full strategy))
      (is (= 4 jobs) "every transform in the schema")
      (is (= [[:pull] [:extract] [:shape] [:mk-best]] levels)
          "one antichain per hop, in dependency order")))
  (testing ":ancestors previews the target AND its ancestors"
    (let [{:keys [scope]} (plan [:bestellung] {:strategy :ancestors})
          {:keys [strategy jobs levels build]} (plan/scope-preview pipeline-schema scope)]
      (is (= :ancestors strategy))
      (is (= 4 jobs))
      (is (= [:extract :mk-best :pull :shape] build))
      (is (= [[:pull] [:extract] [:shape] [:mk-best]] levels))))
  (testing ":selected previews exactly the named node — no ancestors, one level"
    (let [{:keys [scope]} (plan [:bestellung] {:strategy :selected})
          {:keys [jobs levels]} (plan/scope-preview pipeline-schema scope)]
      (is (= 1 jobs))
      (is (= [[:mk-best]] levels))))
  (testing "a typo is previewed as unresolved rather than silently dropped"
    (let [{:keys [scope]} (plan [:bestellung :nope])]
      (is (= [:nope] (:unresolved (plan/scope-preview pipeline-schema scope)))))))

(def ^:private fk-schema
  {:object-types {:buchungssatz    {:object-type-name :buchungssatz :plural "Buchungssätze"}
                  :journal-import {:object-type-name :journal-import :plural "Journal-Importe"}}
   :link-types   {:buchung-import {:from-object-type :buchungssatz
                                   :to-object-type   :journal-import}}
   :transforms
   {:map-journal-imports {:transform-name :map-journal-imports
                           :output {:kind :object-type :object-type :journal-import
                                    :primary-key :import-id}}
    :parse-buchungen      {:transform-name :parse-buchungen
                           :output {:kind :object-type :object-type :buchungssatz
                                    :primary-key :buchung-id}}
    :link-buchung-import  {:transform-name :link-buchung-import
                           :input  {:kind :object-type :object-type :buchungssatz}
                           :output {:kind :link-type :link-type :buchung-import
                                    :fk-attribute :import-id}}}})

(deftest a-foreign-key-link-is-no-build-node
  (testing "estimate and run share one refusal, and it names the side that carries the key"
    (let [{:keys [error data]} (plan/build-scope-refusal fk-schema {:only ["link-buchung-import"]})]
      (is (re-find #"foreign-key link, not a build job" error))
      (is (re-find #"--only buchungssatz" error))
      (is (= "link-buchung-import" (:fk-link-transform data)))))
  (testing "an object-type scope is clean"
    (is (nil? (plan/build-scope-refusal fk-schema {:only [:buchungssatz]}))))
  (testing "the FK producer never enters a build closure"
    (is (not (contains? (get-in (plan/scoped-import-plan fk-schema {:objects {} :links {}}
                                                         [:link-buchung-import])
                                [:scope :build])
                        :link-buchung-import)))))

(def ^:private producerless-schema
  {:object-types {:ausgangsrechnung {:object-type-name :ausgangsrechnung
                                     :plural "Ausgangsrechnungen"}
                  :konto            {:object-type-name :konto :plural "Konten"}}
   :datasets     {:finanz-uploads {:dataset-name :finanz_uploads}}
   :transforms
   {:parse-konten {:transform-name :parse-konten
                   :input  {:kind :dataset :dataset :finanz-uploads}
                   :output {:kind :object-type :object-type :konto :primary-key :konto-id}}}})

(deftest a-scope-that-selects-no-build-node-is-refused
  (testing "an object-type nothing produces and nothing imports: 0 jobs is a refusal, not a green run"
    (let [{:keys [error
                  data]} (plan/build-scope-refusal producerless-schema {:only [:ausgangsrechnung]})]
      (is (re-find #"selects no build node" error))
      (is (re-find #"no transform produces ausgangsrechnung" error))
      (is (= ["ausgangsrechnung"] (:empty-build-scope data)))
      (is (= ["ausgangsrechnung"] (:object-types data)))))
  (testing "a producerless dataset has nothing to build either"
    (is
     (:empty-build-scope
      (:data (plan/build-scope-refusal producerless-schema {:only [:finanz_uploads]})))))
  (testing "one buildable name in the scope is enough"
    (is (nil? (plan/build-scope-refusal producerless-schema {:only [:ausgangsrechnung :konto]})))
    (is (nil? (plan/build-scope-refusal producerless-schema {:only [:parse-konten]})))))
