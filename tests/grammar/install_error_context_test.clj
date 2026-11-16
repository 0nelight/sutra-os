(ns ^:unit grammar.install-error-context-test
  (:require [sutra.kit.anomaly :as anomaly]
            [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [support.in-project :refer [in-project]]
            [sutra.catalog.kernel :as kernel]
            [sutra.cli.errors :as errors]
            [sutra.authoring.install :as install]
            [sutra.authoring.install.plan :as install-plan]
            [sutra.authoring.install.findings.backing :as backing-findings]
            [sutra.authoring.install.world :as onto-install]
            [sutra.parser.sugar :as sugar]))

(defn- prepare
  [sch opts]
  (onto-install/prepare-schema (in-project :t sch) opts))

(deftest a-structural-error-names-the-entity-and-is-a-client-gate
  (let [bad {:datasets   {:x {:kind :standalone
                              :columns [{:attribute-name :id :attribute-type :string}]}}
             :transforms {:doppelt {:language :pg-sql :query "select 1" :inputs {}
                                    :output {:kind :dataset :dataset :x}
                                    :output-columns
                                    [{:attribute-name :id :attribute-type :string}]}}}
        {:keys [invalid findings]} (prepare bad {})
        e    (kernel/schema-refusal invalid findings)
        d    (ex-data e)]
    (is (= :plan-problems (:sutra/gate d)))
    (is (anomaly/client-error? d) "a 400, not a 500")
    (is (= [:doppelt] (map :entity (:problems d))))
    (is (str/includes? (ex-message e) "transforms :doppelt: output-mutex"))
    (is (not (str/includes? (ex-message e) "ri.sutra")))))

(deftest an-escaping-exception-carries-phase-and-entity
  (let [schema (sugar/lower-schema {:objects {:mangel {:attributes [[:id :string]]
                                                       :plural "Mängel"}}})
        e
        (with-redefs [install-plan/object-type-args (fn [& _] (throw (NullPointerException.)))]
          (try (install/plan-schema schema {} {}) nil
               (catch clojure.lang.ExceptionInfo e e)))]
    (is (= {:phase :object-types :kind :object-type :entity :mangel
            :class "java.lang.NullPointerException" ::anomaly/category :fault}
           (ex-data e)))
    (is (str/includes? (ex-message e) "phase object-types — object-type mangel"))
    (testing "the CLI prints where it failed"
      (let [out (errors/explain-error (ex-info "diff → HTTP 500"
                                            {:desc "diff" :status 500
                                             :body {:error (ex-message e) :data (ex-data e)}})
                                   "fallback")]
        (is (str/includes? out "object-types object-type mangel"))))))

(deftest a-name-collision-says-what-owns-the-name
  (let [ot    "ri.sutra.main.object-type.a1"
        world {:object-types {ot {:id ot :object-type-name :halter :plural "Halter"}}}
        origin #(#'kernel/transform-origin world %)]
    (is
     (=
      "produces object-type :halter under the name the platform generates for its backing transform"
      (origin {:transform-name :halter-rows :output {:kind :object-type :object-type ot}})))
    (is (= "produces object-type :halter"
           (origin {:transform-name :mein-shape :output {:kind :object-type :object-type ot}})))))

(deftest a-dropped-binding-names-its-declaration
  (is (= #{[:object-type :halter :external-sources] [:dataset :roh :external-source]}
         (set (backing-findings/declared-bindings
               {:objects  {:halter {:external-sources [{:datasource :db}]}}
                :datasets {:roh {:external-source {:datasource :db}}}})))))
