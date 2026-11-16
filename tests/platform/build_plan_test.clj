(ns ^:unit platform.build-plan-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.builds.plan :as plan]
            [sutra.builds.plan.dag :as dag]
            [support.registry :as registry]))

(def ^:private best "ri.sutra.main.object-type.bestellung")
(def ^:private raw "ri.sutra.main.dataset.raw")
(def ^:private extract "ri.sutra.main.dataset.extract")

(def ^:private schema
  {:object-types {best {:object-type-name :bestellung
                              :attribute-list [{:attribute-name :pos-nr :attribute-type :string}]
                        :plural "Bestellungen"}}
   :sources      {"src-imap" {:source-name :imap :type :imap}}
   :datasets     {raw     {:dataset-name :raw_messages :kind :transform-output}
                  extract {:dataset-name :raw_extract  :kind :transform-output}}
   :transforms
   {:pull    {:transform-name :pull-mailbox :language :python
              :input   {:kind :datasource :datasource :imap}
              :outputs [{:kind :dataset :dataset raw :name :raw_messages}]}
    :extract {:transform-name :extract-positionen :language :python
              :inputs  {:in {:kind :dataset :dataset raw}}
              :outputs [{:kind :dataset :dataset extract :name :raw_extract}]}
    :shape   {:transform-name :shape-positionen :language :pg-sql
              :output-columns [:pos-nr]
              :inputs  {:in {:kind :dataset :dataset extract}}}
    :mk-best {:transform-name :mk-bestellung :language :pg-sql
              :inputs {:in {:kind :transform :transform :shape}}
              :output {:kind :object-type :object-type best
                       :name :mk-bestellung :primary-key :pos-nr}}}})

(deftest unscoped-plan-builds-everything
  (let [{{only-ds :datasets only-mat :materializations} :node-scope
         :keys [jobs scope import datasources reads writes refusal]}
        (plan/build-plan schema {})]
    (is (nil? refusal))
    (is (= #{:pull :extract :shape :mk-best} (set jobs)))
    (is (nil? only-ds) "nil = every producer runs")
    (is (nil? only-mat) "nil = every intermediate runs")
    (is (nil? scope))
    (is (= {:imap {:source-name :imap :type :imap}} datasources))
    (is (= [best] (keys (:objects import))))
    (is (= :mk-best (get-in import [:objects best :from :compute :transform-name])))
    (is (= #{best raw extract ":shape"} writes))
    (is (= #{raw extract ":shape"} reads))))

(deftest default-strategy-builds-the-target-and-its-intermediates
  (let [{{only-ds :datasets only-mat :materializations} :node-scope
         :keys [jobs scope reads writes refusal]}
        (plan/build-plan schema {:only ["bestellung"]})]
    (is (nil? refusal))
    (is (= #{:mk-best :shape} (set jobs)))
    (is (= #{} only-ds) "the upstream producers are not re-run")
    (is (= #{:shape} only-mat))
    (is (= :default (:strategy scope)))
    (is (= #{best ":shape"} writes))
    (is (= #{extract ":shape"} reads) "it reads what the producers left")))

(deftest ancestors-strategy-pulls-every-upstream-producer
  (let [{{only-ds :datasets} :node-scope
         :keys [jobs writes]} (plan/build-plan schema {:only [:bestellung] :strategy :ancestors})]
    (is (= #{:pull :extract :shape :mk-best} (set jobs)))
    (is (= #{:pull :extract} only-ds))
    (is (contains? writes raw))))

(deftest between-strategy-stops-at-the-boundary
  (let [{{only-ds :datasets} :node-scope
         :keys [jobs]} (plan/build-plan schema {:only [:bestellung] :strategy :between
                                                        :from [:raw_extract]})]
    (is (= #{:shape :mk-best} (set jobs)))
    (is (= #{} only-ds))))

(deftest an-exact-job-set-plans-only-those-jobs
  (let [{{only-ds :datasets only-mat :materializations} :node-scope
         :keys [jobs scope writes reads refusal]}
        (plan/build-plan schema {:transforms #{:extract}})]
    (is (nil? refusal))
    (is (= [:extract] jobs))
    (is (= #{:extract} only-ds))
    (is (= #{} only-mat))
    (is (nil? scope))
    (is (= #{extract best} writes)
        "a retry re-imports every object-type, so it claims them all")
    (is (= #{raw} reads))))

(deftest refusals-are-part-of-the-plan
  (testing "an unknown target"
    (is (= {:unresolved ["nope"]} (:data (:refusal (plan/build-plan schema {:only ["nope"]}))))))
  (testing "an unknown strategy"
    (is (= {:strategy :sideways}
           (:data (:refusal (plan/build-plan schema {:only [:bestellung] :strategy :sideways}))))))
  (testing ":between without a boundary"
    (is (some? (:refusal (plan/build-plan schema {:only [:bestellung] :strategy :between})))))
  (testing "a job reading a dataset that is gone"
    (let [broken (update-in schema [:transforms :shape :inputs :in] assoc :dataset :weg)]
      (is (= [{:transform "shape-positionen" :input "in" :kind "dataset" :ref "weg"}]
             (:missing-inputs
              (:data (:refusal (plan/build-plan broken {:only [:bestellung]})))))))))

(deftest a-backing-that-does-not-resolve-throws
  (is (thrown-with-msg? Exception #"Root transform must declare"
                        (plan/build-plan (assoc-in schema [:transforms :mk-best]
                                                   {:transform-name :mk-bestellung
                                                    :input  {:kind :datasource :datasource :imap}
                                                    :source {}
                                                    :output {:kind :object-type :object-type best
                                                             :name :mk-bestellung
                                                             :primary-key :pos-nr}})
                                         {}))))

(deftest schedule-targets-follow-the-schedule-scope
  (is (= {:only #{:bestellung}} (plan/schedule-targets schema {:target "bestellung"})))
  (is (= {:only #{:bestellung :shape-positionen :extract-positionen :pull-mailbox}}
         (plan/schedule-targets schema {:target "bestellung" :scope :with-dependencies}))
      "a transform without an object-type output is named by its transform name")
  (is (= {:only #{:bestellung} :strategy :between :from #{:raw_extract}}
         (plan/schedule-targets schema
                                {:target "bestellung" :scope :connecting :from "raw_extract, "})))
  (is (= {:only nil} (plan/schedule-targets schema {:target ""}))))

(deftest every-scope-kind-runs-as-a-lifted-node-kind
  (registry/complete plan/scope-kinds [:node :selects])
  (registry/covers "a node kind lifted from the transforms is named by a scope kind"
                   (into #{} (map :node) (vals plan/scope-kinds))
                   (into #{} (comp (filter (comp :nodes val)) (map key)) dag/node-kinds))
  (testing "a scoped build lifts only the nodes its scope holds, each by its kind"
    (let [kinds (fn [node-scope] (frequencies (map :kind (dag/scoped-nodes schema node-scope))))]
      (is (= {:producer 3} (kinds nil)) "pull, extract, shape: mk-best is an import")
      (is (= {:producer 1} (kinds {:datasets #{:extract}})))
      (is (= {:producer 1} (kinds {:materializations #{:shape}})))
      (is (= {} (kinds {:exports #{:pull}}))))))
