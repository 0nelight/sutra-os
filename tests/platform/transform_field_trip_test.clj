(ns ^:unit platform.transform-field-trip-test
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [sutra.builds.plan :as plan]
            [sutra.catalog.kernel :as kernel]
            [sutra.types.entities :as ents]
            [sutra.authoring.install.plan :as install-plan]))

(def ^:private fixtures
  {:rechne {:language    :clojure
            :code        "(fn [{:keys [quelle]}] quelle)"
            :modules     {"kern" "def g():\n    return 1\n"}
            :inputs      {:quelle {:kind :dataset :dataset :ein}}
            :output      {:kind :dataset :dataset :aus}
            :profile     :large
            :resources   {:cpu-cores 2 :memory-gb 4}
            :timeout-ms  30000
            :allowed-run-seconds 7200
            :entry-point "rechne"
            :incremental true
            :parameters  {:schwelle {:type :integer :default 3}}
            :container   {:image "registry.local/rechner" :tag "1.0"}
            :media-outputs {:belege {:media-set :belege}}}
   :zieh   {:language         :pg-sql
            :query            "select * from quelle"
            :inputs           {:quelle {:kind :dataset :dataset :ein}}
            :outputs          [{:name :aus :kind :dataset :dataset :aus}]
            :external-systems [:erp]}})

(def ^:private empty-world
  {:object-types {} :link-types {} :interfaces {} :transforms {} :actions {}
   :object-sets {} :datasets {} :projects {:fixture {}}})

(defn- installed
  [k t]
  (let [steps (install-plan/plan-schema {:transforms {k t}} {}
                                        {:only #{:transforms}
                                         :introspect (fn [_ body] [body nil])})
        rules (mapv #(assoc (:args %) :op (:action %) :name (:entity %)) steps)
        world (:world (kernel/eval-steps rules empty-world {}
                                         {:installer {} :project :fixture}))]
    (val (first (:transforms world)))))

(defn- lost
  [authored arrived]
  (sort (remove #(some? (get arrived %)) (keys authored))))

(deftest every-compute-field-has-a-fixture-that-carries-it
  (is (empty? (set/difference (set plan/compute-fields)
                              (set (mapcat keys (vals fixtures)))))
      "a field the registry gains reaches the runner: give it a value in `fixtures`"))

(deftest every-plan-only-field-is-a-transform-field
  (is (empty? (set/difference plan/plan-only-fields
                              (set (keys (:transforms ents/fields)))))))

(deftest an-authored-transform-arrives-whole-at-the-runner
  (doseq [[k t] fixtures
          :let [stored (installed k t)]]
    (testing (str k " — install plan, kernel step, stored world")
      (is (empty? (lost t stored))))
    (testing (str k " — the compute a build hands its runner")
      (is (empty? (lost t (plan/compute stored k :plan)))))))
