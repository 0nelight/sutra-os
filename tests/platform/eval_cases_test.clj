(ns ^:integration platform.eval-cases-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.evals.cases :as cases]
            [sutra.store.access.scope :as ctx]
            [sutra.catalog.lookup :as lookup]
            [sutra.authoring.install.world :as onto-install]
            [support.pod :as pod]
            [support.store :as store]))

(use-fixtures :once pod/with-mem-stores-only)

(def ^:private WORLD
  (@#'onto-install/normalize-schema
   {:object-types {:rechnung  {:object-type-name :rechnung
                               :attribute-list [{:attribute-name :nr :attribute-type :string}
                                                {:attribute-name :netto
                                                 :attribute-type :double}]
                               :primary-key [:nr]}
                   :lieferant {:object-type-name :lieferant
                               :attribute-list [{:attribute-name :name :attribute-type :string}]
                               :primary-key [:name]}}
    :link-types {:rechnung-lieferant {:link-type-name :rechnung-lieferant
                                      :from-object-type :rechnung :to-object-type :lieferant
                                      :cardinality :many-to-one
                                      :forward-api-name :lieferant
                                      :reverse-api-name :rechnungen}}
    :interfaces {} :transforms {} :actions {} :object-sets {} :value-types {}}))

(def ^:private R (lookup/rid-of WORLD :object-types :rechnung))
(def ^:private L (lookup/rid-of WORLD :object-types :lieferant))
(def ^:private RL (lookup/rid-of WORLD :link-types :rechnung-lieferant))

(deftest cases-read-linked-objects-and-every-page
  (let [b (store/fresh-branch! "eval-cases")]
    (try
      (store/install! b WORLD)
      (store/commit-ops!
       b
       (concat [(store/create-object L {:id "l1" :values {:name "Nordlicht"}})]
               (for [i (range 1203)]
                 (store/create-object R {:id (str "r" i)
                                         :values {:nr (str "R" i) :netto (double i)}}))
               [(store/create-link RL {:from-id "r1" :to-id "l1"})]))
      (let [s   (ctx/as-system (ctx/store b))
            out (cases/object-set-cases
                 s WORLD {:object-type :rechnung
                          :id          :nr
                          :input       {:nr :nr :quelle {:val "grok"}}
                          :expected    {:netto :netto
                                        :lieferant {:link :lieferant :property :name}}})
            by  (into {} (map (juxt :id identity)) (:cases out))]
        (testing "no cap: more objects than one page all become cases"
          (is (= 1203 (count (:cases out)))))
        (testing "a linked property and a static value"
          (is (= "Nordlicht" (get-in by ["R1" :expected :lieferant])))
          (is (nil? (get-in by ["R2" :expected :lieferant])))
          (is (= "grok" (get-in by ["R2" :input :quelle]))))
        (testing "an explicit :limit is the author's cap"
          (is (= 10 (count (:cases (cases/object-set-cases
                                    s WORLD {:object-type :rechnung :limit 10
                                             :input {:nr :nr}})))))))
      (finally (store/retire! b)))))
