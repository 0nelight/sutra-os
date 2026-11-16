(ns ^:unit grammar.prepare-schema-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.parser.ent :as ent]
            [support.in-project :refer [in-project]]
            [sutra.catalog.shape :as shape]
            [sutra.authoring.install.world :as onto-install]))

(defn- prepare
  [sch opts]
  (onto-install/prepare-schema (in-project :t sch) opts))

(deftest structurally-invalid-reports-reasons
  (let [{:keys [ok invalid schema]}
        (prepare {:object-types {:triebwerk {:attribute-list "not-a-vector"
                                             :plural "Triebwerke"}}} {})]
    (is (nil? ok))
    (is (some? invalid) "reasons are surfaced")
    (is (map? schema)   "the normalized schema rides along for ex-data")))

(defn- transform-named
  [schema nm]
  (->> (:transforms schema) vals (filter #(= nm (:transform-name %))) first))

(deftest dataset-external-source-lifts-to-sync-transform
  (let [{:keys [ok invalid]}
        (prepare
         {:datasets {:zeitbuchung_raw
                     {:kind    :standalone
                      :columns [{:attribute-name :buchung_id :attribute-type :string}]
                      :external-source {:datasource  :amos
                                        :table       "Zeitbuchung"
                                        :primary-key :buchung_id}}}}
         {})
        sync (transform-named ok :zeitbuchung_raw-sync)]
    (is (nil? invalid) "a source-bound dataset is a valid schema")
    (is (some? sync) "the :external-source lifted to a <name>-sync transform")
    (is (= :dataset (get-in sync [:output :kind]))
        "it outputs the DATASET, not an object-type — that is the whole point")
    (is (= {:kind :datasource :datasource :amos} (:input sync)))
    (is (= "Zeitbuchung" (get-in sync [:source :table])))
    (is (= :buchung_id (:primary-key sync))
        "a PK rides along so an incremental re-emit UPSERTs rather than duplicating")
    (is (nil? (get-in ok [:datasets
                          (get-in sync [:output :dataset])
                          :external-source]))
        "and the field is stripped off the dataset once lifted")))

(deftest dataset-sync-lift-is-idempotent
  (let [authored {:datasets {:raw_belege
                             {:external-source {:datasource :amos
                                                :table      "Freigabescheine"}}}}
        once  (:ok (prepare authored {}))
        twice (:ok (onto-install/prepare-schema once {}))
        n-syncs (fn [s] (count (filter #(= :dataset (get-in % [:output :kind]))
                                       (vals (:transforms s)))))]
    (is (= 1 (n-syncs once)))
    (is (= 1 (n-syncs twice)) "re-lifting an already-lifted schema adds nothing")))

(deftest terse-schema-preserves-sections-unpack-does-not-name
  (let [{:keys [ok invalid]}
        (prepare
         {:objects  {:triebwerk {:plural "Triebwerke" :primary-key :id
                                 :attributes [[:id :string]]}}
          :datasets {:triebwerk_roh
                     {:kind :standalone
                      :external-source {:datasource :amos :table "Triebwerk"}}}}
         {})]
    (is (nil? invalid))
    (is (some? (transform-named ok :triebwerk_roh-sync))
        "the dataset survived the terse-shape unpack and its sync was lifted")))

(deftest unpack-preserves-every-authored-section
  (let [unpack   shape/unpack-schema
        sections (->> (drop 2 ent/EntSchema)
                      (map first)
                      (remove #{:objects :links}))
        terse    (into {:objects {:triebwerk {:plural "Triebwerke" :attributes [[:id :string]]}}}
                       (for [s sections] [s ::sentinel]))
        out      (unpack terse)]
    (is (seq sections) "the grammar was introspected, not silently empty")
    (is (= :sentinel-preserved
           (if (every? #(= ::sentinel (get out %)) sections)
             :sentinel-preserved
             (vec (remove #(= ::sentinel (get out %)) sections))))
        "every authored section survives the terse-shape unpack, by value")
    (testing "and the terse input is still consumed into the canonical keys"
      (is (contains? out :object-types))
      (is (not (contains? out :objects))))))

(deftest object-type-external-source-lifts-but-is-now-refused
  (let [{:keys [ok invalid schema]}
        (prepare
         {:object-types {:triebwerk {:plural "Triebwerke" :attribute-list [{:attribute-name :id
                                                                            :attribute-type :string}]
                                     :external-source {:datasource  :amos
                                                       :table       "Triebwerk"
                                                       :primary-key :id}}}}
         {})
        rows (transform-named (or ok schema) :triebwerk-rows)]
    (is (some? rows) "object-type :external-source still lifts to <type>-rows")
    (is (= :object-type (get-in rows [:output :kind])))
    (is (some? invalid) "but the layer-line gate refuses the result")
    (is (some #{:lint-gate-clean?} (:relational-checks-failed invalid))
        "and names the check that refused it")
    (is (= :source-syncs-into-ontology (:rule (first (:lint-gate-violations invalid))))
        "itemizing the rule, so the author knows what to author instead")))

(deftest a-dataset-can-back-an-object-type-with-no-program
  (let [{:keys [ok invalid]}
        (prepare
         {:datasets     {:triebwerk_roh {:kind :standalone
                                         :columns [{:attribute-name :id
                                                    :attribute-type :string}]
                                         :external-source {:datasource  :amos
                                                           :table       "Triebwerk"
                                                           :primary-key :id}}}
          :object-types {:triebwerk {:plural "Triebwerke" :attribute-list [{:attribute-name :id
                                                                            :attribute-type :string}]}}
          :transforms   {:triebwerk-rows {:transform-name :triebwerk-rows
                                          :input       {:kind :dataset :dataset :triebwerk_roh}
                                          :primary-key :id
                                          :column-map  {:id :id}
                                          :output {:kind :object-type
                                                   :object-type :triebwerk}}}}
         {})]
    (is (nil? invalid) "no program needed — the gate is satisfied")
    (is (some? (transform-named ok :triebwerk_roh-sync)) "the dataset's sync lifted")
    (is (= :dataset (get-in (transform-named ok :triebwerk-rows) [:input :kind]))
        "and the object type reads the dataset directly")))
