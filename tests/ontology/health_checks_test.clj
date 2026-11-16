(ns ^:api ontology.health-checks-test
  (:require [support.wire :refer [ok?]]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.types.naming :as naming]
            [sutra.wire.api :as api]
            [support.pod :as pod]))

(use-fixtures :once pod/with-pod)

(def ^:private schema
  {:projects {:health {}}
   :objects {:widget {:project    :health
                      :name       "Widget"
                      :primary-key :pk :attributes [[:pk :string] [:created-at :timestamp]]
                      :plural "Widgets"}}})

(defn- checks [] (:checks (:body (api/get! :health-checks))))
(defn- by-name [nm] (some #(when (= nm (:name %)) %) (checks)))

(deftest health-check-rider-install-and-run
  (testing "install a minimal object-type"
    (is (ok? (api/post! :install {:schema schema})) "install stages")
    (is (ok? (api/post! :staging)) "commit — reconcile passes"))

  (testing "the .ent rider shape is accepted + normalized server-side"
    (is (ok? (api/post! :health-checks {:name "widgets present" :target :widget :kind :row-count
               :min 1 :severity :critical})))
    (is (ok? (api/post! :health-checks {:name "widgets fresh" :target :widget :kind :freshness
               :column :created-at :within-hours 26 :severity :moderate})))
    (is (ok? (api/post! :health-checks {:name "no drift" :target :widget :kind :schema
               :comparison :column-additions-allowed :severity :critical})))
    (let [rc (by-name "widgets present")]
      (is (some? rc) "row-count check persisted")
      (is (= "row-count" (:kind rc)) "keyword :kind normalized to a DAO string")
      (is (= "object-type" (:node-kind rc)) ":target bound to an object-type node")
      (is (= "widget" (:node-key rc)))
      (is (= :critical (:severity rc)) ":severity round-trips out of the expects payload")))

  (testing "run-all grades each kind against the (empty) backing table"
    (let [results (:results (:body (api/post! :health-run-all)))
          status  (fn [nm] (:status (some #(when (= (:check-id %)
                                                    (:id (by-name nm))) %) results)))]
      (is (= "failed" (status "widgets present")) "row-count min 1 fails on 0 rows")
      (is (= "warning"
             (:status (some #(when (= (:id (by-name "no drift")) (:check-id %)) %) results)))
          "schema-drift kind is declared but evaluator-pending → warning")))

  (testing "re-declaring the same-named rider UPSERTS (idempotent), not duplicates"
    (let [before (count (checks))]
      (is (ok? (api/post! :health-checks {:name "widgets present" :target :widget :kind :row-count
                 :min 2 :severity :critical})))
      (is (= before (count (checks))) "same name → same row, no duplicate")))

  (testing "an `:on` discriminator binds the check to a transform / dataset node"
    (is (ok? (api/post! :health-checks
                        {:name "pipeline fresh" :target :enrich-widgets :on :transform
:kind :freshness :column :created-at :within-hours 24 :severity :critical})))
    (is (ok? (api/post! :health-checks {:name "raw nonempty" :target :raw_widgets :on :dataset
               :kind :row-count :min 1 :severity :moderate})))
    (let [tx (by-name "pipeline fresh")
          ds (by-name "raw nonempty")]
      (is (= "transform" (:node-kind tx)) ":on :transform binds a transform node")
      (is (= "enrich-widgets" (:node-key tx)))
      (is (= "dataset" (:node-kind ds)) ":on :dataset binds a dataset node")
      (is (= "raw_widgets" (:node-key ds)))))

  )

(def ^:private feed-schema
  {:schema
   {:objects {:gizmo {:project :health-feed :symbol "🔩"
                      :attributes [[:pk :string] [:label :string]]
                      :plural "Gizmos"}}
    :transforms
    {:mk-gizmo {:project :health-feed :language :pg-sql
                :query "SELECT gid AS pk, label AS label FROM feed"
                :inputs {:feed {:kind :dataset :dataset :feed}}
                :primary-key :pk
                :output {:kind :object-type :object-type :gizmo}}}}})

(deftest a-check-reads-the-store-that-holds-its-node
  (testing "a dataset with rows, and a transform that reads it into an object-type"
    (is (ok? (api/mutate :create-project {:name :health-feed} {:stage? true})))
    (is (ok? (api/mutate :create-dataset
                         {:name :feed
                          :project :health-feed
                          :columns [{:attribute-name :gid        :attribute-type :string}
                                    {:attribute-name :label      :attribute-type :string}
                                    {:attribute-name :created_at :attribute-type :timestamp}]}
                         {:stage? true})))
    (is (ok? (api/post! :staging)) "commit dataset")
    (is (ok? (api/post! :install feed-schema)) "install transform + object-type")
    (is (ok? (api/post! :staging)) "commit schema")
    (is (ok? (api/post! :dataset-write {:name "feed"}
                        {:mode :snapshot
   :rows [{:values {:gid "g1" :label "alpha" :created_at "2020-01-01 00:00:00"}}
          {:values {:gid "g2" :label ""      :created_at "2020-01-02 00:00:00"}}]
   :message "seed"})))
    (is (ok? (api/post! :import-run)) "build"))

  (testing "declare one check of every kind, over every node kind"
    (let [feed-rid (some (fn [[rid v]] (when (= :feed (:dataset-name v)) rid))
                         (:datasets (:body (api/get! :schema))))]
      (is (some? feed-rid) "found the feed dataset")
      (doseq [c [                 {:name "feed nonempty"  :target :feed     :on :dataset
                                   :kind :row-count :min 1}
                 {:name "gizmos made"    :target :mk-gizmo :on :transform :kind :row-count :min 1}
                 {:name "gizmos present" :target :gizmo                   :kind :row-count :min 1}
                 {:name "feed fresh"     :target :feed :on :dataset :kind :freshness
                  :column :created_at :within-hours 8760000}
                 {:name "labels filled"  :target :feed :on :dataset :kind :null-percentage
                  :column :label :max 60}
                 {:name "gids unique"    :target :feed :on :dataset :kind :approximate-unique
                  :column :gid :min 2}
                 {:name "custom count"   :target :feed :on :dataset :kind :custom
                  :query (str "SELECT COUNT(*) AS n FROM " (naming/dataset-table feed-rid))
                  :min 1}
                 {:name "custom count via ref" :target :feed :on :dataset :kind :custom
                  :query "SELECT COUNT(*) AS n FROM {{dataset:feed}}" :min 1}
                 {:name "gizmo count via ref"  :target :gizmo :kind :custom
                  :query "SELECT COUNT(*) AS n FROM {{gizmo}}" :min 1}
                 {:name "custom bad ref"       :target :feed :on :dataset :kind :custom
                  :query "SELECT COUNT(*) AS n FROM {{dataset:nope}}" :min 1}]]
        (is (ok? (api/post! :health-checks (assoc c :severity :moderate)))
            (str "declare " (:name c))))))

  (let [rs (:results (:body (api/post! :health-run-all)))
        r  (fn [nm] (let [id (:id (by-name nm))]
                      (some #(when (= id (:check-id %)) %) rs)))]
    (testing "row-count reads the store that holds the node — all three kinds"
      (is (= ["passed" "2"] ((juxt :status :value) (r "feed nonempty")))
          (str "a dataset node's rows are the BULK store's — " (pr-str (r "feed nonempty"))))
      (is (= ["passed" "2"] ((juxt :status :value) (r "gizmos made")))
          (str "so are a transform OUTPUT's — " (pr-str (r "gizmos made"))))
      (is (= ["passed" "2"] ((juxt :status :value) (r "gizmos present")))
          (str "an object-type's are the serving plane's — " (pr-str (r "gizmos present")))))

    (testing "the column kinds evaluate, in each store's own dialect"
      (is (= "passed" (:status (r "feed fresh")))
          (str "freshness — " (pr-str (r "feed fresh"))))
      (is (pos? (Long/parseLong (str (:value (r "feed fresh")))))
          "hours since 2020 is a real positive number, not nil")
      (is (= "passed" (:status (r "labels filled")))
          (str "null-percentage — " (pr-str (r "labels filled"))))
      (is (= 50.0 (Double/parseDouble (str (:value (r "labels filled")))))
          "1 empty label of 2 = 50%, computed rather than guessed")
      (is (= ["passed" "2"] ((juxt :status :value) (r "gids unique")))
          (str "approximate-unique — " (pr-str (r "gids unique")))))

    (testing "a custom query runs on the node's store, with its namespace in scope"
      (is (= ["passed" "2"] ((juxt :status :value) (r "custom count")))
          (str "an unqualified name resolved wherever the rows are — "
               (pr-str (r "custom count")))))

    (testing "a custom query may name NODES instead of physical tables"
      (is (= ["passed" "2"] ((juxt :status :value) (r "custom count via ref")))
          (str "{{dataset:feed}} resolved to the same table the literal names — "
               (pr-str (r "custom count via ref"))))
      (let [ot (r "gizmo count via ref")]
        (is (= "failed" (:status ot))
            "an object type is checked by kind, never by SQL against its storage")
        (is (re-find #"object types are checked by kind" (str (:detail ot))) (pr-str ot)))
      (let [bad (r "custom bad ref")]
        (is (= "failed" (:status bad)) "an unresolvable ref fails the CHECK")
        (is (re-find #"\{\{dataset:nope\}\}" (str (:detail bad)))
            (str ":detail names the offending ref rather than a raw SQL error — "
                 (pr-str bad)))))))
