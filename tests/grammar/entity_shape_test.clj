(ns ^:unit grammar.entity-shape-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.types.entities :as ents]
            [sutra.types.shape :as tshape]))

(deftest an-empty-collection-is-indistinguishable-from-an-absent-one
  (doseq [dir [tshape/->author tshape/->canonical]]
    (is (= (dir :datasets {:columns []} {})
           (dir :datasets {} {}))
        "empty and absent must be the same projection")))

(deftest the-two-directions-round-trip
  (let [author    {:kind "standalone" :columns [:a] :title "T" :description "d"
                   :object-type :zeitbuchung :properties [:stunden] :former-names [:alt]
                   :stream {:partitions 2 :partition-keys [:a]}
                   :restricted-view {:dataset :roh :policy {:eq [:a "x"]}}
                   :virtual {:source :erp :table "t"}
                   :project :p :ignore-inherited true
                   :status :deprecated :deprecation {:reason "r"} :markings [:pii]}
        canonical (tshape/->canonical :datasets author {:dataset-name :raw})
        back      (tshape/->author    :datasets canonical {:name :raw})]
    (is (= :standalone (:kind canonical)) "coercion applies on the way in")
    (is (= [:a] (:columns canonical)))
    (testing "every PROJECTED author field survives the round trip"
      (doseq [[k opts] (:datasets tshape/entity-fields)
              :when (not (:custom opts))]
        (is (contains? back k) (str k " lost in projection")))))

  (testing ":custom fields are carried by bespoke code — projector skips them, both ways"
    (is (= {:name :raw}
           (tshape/->author :datasets
                           {:dataset-name    :raw
                            :external-source {:datasource :mro :table "t"}}
                           {:name :raw}))
        "external-source emits nothing generic on the way out")
    (is (= {:dataset-name :raw}
           (tshape/->canonical :datasets
                              {:external-source {:datasource :mro}}
                              {:dataset-name :raw}))
        "and is not picked up on the way in — kernel/dataset-spec assocs it")))

(deftest a-field-absent-under-its-rule-is-simply-not-written
  (let [out (tshape/->canonical :datasets {:kind :standalone} {:dataset-name :raw})]
    (is (= #{:dataset-name :kind} (set (keys out)))
        "no nil-valued keys — an absent field must not become a present nil,
         which would compare unequal to a genuinely absent one")))

(deftest object-types-the-last-and-least-mechanical-entity
  (testing "the two un-dropped fields now survive a construct → export round trip"
    (let [canon (tshape/->canonical :object-types
                                   {:semantic-search {:property :vec :model "m" :k 5}
                                    :edit-conflict   {:strategy :apply-most-recent
                                                      :timestamp-attr :updated-at}}
                                   {:object-type-name :triebwerk})
          back  (tshape/->author :object-types canon {})]
      (is (= {:property :vec :model "m" :k 5} (:semantic-search back))
          ":semantic-search used to construct + compare but never export")
      (is (= {:strategy :apply-most-recent :timestamp-attr :updated-at}
             (:edit-conflict back))
          ":edit-conflict used to be forwarded by the action but stored nowhere")))

  (testing ":custom fields are carried by bespoke code — projector skips them, both ways"
    (is (= {} (tshape/->author :object-types
                              {:object-type-name :triebwerk
                               :attribute-list  [{:attribute-name :id :attribute-type :string}]
                               :implements      [:named-thing]
                               :external-source {:datasource :db :table "t"}}
                              {}))
        "attributes/implements/external-source emit nothing generic")
    (is (= {:object-type-name :w}
           (tshape/->canonical :object-types
                              {:attributes [[:id :string]] :external-source {:datasource :db}}
                              {:object-type-name :w}))
        "and none is picked up on the way in"))

  (testing "the convergence property holds for a scalar: [] and absent project identically"
    (doseq [dir [tshape/->author tshape/->canonical]]
      (is (= (dir :object-types {:match-keys []} {})
             (dir :object-types {} {})))))

  (testing "scalars-satisfied? — the heterogeneous comparison, now :compare data"
    (testing ":lenient — an omitted symbol is 'unspecified', a differing one is a change"
      (let [canon {:object-type-name :triebwerk :symbol "F"}]
        (is (tshape/scalars-satisfied? :object-types {:name :triebwerk} canon))
        (is (not (tshape/scalars-satisfied? :object-types {:symbol "G"} canon)))))
    (testing ":bool — resolve-entity compares truthiness, so absent ≠ live true"
      (let [canon {:object-type-name :triebwerk :resolve-entity true}]
        (is (tshape/scalars-satisfied? :object-types {:resolve-entity true} canon))
        (is (not (tshape/scalars-satisfied? :object-types {:name :triebwerk} canon)))))
    (testing ":eq — edit-conflict is full-replace: a differing value re-stages"
      (let [canon {:object-type-name :triebwerk :edit-conflict {:strategy :apply-most-recent}}]
        (is (tshape/scalars-satisfied? :object-types
                                      {:edit-conflict {:strategy :apply-most-recent}} canon))
        (is (not (tshape/scalars-satisfied? :object-types
                                           {:edit-conflict {:strategy :apply-user-edits}} canon)))))
    (testing "external-source is :custom, so it is NOT compared here — a live-only value is satisfied"
      (is (tshape/scalars-satisfied? :object-types
                                    {:name :triebwerk}
                                    {:object-type-name :triebwerk
                                     :external-source {:datasource :db}})))))

(deftest an-authored-backing-survives-the-export-round-trip
  (let [backing {:dataset :satz-ds :primary-key :satz-id :column-map {:satz-id :SATZ_ID}}
        canon   (tshape/->canonical :object-types {:backing backing}
                                   {:object-type-name :stundenbuchung})]
    (is (= backing (:backing canon)) "authored → canonical")
    (is (= backing (:backing (tshape/->author :object-types canon {}))) "canonical → authored")
    (testing "the dataset is a REF, so it resolves and follows a rename"
      (is (some #(= [:backing :dataset] (:at %))
                (:refs (first (filter #(= :object-type (:tag %)) ents/entities))))))
    (testing ":eq — a different dataset is a change, not an omission"
      (is (tshape/scalars-satisfied? :object-types {:backing backing} canon))
      (is (not (tshape/scalars-satisfied? :object-types
                                         {:backing (assoc backing :dataset :andere-ds)}
                                         canon))))))

(deftest functions-the-last-holdout-needed-a-default
  (testing ":default folds authored-absent and authored-explicit together"
    (is (= {:name :flow :language :clojure}
           (tshape/->author :functions {:source nil} {:name :flow}))
        "an omitted language is projected as the kernel default")
    (is (= (tshape/->author :functions {} {:name :flow})
           (tshape/->author :functions {:language :clojure} {:name :flow}))
        "so the desired side can never lack what the canonical side carries")
    (is (= :clojure (:language (tshape/->canonical :functions {} {:function-name :flow})))
        "and construction applies the same default the kernel used to inline"))

  (testing ":kw-map keywordizes a param-types map's keys AND values, inbound"
    (is (= {:x :string :y :int}
           (:param-types (tshape/->canonical :functions
                                            {:param-types {"x" "string" "y" "int"}}
                                            {:function-name :flow})))))

  (testing "param-types folds {} with absent — the kernel/export split it closed"
    (doseq [dir [tshape/->author tshape/->canonical]]
      (is (= (dir :functions {:param-types {}} {})
             (dir :functions {} {})))))

  (testing "a full function round-trips construct → export unchanged"
    (let [author {:target-type :arbeitsauftrag :language :python :source "print(1)"
                  :param-types {:limit :int}}
          canon  (tshape/->canonical :functions author {:function-name :flow})
          back   (tshape/->author    :functions canon {:name :flow})]
      (is (= :arbeitsauftrag (:target-type canon)))
      (is (= {:limit :int} (:param-types canon)))
      (is (= {:name :flow :target-type :arbeitsauftrag :language :python
              :source "print(1)" :param-types {:limit :int}}
             back)))))
