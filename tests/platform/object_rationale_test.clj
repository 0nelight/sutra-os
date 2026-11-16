(ns ^:api platform.object-rationale-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.store.access.scope :as ctx]
            [sutra.store.world :as world]
            [sutra.authoring.projections :as projections]
            [sutra.store.seat :as seat]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api]))

(use-fixtures :once pod/with-mem-pod-auth)

(use-fixtures :each pod/as-admin)

(def ^:private why "Eine Rolle, kein zweiter Stammsatz.")

(deftest an-object-type-keeps-its-rationale
  (is (ok? (api/post! :install
                      {:schema {:projects {:kasse {:description "Kasse"}}
                                :objects
                                  {:partner {:primary-key :partner-nr
                                             :attributes [[:partner-nr :string]
                                                          [:mutter-nr :string
                                                           {:rationale why}]]
                                             :match-keys [:partner-nr]
                                             :project    :kasse
                                             :rationale  why
                                             :plural "Partner"}}
                                :links    [[:partner-mutter :partner :partner :many-to-one
                                            {:forward-api-name :mutter :reverse-api-name :toechter
                                             :fk :mutter-nr :rationale why}]]}})))
  (is (ok? (api/post! :staging)))
  (let [store (ctx/system-store "main")
        c  {:store (world/at store "main") :schema (seat/branch-schema (world/at store "main"))
            :branch "main"}]
    (testing "stored on the type"
      (is (= why (some #(when (= :partner (:object-type-name %)) (:rationale %))
                       (vals (:object-types (:schema c)))))))
    (testing "on a link type"
      (is (= why (some #(when (= :partner-mutter (:link-type-name %)) (:rationale %))
                       (vals (:link-types (:schema c)))))))
    (testing "on an attribute"
      (is (= why (some #(when (= :mutter-nr (:attribute-name %)) (get-in % [:opts :rationale]))
                       (some #(when (= :partner (:object-type-name %)) (:attribute-list %))
                             (vals (:object-types (:schema c))))))))
    (testing "and written back by a pull, so a re-push is unchanged"
      (is (some #(str/includes? % why)
                (vals (:files (projections/pull c "ontologie" nil false))))))))
