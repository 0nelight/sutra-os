(ns ^:unit grammar.lint-fk-without-link-test
  (:require [sutra.parser.spec :as spec]
            [clojure.test :refer [deftest testing is]]
            [sutra.catalog.kernel :as kernel]
            [sutra.parser.lint :as lint]))

(def ^:private ps-rid "ri.sutra.main.object-type.11111111-1111-4111-8111-111111111111")
(def ^:private ks-rid "ri.sutra.main.object-type.22222222-2222-4222-8222-222222222222")
(def ^:private lt-rid "ri.sutra.main.link-type.33333333-3333-4333-8333-333333333333")

(def ^:private unlinked
  {:object-types
   {ps-rid {:id ps-rid :object-type-name :stundenbuchung :primary-key :buchung-id :title :buchung-id
            :match-keys [:buchung-id] :plural "Stundenbuchungen"
            :attribute-list [{:attribute-name :buchung-id :attribute-type :string}
                             {:attribute-name :station-nr :attribute-type :string
                              :opts {:fk :station}}]}
    ks-rid {:id ks-rid :object-type-name :station :primary-key :station-code :title :bezeichnung
            :match-keys [:station-code] :plural "Stationen"
            :attribute-list [{:attribute-name :station-code :attribute-type :string}
                             {:attribute-name :bezeichnung :attribute-type :string}]}}})

(def ^:private linked
  (assoc unlinked :link-types
         {lt-rid {:link-type-name :stundenbuchung-station
                  :from-object-type ps-rid
                  :to-object-type   ks-rid
                  :cardinality :many-to-one}}))

(defn- fk-findings [schema]
  (filter #(= :fk-without-link (:rule %)) (lint/run-lint schema (spec/vocabulary))))

(deftest a-declared-fk-without-a-link-is-flagged
  (let [f (first (fk-findings unlinked))]
    (is (some? f))
    (is (= "stundenbuchung.station-nr" (:entity f)))
    (is (= :error (:severity f)))
    (is (re-find #"no link-type connects" (:message f)))
    nil))

(deftest the-link-clears-it
  (testing "the FK attribute stays — it is what BACKS the link; only the missing link was the fault"
    (is (empty? (fk-findings linked)))))

(deftest an-fk-naming-nothing-is-flagged-too
  (let [f (first (fk-findings (assoc-in unlinked
                                        [:object-types ps-rid :attribute-list 1 :opts :fk]
                                        :stationen)))]
    (is (some? f))
    (is (re-find #"is not an object type" (:message f)))))

(deftest an-undeclared-fk-is-silent
  (testing "the rule fires on a DECLARATION, never on a name that merely looks like a key —
            `station-nr` does not equal the target PK `station-code`, so inference would
            have missed this very case and hit 25 coincidences elsewhere"
    (is (empty? (fk-findings (update-in unlinked
                                        [:object-types ps-rid :attribute-list 1]
                                        dissoc :opts))))))

(deftest it-is-gated
  (is (contains? lint/gated-rules :fk-without-link))
  (is (seq (lint/gate-violations unlinked (spec/vocabulary))))
  (is (false? (boolean (kernel/accept? unlinked)))
      "the commit gate refuses the schema")
  (is (empty? (lint/value-gate-violations linked (spec/vocabulary)))
      "as a value the linked schema is clean"))

(def ^:private joining-app
  {:applications
   {"rid-app" {:application-name :stunden-je-station
               :layout {:kind :rows
                        :vars {:ph   {:aggregate {:find :stundenbuchung
                                                  :group-by [:station-nr]}}
                               :rows {:source :station
                                      :join {:index :ph :on :station-code :key :station-nr
                                             :as [:stunden]}}}
                        :items [{:widget :object-table :rows {:val :rows}}]}}}})

(defn- join-findings [schema]
  (filter #(= :app-client-join (:rule %)) (lint/run-lint schema (spec/vocabulary))))

(deftest a-client-join-var-is-flagged
  (let [f (first (join-findings joining-app))]
    (is (some? f))
    (is (= "stunden-je-station.rows" (:entity f)))
    (is (= :applications (:entity-class f)))
    (is (= :error (:severity f)))
    (is (re-find #"station" (:message f)))
    (is (re-find #"group-by \[\[:link" (:message f)))))

(deftest a-plain-var-is-silent
  (is (empty? (join-findings (update-in joining-app
                                        [:applications "rid-app" :layout :vars :rows]
                                        dissoc :join)))))
