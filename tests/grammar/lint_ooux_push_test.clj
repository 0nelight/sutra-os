(ns ^:unit grammar.lint-ooux-push-test
  (:require [sutra.parser.spec :as spec]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [sutra.branching.push :as git-http]))

(def ^:private kunde-rid "ri.sutra.main.object-type.11111111-1111-4111-8111-111111111111")

(def ^:private live
  {:object-types {kunde-rid {:id kunde-rid :object-type-name :halter
                             :match-keys [:halter-id] :title-key :name
                             :attribute-list [{:attribute-name :halter-id :attribute-type :string}
                                              {:attribute-name :name :attribute-type :string}]}}})

(def ^:private kunde-authored
  {:match-keys [:halter-id] :title :name
   :attributes [[:halter-id :string] [:name :string]]})

(defn- advisories [objects & [actions]]
  (git-http/ooux-advisories (cond-> {:objects objects} actions (assoc :actions actions)) live
                            (spec/vocabulary)))

(deftest an-unchanged-type-stays-silent
  (is (empty? (advisories {:halter kunde-authored}))
      "re-stating halter as it lives says nothing, however far from OOUX it is"))

(deftest a-changed-type-is-advised
  (let [as (advisories {:halter (update kunde-authored :attributes conj [:ort :string])})]
    (is (some #(str/starts-with? % "halter — ") as))
    (is (some #(re-find #"no :name" %) as))))

(deftest an-added-type-is-advised-and-its-neighbours-are-not
  (let [as (advisories {:halter   kunde-authored
                        :arbeitsauftrag {:match-keys [:auftrag-nr] :title :auftrag-nr
                                  :attributes [[:auftrag-nr :string] [:bezeichnung :string]]}})]
    (is (some #(re-find #"^arbeitsauftrag — .*titled by \"auftrag-nr\"" %) as))
    (is (not-any? #(str/starts-with? % "halter") as))))

(deftest a-broken-lifecycle-on-a-changed-type-is-said-not-gated
  (let [as (advisories {:halter (assoc kunde-authored
                                      :attributes
                                        [[:halter-id :string] [:name :string] [:stand :string]]
                                      :lifecycle {:property :stand :states [:neu :aktiv]
                                                  :transitions [{:action :aktivieren :from [:neu]
                                                                 :to :fertig}]})}
                       {:aktivieren {:params {:halter {:type :halter :required true}}
                                     :rules [{:op :update :object-type :halter
                                              :target [:get :halter :id] :values {}}]}})]
    (testing "the advisory carries the lifecycle problem"
      (is (some #(re-find #"Lifecycle of \"halter\".*unknown states" %) as)))))
