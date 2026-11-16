(ns ^:unit platform.partial-install-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.parser.resolve :as res]
            [sutra.parser.sugar :as sugar]
            [sutra.authoring.stage :as stage]
            [sutra.authoring.install.plan :as plan]))

(def ^:private live-world
  {:object-types
   {"ot_geraet" {:object-type-name :geraet
                 :attribute-list [{:attribute-name :nr :attribute-type :string}]
                 :plural "Geraete"}
    "ot_dienst" {:object-type-name :dienst
                 :attribute-list [{:attribute-name :nutzungen :attribute-type :integer}]
                 :plural "Dienste"}
    "ot_log"    {:object-type-name :nachweis-setzen-log
                 :attribute-list [{:attribute-name :log-id :attribute-type :string}
                                  {:attribute-name :action :attribute-type :string}
                                  {:attribute-name :at :attribute-type :string}
                                  {:attribute-name :user-id :attribute-type :string}]}}
   :link-types
   {"lt_dienst" {:link-type-name :dienst-am-rechner
                 :from-object-type "ot_dienst" :to-object-type "ot_geraet"
                 :cardinality :many-to-one
                 :reverse-api-name :dienste-am-rechner}}})

(def ^:private one-type
  {:objects {:geraet {:description "ein Gerät"
                      :attributes [[:nr :string]
                                   [:angebote :integer
                                    {:derived
                                       {:expression
                                          [:connected-sum :dienste-am-rechner :nutzungen]}}]]
                      :plural "Geraete"}}})

(deftest a-rollup-over-an-api-named-side-resolves-on-a-partial-install
  (testing "a `:connected-*` names a link SIDE of the type; a side is readable only
            from the link's ends, so without the live ends the only text that can be
            pushed is one that drags the link declarations along"
    (is (seq (res/explain (sugar/lower-schema one-type)
                          {:object-names #{:geraet :dienst} :link-names #{:dienst-am-rechner}}))))
  (testing "the live ends are lent, and the side resolves"
    (is (nil? (res/explain (sugar/lower-schema one-type) (plan/live-ref-symbols live-world)))))
  (testing "the lent ends are the live link types, endpoints as names"
    (is (= {:dienst-am-rechner {:from :dienst :to :geraet :cardinality :many-to-one
                                :opts {:reverse-api-name :dienste-am-rechner}}}
           (:links (plan/live-ref-symbols live-world)))))
  (testing "a side no link has is still unresolved — lending is not loosening"
    (is (seq (res/explain (sugar/lower-schema
                           (assoc-in one-type
                                     [:objects :geraet :attributes 1 2 :derived :expression]
                                     [:connected-sum :dienste-am-drucker :nutzungen]))
                          (plan/live-ref-symbols live-world))))))

(def ^:private one-action
  {:actions {:nachweis-setzen
             {:description "Berichtigt die Beweislage"
              :params {:log-at {:exposure :hidden :prefill :now}
                       :log-by {:exposure :hidden :prefill :current-user}
                       :log-id {:exposure :hidden :prefill :uuid}}
              :rules [{:object-type :geraet :op :update :set [:nr] :target [:get :nr]}
                      {:object-type :nachweis-setzen-log :op :create :set [:log-id]
                       :values {:action "nachweis-setzen" :at [:get :log-at]
                                :user-id [:get :log-by]}}]}}})

(defn- lowers? [live]
  (try (sugar/lower-schema-against one-action live) true
       (catch clojure.lang.ExceptionInfo _ false)))

(deftest a-parameter-derives-its-type-from-the-live-log-type
  (let [live (#'stage/live-objects live-world)]
    (testing "the log type of an action is a live object type like any other — a text
              holding the action does not hold it, and :log-id has its type from there"
      (is (contains? live :nachweis-setzen-log))
      (is (lowers? live)))
    (testing "without it the push refuses the action it did not change"
      (is (not (lowers? (dissoc live :nachweis-setzen-log)))))))
