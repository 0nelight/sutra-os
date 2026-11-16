(ns ^:unit platform.instance-facts-test
  (:require [clojure.test :refer [deftest is]]
            [sutra.cli.instance :as inst]
            [sutra.serve.instance :as instance]))

(deftest the-description-names-roles-without-values
  (let [d (instance/describe
           {:routes        [:b :a]
            :identity      {:ldap {:label "LDAP" :source "spnego" :ready? any? :password any?
                                   :directory any?
                                   :settings
                                     {:bind-pw {:key "ldap_bind_pw" :type :string :secret? true}}}}
            :offers        {:z {:name "Z" :description "" :order 2}
                            :y {:name "Y" :description "" :order 1 :requires [:z]}}
            :catalogues    {:xai {:host "x.ai" :models-path "m" :parse any?}}
            :source-types  {:rest {:label "REST"}}
            :profiles      {:postgres {:capabilities #{:sql} :endpoint :jdbc-url :scan any?}}
            :policy-fields [{:key :protected :kind :bool :default false}]})]
    (is (= [:a :b] (:routes d)))
    (is (= [{:id :ldap :label "LDAP" :source "spnego" :can [:password :directory]
             :settings {:bind-pw {:type :string :secret? true}}}]
           (:identity-providers d))
        "no storage key, no function")
    (is (= [:y :z] (mapv :id (:product-offers d))) "offers in their order")
    (is (= [{:id :xai :host "x.ai"}] (:llm-catalogues d)))
    (is (= {:postgres {:capabilities #{:sql} :endpoint :jdbc-url}} (:connector-profiles d)))))

(deftest drift-names-what-one-side-lacks
  (is (= {:server-only [:new] :cli-only [:gone]}
         (inst/drift [:a :new] [:a :gone :plug] #{:plug}))
      "a route a plugin brings is no drift when the plugin is absent"))
