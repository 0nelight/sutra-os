(ns ^:api apps.action-param-format-test
  (:require [sutra.catalog.grants :as grants]
            [support.wire :refer [commit! ok? stage!]]
            [sutra.access.principals :as pr]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [support.pod :as pod]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]
            [sutra.types.paths :as paths]
            [sutra.types.rid :as rid]
            [sutra.types.submission :as csub]))

(use-fixtures :once pod/with-mem-pod-auth)

(deftest das-browser-schema-gibt-zahlenparametern-ihr-format
  (let [tok (-> (api/post! :login {:username grants/admin-username
                                   :password pr/default-admin-password})
                :body :token)]
    (tx/with-token tok
      (is (ok? (stage! :create-project {:name :werft})))
      (stage! :create-value-type {:name :eur-betrag :type :decimal
                                  :format {:kind :currency :currency "EUR" :decimals 2}})
      (stage! :create-object-type
              {:name :befundlesung :project :werft
               :primary-key :kosten
               :attributes [[:kosten :double {:name "Kosten"
                                             :format {:currency "EUR" :decimals 2
                                                      :grouping true :kind :currency}}]]
               :plural "Befundlesungen"})
      (stage! :create-object-type
              {:name :kostenvoranschlag :project :werft
               :primary-key :kosten :attributes [[:kosten :decimal {:value-type :eur-betrag}]]
               :plural "Kostenvoranschläge"})
      (stage! :create-object-type
              {:name :arbeitskarte :project :werft :primary-key :titel
               :attributes [[:titel :string]]
               :plural "Arbeitskarten"})
      (is (ok? (commit!)))
      (stage! :create-action
              {:name :kosten-aus-befund-erfassen
               :params {:karte {:name "Karte" :type :arbeitskarte :prefill :current-object
                                :required true}
                        :lesung {:type :befundlesung :exposure :hidden :required true}
                        :kosten {:name "Kosten" :type :number
                                 :default {:from :lesung :property :kosten}}}
               :rules [{:object-type :kostenvoranschlag :op :create :set [:kosten]}]})
      (is (ok? (commit!)))
      (let [snap (:body (tx/send! :get (paths/url :snapshot)))
            nk   (rid/name-keyed (:schema snap))
            a    (get-in nk [:actions :kosten-aus-befund-erfassen])]
        (is (string? (:object-type (first (:rules a))))
            "precondition: the rule names its type by RID, as on prod")
        (is (= {:kind :currency :currency "EUR" :decimals 2}
               (:kosten (csub/param-formats nk a)))
            "the rule target :kostenvoranschlag.netto (value type :eur-betrag) wins over the prefill source")
        (testing "without a formatted target, the prefill source answers"
          (let [a' (assoc a :rules [])]
            (is (= {:currency "EUR" :decimals 2 :grouping true :kind :currency}
                   (:kosten (csub/param-formats nk a'))))))))))
