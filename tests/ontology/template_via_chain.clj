(ns ^:integration ontology.template-via-chain
  (:require [support.system :as system]
            [support.install :as inst]
            [clojure.test :refer [deftest is use-fixtures]]
            [sutra.store.seat]
            [sutra.store.access.scope :as ctx]
            [sutra.catalog.grants :as grants]
            [sutra.compute.render :as render]
            [support.pod :as pod]
            [support.store :as store]))

(use-fixtures :once pod/with-mem-stores-only)

(defn- uuid [] (str (java.util.UUID/randomUUID)))

(deftest a-via-chain-walks-three-links-from-one-object
  (let [branch (str "via3-" (subs (uuid) 0 8))]
    (try
      (store/fork! branch)
      (let [w   (inst/run-installing!
                     branch
                     {:object-types {} :link-types {} :interfaces {} :transforms {} :actions {}
                      :object-sets {}}
                     [{:op :create-object-type :name :kundenauftrag :primary-key :nr
                       :attributes [[:nr :string]]
                       :plural "Kundenaufträge"}
                      {:op :create-object-type :name :projekt :primary-key :nr
                       :attributes [[:nr :string] [:auftrag-nr :string]]
                       :plural "Projekte"}
                      {:op :create-object-type :name :arbeitsauftrag :primary-key :nr
                       :attributes [[:nr :string] [:projekt-nr :string]]
                       :plural "Arbeitsaufträge"}
                      {:op :create-object-type :name :zeitbuchung :primary-key :znr
                       :attributes [[:znr :string] [:std :integer] [:arbeitsauftrag-nr :string]]
                       :plural "Zeitbuchungen"}
                      {:op :create-link-type :name :projekt-kundenauftrag
                       :from :projekt :to :kundenauftrag :cardinality :many-to-one}
                      {:op :create-link-type :name :arbeitsauftrag-projekt
                       :from :arbeitsauftrag :to :projekt :cardinality :many-to-one}
                      {:op :create-link-type :name :zeitbuchung-arbeitsauftrag
                       :from :zeitbuchung :to :arbeitsauftrag :cardinality :many-to-one}
                      {:op :create-transform :name :link-projekt-kundenauftrag
                       :input {:kind :object-type :object-type :projekt}
                       :output {:kind :link-type :link-type :projekt-kundenauftrag
                                :fk-attribute :auftrag-nr}}
                      {:op :create-transform :name :link-arbeitsauftrag-projekt
                       :input {:kind :object-type :object-type :arbeitsauftrag}
                       :output {:kind :link-type :link-type :arbeitsauftrag-projekt
                                :fk-attribute :projekt-nr}}
                      {:op :create-transform :name :link-zeitbuchung-arbeitsauftrag
                       :input {:kind :object-type :object-type :zeitbuchung}
                       :output {:kind :link-type :link-type :zeitbuchung-arbeitsauftrag
                                :fk-attribute :arbeitsauftrag-nr}}])
              ot  (fn [nm]
                    (some (fn [[r s]] (when (= nm (:object-type-name s)) r)) (:object-types w)))
              obj! (fn [t vals]
                     (let [id (uuid)] (store/insert-object! branch (ot t) {:id id :values vals})
                       id))
              ka  (obj! :kundenauftrag {:nr "KA-1"})]
          (doseq [[t vals] [[:kundenauftrag {:nr "KA-2"}]
                            [:projekt {:nr "P-1" :auftrag-nr "KA-1"}]
                            [:projekt {:nr "P-2" :auftrag-nr "KA-2"}]
                            [:arbeitsauftrag {:nr "WO-1" :projekt-nr "P-1"}]
                            [:arbeitsauftrag {:nr "WO-2" :projekt-nr "P-1"}]
                            [:arbeitsauftrag {:nr "WO-3" :projekt-nr "P-2"}]
                            [:zeitbuchung {:std 1 :arbeitsauftrag-nr "WO-1"}]
                            [:zeitbuchung {:std 2 :arbeitsauftrag-nr "WO-1"}]
                            [:zeitbuchung {:std 4 :arbeitsauftrag-nr "WO-2"}]
                            [:zeitbuchung {:std 8 :arbeitsauftrag-nr "WO-3"}]]]
            (obj! t vals))
          (system/seat! branch)
          (let [params {:ka     {:type :object :object-type :kundenauftrag}
                        :zeiten {:type :object-set :object-type :zeitbuchung :of :ka
                                 :search-around [:projekt-kundenauftrag :arbeitsauftrag-projekt
                                       :zeitbuchung-arbeitsauftrag]}
                        :einer  {:type :object-set :object-type :projekt :of :ka
                                 :search-around :projekt-kundenauftrag}}
                store     (ctx/store branch grants/system-authority)
                ctx*   (render/bind store w {:parameters params :limits {:max-objects 10}} {:ka ka}
                                    false)]
            (is (= #{1 2 4} (set (map #(get-in % [:values :std] (:std %)) (:zeiten ctx*))))
                "three links from KA-1: its project, both of its orders, their three bookings")
            (is (= 1 (count (:einer ctx*))) "the one-link form still works")
            (is (= 2
                   (count (:zeiten (render/bind store w
                                                {:parameters params :limits {:max-objects 2}}
                                                {:ka ka} false))))
                ":max-objects still caps the walked set")))
      (finally (try (system/unseat!) (store/retire! branch) (catch Exception _ nil))))))
