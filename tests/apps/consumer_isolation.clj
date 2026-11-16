(ns ^:integration apps.consumer-isolation
  (:require [support.system :as system]
            [support.install :as inst]
            [clojure.test :refer [deftest use-fixtures]]
            [sutra.store.seat]
            [sutra.store.access.scope :as ctx]
            [sutra.store.access.substrate :as sub]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.lookup :as lookup]
            [sutra.catalog.system-datasets :as sysds]
            [support.helpers :refer [check section]]
            [support.pod :as pod]
            [support.store :as store]))

(use-fixtures :once pod/with-mem-stores-only)

(defn- uuid [] (str (java.util.UUID/randomUUID)))

(defn- visible [auth branch T] (sub/read-type (ctx/store branch auth) T))

(deftest consumer-type-isolation
  (let [branch (str "iso-" (subs (uuid) 0 8))]
    (store/fork! branch)
    (try
      (let [run (fn [world rules] (inst/run-installing! branch world rules))
            empty-w {:object-types {} :link-types {} :interfaces {}
                     :transforms {} :actions {} :object-sets {}}]

        (section "install an identity type (:user) alongside ordinary data (:mechaniker)")
        (let [w (run empty-w
                     [{:op :create-object-type :name :user :primary-key :uid
                       :attributes [[:uid :string] [:rolle :string]]
                       :plural "Users"}
                      {:op :create-object-type :name :mechaniker :primary-key :name
                       :attributes [[:name :string]]
                       :plural "Mechaniker"}])
              user-rid   (lookup/rid-of w :object-types :user)
              mechaniker-rid (lookup/rid-of w :object-types :mechaniker)]
          (system/seat! branch)
          (store/insert-object! branch user-rid   {:id (uuid) :values {:rolle "consumer"}})
          (store/insert-object! branch mechaniker-rid {:id (uuid) :values {:name "Jannik"}})

          (section "baseline — with nothing hidden, both types are readable")
          (check ":user readable"   (= 1 (count (visible grants/system-authority branch user-rid))))
          (check ":mechaniker readable"
                 (= 1 (count (visible grants/system-authority branch mechaniker-rid))))

          (section "hide :user — it reads as empty; :mechaniker is unaffected")
          (let [hiding (assoc grants/system-authority :hidden-object-types #{user-rid})]
            (check ":user is hidden — empty, no count, no leak"
                   (= [] (visible hiding branch user-rid)))
            (check ":mechaniker still fully readable"
                   (= 1 (count (visible hiding branch mechaniker-rid)))))

          (section "the hidden set is the platform's identity model")
          (check ":user + :marking are the declared system types"
                 (= #{:user :marking} sysds/system-object-types))))
      (finally
        (system/unseat!)
        (try (store/retire! branch) (catch Exception _ nil))))))
