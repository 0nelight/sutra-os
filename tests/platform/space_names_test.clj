(ns ^:api platform.space-names-test
  (:require [sutra.catalog.grants :as grants]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.access.principals :as pr]
            [support.pod :as pod]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(use-fixtures :each pod/with-mem-pod-auth)

(defn- named [schema section nm]
  (filter (fn [[_ s]] (= (name nm) (some-> (:object-type-name s) name))) (get schema section)))

(deftest two-spaces-each-own-a-name
  (tx/with-token (-> (api/post! :login
                                {:username grants/admin-username
                                 :password pr/default-admin-password})
                     :body :token)
    (let [r1 (api/post! :install
                        {:schema {:spaces   {:flug {} :controlling {}}
                                  :projects {:wartung {:space :flug} :kosten {:space :controlling}}
                                  :objects
                                    {:mangel {:project :wartung :primary-key :id
                                              :attributes [[:id :string]]
                                              :plural "Mängel"}}}})]
      (is (= 200 (:status r1)) (pr-str (:body r1)))
      (is (= 200 (:status (api/post! :staging)))))
    (let [r2 (api/post! :install
                        {:schema {:spaces   {:flug {} :controlling {}}
                                  :projects {:wartung {:space :flug} :kosten {:space :controlling}}
                                  :objects
                                    {:mangel {:project :kosten :primary-key :nr
                                              :attributes [[:nr :string]]
                                              :plural "Mängel"}}}})]
      (is (= 200 (:status r2)) (pr-str (:body r2)))
      (let [c (api/post! :staging)] (is (= 200 (:status c)) (pr-str (:body c)))))
    (let [schema (:body (api/get! :schema))
          ms     (named schema :object-types :mangel)
          proj   (fn [[_ s]] (some-> (get-in schema [:projects (:project s) :project-name]) name))]
      (testing "two types of one name, one in each space's project"
        (is (= 2 (count ms)) (pr-str (map (comp :project val) ms)))
        (is (= #{"wartung" "kosten"} (set (map proj ms))))))))
