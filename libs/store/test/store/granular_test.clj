(ns ^:unit store.granular-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.store.api :as api]
            [sutra.store.conformance :as store-conf]
            [sutra.store.core]
            [sutra.store.mem :as mem]
            [sutra.types.granular :as granular]))

(def ^:private secured
  (assoc-in store-conf/world [:object-types store-conf/widget :security]
            {:policy            [:= [:user :username] [:property :owner]]
             :property-policies {:sizes {:properties [:size] :markings [:metrics]}}}))

(defn- db []
  (doto (sutra.store.core/db (mem/engine) {})
    sutra.store.core/adopt!
    (api/transact! {:basis {:branch "main" :rev 0} :actor {:principal "t"}
                    :schema {:authored secured :migration []} :ops store-conf/seed})))

(defn- as [who cleared]
  {:clearances      #{}
   :denied-markings (if cleared #{} #{:metrics})
   :subject         (granular/subject {:id who :username who})})

(defn- read-as [db authority q]
  (with-open [s (api/snapshot db {:branch "main"})] (api/read s authority q)))

(defn- names [rows] (set (map (comp :name :values) rows)))

(deftest a-policy-admits-the-members-of-a-group-and-no-one-else
  (let [db (doto (sutra.store.core/db (mem/engine) {})
             sutra.store.core/adopt!
             (api/transact! {:basis {:branch "main" :rev 0} :actor {:principal "t"}
                             :schema {:authored
                                      (assoc-in store-conf/world
                                                [:object-types store-conf/widget :security]
                                                {:policy [:intersects [:user :groups] #{"g"}]})
                                      :migration []}
                             :ops store-conf/seed}))
        as-groups (fn [& gs] {:clearances #{}
                              :subject (granular/subject {:id "u" :username "u" :groups gs})})]
    (is (= 4 (count (read-as db (as-groups "g" "h") [:select {:root store-conf/widget}]))))
    (is (empty? (read-as db (as-groups "h") [:select {:root store-conf/widget}])))
    (is (empty? (read-as db (as-groups) [:select {:root store-conf/widget}])))))

(deftest the-object-security-policy-binds-every-read
  (let [db (db)]
    (testing "select, by-id read and count answer only the rows the policy admits"
      (is (= #{"alpha" "beta"}
             (names (read-as db (as "ava" true) [:select {:root store-conf/widget}]))))
      (is (= #{"alphabet"}
             (names (read-as db (as "bob" true) [:objects store-conf/widget nil]))))
      (is (= 2 (read-as db (as "ava" true) [:objects/count store-conf/widget])))
      (is (empty? (read-as db (as "ava" true)
                           [:objects store-conf/widget [(store-conf/id 2)]]))))
    (testing "a row with a null policy field is no one's, and no subject passes nothing"
      (is (not (contains? (names (read-as db (as "ava" true)
                                          [:select {:root store-conf/widget}]))
                          "gamma")))
      (is (empty? (read-as db {:clearances #{}} [:select {:root store-conf/widget}]))))
    (testing "a filter cannot reach a guarded property it may not read"
      (is (empty? (read-as db (as "ava" false)
                           [:select {:op :filter :in {:op :scan :root store-conf/widget}
                                     :where [:= :size 10]}]))))
    (testing "the property policy blanks what the marking guards, and nothing else"
      (is (every? #(nil? (get-in % [:values :size]))
                  (read-as db (as "ava" false) [:select {:root store-conf/widget}])))
      (is (= #{10 30}
             (set (map (comp :size :values)
                       (read-as db (as "ava" true) [:select {:root store-conf/widget}]))))))
    (testing "a service read and an unrestricted principal are exempt"
      (is (= 4 (count (read-as db {:system? true} [:select {:root store-conf/widget}]))))
      (is (= 4 (count (read-as db {:clearances :all} [:select {:root store-conf/widget}])))))
    (testing "a type without a policy reads as before"
      (is (= 1 (count (read-as db (as "ava" true) [:select {:root store-conf/other}])))))))
