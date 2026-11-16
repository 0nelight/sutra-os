(ns ^:unit platform.grants-meet-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.catalog.grants :as grants]))

(def ^:private editor
  {:user "u1" :operations #{:read :update} :clearances #{:pii}
   :project-operations {"ri.p.a" #{:read :create} "ri.p.b" #{:read}}
   :hidden-object-types #{"ri.ot.h"}})

(def ^:private samples
  [nil {} editor grants/system-authority
   {:operations #{:read} :project-operations {"ri.p.a" #{:delete}} :clearances #{}}
   {:operations grants/all :project-operations {"ri.p.b" #{:read :update}}
    :denied-markings #{:secret} :blocked-resources #{"ri.ot.x"}}])

(def ^:private ops [:read :create :update :delete :manage-permissions])

(def ^:private projects [nil "ri.p.a" "ri.p.b" "ri.p.c"])

(defn- allows? [auth p op] (grants/allows? auth p false op))

(defn- pairs [] (for [p projects op ops] [p op]))

(defn- allows-all? [auth] (every? (fn [[p op]] (allows? auth p op)) (pairs)))

(defn- allows-none? [auth] (not-any? (fn [[p op]] (allows? auth p op)) (pairs)))

(deftest a-missing-axis-allows-nothing
  (doseq [auth [nil {} {:user "u"}]]
    (is (allows-none? auth) (pr-str auth))
    (is (not (grants/cleared? (grants/clearances auth) [:pii])))
    (is (not (grants/all-projects? auth)))
    (is (false? ((grants/owner-of auth) "ri.p.a")))))

(deftest all-is-said-not-left-out
  (is (allows-all? grants/system-authority))
  (is (grants/all-projects? grants/system-authority))
  (is (grants/cleared? (grants/clearances grants/system-authority) [:pii :secret]))
  (is (true? ((grants/owner-of grants/system-authority) "ri.p.zz"))))

(deftest a-project-grant-adds-to-the-own-operations
  (is (allows? editor "ri.p.a" :create))
  (is (not (allows? editor "ri.p.b" :create)))
  (is (allows? editor "ri.p.c" :update) "the own operations hold in every project")
  (is (not (grants/allows? editor "ri.p.a" true :create)) "ignore-inherited drops the grant"))

(deftest meet-never-grants-more-than-either-side
  (doseq [a samples b samples
          :let [m (grants/meet a b)]]
    (testing (pr-str [a b])
      (doseq [p projects op ops]
        (when (allows? m p op)
          (is (and (allows? a p op) (allows? b p op)) [p op])))
      (doseq [mk [[:pii] [:secret] []]]
        (when (grants/cleared? (grants/clearances m) mk)
          (is (and (grants/cleared? (grants/clearances a) mk)
                   (grants/cleared? (grants/clearances b) mk)))))
      (is (every? (grants/hidden-object-types m) (grants/hidden-object-types a)))
      (is (every? (grants/hidden-object-types m) (grants/hidden-object-types b)))
      (is (every? (grants/denied-markings m) (grants/denied-markings b)))
      (is (every? (grants/blocked-resources m) (grants/blocked-resources b))))))

(deftest all-is-the-identity-of-meet
  (let [m (grants/meet grants/system-authority editor)]
    (is (= (grants/operations editor) (grants/operations m)))
    (is (= (grants/project-operations editor) (grants/project-operations m)))
    (is (not (:system? m)) "only two systems meet as the system")))

(deftest a-stored-row-keeps-its-old-meaning
  (testing "an outbox entry from before explicit axes: {:system? true} was everything"
    (is (grants/all-projects? (grants/stored {:system? true})))
    (is (allows-all? (grants/stored {:system? true}))))
  (testing "what a row said, it keeps"
    (is (= #{:read} (grants/operations (grants/stored {:operations #{:read}})))))
  (is (nil? (grants/stored nil)) "an absent authority stays no one"))
