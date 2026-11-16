(ns ^:unit apps.delivery-restricted-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.apps.http.delivery :as delivery]
            [sutra.objects.api :as objects]))

(def ^:private rid "ri.ot.luefter")

(def ^:private schema
  {:object-types {rid {:object-type-name :luefter :project "ri.p.gt"
                       :attribute-list []}}})

(def ^:private specs
  {:hk {:source :luefter}
   :n  {:agg [:count] :of :hk}
   :s  {:agg [:sum :laenge-m] :of :hk}
   :mx {:agg [:max :laenge-m] :of :hk}})

(defn- denied [& _]
  (throw (ex-info "not cleared" {:sutra/acl-denied true :resource rid})))

(defn- broken [& _] (throw (ex-info "store down" {})))

(defn- answers [hidden]
  (delivery/view-answers nil schema hidden [{:kind :rows :vars specs}] {} {}))

(deftest a-hidden-type-answers-an-empty-set-and-aggregations
  (let [{:keys [answers refused]} (answers #{rid})]
    (is (= [] (:hk answers)))
    (is (= 0 (:n answers)))
    (is (= 0 (:s answers)))
    (is (nil? (:mx answers)))
    (is (empty? refused))))

(deftest an-access-refusal-while-reading-answers-empty
  (with-redefs [delivery/read-page denied
                objects/count-matching denied
                objects/aggregate-matching denied]
    (let [{:keys [answers refused]} (answers #{})]
      (is (= [] (:hk answers)))
      (is (= 0 (:n answers)))
      (is (empty? refused)))))

(deftest any-other-failure-stays-refused
  (with-redefs [delivery/read-page broken
                objects/count-matching broken
                objects/aggregate-matching broken]
    (let [{:keys [answers refused]} (answers #{})]
      (is (not (contains? answers :hk)))
      (is (re-find #"store down" (str (:hk refused))))
      (is (re-find #"store down" (str (:n refused)))))))

(deftest a-filter-over-a-forbidden-set-shows-its-facets-empty
  (let [want [{:prop :stockwerk}]
        facet (fn [hidden]
                (delivery/facet-set nil schema (constantly rid) hidden specs :hk want {} false))]
    (testing "hidden type"
      (is (= [{:type :luefter :total 0 :restricted true
               :facets [{:prop :stockwerk :link nil :buckets [] :cut? false}]}]
             (:parts (facet #{rid})))))
    (testing "refused while counting"
      (with-redefs [objects/count-matching denied]
        (is (= 0 (-> (facet #{}) :parts first :total)))
        (is (true? (-> (facet #{}) :parts first :restricted)))))
    (testing "a real error"
      (with-redefs [objects/count-matching broken]
        (is (re-find #"store down" (str (:refused (facet #{})))))))))

(deftest a-search-over-a-forbidden-set-finds-nothing
  (is (= {:rows [] :total 0 :restricted true}
         (delivery/search-set nil schema (constantly rid) #{rid} specs :hk "rad" nil {} #{})))
  (with-redefs [delivery/read-page denied]
    (is (= {:rows [] :total 0 :restricted true}
           (delivery/search-set nil schema (constantly rid) #{} specs :hk "rad" nil {} #{})))))
