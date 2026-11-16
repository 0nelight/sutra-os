(ns ^:unit platform.orphan-table-cleanup-test
  (:require [sutra.ref.install :as ri]
            [sutra.ref.clock :as clock]
            [clojure.test :refer [deftest is testing]]
            [clojure.set :as set]
            [sutra.catalog.lookup :as lookup]
            [sutra.types.naming :as naming]
            [sutra.catalog.signature :as signature]
            [sutra.types.world-rules :as rules]
            [sutra.authoring.http.oms :as oms]
            [sutra.store.pg.layout :as layout]
            [sutra.types.rid :as rid]))

(defn- ot [nm]
  {:object-type-name nm :attribute-list [{:attribute-name :name :attribute-type :string}]})

(defn- schema [link transform]
  {:object-types {:kunde (ot :kunde) :projekt (ot :projekt)}
   :link-types   {link {:link-type-name link :from-object-type :projekt :to-object-type :kunde
                        :cardinality :many-to-one}}
   :transforms   {transform {:language :pg-sql :query "SELECT 1"
                             :output {:kind :link-type :link-type link}}}
   :datasets     {:raw-rechnungen {:kind :standalone
                                   :columns [{:attribute-name :nr :attribute-type :string}]}}})

(def ^:private authored (schema :projekt-kunde :link-projekt-kunde))

(def ^:private W (ri/derived {:sigma clock/system} authored))

(defn- claimed
  [W]
  (into (set (keys (layout/declared-tables (rules/signature W))))
        (keys (signature/datasets-of W))))

(defn- unclaimed [old new] (set/difference (claimed old) (claimed new)))

(defn- dataset-table [W section nm]
  (naming/dataset-table (rid/dataset-rid-for (lookup/rid-of W section nm))))

(deftest removing-a-dataset-strands-its-table
  (testing "a removed standalone dataset"
    (is (= #{(naming/dataset-table (lookup/rid-of W :datasets :raw-rechnungen))}
           (unclaimed W
                      (ri/derived {:sigma clock/system}
                                  (update authored :datasets dissoc :raw-rechnungen) W)))))
  (testing "a removed transform"
    (is (= #{(dataset-table W :transforms :link-projekt-kunde)}
           (unclaimed W
                      (ri/derived {:sigma clock/system}
                                  (update authored :transforms dissoc :link-projekt-kunde) W))))))

(deftest a-rid-re-mint-strands-the-old-pair
  (let [re-minted (ri/derived {:sigma clock/system}
                              (schema :projekt-kunde-v2 :link-projekt-kunde-v2) W)]
    (is (= #{(layout/link-table (lookup/rid-of W :link-types :projekt-kunde))
             (dataset-table W :transforms :link-projekt-kunde)}
           (unclaimed W re-minted)))))

(deftest orphan-storage-does-not-gate-a-commit
  (is (empty? (oms/blocking-drift {:orphan-tables ["d_dead" "c_dead"]}))
      "an orphan table alone leaves the schema committable")
  (is (seq (oms/blocking-drift {:orphan-tables  ["d_dead"]
                                :stale-link-fks [{:link :x}]}))
      "a stale link FK still blocks, orphans alongside it or not")
  (is (seq (oms/blocking-drift {:tableless-types {:object-types ["x"]}}))
      "a tableless type still blocks"))
