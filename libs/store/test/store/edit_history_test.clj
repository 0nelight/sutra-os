(ns ^:unit store.edit-history-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.store.api :as api]
            [sutra.store.conformance :as store-conf]
            [sutra.store.core]
            [sutra.store.core.kv]
            [sutra.store.mem :as mem]
            [sutra.store.world :as world]
            [sutra.types.granular :as granular]))

(def ^:private widget store-conf/widget)

(def ^:private one (store-conf/id 1))

(def ^:private secured
  (assoc-in store-conf/world [:object-types widget :security]
            {:policy            [:= [:user :username] [:property :owner]]
             :property-policies {:sizes {:properties [:size] :markings [:metrics]}}}))

(defn- head [db]
  (with-open [s (api/snapshot db {:branch "main"})] (api/basis s)))

(defn- seeded [w]
  (doto (mem/store mem/empty-seed {:collections store-conf/collections})
    (api/transact! {:basis {:branch "main" :rev 0} :actor {:principal "t"} :origin :index
                    :schema {:authored w :migration []} :ops store-conf/seed})))

(defn- edit!
  ([db values] (edit! db one values {:principal "ava"}))
  ([db id values actor]
   (api/transact! db {:basis (head db) :actor actor
                      :ops [{:op :update-object :type widget
                             :payload {:id id :values values}}]})))

(defn- world-of [db] (world/at {:db db :clock #(java.util.Date.)} "main"))

(defn- as [who cleared]
  {:clearances      #{}
   :denied-markings (if cleared #{} #{:metrics})
   :subject         (granular/subject {:id who :username who})})

(deftest an-edit-keeps-what-it-replaced
  (let [db (seeded store-conf/world)]
    (edit! db {:name "zwei" :size 11})
    (edit! db {:name "drei"})
    (let [[newest oldest] (world/edit-history (world-of db) widget {:pk one})]
      (is (= {:name "zwei"} (:previous-values newest)))
      (is (= {:name "alpha" :size 10} (:previous-values oldest))
          "the value from the source, which no earlier edit wrote"))))

(deftest the-commit-names-the-action-and-the-agent-run
  (let [db (seeded store-conf/world)]
    (edit! db one {:name "x"} {:principal "ava" :action "rename" :action-id "a1"
                               :agent "helfer" :agent-run "r1"})
    (is (= {:action "rename" :action-id "a1" :agent "helfer" :agent-run "r1"}
           (select-keys (first (world/edit-history (world-of db) widget {:pk one}))
                        [:action :action-id :agent :agent-run])))))

(deftest pages-stand-still-while-edits-arrive
  (let [db (seeded store-conf/world)
        w  (world-of db)
        _  (doseq [n (range 5)] (edit! db {:name (str "v" n)}))
        names #(mapv (comp :name :edited-values) (:history %))
        p1 (world/edit-history-page w widget {:pk one :limit 2})
        _  (edit! db {:name "neu"})
        p2 (world/edit-history-page w widget {:pk one :limit 2 :token (:next-token p1)})
        p3 (world/edit-history-page w widget {:pk one :limit 2 :token (:next-token p2)})]
    (is (= ["v4" "v3"] (names p1)))
    (is (= ["v2" "v1"] (names p2)) "the new edit does not shift the second page")
    (is (= ["v0"] (names p3)))
    (is (= 5 (:total p3)))
    (is (nil? (:next-token p3)))
    (testing "oldest first starts at the oldest edit, not at the newest page turned round"
      (is (= ["v0" "v1"] (names (world/edit-history-page w widget {:pk one :limit 2
                                                                   :oldest-first? true})))))
    (testing "a token this read did not give is refused"
      (is (thrown? Exception (world/edit-history-page w widget {:pk one :token "kaputt"}))))))

(deftest filters-apply-before-the-limit
  (let [db (seeded store-conf/world)
        w  (world-of db)]
    (edit! db {:size 11})
    (edit! db {:name "x"})
    (edit! db one {:name "y"} {:principal "bob"})
    (testing "the edit of a chosen property is found past edits of others"
      (let [{:keys [history total]} (world/edit-history-page w widget {:pk one :limit 1
                                                                       :props [:size]})]
        (is (= 1 total))
        (is (= [{:size 11}] (map :edited-values history)))))
    (testing "a property no edit touched has no edits"
      (is (= {:history [] :total 0}
             (world/edit-history-page w widget {:pk one :props [:owner]}))))
    (testing "by editor and kind"
      (is (= 1 (:total (world/edit-history-page w widget {:pk one :editors ["bob"]}))))
      (is (= 0 (:total (world/edit-history-page w widget {:pk one :kinds ["delete"]})))))))

(deftest the-history-is-read-as-the-reader-may
  (let [db (seeded secured)
        w  (world-of db)
        _  (edit! db {:name "x" :size 11})
        read-as #(world/edit-history w widget {:pk one :authority %})]
    (testing "the object security policy: bob may not read ava's object"
      (is (empty? (read-as (as "bob" true))))
      (is (empty? (world/edit-provenance w widget {:authority (as "bob" true)}))))
    (testing "the property policy blanks the new and the old value"
      (let [[e] (read-as (as "ava" false))]
        (is (= {:name "x"} (:edited-values e)))
        (is (= {:name "alpha"} (:previous-values e)))))
    (testing "cleared, both are there"
      (let [[e] (read-as (as "ava" true))]
        (is (= {:name "x" :size 11} (:edited-values e)))
        (is (= {:name "alpha" :size 10} (:previous-values e)))))
    (testing "a service read names no authority"
      (is (= 1 (count (world/edit-history w widget {:pk one})))))))

(deftest a-denied-marking-hides-the-history-of-its-object
  (let [db     (seeded store-conf/world)
        w      (world-of db)
        secret (store-conf/id 11)
        deny   {:denied-markings #{:secret}}]
    (api/transact! db {:basis (head db) :actor {:principal "t"}
                       :ops [(assoc-in (store-conf/create widget 11 {:name "s"})
                                       [:payload :markings] [:secret])]})
    (edit! db secret {:name "t"} {:principal "ava"})
    (is (empty? (world/edit-history w widget {:pk secret :authority deny})))
    (is (= 2 (count (world/edit-history w widget {:pk secret :authority {:clearances :all}}))))
    (testing "a hidden type answers nothing"
      (is (empty? (world/edit-history w widget {:pk secret
                                                :authority {:hidden-object-types #{widget}}}))))))

(deftest warming-the-history-answers-nothing
  (let [db (seeded store-conf/world)]
    (edit! db {:name "x"})
    (is (nil? (world/warm-history! (world-of db))))))

(deftest forgetting-edits-does-not-rewrite-the-history
  (let [db (seeded store-conf/world)
        w  (world-of db)]
    (edit! db {:name "x"})
    (let [before (world/edit-history w widget {:pk one})]
      (world/forget-edits! w [widget])
      (is (empty? (world/edited w widget)) "the overlay is gone")
      (is (= before (world/edit-history w widget {:pk one}))
          "the audit trail stands"))))
