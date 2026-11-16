(ns ^:unit store.records-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.store.api :as api]
            [sutra.store.mem :as mem]
            [sutra.store.records :as r]
            [sutra.store.world :as world]))

(def ^:private t0 #inst "2026-01-01T00:00:00Z")

(def ^:private job
  {:coll :rt/job :scope :global :key any? :value :map
   :branch? true :key-cols [:branch :id] :touch [:at] :order [[:at :desc]]
   :merge {:first-seen :keep :note :coalesce-new :peak :greatest}
   :index [:id :branch :status [:at :range]]})

(def ^:private entry-log
  {:coll :rt/entry :scope :global :key any? :value :map
   :branch? true :key-cols [:seq] :auto :seq :order [[:seq :asc]]
   :index [[:seq :range] :branch]})

(defn- store
  ([] (store t0))
  ([t] {:db (mem/store mem/empty-seed {:collections [job entry-log]}) :clock (constantly t)}))

(deftest entry-is-pure
  (testing "the same row and time give the same entry; now resolves to the time given"
    (is (= (r/entry job {:id "a" :status r/now} t0) (r/entry job {:id "a" :status r/now} t0)))
    (is (= {:coll :rt/job :key ["main" "a"] :expect api/absent
            :value {:id "a" :branch "main" :status t0 :at t0}}
           (r/entry job {:id "a" :status r/now} t0))))
  (testing "an auto-keyed entry lets the store assign the key"
    (is (= api/auto (:key (r/entry entry-log {:text "x"} t0))))))

(deftest insert-get-list-count
  (let [s (store)]
    (is (= {:id "a" :branch "main" :status "new" :at t0} (r/insert! s job {:id "a" :status "new"})))
    (r/insert! s job {:id "b" :status "done"})
    (r/insert! s job {:id "c" :status "new" :branch "feat"})
    (is (= "new" (:status (r/get s job {:id "a"}))))
    (is (= #{"a" "b"} (set (map :id (r/list s job {})))) "a read without a branch reads main")
    (is (= #{"c"} (set (map :id (r/list s job {:branch "feat"})))))
    (is (= 3 (count (r/list s job {} {:all-branches? true}))))
    (is (= 1 (r/count s job {:status "new"})))
    (is (= {"new" 1 "done" 1} (r/counts s job {} :status)))
    (testing ":records/present asks for any value but nil"
      (r/insert! s job {:id "d"})
      (is (= #{"a" "b"} (set (map :id (r/list s job {:status :records/present})))))
      (is (= #{"d"} (set (map :id (r/list s job {:status nil}))))))
    (testing "an insert under a taken key is refused"
      (is (thrown? clojure.lang.ExceptionInfo (r/insert! s job {:id "a"}))))
    (testing "a where is data, never SQL"
      (is (thrown? clojure.lang.ExceptionInfo (r/list s job {:sql "1=1"}))))))

(deftest auto-keys-rise
  (let [s  (store)
        ks (mapv #(:seq (r/insert! s entry-log {:text (str %)})) (range 3))]
    (is (= ks (sort ks)))
    (is (apply distinct? ks))
    (is (= ["0" "1" "2"] (mapv :text (r/list s entry-log {}))))))

(deftest upsert-merges-by-rule
  (let [s (store)]
    (r/upsert! s job {:id "a" :first-seen 1 :note "n" :peak 5})
    (let [v (r/upsert! s job {:id "a" :first-seen 2 :note nil :peak 3})]
      (is (= 1 (:first-seen v)) ":keep leaves the stored value")
      (is (= "n" (:note v)) ":coalesce-new keeps the stored value over nil")
      (is (= 5 (:peak v)) ":greatest keeps the larger"))
    (testing "{:conflict :nothing} leaves an existing record"
      (is (= 5 (:peak (r/upsert! s job {:id "a" :peak 9} {:conflict :nothing})))))))

(deftest upsert-many-batches
  (let [t1   #inst "2026-02-02T00:00:00Z"
        now  (atom t0)
        s    (assoc (store) :clock #(deref now))
        head #(world/head (world/at s r/trunk))]
    (r/insert-many! s job [{:id "a" :first-seen 1 :note "n" :peak 5} {:id "b" :peak 1}]
                    {:upsert? true})
    (reset! now t1)
    (testing "each row merges by the declaration's rules, as upsert! does"
      (r/insert-many! s job
                      [{:id "a" :first-seen 2 :note nil :peak 3 :status "x"} {:id "c" :peak 7}]
                      {:upsert? true})
      (is (= {:first-seen 1 :note "n" :peak 5 :status "x" :at t1}
             (select-keys (r/get s job {:id "a"}) [:first-seen :note :peak :status :at])))
      (is (= {:peak 1 :at t0} (select-keys (r/get s job {:id "b"}) [:peak :at]))
          "a row not given is left")
      (is (= 7 (:peak (r/get s job {:id "c"})))))
    (testing "rows sharing a key: the last one wins"
      (r/insert-many! s job [{:id "d" :status "x"} {:id "d" :status "y"}] {:upsert? true})
      (is (= "y" (:status (r/get s job {:id "d"})))))
    (testing "rows changing nothing but :touch commit nothing"
      (let [h (head)]
        (r/insert-many! s job [{:id "b" :peak 1} {:id "c" :peak 7}] {:upsert? true})
        (is (= h (head)))
        (is (= t0 (:at (r/get s job {:id "b"}))))))
    (testing "more rows than one commit holds"
      (r/insert-many! s job (for [i (range 1203)] {:id (str "n" i) :n i}) {:upsert? true})
      (is (= 1202 (:n (r/get s job {:id "n1202"}))))
      (is (= 1207 (r/count s job {}))))))

(deftest swap-update-delete
  (let [s (store)]
    (r/insert-many! s job
                    [{:id "a" :status "new" :n 0} {:id "b" :status "new" :n 0}
                     {:id "c" :status "done" :n 0}])
    (is (= 1 (:n (r/swap! s job {:id "a"} #(update % :n inc)))))
    (is (nil? (r/swap! s job {:id "a"} (constantly nil))) "f returning nil writes nothing")
    (is (= {:id "z" :branch "main" :n 1 :at t0}
           (r/swap! s job {:id "z"} (fn [old] (assert (nil? old)) {:n 1}))))
    (is (= 2 (r/update! s job {:status "new"} {:status "running"})))
    (is (= 2 (r/count s job {:status "running"})))
    (is (= 1 (r/update! s job {:id "c"} (fn [o] {:note (str "was " (:status o))}))))
    (is (= "was done" (:note (r/get s job {:id "c"}))))
    (is (= 2 (r/delete! s job {:status "running"})))
    (is (= 0 (r/delete! s job {:status "running"})) "nothing to delete commits nothing")
    (is (= #{"c" "z"} (set (map :id (r/list s job {})))))))

(deftest coalesce-and-now
  (let [t1 #inst "2026-02-02T00:00:00Z"
        s  (store t1)]
    (r/insert! s job {:id "a" :note "kept" :done-at nil})
    (r/update! s job {:id "a"} {:note (r/coalesce nil) :done-at r/now})
    (is (= {:note "kept" :done-at t1 :at t1}
           (select-keys (r/get s job {:id "a"}) [:note :done-at :at])))))

(deftest concurrent-swaps-each-land-once
  (let [s  (store)
        fs (doall (repeatedly 8
                              #(future (r/swap! s job {:id "ctr"}
                                                (fn [o] {:n (inc (or (:n o) 0))})))))]
    (run! deref fs)
    (is (= 8 (:n (r/get s job {:id "ctr"}))))))
