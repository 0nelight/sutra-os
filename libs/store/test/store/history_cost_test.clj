(ns ^:unit store.history-cost-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.store.api :as api]
            [sutra.store.conformance :as store-conf]
            [sutra.store.core :as core]
            [sutra.store.core.kv]
            [sutra.store.mem :as mem]))

(def ^:private actor {:principal "t"})

(defn- head [db b] (with-open [s (api/snapshot db {:branch b})] (api/basis s)))

(defn- tx! [db b tx] (api/transact! db (merge {:basis (head db b) :actor actor} tx)))

(defn- ask [db b q] (with-open [s (api/snapshot db {:branch b})] (api/read s nil q)))

(defn- described [n]
  (assoc-in store-conf/world [:object-types store-conf/other :description] (str "d" n)))

(defn- store
  []
  (let [db (doto (mem/store mem/empty-seed {:collections store-conf/collections})
             (tx! "main"
                  {:schema {:authored store-conf/world :migration []}
                   :ops store-conf/seed :origin :index}))]
    (api/branch! db {:op :fork :name "f" :from (head db "main")})
    (dotimes [i 200]
      (if (zero? (mod i 10))
        (tx! db "main" {:schema {:authored (described i) :migration []}})
        (tx! db "main" {:ops [(store-conf/create store-conf/widget (+ 1000 i) {:name (str "w" i)})]}))
      (when (= i 100)
        (tx! db "f" {:ops [(store-conf/create store-conf/widget 5000 {:name "on-f"})]})
        (tx! db "main" {:parents [(head db "f")] :message "merge f"})))
    (api/branch! db {:op :fork :name "g" :from (head db "main")})
    (tx! db "g" {:schema {:authored (described "g") :migration []}})
    (tx! db "g" {:ops [(store-conf/create store-conf/widget 6000 {:name "on-g"})]})
    (api/branch! db {:op :fork :name "h" :from (head db "main")})
    db))

(defn- counted
  [f]
  (let [n (atom 0) read @#'sutra.store.core.kv/kv-read scan @#'sutra.store.core.kv/kv-scan]
    (with-redefs [sutra.store.core.kv/kv-read (fn [db h ks] (swap! n inc) (read db h ks))
                  sutra.store.core.kv/kv-scan (fn [db h p o] (swap! n inc) (scan db h p o))]
      [(f) {:calls @n}])))

(deftest the-newest-commits-are-the-newest-of-the-whole-history
  (let [db  (store)
        all (ask db "main" [:schema/history {}])]
    (doseq [opts [{:limit 1} {:limit 7} {:limit 50} {:limit 500}
                  {:limit 5 :schema? true} {:limit 40 :schema? true}
                  {:limit 3 :before (:rev (nth all 30))}
                  {:limit 4 :schema? true :before (:rev (nth all 60))}]]
      (testing (pr-str opts)
        (is (= (->> all
                    (filter #(or (not (:schema? opts)) (:schema? %)))
                    (filter #(or (nil? (:before opts)) (< (:rev %) (:before opts))))
                    (take (:limit opts)))
               (ask db "main" [:schema/history opts])))))))

(deftest a-limited-history-reads-what-it-returns-not-the-history
  (let [db (store)
        n  (count (ask db "main" [:schema/history {}]))]
    (testing "the last five commits of a 200-commit history"
      (let [[h {:keys [calls]}] (counted #(ask db "main" [:schema/history {:limit 5}]))]
        (is (= 5 (count h)))
        (is (< n 300))
        (is (<= calls 3) (str calls " reads (a snapshot's own read, then the scan)"))))
    (testing "the last five schema commits scan further, in doubling pages"
      (let [[h
             {:keys [calls]}] (counted #(ask db "main" [:schema/history {:limit 5 :schema? true}]))]
        (is (= 5 (count h)))
        (is (<= calls 4) (str calls " reads"))))))

(deftest branch-tips-are-two-reads-for-any-number-of-branches
  (let [db (store)
        [tips
         {:keys [calls]}] (counted #(ask db "main" [:branch/tips ["main" "f" "g" "h" "nope"]]))]
    (testing "a branch's own last schema commit, and none for one that made none"
      (is (= (:rev (first (ask db "g" [:schema/history {:limit 1 :schema? true}])))
             (get-in tips ["g" :last :rev])))
      (is (= "t" (get-in tips ["g" :last :actor :principal])))
      (is (nil? (get-in tips ["h" :last])))
      (is (= (:rev (first (ask db "main" [:schema/history {:limit 1 :schema? true}])))
             (get-in tips ["main" :last :rev]))))
    (testing "the commit a branch was forked at"
      (is (= (get-in (ask db "main" [:branch/get "h"]) [:base :rev])
             (get-in tips ["h" :fork :rev])))
      (is (some? (get-in tips ["h" :fork :at]))))
    (testing "an unknown branch has no tips"
      (is (= {:last nil :fork nil} (get tips "nope"))))
    (is (= 3 calls) "a snapshot's own read, then two")))

(deftest an-interrupted-thread-stops-at-its-next-round-trip
  (let [db (store)]
    (try
      (.interrupt (Thread/currentThread))
      (is (thrown? InterruptedException (ask db "main" [:schema/history {:limit 5}])))
      (finally (Thread/interrupted)))
    (is (= 5 (count (ask db "main" [:schema/history {:limit 5}])))
        "and reads again once it is cleared")))

(deftest a-world-at-a-fork-base-costs-the-fork-chain-not-the-distance
  (let [db   (store)
        base (get-in (ask db "main" [:branch/get "f"]) [:base :rev])
        n    (count (ask db "main" [:schema/history {}]))]
    (is (< 150 n) "main's history is long enough that walking it would show")
    (is (< base (:rev (head db "main"))) "and f was forked far behind the head")
    (let [[w
           {:keys [calls]}]
            (counted #(api/read-at db {:branch "main" :rev base} nil [:schema/world]))]
      (is (map? w) "the world as of the fork base reads")
      (is (<= calls 12)
          (str calls " round trips for a basis " (- (:rev (head db "main")) base)
               " revisions behind the head — the question is answered from the revision's own"
               " branch and the chain of fork bases, never one read per revision between")))))

(deftest a-revision-reached-only-through-a-merge-parent-is-still-an-ancestor
  (let [db    (store)
        on-f  (->> (ask db "f" [:schema/history {}]) (map :rev) (apply min))]
    (testing "f was merged into main, so its own revisions are on main's history"
      (is (map? (api/read-at db {:branch "main" :rev on-f} nil [:schema/world]))
          "bookkeeping cannot show this one — the walk still answers it"))))

(deftest a-revision-of-another-branch-is-still-refused
  (let [db   (store)
        on-g (:rev (first (ask db "g" [:schema/history {:limit 1 :schema? true}])))]
    (is (thrown? clojure.lang.ExceptionInfo
                 (api/read-at db {:branch "main" :rev on-g} nil [:schema/world]))
        "g forked off main and committed there; that revision is not on main")))

(deftest the-cost-law-is-the-slope-not-the-figure
  (let [m (fn [small big] {:what "q" :sizes [150 450] :small small :big big})]
    (testing "a cost that stays flat keeps the law, however large it is"
      (is (empty? (store-conf/cost-verdicts (m {:calls 900 :rows 900} {:calls 900 :rows 900}) 12)))
      (is (empty? (store-conf/cost-verdicts (m {:calls 3 :rows 132} {:calls 3 :rows 132}) 12))))
    (testing "a constant more is allowed — a paged reader may take one page more"
      (is (empty? (store-conf/cost-verdicts (m {:calls 3 :rows 60} {:calls 4 :rows 64}) 12))))
    (testing "proportional growth breaks it, and the verdict names the currency and the slope"
      (let [[v & more] (store-conf/cost-verdicts (m {:calls 2 :rows 153} {:calls 2 :rows 453}) 12)]
        (is (nil? more) "calls stayed flat, so only rows broke")
        (is (= :rows (:currency v)))
        (is (= [153 453] [(:small v) (:big v)]))
        (is (re-find #"153 -> 453" (:error v)))
        (is (re-find #"followed the data" (:error v)))))
    (testing "both currencies can break, and both are reported"
      (is (= [:calls :rows]
             (mapv :currency
                   (store-conf/cost-verdicts
                    (m {:calls 153 :rows 155} {:calls 453 :rows 455}) 12)))))
    (testing "a currency the counter never reported is zero, not a free pass"
      (is (= [:rows]
             (mapv :currency (store-conf/cost-verdicts (m {:calls 3} {:calls 3 :rows 400}) 12)))))))

(deftest the-other-direction-allows-rows-and-forbids-calls
  (let [m (fn [small big] {:what "q" :sizes [1 50] :small small :big big})]
    (testing "fifty items may cost fifty rows — that is the answer growing, and its size
              does appear in it"
      (is (empty? (store-conf/proportional-verdicts
                   (m {:calls 4 :rows 6} {:calls 4 :rows 50}) 12))))
    (testing "but not fifty CALLS: one op per item is the N+1 that reads as correct and
              scales like a loop"
      (let [[v & more] (store-conf/proportional-verdicts
                        (m {:calls 4 :rows 6} {:calls 54 :rows 50}) 12)]
        (is (nil? more) "rows stayed within the request, so only calls broke")
        (is (= :calls (:currency v)))
        (is (re-find #"4 -> 54" (:error v)))
        (is (re-find #"A batch is one op" (:error v)))))
    (testing "and rows beyond the request break it too — fifty items, five thousand rows"
      (is (= [:rows]
             (mapv :currency
                   (store-conf/proportional-verdicts (m {:calls 4 :rows 6} {:calls 4 :rows 5000})
                                               12)))))))

(deftest a-measurement-that-could-not-be-read-is-not-a-pass
  (testing "an answer whose size needs forcing is refused as unjudged, not counted as zero"
    (let [[v & more] (store-conf/cost-verdicts {:what "q" :sizes [150 450]
                                          :small {:calls 2 :rows 3 :uncounted 1}
                                          :big   {:calls 2 :rows 3}} 12)]
      (is (nil? more))
      (is (= :uncounted (:currency v)))
      (is (re-find #"not a free pass" (:error v))))))

(deftest a-source-emitted-again-unchanged-changes-nothing
  (let [db    (doto (mem/store mem/empty-seed {:collections store-conf/collections})
                (tx! "main" {:schema {:authored store-conf/world :migration []}
                             :ops store-conf/seed :origin :index}))
        emit! #(tx! db "main" {:ops [(store-conf/create store-conf/widget 7000 {:name %})]
                               :origin :index})
        read  #(with-open [s (api/snapshot db {:branch "main"})] (api/read s api/unrestricted %))
        clock #(read [:objects/clock store-conf/widget])
        feed  #(read [:objects/changes store-conf/widget {:ids [(store-conf/id 7000)]}])]
    (emit! "a")
    (let [c0 (clock) n0 (count (feed))]
      (emit! "a")
      (testing "the same row again: no change entry, the type's clock stands"
        (is (= c0 (clock)))
        (is (= n0 (count (feed)))))
      (emit! "b")
      (testing "a changed value: one entry naming only it, the clock moves"
        (is (< c0 (clock)))
        (is (= [:name] (:attrs (last (feed)))))))))

(deftest compacting-the-index-feed-keeps-edits-and-clocks
  (let [db    (doto (mem/store mem/empty-seed {:collections store-conf/collections})
                (tx! "main" {:schema {:authored store-conf/world :migration []}
                             :ops store-conf/seed :origin :index}))
        id    (store-conf/id 7000)
        emit! #(tx! db "main" {:ops [(store-conf/create store-conf/widget 7000 {:name %})]
                               :origin :index})
        read  #(with-open [s (api/snapshot db {:branch "main"})] (api/read s api/unrestricted %))
        feed  #(read [:objects/changes store-conf/widget {:ids [id]}])]
    (emit! "a")
    (tx! db "main" {:ops [{:op :update-object :type store-conf/widget
                           :payload {:id id :values {:name "edited"}}}]
                    :origin :edit})
    (emit! "b")
    (let [clock (read [:objects/clock store-conf/widget])
          edits (read [:objects/changes store-conf/widget {:ids [id] :origin :edit}])
          r     (core/compact-index-feed! db (:rev (head db "main")))]
      (is (pos? (:commits r)))
      (testing "the edit stays whole, every index entry is gone, the clock stands"
        (is (= edits (feed)))
        (is (= [:name] (:attrs (first edits))))
        (is (= clock (read [:objects/clock store-conf/widget]))))
      (testing "once is enough"
        (is (zero? (:commits (core/compact-index-feed! db (:rev (head db "main"))))))))))
