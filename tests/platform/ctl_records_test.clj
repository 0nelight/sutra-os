(ns ^:integration platform.ctl-records-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.store.control.cadence :as cadence]
            [sutra.store.control :as ctl]
            [sutra.projects.entity-resolution :as entity-rows]
            [sutra.canvas.layouts]
            [sutra.catalog.grants :as grants]
            [sutra.foundation.ops :as op]
            [sutra.store.control.import-keys :as import-keys]
            [sutra.builds.run.rows :as run-rows]
            [sutra.actions.staged-rows :as staged]
            [sutra.store.access.run-log :as run-log]
            [sutra.store.access.scope :as ctx]
            [sutra.automation.runs :as autos]
            [sutra.store.records :as r]
            [sutra.store.world :as world]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-stores-only)

(defn- store [] (ctx/store "main"))

(def ^:private colls
  [:staged-actions :runs :import-keys :index-times :build-schedules :automations :graph-layouts
   :entity-crosswalk :entity-provenance])

(defn- clear!
  []
  (doseq [t colls
          b (distinct (map :branch (r/list (store) (ctl/decl t) {} {:all-branches? true})))]
    (ctl/delete! (store) t {:branch b})))

(defn- fresh-branch!
  [prefix]
  (let [b (str prefix "-" (subs (str (random-uuid)) 0 8))]
    (world/fork! (store) "main" b)
    b))

(defn- log! [branch action-name & [objects]]
  (run-log/append! (ctx/store branch) [(run-log/action-entry action-name "{}" objects nil)]))

(defn- log-tail [branch] (run-log/action-log-tail (ctx/store branch) 10))

(deftest a-staged-queue-is-not-inherited
  (clear!)
  (staged/stage! (store) "main" :create-object-type {:name :widget})
  (is (= 1 (count (staged/list-staged (store) "main"))))
  (is (empty? (staged/list-staged (store) "feature-x"))
      "a fresh branch has NO pending work — inheriting it would re-apply the trunk's"))

(deftest staged-actions-keep-arrival-order-per-branch
  (clear!)
  (staged/stage! (store) "main" :first  "1")
  (staged/stage! (store) "b"    :other  "x")
  (staged/stage! (store) "main" :second "2")
  (is (= [:first :second] (mapv :action (staged/list-staged (store) "main")))
      "interleaved writes from another branch do not disturb this one's order")
  (is (= [:other] (mapv :action (staged/list-staged (store) "b")))))

(deftest clearing-one-branch-leaves-the-others-alone
  (clear!)
  (staged/stage! (store) "main" :a "1")
  (staged/stage! (store) "keep" :b "2")
  (staged/stage! (store) "keep" :c "3")
  (is (= 1 (staged/clear-staged! (store) "main")))
  (is (empty? (staged/list-staged (store) "main")))
  (is (= 2 (count (staged/list-staged (store) "keep"))) "the other branch is untouched"))

(deftest deleting-a-staged-row-refuses-another-branchs
  (clear!)
  (let [s (staged/stage! (store) "mine" :a "1")]
    (is (= 0 (ctl/delete! (store) :staged-actions {:branch "theirs" :seq s}))
        "not yours, not deleted")
    (is (= 1 (count (staged/list-staged (store) "mine"))))
    (is (= 1 (ctl/delete! (store) :staged-actions {:branch "mine" :seq s})))
    (is (empty? (staged/list-staged (store) "mine")))))

(deftest a-staged-row-remembers-who-staged-it-and-through-which-door
  (clear!)
  (staged/stage! (store) "main" :create-object-type {:name :widget} nil
                 {:author "gwen" :origin :git-push :origin-ref "beschaffung main 4c42aeb9"})
  (let [[r] (staged/list-staged (store) "main")]
    (is (= "gwen" (:author r)))
    (is (= "git-push" (:origin r)))
    (is (= "beschaffung main 4c42aeb9" (:origin-ref r)))))

(deftest provenance-is-optional-and-its-absence-is-not-an-error
  (clear!)
  (staged/stage! (store) "main" :create-object-type {:name :widget})
  (let [[r] (staged/list-staged (store) "main")]
    (is (nil? (:author r)))
    (is (nil? (:origin r)))
    (is (nil? (:origin-ref r)))))

(deftest open-batches-names-every-branch-that-has-one
  (clear!)
  (staged/stage! (store) "staging/gwen/main"  :a "1" nil {:author "gwen" :origin :action})
  (staged/stage! (store) "staging/gwen/main"  :b "2" nil {:author "gwen" :origin :action})
  (staged/stage! (store) "staging/alwin/main" :c "3" nil {:author "alwin" :origin :git-push})
  (let [by (into {} (map (juxt :branch identity)) (staged/open-batches (store)))]
    (is (= 2 (count by)))
    (is (= 2 (:n (get by "staging/gwen/main"))))
    (is (= "gwen" (:author (get by "staging/gwen/main"))))
    (is (= "alwin" (:author (get by "staging/alwin/main"))))
    (is (= "git-push" (:origin (get by "staging/alwin/main"))))
    (is (every? some? (map :opened-at (vals by)))
        "the batch's age is its first row's created-at")))

(deftest open-batches-reports-the-row-that-OPENED-the-batch
  (clear!)
  (staged/stage! (store) "staging/gwen/main" :a "1" nil {:author "gwen"  :origin :action})
  (staged/stage! (store) "staging/gwen/main" :b "2" nil {:author "root" :origin :install})
  (let [[b] (staged/open-batches (store))]
    (is (= "gwen" (:author b)))
    (is (= "action" (:origin b)))
    (is (= 2 (:n b)))))

(deftest an-action-log-is-per-branch-and-newest-first
  (let [a (fresh-branch! "log-a") other (fresh-branch! "log-o") silent (fresh-branch! "log-s")]
    (log! a "create-widget")
    (log! a "delete-widget")
    (log! other "unrelated")
    (is (= ["delete-widget" "create-widget"] (mapv :action-name (log-tail a))))
    (is (= ["unrelated"] (mapv :action-name (log-tail other))))
    (testing "and a branch that logged nothing sees nothing"
      (is (empty? (log-tail silent))))))

(deftest a-renamed-node-keeps-its-saved-position
  (clear!)
  (ctl/upsert! (store) :graph-layouts {:branch "main" :graph-id "g1" :node-kind :transform
                                       :node-key "alt" :x 10 :y 20})
  (ctl/upsert! (store) :graph-layouts {:branch "main" :graph-id "g2" :node-kind :transform
                                       :node-key "alt" :x 1 :y 2})
  (ctl/upsert! (store) :graph-layouts {:branch "main" :graph-id "g2" :node-kind :transform
                                       :node-key "neu" :x 5 :y 5})
  (is (= 1 (op/run! {:store (store)} :canvas/move-layout-node
                     {:branch "main" :node-kind :transform :from "alt" :to "neu"})))
  (let [at (fn [g k] (some-> (ctl/get (store) :graph-layouts {:branch "main" :graph-id g
                                                              :node-kind :transform :node-key k})
                             ((juxt :x :y)) (->> (mapv long))))]
    (is (= [10 20] (at "g1" "neu")) "the position moves to the new name")
    (is (nil? (at "g1" "alt")))
    (is (= [5 5] (at "g2" "neu")) "a graph that already placed the new name keeps its own")))

(deftest a-dropped-graph-takes-its-side-data-along
  (clear!)
  (doseq [g ["g1" "g2"]]
    (ctl/upsert! (store) :graph-layouts {:branch "main" :graph-id g :node-kind :transform
                                         :node-key "t" :x 1 :y 1}))
  (is (= 1 (op/run! {:store (store) :auth grants/system-authority}
                    :canvas/sweep-dropped-graphs
                    {:branch "main" :before {"g1" {} "g2" {}} :after {"g2" {}}})))
  (let [graphs (set (map :graph-id (ctl/list (store) :graph-layouts {:branch "main"})))]
    (is (= #{"g2"} graphs) "only the dropped graph's layouts go"))
  (testing "a decision that writes nothing commits nothing"
    (is (= 0 (op/run! {:store (store) :auth grants/system-authority}
                      :canvas/sweep-dropped-graphs
                      {:branch "main" :before {"g2" {}} :after {"g2" {}}})))))

(deftest an-action-log-entry-names-the-objects-it-edited
  (let [from (fresh-branch! "log-from") fork (fresh-branch! "log-fork")]
    (log! from "freigeben" [{:type "auftrag" :id "a1"}])
    (is (= [{:type "auftrag" :id "a1"}] (:objects (first (log-tail from)))))
    (testing "a branch fork carries the named objects with the log"
      (run-log/copy-action-log! (store) from fork)
      (is (= [{:type "auftrag" :id "a1"}] (:objects (first (log-tail fork))))))))

(deftest dropping-a-branch-takes-its-queue-and-log-with-it
  (clear!)
  (let [doomed (fresh-branch! "doomed")]
    (staged/stage! (store) doomed :pending "1")
    (log! doomed "did-something")
    (world/retire! (store) doomed)
    (is (empty? (staged/list-staged (store) doomed)))
    (testing "and a branch reborn under the same name inherits nothing of it"
      (world/fork! (store) "main" doomed)
      (is (empty? (staged/list-staged (store) doomed)))
      (is (empty? (log-tail doomed))))))

(deftest a-run-goes-through-its-lifecycle
  (clear!)
  (run-rows/start-run! (store) "main" {:id "r1" :kind "import" :actor "alice"
                                       :inputs-hash "h1" :inputs {}
                                       :parent-run-id nil :working-branch "run/main/abc"})
  (let [r (run-rows/fetch-run (store) "main" "r1")]
    (is (= "running" (:status r)))
    (is (= "alice" (:actor r)))
    (is (nil? (:finished-at r))))
  (run-rows/finish-run! (store) "main" "r1" {:objects-count 7 :links-count 3
                                             :summary {:ok true} :commit-hash "42"})
  (let [r (run-rows/fetch-run (store) "main" "r1")]
    (is (= "succeeded" (:status r)))
    (is (= 7 (:objects-count r)))
    (is (some? (:finished-at r)))
    (is (= "42" (:snapshot-id r)))))

(deftest a-failed-run-records-why
  (clear!)
  (run-rows/start-run! (store) "main" {:id "r2" :kind "import" :inputs-hash "" :inputs nil})
  (run-rows/fail-run! (store) "main" "r2" {:error-message "boom" :working-branch "run/main/x"})
  (let [r (run-rows/fetch-run (store) "main" "r2")]
    (is (= "failed" (:status r)))
    (is (= "boom" (:error-message r)))
    (is (= "run/main/x" (:working-branch r))))
  (testing "and a nil working-branch does not erase the recorded one"
    (run-rows/fail-run! (store) "main" "r2" {:error-message "again" :working-branch nil})
    (is (= "run/main/x" (:working-branch (run-rows/fetch-run (store) "main" "r2"))))))

(deftest runs-are-per-branch
  (clear!)
  (run-rows/start-run! (store) "main" {:id "m1" :kind "import" :inputs-hash "" :inputs nil})
  (run-rows/start-run! (store) "feat" {:id "f1" :kind "import" :inputs-hash "" :inputs nil})
  (is (= ["m1"] (mapv :id (run-rows/recent-runs (store) "main" 10 nil))))
  (is (= ["f1"] (mapv :id (run-rows/recent-runs (store) "feat" 10 nil))))
  (testing "and a run id from another branch is not fetchable"
    (is (nil? (run-rows/fetch-run (store) "main" "f1")))))

(deftest a-run-list-leaves-the-inputs-to-the-single-run
  (clear!)
  (run-rows/start-run! (store) "main" {:id "big" :kind "import" :inputs-hash "h"
                                       :inputs {:t (vec (range 100))}})
  (run-rows/finish-run! (store) "main" "big" {:objects-count 1 :summary {:ok true}})
  (let [[r] (run-rows/recent-runs (store) "main" 10 nil 3600)]
    (is (= "big" (:id r)))
    (is (not (contains? r :inputs)) "a list never reads a run's inputs")
    (is (= {:ok true} (:summary r)) "what a list shows stays")
    (is (= ["main" "import" "succeeded"] [(:branch r) (name (:kind r)) (:status r)])))
  (is (= {:t (vec (range 100))} (:inputs (run-rows/fetch-run (store) "main" "big")))
      "the single run still carries them"))

(deftest recent-runs-filters-by-kind-and-orders-newest-first
  (clear!)
  (doseq [[id kind] [["a" "import"] ["b" "publish"] ["c" "import"]]]
    (run-rows/start-run! (store) "main" {:id id :kind kind :inputs-hash "" :inputs nil}))
  (is (= #{"a" "c"} (set (mapv :id (run-rows/recent-runs (store) "main" 10 "import")))))
  (is (= 3 (count (run-rows/recent-runs (store) "main" 10 nil))))
  (is (= 1 (count (run-rows/recent-runs (store) "main" 1 nil))) "the limit is honoured")
  (testing "and by a time WINDOW — the question a build timeline asks, which a
            row limit cannot express"
    (is (= 3 (count (run-rows/recent-runs (store) "main" 10 nil 3600)))
        "all three started inside the last hour")
    (is (empty? (run-rows/recent-runs (store) "main" 10 nil -1))
        "a window that ends before now selects nothing")
    (is (= #{"a" "c"} (set (mapv :id (run-rows/recent-runs (store) "main" 10 "import" 3600))))
        "kind and window compose")))

(deftest the-parent-pointer-follows-the-latest-SUCCEEDED-run-on-this-branch
  (clear!)
  (is (nil? (run-rows/latest-succeeded-run-id (store) "main")))
  (run-rows/start-run! (store) "main" {:id "s1" :kind "import" :inputs-hash "" :inputs nil})
  (is (nil? (run-rows/latest-succeeded-run-id (store) "main")) "a RUNNING run is not a parent")
  (run-rows/finish-run! (store) "main" "s1" {:objects-count 0 :links-count 0})
  (is (= "s1" (run-rows/latest-succeeded-run-id (store) "main")))
  (run-rows/start-run! (store) "other" {:id "o1" :kind "import" :inputs-hash "" :inputs nil})
  (run-rows/finish-run! (store) "other" "o1" {:objects-count 0 :links-count 0})
  (is (= "s1" (run-rows/latest-succeeded-run-id (store) "main"))
      "another branch's success does not become this branch's parent"))

(deftest dropping-a-branch-takes-its-runs
  (clear!)
  (let [doomed (fresh-branch! "doomed")]
    (run-rows/start-run! (store) doomed {:id "d1" :kind "import" :inputs-hash "" :inputs nil})
    (world/retire! (store) doomed)
    (is (empty? (run-rows/recent-runs (store) doomed 10 nil)))))

(deftest import-keys-are-counted-per-object-type
  (clear!)
  (import-keys/write-import-keys! (store) "main" [["widget" "\"a\"" "u1"]
                                                  ["widget" "\"b\"" "u2"]
                                                  ["gadget" "\"c\"" "u3"]])
  (let [by-type (into {} (map (juxt :object-type identity))
                      (import-keys/import-key-counts (store) "main"))]
    (is (= 2 (get-in by-type ["widget" :n])))
    (is (= 1 (get-in by-type ["gadget" :n])))))

(deftest import-keys-written-again-unchanged-are-left-as-stored
  (clear!)
  (let [k   {:branch "main" :object-type "widget" :pk-value "\"a\""}
        _   (import-keys/write-import-keys! (store) "main" [["widget" "\"a\"" "u1"]])
        at0 (:imported-at (ctl/get (store) :import-keys k))]
    (Thread/sleep 5)
    (import-keys/write-import-keys! (store) "main" [["widget" "\"a\"" "u1"]])
    (is (= at0 (:imported-at (ctl/get (store) :import-keys k))))))

(deftest an-indexed-type-answers-the-time-of-its-last-index
  (clear!)
  (import-keys/write-index-times! (store) "main" ["widget" "gadget"] "run-1")
  (let [t1 (get (import-keys/index-times (store) "main") "widget")]
    (is (inst? t1))
    (Thread/sleep 5)
    (import-keys/write-index-times! (store) "main" ["widget"] "run-2")
    (let [times (import-keys/index-times (store) "main")]
      (is (pos? (compare (get times "widget") t1)) "a later index moves the type's time")
      (is (= t1 (get times "gadget")) "a type not indexed keeps its time")
      (is (= "run-2"
             (:run-id (ctl/get (store) :index-times {:branch "main" :object-type "widget"})))))
    (is (empty? (import-keys/index-times (store) "feat")) "a branch has its own times")))

(deftest re-importing-a-key-updates-in-place
  (clear!)
  (import-keys/write-import-keys! (store) "main" [["widget" "\"a\"" "u1"]])
  (import-keys/write-import-keys! (store) "main" [["widget" "\"a\"" "u1-again"]])
  (is (= 1 (:n (first (import-keys/import-key-counts (store) "main"))))
      "the same source key is one row, not two")
  (is (= "u1-again"
         (:uuid (ctl/get (store) :import-keys
                         {:branch "main" :object-type "widget" :pk-value "\"a\""})))))

(deftest import-keys-are-not-inherited-from-the-trunk
  (clear!)
  (import-keys/write-import-keys! (store) "main" [["widget" "\"a\"" "u1"]])
  (is (empty? (import-keys/import-key-counts (store) "fresh"))))

(deftest a-branch-importing-the-same-key-does-not-touch-the-trunks-row
  (clear!)
  (import-keys/write-import-keys! (store) "main" [["widget" "\"a\"" "main-uuid"]])
  (import-keys/write-import-keys! (store) "feat" [["widget" "\"a\"" "feat-uuid"]])
  (is (= 1 (:n (first (import-keys/import-key-counts (store) "main")))))
  (is (= 1 (:n (first (import-keys/import-key-counts (store) "feat")))))
  (is (= #{"main-uuid" "feat-uuid"}
         (into #{} (map :uuid)
               (r/list (store) (ctl/decl :import-keys) {} {:all-branches? true})))
      "two rows for one source key — one per branch"))

(deftest a-write-larger-than-one-batch-lands-whole
  (clear!)
  (import-keys/write-import-keys!
   (store) "main" (for [i (range 1201)] ["widget" (pr-str i) (str "u" i)]))
  (is (= 1201 (:n (first (import-keys/import-key-counts (store) "main"))))))

(deftest dropping-a-branch-takes-its-import-keys
  (clear!)
  (let [doomed (fresh-branch! "doomed")]
    (import-keys/write-import-keys! (store) doomed [["widget" "\"a\"" "u1"]])
    (world/retire! (store) doomed)
    (is (empty? (import-keys/import-key-counts (store) doomed)))))

(def ^:private bs :build-schedules)

(defn- sched! [branch id nm every-ms]
  (ctl/upsert! (store) bs {:branch branch :id id :name nm :target nil :cron nil :every-ms every-ms
                           :trigger nil :scope nil :build-from nil :paused false}))

(deftest a-schedule-round-trips
  (clear!)
  (let [r (ctl/upsert! (store) bs {:branch "main" :id "s1" :name "nightly" :every-ms 60000
                                   :trigger {:kind :parent-updated} :scope :self :paused false})]
    (is (= "nightly" (:name r)))
    (is (= {:kind :parent-updated} (:trigger r)))
    (is (= :self (:scope r)))
    (is (false? (:paused r)))))

(deftest an-upsert-updates-in-place
  (clear!)
  (sched! "main" "s1" "first" 1000)
  (let [r (sched! "main" "s1" "second" 2000)]
    (is (= "second" (:name r)))
    (is (= 2000 (:every-ms r))))
  (is (= 1 (count (ctl/list (store) bs {:branch "main"})))))

(deftest schedules-are-per-branch
  (clear!)
  (sched! "main" "m1" "trunk-sched" 1000)
  (sched! "feat" "f1" "branch-sched" 1000)
  (is (= ["m1"] (mapv :id (ctl/list (store) bs {:branch "main"}))))
  (is (= ["f1"] (mapv :id (ctl/list (store) bs {:branch "feat"}))))
  (is (nil? (ctl/get (store) bs {:branch "main" :id "f1"}))))

(deftest two-branches-hold-their-own-row-for-the-same-id
  (clear!)
  (sched! "main" "dup" "on-main" 1000)
  (sched! "feat" "dup" "on-feat" 1000)
  (is (= "on-main" (:name (ctl/get (store) bs {:branch "main" :id "dup"})))
      "the trunk's schedule is untouched by the branch's")
  (is (= "on-feat" (:name (ctl/get (store) bs {:branch "feat" :id "dup"}))))
  (testing "and each branch lists only its own"
    (is (= 1 (count (ctl/list (store) bs {:branch "main"}))))
    (is (= 1 (count (ctl/list (store) bs {:branch "feat"}))))))

(deftest list-due-honours-paused-and-the-interval
  (clear!)
  (sched! "main" "due"    "due-now"  1)
  (sched! "main" "notdue" "far-off"  86400000)
  (ctl/upsert! (store) bs {:branch "main" :id "off" :name "paused" :every-ms 1 :paused true})
  (Thread/sleep 50)
  (let [due (set (mapv :id (cadence/cadence-list-due (store) "main" bs)))]
    (is (contains? due "due")          "an elapsed interval is due")
    (is (not (contains? due "notdue")) "a distant interval is not")
    (is (not (contains? due "off"))    "a paused schedule never fires")))

(deftest a-null-or-nonpositive-interval-never-fires
  (clear!)
  (ctl/upsert! (store) bs {:branch "main" :id "nil" :name "no-cadence" :every-ms nil :paused false})
  (ctl/upsert! (store) bs {:branch "main" :id "zero" :name "zero" :every-ms 0 :paused false})
  (Thread/sleep 20)
  (is (empty? (cadence/cadence-list-due (store) "main" bs))))

(deftest marking-fired-stamps-the-bookkeeping-column
  (clear!)
  (sched! "main" "s1" "nightly" 1)
  (Thread/sleep 20)
  (is (seq (cadence/cadence-list-due (store) "main" bs)))
  (cadence/cadence-mark-fired! (store) "main" bs "s1" :last-run-id "run-42")
  (let [r (ctl/get (store) bs {:branch "main" :id "s1"})]
    (is (= "run-42" (:last-run-id r)))
    (is (some? (:last-fired-at r))))
  (testing "and the interval now measures from the firing, not from creation"
    (ctl/upsert! (store) bs
                 {:branch "main" :id "s1" :name "nightly" :every-ms 86400000 :paused false})
    (is (empty? (cadence/cadence-list-due (store) "main" bs)))))

(deftest deleting-and-dropping-are-branch-scoped
  (clear!)
  (let [gone (fresh-branch! "gone")]
    (sched! "main" "keep" "keep" 1000)
    (sched! gone "g1" "gone" 1000)
    (ctl/delete! (store) bs {:branch "main" :id "g1"})
    (is (= 1 (count (ctl/list (store) bs {:branch gone}))) "another branch's row is not deleted")
    (world/retire! (store) gone)
    (is (empty? (ctl/list (store) bs {:branch gone})))
    (is (= 1 (count (ctl/list (store) bs {:branch "main"}))))))

(def ^:private au :automations)

(deftest automations-ride-the-same-cadence-arm
  (clear!)
  (let [r (ctl/upsert! (store) au {:branch "main" :id "a1" :name "notify" :on-type "widget"
                                   :when-spec "{:gt 5}" :effect-spec "{:submit :alert}"
                                   :every-ms 1 :paused false})]
    (is (= "notify" (:name r)))
    (is (= "widget" (:on-type r))))
  (Thread/sleep 20)
  (is (= ["a1"] (mapv :id (cadence/cadence-list-due (store) "main" au))))
  (testing "mark-fired! stamps this table's OWN bookkeeping column"
    (cadence/cadence-mark-fired! (store) "main" au "a1" :last-matched 7)
    (is (= 7 (:last-matched (ctl/get (store) au {:branch "main" :id "a1"})))))
  (testing "and it is branch-scoped like every other control row"
    (ctl/upsert! (store) au {:branch "feat" :id "a1" :name "other" :on-type "gadget"
                             :effect-spec "{}" :paused false})
    (is (= "notify" (:name (ctl/get (store) au {:branch "main" :id "a1"}))))
    (is (= "other"  (:name (ctl/get (store) au {:branch "feat" :id "a1"}))))))

(deftest a-due-list-spans-the-branches-it-is-asked-for
  (clear!)
  (doseq [b ["main" "feat" "closed"]]
    (ctl/upsert! (store) au {:branch b :id "a1" :name b :on-type "widget"
                             :effect-spec "{}" :every-ms 1 :paused false}))
  (Thread/sleep 20)
  (is (= #{["main" "a1"] ["feat" "a1"]}
         (set (map (juxt :branch :id) (cadence/cadence-list-due (store) #{"main" "feat"} au))))
      "each row carries its branch, and a branch not asked for is not fired"))

(deftest a-branch-fires-only-the-automations-it-owns
  (clear!)
  (let [row     {:id "a1" :name "mahnen" :on-type "rechnung" :when-spec "{:gt 5}"
                 :effect-spec "{:action :mahnen}" :every-ms 1 :paused false}
        due     (fn [] (Thread/sleep 20)
                  (set (map (juxt :branch :id)
                            (autos/own-rows (cadence/cadence-list-due (store) #{"main" "try-x"} au)
                                            (constantly "main")
                                            #(ctl/get (store) au {:branch %1 :id %2})))))]
    (ctl/upsert! (store) au (assoc row :branch "main"))
    (testing "a freshly forked branch has no row of its own — main's fires on main only"
      (is (= #{["main" "a1"]} (due))))
    (testing "a push to the branch restates main's automation unchanged: still inherited"
      (ctl/upsert! (store) au (assoc row :branch "try-x"))
      (is (= #{["main" "a1"]} (due))))
    (testing "changed on the branch, it is the branch's own and fires there"
      (ctl/upsert! (store) au (assoc row :branch "try-x" :when-spec "{:gt 9}"))
      (is (= #{["main" "a1"] ["try-x" "a1"]} (due))))))

(deftest the-two-cadence-tables-do-not-see-each-other
  (clear!)
  (ctl/upsert! (store) bs {:branch "main" :id "x" :name "sched" :every-ms 1000 :paused false})
  (ctl/upsert! (store) au {:branch "main" :id "x" :name "auto" :on-type "widget" :effect-spec "{}"
                           :every-ms 1000 :paused false})
  (is (= "sched" (:name (ctl/get (store) bs {:branch "main" :id "x"}))))
  (is (= "auto"  (:name (ctl/get (store) au {:branch "main" :id "x"})))))

(defn- days-ago [n] (java.util.Date. (- (System/currentTimeMillis) (* (long n) 86400000))))

(defn- seed-run! [id kind age-days summary]
  (ctl/insert! (store) :runs {:id id :branch "main" :kind kind :status "succeeded"
                              :summary summary :inputs "{:some :inputs}"
                              :started-at (days-ago age-days) :finished-at (days-ago age-days)}))

(defn- run-row [id] (run-rows/fetch-run (store) "main" id))

(deftest a-run-keeps-its-provenance-and-loses-only-its-bulk
  (clear!)
  (seed-run! "old" :action 90 "{:result {:world :enormous}}")
  (seed-run! "new" :action 1  "{:result :small}")

  (testing "a dry run reports and writes nothing"
    (let [r (run-rows/prune-run-payloads! (store) {:days 30})]
      (is (false? (:applied? r)))
      (is (= 1 (:runs r)) "only the one past the window")
      (is (pos? (:bytes r)))
      (is (some? (:summary (run-row "old"))))))

  (testing "applied, the payload goes and the ROW stays"
    (run-rows/prune-run-payloads! (store) {:days 30 :apply? true})
    (let [old (run-row "old")]
      (is (nil? (:summary old)))
      (is (nil? (:inputs old)))
      (is (= "succeeded" (:status old)) "a run that lost its text still ran")
      (is (= :action (:kind old))))
    (is (some? (:summary (run-row "new"))) "inside the window, untouched"))

  (testing "and it is idempotent — nothing left to clear"
    (is (= 0 (:runs (run-rows/prune-run-payloads! (store) {:days 30}))))))

(deftest pruning-pages-through-more-runs-than-one-page
  (clear!)
  (doseq [i (range 450)] (seed-run! (str "old-" i) :import (+ 31 (mod i 40)) "{:rows [1 2 3]}"))
  (seed-run! "new" :import 1 "{:rows [4]}")
  (is (= 450 (:runs (run-rows/prune-run-payloads! (store) {:days 30 :apply? true}))))
  (is (every? #(nil? (:summary (run-row (str "old-" %)))) (range 450)))
  (is (some? (:summary (run-row "new"))))
  (is (= 0 (:runs (run-rows/prune-run-payloads! (store) {:days 30})))))

(deftest an-unfinished-run-ages-from-when-it-started
  (clear!)
  (ctl/insert! (store) :runs {:id "stuck" :branch "main" :kind "import" :status "running"
                              :summary "{:big :payload}" :started-at (days-ago 90)})
  (is (= 1 (:runs (run-rows/prune-run-payloads! (store) {:days 30})))))

(defn- cw! [branch object-type id-kind id-value entity-id]
  (ctl/upsert! (store) :entity-crosswalk {:branch branch :object-type object-type :id-kind id-kind
                                          :id-value id-value :entity-id entity-id}))

(defn- pv! [branch entity-id property]
  (ctl/insert-many! (store) :entity-provenance
                    [{:branch branch :entity-id entity-id :property property
                      :value "v" :source "s" :run-id "r"}]))

(defn- er-rows [t branch] (ctl/count (store) t {:branch branch}))

(defn- seed-er! []
  (clear!)
  (cw! "main" "rid-alive" "k" "a1" "e-alive")
  (cw! "main" "rid-dead"  "k" "d1" "e-dead")
  (cw! "main" "rid-gone"  "k" "g1" "e-gone")
  (pv! "main" "e-alive" "p")
  (pv! "main" "e-dead"  "p")
  (pv! "main" "e-gone"  "p"))

(deftest the-sweep-keeps-what-a-resolved-type-still-claims
  (seed-er!)
  (let [r (entity-rows/sweep-entity-resolution! (store) "main" #{"rid-alive"} {:apply? true})]
    (is (= 2 (:crosswalk r)) "rid-dead and rid-gone")
    (is (= 1 (:kept r)))
    (is (= 1 (er-rows :entity-crosswalk "main")))
    (is (= 1 (er-rows :entity-provenance "main"))
        "provenance is judged THROUGH the crosswalk — e-alive is still reachable")))

(deftest an-empty-resolved-set-is-the-ordinary-case-not-a-refusal
  (seed-er!)
  (let [r (entity-rows/sweep-entity-resolution! (store) "main" #{} {:apply? true})]
    (is (= 3 (:crosswalk r)))
    (is (= 3 (:provenance r)))
    (is (zero? (:kept r)))
    (is (zero? (er-rows :entity-crosswalk "main")))
    (is (zero? (er-rows :entity-provenance "main")))))

(deftest a-dry-run-reports-and-writes-nothing
  (seed-er!)
  (let [r (entity-rows/sweep-entity-resolution! (store) "main" #{"rid-alive"})]
    (is (= 2 (:crosswalk r)))
    (is (= 2 (:provenance r)))
    (is (= 3 (er-rows :entity-crosswalk "main")) "still all three")
    (is (= 3 (er-rows :entity-provenance "main")))))

(deftest the-sweep-is-idempotent
  (seed-er!)
  (entity-rows/sweep-entity-resolution! (store) "main" #{"rid-alive"} {:apply? true})
  (let [again (entity-rows/sweep-entity-resolution! (store) "main" #{"rid-alive"} {:apply? true})]
    (is (zero? (:crosswalk again)))
    (is (zero? (:provenance again)))))

(deftest another-branch-is-reported-and-never-swept
  (seed-er!)
  (cw! "feature" "rid-dead" "k" "d1" "e-dead-f")
  (pv! "feature" "e-dead-f" "p")
  (entity-rows/sweep-entity-resolution! (store) "main" #{"rid-alive"} {:apply? true})
  (is (= 1 (er-rows :entity-crosswalk "feature")))
  (is (= 1 (er-rows :entity-provenance "feature")))
  (let [[e] (entity-rows/entity-state-elsewhere (store) "main")]
    (is (= "feature" (:branch e)))
    (is (= 1 (:crosswalk e)))
    (is (= 1 (:provenance e)))))

(deftest the-census-names-every-type-holding-rows
  (seed-er!)
  (let [by-type (into {} (map (juxt :object-type :rows))
                      (entity-rows/entity-state-by-type (store) "main"))]
    (is (= #{"rid-alive" "rid-dead" "rid-gone"} (set (keys by-type))))
    (is (every? #(= 1 %) (vals by-type)))))
