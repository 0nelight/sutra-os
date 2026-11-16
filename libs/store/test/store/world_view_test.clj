(ns ^:unit store.world-view-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.store.api :as api]
            [sutra.store.mem :as mem]
            [sutra.store.world :as world]))

(def ^:private w0
  {:object-types {"ri.t" {:object-type-name :t
                          :attribute-list [{:attribute-name :a :attribute-type :string}]}}})

(defn- store [db] (world/store db "main" {}))

(defn- described [w d] (assoc-in w [:object-types "ri.t" :description] d))

(deftest a-view-reads-one-world-per-branch-and-its-own-writes
  (let [db   (mem/store)
        _    (world/install! (store db) w0)
        mine (world/viewing (store db))
        seen (world/authored mine)]
    (testing "another scope's commit is not seen: the view is at its first basis"
      (world/install! (store db) (described w0 "theirs"))
      (is (= seen (world/authored mine)))
      (is (= "theirs" (get-in (world/authored (store db)) [:object-types "ri.t" :description]))
          "without a view a read is at the head"))
    (testing "a world decided on what it saw is refused once another scope's world landed"
      (is (= :conflict (:store/refusal (try (world/install! mine (described w0 "stale")) nil
                                            (catch clojure.lang.ExceptionInfo e (ex-data e)))))))
    (testing "its own commit moves the view to what it left"
      (reset! (:view mine) {})
      (world/authored mine)
      (world/install! mine (described w0 "mine"))
      (is (= "mine" (get-in (world/authored mine) [:object-types "ri.t" :description]))))
    (testing "each branch has its own basis, a fork starts the view at the fork"
      (world/fork! mine "main" "f")
      (is (= (world/authored mine) (world/authored (world/at mine "f"))))
      (world/install! (world/at mine "f") (described w0 "on f"))
      (is (= "on f"
             (get-in (world/authored (world/at mine "f")) [:object-types "ri.t" :description])))
      (is (= "mine" (get-in (world/authored mine) [:object-types "ri.t" :description]))))
    (testing "a retired branch leaves the view"
      (world/retire! mine "f")
      (is (not (contains? @(:view mine) "f"))))
    (testing "an explicit revision is read as of it, view or not"
      (let [r (:rev (world/head (store db)))]
        (is (= (world/authored (store db) r) (world/authored mine r)))))
    (testing "a world decided on a basis another scope's world commit moved is refused, not lost"
      (let [late (world/viewing (store db))]
        (world/authored late)
        (world/install! (store db) (described w0 "first"))
        (is (= :conflict (:store/refusal (try (world/install! late (described w0 "late")) nil
                                              (catch clojure.lang.ExceptionInfo e (ex-data e))))))
        (is (= "first" (get-in (world/authored (store db)) [:object-types "ri.t" :description])))))
    (testing "a commit that changes no world moves no fence: records and the log go past the view"
      (let [v (world/viewing (store db))]
        (world/authored v)
        (world/index! (store db) [])
        (world/install! v (described w0 "after-index"))
        (is (= "after-index"
               (get-in (world/authored (store db)) [:object-types "ri.t" :description])))))
    (testing "the view holds bases, which are values, and nothing else"
      (is (every? #(= #{:branch :rev} (set (keys %))) (vals @(:view mine))))
      (is (satisfies? api/Db db)))))
