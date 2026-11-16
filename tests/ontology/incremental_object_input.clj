(ns ^:integration ontology.incremental-object-input
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [sutra.store.access.scope :as ctx]
            [sutra.store.access.substrate :as sub]
            [sutra.catalog.transforms :as transforms]
            [sutra.store.world :as world]
            [support.helpers :refer [check section]]
            [support.pod :as pod]
            [support.store :as store]))

(use-fixtures :once pod/with-mem-stores-only)

(defn- uuid [] (str (java.util.UUID/randomUUID)))
(defn- ot [nm attrs] {:object-type-name nm :id nm
                      :attribute-list (mapv (fn [[n t]] {:attribute-name n :attribute-type t})
                                            attrs)})

(deftest which-change-ends-an-incremental-run
  (let [incremental {:incremental {:read-mode :added}}]
    (is (transforms/snapshot-ends-increment? incremental :w))
    (is
     (not (transforms/snapshot-ends-increment? (assoc-in incremental [:incremental :static-inputs] [:w])
                                         :w))
     "a declared snapshot input is a lookup")
    (is (transforms/snapshot-ends-increment? (assoc-in incremental [:incremental :static-inputs]
                                                 [:other]) :w))
    (is (not (transforms/snapshot-ends-increment? {:incremental {:read-mode :current}} :w))
        "a transform that reads the current state is whole anyway")
    (is (not (transforms/snapshot-ends-increment? {} :w)))))

(deftest an-object-type-input-is-a-snapshot
  (let [branch (str "inc-" (subs (uuid) 0 8))
        store     #(ctx/store branch)
        names  #(set (map (comp :name :values)
                          (sub/read-type (ctx/as-system (store)) :widget nil)))
        moved? #(sub/changed-since? (store) :widget %)
        W1 (uuid) W2 (uuid) W3 (uuid)]
    (try
      (store/fork! branch)
      (store/install! branch
                      {:object-types {:widget (ot :widget [[:name :string]])} :link-types {}})
      (store/insert-object! branch :widget {:id W1 :values {:name "one"}})
      (store/insert-object! branch :widget {:id W2 :values {:name "two"}})
      (let [read-at (:rev (world/head (world/at (store) branch)))]
        (section "nothing changed since the consumer read the type at its cursor")
        (check "an unchanged type is not moved" (not (moved? read-at)))

        (section "a USER edits W1 and creates W3 (no build)")
        (store/insert-object! branch :widget {:id W3 :values {:name "three"}})
        (store/update-object! branch :widget W1 {:name "one-edited"})
        (check "the type moved past the cursor" (moved? read-at))
        (check "the input is the whole current state, edits included"
               (= #{"one-edited" "two" "three"} (names)))
        (check "a cursor at the head sees nothing new"
               (not (moved? (:rev (world/head (world/at (store) branch)))))))
      (finally (try (store/retire! branch) (catch Exception _ nil))))))
