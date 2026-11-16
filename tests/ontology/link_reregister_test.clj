(ns ^:integration ontology.link-reregister-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [sutra.builds.import.reconcile :as reconcile]
            [sutra.catalog.lookup :as lookup]
            [sutra.governance.merge :as bm]
            [sutra.ref.clock :as clock]
            [sutra.ref.install :as ri]
            [sutra.store.access.scope :as ctx]
            [sutra.store.run :as run]
            [sutra.store.world :as world]
            [support.pod :as pod]
            [support.store :as store]))

(use-fixtures :once pod/with-mem-stores-only)

(defn- ot [nm] {:object-type-name nm :id nm
                :attribute-list [{:attribute-name :name :attribute-type :string}]})

(def ^:private w0
  {:object-types {:person (ot :person) :bereich (ot :bereich)}
   :link-types   {:cleared-for {:link-type-name :cleared-for :id :cleared-for
                                :from-object-type :person :to-object-type :bereich
                                :cardinality :many-to-many}}})

(defn- link-rid [b] (lookup/rid-of (world/world (ctx/system-store b)) :link-types :cleared-for))

(defn- with-producer
  [b nm]
  (assoc (world/authored (ctx/system-store b)) :transforms
         {nm {:transform-name nm :language :pg-sql :sql "select 1"
              :output {:kind :link-type :link-type (link-rid b)}}}))

(defn- ids [b] (into #{} (map (comp str :id)) (store/links b (link-rid b))))

(defn- edge [b id]
  (store/create-link (link-rid b) {:id id :from-id "p1" :to-id (str "b-" id)}))

(defn- platform-edge!
  [b id]
  (let [op (edge b id)]
    (run/run! (ctx/system-store b)
              {:fences [[:link-type (:type op)]]
               :decide (fn [_] {:tx {:ops [op] :message "platform"}})})))

(defn- commit!
  [b w]
  (let [base (world/world (ctx/system-store b))]
    (store/install! b (ri/derived {:sigma clock/system} w))
    (bm/reconcile-after-commit! (ctx/system-store b)
                                {:reregister-link! reconcile/reregister-link-backing!}
                                base)))

(defn- seed! [b]
  (store/install! b (ri/derived {:sigma clock/system} w0))
  (bm/reconcile-after-commit! (ctx/system-store b) {} nil)
  (doseq [[T id] [[:person "p1"] [:bereich "b-e"] [:bereich "b-p"] [:bereich "b-x"]]]
    (store/insert-object! b T {:id id :values {:name id}})))

(defn- build-empty!
  [b]
  (reconcile/reconcile-link-tables! (ctx/system-store b) b (world/world (ctx/system-store b))
                                    {(link-rid b) []} nil))

(deftest a-first-backing-keeps-every-edge-no-producer-made
  (store/with-fresh-branch [b "rereg-first"]
    (seed! b)
    (store/commit-ops! b [(edge b "e")])
    (platform-edge! b "p")
    (world/index! (ctx/system-store b) [(edge b "x")])
    (is (= #{"e" "p" "x"} (ids b)))
    (commit! b (with-producer b :cleared-for-conn-rows))
    (is (= #{"e" "p" "x"} (ids b)) "the merge retracts none of them")
    (build-empty! b)
    (is (= #{"e" "p" "x"} (ids b)) "they are edits: a build of an empty source keeps them")))

(deftest a-new-producer-retracts-what-the-old-one-wrote
  (store/with-fresh-branch [b "rereg-swap"]
    (seed! b)
    (commit! b (with-producer b :old-rows))
    (world/index! (ctx/system-store b) [(edge b "x")])
    (store/commit-ops! b [(edge b "e")])
    (platform-edge! b "p")
    (is (= #{"e" "p" "x"} (ids b)))
    (commit! b (with-producer b :new-rows))
    (is (= #{"e" "p"} (ids b)))))
