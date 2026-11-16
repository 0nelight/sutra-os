(ns ^:integration platform.pg-layout-narrowed-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [sutra.authoring.install.world :as onto-install]
            [sutra.store.access.scope :as ctx]
            [sutra.store.core.orakel :as orakel]
            [sutra.store.pg.layout :as layout]
            [support.pod :as pod]
            [support.store :as store]))

(use-fixtures :once pod/with-stores-only)

(defn- ot [nm & attrs]
  {:object-type-name nm :plural (str (name nm) "s")
   :attribute-list (vec (for [a attrs] {:attribute-name a :attribute-type :string}))})

(def ^:private a {"ri.t.a" (ot :a :nr)})

(def ^:private welten
  [{:object-types a}
   {:object-types (assoc a "ri.t.b" (ot :b :nr))}
   {:object-types (assoc a "ri.t.b" (ot :b :nr :stufe))}
   {:object-types (assoc a "ri.t.b" (ot :b :nr :stufe))
    :link-types   {"ri.l.ab" {:link-type-name :ab :from-object-type "ri.t.a"
                              :to-object-type "ri.t.b" :cardinality :many-to-many}}}
   {:object-types a}])

(defn- install! [b w drop?]
  (onto-install/install-world! (ctx/system-store b) b w
                               (cond-> {:actor {:principal "test"}}
                                 drop? (assoc :steps [[:drop-unclaimed]]
                                              :drop-unclaimed? true))))

(deftest a-commit-lays-out-the-tables-its-step-touched
  (doseq [drop? [false true]]
    (store/with-fresh-branch [b "narrow"]
      (let [narrowed (atom 0)
            lay      layout/layout!]
        (with-redefs [orakel/on?     true
                      layout/layout! (fn [c ns rev change]
                                       (when (:only change) (swap! narrowed inc))
                                       (lay c ns rev change))]
          (doseq [w welten] (install! b w drop?)))
        (is (<= (dec (count welten)) @narrowed)
            (str "every install after the first lays out narrowed, drop? " drop?))))))
