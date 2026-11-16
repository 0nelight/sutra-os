(ns ^:integration platform.delete-mutation-pg-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.authoring.install.world :as onto-install]
            [sutra.authoring.mutation :as mutation]
            [sutra.catalog.kernel :as kernel]
            [sutra.store.access.scope :as ctx]
            [sutra.store.protection :as protection]
            [sutra.store.seat :as seat]
            [support.pod :as pod]
            [support.store :as store]))

(use-fixtures :once pod/with-stores-only)

(defn- ot [nm]
  {:object-type-name nm :plural (str (name nm) "s") :primary-key :nr
   :attribute-list [{:attribute-name :nr :attribute-type :string}]})

(def ^:private ta "ri.sutra.main.object-type.00000000-0000-4000-8000-00000000000a")
(def ^:private tb "ri.sutra.main.object-type.00000000-0000-4000-8000-00000000000b")
(def ^:private lab "ri.sutra.main.link-type.00000000-0000-4000-8000-0000000000ab")

(def ^:private welt
  {:object-types {ta (ot :a) tb (ot :b)}
   :link-types   {lab {:link-type-name :ab :from-object-type ta
                             :to-object-type tb :cardinality :many-to-one}}
   :actions      {:setz {:params {:p {:type ta}}
                         :rules  [{:op :update :object-type ta :target [:get :p]
                                   :set [:nr]}]}}})

(defn- deleted
  [cascade? W]
  (store/with-fresh-branch [b "del"]
    (onto-install/install-world! (ctx/system-store b) b W {:actor {:principal "test"}})
    (let [st   (ctx/system-store b)
          step (assoc (kernel/step nil :delete :object-type {:name :a}) :cascade? cascade?)]
      {:answer (mutation/run-step st (seat/branch-schema st) step
                                  {:edge (protection/guard-reads st)})
       :types  (:object-types (seat/branch-schema (ctx/store b)))})))

(deftest a-referenced-type-is-refused
  (testing "refused: the link type and the action still name it"
    (is (= #{:link-types :actions}
           (into #{} (map :in) (get-in (deleted false welt) [:answer :conflict :referrers]))))))

(deftest a-cascading-delete-removes-the-type
  (let [{:keys [answer types]} (deleted true (dissoc welt :actions))]
    (is (nil? (:conflict answer)) (pr-str answer))
    (is (not (contains? types ta)))))
