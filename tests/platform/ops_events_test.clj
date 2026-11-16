(ns ^:integration platform.ops-events-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.catalog.grants :as grants]
            [sutra.foundation.ops :as op]
            [sutra.store.access.events :as events]
            [sutra.store.access.scope :as ctx]
            [sutra.store.control :as ctl]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-stores-only)

(def ^:private extra
  {:test/note   {:service :test :kind :command :domain :test :doc "notes n"
                 :in [:map [:n :int]] :out :any :emits #{:test/noted :test/bad}}
   :test/noted  {:service :test :kind :event :doc "n was noted" :payload [:map [:n :int]]}
   :test/bad    {:service :test :kind :event :doc "a bad n" :payload [:map [:n :int]]}
   :test/mirror {:service :test :kind :reaction :domain :test :on :test/noted
                 :consistency :same-commit :doc "notes n in the same commit"}
   :test/echo   {:service :test :kind :reaction :domain :test :on :test/noted
                 :doc "notes n after the commit"}
   :test/boom   {:service :test :kind :reaction :domain :test :on :test/bad
                 :doc "fails, always"}})

(defn- row [store graph n]
  (ctl/entry store :graph-layouts {:branch "main" :graph-id graph :node-kind :transform
                                   :node-key (str n) :x 0 :y 0}))

(defn- with-ops [f]
  (binding [op/*extra* extra]
    (op/register! :test/note nil 'platform.ops-events-test #{}
                  (fn [{:keys [store]} {:keys [n]}]
                    {:tx {:records [(row store "noted" n)]} :out n
                     :emit [(if (neg? n) [:test/bad {:n n}] [:test/noted {:n n}])]}))
    (op/register! :test/mirror nil 'platform.ops-events-test #{}
                  (fn [{:keys [store]} {:keys [n]}] {:tx {:records [(row store "mirror" n)]}}))
    (op/register! :test/echo nil 'platform.ops-events-test #{}
                  (fn [{:keys [store]} {:keys [n]}] {:tx {:records [(row store "echo" n)]}}))
    (op/register! :test/boom nil 'platform.ops-events-test #{}
                  (fn [_ _] (throw (ex-info "boom" {}))))
    (try (f)
         (finally (doseq [o (keys extra)] (op/unregister! o))))))

(defn- store [] (ctx/store "main"))

(defn- keys-of [graph]
  (set (map :node-key (ctl/list (store) :graph-layouts {:branch "main" :graph-id graph}))))

(deftest an-event-reaches-its-reactions
  (with-ops
    (fn []
      (is (= 7 (op/run! {:store (store) :auth grants/system-authority} :test/note {:n 7})))
      (testing "a :same-commit reaction is part of the decision"
        (is (contains? (keys-of "mirror") "7")))
      (testing "an :after-commit reaction ran right after the commit (:solo)"
        (is (contains? (keys-of "echo") "7")))
      (testing "its cursor moved: another round answers nothing twice"
        (events/dispatch-branch! (store) "main")
        (is (= #{"7"} (keys-of "echo")))))))

(deftest a-reaction-that-keeps-failing-parks-the-event
  (with-ops
    (fn []
      (op/run! {:store (store) :auth grants/system-authority} :test/note {:n -1})
      (dotimes [_ events/attempts-before-parking]
        (events/dispatch-branch! (store) "main"))
      (let [p (filter #(= :test/boom (:reaction %)) (events/parked (store) "main"))]
        (is (= 1 (count p)))
        (is (= {:n -1} (:payload (first p))))
        (is (= "boom" (:error (first p))))))))
