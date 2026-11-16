(ns ^:unit platform.role-parity-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.kit.system :as sys]
            [sutra.viz.layouts :as layouts]))

(def ^:private role-tables
  (into (sorted-map) (filter (comp :role val)) (:tables sys/registry)))

(def ^:private host-entries
  {:viz/layouts layouts/built-in})

(deftest the-registry-declares-role-tables
  (is (seq role-tables) "no table is marked :role — the law would be vacuous")
  (testing "every role table names the schema an entry is held to"
    (doseq [[t spec] role-tables]
      (is (contains? spec :entry) (str t " declares no :entry schema")))))

(deftest the-hosts-own-entries-pass-the-strangers-door
  (doseq [[t entries] host-entries
          :let [spec (get role-tables t)]]
    (is spec (str t " is no role table, but the host contributes to it"))
    (doseq [[k v] entries]
      (is (nil? (sys/entry-problem t k v))
          (str t " " k " — the host's own entry does not conform to the schema a"
               " plugin's is held to")))))

(deftest a-host-entry-is-reached-only-through-the-table
  (testing "the layout menu is the table: an empty table offers nothing, however many
            layouts the platform ships"
    (is (empty? (layouts/labels {}))))
  (testing "a contributed entry answers under its own name"
    (let [fremd {:label "Fremd" :layout (fn [nodes _ _] (mapv #(assoc % :x 1 :y 2) nodes))}]
      (is (= [{:id :a :x 1 :y 2}]
             (layouts/place {:nur-fremd fremd} [{:id :a}] [] {:layout :nur-fremd})))))
  (testing "and the host's default is no exception — without the table it is unknown"
    (is (thrown? clojure.lang.ExceptionInfo
                 (layouts/place {} [{:id :a}] [] nil)))))

(deftest the-platform-contributes-its-entries-as-a-module
  (testing "what host-entries lists is what some discovered module actually contributes —
            otherwise the law guards a map nothing installs"
    (let [tables (:tables (sys/assemble (sys/discover)))]
      (doseq [[t entries] host-entries
              [k v] entries]
        (is (= v (get-in tables [t k]))
            (str t " " k " is not what the assembled system holds — the host's entry"
                 " reaches the table some other way, or not at all"))))))
