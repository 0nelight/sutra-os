(ns ^:unit store.count-ticks-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.store.core.reads]))

(def ^:private type-ticks #'sutra.store.core.reads/type-ticks)
(def ^:private count-ticks #'sutra.store.core.reads/count-ticks)

(def ^:private sig
  {:types      {"ri.a" {} "ri.b" {} "ri.plain" {}}
   :interfaces {"ri.i"     {:implementers ["ri.a" "ri.b"]}
                "ri.empty" {:implementers []}}
   :links      {"ri.stored"  {:from "ri.a" :to "ri.b" :cardinality :many-to-many}
                "ri.derived" {:from "ri.a" :to "ri.plain" :cardinality :many-to-one
                              :fk {:holder "ri.a" :target "ri.plain" :side :from :attr :k}}
                "ri.iface"   {:from "ri.i" :to "ri.plain" :cardinality :many-to-one
                              :fk {:holder "ri.i" :target "ri.plain" :side :from :attr :k}}}})

(deftest an-interface-is-its-implementers
  (is (= [[:type "ri.plain"]] (type-ticks sig "ri.plain")))
  (is (= [[:type "ri.a"] [:type "ri.b"]] (type-ticks sig "ri.i"))
      "its own clock never ticks, so the implementers' stand for it")
  (is (= [] (type-ticks sig "ri.empty"))
      "no implementer means no rows: only [:schema] can move the number"))

(deftest a-stored-link-is-keyed-on-its-own-clock
  (is (= [[:schema] [:link-type "ri.stored"]] (count-ticks sig "ri.stored"))
      "a write of ITS links is the only thing that moves it"))

(deftest a-derived-link-is-keyed-on-both-ends
  (testing "nothing writes it, so its own clock cannot report a change"
    (is (= [[:schema] [:link-type "ri.derived"] [:type "ri.a"] [:type "ri.plain"]]
           (count-ticks sig "ri.derived"))))
  (testing "an end may be an interface, and then its implementers stand for it"
    (is (= [[:schema] [:link-type "ri.iface"] [:type "ri.a"] [:type "ri.b"] [:type "ri.plain"]]
           (count-ticks sig "ri.iface")))))
