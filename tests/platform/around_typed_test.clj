(ns ^:unit platform.around-typed-test
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [sutra.catalog.interfaces :as interfaces]
            [sutra.catalog.link-ends :as link-ends]
            [sutra.world.gen.near :as gn]
            [sutra.objects.reads :as reads]
            [sutra.types.schema :as sch])
  (:import (java.util Random)))

(defn- objects-of [W] (for [T (keys (:object-types W)) i (range 3)] {:id (str T "#" i) :type T}))

(defn- links-of
  [W objs ^Random rnd]
  (vec (for [[L lt] (:link-types W)
             a objs :when (interfaces/satisfies-endpoint? W (:type a) (:from-object-type lt))
             b objs :when (interfaces/satisfies-endpoint? W (:type b) (:to-object-type lt))
             :when (< (.nextDouble rnd) 0.3)]
         {:id (str L "/" (:id a) "/" (:id b)) :L L :from (:id a) :to (:id b)})))

(defn- reader [links]
  (fn [L {:keys [from-ids to-ids]}]
    (let [ids (set (or from-ids to-ids)) end (if from-ids :from :to)]
      (filter #(and (= L (:L %)) (ids (get % end))) links))))

(defspec typed-seeds-read-the-links-of-every-link-type 500
  (prop/for-all [W gn/world cs (gen/vector gn/change 0 3) n gen/large-integer]
    (let [W     (sch/indexed (reduce gn/apply-change W cs))
          rnd   (Random. n)
          objs  (objects-of W)
          read  (reader (links-of W objs rnd))
          seeds (filter (fn [_] (< (.nextDouble rnd) 0.4)) objs)
          ids   (into #{} (map :id) seeds)
          types (distinct (map :type seeds))
          old   (reads/around-links (keys (:link-types W)) ids read)
          new   (reads/around-links (reads/seed-link-types #(link-ends/reaching W %) types)
                                    ids read)]
      (and (= (set old) (set new)) (= (count old) (count new))))))
