(ns sutra.catalog.link-ends
  (:require [sutra.catalog.interfaces :as interfaces]
            [sutra.world.view :as view]
            [sutra.types.link-side :as link-side]
            [sutra.types.schema :as sch]))

(set! *warn-on-reflection* true)

(def ^:private facts-of {:link-types (fn [_ lt] (link-side/ends lt))})

(defn- member [_ L] L)

(defn by-end
  [link-types]
  (view/fact-index facts-of member {:link-types link-types}))

(def view
  (view/facts ::ends facts-of member))

(defn reaching
  [schema T]
  (let [ends (sch/column schema ::ends #(by-end (:link-types %)))
        ifs  (:interfaces schema)]
    (into (get ends T #{})
          (comp (filter #(contains? ifs %)) (mapcat #(get ends %)))
          (interfaces/effective-implements schema (get-in schema [:object-types T])))))
