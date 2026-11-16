(ns platform-sdk.objects
  (:require [platform-sdk.client :as client]
            [platform-sdk.errors :as err]
            [platform-sdk.wire :as w]
            [sutra.ref.value :as rv]))

(set! *warn-on-reflection* true)

(defn id [row] (:id row))

(defn values [row] (:values row))

(defn value [row attr] (rv/value row attr))

(defn title [row] (or (rv/value row :name) (:id row)))

(defn neighbourhood
  ([c row] (neighbourhood c row nil))
  ([c row scope-type] (w/around c [(id row)] scope-type)))

(defn linked
  ([c row side] (linked c (:object-type row) row side))
  ([c type-kw row side]
   (when-not (keyword? type-kw)
     (err/raise! :usage (str "linked needs the row's object type — the row carries none, "
                             "call (linked c type-kw row side)")
                 {:row (id row) :link side}))
   (let [far (:far-type (client/side c type-kw side))
         rows (:rows (w/query c {:find type-kw :ids [(id row)] :around [side]}))]
     (mapv #(assoc % :object-type far) rows))))
